-----------------------------------------------------------------------------
-- |
-- Module      :  DAP.Server
-- Copyright   :  (C) 2023 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <djohnson.m@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE LambdaCase                 #-}
----------------------------------------------------------------------------
module DAP.Server
  ( runDAPServer
  ) where
----------------------------------------------------------------------------
import           Control.Concurrent.MVar    ( MVar )
import           Control.Monad              ( when )
import           Control.Monad.Except       ( runExceptT )
import           Control.Concurrent.MVar    ( newMVar, newEmptyMVar, modifyMVar_
                                            , takeMVar, putMVar, readMVar )
import           Control.Concurrent.STM     ( newTVarIO )
import           Control.Exception          ( SomeException
                                            , IOException
                                            , catch
                                            , fromException
                                            , throwIO )
import           Control.Monad              ( forever, void )
import           Control.Monad.State        ( evalStateT, runStateT, execStateT, gets )
import           DAP.Internal               ( withGlobalLock )
import           Data.Aeson                 ( decodeStrict, eitherDecode, Value )
import           Data.Aeson.Encode.Pretty   ( encodePretty )
import           Data.ByteString            ( ByteString )
import           Data.Char                  ( isDigit )
import           Network.Simple.TCP         ( serve, HostPreference(Host) )
import           Network.Socket             ( socketToHandle, withSocketsDo, SockAddr, Socket )
import           System.IO                  ( hClose, hSetNewlineMode, Handle, Newline(CRLF)
                                            , NewlineMode(NewlineMode, outputNL, inputNL)
                                            , IOMode(ReadWriteMode) )
import           System.IO.Error            ( isEOFError )
import           Text.Read                  ( readMaybe )
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.ByteString.Char8      as BS
----------------------------------------------------------------------------
import           DAP.Types
import           DAP.Internal
import           DAP.Utils
import           DAP.Adaptor
----------------------------------------------------------------------------
runDAPServer
  :: ServerConfig
  -- ^ Top-level Server configuration, global across all debug sessions
  -> (Command -> Adaptor app ())
  -- ^ A function to facilitate communication between DAP clients, debug adaptors and debuggers
  -> IO ()
runDAPServer serverConfig@ServerConfig {..} communicate = withSocketsDo $ do
  putStrLn ("Running DAP server on " <> show port <> "...")
  appStore <- newTVarIO mempty
  serve (Host host) (show port) $ \(socket, address) -> do
    withGlobalLock (putStrLn $ "TCP connection established from " ++ show address)
    handle <- socketToHandle socket ReadWriteMode
    hSetNewlineMode handle NewlineMode { inputNL  = CRLF, outputNL = CRLF }
    request <- getRequest handle address serverConfig
    adaptorStateMVar <- initAdaptorState handle address appStore serverConfig request
    serviceClient communicate adaptorStateMVar `catch` exceptionHandler handle address

-- | Initializes the Adaptor
--
initAdaptorState
  :: Handle
  -> SockAddr
  -> AppStore app
  -> ServerConfig
  -> Request
  -> IO (MVar (AdaptorState app))
initAdaptorState handle address appStore serverConfig request = do
  handleLock               <- newMVar ()
  seqRef                   <- pure 0
  variablesMap             <- pure mempty
  sourceReferencesMap      <- pure mempty
  sessionId                <- pure Nothing
  currentFrameId           <- pure 0
  currentScopeId           <- pure 0
  currentVariableId        <- pure 0
  currentSourceReferenceId <- pure 0
  currentBreakpointId      <- pure 0
  adaptorStateMVar         <- newEmptyMVar
  putMVar adaptorStateMVar AdaptorState
    { messageType = MessageTypeResponse
    , payload = []
    , ..
    }
  pure adaptorStateMVar
----------------------------------------------------------------------------
-- | Updates sequence number, puts the new request into the AdaptorState
--
updateAdaptorState
  :: AdaptorState app
  -> Request
  -> AdaptorState app
updateAdaptorState state request = do
  state { request = request
        , seqRef = requestSeqNum request
        }
----------------------------------------------------------------------------
-- | Communication loop between editor and adaptor
-- Evaluates the current 'Request' located in the 'AdaptorState'
-- Fetches, updates and recurses on the next 'Request'
--
serviceClient
  :: (Command -> Adaptor app ())
  -> MVar (AdaptorState app)
  -> IO ()
serviceClient communicate adaptorStateMVar = do
  runAdaptorWith adaptorStateMVar $ do
    request <- gets request
    communicate (command request)

  -- HINT: getRequest is a blocking action so we use readMVar to leave MVar available
  AdaptorState { address, handle, serverConfig } <- readMVar adaptorStateMVar
  nextRequest <- getRequest handle address serverConfig
  modifyMVar_ adaptorStateMVar $ \s -> pure $ updateAdaptorState s nextRequest

  -- loop: serve the next request
  serviceClient communicate adaptorStateMVar

----------------------------------------------------------------------------
-- | Handle exceptions from client threads, parse and log accordingly
exceptionHandler :: Handle -> SockAddr -> SomeException -> IO ()
exceptionHandler handle address (e :: SomeException) = do
  let
    dumpError
      | Just (ParseException msg) <- fromException e
          = logger ERROR address Nothing
            $ withBraces
            $ BL8.pack ("Parse Exception encountered: " <> msg)
      | Just (err :: IOException) <- fromException e, isEOFError err
          = logger ERROR address Nothing
            $ withBraces "Empty payload received"
      | otherwise
          = logger ERROR address Nothing
            $ withBraces
            $ BL8.pack ("Unknown Exception: " <> show e)
  dumpError
  logger ERROR address Nothing (withBraces "Closing Connection")
  hClose handle
----------------------------------------------------------------------------
-- | Internal function for parsing a 'ProtocolMessage' header
-- This function also dispatches on 'talk'
--
-- 'parseHeader' Attempts to parse 'Content-Length: <byte-count>'
-- Helper function for parsing message headers
-- e.g. ("Content-Length: 11\r\n")
getRequest :: Handle -> SockAddr -> ServerConfig -> IO Request
getRequest handle addr ServerConfig {..} = do
  headerBytes <- BS.hGetLine handle
  void (BS.hGetLine handle)
  parseHeader headerBytes >>= \case
    Left errorMessage -> do
      logger ERROR addr Nothing (BL8.pack errorMessage)
      throwIO (ParseException errorMessage)
    Right count -> do
      body <- BS.hGet handle count
      when debugLogging $ do
        logger DEBUG addr (Just RECEIVED)
          ("\n" <> encodePretty (decodeStrict body :: Maybe Value))
      case eitherDecode (BL8.fromStrict body) of
        Left couldn'tDecodeBody -> do
          logger ERROR addr Nothing (BL8.pack couldn'tDecodeBody)
          throwIO (ParseException couldn'tDecodeBody)
        Right request ->
          pure request
  where
    ----------------------------------------------------------------------------
    -- | Parses the HeaderPart of all ProtocolMessages
    parseHeader :: ByteString -> IO (Either String PayloadSize)
    parseHeader bytes = do
      let byteSize = BS.takeWhile isDigit (BS.drop (BS.length "Content-Length: ") bytes)
      case readMaybe (BS.unpack byteSize) of
        Just contentLength ->
          pure (Right contentLength)
        Nothing ->
          pure $ Left ("Invalid payload: " <> BS.unpack bytes)

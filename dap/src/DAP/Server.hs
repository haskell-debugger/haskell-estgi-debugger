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
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE LambdaCase                 #-}
----------------------------------------------------------------------------
module DAP.Server
  ( runDAPServer
  ) where
----------------------------------------------------------------------------
import           Control.Monad              (when)
import           Data.IORef                 ( newIORef, atomicWriteIORef )
import           Control.Concurrent.MVar    ( newMVar )
import           Control.Concurrent.STM     ( newTVarIO )
import           Control.Exception          ( SomeException
                                            , IOException
                                            , catch
                                            , fromException
                                            , throwIO )
import           Control.Monad              ( forever, void )
import           Control.Monad.State        ( evalStateT )
import           DAP.Internal               ( withGlobalLock )
import           Data.Aeson                 ( decodeStrict, eitherDecode, Value )
import           Data.Aeson.Encode.Pretty   ( encodePretty )
import           Data.ByteString            ( ByteString )
import           Data.Char                  ( isDigit )
import           Network.Simple.TCP         ( serve, HostPreference(Host) )
import           Network.Socket             ( socketToHandle, withSocketsDo, SockAddr )
import           System.IO                  ( hClose, hSetNewlineMode, Handle, Newline(CRLF)
                                            , NewlineMode(NewlineMode, outputNL, inputNL)
                                            , IOMode(ReadWriteMode)
                                            )
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
  -> (Command -> AdaptorClient app ())
  -- ^ A function to facilitate communication between DAP clients, debug adaptors and debuggers
  -> IO ()
runDAPServer serverConfig@ServerConfig {..} communicate = withSocketsDo $ do
  putStrLn ("Running DAP server on " <> show port <> "...")
  appStore <- newTVarIO mempty
  serve (Host host) (show port) $ \(socket, address) -> do
    withGlobalLock (putStrLn $ "TCP connection established from " ++ show address)
    handle <- socketToHandle socket ReadWriteMode
    hSetNewlineMode handle NewlineMode { inputNL  = CRLF, outputNL = CRLF }
    connectionLock <- newMVar ()
    seqRef <- newIORef 0
    sessionRef <- newIORef Nothing
    flip catch (exceptionHandler handle address) $ forever $ do
      request <- getRequest handle address
      atomicWriteIORef seqRef (requestSeqNum request)
      let adaptorState = mkAdaptorState appStore request handle address seqRef connectionLock sessionRef
      runAdaptorClient adaptorState $ communicate (command request)
  where
    ----------------------------------------------------------------------------
    -- | Makes empty adaptor state
    mkAdaptorState appStore request handle address seqRef connectionLock sessionRef
      = AdaptorState MessageTypeResponse [] appStore serverConfig
          seqRef handle request address sessionRef connectionLock
    ----------------------------------------------------------------------------
    -- | Utility for evaluating a monad transformer stack
    runAdaptorClient :: AdaptorState app -> AdaptorClient app a -> IO a
    runAdaptorClient adaptorState (AdaptorClient client) = evalStateT client adaptorState
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
      logger ERROR address Nothing "[Closing Connection]"
      hClose handle
    ----------------------------------------------------------------------------
    -- | Internal function for parsing a 'ProtocolMessage' header
    -- This function also dispatches on 'talk'
    --
    -- 'parseHeader' Attempts to parse 'Content-Length: <byte-count>'
    -- Helper function for parsing message headers
    -- e.g. ("Content-Length: 11\r\n")
    getRequest :: Handle -> SockAddr -> IO Request
    getRequest handle addr = do
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

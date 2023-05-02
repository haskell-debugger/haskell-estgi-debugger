-----------------------------------------------------------------------------
-- |
-- Module      :  DAP.Adaptor
-- Copyright   :  (C) 2023 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <djohnson.m@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE LambdaCase                 #-}
----------------------------------------------------------------------------
module DAP.Adaptor
  ( debugMessage
  , sendSuccesfulEmptyResponse
  , sendSuccesfulResponse
  , sendSuccesfulEvent
  , getServerCapabilities
  , setBody
  , withConnectionLock
  , getArguments
  , registerNewDebugSession
  , withDebugSession
  , getDebugSessionId
  , destroyDebugSession
  -- * Logging
  , logWarn
  , logError
  , logInfo
  , logger
  ) where
----------------------------------------------------------------------------
import           Control.Concurrent.Lifted  ( fork, killThread )
import           Control.Exception          ( throwIO )
import           Control.Concurrent.STM     ( atomically, readTVarIO, modifyTVar' )
import           Control.Monad              ( when )
import           Control.Monad.State        ( MonadIO(liftIO), gets, modify' )
import           Data.Aeson                 ( FromJSON, Result (..), fromJSON )
import           Data.Aeson.Encode.Pretty   ( encodePretty )
import           Data.Aeson.Types           ( object, Key, KeyValue((.=)), ToJSON )
import           Data.IORef                 ( atomicModifyIORef', readIORef, atomicWriteIORef )
import           Data.Text                  ( unpack, Text )
import           Network.Socket             ( SockAddr )
import           System.IO                  ( Handle )
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.ByteString.Char8      as BS
import qualified Data.HashMap.Strict        as H
----------------------------------------------------------------------------
import           DAP.Types
import           DAP.Utils
import           DAP.Internal
----------------------------------------------------------------------------
-- | Meant for internal consumption
logDebug :: DebugStatus -> BL8.ByteString -> AdaptorClient app ()
logDebug d = logWithAddr DEBUG (Just d)
----------------------------------------------------------------------------
logWarn :: BL8.ByteString -> AdaptorClient app ()
logWarn msg = logWithAddr WARN Nothing (withBraces msg)
----------------------------------------------------------------------------
logError :: BL8.ByteString -> AdaptorClient app ()
logError msg = logWithAddr ERROR Nothing (withBraces msg)
----------------------------------------------------------------------------
logInfo :: BL8.ByteString -> AdaptorClient app ()
logInfo msg = logWithAddr INFO Nothing (withBraces msg)
----------------------------------------------------------------------------
-- | Meant for internal consumption
debugMessage :: BL8.ByteString -> AdaptorClient app ()
debugMessage msg = do
  shouldLog <- getDebugLogging
  addr <- getAddress
  liftIO
    $ when shouldLog
    $ logger DEBUG addr Nothing msg
----------------------------------------------------------------------------
-- | Meant for external consumption
logWithAddr :: Level -> Maybe DebugStatus -> BL8.ByteString -> AdaptorClient app ()
logWithAddr level status msg = do
  addr <- getAddress
  liftIO (logger level addr status msg)
----------------------------------------------------------------------------
-- | Meant for external consumption
logger :: Level -> SockAddr -> Maybe DebugStatus -> BL8.ByteString -> IO ()
logger level addr maybeDebug msg = do
  liftIO
    $ withGlobalLock
    $ BL8.putStrLn formatted
  where
    formatted
      = BL8.concat
      [ withBraces $ BL8.pack (show addr)
      , withBraces $ BL8.pack (show level)
      , maybe mempty (withBraces . BL8.pack . show) maybeDebug
      , msg
      ]
----------------------------------------------------------------------------
getDebugLogging :: AdaptorClient app Bool
getDebugLogging = gets (debugLogging . adaptorServerConfig)
----------------------------------------------------------------------------
getServerCapabilities :: AdaptorClient app Capabilities
getServerCapabilities = gets (serverCapabilities . adaptorServerConfig)
----------------------------------------------------------------------------
getAddress :: AdaptorClient app SockAddr
getAddress = gets address
----------------------------------------------------------------------------
getHandle :: AdaptorClient app Handle
getHandle = gets handle
----------------------------------------------------------------------------
getNextSeqNum :: AdaptorClient app Seq
getNextSeqNum = do
  ref <- gets seqRef
  liftIO $ atomicModifyIORef' ref $ \x -> (x, x + 1)
----------------------------------------------------------------------------
getRequestSeqNum :: AdaptorClient app Seq
getRequestSeqNum = gets (requestSeqNum . request)
----------------------------------------------------------------------------
getDebugSessionId :: AdaptorClient app (Maybe SessionId)
getDebugSessionId = liftIO . readIORef =<< gets sessionId
----------------------------------------------------------------------------
setDebugSessionId :: SessionId -> AdaptorClient app ()
setDebugSessionId session = do
  ref <- gets sessionId
  liftIO (atomicWriteIORef ref (Just session))
----------------------------------------------------------------------------
registerNewDebugSession
  :: SessionId
  -> app
  -> AdaptorClient app ()
  -- ^ Long running operation, meant to be used as a sink for
  -- the debugger to emit events and for the adaptor to forward to the editor
  -- This function should be in a 'forever' loop waiting on the read end of
  -- a debugger channel.
  -> AdaptorClient app ()
registerNewDebugSession k v action = do
  store <- gets adaptorAppStore
  tid <- fork (resetAdaptorStatePayload >> action)
  liftIO . atomically $ modifyTVar' store (H.insert k (tid, v))
  setDebugSessionId k
  logInfo $ BL8.pack $ "Registered new debug session: " <> unpack k
----------------------------------------------------------------------------
withDebugSession :: (app -> AdaptorClient app ()) -> AdaptorClient app ()
withDebugSession continuation = do
  getDebugSessionId >>= \case
    Nothing -> sessionNotFound
    Just sessionId -> do
      appStore <- liftIO . readTVarIO =<< getAppStore
      case H.lookup sessionId appStore of
        Nothing ->
          appNotFound sessionId
        Just (_, state) ->
          continuation state
    where
      appNotFound sessionId = do
        logError "A Session exists but no debugger has been registered"
        let err = concat
              [ "SessionID: " <> unpack sessionId
              , "has no corresponding Debugger registered"
              ]
        liftIO $ throwIO (DebugSessionIdException err)
      sessionNotFound =
        logWarn "No Debug Session has started"
----------------------------------------------------------------------------
-- | Whenever a debug Session ends (cleanly or otherwise) this function
-- will remove the local debugger communication state from the global state
----------------------------------------------------------------------------
destroyDebugSession :: AdaptorClient app ()
destroyDebugSession = do
  getDebugSessionId >>= \case
    Nothing -> sessionNotFound
    Just sessionId -> do
      appStoreTVar <- getAppStore
      appStore <- liftIO (readTVarIO appStoreTVar)
      case H.lookup sessionId appStore of
        Nothing -> sessionNotFound
        Just (tid, state) -> do
          killThread tid
          liftIO . atomically $ modifyTVar' appStoreTVar (H.delete sessionId)
          logInfo $ BL8.pack $ "SessionId " <> unpack sessionId <> " ended"
  where
    sessionNotFound =
      logWarn "No Debug Session has started"
----------------------------------------------------------------------------
getAppStore :: AdaptorClient app (AppStore app)
getAppStore = gets adaptorAppStore
----------------------------------------------------------------------------
getCommand :: AdaptorClient app Command
getCommand = command <$> gets request
----------------------------------------------------------------------------
-- | Function for constructing a payload and writing bytes to a socket.
-- This function takes care of incrementing sequence numbers
-- and setting fields automatically that are required for 'response' messages.
-- i.e. "request_seq" and "command".
-- We also have to be sure to reset the message payload
----------------------------------------------------------------------------
send :: AdaptorClient app () -> AdaptorClient app ()
send action = do
  ()            <- action
  cmd           <- getCommand
  handle        <- getHandle
  seqNum        <- getNextSeqNum
  address       <- getAddress
  messageType   <- gets adaptorMessageType
  requestSeqNum <- getRequestSeqNum

  -- Additional fields are required to be set for 'response' or 'reverse_request' messages.
  when (messageType == MessageTypeResponse) (setField "request_seq" requestSeqNum)
  when (messageType `elem` [MessageTypeResponse, MessageTypeRequest]) (setField "command" cmd)

  -- "seq" and "type" must be set for all protocol messages
  setField "type" messageType
  setField "seq" seqNum

  -- Once all fields are set, fetch the payload for sending
  payload <- object <$> gets adaptorPayload

  -- Send payload to client from debug adaptor
  writeToHandle address handle payload

  -- Reset payload each time a send occurs
  resetAdaptorStatePayload
----------------------------------------------------------------------------
-- | Writes payload to the given 'Handle' using the local connection lock
----------------------------------------------------------------------------
writeToHandle
  :: ToJSON event
  => SockAddr
  -> Handle
  -> event
  -> AdaptorClient app ()
writeToHandle addr handle evt = do
  let msg = encodeBaseProtocolMessage evt
  logDebug SENT ("\n" <> encodePretty evt)
  withConnectionLock (BS.hPutStr handle msg)
----------------------------------------------------------------------------
-- | Resets Adaptor's payload
----------------------------------------------------------------------------
resetAdaptorStatePayload :: AdaptorClient app ()
resetAdaptorStatePayload = modify' $ \s -> s { adaptorPayload = [] }
----------------------------------------------------------------------------
sendSuccesfulResponse :: AdaptorClient app () -> AdaptorClient app ()
sendSuccesfulResponse action = do
 send $ do
    setType MessageTypeResponse
    setSuccess True
    action
----------------------------------------------------------------------------
sendSuccesfulEmptyResponse :: AdaptorClient app ()
sendSuccesfulEmptyResponse = sendSuccesfulResponse (pure ())
----------------------------------------------------------------------------
-- | Sends successful event
sendSuccesfulEvent :: EventType -> AdaptorClient app () -> AdaptorClient app ()
sendSuccesfulEvent event action = do
  send $ do
    setEvent event
    setType MessageTypeEvent
    action
----------------------------------------------------------------------------
-- | Sends unsuccessful response
-- > sendErrorResponse errorResponse errorMesssage sendSuccesfulEmptyResponse
--
sendErrorResponse
  :: ErrorResponse
  -> ErrorMessage
  -> AdaptorClient app ()
sendErrorResponse errorResponse errorMessage = do
  send $ do
    setType MessageTypeResponse
    setSuccess False
    setErrorMessage errorMessage
    setBody errorResponse
----------------------------------------------------------------------------
setErrorMessage
  :: ErrorMessage
  -> AdaptorClient app ()
setErrorMessage v = setField "message" v
----------------------------------------------------------------------------
-- | Sends successful event
setSuccess
  :: Bool
  -> AdaptorClient app ()
setSuccess = setField "success"
----------------------------------------------------------------------------
setBody
  :: ToJSON value
  => value
  -> AdaptorClient app ()
setBody value = setField "body" value
----------------------------------------------------------------------------
setType
  :: MessageType
  -> AdaptorClient app ()
setType messageType = do
  modify' $ \adaptorState ->
    adaptorState
    { adaptorMessageType = messageType
    }
----------------------------------------------------------------------------
setEvent
  :: EventType
  -> AdaptorClient app ()
setEvent = setField "event"
----------------------------------------------------------------------------
setField
  :: ToJSON value
  => Key
  -> value
  -> AdaptorClient app ()
setField key value = do
  payload <- gets adaptorPayload
  modify' $ \adaptorState ->
    adaptorState
    { adaptorPayload = (key .= value) : payload
    }
----------------------------------------------------------------------------
withConnectionLock
  :: IO ()
  -> AdaptorClient app ()
withConnectionLock action = do
  lock <- gets handleLock
  liftIO (withLock lock action)
----------------------------------------------------------------------------
-- | Attempt to parse arguments from the Request
----------------------------------------------------------------------------
getArguments
  :: (Show value, FromJSON value)
  => AdaptorClient app value
getArguments = do
  maybeArgs <- gets (args . request)
  let msg = "No args found for this message"
  case maybeArgs of
    Nothing -> do
      logError (BL8.pack msg)
      liftIO $ throwIO (ExpectedArguments msg)
    Just val ->
      case fromJSON val of
        Success r -> pure r
        x -> do
          logError (BL8.pack (show x))
          liftIO $ throwIO (ParseException (show x))
----------------------------------------------------------------------------

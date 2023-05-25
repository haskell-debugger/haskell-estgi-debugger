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
  ( -- * Message Construction
    setBody
  , setField
    -- * Response
  , sendSuccesfulEmptyResponse
  , sendSuccesfulResponse
  , sendErrorResponse
  -- * Events
  , sendSuccesfulEvent
  -- * Server
  , getServerCapabilities
  , withConnectionLock
  -- * Request Arguments
  , getArguments
  , getRequestSeqNum
  -- * Debug Session
  , registerNewDebugSession
  , updateDebugSession
  , getDebugSession
  , getDebugSessionId
  , destroyDebugSession
  -- * Error handling
  , sendError
  -- * Logging
  , logWarn
  , logError
  , logInfo
  , logger
  , debugMessage
  -- * Internal use
  , send
  , sendRaw
  ) where
----------------------------------------------------------------------------
import           Control.Concurrent         ( ThreadId )
import           Control.Concurrent.Lifted  ( fork, killThread )
import qualified Control.Concurrent.MVar    as MVar
import           Control.Exception          ( throwIO )
import           Control.Concurrent.STM     ( atomically, readTVarIO, modifyTVar' )
import           Control.Monad              ( when, unless )
import           Control.Monad.Except       ( throwError )
import           Control.Monad.State.Strict ( MonadIO(liftIO), gets, modify', put )
import           Data.Aeson                 ( FromJSON, Result (..), fromJSON )
import           Data.Maybe                 ( fromMaybe )
import           Data.Aeson.Encode.Pretty   ( encodePretty )
import           Data.Aeson.Types           ( object, Key, KeyValue((.=)), ToJSON )
import qualified Data.IntMap.Strict         as I
import           Data.IntMap.Strict         (IntMap)
import           Data.Text                  ( unpack, Text, pack )
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
logDebug :: DebugStatus -> BL8.ByteString -> Adaptor app ()
logDebug d = logWithAddr DEBUG (Just d)
----------------------------------------------------------------------------
logWarn :: BL8.ByteString -> Adaptor app ()
logWarn msg = logWithAddr WARN Nothing (withBraces msg)
----------------------------------------------------------------------------
logError :: BL8.ByteString -> Adaptor app ()
logError msg = logWithAddr ERROR Nothing (withBraces msg)
----------------------------------------------------------------------------
logInfo :: BL8.ByteString -> Adaptor app ()
logInfo msg = logWithAddr INFO Nothing (withBraces msg)
----------------------------------------------------------------------------
-- | Meant for internal consumption, used to signify a message has been
-- SENT from the server
debugMessage :: BL8.ByteString -> Adaptor app ()
debugMessage msg = do
  shouldLog <- getDebugLogging
  addr <- getAddress
  liftIO
    $ when shouldLog
    $ logger DEBUG addr (Just SENT) msg
----------------------------------------------------------------------------
-- | Meant for external consumption
logWithAddr :: Level -> Maybe DebugStatus -> BL8.ByteString -> Adaptor app ()
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
getDebugLogging :: Adaptor app Bool
getDebugLogging = gets (debugLogging . serverConfig)
----------------------------------------------------------------------------
getServerCapabilities :: Adaptor app Capabilities
getServerCapabilities = gets (serverCapabilities . serverConfig)
----------------------------------------------------------------------------
getAddress :: Adaptor app SockAddr
getAddress = gets address
----------------------------------------------------------------------------
getHandle :: Adaptor app Handle
getHandle = gets handle
----------------------------------------------------------------------------
getRequestSeqNum :: Adaptor app Seq
getRequestSeqNum = gets (requestSeqNum . request)
----------------------------------------------------------------------------
getDebugSessionId :: Adaptor app SessionId
getDebugSessionId = do
  gets sessionId >>= \case
    Nothing -> sessionNotFound
    Just sessionId -> pure sessionId
  where
    sessionNotFound = do
      let err = "No Debug Session has started"
      sendError (ErrorMessage (pack err)) Nothing
----------------------------------------------------------------------------
setDebugSessionId :: SessionId -> Adaptor app ()
setDebugSessionId session = modify' $ \s -> s { sessionId = Just session }
----------------------------------------------------------------------------
registerNewDebugSession
  :: SessionId
  -> app
  -> Adaptor app ()
  -- ^ Action to run debugger (operates in a forked thread that gets killed when disconnect is set)
  -> Adaptor app ()
  -- ^ Long running operation, meant to be used as a sink for
  -- the debugger to emit events and for the adaptor to forward to the editor
  -- This function should be in a 'forever' loop waiting on the read end of
  -- a debugger channel.
  -> Adaptor app ()
registerNewDebugSession k v debuggerExecution outputEventSink = do
  store <- gets appStore
  lock1 <- liftIO $ MVar.newEmptyMVar
  lock2 <- liftIO $ MVar.newEmptyMVar
  let setupAndRun lock action = do
        liftIO (MVar.takeMVar lock)
        resetAdaptorStatePayload
        action
  setDebugSessionId k
  debuggerThreadState <-
    DebuggerThreadState
      <$> fork (setupAndRun lock1 debuggerExecution)
      <*> fork (setupAndRun lock2 outputEventSink)
  liftIO $ do
    atomically $ modifyTVar' store (H.insert k (debuggerThreadState, v))
    MVar.putMVar lock1 ()
    MVar.putMVar lock2 ()
  logInfo $ BL8.pack $ "Registered new debug session: " <> unpack k
----------------------------------------------------------------------------
getDebugSession :: Adaptor a a
getDebugSession = do
  (_, _, app) <- getDebugSessionWithThreadIdAndSessionId
  pure app
----------------------------------------------------------------------------
updateDebugSession :: (app -> app) -> Adaptor app ()
updateDebugSession updateFun = do
  sessionId <- getDebugSessionId
  store <- gets appStore
  liftIO . atomically $ modifyTVar' store (H.adjust (fmap updateFun) sessionId)
----------------------------------------------------------------------------
getDebugSessionWithThreadIdAndSessionId :: Adaptor app (SessionId, DebuggerThreadState, app)
getDebugSessionWithThreadIdAndSessionId = do
  sessionId <- getDebugSessionId
  appStore <- liftIO . readTVarIO =<< getAppStore
  case H.lookup sessionId appStore of
    Nothing -> do
      sendError (ErrorMessage (pack "")) Nothing
    Just (tid, app) ->
      pure (sessionId, tid, app)
  where
    appNotFound sessionId = do
      let err = concat
            [ "SessionID: " <> unpack sessionId
            , "has no corresponding Debugger registered"
            ]
      sendError (ErrorMessage (pack err)) Nothing
----------------------------------------------------------------------------
-- | Whenever a debug Session ends (cleanly or otherwise) this function
-- will remove the local debugger communication state from the global state
----------------------------------------------------------------------------
destroyDebugSession :: Adaptor app ()
destroyDebugSession = do
  (sessionId, DebuggerThreadState {..}, app) <- getDebugSessionWithThreadIdAndSessionId
  store <- getAppStore
  liftIO $ do
    killThread debuggerThread
    killThread debuggerOutputEventThread
    atomically $ modifyTVar' store (H.delete sessionId)
  logInfo $ BL8.pack $ "SessionId " <> unpack sessionId <> " ended"
----------------------------------------------------------------------------
getAppStore :: Adaptor app (AppStore app)
getAppStore = gets appStore
----------------------------------------------------------------------------
getCommand :: Adaptor app Command
getCommand = command <$> gets request
----------------------------------------------------------------------------
-- | 'sendRaw' (internal use only)
-- Sends a raw JSON payload to the editor. No "seq", "type" or "command" fields are set.
-- The message is still encoded with the ProtocolMessage Header, byte count, and CRLF.
--
sendRaw :: ToJSON value => value -> Adaptor app ()
sendRaw value = do
  handle        <- getHandle
  address       <- getAddress
  writeToHandle address handle value
----------------------------------------------------------------------------
-- | Function for constructing a payload and writing bytes to a socket.
-- This function takes care of incrementing sequence numbers
-- and setting fields automatically that are required for 'response' messages.
-- i.e. "request_seq" and "command".
-- We also have to be sure to reset the message payload
----------------------------------------------------------------------------
send :: Adaptor app () -> Adaptor app ()
send action = do
  ()            <- action
  cmd           <- getCommand
  handle        <- getHandle
  messageType   <- gets messageType
  address       <- getAddress
  requestSeqNum <- getRequestSeqNum
  let seqNum    =  requestSeqNum + 1

  -- Additional fields are required to be set for 'response' or 'reverse_request' messages.
  when (messageType == MessageTypeResponse) (setField "request_seq" requestSeqNum)
  when (messageType `elem` [MessageTypeResponse, MessageTypeRequest]) (setField "command" cmd)

  -- "seq" and "type" must be set for all protocol messages
  setField "type" messageType
  unless (messageType == MessageTypeEvent) $
    setField "seq" seqNum

  -- Once all fields are set, fetch the payload for sending
  payload <- object <$> gets payload

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
  -> Adaptor app ()
writeToHandle addr handle evt = do
  let msg = encodeBaseProtocolMessage evt
  debugMessage ("\n" <> encodePretty evt)
  withConnectionLock (BS.hPutStr handle msg)
----------------------------------------------------------------------------
-- | Resets Adaptor's payload
----------------------------------------------------------------------------
resetAdaptorStatePayload :: Adaptor app ()
resetAdaptorStatePayload = modify' $ \s -> s { payload = [] }
----------------------------------------------------------------------------
sendSuccesfulResponse :: Adaptor app () -> Adaptor app ()
sendSuccesfulResponse action = do
 send $ do
    setType MessageTypeResponse
    setSuccess True
    action
----------------------------------------------------------------------------
sendSuccesfulEmptyResponse :: Adaptor app ()
sendSuccesfulEmptyResponse = sendSuccesfulResponse (pure ())
----------------------------------------------------------------------------
-- | Sends successful event
sendSuccesfulEvent :: EventType -> Adaptor app () -> Adaptor app ()
sendSuccesfulEvent event action = do
  send $ do
    setEvent event
    setType MessageTypeEvent
    action
----------------------------------------------------------------------------
-- | Raises an error
-- Meant abort the current reqeust / response cycle, prematurely sending an 'ErrorResponse'
-- <https://microsoft.github.io/debug-adapter-protocol/specification#Base_Protocol_ErrorResponse>
--
sendError
  :: ErrorMessage
  -> Maybe Message
  -> Adaptor app a
sendError errorMessage maybeMessage = do
  throwError (errorMessage, maybeMessage)
----------------------------------------------------------------------------
-- | Sends unsuccessful response
-- Only used internally within the Server module
sendErrorResponse
  :: ErrorMessage
  -> Maybe Message
  -> Adaptor app ()
sendErrorResponse errorMessage maybeMessage = do
  send $ do
    setType MessageTypeResponse
    setSuccess False
    setErrorMessage errorMessage
    setBody (ErrorResponse maybeMessage)
----------------------------------------------------------------------------
setErrorMessage
  :: ErrorMessage
  -> Adaptor app ()
setErrorMessage v = setField "message" v
----------------------------------------------------------------------------
-- | Sends successful event
setSuccess
  :: Bool
  -> Adaptor app ()
setSuccess = setField "success"
----------------------------------------------------------------------------
setBody
  :: ToJSON value
  => value
  -> Adaptor app ()
setBody value = setField "body" value
----------------------------------------------------------------------------
setType
  :: MessageType
  -> Adaptor app ()
setType messageType = do
  modify' $ \adaptorState ->
    adaptorState
    { messageType = messageType
    }
----------------------------------------------------------------------------
setEvent
  :: EventType
  -> Adaptor app ()
setEvent = setField "event"
----------------------------------------------------------------------------
setField
  :: ToJSON value
  => Key
  -> value
  -> Adaptor app ()
setField key value = do
  currentPayload <- gets payload
  modify' $ \adaptorState ->
    adaptorState
    { payload = (key .= value) : currentPayload
    }
----------------------------------------------------------------------------
withConnectionLock
  :: IO ()
  -> Adaptor app ()
withConnectionLock action = do
  lock <- gets handleLock
  liftIO (withLock lock action)
----------------------------------------------------------------------------
-- | Attempt to parse arguments from the Request
----------------------------------------------------------------------------
getArguments
  :: (Show value, FromJSON value)
  => Adaptor app value
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

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
  -- * Object lifetime reset
  , resetObjectLifetimes
  -- * Variable reference handling API
  , addVariable
  , getVariables
  , getAllVars
  , getVariableReferencesMap
  -- * Frame handling
  , setCurrentFrameId
  , getCurrentFrameId
  -- * Scope handling
  , setCurrentScopeId
  , getCurrentScopeId
  , getNextScopeId
  , resetNextScopeId
  -- * Variable handling
  , setCurrentVariableId
  , getCurrentVariableId
  , getNextVariableId
  -- * Source handling
  , getNextSourceReferenceId
  , getSourcePathBySourceReferenceId
  , addSourcePathBySourceReferenceId
  -- * Internal use
  , send
  , sendRaw
  ) where
----------------------------------------------------------------------------
import           Control.Concurrent         ( ThreadId )
import           Control.Concurrent.Lifted  ( fork, killThread )
import           Control.Exception          ( throwIO )
import           Control.Concurrent.STM     ( atomically, readTVarIO, modifyTVar' )
import           Control.Monad              ( when )
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
-- | Meant for internal consumption
debugMessage :: BL8.ByteString -> Adaptor app ()
debugMessage msg = do
  shouldLog <- getDebugLogging
  addr <- getAddress
  liftIO
    $ when shouldLog
    $ logger DEBUG addr Nothing msg
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
getNextSeqNum :: Adaptor app Seq
getNextSeqNum = do
  modify' $ \s -> s { seqRef = seqRef s + 1 }
  gets seqRef
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
  -- ^ Long running operation, meant to be used as a sink for
  -- the debugger to emit events and for the adaptor to forward to the editor
  -- This function should be in a 'forever' loop waiting on the read end of
  -- a debugger channel.
  -> Adaptor app ()
registerNewDebugSession k v action = do
  store <- gets appStore
  tid <- fork (resetAdaptorStatePayload >> action)
  liftIO . atomically $ modifyTVar' store (H.insert k (tid, v))
  setDebugSessionId k
  logInfo $ BL8.pack $ "Registered new debug session: " <> unpack k
----------------------------------------------------------------------------
getDebugSession :: Adaptor a a
getDebugSession = do
  (_, _, app) <- getDebugSessionWithThreadIdAndSessionId
  pure app
----------------------------------------------------------------------------
getDebugSessionWithThreadIdAndSessionId :: Adaptor app (SessionId, ThreadId, app)
getDebugSessionWithThreadIdAndSessionId = do
  sessionId <- getDebugSessionId
  appStore <- liftIO . readTVarIO =<< getAppStore
  case H.lookup sessionId appStore of
    Nothing -> do
      -- appNotFound sessionId
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
  (sessionId, tid, app) <- getDebugSessionWithThreadIdAndSessionId
  store <- getAppStore
  liftIO $ do
    killThread tid
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
  seqNum        <- getNextSeqNum
    -- if messageType == MessageTypeEvent
    --   then getRequestSeqNum
    --   else 
  address       <- getAddress
  requestSeqNum <- getRequestSeqNum

  -- Additional fields are required to be set for 'response' or 'reverse_request' messages.
  when (messageType == MessageTypeResponse) (setField "request_seq" requestSeqNum)
  when (messageType `elem` [MessageTypeResponse, MessageTypeRequest]) (setField "command" cmd)

  -- "seq" and "type" must be set for all protocol messages
  setField "type" messageType
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
  logDebug SENT ("\n" <> encodePretty evt)
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

resetNextScopeId :: Adaptor app ()
resetNextScopeId = modify' $ \s -> s { currentScopeId = 0 }
----------------------------------------------------------------------------
-- | Note: this `Int` should act as if it were an unsigned 31-bit integer (0, 2^31).
resetVariableId :: Adaptor app ()
resetVariableId = modify' $ \s -> s { currentVariableId = 0 }

addVariable :: ScopeId -> Variable -> Adaptor app ()
addVariable scopeId var = do
  frameId <- getCurrentFrameId
  varId <- getNextVariableId
  let
    val = I.singleton frameId (I.singleton scopeId (I.singleton varId var))
    combine = I.unionWith (I.unionWith (<>))
  modify' $ \s -> s { variablesMap = variablesMap s `combine` val }

getVariables :: Adaptor app [Variable]
getVariables = do
  frameId <- getCurrentFrameId
  scopeId <- getCurrentScopeId
  varMap <- gets variablesMap
  pure $ fromMaybe [] $ do
    scopeMap <- I.lookup frameId varMap
    varMap' <- I.lookup scopeId scopeMap
    pure (I.elems varMap')

getVariableReferencesMap :: Adaptor app VariableReferences
getVariableReferencesMap = gets variablesMap

getAllVars :: Adaptor app [Variable]
getAllVars = do
  varMap <- gets variablesMap
  pure (I.elems =<< I.elems =<< I.elems varMap)

-- | Invoked when a StepEvent has occurred
resetObjectLifetimes :: Adaptor app ()
resetObjectLifetimes = do
  modify' $ \s -> s
    { variablesMap = mempty
    , currentFrameId = 0
    , currentScopeId = 0
    , currentVariableId = 0
    }

-- | Sets the current StackFrame ID.
-- Each new StackFrame resets the Scope ID counter.
setCurrentFrameId :: FrameId -> Adaptor app ()
setCurrentFrameId frameId = modify' $ \s ->
  s { currentFrameId = frameId
    , currentScopeId = 0
    }

getCurrentFrameId :: Adaptor app FrameId
getCurrentFrameId = gets currentFrameId

-- | If a new scope is being set we reset the frameId
setCurrentScopeId :: ScopeId -> Adaptor app ()
setCurrentScopeId scopeId = modify' $ \s ->
  s { currentScopeId = scopeId
    , currentVariableId = 0
    }

----------------------------------------------------------------------------
-- | Note: this `Int` should act as if it were an unsigned 31-bit integer (0, 2^31).
-- Whenever we are incrementing the scope ID it is always in the context
-- of populating the scopes for a current stack frame.
--
-- Each new Scope resets the variable ID counter.
--
getNextScopeId :: Adaptor app Int
getNextScopeId = do
  modify' $ \s -> s
    { currentScopeId = currentScopeId s + 1
    , currentVariableId = 0
    }
  gets currentScopeId

getCurrentScopeId :: Adaptor app ScopeId
getCurrentScopeId = gets currentScopeId

setCurrentVariableId :: VariableId -> Adaptor app ()
setCurrentVariableId variableId = modify' $ \s -> s { currentVariableId = variableId }

getCurrentVariableId :: Adaptor app VariableId
getCurrentVariableId = gets currentVariableId

getNextVariableId :: Adaptor app VariableId
getNextVariableId = do
  modify' $ \s -> s { currentVariableId = currentVariableId s + 1 }
  gets currentVariableId

getNextSourceReferenceId :: Adaptor app SourceId
getNextSourceReferenceId = do
  modify' $ \s -> s { currentSourceReferenceId = currentSourceReferenceId s + 1 }
  gets currentSourceReferenceId

setSourceReferenceId :: Adaptor app SourceId
setSourceReferenceId = do
  modify' $ \s -> s { currentSourceReferenceId = currentSourceReferenceId s + 1 }
  gets currentSourceReferenceId

-- | DMJ: TODO: don't use (I.!)
getSourcePathBySourceReferenceId :: SourceId -> Adaptor app SourcePath
getSourcePathBySourceReferenceId sourceId =
  (I.! sourceId) <$> gets sourceReferencesMap

addSourcePathBySourceReferenceId
  :: SourcePath
  -> SourceId
  -> Adaptor app ()
addSourcePathBySourceReferenceId path sourceId =
  modify' $ \s -> s
    { sourceReferencesMap = I.insert sourceId path (sourceReferencesMap s)
    }

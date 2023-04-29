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
----------------------------------------------------------------------------
module DAP.Adaptor
  ( debugMessage
  , sendSuccesfulEmptyResponse
  , sendSuccesfulResponse
  , sendSuccesfulEvent
  , getServerCapabilities
  , setBody
  ) where
----------------------------------------------------------------------------
import           Control.Concurrent.STM     ( atomically, readTVarIO, modifyTVar' )
import           Control.Monad              ( when )
import           Control.Monad.State        ( MonadIO(liftIO), gets, modify' )
import           Data.Aeson.Encode.Pretty   ( encodePretty )
import           Data.Aeson.Types           ( object, Key, KeyValue((.=)), ToJSON )
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
debugMessage :: BL8.ByteString -> AdaptorClient app ()
debugMessage msg = do
  shouldLog <- getDebugLogging
  liftIO
    $ when shouldLog
    $ withGlobalLock
    $ BL8.putStrLn msg
----------------------------------------------------------------------------
getDebugLogging :: AdaptorClient app Bool
getDebugLogging = gets (debugLogging . adaptorServerConfig)
----------------------------------------------------------------------------
sendMessage :: ToJSON event => SockAddr -> Handle -> event -> AdaptorClient app ()
sendMessage addr handle evt = do
  let msg = encodeBaseProtocolMessage evt
  shouldLog <- getDebugLogging
  debugMessage $ BL8.intercalate "\n"
    [ BL8.pack ("[SENT][" <> show addr <> "]")
    , encodePretty evt
    ]
  liftIO (BS.hPutStr handle msg)
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
  modify' $ \s -> s { seqNum = seqNum s + 1 }
  gets seqNum
----------------------------------------------------------------------------
getRequestSeqNum :: AdaptorClient app Seq
getRequestSeqNum = gets (requestSeqNum . request)
----------------------------------------------------------------------------
getSessionId :: AdaptorClient app (Maybe SessionId)
getSessionId = gets sessionId
----------------------------------------------------------------------------
registerNewSession :: SessionId -> app -> AdaptorClient app ()
registerNewSession k v = do
  store <- gets adaptorAppStore
  liftIO . atomically $ modifyTVar' store (H.insert k v)
----------------------------------------------------------------------------
withSession :: (app -> AdaptorClient app ()) -> AdaptorClient app ()
withSession continuation = do
  maybeSessionId <- getSessionId
  case maybeSessionId of
    Nothing -> sessionNotFound
    Just sessionId -> do
      appStore <- liftIO . readTVarIO =<< getAppStore
      maybe appNotFound continuation (H.lookup sessionId appStore)
    where
      appNotFound = pure ()
      sessionNotFound = pure ()
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
  sendMessage address handle payload

  -- Reset payload each time a send occurs
  resetAdaptorStatePayload
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

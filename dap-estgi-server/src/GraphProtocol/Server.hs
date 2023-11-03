{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module GraphProtocol.Server where

import Control.Monad
import           Network.Simple.TCP         ( serve, HostPreference(Host) )
import           Network.Socket             ( socketToHandle, withSocketsDo, SockAddr )
import           System.IO                  ( hClose, hSetNewlineMode, Handle, Newline(CRLF)
                                            , NewlineMode(NewlineMode, outputNL, inputNL)
                                            , IOMode(ReadWriteMode) )
import           Data.Aeson                 ( Value, (.=), ToJSON )
import qualified Data.Aeson                 as Aeson
import qualified Data.Aeson.KeyMap          as Aeson

import qualified Data.ByteString.Char8      as BS

import           Data.String.Conversions               (cs)
import           Data.Text                             ( Text )
import qualified Data.Map.Strict                       as Map
import           Data.Map.Strict                       ( Map )

import DAP
import DAP.Utils (encodeBaseProtocolMessage)
import Data.IORef
import System.IO.Unsafe

import Control.Concurrent.MVar
import Control.Concurrent.Chan.Unagi.Bounded as Unagi

data GraphServerConfig
  = GraphServerConfig
  { graphServerHost         :: String
  , graphServerPort         :: Int
  , graphServerDebugLogging :: Bool
  }

data GraphEvent
  = GraphEventShowValue Text
  deriving (Show, Eq, Ord)

data GraphChan
  = GraphChan
  { graphAsyncEventIn   :: InChan GraphEvent
  , graphAsyncEventOut  :: OutChan GraphEvent
  }
  deriving Eq

instance Show GraphChan where
  show _ = "GraphChan"

data GraphServerState
  = GraphServerState
  { gssHandle     :: Maybe Handle
  , gssGraphChan  :: Maybe GraphChan
  , gssConfig     :: GraphServerConfig
  }

{-
  DESIGN:
    currently only one dap session and one gephi session is supported
    i.e. vscode --- estgi-dap --- gephi
    multi session dap or multi session gephi is not supported

  use cases:
    debug one program
      1 vscode
      1 gephi
      1 estgi dap session / program
    debug multiple programs - not supported yet
-}

{-# NOINLINE graphServerStateIORef #-}
graphServerStateIORef :: IORef GraphServerState
graphServerStateIORef = unsafePerformIO $ newIORef $ error "uninitialized graph server"

registerGraphChan :: Text -> GraphChan -> IO ()
registerGraphChan _sessionId graphChan = do
    -------------------------------------------
    -- NOTE: only one dap session is supported
    -------------------------------------------
  modifyIORef' graphServerStateIORef $ \s@GraphServerState{..} -> s {gssGraphChan = Just graphChan}

sendGraphCommand :: ToJSON a => a -> IO ()
sendGraphCommand msg = do
  GraphServerState{..} <- readIORef graphServerStateIORef
  case gssHandle of
    Nothing -> when (graphServerDebugLogging gssConfig) $ putStrLn $ "no graph client, can not send graph command: " ++ cs (Aeson.encode msg)
    Just h  -> BS.hPut h $ encodeBaseProtocolMessage msg

sendGraphEvent :: GraphEvent -> IO ()
sendGraphEvent ev = do
  GraphServerState{..} <- readIORef graphServerStateIORef
  case gssGraphChan of
    Nothing             -> when (graphServerDebugLogging gssConfig) $ putStrLn $ "no dap session, can not send graph event: " ++ show ev
    Just GraphChan{..}  -> Unagi.writeChan graphAsyncEventIn ev

runGraphServer :: GraphServerConfig -> IO ()
runGraphServer serverConfig = withSocketsDo $ do
  let GraphServerConfig{..} = serverConfig
  writeIORef graphServerStateIORef GraphServerState
    { gssHandle     = Nothing
    , gssGraphChan  = Nothing
    , gssConfig     = serverConfig
    }
  when graphServerDebugLogging $ putStrLn ("Running GRAPH server on " <> show graphServerPort <> "...")
  serve (Host graphServerHost) (show graphServerPort) $ \(socket, address) -> do
    when graphServerDebugLogging $ do
      putStrLn $ "TCP connection established from " ++ show address
    handle <- socketToHandle socket ReadWriteMode
    hSetNewlineMode handle NewlineMode { inputNL = CRLF, outputNL = CRLF }
    -------------------------------------------
    -- NOTE: only one gephi client is supported
    -------------------------------------------
    modifyIORef' graphServerStateIORef $ \s -> s {gssHandle = Just handle}
    serviceClient handle -- `catch` exceptionHandler handle address debugLogging

serviceClient :: Handle -> IO ()
serviceClient handle = do
  nextRequest <- readPayload handle :: IO (Either String Value)
  print nextRequest
  case nextRequest of
    Left err -> do
      putStrLn $ "error: " ++ err
    Right (Aeson.Object json)
      | Just "showValue" <- Aeson.lookup "event" json
      , Just (Aeson.String nodeId) <- Aeson.lookup "nodeId" json
      -> do
        sendGraphEvent $ GraphEventShowValue nodeId
    Right json -> do
      putStrLn $ "unknown event: " ++ show nextRequest
  -- loop: serve the next request
  serviceClient handle

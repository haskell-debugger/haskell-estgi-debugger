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

import           Data.Text                             ( Text )
import qualified Data.Map.Strict                       as Map
import           Data.Map.Strict                       ( Map )

import DAP
import DAP.Utils (encodeBaseProtocolMessage)
import Data.IORef
import System.IO.Unsafe

import Control.Concurrent.MVar
import Control.Concurrent.Chan.Unagi.Bounded as Unagi

serverConfig0 = ServerConfig
  { host               = "0.0.0.0"
  , port               = 4721
  , serverCapabilities = defaultCapabilities
  , debugLogging       = True
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
  { gssHandle       :: Handle
  , gssGraphChanMap :: Map Text GraphChan
  }

emptyGraphServerState :: GraphServerState
emptyGraphServerState = GraphServerState
  { gssHandle       = error "missing gssHandle"
  , gssGraphChanMap = mempty
  }

{-# NOINLINE graphServerStateIORef #-}
graphServerStateIORef :: IORef GraphServerState
graphServerStateIORef = unsafePerformIO $ newIORef emptyGraphServerState

registerGraphChan :: Text -> GraphChan -> IO ()
registerGraphChan sessionId graphChan = do
  modifyIORef' graphServerStateIORef $ \s@GraphServerState{..} -> s {gssGraphChanMap = Map.insert sessionId graphChan gssGraphChanMap}

sendGraphCommand :: ToJSON a => a -> IO ()
sendGraphCommand msg = do
  GraphServerState{..} <- readIORef graphServerStateIORef
  BS.hPut gssHandle $ encodeBaseProtocolMessage msg

runGraphServer :: IO ()
runGraphServer = withSocketsDo $ do
  let ServerConfig{..} = serverConfig0
      serverConfig = serverConfig0
  when debugLogging $ putStrLn ("Running GRAPH server on " <> show port <> "...")
  serve (Host host) (show port) $ \(socket, address) -> do
    when debugLogging $ do
      putStrLn $ "TCP connection established from " ++ show address
    handle <- socketToHandle socket ReadWriteMode
    hSetNewlineMode handle NewlineMode { inputNL = CRLF, outputNL = CRLF }
    modifyIORef' graphServerStateIORef $ \s -> s {gssHandle = handle}
    serviceClient handle -- `catch` exceptionHandler handle address debugLogging

serviceClient :: Handle -> IO ()
serviceClient handle = do
  {-
    get session id from message
    lookup the communication channel based on session id
      if there is no match then report and error, or use the first session as a fallback
  -}
  nextRequest <- readPayload handle :: IO (Either String Value)
  print nextRequest
  case nextRequest of
    Left err -> do
      putStrLn $ "error: " ++ err
    Right (Aeson.Object json)
      | Just "showValue" <- Aeson.lookup "event" json
      , Just (Aeson.String nodeId) <- Aeson.lookup "nodeId" json
      -> do
        GraphServerState{..} <- readIORef graphServerStateIORef
        -- TODO: handle sessions correctly, select the right session
        forM_ (Map.elems gssGraphChanMap) $ \GraphChan{..} -> do
          Unagi.writeChan graphAsyncEventIn $ GraphEventShowValue nodeId
    Right json -> do
      putStrLn $ "unknown event: " ++ show nextRequest
  -- loop: serve the next request
  serviceClient handle

{-
  use cases:
    debug one program
      1 vscode
      1 gephi
      1 estgi dap session / program
    debug multiple programs
-}
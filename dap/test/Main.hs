-----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (C) 2023 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <djohnson.m@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
----------------------------------------------------------------------------
module Main
  ( main
  ) where
----------------------------------------------------------------------------
import           Control.Monad
import           Control.Concurrent
import qualified Data.HashMap.Strict        as H
import           Data.Aeson.Encode.Pretty
import           Data.Aeson.Types
import           Data.Aeson
import           Data.Aeson.KeyMap
import           Control.Concurrent.Async
import           Control.Concurrent
import           Control.Exception
import qualified Data.ByteString.Lazy.Char8 as BL8 ( hPutStrLn )
import           Network.Simple.TCP         hiding (send)
import           Network.Socket             (socketToHandle)
import           System.IO
import           Data.String.Conversions
import           Test.Hspec
----------------------------------------------------------------------------
import           DAP.Utils                  hiding (send)
import           DAP.Internal
import           DAP.Types
import           DAP.Event
import           DAP.Server
import           DAP.Response
----------------------------------------------------------------------------
main :: IO ()
main = withServer $
  hspec $ do
    describe "Should connect to the mock DAP server from a client" $ do

      it "Should increment sequence number properly" $ do
        withNewClient $ \handle -> do
          send handle
            [ "command" .= ("initialize" :: String)
            , "seq"     .= (1 :: Int)
            , "type"    .= ("request" :: String)
            ]
          shouldReceive handle
            [ "seq"         .= (2 :: Int)
            , "request_seq" .= (1 :: Int)
            ]

      it "Should connect / disconnect 100 clients" $ do
        replicateM_ 100 $
          withNewClient $ \handle -> do
            send handle
              [ "command" .= ("initialize" :: String)
              , "seq"     .= (1 :: Int)
              , "type"    .= ("request" :: String)
              ]
            shouldReceive handle
              [ "seq"         .= (2 :: Int)
              , "request_seq" .= (1 :: Int)
              ]

      it "Should perform req/resp. for initialize and receive initialized event" $ do
          withNewClient $ \handle -> do
            send handle
              [ "command" .= ("initialize" :: String)
              , "seq"     .= (1 :: Int)
              , "type"    .= ("request" :: String)
              ]
            shouldReceive handle
              [ "seq"         .= (2 :: Int)
              , "request_seq" .= (1 :: Int)
              , "command"     .= ("initialize" :: String)
              , "type"        .= ("response" :: String)
              ]
            shouldReceive handle
              [ "type"     .= ("event" :: String)
              , "event"    .= ("initialized" :: String)
              ]

      it "Should receive configuration done and stop event" $ do
          withNewClient $ \handle -> do
            send handle
              [ "command" .= ("configurationDone" :: String)
              , "seq"     .= (100 :: Int)
              , "type"    .= ("request" :: String)
              ]
            shouldReceive handle
              [ "seq"         .= (101 :: Int)
              , "request_seq" .= (100 :: Int)
              , "command"     .= ("configurationDone" :: String)
              , "type"        .= ("response" :: String)
              ]
            shouldReceive handle
              [ "type"     .= ("event" :: String)
              , "event"    .= ("stopped" :: String)
              ]

-- | Mock server communication, used in test runner
--
mockServerTalk
  :: Command
  -> Adaptor app ()
mockServerTalk CommandInitialize = do
  sendInitializeResponse
  sendInitializedEvent
mockServerTalk CommandConfigurationDone = do
  sendConfigurationDoneResponse
  sendStoppedEvent defaultStoppedEvent

-- | Sample port shared amongst client and server
--
testPort :: Int
testPort = 8001

-- | Sample host shared amongst client and server
--
testHost :: String
testHost = "localhost"

-- | Runs server in a thread, 'withAsync' ensures cleanup.
--
withServer :: IO () -> IO ()
withServer test = withAsync server (const test)
  where
    server = runDAPServer config mockServerTalk
    config = ServerConfig
      { host = testHost
      , port = testPort
      , serverCapabilities = defaultCapabilities
      , debugLogging = False
      }

-- | Spawns a new mock client that connects to the mock server.
--
withNewClient :: (Handle -> IO ()) -> IO ()
withNewClient continue = flip catch exceptionHandler $
  connect testHost (show testPort) $ \(socket, _) -> do
    handle <- socketToHandle socket ReadWriteMode
    hSetNewlineMode handle NewlineMode { inputNL = CRLF, outputNL = CRLF }
    continue handle `finally` hClose handle
      where
        exceptionHandler :: SomeException -> IO ()
        exceptionHandler _ = do
          threadDelay 100
          putStrLn "Retrying connection..."
          withNewClient continue

-- | Helper to send JSON payloads to the server
--
send :: Handle -> [Pair] -> IO ()
send handle message
  = BL8.hPutStrLn handle
  $ cs (encodeBaseProtocolMessage (object message))

-- | Helper to receive JSON payloads to the client
-- checks if 'Handle' returns a subset expected payload
--
shouldReceive
  :: Handle
  -- ^ Handle to receive bytes from
  -> [Pair]
  -- ^ Subset of JSON values that should be present in the payload
  -> IO ()
shouldReceive h expected = do
  let Object ex = object expected
  readPayload h >>= \case
    Left e -> fail e
    Right actual
      | toHashMapText ex `H.isSubmapOf` toHashMapText actual -> pure ()
      | otherwise -> encodePretty actual `shouldBe` encodePretty ex

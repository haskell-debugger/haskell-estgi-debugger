----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (C) 2023 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <djohnson.m@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
-- |
----------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE DerivingStrategies  #-}
----------------------------------------------------------------------------
module Main (main) where
----------------------------------------------------------------------------
import           Data.Aeson                 ( Value(Null) )
import           Data.Maybe                 ( fromMaybe )
import           System.Environment         ( lookupEnv )
import           Text.Read                  ( readMaybe )
import qualified Data.ByteString.Lazy.Char8 as BL8 ( pack )
----------------------------------------------------------------------------
import           DAP
----------------------------------------------------------------------------
-- | DAP entry point
-- Extracts configuration information from the environment
-- Opens a listen socket on a port (defaulting to '4711')
-- Converts the 'Socket' to a 'Handle' for convenience
main :: IO ()
main = do
  config <- getConfig
  runDAPServer config talk
----------------------------------------------------------------------------
-- | Fetch config from environment, fallback to sane defaults
getConfig :: IO ServerConfig
getConfig = do
  let
    hostDefault = "127.0.0.1"
    portDefault = 4711
  ServerConfig
    <$> do fromMaybe hostDefault <$> lookupEnv "DAP_HOST"
    <*> do fromMaybe portDefault . (readMaybe =<<) <$> do lookupEnv "DAP_PORT"
    <*> pure defaultCapabilities
    <*> pure True
----------------------------------------------------------------------------
-- | Main function where requests are received and Events + Responses are returned.
-- The core logic of communicating between the client <-> adaptor <-> debugger
-- is implemented in this function.
----------------------------------------------------------------------------
talk :: Command -> AdaptorClient app ()
----------------------------------------------------------------------------
-- | Register SessionId and initialize program in the 'AppStore'
talk CommandAttach =
  sendAttachResponse
----------------------------------------------------------------------------
talk CommandBreakpointLocations = sendBreakpointsLocationResponse []
----------------------------------------------------------------------------
talk CommandContinue = sendContinueResponse (ContinueResponse True)
----------------------------------------------------------------------------
talk CommandConfigurationDone = sendConfigurationDoneResponse
----------------------------------------------------------------------------
talk CommandDisconnect = sendDisconnectResponse
----------------------------------------------------------------------------
talk CommandInitialize = do
  sendInitializedResponse
  sendInitializedEvent
----------------------------------------------------------------------------
talk CommandLoadedSources = sendLoadedSourcesResponse []
----------------------------------------------------------------------------
talk CommandModules = do
  sendModulesResponse (ModulesResponse [] Nothing)
----------------------------------------------------------------------------
talk CommandPause = sendPauseResponse
----------------------------------------------------------------------------
talk CommandSetBreakpoints = sendSetBreakpointsResponse []
----------------------------------------------------------------------------
talk CommandSetDataBreakpoints = sendSetDataBreakpointsResponse []
----------------------------------------------------------------------------
talk CommandSetExceptionBreakpoints = sendSetExceptionBreakpointsResponse []
----------------------------------------------------------------------------
talk CommandSetFunctionBreakpoints = sendSetFunctionBreakpointsResponse []
----------------------------------------------------------------------------
talk CommandSetInstructionBreakpoints = sendSetInstructionBreakpointsResponse []
----------------------------------------------------------------------------
talk CommandStackTrace
  = sendStackTraceResponse
  $ flip  StackTraceResponse (Just 1)
  [ StackFrame 1 "frame" source 1 1 10 10 False (Just "pointer-ref") (Just (Right "module"))
      (Just PresentationHintNormal)
  ] where
      source :: Source
      source = Source
        (Just "Main.hs")
        "/Users/j22293/Desktop/external-stg-dap/external-stg-dap/exe/Main.hs"
        (Just 0)
        (Just SourcePresentationHintNormal)
        (Just "/Users/j22293/Desktop/external-stg-dap/external-stg-dap/exe/Main.hs")
        []
        Null
        []
----------------------------------------------------------------------------
talk CommandSource = sendSourceResponse (SourceResponse "file content" (Just "text/plain"))
----------------------------------------------------------------------------
talk CommandThreads
  = do sendThreadsResponse
         [ Thread 1 "main"
         ]
       sendStoppedEvent $
         StoppedEvent
           StoppedEventReasonPause
           (Just "starting")
           (Just 1)
           False
           (Just "starting now?")
           False
           []

----------------------------------------------------------------------------
talk command = debugMessage $ BL8.pack ("GOT command: " <> show command)
----------------------------------------------------------------------------

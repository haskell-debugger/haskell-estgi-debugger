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
import           Control.Exception                     hiding (catch)
import           Control.Monad.IO.Class                (liftIO)
import           Control.Exception.Lifted              (catch)
import           Control.Monad
import           Data.Aeson                            ( Value(Null), FromJSON )
import           Data.Text                             ( Text )
import           Data.Maybe                            ( fromMaybe )
import           GHC.Generics                          ( Generic )
import           System.Environment                    ( lookupEnv )
import           Text.Read                             ( readMaybe )
import qualified Data.ByteString.Lazy.Char8            as BL8 ( pack )
import qualified Control.Concurrent.Chan.Unagi.Bounded as Unagi
----------------------------------------------------------------------------
import           Stg.Interpreter
import           Stg.Interpreter.Debug
import           Stg.Interpreter.Base                  hiding (lookupEnv)
import           Stg.Interpreter.Debugger
import           Stg.Interpreter.Debugger.UI
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
-- | VSCode arguments are custom for attach
-- > "arguments": {
-- >      "__configurationTarget": 6,
-- >      "__sessionId": "6c0ba6f8-e478-4698-821e-356fc4a72c3d",
-- >      "name": "thing",
-- >      "program": "/home/dmjio/Desktop/stg-dap/test.fullpak",
-- >      "request": "attach",
-- >      "type": "dap-extension"
-- >  }
--
data AttachArgs
  = AttachArgs
  { __sessionId :: Text
    -- ^ SessionID from VSCode
  , program :: String
    -- ^ Path to .fullpak file
  } deriving stock (Show, Eq, Generic)
    deriving anyclass FromJSON
----------------------------------------------------------------------------
-- | ESTG specific state used with our AdaptorState
--
----------------------------------------------------------------------------
data ESTG
  = ESTG
  { inChan :: Unagi.InChan DebugCommand
  , outChan :: Unagi.OutChan DebugOutput
  , fullPakPath :: String
  }
----------------------------------------------------------------------------
-- | Intialize ESTG interpreter
----------------------------------------------------------------------------
initESTG :: AttachArgs -> AdaptorClient ESTG ()
initESTG AttachArgs {..} = do
  (dbgCmdI, dbgCmdO) <- liftIO (Unagi.newChan 100)
  (dbgOutI, dbgOutO) <- liftIO (Unagi.newChan 100)
  let dbgChan = DebuggerChan (dbgCmdO, dbgOutI)
  registerNewDebugSession __sessionId (ESTG dbgCmdI dbgOutO program) $ do
    (liftIO (loadAndRunProgram True True program [] dbgChan DbgStepByStep False))
       `catch`
          handleDebuggerExceptions
----------------------------------------------------------------------------
-- | Exception Handler
handleDebuggerExceptions :: SomeException -> AdaptorClient ESTG ()
handleDebuggerExceptions e = do
  logError $ BL8.pack ("Caught: " <> show e)
  sendTerminatedEvent (TerminatedEvent False)
----------------------------------------------------------------------------
-- | Main function where requests are received and Events + Responses are returned.
-- The core logic of communicating between the client <-> adaptor <-> debugger
-- is implemented in this function.
----------------------------------------------------------------------------
talk :: Command -> AdaptorClient ESTG ()
----------------------------------------------------------------------------
-- | Register SessionId and initialize program in the 'AppStore'
talk CommandAttach = do
  initESTG =<< getArguments
  sendAttachResponse
    where
      emitEvent :: DebugOutput -> AdaptorClient ESTG ()
      emitEvent cmd = logInfo $ BL8.pack (show cmd)
----------------------------------------------------------------------------
talk CommandBreakpointLocations = sendBreakpointLocationsResponse []
----------------------------------------------------------------------------
talk CommandContinue = sendContinueResponse (ContinueResponse True)
----------------------------------------------------------------------------
talk CommandConfigurationDone = do
  withDebugSession $ \ESTG {..} ->
    liftIO (Unagi.writeChan inChan CmdListClosures)
  sendConfigurationDoneResponse
----------------------------------------------------------------------------
talk CommandDisconnect = do
  destroyDebugSession
  sendExitedEvent (ExitedEvent 1)
  sendDisconnectResponse
----------------------------------------------------------------------------
talk CommandInitialize = do
  sendInitializeResponse
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
-- talk CommandStackTrace = do
--   sendStackTraceResponse resp
--   sendTerminatedEvent (TerminatedEvent False)
--   sendExitedEvent (ExitedEvent 0)
--     where
--       resp = StackTraceResponse
--         [ StackFrame 1 "frame" source 1 1 10 10 False
--             (Just "pointer-ref") (Just (Right "module"))(Just PresentationHintNormal)
--         ] (Just 1)

--       source :: Source
--       source = Source
--         (Just "cbits/main.c.out")
--         "/Users/j22293/Desktop/external-stg-dap/external-stg-dap/exe/Main.hs"
--         (Just 0)
--         (Just SourcePresentationHintNormal)
--         (Just "/Users/j22293/Desktop/external-stg-dap/external-stg-dap/exe/Main.hs")
--         []
--         Null
--         []
----------------------------------------------------------------------------
talk CommandSource = sendSourceResponse (SourceResponse "file content" (Just "text/plain"))
----------------------------------------------------------------------------
-- talk CommandThreads
--   = do sendThreadsResponse
--          [ Thread 1 "main"
--          ]
--        sendStoppedEvent $
--          StoppedEvent
--            StoppedEventReasonPause
--            (Just "starting")
--            (Just 1)
--            False
--            (Just "starting now?")
--            False
--            []

----------------------------------------------------------------------------
talk command = logInfo $ BL8.pack ("GOT command: " <> show command)
----------------------------------------------------------------------------

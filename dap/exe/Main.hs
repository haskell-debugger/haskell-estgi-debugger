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
import           Data.IORef
import qualified Data.IntMap.Strict                    as I
import qualified Data.Map.Strict                       as M
import           Control.Exception                     hiding (catch)
import           Control.Monad.IO.Class                (liftIO)
import           Control.Exception.Lifted              (catch)
import           Control.Monad
import           Data.Aeson                            ( Value(Null), FromJSON )
import           Data.Text                             ( Text )
import qualified Data.Text                             as T
import           Data.Maybe                            ( fromMaybe )
import           GHC.Generics                          ( Generic )
import           System.Environment                    ( lookupEnv )
import           Text.Read                             ( readMaybe )
import qualified Data.ByteString.Lazy.Char8            as BL8 ( pack, unpack, fromStrict )
import qualified Control.Concurrent.Chan.Unagi.Bounded as Unagi
----------------------------------------------------------------------------
import           Stg.Syntax                            hiding (sourceName)
import           Stg.Interpreter
import           Stg.Interpreter.Debug
import           Stg.Interpreter.Base                  hiding (lookupEnv)
import           Stg.Interpreter.Debugger
import           Stg.Interpreter.Debugger.UI
import           Stg.IO
import           Stg.Program
import           Data.Yaml
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
addNewScopeToFrame frameId ESTG {..} = do
  frameToScopes <- readIORef frameToScopeRef
  pure (frameToScopes I.! frameId)
  -- let updated = I.insertWith (+) frameId 1 frameToScopes
  -- writeIORef frameToScopeRef updated
  -- pure (updated I.! frameId)

-- addNewVarToScope scopeId ESTG {..} = do
--   scopesToVars <- readIORef scopeToVariableRef
--   let updated = I.insertWith (+) scopeId 1 frameToScopes
--   writeIORef frameToScopeRef updated
--   pure (updated I.! frameId)

data ESTG
  = ESTG
  { inChan :: Unagi.InChan DebugCommand
  , outChan :: Unagi.OutChan DebugOutput
  , fullPakPath :: String
  , frameToScopeRef :: IORef (I.IntMap Int)
  }
----------------------------------------------------------------------------
-- | Intialize ESTG interpreter
----------------------------------------------------------------------------
initESTG :: AttachArgs -> AdaptorClient ESTG ()
initESTG AttachArgs {..} = do
  -- 0) Set sessionId and Program path here
  -- 1) register session in attach (use DbgStepByStep)
  -- 2) set breakpoints in breakpoint configuration
  -- 3) in configuration done, change the status from DbgStepByStep to DbgRun, and wait until breakpoint is hit

  (dbgCmdI, dbgCmdO) <- liftIO (Unagi.newChan 100)
  (dbgOutI, dbgOutO) <- liftIO (Unagi.newChan 100)
  let dbgChan = DebuggerChan (dbgCmdO, dbgOutI)
  frameRef <- liftIO (newIORef scopes')
  registerNewDebugSession __sessionId (ESTG dbgCmdI dbgOutO program frameRef) $ do
    (liftIO (loadAndRunProgram True True program [] dbgChan DbgStepByStep False))
      `catch`
          handleDebuggerExceptions

scopes' = I.fromList (zip [0..100] [ 1000 .. 2000 ])
----------------------------------------------------------------------------
-- | Exception Handler
handleDebuggerExceptions :: SomeException -> AdaptorClient ESTG ()
handleDebuggerExceptions e = do
  logError $ BL8.pack ("Caught: " <> show e)
  sendTerminatedEvent (TerminatedEvent False)

sendStop =
  sendStoppedEvent $
      StoppedEvent
           StoppedEventReasonPause
           (Just "paused")
           (Just 0)
           False
           (Just "starting now?")
           False
           []

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
talk CommandContinue = do
--  NextArguments {..} <- getArguments
  withDebugSession $ \estg@ESTG {..} -> do
    liftIO $ do
       () <- Unagi.writeChan inChan CmdContinue
       pure ()
  sendContinueResponse (ContinueResponse True)
----------------------------------------------------------------------------
talk CommandConfigurationDone = do
  withDebugSession $ \ESTG {..} -> do
    liftIO (Unagi.writeChan inChan CmdCurrentClosure)
    msg <- liftIO (Unagi.readChan outChan)
    logInfo $ BL8.pack (show msg)

  sendConfigurationDoneResponse
  sendStop

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
talk CommandLoadedSources = do
  modules <- getModuleListFromFullPak
  sendLoadedSourcesResponse
    [ sourceSTG { sourceName = Just (T.pack moduleName)
                , sourceSourceReference = Just index
                , sourcePath = Just (T.pack moduleName)
                }
    | (index, moduleName) <- zip [1000..] modules
    ]

----------------------------------------------------------------------------
talk CommandModules = do
  sendModulesResponse (ModulesResponse [] Nothing)
----------------------------------------------------------------------------
talk CommandPause = sendPauseResponse
----------------------------------------------------------------------------
talk CommandSetBreakpoints = sendSetBreakpointsResponse []
  -- where
  --   bp = Breakpoint
  --     { breakpointId = Just 0
  --     , breakpointVerified = True
  --     , breakpointMessage = Just "stop here"
  --     , breakpointSource = Nothing
  --     , breakpointLine = Just 0
  --     , breakpointColumn = Just 0
  --     , breakpointEndLine = Just 0
  --     , breakpointEndColumn = Just 0
  --     , breakpointInstructionReference = "instruction"
  --     , breakpointOffset = Just 0
  --     }
----------------------------------------------------------------------------
talk CommandSetDataBreakpoints = sendSetDataBreakpointsResponse []
----------------------------------------------------------------------------
talk CommandSetExceptionBreakpoints = sendSetExceptionBreakpointsResponse []
----------------------------------------------------------------------------
talk CommandSetFunctionBreakpoints = sendSetFunctionBreakpointsResponse []
----------------------------------------------------------------------------
talk CommandSetInstructionBreakpoints = sendSetInstructionBreakpointsResponse []
----------------------------------------------------------------------------
talk CommandStackTrace = do
  withDebugSession $ \ESTG {..} -> do
    result <- liftIO $ do
       () <- Unagi.writeChan inChan (CmdInternal "get-current-thread-state")
       Unagi.readChan outChan
    case result of
      DbgOutThreadReport threadId threadState closureName closureAddr _ -> do
        let threadLabel = maybe "<unknown>" (BL8.unpack . BL8.fromStrict) (tsLabel threadState)
            frames = convertThreadStateToStackTraceResponse threadState
        sendStackTraceResponse $ StackTraceResponse frames (Just (length frames))
  where
    convertThreadStateToStackTraceResponse
      :: ThreadState
      -> [StackFrame]
    convertThreadStateToStackTraceResponse ThreadState {..} =
      [ StackFrame
        { stackFrameId = stackId
        , stackFrameName = T.pack (showStackCont stackCont)
        , stackFrameSource = Just sourceSTG
        , stackFrameLine = 0
        , stackFrameColumn = 0
        , stackFrameEndLine = 1
        , stackFrameEndColumn = 5
        , stackFrameCanRestart = False
        , stackFrameInstructionPointerReference = Nothing
        , stackFrameModuleId = Just (Right "ModuleID")
        , stackFramePresentationHint = Nothing
        }
      | (stackId, stackCont) <- zip [0..] tsStack
      ]

    -- source = Source
    --   (Just "Main.hs")
    --   Nothing
    --   (Just 1)
    --   (Just SourcePresentationHintNormal)
    --   (Just "Haskell source code")
    --   Nothing
    --   Nothing
    --   Nothing

    -- sourceNameFromStackContinuation :: StackContinuation -> Maybe ModuleName
    -- sourceNameFromStackContinuation (CaseOf _ _ _ Binder {..} _ _) = Just (show binderModule)
    -- sourceNameFromStackContinuation _ = Nothing
----------------------------------------------------------------------------
talk CommandSource = do
  SourceArguments {..} <- getArguments
  string <- pure (show sourceArgumentsSourceReference <> " test source")
  -- liftIO (readFile "./exe/Main.hs")
  sendSourceResponse (SourceResponse (T.pack string) Nothing)
----------------------------------------------------------------------------
talk CommandThreads
  = do withDebugSession $ \ESTG {..} -> do
         result <- liftIO $ do
            () <- Unagi.writeChan inChan (CmdInternal "get-current-thread-state")
            Unagi.readChan outChan
         case result of
           DbgOutThreadReport threadId threadState closureName closureAddr _ -> do
             let threadLabel = maybe "<unknown>" (BL8.unpack . BL8.fromStrict) (tsLabel threadState)
             sendThreadsResponse
               [ Thread threadId $ T.pack (show threadId <> " " <> threadLabel)
               ]
           _ -> do logInfo $ BL8.pack ("Unexpected Message received: " <> show result)

----------------------------------------------------------------------------
talk CommandScopes = do
  ScopesArguments {..} <- getArguments
  withDebugSession $ \estg@ESTG {..} -> do
    result <- liftIO $ do
       () <- Unagi.writeChan inChan (CmdInternal "get-current-thread-state")
       Unagi.readChan outChan
    case result of
      DbgOutThreadReport threadId threadState closureName closureAddr _ -> do
        let threadLabel = maybe "<unknown>" (BL8.unpack . BL8.fromStrict) (tsLabel threadState)
            frameId = scopesArgumentsFrameId
        logInfo $ BL8.pack ("FRAME ID: " <> show frameId)
        scopeId <- liftIO (addNewScopeToFrame frameId estg)
        let frame = tsStack threadState !! frameId
            scope count = Scope
                  { scopeName = "defaultScopeName"
                  , presentationHint = Just ScopePresentationHintLocals
                  , variablesReference = scopeId
                  , namedVariables = Just count
                  , indexedVariables = Nothing
                  , expensive = False
                  , source = Nothing
                  , line = Just 0
                  , column = Just 0
                  , endLine = Just 0
                  , endColumn = Just 0
                  }
        let updatedScopes =
              case frame of
                CaseOf _ _ env _ _ _ ->
                  [ (scope (M.size env)) { scopeName = "CaseOf" }
                  ]
                Update _ ->
                  [ (scope 1) { scopeName = "Update" }
                  ]
                Apply atoms ->
                  [ (scope (length atoms)) { scopeName = "Locals" }
                  ]
                Catch{} ->
                  [ (scope 3) { scopeName = "Catch" }
                  ]
                RestoreExMask{} ->
                  [ (scope 2) { scopeName = "RestoreExMask" }
                  ]
                RunScheduler{} ->
                  [ (scope 1) { scopeName = "RunScheduler" }
                  ]
                Atomically{} ->
                  [ (scope 1) { scopeName = "Atomically" }
                  ]
                CatchRetry{} ->
                  [ (scope 3) { scopeName = "CatchRetry" }
                  ]
                CatchSTM{} ->
                  [ (scope 2) { scopeName = "CatchSTM" }
                  ]
                DataToTagOp ->
                  [ (scope 0) { scopeName = "DataToTagOp" }
                  ]
                RaiseOp{} ->
                  [ (scope 1) { scopeName = "RaiseOp" }
                  ]
                KeepAlive{} ->
                  [ (scope 1) { scopeName = "KeepAlive" }
                  ]
                DebugFrame{} ->
                  [ (scope 1) { scopeName = "DebugFrame" }
                  ]

        sendScopesResponse (ScopesResponse updatedScopes)

----------------------------------------------------------------------------
talk CommandVariables = do
  VariablesArguments {..} <- getArguments
  withDebugSession $ \estg@ESTG {..} -> do
    result <- liftIO $ do
       () <- Unagi.writeChan inChan (CmdInternal "get-current-thread-state")
       Unagi.readChan outChan
    case result of
      DbgOutThreadReport threadId threadState closureName closureAddr _ -> do
        let threadLabel = maybe "<unknown>" (BL8.unpack . BL8.fromStrict) (tsLabel threadState)
            varRefId = variablesArgumentsVariablesReference
        logInfo $ BL8.pack ("Var Ref ID: " <> show varRefId)
        let frameId = varRefId - 1000
        let frame = tsStack threadState !! frameId
        let defaultVar
              = Variable
                { variableName = "variable"
                , variableValue = "value"
                , variableType = Just "int"
                , variablePresentationHint = Nothing
                , variableEvaluateName = Nothing
                , variableVariablesReference = 0
                , variableNamedVariables = Nothing
                , variableIndexedVariables = Nothing
                , variableMemoryReference = Nothing
                }
        let vars =
              case frame of
                CaseOf _ _ env _ _ _ ->
                  [ defaultVar { variableName = T.pack (show k)
                               , variableValue = T.pack (show v)
                               , variableType =
                                   case unId k of
                                     Binder {..} -> Just $ T.pack (show binderType)
                               }
                  | (k,v) <- M.toList env
                  ]
                Update _ ->
                  [ defaultVar { variableName = "Update" }
                  ]
                Apply atoms ->
                  [ defaultVar { variableName = "arg: " <> T.pack (show index)
                               , variableValue = T.pack (show atom)
                               }
                  | (index, atom) <- zip [0..] atoms
                  ]
                Catch{} ->
                  [ defaultVar { variableName = "Catch" }
                  ]
                RestoreExMask{} ->
                  [ defaultVar { variableName = "RestoreExMask" }
                  ]
                RunScheduler reason ->
                  [ defaultVar
                    { variableName = "ScheduleReason"
                    , variableValue = T.pack (show reason)
                    }
                  ]
                Atomically{} ->
                  [ defaultVar { variableName = "Atomically" }
                  ]
                CatchRetry{} ->
                  [ defaultVar { variableName = "CatchRetry" }
                  ]
                CatchSTM{} ->
                  [ defaultVar { variableName = "CatchSTM" }
                  ]
                DataToTagOp ->
                  [ defaultVar { variableName = "DataToTagOp" }
                  ]
                RaiseOp{} ->
                  [ defaultVar { variableName = "RaiseOp" }
                  ]
                KeepAlive{} ->
                  [ defaultVar{ variableName = "KeepAlive" }
                  ]
                DebugFrame (RestoreProgramPoint maybeId pp) ->
                  [ defaultVar { variableName = "arg0: Maybe Id"
                               , variableValue = T.pack (show maybeId)
                               }
                  , defaultVar { variableName = "arg1: ProgramPoint"
                               , variableValue = T.pack (show pp)
                               }
                  ]
        sendVariablesResponse (VariablesResponse vars)

talk CommandNext = do
  NextArguments {..} <- getArguments
  withDebugSession $ \estg@ESTG {..} -> do
    liftIO $ do
       () <- Unagi.writeChan inChan CmdStep
       pure ()
    sendStoppedBecauseOfStep

----------------------------------------------------------------------------
talk cmd = logInfo $ BL8.pack ("GOT cmd " <> show cmd)
----------------------------------------------------------------------------

-- + on watch causes "CommandEvaluate"
sendStoppedBecauseOfStep =
  sendStoppedEvent $
      StoppedEvent
           StoppedEventReasonStep
           (Just "stepped")
           (Just 0)
           False
           (Just "stepped event")
           False
           []


-- right click - set value - [127.0.0.1:49599][INFO][GOT cmd CommandSetVariable]
-- right click - copy value - [127.0.0.1:49599][INFO][GOT cmd CommandEvaluate]
-- save breakpoints from breakpoints request into AdaptrClient set, set them on the interpreter after configuration done (not attach)

sourceSTG
  = Source
      (Just "Main.stg")
      (Just "./exe/stg.txt")
      (Just 1)
      (Just SourcePresentationHintEmphasize)
      Nothing -- (Just "STG source code")
      (Just [ sourceHS, sourceCore ])
      Nothing
      Nothing

sourceHS
  = Source
      (Just "Main.hs")
      (Just "./exe/hs.txt")
      (Just 2)
      (Just SourcePresentationHintNormal)
      Nothing -- (Just "Haskell source code")
      Nothing
      Nothing
      Nothing

sourceCore
  = Source
      (Just "Main.cmm")
      (Just "./exe/cmm.txt")
      (Just 3)
      (Just SourcePresentationHintDeemphasize)
      Nothing -- (Just "Core source code")
      Nothing
      Nothing
      Nothing


-- readModpakS :: FilePath -> String -> (BS.ByteString -> a) -> IO a

-- readGhcStgApp :: FilePath -> IO GhcStgApp
-- readGhcStgApp = Y.decodeFileThrow


getModuleListFromFullPak :: AdaptorClient ESTG [String]
getModuleListFromFullPak = do
  ESTG {..} <- getDebugSession
  bytes <- liftIO (readModpakS fullPakPath  "app.ghc_stgapp" id)
  GhcStgApp {..} <- liftIO (decodeThrow bytes)

  let
      unitModules :: [String]
      unitModules = concat
        [ unitExposedModules ++ unitHiddenModules
        | UnitLinkerInfo {..} <- appLibDeps
        ]

  pure (unitModules <> appModules)

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
import           Codec.Archive.Zip                     (withArchive, unEntrySelector, getEntries)
import qualified Data.Set                              as Set
import           Control.Arrow
import           Data.IORef
import           Control.Exception                     hiding (catch)
import           Control.Monad.IO.Class                (liftIO)
import           Control.Exception.Lifted              (catch)
import           Control.Monad
import           Data.Aeson                            ( Value(Null), FromJSON )
import qualified Data.IntMap.Strict                    as I
import qualified Data.Map.Strict                       as M
import qualified Data.Text.Encoding                    as T
import           Data.Text                             ( Text )
import qualified Data.Text                             as T
import           Data.Typeable                         ( typeOf )
import           Data.Maybe                            ( fromMaybe )
import           Data.List                             ( sortOn )
import           GHC.Generics                          ( Generic )
import           System.Environment                    ( lookupEnv )
import           System.FilePath                       ((</>), takeDirectory, takeExtension)
import           Text.Read                             ( readMaybe )
import qualified Data.ByteString.Lazy.Char8            as BL8 ( pack, unpack, fromStrict, toStrict )
import qualified Control.Concurrent.Chan.Unagi.Bounded as Unagi
----------------------------------------------------------------------------
import           Stg.Syntax                            hiding (sourceName, Scope)
import           Stg.IRLocation
import           Stg.Pretty
import           Stg.Interpreter
import           Stg.Interpreter.Debug
import           Stg.Interpreter.Base                  hiding (lookupEnv, getCurrentThreadState, getCurrentThreadState)
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
-- | External STG Interpreter application internal state
data ESTG
  = ESTG
  { inChan :: Unagi.InChan DebugCommand
  , outChan :: Unagi.OutChan DebugOutput
  , fullPakPath :: String
  }
----------------------------------------------------------------------------
-- | Intialize ESTG interpreter
----------------------------------------------------------------------------
initESTG :: AttachArgs -> Adaptor ESTG ()
initESTG AttachArgs {..} = do
  (dbgCmdI, dbgCmdO) <- liftIO (Unagi.newChan 100)
  (dbgOutI, dbgOutO) <- liftIO (Unagi.newChan 100)
  let dbgChan = DebuggerChan (dbgCmdO, dbgOutI)
  frameRef <- liftIO (newIORef scopes')
  registerNewDebugSession __sessionId (ESTG dbgCmdI dbgOutO program)
    $ flip catch handleDebuggerExceptions
    $ do liftIO $ loadAndRunProgram True True program [] dbgChan DbgStepByStep False defaultDebugSettings
         -- ^ doesn't seem to return here
         sendTerminatedEvent (TerminatedEvent False)
         sendExitedEvent (ExitedEvent 0)

scopes' = I.fromList (zip [0..100] [ 1000 .. 2000 ])
----------------------------------------------------------------------------
-- | Exception Handler
handleDebuggerExceptions :: SomeException -> Adaptor ESTG ()
handleDebuggerExceptions e | Just ThreadKilled <- fromException e = do
  sendTerminatedEvent (TerminatedEvent False)
  sendExitedEvent (ExitedEvent 0)
handleDebuggerExceptions e = do
  logError $ BL8.pack ("Caught: " <> show e)
  sendTerminatedEvent (TerminatedEvent False)
  sendExitedEvent (ExitedEvent 1)

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
talk :: Command -> Adaptor ESTG ()
----------------------------------------------------------------------------
-- | Register SessionId and initialize program in the 'AppStore'
talk CommandAttach = do
  initESTG =<< getArguments
  sendAttachResponse
    where
      emitEvent :: DebugOutput -> Adaptor ESTG ()
      emitEvent cmd = logInfo $ BL8.pack (show cmd)
----------------------------------------------------------------------------
talk CommandContinue = do
  ESTG {..} <- getDebugSession
  send CmdContinue
  sendContinueResponse (ContinueResponse True)

  ESTG {..} <- getDebugSession
  _ <- liftIO $ Unagi.readChan outChan
  resetObjectLifetimes
  sendStoppedEvent defaultStoppedEvent
    { stoppedEventReason = StoppedEventReasonBreakpoint
    , stoppedEventThreadId = Just 0
    }
{-
data StoppedEvent
  = StoppedEvent
  { stoppedEventReason :: StoppedEventReason
  , stoppedEventDescription :: Maybe Text
  , stoppedEventThreadId :: Maybe Int
  , stoppedEventPreserveFocusHint :: Bool
  , stoppedEventText :: Maybe Text
  , stoppedEventAllThreadsStopped :: Bool
  , stoppedEventHitBreakpointIds :: [Int]
-}

----------------------------------------------------------------------------
talk CommandConfigurationDone = do
  sendConfigurationDoneResponse
  sendStop
----------------------------------------------------------------------------
talk CommandDisconnect = do
  destroyDebugSession
  sendExitedEvent (ExitedEvent 0)
  sendDisconnectResponse
----------------------------------------------------------------------------
talk CommandInitialize = do
  sendInitializeResponse
  sendInitializedEvent
----------------------------------------------------------------------------
talk CommandLoadedSources = do
  moduleInfos <- getModuleListFromFullPak
  sendLoadedSourcesResponse =<< do
    forM moduleInfos $ \ModuleInfo {..} -> do
      hsSourceReferenceId   <- getNextSourceReferenceId
      coreSourceReferenceId <- getNextSourceReferenceId
      estgSourceReferenceId  <- getNextSourceReferenceId
      stgSourceReferenceId  <- getNextSourceReferenceId
      cmmSourceReferenceId  <- getNextSourceReferenceId
      asmSourceReferenceId  <- getNextSourceReferenceId
      cStubSourceReferenceId  <- getNextSourceReferenceId
      hStubSourceReferenceId  <- getNextSourceReferenceId

      let
        hsModuleName   = T.pack (qualifiedModuleName </> "module.hs")
        coreModuleName = T.pack (qualifiedModuleName </> "module.ghccore")
        estgModuleName = T.pack (qualifiedModuleName </> "module.stgbin")
        stgModuleName  = T.pack (qualifiedModuleName </> "module.ghcstg")
        cmmModuleName  = T.pack (qualifiedModuleName </> "module.cmm")
        asmModuleName  = T.pack (qualifiedModuleName </> "module.s")
        cStubModuleName = T.pack (qualifiedModuleName </> "module_stub.c")
        hStubModuleName = T.pack (qualifiedModuleName </> "module_stub.h")

      addSourcePathBySourceReferenceId hsModuleName hsSourceReferenceId
      addSourcePathBySourceReferenceId coreModuleName coreSourceReferenceId
      addSourcePathBySourceReferenceId stgModuleName stgSourceReferenceId
      addSourcePathBySourceReferenceId estgModuleName estgSourceReferenceId
      addSourcePathBySourceReferenceId cmmModuleName cmmSourceReferenceId
      addSourcePathBySourceReferenceId asmModuleName asmSourceReferenceId

      when cStub (addSourcePathBySourceReferenceId cStubModuleName cStubSourceReferenceId)
      when hStub (addSourcePathBySourceReferenceId hStubModuleName hStubSourceReferenceId)

      let
        hsSource =
          defaultSource
          { sourceName = Just hsModuleName
          , sourceSourceReference = Just hsSourceReferenceId
          , sourcePath = Just hsModuleName
          }
        coreSource =
          defaultSource
          { sourceName = Just coreModuleName
          , sourceSourceReference = Just coreSourceReferenceId
          , sourcePath = Just coreModuleName
          }
        stgSource =
          defaultSource
          { sourceName = Just stgModuleName
          , sourceSourceReference = Just stgSourceReferenceId
          , sourcePath = Just stgModuleName
          }
        cStubSource =
          defaultSource
          { sourceName = Just cStubModuleName
          , sourceSourceReference = Just cStubSourceReferenceId
          , sourcePath = Just cStubModuleName
          }
        hStubSource =
          defaultSource
          { sourceName = Just hStubModuleName
          , sourceSourceReference = Just hStubSourceReferenceId
          , sourcePath = Just hStubModuleName
          }
        estgSource =
          defaultSource
          { sourceName = Just estgModuleName
          , sourceSourceReference = Just estgSourceReferenceId
          , sourcePath = Just estgModuleName
          , sourceSources = Just $
              [ hsSource
              , coreSource
              , cmmSource
              , asmSource
              , stgSource
              ] ++
              [ cStubSource
              | cStub
              ] ++
              [ hStubSource
              | hStub
              ]
          }
        cmmSource =
          defaultSource
          { sourceName = Just cmmModuleName
          , sourceSourceReference = Just cmmSourceReferenceId
          , sourcePath = Just cmmModuleName
          }
        asmSource =
          defaultSource
          { sourceName = Just asmModuleName
          , sourceSourceReference = Just asmSourceReferenceId
          , sourcePath = Just asmModuleName
          }

      pure estgSource

----------------------------------------------------------------------------
talk CommandModules = do
  sendModulesResponse (ModulesResponse [] Nothing)
----------------------------------------------------------------------------
talk CommandPause = sendPauseResponse
----------------------------------------------------------------------------
-- {
--     "arguments": {
--         "breakpoints": [
--             {
--                 "line": 1
--             }
--         ],
--         "lines": [
--             1
--         ],
--         "source": {
--             "name": "Main",
--             "path": "Main",
--             "sourceReference": 277
--         },
--         "sourceModified": false
--     },
--     "command": "setBreakpoints",
--     "seq": 18,
--     "type": "request"
-- }
talk CommandSetBreakpoints = do
  SetBreakpointsArguments {..} <- getArguments
  let maybeSourceRef = sourceSourceReference setBreakpointsArgumentsSource
  case (setBreakpointsArgumentsBreakpoints, maybeSourceRef) of
    (Just sourceBreakpoints, Just sourceRef) -> do
      (_sourceCodeText, locations) <- getSourceFromFullPak sourceRef
      breakpoints <- forM sourceBreakpoints $ \SourceBreakpoint{..} -> do
        -- filter all relevant ranges
        {-
          SP_RhsClosureExpr
        -}
        let onlySupported = \case
              SP_RhsClosureExpr{} -> True
              _ -> False
        let relevantLocations = filter (onlySupported . fst) $ case sourceBreakpointColumn of
              Nothing ->
                [ p
                | p@(_,((startRow, startCol), (endRow, endCol))) <- locations
                , startRow <= sourceBreakpointLine
                , endRow >= sourceBreakpointLine
                ]
              Just col  ->
                [ p
                | p@(_,((startRow, startCol), (endRow, endCol))) <- locations
                , startRow <= sourceBreakpointLine
                , endRow >= sourceBreakpointLine
                , startCol <= col
                , endCol >= col
                ]
        liftIO $ putStrLn $ "relevantLocations: " ++ show relevantLocations
        -- use the first location found
        case sortOn snd relevantLocations of
          (stgPoint@(SP_RhsClosureExpr closureName), ((startRow, startCol), (endRow, endCol))) : _ -> do
            let hitCount = fromMaybe 0 (sourceBreakpointHitCondition >>= readMaybe . T.unpack) :: Int
            send (CmdAddBreakpoint closureName hitCount)
            pure $ defaultBreakpoint
              { breakpointVerified  = True
              , breakpointSource    = Just setBreakpointsArgumentsSource
              , breakpointLine      = Just startRow
              , breakpointColumn    = Just startCol
              , breakpointEndLine   = Just endRow
              , breakpointEndColumn = Just endCol
              }
          _ ->
            pure $ defaultBreakpoint
              { breakpointVerified  = False
              , breakpointSource    = Just setBreakpointsArgumentsSource
              , breakpointMessage   = Just "no code found"
              }
      sendSetBreakpointsResponse breakpoints
    _ ->
      sendSetBreakpointsResponse []
----------------------------------------------------------------------------
talk CommandStackTrace = do
  stackFrames <- interpreterFramesToDAPFrames <$> getCurrentThreadStack
  let totalFrames = Just (length stackFrames)
  sendStackTraceResponse StackTraceResponse {..}
    -- INFO: (Just stackFramesLength) = totalFrames, this can be used to enforce pagination (if Nothing)
    -- this is good if the stack is extremely large. We'll need to add an internal counter here
    -- to support that if so.
    where
      interpreterFramesToDAPFrames
        :: [StackContinuation]
        -> [StackFrame]
      interpreterFramesToDAPFrames stackFrames =
        [ defaultStackFrame
          { stackFrameId = stackId
          , stackFrameName = T.pack (showStackCont stackCont)
          , stackFrameSource = Nothing -- TODO: Get Frame to Source mapping (e.g. Source of GHC.IO.FD)
            -- ^ Create internal mapping
          , stackFrameModuleId = Nothing -- TODO: Get Frame to Module mapping (e.g. GHC.IO.FD)
          }
        | (stackId, stackCont) <- zip [0..] stackFrames
        ]
----------------------------------------------------------------------------
talk CommandSource = do
  SourceArguments {..} <- getArguments -- save path of fullpak in state
  (source, _locations) <- getSourceFromFullPak sourceArgumentsSourceReference
  sendSourceResponse (SourceResponse source Nothing)
----------------------------------------------------------------------------
talk CommandThreads = do
  (threadId, threadState) <- getCurrentThreadStateWithId
  let
    threadLabel = mkThreadLabel threadState
    threadName = T.pack (show threadId <> " " <> threadLabel)
  sendThreadsResponse [ Thread {..} ]
----------------------------------------------------------------------------
talk CommandScopes = do
  ScopesArguments {..} <- getArguments
  setCurrentFrameId scopesArgumentsFrameId
  (threadState, stackFrame) <- getCurrentThreadStateAndStackFrame
  scopes <- generateScopes stackFrame
  sendScopesResponse (ScopesResponse scopes)
----------------------------------------------------------------------------
talk CommandVariables = do
  VariablesArguments {..} <- getArguments
  threadState <- getCurrentThreadState
  setCurrentScopeId variablesArgumentsVariablesReference
  variables <- getVariables
  sendVariablesResponse (VariablesResponse variables)
----------------------------------------------------------------------------
talk CommandNext = do
  NextArguments {..} <- getArguments
  send CmdStep
  resetObjectLifetimes
  sendStoppedEvent defaultStoppedEvent
    { stoppedEventReason = StoppedEventReasonStep
    , stoppedEventText = Just "Stepping..."
    , stoppedEventThreadId = Just 0
    }
----------------------------------------------------------------------------
talk CommandBreakpointLocations       = sendBreakpointLocationsResponse []
talk CommandSetDataBreakpoints        = sendSetDataBreakpointsResponse []
talk CommandSetExceptionBreakpoints   = sendSetExceptionBreakpointsResponse []
talk CommandSetFunctionBreakpoints    = sendSetFunctionBreakpointsResponse []
talk CommandSetInstructionBreakpoints = sendSetInstructionBreakpointsResponse []
----------------------------------------------------------------------------
talk cmd = logInfo $ BL8.pack ("GOT cmd " <> show cmd)
----------------------------------------------------------------------------
data ModuleInfo
  = ModuleInfo
  { cStub :: Bool
    -- ^ If stubs.c is included in the .fullpak for this module
  , hStub :: Bool
    -- ^ If stubs.h is included in the .fullpak for this module
  , qualifiedModuleName :: String
    -- ^ Fully qualified module name
  }
----------------------------------------------------------------------------
-- | Retrieves list of modules from .fullpak file
-- TODO: Check if stubs file exists, if so, return it.
getModuleListFromFullPak :: Adaptor ESTG [ModuleInfo]
getModuleListFromFullPak = do
  ESTG {..} <- getDebugSession
  let appName = "app.ghc_stgapp"
  bytes <- liftIO (readModpakL fullPakPath appName id)
  rawEntries <- fmap unEntrySelector . M.keys <$> withArchive fullPakPath getEntries
  let folderNames = Set.fromList (takeDirectory <$> rawEntries)
  GhcStgApp {..} <- liftIO (decodeThrow (BL8.toStrict bytes))
  let
    unitModules :: [String]
    unitModules = concat
      [ unitExposedModules ++ unitHiddenModules
      | UnitLinkerInfo {..} <- appLibDeps
      ]


  let rawEntriesSet = Set.fromList rawEntries

  pure
    [ ModuleInfo
      { cStub = (moduleName </> "module_stub.c") `Set.member` rawEntriesSet
      , hStub = (moduleName </> "module_stub.h") `Set.member` rawEntriesSet
      , qualifiedModuleName = moduleName
      }
    | moduleName <- unitModules <> appModules
    , moduleName `Set.member` folderNames
    ]
----------------------------------------------------------------------------
-- | Retrieves list of modules from .fullpak file
getSourceFromFullPak :: SourceId -> Adaptor ESTG (Text, [(StgPoint, SrcRange)])
getSourceFromFullPak sourceId = do
  sourcePath <- T.unpack <$> getSourcePathBySourceReferenceId sourceId
  ESTG {..} <- getDebugSession
  liftIO $
    if takeExtension sourcePath == ".stgbin"
      then do
        m <- readModpakL fullPakPath sourcePath decodeStgbin
        pure . pShow $ pprModule m
      else do
        ir <- readModpakS fullPakPath sourcePath T.decodeUtf8
        pure (ir, [])
----------------------------------------------------------------------------
-- | Asynchronous call to Debugger, sends message, does not wait for response
send
  :: DebugCommand
  -> Adaptor ESTG ()
send cmd = do
  ESTG {..} <- getDebugSession
  liftIO (Unagi.writeChan inChan cmd)
----------------------------------------------------------------------------
-- | Synchronous call to Debugger, sends message and waits for response
sendAndWait :: DebugCommand -> Adaptor ESTG DebugOutput
sendAndWait cmd = do
  ESTG {..} <- getDebugSession
  liftIO $ do
    Unagi.writeChan inChan cmd
    Unagi.readChan outChan
----------------------------------------------------------------------------
-- | Receive Thread Report
-- Fails if anything but 'DbgOutThreadReport' is returned
getCurrentThreadStateWithId :: Adaptor ESTG (ThreadId, ThreadState)
getCurrentThreadStateWithId = do
  sendAndWait (CmdInternal "get-current-thread-state") >>= \case
    DbgOutThreadReport tid tstate _ _ _ ->
      pure (tid, tstate)
    otherMessage -> do
      let errorMsg
            = concat
            [ "Unexpected Message received from interpreter: "
            , show otherMessage
            ]
      logInfo (BL8.pack errorMsg)
      sendError (ErrorMessage (T.pack errorMsg)) Nothing

----------------------------------------------------------------------------
-- | Receive Thread State
getCurrentThreadState :: Adaptor ESTG ThreadState
getCurrentThreadState = snd <$> getCurrentThreadStateWithId

----------------------------------------------------------------------------
-- | Receive Thread State
getCurrentThreadStateAndStackFrame :: Adaptor ESTG (ThreadState, StackContinuation)
getCurrentThreadStateAndStackFrame = do
  ts <- snd <$> getCurrentThreadStateWithId
  frameId <- getCurrentFrameId
  pure (ts, tsStack ts !! frameId)

type ThreadId = Int
type FrameId  = Int
----------------------------------------------------------------------------
-- | Receive Thread Report
-- Fails if anything but 'DbgOutThreadReport' is returned
getCurrentThreadStack :: Adaptor ESTG [StackContinuation]
getCurrentThreadStack = tsStack <$> getCurrentThreadState

----------------------------------------------------------------------------
-- | Receive Thread Report
-- Retrieve a specific stack frame
-- TODO: Error handling
getStackFrame :: Adaptor ESTG StackContinuation
getStackFrame = do
  frameId <- getCurrentFrameId
  (!! frameId) <$> getCurrentThreadStack

----------------------------------------------------------------------------
mkThreadLabel :: ThreadState -> String
mkThreadLabel = maybe "<unknown>" (BL8.unpack . BL8.fromStrict) . tsLabel

----------------------------------------------------------------------------
generateScopes
  :: StackContinuation
  -- ^ The stack instruction that we're generating Scopes for
  -> Adaptor app [Scope]
  -- ^ List of Scopes for this StackFrame
generateScopes stackCont@(CaseOf _ _ env _ _ _) = do
  frameId <- getCurrentFrameId
  localsScopeIndex <- getNextScopeId
  forM_ (M.toList env) $ \(k,v) -> do
    -- DMJ: for now everything is local.
    -- Inspect StaticOrigin to put things top-level, or as arguments, where applicable
    addVariable localsScopeIndex defaultVariable
      { variableName = T.pack (show k)
      , variableValue = T.pack (show v)
      , variableType =
          case unId k of
            Binder {..} -> Just $ T.pack (show binderType)
      }
  pure [ localScope localsScopeIndex ]
    where
      localScope localsScopeIndex
        = defaultScope
        { scopeName = "Locals: " <> T.pack (showStackCont stackCont)
        , scopePresentationHint = Just ScopePresentationHintLocals
        , scopeVariablesReference = localsScopeIndex
        , scopeNamedVariables = Just (M.size env)
        }
generateScopes stackCont@(Update addr) = do
  frameId <- getCurrentFrameId
  localsScopeIndex <- getNextScopeId
  addVariable localsScopeIndex defaultVariable
    { variableName = "Address"
    , variableValue = T.pack (show addr)
    , variableType = Just "Ptr"
    }
  pure [ localScope localsScopeIndex ]
    where
      localScope localsScopeIndex
        = defaultScope
        { scopeName = "Locals: " <> T.pack (showStackCont stackCont)
        , scopePresentationHint = Just ScopePresentationHintLocals
        , scopeVariablesReference = localsScopeIndex
        , scopeNamedVariables = Just 1
        }
generateScopes stackCont@(Apply atoms) = do
  frameId <- getCurrentFrameId
  localsScopeIndex <- getNextScopeId
  forM_ atoms $ \atom -> do
    addVariable localsScopeIndex defaultVariable
      { variableName = "Atom" -- DMJ: Write a pretty printing Atom function (see below)
      , variableValue = T.pack (show atom)
      , variableType = Just (T.pack (show atom))
      }
  pure [ localScope localsScopeIndex ]
    where
      localScope localsScopeIndex
        = defaultScope
        { scopeName = "Locals: " <> T.pack (showStackCont stackCont)
        , scopePresentationHint = Just ScopePresentationHintLocals
        , scopeVariablesReference = localsScopeIndex
        , scopeNamedVariables = Just (length atoms)
        }
generateScopes stackCont@(Catch atom blockAsync interruptible) = do
  frameId <- getCurrentFrameId
  localsScopeIndex <- getNextScopeId
  addVariable localsScopeIndex defaultVariable
    { variableName = "Atom"
    , variableValue = T.pack (show atom)
    , variableType = Just (T.pack (show atom))
    }
  addVariable localsScopeIndex defaultVariable
    { variableName = "BlockAsyncExceptions"
    , variableValue = T.pack (show blockAsync)
    , variableType = Just $ T.pack $ show (typeOf blockAsync)
    }
  addVariable localsScopeIndex defaultVariable
    { variableName = "Interruptible"
    , variableValue = T.pack (show interruptible)
    , variableType = Just $ T.pack $ show (typeOf interruptible)
    }
  pure [ localScope localsScopeIndex ]
    where
      localScope localsScopeIndex
        = defaultScope
        { scopeName = "Locals: " <> T.pack (showStackCont stackCont)
        , scopePresentationHint = Just ScopePresentationHintLocals
        , scopeVariablesReference = localsScopeIndex
        , scopeNamedVariables = Just 3
        }
generateScopes stackCont@(RestoreExMask blockAsync interruptible) = do
  frameId <- getCurrentFrameId
  localsScopeIndex <- getNextScopeId
  addVariable localsScopeIndex defaultVariable
    { variableName = "BlockAsyncExceptions"
    , variableValue = T.pack (show blockAsync)
    , variableType = Just $ T.pack $ show (typeOf blockAsync)
    }
  addVariable localsScopeIndex defaultVariable
    { variableName = "Interruptible"
    , variableValue = T.pack (show interruptible)
    , variableType = Just $ T.pack $ show (typeOf interruptible)
    }
  pure [ localScope localsScopeIndex ]
    where
      localScope localsScopeIndex
        = defaultScope
        { scopeName = "Locals: " <> T.pack (showStackCont stackCont)
        , scopePresentationHint = Just ScopePresentationHintLocals
        , scopeVariablesReference = localsScopeIndex
        , scopeNamedVariables = Just 2
        }
generateScopes stackCont@(RunScheduler reason) = do
  frameId <- getCurrentFrameId
  localsScopeIndex <- getNextScopeId
  addVariable localsScopeIndex defaultVariable
    { variableName = T.pack $ show (typeOf reason)
    , variableValue = T.pack (show reason)
    , variableType = Just $ T.pack $ show (typeOf reason)
    }
  pure [ localScope localsScopeIndex ]
    where
      localScope localsScopeIndex
        = defaultScope
        { scopeName = "Locals: " <> T.pack (showStackCont stackCont)
        , scopePresentationHint = Just ScopePresentationHintLocals
        , scopeVariablesReference = localsScopeIndex
        , scopeNamedVariables = Just 1
        }
generateScopes stackCont@(Atomically atom) = do
  frameId <- getCurrentFrameId
  localsScopeIndex <- getNextScopeId
  addVariable localsScopeIndex defaultVariable
    { variableName = "Atom"
    , variableValue = T.pack (show atom)
    , variableType = Just (T.pack (show atom))
    }
  pure [ localScope localsScopeIndex ]
    where
      localScope localsScopeIndex
        = defaultScope
        { scopeName = "Locals: " <> T.pack (showStackCont stackCont)
        , scopePresentationHint = Just ScopePresentationHintLocals
        , scopeVariablesReference = localsScopeIndex
        , scopeNamedVariables = Just 1
        }
generateScopes stackCont@(CatchRetry atom1 atom2 interruptible) = do
  frameId <- getCurrentFrameId
  localsScopeIndex <- getNextScopeId
  addVariable localsScopeIndex defaultVariable
    { variableName = "STM1"
    , variableValue = T.pack (show atom1)
    , variableType = Just (T.pack (show (typeOf atom1)))
    }
  addVariable localsScopeIndex defaultVariable
    { variableName = "STM2"
    , variableValue = T.pack (show atom2)
    , variableType = Just (T.pack (show (typeOf atom2)))
    }
  addVariable localsScopeIndex defaultVariable
    { variableName = "altCode"
    , variableValue = T.pack (show interruptible)
    , variableType = Just (T.pack (show (typeOf interruptible)))
    }
  pure [ localScope localsScopeIndex ]
    where
      localScope localsScopeIndex
        = defaultScope
        { scopeName = "Locals: " <> T.pack (showStackCont stackCont)
        , scopePresentationHint = Just ScopePresentationHintLocals
        , scopeVariablesReference = localsScopeIndex
        , scopeNamedVariables = Just 3
        }
generateScopes stackCont@(CatchSTM atom1 atom2) = do
  frameId <- getCurrentFrameId
  localsScopeIndex <- getNextScopeId
  addVariable localsScopeIndex defaultVariable
    { variableName = "STM"
    , variableValue = T.pack (show atom1)
    , variableType = Just (T.pack (show (typeOf atom1)))
    }
  addVariable localsScopeIndex defaultVariable
    { variableName = "Exception Handler"
    , variableValue = T.pack (show atom2)
    , variableType = Just (T.pack (show (typeOf atom2)))
    }
  pure [ localScope localsScopeIndex ]
    where
      localScope localsScopeIndex
        = defaultScope
        { scopeName = "Locals: " <> T.pack (showStackCont stackCont)
        , scopePresentationHint = Just ScopePresentationHintLocals
        , scopeVariablesReference = localsScopeIndex
        , scopeNamedVariables = Just 2
        }
generateScopes stackCont@DataToTagOp = do
  frameId <- getCurrentFrameId
  localsScopeIndex <- getNextScopeId
  pure [ localScope localsScopeIndex ]
    where
      localScope localsScopeIndex
        = defaultScope
        { scopeName = "Locals: " <> T.pack (showStackCont stackCont)
        , scopePresentationHint = Just ScopePresentationHintLocals
        , scopeVariablesReference = localsScopeIndex
        , scopeNamedVariables = Just 0
        }
generateScopes stackCont@(RaiseOp atom) = do
  frameId <- getCurrentFrameId
  localsScopeIndex <- getNextScopeId
  addVariable localsScopeIndex defaultVariable
    { variableName = "Atom"
    , variableValue = T.pack (show atom)
    , variableType = Just (T.pack (show (typeOf atom)))
    }
  pure [ localScope localsScopeIndex ]
    where
      localScope localsScopeIndex
        = defaultScope
        { scopeName = "Locals: " <> T.pack (showStackCont stackCont)
        , scopePresentationHint = Just ScopePresentationHintLocals
        , scopeVariablesReference = localsScopeIndex
        , scopeNamedVariables = Just 1
        }
generateScopes stackCont@(KeepAlive atom) = do
  frameId <- getCurrentFrameId
  localsScopeIndex <- getNextScopeId
  addVariable localsScopeIndex defaultVariable
    { variableName = "Atom"
    , variableValue = T.pack (show atom)
    , variableType = Just (T.pack (show (typeOf atom)))
    }
  pure [ localScope localsScopeIndex ]
    where
      localScope localsScopeIndex
        = defaultScope
        { scopeName = "Locals: " <> T.pack (showStackCont stackCont)
        , scopePresentationHint = Just ScopePresentationHintLocals
        , scopeVariablesReference = localsScopeIndex
        , scopeNamedVariables = Just 1
        }
generateScopes stackCont@(DebugFrame atom) = do
  frameId <- getCurrentFrameId
  localsScopeIndex <- getNextScopeId
  addVariable localsScopeIndex defaultVariable
    { variableName = "Atom"
    , variableValue = T.pack (show atom)
    , variableType = Just (T.pack (show (typeOf atom)))
    }
  pure [ localScope localsScopeIndex ]
    where
      localScope localsScopeIndex
        = defaultScope
        { scopeName = "Locals: " <> T.pack (showStackCont stackCont)
        , scopePresentationHint = Just ScopePresentationHintLocals
        , scopeVariablesReference = localsScopeIndex
        , scopeNamedVariables = Just 1
        }

-- getAtomNameAndValueAndType = \case
--   HeapPtr addr -> ("HeapPtr", showText addr)
--   Literal (LitChar char) -> (T.pack [char], "LitChar")
--   Literal (LitString bytes) -> (T.decodeUtf8 bytes, "LitString")

  -- Literal LitNullAddr -> ("0x0", showType LitNullAddr)
  -- Literal (LitString bytes) -> ("0x0", showType LitNullAddr)
--   | Void
--   | PtrAtom !PtrOrigin {-# UNPACK #-}(GHC.Ptr.Ptr GHC.Word.Word8)
--   | IntAtom {-# UNPACK #-}Int
--   | WordAtom {-# UNPACK #-}Word
--   | FloatAtom {-# UNPACK #-}Float
--   | DoubleAtom {-# UNPACK #-}Double
--   | MVar {-# UNPACK #-}Int
--   | MutVar {-# UNPACK #-}Int
--   | TVar {-# UNPACK #-}Int
--   | Stg.Interpreter.Base.Array !ArrIdx
--   | MutableArray !ArrIdx
--   | SmallArray !SmallArrIdx
--   | SmallMutableArray !SmallArrIdx
--   | ArrayArray !ArrayArrIdx
--   | MutableArrayArray !ArrayArrIdx
--   | ByteArray !ByteArrayIdx
--   | MutableByteArray !ByteArrayIdx
--   | WeakPointer {-# UNPACK #-}Int
--   | StableName {-# UNPACK #-}Int
--   | ThreadId {-# UNPACK #-}Int
--   | LiftedUndefined
--   | Rubbish (allocated but /not/ initialized)
    -- where
    --   showType = Just . T.pack . show . typeOf

-- + on watch causes "CommandEvaluate"
-- right click - set value - [127.0.0.1:49599][INFO][GOT cmd CommandSetVariable]
-- right click - copy value - [127.0.0.1:49599][INFO][GOT cmd CommandEvaluate]
-- save breakpoints from breakpoints request into AdaptrClient set, set them on the interpreter after configuration done (not attach)

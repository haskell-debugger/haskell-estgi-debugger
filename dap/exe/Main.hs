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
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE LambdaCase          #-}
----------------------------------------------------------------------------
module Main (main) where
----------------------------------------------------------------------------
import           Data.List
import           Data.String.Conversions               (cs)
import           Text.PrettyPrint.ANSI.Leijen          (pretty, plain)
import           Codec.Archive.Zip                     (withArchive, unEntrySelector, getEntries)
import           Data.IntSet                           ( IntSet )
import qualified Data.IntSet                           as IntSet
import qualified Data.Set                              as Set
import           Control.Arrow
import           Data.IORef
import           Control.Exception                     hiding (catch)
import           Control.Monad.IO.Class                (liftIO)
import           Control.Exception.Lifted              (catch)
import           Control.Monad.State.Strict            ( gets )
import           Control.Monad
import           Data.Aeson                            ( Value(Null), FromJSON )
import qualified Data.IntMap.Strict                    as I
import qualified Data.Map.Strict                       as M
import           Data.Map.Strict                       ( Map )
import qualified Data.Text.Encoding                    as T
import           Data.Text                             ( Text )
import qualified Data.Text                             as T
import           Data.Typeable                         ( typeOf )
import           Data.Maybe                            ( fromMaybe, catMaybes )
import           Data.List                             ( sortOn )
import           GHC.Generics                          ( Generic )
import           System.Environment                    ( lookupEnv )
import           System.FilePath                       ((</>), takeDirectory, takeExtension, dropExtension, splitFileName)
import           Text.Read                             ( readMaybe )
import qualified Data.ByteString.Lazy.Char8            as BL8 ( pack, unpack, fromStrict, toStrict )
import qualified Control.Concurrent.Chan.Unagi.Bounded as Unagi
import           Control.Concurrent.MVar               ( MVar )
import qualified Control.Concurrent.MVar               as MVar
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
import           Data.Yaml                             hiding (Array)
----------------------------------------------------------------------------
import           DAP                                   hiding (send)
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
  { debuggerChan  :: DebuggerChan
  , fullPakPath   :: String
  , breakpointMap :: Map StgPoint IntSet
  }
----------------------------------------------------------------------------
-- | Intialize ESTG interpreter
----------------------------------------------------------------------------
initESTG :: AttachArgs -> Adaptor ESTG ()
initESTG AttachArgs {..} = do
  (dbgAsyncI, dbgAsyncO) <- liftIO (Unagi.newChan 100)
  dbgRequestMVar <- liftIO MVar.newEmptyMVar
  dbgResponseMVar <- liftIO MVar.newEmptyMVar
  let dbgChan = DebuggerChan
        { dbgSyncRequest    = dbgRequestMVar
        , dbgSyncResponse   = dbgResponseMVar
        , dbgAsyncEventIn   = dbgAsyncI
        , dbgAsyncEventOut  = dbgAsyncO
        }
      estg = ESTG
        { debuggerChan  = dbgChan
        , fullPakPath   = program
        , breakpointMap = mempty
        }
  flip catch handleDebuggerExceptions
    $ registerNewDebugSession __sessionId estg
      (loadAndRunProgram True True program [] dbgChan DbgStepByStep False defaultDebugSettings)
      (handleDebugEvents dbgChan)

----------------------------------------------------------------------------
-- | Debug Event Handler
handleDebugEvents :: DebuggerChan -> (Adaptor ESTG () -> IO ()) -> IO ()
handleDebugEvents DebuggerChan{..} withAdaptor = forever $ do
  dbgEvent <- liftIO (Unagi.readChan dbgAsyncEventOut)
  withAdaptor $ do
    ESTG {..} <- getDebugSession
    let sendEvent ev = sendSuccesfulEvent ev . setBody
    case dbgEvent of
      DbgEventStopped -> do
        resetObjectLifetimes
        sendEvent EventTypeStopped $ object
          [ "reason"             .= String "step"
          , "allThreadsStopped"  .= True
          ]

      DbgEventHitBreakpoint bkpName -> do
        resetObjectLifetimes
        sendEvent EventTypeStopped . object $
          [ "reason"             .= String "breakpoint"
          , "allThreadsStopped"  .= True
          ] ++
          catMaybes
            [ do
                idSet <- M.lookup (SP_RhsClosureExpr bkpName) breakpointMap
                Just ("hitBreakpointIds" .= idSet)
            ]

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

pathToName path =
  case splitFileName (cs path) of
    (init -> moduleName, takeExtension -> ".ghccore") ->
      cs (moduleName <> ".core")
    (init -> moduleName, takeExtension -> ".stgbin") ->
      cs (moduleName <> ".stg")
    (init -> moduleName, takeExtension -> ext) ->
      cs (moduleName <> ext)

----------------------------------------------------------------------------
-- | Clears the currently known breakpoint set
clearBreakpoints :: Adaptor ESTG ()
clearBreakpoints = do
  updateDebugSession $ \estg -> estg {breakpointMap = mempty}

----------------------------------------------------------------------------
-- | Adds new BreakpointId for a givent StgPoint
addNewBreakpoint :: StgPoint -> Adaptor ESTG BreakpointId
addNewBreakpoint stgPoint = do
  bkpId <- getNextBreakpointId
  updateDebugSession $ \estg@ESTG{..} -> estg {breakpointMap = M.insertWith mappend stgPoint (IntSet.singleton bkpId) breakpointMap}
  pure bkpId

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
  sendAndWait CmdContinue
  sendContinueResponse (ContinueResponse True)

----------------------------------------------------------------------------
talk CommandConfigurationDone = do
  sendConfigurationDoneResponse
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
      hsSourceReferenceId       <- getNextSourceReferenceId
      coreSourceReferenceId     <- getNextSourceReferenceId
      estgSourceReferenceId     <- getNextSourceReferenceId
      stgSourceReferenceId      <- getNextSourceReferenceId
      cmmSourceReferenceId      <- getNextSourceReferenceId
      asmSourceReferenceId      <- getNextSourceReferenceId
      cStubSourceReferenceId    <- getNextSourceReferenceId
      hStubSourceReferenceId    <- getNextSourceReferenceId
      cSourcesSourceReferenceId <- getNextSourceReferenceId

      let
        hsModuleName        = cs (qualifiedModuleName </> "module.hs")
        coreModuleName      = cs (qualifiedModuleName </> "module.ghccore")
        estgModuleName      = cs (qualifiedModuleName </> "module.stgbin")
        stgModuleName       = cs (qualifiedModuleName </> "module.ghcstg")
        cmmModuleName       = cs (qualifiedModuleName </> "module.cmm")
        asmModuleName       = cs (qualifiedModuleName </> "module.s")
        cStubModuleName     = cs (qualifiedModuleName </> "module_stub.c")
        hStubModuleName     = cs (qualifiedModuleName </> "module_stub.h")
        cSourcesModulesName = cs qualifiedModuleName

      addSourcePathBySourceReferenceId hsModuleName hsSourceReferenceId
      addSourcePathBySourceReferenceId coreModuleName coreSourceReferenceId
      addSourcePathBySourceReferenceId stgModuleName stgSourceReferenceId
      addSourcePathBySourceReferenceId estgModuleName estgSourceReferenceId
      addSourcePathBySourceReferenceId cmmModuleName cmmSourceReferenceId
      addSourcePathBySourceReferenceId asmModuleName asmSourceReferenceId
      addSourcePathBySourceReferenceId cSourcesModulesName cSourcesSourceReferenceId

      when cStub (addSourcePathBySourceReferenceId cStubModuleName cStubSourceReferenceId)
      when hStub (addSourcePathBySourceReferenceId hStubModuleName hStubSourceReferenceId)

      let
        hsSource =
          defaultSource
          { sourceName = Just (pathToName hsModuleName)
          , sourceSourceReference = Just hsSourceReferenceId
          , sourcePath = Just hsModuleName
          }
        coreSource =
          defaultSource
          { sourceName = Just (pathToName coreModuleName)
          , sourceSourceReference = Just coreSourceReferenceId
          , sourcePath = Just coreModuleName
          }
        stgSource =
          defaultSource
          { sourceName = Just (pathToName stgModuleName)
          , sourceSourceReference = Just stgSourceReferenceId
          , sourcePath = Just stgModuleName
          }
        cStubSource =
          defaultSource
          { sourceName = Just (pathToName cStubModuleName)
          , sourceSourceReference = Just cStubSourceReferenceId
          , sourcePath = Just cStubModuleName
          }
        hStubSource =
          defaultSource
          { sourceName = Just (pathToName hStubModuleName)
          , sourceSourceReference = Just hStubSourceReferenceId
          , sourcePath = Just hStubModuleName
          }
        cmmSource =
          defaultSource
          { sourceName            = Just (pathToName cmmModuleName)
          , sourceSourceReference = Just cmmSourceReferenceId
          , sourcePath            = Just cmmModuleName
          }
        asmSource =
          defaultSource
          { sourceName            = Just (pathToName asmModuleName)
          , sourceSourceReference = Just asmSourceReferenceId
          , sourcePath            = Just asmModuleName
          }
        estgSource =
          defaultSource
          { sourceName = Just (pathToName estgModuleName)
          , sourceSourceReference = Just estgSourceReferenceId
          , sourcePath = Just (pathToName estgModuleName)
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
        cbitsSource =
          defaultSource
          { sourceName            = Just (cs qualifiedModuleName)
          , sourceSourceReference = Just cSourcesSourceReferenceId
          , sourcePath            = Just (cs qualifiedModuleName)
          }
      pure $
        if isCSource
          then cbitsSource
          else estgSource
----------------------------------------------------------------------------
talk CommandModules = do
  sendModulesResponse (ModulesResponse [] Nothing)
----------------------------------------------------------------------------
talk CommandPause = sendPauseResponse
----------------------------------------------------------------------------
talk CommandSetBreakpoints = do
  SetBreakpointsArguments {..} <- getArguments
  let maybeSourceRef = sourceSourceReference setBreakpointsArgumentsSource
  clearBreakpoints
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
            sendAndWait (CmdAddBreakpoint closureName hitCount)
            bkpId <- addNewBreakpoint stgPoint
            pure $ defaultBreakpoint
              { breakpointVerified  = True
              , breakpointSource    = Just setBreakpointsArgumentsSource
              , breakpointLine      = Just startRow
              , breakpointColumn    = Just startCol
              , breakpointEndLine   = Just endRow
              , breakpointEndColumn = Just endCol
              , breakpointId        = Just bkpId
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
  sendThreadsResponse [ Thread {..}
                      , defaultThread { threadName = "other-thread", threadId = 1 }
                      ]
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
  sendAndWait CmdStep
  pure ()
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
  , isCSource :: Bool
    -- ^ Is a C source file located in c-sources
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

    cbitsSources :: [String]
    cbitsSources =
      [ entry
      | entry <- rawEntries
      , "cbits-source" `isPrefixOf` entry
      ]

  let rawEntriesSet = Set.fromList rawEntries

  pure
    [ ModuleInfo
      { cStub = (moduleName </> "module_stub.c") `Set.member` rawEntriesSet
      , hStub = (moduleName </> "module_stub.h") `Set.member` rawEntriesSet
      , qualifiedModuleName = moduleName
      , ..
      }
    | moduleName <- unitModules <> appModules <> cbitsSources
    , let isCSource = "cbits-source" `isPrefixOf` moduleName
    , moduleName `Set.member` folderNames || isCSource
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
-- | Synchronous call to Debugger, sends message and waits for response
sendAndWait :: DebugCommand -> Adaptor ESTG DebugOutput
sendAndWait cmd = do
  ESTG {..} <- getDebugSession
  let DebuggerChan{..} = debuggerChan
  liftIO $ do
    MVar.putMVar dbgSyncRequest cmd
    MVar.takeMVar dbgSyncResponse
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
  forM_ (M.toList env) $ \(k, (_, atom)) -> do
    -- DMJ: for now everything is local.
    -- Inspect StaticOrigin to put things top-level, or as arguments, where applicable
    let (variableType, variableValue) = getAtomTypeAndValue atom
    addVariable localsScopeIndex defaultVariable
      { variableName =
          case unId k of
            Binder {..} -> cs binderName
      , variableValue = cs variableValue
      , variableType = Just (cs variableType)

      }
  pure [ localScope localsScopeIndex ]
    where
      localScope localsScopeIndex
        = defaultScope
        { scopeName = "Locals"
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
    let (variableType, variableValue) = getAtomTypeAndValue atom
    addVariable localsScopeIndex defaultVariable
      { variableName = "Closure argument"
      , variableValue = cs variableValue
      , variableType = Just (cs variableType)
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
  let (variableType, variableValue) = getAtomTypeAndValue atom
  addVariable localsScopeIndex defaultVariable
    { variableName = "Exception Handler"
    , variableValue = cs variableValue
    , variableType = Just (cs variableType)
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
        { scopeName = "Locals"
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
        { scopeName = "Locals"
        , scopePresentationHint = Just ScopePresentationHintLocals
        , scopeVariablesReference = localsScopeIndex
        , scopeNamedVariables = Just 2
        }
generateScopes stackCont@(RunScheduler reason) = do
  frameId <- getCurrentFrameId
  localsScopeIndex <- getNextScopeId
  addVariable localsScopeIndex defaultVariable
    { variableName = "Schedule Reason"
    , variableValue = showScheduleReason reason
    }
  pure [ localScope localsScopeIndex ]
    where
      localScope localsScopeIndex
        = defaultScope
        { scopeName = "Locals"
        , scopePresentationHint = Just ScopePresentationHintLocals
        , scopeVariablesReference = localsScopeIndex
        , scopeNamedVariables = Just 1
        }
      showScheduleReason :: ScheduleReason            -> Text
      showScheduleReason SR_ThreadFinished            = "Thread Finished"
      showScheduleReason SR_ThreadFinishedFFICallback = "Thread Finished FFI Callback"
      showScheduleReason SR_ThreadBlocked             = "Thread Blocked"
      showScheduleReason SR_ThreadYield               = "Thread Yield"
      showScheduleReason SR_ThreadFinishedMain        = "Thread Finished Main"

generateScopes stackCont@(Atomically atom) = do
  frameId <- getCurrentFrameId
  localsScopeIndex <- getNextScopeId
  let (variableType, variableValue) = getAtomTypeAndValue atom
  addVariable localsScopeIndex defaultVariable
    { variableName = "STM action"
    , variableValue = cs variableValue
    , variableType = Just (cs variableType)
    }
  pure [ localScope localsScopeIndex ]
    where
      localScope localsScopeIndex
        = defaultScope
        { scopeName = "Locals"
        , scopePresentationHint = Just ScopePresentationHintLocals
        , scopeVariablesReference = localsScopeIndex
        , scopeNamedVariables = Just 1
        }
generateScopes stackCont@(CatchRetry atom1 atom2 interruptible) = do
  frameId <- getCurrentFrameId
  localsScopeIndex <- getNextScopeId
  let (variableType1, variableValue1) = getAtomTypeAndValue atom1
  let (variableType2, variableValue2) = getAtomTypeAndValue atom2
  addVariable localsScopeIndex defaultVariable
    { variableName = "First STM action"
    , variableValue = cs variableValue1
    , variableType = Just (cs variableType1)
    }
  addVariable localsScopeIndex defaultVariable
    { variableName = "Second STM action"
    , variableValue = T.pack (show atom2)
    , variableType = Just (T.pack (show (typeOf atom2)))
    }
  addVariable localsScopeIndex defaultVariable
    { variableName = "Interruptible"
    , variableValue = T.pack (show interruptible)
    , variableType = Just "Bool"
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
generateScopes (CatchSTM atom1 atom2) = do
  frameId <- getCurrentFrameId
  localsScopeIndex <- getNextScopeId
  let (variableType1, variableValue1) = getAtomTypeAndValue atom1
  addVariable localsScopeIndex defaultVariable
    { variableName = "STM action"
    , variableValue = cs variableValue1
    , variableType = Just (cs variableValue1)
    }
  let (variableType2, variableValue2) = getAtomTypeAndValue atom2
  addVariable localsScopeIndex defaultVariable
    { variableName = "Exception Handler"
    , variableValue = cs variableValue2
    , variableType = Just (cs variableType2)
    }
  pure [ localScope localsScopeIndex ]
    where
      localScope localsScopeIndex
        = defaultScope
        { scopeName = "Locals"
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
        { scopeName = "Locals"
        , scopePresentationHint = Just ScopePresentationHintLocals
        , scopeVariablesReference = localsScopeIndex
        , scopeNamedVariables = Just 0
        }
generateScopes stackCont@(RaiseOp atom) = do
  frameId <- getCurrentFrameId
  localsScopeIndex <- getNextScopeId
  let (variableType, variableValue) = getAtomTypeAndValue atom
  addVariable localsScopeIndex defaultVariable
    { variableName = "RaiseOp"
    , variableValue = cs variableValue
    , variableType = Just (cs variableType)
    }
  pure [ localScope localsScopeIndex ]
    where
      localScope localsScopeIndex
        = defaultScope
        { scopeName = "Locals"
        , scopePresentationHint = Just ScopePresentationHintLocals
        , scopeVariablesReference = localsScopeIndex
        , scopeNamedVariables = Just 1
        }
generateScopes stackCont@(KeepAlive atom) = do
  frameId <- getCurrentFrameId
  localsScopeIndex <- getNextScopeId
  let (variableType, variableValue) = getAtomTypeAndValue atom
  addVariable localsScopeIndex defaultVariable
    { variableName = "Managed Object"
    , variableValue = cs variableValue
    , variableType = Just (cs variableType)
    }
  pure [ localScope localsScopeIndex ]
    where
      localScope localsScopeIndex
        = defaultScope
        { scopeName = "Locals"
        , scopePresentationHint = Just ScopePresentationHintLocals
        , scopeVariablesReference = localsScopeIndex
        , scopeNamedVariables = Just 1
        }
generateScopes stackCont@(DebugFrame (RestoreProgramPoint maybeId _)) = do
  frameId <- getCurrentFrameId
  localsScopeIndex <- getNextScopeId
  addVariable localsScopeIndex defaultVariable
    { variableName = "DebugFrame"
    , variableValue = cs (show maybeId)
          -- case unId maybeId of
          --   Just Binder {..} -> Just $ T.pack (show binderType)
          --   Nothing -> mempty
    , variableType = Just "RestoreProgramPoint"
    }
  pure [ localScope localsScopeIndex ]
    where
      localScope localsScopeIndex
        = defaultScope
        { scopeName = "Locals"
        , scopePresentationHint = Just ScopePresentationHintLocals
        , scopeVariablesReference = localsScopeIndex
        , scopeNamedVariables = Just 1
        }

showLitNumType :: LitNumType -> String
showLitNumType LitNumInt8   = "Int8"
showLitNumType LitNumInt16  = "Int16"
showLitNumType LitNumInt32  = "Int32"
showLitNumType LitNumInt64  = "Int64"
showLitNumType LitNumWord   = "Word"
showLitNumType LitNumWord8  = "Word8"
showLitNumType LitNumWord16 = "Word16"
showLitNumType LitNumWord32 = "Word32"
showLitNumType LitNumWord64 = "Word64"

showElemRep :: PrimElemRep -> String
showElemRep Int8ElemRep   = "Int8Rep"
showElemRep Int16ElemRep  = "Int16Rep"
showElemRep Int32ElemRep  = "Int32Rep"
showElemRep Int64ElemRep  = "Int64Rep"
showElemRep Word8ElemRep  = "Word8Rep"
showElemRep Word16ElemRep = "Word16Rep"
showElemRep Word32ElemRep = "Word32Rep"
showElemRep Word64ElemRep = "Word64Rep"
showElemRep FloatElemRep  = "FloatRep"
showElemRep DoubleElemRep = "DoubleRep"

showRubbishType :: Type -> String
showRubbishType (SingleValue primRep) = showPrimRep primRep

showRubbishType (UnboxedTuple primReps) =
  concat
  [ "(# "
  , intercalate "," (showPrimRep <$> primReps)
  , " #)"
  ]
showRubbishType PolymorphicRep = show PolymorphicRep

showPrimRep :: PrimRep -> String
showPrimRep (VecRep n primElemRep) =
  concat
  [ "<"
  , intercalate "," (replicate n (showElemRep primElemRep))
  , ">"
  ]
showPrimRep rep = show rep

getAtomTypeAndValue
  :: Atom
  -> (String, String)
getAtomTypeAndValue = \case
  HeapPtr addr                                 -> ("HeapPtr", show addr)
  Literal (LitChar char)                       -> ("Char", [char])
  Literal (LitString bytes)                    -> ("String", cs bytes)
  Literal LitNullAddr                          -> ("Address", "0x00000000")
  Literal (LitFloat float)                     -> ("Float", show float)
  Literal (LitDouble double)                   -> ("Double", show double)
  Literal (LitLabel labelName FunctionLabel{}) -> ("Foreign Function", cs labelName)
  Literal (LitLabel labelName DataLabel)       -> ("Foreign Data", cs labelName)
  Literal (LitNumber num value)                -> (showLitNumType num, show value)
  Literal (LitRubbish rubbishType)             -> ("Rubbish", showRubbishType rubbishType)
  Void                                         -> ("Void", "()")
  PtrAtom _ x                                  -> ("Ptr", show x)
  IntAtom x                                    -> ("Int", show x)
  WordAtom x                                   -> ("Word", show x)
  FloatAtom x                                  -> ("Float", show x)
  DoubleAtom x                                 -> ("Double", show x)
  MVar x                                       -> ("MVar", show x)
  MutVar x                                     -> ("MutVar", show x)
  TVar x                                       -> ("TVar", show x)
  Array idx                                    -> ("Array", show idx)
  MutableArray idx                             -> ("MutableArray", show idx)
  SmallArray idx                               -> ("SmallArray", show idx)
  SmallMutableArray idx                        -> ("SmallMutableArray", show idx)
  ArrayArray idx                               -> ("ArrayArray", show idx)
  MutableArrayArray idx                        -> ("MutableArrayArray", show idx)
  ByteArray idx                                -> ("ByteArray", show idx)
  MutableByteArray idx                         -> ("MutableByteArray", show idx)
  WeakPointer x                                -> ("WeakPoint", show x)
  StableName x                                 -> ("StableName", show x)
  ThreadId x                                   -> ("ThreadId", show x)
  LiftedUndefined                              -> ("LiftedUndefined","undefined")

-- + on watch causes "CommandEvaluate"
-- right click - set value - [127.0.0.1:49599][INFO][GOT cmd CommandSetVariable]
-- right click - copy value - [127.0.0.1:49599][INFO][GOT cmd CommandEvaluate]
-- save breakpoints from breakpoints request into AdaptrClient set, set them on the interpreter after configuration done (not attach)

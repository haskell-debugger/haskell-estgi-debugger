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
{-# LANGUAGE TupleSections       #-}
----------------------------------------------------------------------------
module Main (main) where
----------------------------------------------------------------------------
import           Data.List
import           Data.String.Conversions               (cs)
import           Text.PrettyPrint.ANSI.Leijen          (pretty, plain)
import           Codec.Archive.Zip                     (withArchive, unEntrySelector, getEntries)
import           Data.IntSet                           ( IntSet )
import qualified Data.IntSet                           as IntSet
import           Data.Set                              ( Set )
import qualified Data.Set                              as Set
import           Control.Arrow
import           Data.IORef
import           Control.Exception                     hiding (catch)
import           Control.Monad.IO.Class                (liftIO)
import           Control.Exception.Lifted              (catch)
import           Control.Monad.State.Strict            ( gets )
import           Control.Monad
import           Control.Monad.State.Strict            ( gets )
import           Data.Aeson                            ( Value(Null), FromJSON )
import qualified Data.Aeson                            as Aeson
import           Data.IntMap.Strict                    ( IntMap )
import qualified Data.IntMap.Strict                    as IntMap
import           Data.Bimap                            ( Bimap )
import qualified Data.Bimap                            as Bimap
import qualified Data.Map.Strict                       as M
import           Data.Map.Strict                       ( Map )
import qualified Data.Text.Encoding                    as T
import           Data.Text                             ( Text )
import qualified Data.Text                             as T
import qualified Data.Text.Lazy                        as LazyText
import           Data.Typeable                         ( typeOf )
import           Data.Maybe                            ( fromMaybe, maybeToList )
import           Data.List                             ( sortOn )
import           GHC.Generics                          ( Generic )
import           System.Environment                    ( lookupEnv )
import           System.FilePath                       ( (-<.>), (</>), takeDirectory, takeExtension, dropExtension, splitFileName, splitPath, joinPath, splitDirectories)
import           Text.Read                             ( readMaybe )
import qualified Data.ByteString.Lazy.Char8            as BL8 ( pack, unpack, fromStrict, toStrict )
import qualified Control.Concurrent.Chan.Unagi.Bounded as Unagi
import           Control.Concurrent.MVar               ( MVar )
import qualified Control.Concurrent.MVar               as MVar
import qualified System.FilePath.Find                  as Glob
----------------------------------------------------------------------------
import           Stg.Syntax                            hiding (sourceName, Scope)
import           Stg.IRLocation
import           Stg.Tickish                           ( collectTickish )
import           Stg.Pretty
import           Stg.Interpreter
import           Stg.Interpreter.Debug
import           Stg.Interpreter.Base                  hiding (lookupEnv, getCurrentThreadState, Breakpoint)
import qualified Stg.Interpreter.Base                  as Stg
import           Stg.Interpreter.Debugger
import           Stg.Interpreter.Debugger.UI
import           Stg.IO
import           Stg.Program
import           Stg.Fullpak
import           Data.Yaml                             hiding (Array)
import qualified Text.Pretty.Simple                    as PP
----------------------------------------------------------------------------
import           DAP                                   hiding (send)
----------------------------------------------------------------------------
import           CustomCommands
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
    hostDefault = "0.0.0.0"
    portDefault = 4711
    capabilities = defaultCapabilities
      { supportsConfigurationDoneRequest      = True
      , supportsHitConditionalBreakpoints     = True
      , supportsEvaluateForHovers             = False
      , supportsModulesRequest                = True
      , additionalModuleColumns               = [ defaultColumnDescriptor
                                                  { columnDescriptorAttributeName = "Extra"
                                                  , columnDescriptorLabel = "Label"
                                                  }
                                                ]
      , supportsValueFormattingOptions        = True
      , supportTerminateDebuggee              = True
      , supportsLoadedSourcesRequest          = True
      }
  ServerConfig
    <$> do fromMaybe hostDefault <$> lookupEnv "DAP_HOST"
    <*> do fromMaybe portDefault . (readMaybe =<<) <$> do lookupEnv "DAP_PORT"
    <*> pure capabilities
    <*> pure True

----------------------------------------------------------------------------
-- | VSCode arguments are custom for attach
-- > "arguments": {
-- >      "__configurationTarget": 6,
-- >      "__sessionId": "6c0ba6f8-e478-4698-821e-356fc4a72c3d",
-- >      "name": "thing",
-- >      "program": "/home/dmjio/Desktop/stg-dap/test.ghc_stgapp",
-- >      "request": "attach",
-- >      "type": "dap-estgi-extension"
-- >  }
--
data AttachArgs
  = AttachArgs
  { __sessionId :: Text
    -- ^ SessionID from VSCode
  , program :: String
    -- ^ Path or glob pattern to .ghc_stgapp file
  } deriving stock (Show, Eq, Generic)
    deriving anyclass FromJSON
----------------------------------------------------------------------------
-- | External STG Interpreter application internal state
data ESTG
  = ESTG
  { debuggerChan      :: DebuggerChan
  , fullPakPath       :: String
  , breakpointMap     :: Map Stg.Breakpoint IntSet
  , sourceCodeSet     :: Set SourceCodeDescriptor
  , unitIdMap         :: Bimap UnitId PackageName
  , haskellSrcPathMap :: Bimap Name SourceCodeDescriptor
  , dapSourceNameMap  :: Bimap Text DapSourceRefDescriptor

  -- application specific resource handling

  , dapSourceRefMap       :: !(Bimap DapSourceRefDescriptor Int)
    -- ^ Used to track source reference IDs
    --
  , dapFrameIdMap         :: !(Bimap DapFrameIdDescriptor Int)
    -- ^ Used to track stack frame IDs
    --
  , dapVariablesRefMap    :: !(Bimap DapVariablesRefDescriptor Int)
    -- ^ Used to track variablesReferences
    --
  , dapVariablesRefStore  :: !(IntMap [Variable])
    -- ^ Stores the assigned Variables for each VariablesReference
    --
  , nextFreshBreakpointId :: !Int
    -- ^ monotinic counter for unique BreakpointId assignment
    --
  }

findProgram :: String -> IO [FilePath]
findProgram globPattern = do
  let isPattern = any (`elem` ("[*?" :: String))
      startDir = joinPath . takeWhile (not . isPattern) . splitPath $ takeDirectory globPattern
  Glob.find Glob.always (Glob.filePath Glob.~~? globPattern) startDir

----------------------------------------------------------------------------
-- | Intialize ESTG interpreter
----------------------------------------------------------------------------
initESTG :: AttachArgs -> Adaptor ESTG ()
initESTG AttachArgs {..} = do
  ghcstgappPath <- (liftIO $ findProgram program) >>= \case
    [fname] -> pure fname
    []      -> sendError (ErrorMessage (T.pack $ unlines ["No .ghc_stgapp program found at:", program])) Nothing
    names   -> sendError (ErrorMessage (T.pack $ unlines $ ["Ambiguous program path:", program, "Use more specific path pattern to fix the issue.", "Multiple matches:"] ++ names)) Nothing
  let fullpakPath = ghcstgappPath -<.> ".fullpak"
  liftIO $ mkFullpak ghcstgappPath False False fullpakPath
  (sourceCodeList, unitIdMap, haskellSrcPathMap) <- liftIO $ getSourceCodeListFromFullPak fullpakPath
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
        { debuggerChan          = dbgChan
        , fullPakPath           = fullpakPath
        , breakpointMap         = mempty
        , sourceCodeSet         = Set.fromList sourceCodeList
        , unitIdMap             = unitIdMap
        , haskellSrcPathMap     = haskellSrcPathMap
        , dapSourceNameMap      = Bimap.fromList [(cs $ getSourceName d, SourceRef_SourceFileInFullpak d) | d <- sourceCodeList]
        , dapSourceRefMap       = Bimap.fromList $ zip (map SourceRef_SourceFileInFullpak sourceCodeList) [1..]
        , dapFrameIdMap         = Bimap.empty
        , dapVariablesRefMap    = Bimap.empty
        , dapVariablesRefStore  = mempty
        , nextFreshBreakpointId = 1
        }
  flip catch handleDebuggerExceptions
    $ registerNewDebugSession __sessionId estg
      (loadAndRunProgram True True fullpakPath [] dbgChan DbgStepByStep False defaultDebugSettings)
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
        StgState{..} <- getStgState
        sendEvent EventTypeStopped $ object
          [ "reason"             .= String "step"
          , "allThreadsStopped"  .= True
          , "threadId"           .= Number (fromIntegral ssCurrentThreadId)
          ]

      DbgEventHitBreakpoint bkpName -> do
        resetObjectLifetimes
        StgState{..} <- getStgState
        sendEvent EventTypeStopped . object $
          [ "reason"             .= String "breakpoint"
          , "allThreadsStopped"  .= True
          , "threadId"           .= Number (fromIntegral ssCurrentThreadId)
          ] ++
          [ "hitBreakpointIds" .= idSet
          | Just idSet <- pure $ M.lookup bkpName breakpointMap
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

----------------------------------------------------------------------------
-- | Clears the currently known breakpoint set
clearBreakpoints :: Adaptor ESTG ()
clearBreakpoints = do
  updateDebugSession $ \estg -> estg {breakpointMap = mempty}

----------------------------------------------------------------------------
-- | Adds new BreakpointId for a givent StgPoint
addNewBreakpoint :: Stg.Breakpoint -> Adaptor ESTG BreakpointId
addNewBreakpoint breakpoint = do
  bkpId <- getFreshBreakpointId
  updateDebugSession $ \estg@ESTG{..} -> estg {breakpointMap = M.insertWith mappend breakpoint (IntSet.singleton bkpId) breakpointMap}
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
talk (CustomCommand "garbageCollect") = do
  logInfo "Running garbage collection..."
  sendAndWait (CmdInternal "gc")
  sendSuccesfulEmptyResponse
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
  sendLoadedSourcesResponse =<< do
    {-
      list only Haskell ExtStg and ForeignC files
    -}
    let shouldInclude = \case
          Haskell{}   -> True
          ForeignC{}  -> True
          _           -> False
    srcSet <- getsApp sourceCodeSet
    mapM (getSourceFromSourceRefDescriptor . SourceRef_SourceFileInFullpak) $ filter shouldInclude $ Set.toList srcSet

----------------------------------------------------------------------------
talk (CustomCommand "getSourceLinks") = do
  GetSourceLinksArguments {..} <- getArguments
  ESTG {..} <- getDebugSession
  sourceLinks <- case Bimap.lookup getSourceLinksArgumentsPath dapSourceNameMap of
    Just srcDesc@(SourceRef_SourceFileInFullpak ExtStg{}) -> do
      source <- getSourceFromSourceRefDescriptor srcDesc
      let Just sourceRef = sourceSourceReference source
      (_sourceCodeText, locations, hsSrcLocs) <- getSourceFromFullPak sourceRef
      let hsTickishLocMap = M.unionsWith mappend [M.singleton stgPoint [tickish] | (stgPoint, tickish) <- hsSrcLocs]
          -- collect tickish locations
          estgLocMap = M.unionsWith mappend
            [ M.singleton stgPoint [range]
            | (SP_Tickish stgPoint, range) <- locations
            ]
      liftIO $ do
        print hsTickishLocMap
        print estgLocMap
      pure $
        [ SourceLink
          { sourceLinkSourceLine        = estgStartLine
          , sourceLinkSourceColumn      = estgStartCol
          , sourceLinkSourceEndLine     = estgEndLine
          , sourceLinkSourceEndColumn   = estgEndCol
          , sourceLinkTargetLine        = srcSpanSLine
          , sourceLinkTargetColumn      = srcSpanSCol
          , sourceLinkTargetEndLine     = srcSpanELine
          , sourceLinkTargetEndColumn   = srcSpanECol
          , sourceLinkTargetPath        = cs $ getSourceName hsCodeDesc
          }
        | (stgPoint, hsTickishList) <- M.toList hsTickishLocMap
        , estgLocList <- maybeToList $ M.lookup stgPoint estgLocMap
        , (((estgStartLine, estgStartCol),(estgEndLine, estgEndCol)), SourceNote{..}) <- zip estgLocList hsTickishList
        , let RealSrcSpan'{..} = sourceSpan
        , hsCodeDesc <- maybeToList $ Bimap.lookup srcSpanFile haskellSrcPathMap
        ]
    _ -> pure []
  sendSuccesfulResponse . setBody $ GetSourceLinksResponse
    { getSourceLinksResponseSourceLinks = sourceLinks
    }

----------------------------------------------------------------------------
talk CommandModules = do
  sendModulesResponse (ModulesResponse [] Nothing)
----------------------------------------------------------------------------
talk CommandPause = do
  sendAndWait CmdStop
  sendPauseResponse
----------------------------------------------------------------------------
talk CommandSetBreakpoints = do
  SetBreakpointsArguments {..} <- getArguments
  maybeSourceRef <- getValidSourceRefFromSource setBreakpointsArgumentsSource

  -- the input SourceRef might be a remain of a previous DAP session, update it with the new valid one
  let refUpdatedSource = setBreakpointsArgumentsSource { sourceSourceReference = maybeSourceRef }

  clearBreakpoints
  {-
    supports placing breakpoint on:
      - Haskell
      - ExtStg
  -}
  ESTG {..} <- getDebugSession
  case (setBreakpointsArgumentsBreakpoints, maybeSourceRef, maybeSourceRef >>= flip Bimap.lookupR dapSourceRefMap) of
    -- HINT: breakpoint on Haskell
    (Just sourceBreakpoints, Just sourceRef, Just (SourceRef_SourceFileInFullpak hsCodeDesc@(Haskell pkg mod)))
      | Just extStgSourceRef <- Bimap.lookup (SourceRef_SourceFileInFullpak $ ExtStg pkg mod) dapSourceRefMap
      , Just hsSourceFilePath <- Bimap.lookupR hsCodeDesc haskellSrcPathMap
      -> do
      (_sourceCodeText, _locations, hsSrcLocs) <- getSourceFromFullPak extStgSourceRef
      breakpoints <- forM sourceBreakpoints $ \SourceBreakpoint{..} -> do
        -- filter all relevant ranges
        {-
          SP_RhsClosureExpr
        -}
        let onlySupported = \case
              SP_RhsClosureExpr{} -> True
              _ -> True -- TODO
        let relevantLocations = filter (onlySupported . fst . fst) $ case sourceBreakpointColumn of
              Nothing ->
                [ (p, spanSize)
                | p@(_,SourceNote RealSrcSpan'{..} _) <- hsSrcLocs
                , srcSpanFile == hsSourceFilePath
                , srcSpanSLine <= sourceBreakpointLine
                , srcSpanELine >= sourceBreakpointLine
                , let spanSize = (srcSpanELine - srcSpanSLine, srcSpanECol - srcSpanSCol)
                ]
              Just col  ->
                [ (p, spanSize)
                | p@(_,SourceNote RealSrcSpan'{..} _) <- hsSrcLocs
                , srcSpanFile == hsSourceFilePath
                , srcSpanSLine <= sourceBreakpointLine
                , srcSpanELine >= sourceBreakpointLine
                , srcSpanSCol <= col
                , srcSpanECol >= col
                , let spanSize = (srcSpanELine - srcSpanSLine, srcSpanECol - srcSpanSCol)
                ]
        debugMessage . cs . unlines $ "relevant haskell locations:" : map show relevantLocations
        -- use the first location found
        -- HINT: locations are sorted according the span size, small spans are preferred more
        case map fst . take 1 $ sortOn snd relevantLocations of
          (stgPoint@(SP_RhsClosureExpr closureName), SourceNote RealSrcSpan'{..} _) : _ -> do
            let hitCount = fromMaybe 0 (sourceBreakpointHitCondition >>= readMaybe . T.unpack) :: Int
            sendAndWait (CmdAddBreakpoint (BkpStgPoint stgPoint) hitCount)
            bkpId <- addNewBreakpoint $ BkpStgPoint stgPoint
            pure $ defaultBreakpoint
              { breakpointVerified  = True
              , breakpointSource    = Just refUpdatedSource
              , breakpointLine      = Just srcSpanSLine
              , breakpointColumn    = Just srcSpanSCol
              , breakpointEndLine   = Just srcSpanELine
              , breakpointEndColumn = Just srcSpanECol
              , breakpointId        = Just bkpId
              }
          _ ->
            pure $ defaultBreakpoint
              { breakpointVerified  = False
              , breakpointSource    = Just refUpdatedSource
              , breakpointMessage   = Just "no hs code found"
              }
      sendSetBreakpointsResponse breakpoints

    -- HINT: breakpoint on ExtStg
    (Just sourceBreakpoints, Just sourceRef, Just (SourceRef_SourceFileInFullpak ExtStg{})) -> do
      (_sourceCodeText, locations, _hsSrcLocs) <- getSourceFromFullPak sourceRef
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
        debugMessage . cs $ "relevantLocations: " ++ show relevantLocations
        -- use the first location found
        case sortOn snd relevantLocations of
          (stgPoint@(SP_RhsClosureExpr closureName), ((startRow, startCol), (endRow, endCol))) : _ -> do
            let hitCount = fromMaybe 0 (sourceBreakpointHitCondition >>= readMaybe . T.unpack) :: Int
            sendAndWait (CmdAddBreakpoint (BkpStgPoint stgPoint) hitCount)
            bkpId <- addNewBreakpoint $ BkpStgPoint stgPoint
            pure $ defaultBreakpoint
              { breakpointVerified  = True
              , breakpointSource    = Just refUpdatedSource
              , breakpointLine      = Just startRow
              , breakpointColumn    = Just startCol
              , breakpointEndLine   = Just endRow
              , breakpointEndColumn = Just endCol
              , breakpointId        = Just bkpId
              }
          _ ->
            pure $ defaultBreakpoint
              { breakpointVerified  = False
              , breakpointSource    = Just refUpdatedSource
              , breakpointMessage   = Just "no code found"
              }
      sendSetBreakpointsResponse breakpoints
    v -> do
      sendSetBreakpointsResponse []
----------------------------------------------------------------------------
talk CommandStackTrace = do
  {-
    TODO:
    done - use the thread id from the arguments
    done - generate source location for stack frames where possible
    done - add the top frame derived from the current closure and env
    done - generate frameIds properly, store thread id and frame index for each frameId
      REQUIREMENT:
        move all resource handling code to the application side, the library should only be a message framework
  -}
  StackTraceArguments {..} <- getArguments
  StgState{..} <- getStgState
  case IntMap.lookup stackTraceArgumentsThreadId ssThreads of
    Nothing -> do
      sendError (ErrorMessage (T.pack $ "Unknown threadId: " ++ show stackTraceArgumentsThreadId)) Nothing

    Just ThreadState{..} -> do
      -- create the top stack frame from the current closure, but only for the current thread
      --  other (not currently running) threads do have everything on the thread stack
      topFrame <- case ssCurrentClosure of
        Just currentClosureId
          | ssCurrentThreadId == stackTraceArgumentsThreadId
          -> do
          (source, line, column, endLine, endColumn) <- getSourceAndPositionForStgPoint currentClosureId (SP_RhsClosureExpr currentClosureId)
          frameId <- getFrameId FrameId_CurrentThreadTopStackFrame
          pure [ defaultStackFrame
                  { stackFrameId        = frameId
                  , stackFrameName      = T.pack (show currentClosureId)
                  , stackFrameSource    = source
                  , stackFrameLine      = line
                  , stackFrameColumn    = column
                  , stackFrameEndLine   = Just endLine
                  , stackFrameEndColumn = Just endColumn
                  }
               ]
        _ -> pure []

      -- create the rest of frames from the Thread's stack frames
      stackFrames <- forM (zip [0..] tsStack) $ \(frameIndex, stackCont) -> case stackCont of
        CaseOf _ closureId _ scrutResultId _ _ -> do
          -- HINT: use the case scrutinee result's unique binder id to lookup source location info
          (source, line, column, endLine, endColumn) <- getSourceAndPositionForStgPoint scrutResultId (SP_CaseScrutineeExpr scrutResultId)
          frameId <- getFrameId $ FrameId_ThreadStackFrame stackTraceArgumentsThreadId frameIndex
          pure $ defaultStackFrame
            { stackFrameId        = frameId
            , stackFrameName      = cs $ "CaseOf " ++ show closureId
            , stackFrameSource    = source
            , stackFrameLine      = line
            , stackFrameColumn    = column
            , stackFrameEndLine   = Just endLine
            , stackFrameEndColumn = Just endColumn
            }

        _ -> do
          frameId <- getFrameId $ FrameId_ThreadStackFrame stackTraceArgumentsThreadId frameIndex
          pure $ defaultStackFrame
              -- HINT: no source location info
              { stackFrameId        = frameId
              , stackFrameName      = T.pack (showStackCont stackCont)
              , stackFrameLine      = 0
              , stackFrameColumn    = 0
              }

      sendStackTraceResponse $ StackTraceResponse
        { stackFrames = topFrame ++ stackFrames
        , totalFrames = Just (length topFrame + length stackFrames)
        }

----------------------------------------------------------------------------
talk CommandSource = do
  SourceArguments {..} <- getArguments -- save path of fullpak in state
  {-
    primary:    sourceArgumentsSource
    secondary:  sourceArgumentsSourceReference
  -}
  sourceRef <- fromMaybe sourceArgumentsSourceReference <$>
    case sourceArgumentsSource of
      Just source -> getValidSourceRefFromSource source
      Nothing     -> pure Nothing

  (source, _locations, _hsSrcLocs) <- getSourceFromFullPak sourceRef
  sendSourceResponse (SourceResponse source Nothing)
----------------------------------------------------------------------------
talk CommandThreads = do
  allThreads <- IntMap.toList . ssThreads <$> getStgState
  sendThreadsResponse
    [ Thread
      { threadId    = threadId
      , threadName  = T.pack ("thread id: " <> show threadId <> " " <> threadLabel)
      }
    | (threadId, threadState) <- allThreads
    , isThreadLive $ tsStatus threadState
    , let threadLabel = mkThreadLabel threadState
    ]
----------------------------------------------------------------------------
talk CommandScopes = do
  ScopesArguments {..} <- getArguments
  StgState{..} <- getStgState
  ESTG {..} <- getDebugSession
  case Bimap.lookupR scopesArgumentsFrameId dapFrameIdMap of
    Nothing -> do
      sendError (ErrorMessage (T.pack $ "Unknown frameId: " ++ show scopesArgumentsFrameId)) Nothing

    Just frameIdDescriptor@FrameId_CurrentThreadTopStackFrame
      | Just currentClosureId <- ssCurrentClosure
      -> do
        scopes <- generateScopesForTopStackFrame frameIdDescriptor currentClosureId ssCurrentClosureEnv
        sendScopesResponse (ScopesResponse scopes)

    Just frameIdDescriptor@(FrameId_ThreadStackFrame threadId frameIndex) -> do
      let stackFrame = (tsStack $ ssThreads IntMap.! threadId) !! frameIndex
      scopes <- generateScopes frameIdDescriptor stackFrame
      sendScopesResponse (ScopesResponse scopes)

    _ -> sendScopesResponse (ScopesResponse [])

----------------------------------------------------------------------------
talk CommandVariables = do
  VariablesArguments {..} <- getArguments
  getsApp (Bimap.lookupR variablesArgumentsVariablesReference . dapVariablesRefMap) >>= \case
    Just VariablesRef_StackFrameVariables{} -> do
      variables <- getVariables variablesArgumentsVariablesReference
      sendVariablesResponse (VariablesResponse variables)
    Just (VariablesRef_HeapObject frameIdDesc addr) -> do
      stgState <- getStgState
      ho <- case IntMap.lookup addr $ ssHeap stgState of
        Nothing -> sendError (ErrorMessage (T.pack $ "Unknown heap object: " ++ show addr)) Nothing
        Just v  -> pure v
      variables <- getVariablesForHeapObject stgState frameIdDesc ho
      -- detect and annotate loops
      let markLoop v
            | variableVariablesReference v == 0
            = v
            | variableVariablesReference v > variablesArgumentsVariablesReference
            = v
            | otherwise
            = v {variableName = variableName v <> " <also-shown-in-ancestor>"}
      sendVariablesResponse (VariablesResponse $ map markLoop variables)
    Nothing -> do
      sendVariablesResponse (VariablesResponse [])
----------------------------------------------------------------------------
talk CommandNext = do
  NextArguments {..} <- getArguments
  sendAndWait CmdStep
  sendNextResponse
----------------------------------------------------------------------------
talk CommandBreakpointLocations       = sendBreakpointLocationsResponse []
talk CommandSetDataBreakpoints        = sendSetDataBreakpointsResponse []
talk CommandSetExceptionBreakpoints   = sendSetExceptionBreakpointsResponse []
talk CommandSetFunctionBreakpoints    = sendSetFunctionBreakpointsResponse []
talk CommandSetInstructionBreakpoints = sendSetInstructionBreakpointsResponse []
----------------------------------------------------------------------------
talk CommandEvaluate = do
  EvaluateArguments {..} <- getArguments
  sendEvaluateResponse EvaluateResponse
    { evaluateResponseResult  = "evaluated value for " <> evaluateArgumentsExpression
    , evaluateResponseType    = "evaluated type for " <> evaluateArgumentsExpression
    , evaluateResponsePresentationHint    = Nothing
    , evaluateResponseVariablesReference  = 1
    , evaluateResponseNamedVariables      = Just 1
    , evaluateResponseIndexedVariables    = Nothing
    , evaluateResponseMemoryReference     = Nothing
    }
----------------------------------------------------------------------------
talk cmd = logInfo $ BL8.pack ("GOT cmd " <> show cmd)
----------------------------------------------------------------------------


getSourceAndPositionForStgPoint :: Id -> StgPoint -> Adaptor ESTG (Maybe Source, Int, Int, Int, Int)
getSourceAndPositionForStgPoint (Id Binder{..}) stgPoint = do
  ESTG {..} <- getDebugSession
  packageName <- case Bimap.lookup binderUnitId unitIdMap of
    Nothing -> sendError (ErrorMessage (T.pack $ "Unknown unit id: " ++ show binderUnitId)) Nothing
    Just v  -> pure v
  let moduleName = cs $ getModuleName binderModule
  source <- getSourceFromSourceRefDescriptor $ SourceRef_SourceFileInFullpak $ ExtStg packageName moduleName
  let Just sourceRef = sourceSourceReference source
  (_sourceCodeText, locations, hsSrcLocs) <- getSourceFromFullPak sourceRef
  let inModule pkg mod (_, SourceNote{..})
        | RealSrcSpan'{..} <- sourceSpan
        , Just hsSrcDesc <- Bimap.lookup srcSpanFile haskellSrcPathMap
        = hsSrcDesc == Haskell pkg mod
      inModule _ _ _ = False

      stgPointLocs  = filter ((== stgPoint) . fst) hsSrcLocs
      hsModLocs     = filter (inModule packageName moduleName) stgPointLocs
  forM_ stgPointLocs $ \(_, tickish) -> liftIO $ print tickish
  {-
    source location priorities:
      - haskell module local
      - stg
  -}
  case hsModLocs of
    (_, SourceNote{..}) : _
      | RealSrcSpan'{..} <- sourceSpan
      , Just hsSrcDesc <- Bimap.lookup srcSpanFile haskellSrcPathMap
      -> do
      sourceHs <- getSourceFromSourceRefDescriptor $ SourceRef_SourceFileInFullpak hsSrcDesc
      pure (Just sourceHs, srcSpanSLine, srcSpanSCol, srcSpanELine, srcSpanECol)
    _ -> do
      case filter ((== stgPoint) . fst) locations of
        (_, ((line, column),(endLine, endColumn))) : _ -> do
          pure (Just source, line, column, endLine, endColumn)
        _ -> do
          pure (Just source, 0, 0, 0, 0)

----------------------------------------------------------------------------

----------------------------------------------------------------------------
-- | Retrieves list of modules from .fullpak file
getSourceCodeListFromFullPak :: FilePath -> IO ([SourceCodeDescriptor], Bimap UnitId PackageName, Bimap Name SourceCodeDescriptor)
getSourceCodeListFromFullPak fullPakPath = do
  rawEntries <- fmap unEntrySelector . M.keys <$> withArchive fullPakPath getEntries
  let folderNames = Set.fromList (takeDirectory <$> rawEntries)
      appInfoName = "app.info"
  appInfoBytes <- readModpakL fullPakPath appInfoName id
  AppInfo{..} <- decodeThrow (BL8.toStrict appInfoBytes)
  let unitIdMap = Bimap.fromList
        [ (UnitId $ cs ciUnitId, cs ciPackageName)
        | CodeInfo{..} <- aiLiveCode
        ]
  {-
    program source content:
      haskell modules
      foreign files
  -}
  let rawEntriesSet = Set.fromList rawEntries
      moduleCodeItems pkg mod =
        [ Haskell   pkg mod
        , GhcCore   pkg mod
        , GhcStg    pkg mod
        , Cmm       pkg mod
        , Asm       pkg mod
        , ExtStg    pkg mod
        , FFICStub  pkg mod
        , FFIHStub  pkg mod
        , ModInfo   pkg mod
        ]
      haskellModuleCode :: [SourceCodeDescriptor]
      haskellModuleCode =
        [ srcDesc
        | CodeInfo{..} <- aiLiveCode
        , srcDesc <- moduleCodeItems (cs ciPackageName) (cs ciModuleName)
        , Set.member (getSourcePath srcDesc) rawEntriesSet
        ]

      cbitsSources :: [SourceCodeDescriptor]
      cbitsSources =
        [ ForeignC packageName path
        | path <- rawEntries
        , ("cbits-source" : unitIdString : _) <- [splitDirectories path]
        , Just packageName <- [Bimap.lookup (UnitId $ cs unitIdString) unitIdMap]
        ]

  hsPathList <- forM aiLiveCode $ \CodeInfo{..} -> do
    let extStgPath = getSourcePath $ ExtStg (cs ciPackageName) (cs ciModuleName)
    (_phase, _unitId, _modName, mSrcFilePath, _stubs, _hasForeignExport, _deps) <- readModpakL fullPakPath extStgPath decodeStgbinInfo
    case mSrcFilePath of
      Nothing -> pure []
      Just p  -> pure [(cs p, Haskell (cs ciPackageName) (cs ciModuleName))]
  let hsPathMap = Bimap.fromList $ concat hsPathList
  pure (haskellModuleCode ++ cbitsSources, unitIdMap, hsPathMap)

getValidSourceRefFromSource :: Source -> Adaptor ESTG (Maybe Int)
getValidSourceRefFromSource Source{..} = do
  ESTG {..} <- getDebugSession
  {-
    fallback chain:
      1. sourcePath
      2. sourceSourceReference
  -}
  let maybeSrcDesc = do
        srcName <- sourcePath
        Bimap.lookup srcName dapSourceNameMap
  case maybeSrcDesc of
    Just srcDesc  -> Just <$> getSourceRef srcDesc
    Nothing       -> case sourceSourceReference of
      Just srcRef
        | Bimap.memberR srcRef dapSourceRefMap
        -> pure sourceSourceReference
      _ -> pure Nothing

----------------------------------------------------------------------------
-- | Retrieves list of modules from .fullpak file
getSourceFromFullPak :: SourceId -> Adaptor ESTG (Text, [(StgPoint, SrcRange)], [(StgPoint, Tickish)])
getSourceFromFullPak sourceId = do
  ESTG {..} <- getDebugSession
  SourceRef_SourceFileInFullpak srcDesc <- case Bimap.lookupR sourceId dapSourceRefMap of
    Nothing     -> do
      sendError (ErrorMessage (T.pack $ "Unknown sourceId: " ++ show sourceId)) Nothing
    Just value  -> pure value
  let sourcePath = getSourcePath srcDesc
  liftIO $
    case srcDesc of
      ExtStg{} -> do
        m <- readModpakL fullPakPath sourcePath decodeStgbin
        let (stgCode, stgLocs)  = pShowWithConfig Config {cfgPrintTickish = True} $ pprModule m
            tickishList         = collectTickish m
        pure (stgCode, stgLocs, tickishList)
      _ -> do
        ir <- readModpakS fullPakPath sourcePath T.decodeUtf8
        pure (ir, [], [])
----------------------------------------------------------------------------
-- | Synchronous call to Debugger, sends message and waits for response
sendAndWait :: DebugCommand -> Adaptor ESTG DebugOutput
sendAndWait cmd = do
  ESTG {..} <- getDebugSession
  let DebuggerChan{..} = debuggerChan
  liftIO $ do
    MVar.putMVar dbgSyncRequest cmd
    MVar.takeMVar dbgSyncResponse

getStgState :: Adaptor ESTG StgState
getStgState = do
  sendAndWait (CmdInternal "get-stg-state") >>= \case
    DbgOutStgState stgState ->
      pure stgState
    otherMessage -> do
      let errorMsg
            = concat
            [ "Unexpected Message received from interpreter: "
            , show otherMessage
            ]
      logInfo (BL8.pack errorMsg)
      sendError (ErrorMessage (T.pack errorMsg)) Nothing

----------------------------------------------------------------------------
mkThreadLabel :: ThreadState -> String
mkThreadLabel = maybe "" (BL8.unpack . BL8.fromStrict) . tsLabel

generateScopesForTopStackFrame
  :: DapFrameIdDescriptor
  -> Id
  -> Env
  -> Adaptor ESTG [Scope]
generateScopesForTopStackFrame frameIdDesc closureId env = do
  (source, line, column, endLine, endColumn) <- getSourceAndPositionForStgPoint closureId (SP_RhsClosureExpr closureId)
  scopeVarablesRef <- getVariablesRef $ VariablesRef_StackFrameVariables frameIdDesc
  stgState <- getStgState
  varList <- forM (M.toList env) $ \(Id (Binder{..}), (_, atom)) -> do
    let BinderId u = binderId
        displayName = if binderScope == ModulePublic then cs binderName else cs (show u)
    (variableType, variableValue, varsRef) <- getAtomTypeAndValueM stgState frameIdDesc atom
    pure defaultVariable
      { variableName  = displayName
      , variableValue = cs variableValue
      , variableType  = Just (cs variableType)
      -- , variableEvaluateName = Just $ displayName <> " evaluate"
      , variableVariablesReference = varsRef
      }
  setVariables scopeVarablesRef varList
  pure
    [ defaultScope
      { scopeName = "Locals"
      , scopePresentationHint = Just ScopePresentationHintLocals
      , scopeVariablesReference = scopeVarablesRef
      , scopeSource = source
      , scopeLine = Just line
      , scopeColumn = Just column
      , scopeEndLine = Just endLine
      , scopeEndColumn = Just endColumn
      }
    ]

getHeapObjectSummary :: HeapObject -> String
getHeapObjectSummary = \case
  Con{..} -> "Con: " ++ show hoCon
  Closure{..} -> if hoCloMissing == 0
    then "Thunk: " ++ show hoName
    else "Closure: " ++ show hoName
  BlackHole{} -> "BlackHole"
  ApStack{} -> "ApStack"
  RaiseException{} -> "RaiseException"

getStgSourceLocJSON :: Id -> Adaptor ESTG (Maybe Text)
getStgSourceLocJSON i = do
  (mSource, startL, startC, endL, endC) <- getSourceAndPositionForStgPoint i (SP_Binding i)
  let mkPosObject line column = Aeson.object
        [ ("line",    Aeson.Number $ fromIntegral line)
        , ("column",  Aeson.Number $ fromIntegral column)
        ]
      srcLocJson = do
        Source{..} <- mSource
        path <- sourcePath
        pure . cs . Aeson.encode $ Aeson.object
          [ ("path",  Aeson.String path)
          , ("start", mkPosObject startL startC)
          , ("end",   mkPosObject endL endC)
          ]
  pure srcLocJson

getVariablesForHeapObject :: StgState -> DapFrameIdDescriptor -> HeapObject -> Adaptor ESTG [Variable]
getVariablesForHeapObject stgState frameIdDesc = \case
  Con{..} -> forM (zip [0..] hoConArgs) $ \(idx, atom) -> do
    (variableType, variableValue, varsRef) <- getAtomTypeAndValueM stgState frameIdDesc atom
    pure defaultVariable
      { variableName = cs $ "arg" ++ show idx
      , variableValue = cs variableValue
      , variableType = Just (cs variableType)
      , variableVariablesReference = varsRef
      }
  Closure{..} -> do
    srcLocJson <- getStgSourceLocJSON hoName
    let bodyVar = defaultVariable
          { variableName = "code"
          , variableValue = cs $ show hoName
          , variableEvaluateName = srcLocJson
          }
    {-
      TODO:
        show env in subnode
        show args in subnode
        show missing-args-count / is thunk?
    -}
    argVarList <- forM (zip [0..] hoCloArgs) $ \(idx, atom) -> do
      (variableType, variableValue, varsRef) <- getAtomTypeAndValueM stgState frameIdDesc atom
      pure defaultVariable
        { variableName = cs $ "arg" ++ show idx
        , variableValue = cs variableValue
        , variableType = Just (cs variableType)
        , variableVariablesReference = varsRef
        }
    envVarList <- forM (M.toList hoEnv) $ \(Id (Binder{..}), (_, atom)) -> do
      (variableType, variableValue, varsRef) <- getAtomTypeAndValueM stgState frameIdDesc atom
      let BinderId u = binderId
          displayName = if binderScope == ModulePublic then cs binderName else cs (show u)
      pure defaultVariable
        { variableName  = displayName
        , variableValue = cs variableValue
        , variableType  = Just (cs variableType)
        -- , variableEvaluateName = Just $ displayName <> " evaluate"
        , variableVariablesReference = varsRef
        }
    pure $ bodyVar : argVarList ++ envVarList
  BlackHole{..} -> do
    (ownerVarType, ownerVarValue, ownerVarsRef) <- getAtomTypeAndValueM stgState frameIdDesc $ ThreadId hoBHOwnerThreadId
    bodyVar <- case hoBHOriginalThunk of
      Closure{..} -> do
        srcLocJson <- getStgSourceLocJSON hoName
        pure . pure $ defaultVariable
          { variableName = "code"
          , variableValue = cs $ show hoName
          , variableEvaluateName = cs <$> srcLocJson
          }
      _ -> pure []
    let onwerVar = defaultVariable
          { variableName = "owner thread id"
          , variableValue = cs ownerVarValue
          , variableType = Just (cs ownerVarType)
          , variableVariablesReference = ownerVarsRef
          }

    queueVarList <- forM hoBHWaitQueue $ \tid -> do
      (variableType, variableValue, varsRef) <- getAtomTypeAndValueM stgState frameIdDesc $ ThreadId tid
      pure defaultVariable
        { variableName = "waiting thread id"
        , variableValue = cs variableValue
        , variableType = Just (cs variableType)
        , variableVariablesReference = varsRef
        }
    pure $ bodyVar ++ onwerVar : queueVarList
  ApStack{..} -> do
    resultVarList <- forM hoResult $ \atom -> do
      (variableType, variableValue, varsRef) <- getAtomTypeAndValueM stgState frameIdDesc atom
      pure defaultVariable
        { variableName = "latest result"
        , variableValue = cs variableValue
        , variableType = Just (cs variableType)
        , variableVariablesReference = varsRef
        }
      -- TODO: hoStack
    pure resultVarList
  RaiseException ex -> do
    (variableType, variableValue, varsRef) <- getAtomTypeAndValueM stgState frameIdDesc ex
    pure $ pure defaultVariable
      { variableName = "exception"
      , variableValue = cs variableValue
      , variableType = Just (cs variableType)
      , variableVariablesReference = varsRef
      }

----------------------------------------------------------------------------
generateScopes
  :: DapFrameIdDescriptor
  -> StackContinuation
  -- ^ The stack instruction that we're generating Scopes for
  -> Adaptor ESTG [Scope]
  -- ^ List of Scopes for this StackFrame
generateScopes frameIdDesc stackCont@(CaseOf _ closureId env _ _ _) = do
  (source, line, column, endLine, endColumn) <- getSourceAndPositionForStgPoint closureId (SP_RhsClosureExpr closureId)
  scopeVarablesRef <- getVariablesRef $ VariablesRef_StackFrameVariables frameIdDesc
  stgState <- getStgState
  varList <- forM (M.toList env) $ \(Id (Binder{..}), (_, atom)) -> do
    -- DMJ: for now everything is local.
    -- Inspect StaticOrigin to put things top-level, or as arguments, where applicable
    (variableType, variableValue, varsRef) <- getAtomTypeAndValueM stgState frameIdDesc atom
    let BinderId u = binderId
        displayName = if binderScope == ModulePublic then cs binderName else cs (show u)
    pure defaultVariable
      { variableName  = displayName
      , variableValue = cs variableValue
      , variableType  = Just (cs variableType)
      -- , variableEvaluateName = Just $ displayName <> " evaluate"
      , variableVariablesReference = varsRef
      }
  setVariables scopeVarablesRef varList
  pure
    [ defaultScope
      { scopeName = "Locals"
      , scopePresentationHint = Just ScopePresentationHintLocals
      , scopeVariablesReference = scopeVarablesRef
      , scopeSource = source
      , scopeLine = Just line
      , scopeColumn = Just column
      , scopeEndLine = Just endLine
      , scopeEndColumn = Just endColumn
      }
    ]
generateScopes frameIdDesc stackCont@(Update addr) = do
  scopeVarablesRef <- getVariablesRef $ VariablesRef_StackFrameVariables frameIdDesc
  stgState <- getStgState
  (variableType, variableValue, varsRef) <- getAtomTypeAndValueM stgState frameIdDesc $ HeapPtr addr
  setVariables scopeVarablesRef
    [ defaultVariable
      { variableName = "Thunk Address"
      , variableValue = cs variableValue
      , variableType = Just (cs variableType)
      , variableVariablesReference = varsRef
      }
    ]
  pure
    [ defaultScope
      { scopeName = "Locals: " <> T.pack (showStackCont stackCont)
      , scopePresentationHint = Just ScopePresentationHintLocals
      , scopeVariablesReference = scopeVarablesRef
      }
    ]
generateScopes frameIdDesc stackCont@(Apply atoms) = do
  scopeVarablesRef <- getVariablesRef $ VariablesRef_StackFrameVariables frameIdDesc
  stgState <- getStgState
  varList <- forM atoms $ \atom -> do
    (variableType, variableValue, varsRef) <- getAtomTypeAndValueM stgState frameIdDesc atom
    pure defaultVariable
      { variableName = "Closure argument"
      , variableValue = cs variableValue
      , variableType = Just (cs variableType)
      , variableVariablesReference = varsRef
      }
  setVariables scopeVarablesRef varList
  pure
    [ defaultScope
      { scopeName = "Locals: " <> T.pack (showStackCont stackCont)
      , scopePresentationHint = Just ScopePresentationHintLocals
      , scopeVariablesReference = scopeVarablesRef
      }
    ]
generateScopes frameIdDesc stackCont@(Catch atom blockAsync interruptible) = do
  scopeVarablesRef <- getVariablesRef $ VariablesRef_StackFrameVariables frameIdDesc
  stgState <- getStgState
  (variableType, variableValue, varsRef) <- getAtomTypeAndValueM stgState frameIdDesc atom
  setVariables scopeVarablesRef
    [ defaultVariable
      { variableName = "Exception Handler"
      , variableValue = cs variableValue
      , variableType = Just (cs variableType)
      , variableVariablesReference = varsRef
      }
    , defaultVariable
      { variableName = "BlockAsyncExceptions"
      , variableValue = T.pack (show blockAsync)
      , variableType = Just $ T.pack $ show (typeOf blockAsync)
      }
    , defaultVariable
      { variableName = "Interruptible"
      , variableValue = T.pack (show interruptible)
      , variableType = Just $ T.pack $ show (typeOf interruptible)
      }
    ]
  pure
    [ defaultScope
      { scopeName = "Locals"
      , scopePresentationHint = Just ScopePresentationHintLocals
      , scopeVariablesReference = scopeVarablesRef
      }
    ]
generateScopes frameIdDesc stackCont@(RestoreExMask _ blockAsync interruptible) = do
  scopeVarablesRef <- getVariablesRef $ VariablesRef_StackFrameVariables frameIdDesc
  setVariables scopeVarablesRef
    [ defaultVariable
      { variableName = "BlockAsyncExceptions"
      , variableValue = T.pack (show blockAsync)
      , variableType = Just $ T.pack $ show (typeOf blockAsync)
      }
    , defaultVariable
      { variableName = "Interruptible"
      , variableValue = T.pack (show interruptible)
      , variableType = Just $ T.pack $ show (typeOf interruptible)
      }
    ]
  pure
    [ defaultScope
      { scopeName = "Locals"
      , scopePresentationHint = Just ScopePresentationHintLocals
      , scopeVariablesReference = scopeVarablesRef
      }
    ]
generateScopes frameIdDesc stackCont@(RunScheduler reason) = do
  scopeVarablesRef <- getVariablesRef $ VariablesRef_StackFrameVariables frameIdDesc
  setVariables scopeVarablesRef
    [ defaultVariable
      { variableName = "Schedule Reason"
      , variableValue = showScheduleReason reason
      }
    ]
  pure
    [ defaultScope
      { scopeName = "Locals"
      , scopePresentationHint = Just ScopePresentationHintLocals
      , scopeVariablesReference = scopeVarablesRef
      }
    ] where
      showScheduleReason :: ScheduleReason            -> Text
      showScheduleReason SR_ThreadFinished            = "Thread Finished"
      showScheduleReason SR_ThreadFinishedFFICallback = "Thread Finished FFI Callback"
      showScheduleReason SR_ThreadBlocked             = "Thread Blocked"
      showScheduleReason SR_ThreadYield               = "Thread Yield"
      showScheduleReason SR_ThreadFinishedMain        = "Thread Finished Main"

generateScopes frameIdDesc stackCont@(Atomically atom) = do
  scopeVarablesRef <- getVariablesRef $ VariablesRef_StackFrameVariables frameIdDesc
  stgState <- getStgState
  (variableType, variableValue, varsRef) <- getAtomTypeAndValueM stgState frameIdDesc atom
  setVariables scopeVarablesRef
    [ defaultVariable
      { variableName = "STM action"
      , variableValue = cs variableValue
      , variableType = Just (cs variableType)
      , variableVariablesReference = varsRef
      }
    ]
  pure
    [ defaultScope
      { scopeName = "Locals"
      , scopePresentationHint = Just ScopePresentationHintLocals
      , scopeVariablesReference = scopeVarablesRef
      }
    ]
generateScopes frameIdDesc stackCont@(CatchRetry primaryAction alternativeAction isRunningAlternative _tlog) = do
  scopeVarablesRef <- getVariablesRef $ VariablesRef_StackFrameVariables frameIdDesc
  stgState <- getStgState
  (variableType1, variableValue1, varsRef1) <- getAtomTypeAndValueM stgState frameIdDesc primaryAction
  (variableType2, variableValue2, varsRef2) <- getAtomTypeAndValueM stgState frameIdDesc alternativeAction
  setVariables scopeVarablesRef
    [ defaultVariable
      { variableName = "First STM action"
      , variableValue = cs variableValue1
      , variableType = Just (cs variableType1)
      , variableVariablesReference = varsRef1
      }
    , defaultVariable
      { variableName = "Second STM action"
      , variableValue = cs variableValue2
      , variableType = Just (cs variableType2)
      , variableVariablesReference = varsRef2
      }
    , defaultVariable
      { variableName = "Is running alternative STM action"
      , variableValue = T.pack (show isRunningAlternative)
      , variableType = Just "Bool"
      }
      -- todo add tlog
    ]
  pure
    [ defaultScope
      { scopeName = "Locals: " <> T.pack (showStackCont stackCont)
      , scopePresentationHint = Just ScopePresentationHintLocals
      , scopeVariablesReference = scopeVarablesRef
      }
    ]
generateScopes frameIdDesc (CatchSTM action handler) = do
  scopeVarablesRef <- getVariablesRef $ VariablesRef_StackFrameVariables frameIdDesc
  stgState <- getStgState
  (variableType1, variableValue1, varsRef1) <- getAtomTypeAndValueM stgState frameIdDesc action
  (variableType2, variableValue2, varsRef2) <- getAtomTypeAndValueM stgState frameIdDesc handler
  setVariables scopeVarablesRef
    [ defaultVariable
      { variableName = "STM action"
      , variableValue = cs variableValue1
      , variableType = Just (cs variableValue1)
      , variableVariablesReference = varsRef1
      }
    , defaultVariable
      { variableName = "Exception Handler"
      , variableValue = cs variableValue2
      , variableType = Just (cs variableType2)
      , variableVariablesReference = varsRef2
      }
    ]
  pure
    [ defaultScope
      { scopeName = "Locals"
      , scopePresentationHint = Just ScopePresentationHintLocals
      , scopeVariablesReference = scopeVarablesRef
      }
    ]
generateScopes frameIdDesc stackCont@DataToTagOp = do
  scopeVarablesRef <- getVariablesRef $ VariablesRef_StackFrameVariables frameIdDesc
  pure
    [ defaultScope
      { scopeName = "Locals"
      , scopePresentationHint = Just ScopePresentationHintLocals
      , scopeVariablesReference = scopeVarablesRef
      }
    ]
generateScopes frameIdDesc stackCont@(RaiseOp atom) = do
  scopeVarablesRef <- getVariablesRef $ VariablesRef_StackFrameVariables frameIdDesc
  stgState <- getStgState
  (variableType, variableValue, varsRef) <- getAtomTypeAndValueM stgState frameIdDesc atom
  setVariables scopeVarablesRef
    [ defaultVariable
      { variableName = "RaiseOp"
      , variableValue = cs variableValue
      , variableType = Just (cs variableType)
      , variableVariablesReference = varsRef
      }
    ]
  pure
    [ defaultScope
      { scopeName = "Locals"
      , scopePresentationHint = Just ScopePresentationHintLocals
      , scopeVariablesReference = scopeVarablesRef
      }
    ]
generateScopes frameIdDesc stackCont@(KeepAlive atom) = do
  scopeVarablesRef <- getVariablesRef $ VariablesRef_StackFrameVariables frameIdDesc
  stgState <- getStgState
  (variableType, variableValue, varsRef) <- getAtomTypeAndValueM stgState frameIdDesc atom
  setVariables scopeVarablesRef
    [ defaultVariable
      { variableName = "Managed Object"
      , variableValue = cs variableValue
      , variableType = Just (cs variableType)
      , variableVariablesReference = varsRef
      }
    ]
  pure
    [ defaultScope
      { scopeName = "Locals"
      , scopePresentationHint = Just ScopePresentationHintLocals
      , scopeVariablesReference = scopeVarablesRef
      }
    ]
generateScopes frameIdDesc stackCont@(DebugFrame (RestoreProgramPoint maybeId _)) = do
  scopeVarablesRef <- getVariablesRef $ VariablesRef_StackFrameVariables frameIdDesc
  setVariables scopeVarablesRef
    [ defaultVariable
      { variableName = "DebugFrame"
      , variableValue = cs (show maybeId)
      , variableType = Just "RestoreProgramPoint"
      }
    ]
  pure
    [ defaultScope
      { scopeName = "Locals"
      , scopePresentationHint = Just ScopePresentationHintLocals
      , scopeVariablesReference = scopeVarablesRef
      }
    ]

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

getAtomTypeAndValueM
  :: StgState
  -> DapFrameIdDescriptor
  -> Atom
  -> Adaptor ESTG (String, String, Int)
getAtomTypeAndValueM ss@StgState{..} frameIdDesc = \case
  HeapPtr addr
    | Just o <- IntMap.lookup addr ssHeap
    -> do
      varsRef <- getVariablesRef $ VariablesRef_HeapObject frameIdDesc addr
      pure ("HeapPtr", show addr ++ " " ++ getHeapObjectSummary o ++ "\n --- \n" ++ LazyText.unpack (PP.pShowNoColor o), varsRef)
  atom
    | (t, v) <- getAtomTypeAndValue ss atom
    -> pure (t, v, 0)

getAtomTypeAndValue
  :: StgState
  -> Atom
  -> (String, String)
getAtomTypeAndValue StgState{..} = \case
  HeapPtr addr
    | Just o <- IntMap.lookup addr ssHeap
    -> ("HeapPtr", show addr ++ "\n --- \n" ++ LazyText.unpack (PP.pShowNoColor o))
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

-- resource handling

getsApp f = f <$> getDebugSession
modifyApp = updateDebugSession


-------------------------------------
{-
  IDEA:
    pure design
      use pure and unique resource descriptors to select items from StgState
      maintain a bimap between the pure resource descriptors and DAP integer ids

    IMPORTANT: avoid use of counters
    BENEFIT:
      DAP request order independence
      no resource caching is needed
      stateless
      use of descriptive resource identification instead of integers

  IDEA:
    ResourceID ADT - structured key
    idMap :: ResourceID -> Int

    DAP request argument -> estg domian idientifiers
    request argument's id -> estg domain 

  resource ids
    threadRef     = thread id
    frameRef      = thread id + frame index
    scopeRef      = thread id + frame index + argument index
    variablesRef  = ??
    sourceRef

  HINT: VariablesRef -> [Variable]

  DAP id types:
    thread
    stack frame
    variable


  Threads             args: NONE
    StackTrace        args: threadId
      Scopes          args: frameId
        Variables     args: variablesRef
          ...
            Variables
-}

type StackFrameIndex = Int

data DapFrameIdDescriptor
  = FrameId_CurrentThreadTopStackFrame
  | FrameId_ThreadStackFrame ThreadId StackFrameIndex
  deriving (Show, Eq, Ord)

data DapVariablesRefDescriptor
  = VariablesRef_StackFrameVariables  DapFrameIdDescriptor
  | VariablesRef_HeapObject           DapFrameIdDescriptor Int
  deriving (Show, Eq, Ord)

data SourceCodeDescriptor
  = Haskell   PackageName QualifiedModuleName
  | GhcCore   PackageName QualifiedModuleName
  | GhcStg    PackageName QualifiedModuleName
  | Cmm       PackageName QualifiedModuleName
  | Asm       PackageName QualifiedModuleName
  | ExtStg    PackageName QualifiedModuleName
  | FFICStub  PackageName QualifiedModuleName
  | FFIHStub  PackageName QualifiedModuleName
  | ModInfo   PackageName QualifiedModuleName
  | ForeignC  PackageName FilePath
  deriving (Show, Read, Eq, Ord)

data DapSourceRefDescriptor
  = SourceRef_SourceFileInFullpak SourceCodeDescriptor
  deriving (Show, Read, Eq, Ord)

getSourcePath :: SourceCodeDescriptor -> FilePath
getSourcePath = \case
  Haskell  pkg mod -> "haskell" </> cs pkg </> cs mod </> "module.hs"
  GhcCore  pkg mod -> "haskell" </> cs pkg </> cs mod </> "module.ghccore"
  GhcStg   pkg mod -> "haskell" </> cs pkg </> cs mod </> "module.ghcstg"
  Cmm      pkg mod -> "haskell" </> cs pkg </> cs mod </> "module.cmm"
  Asm      pkg mod -> "haskell" </> cs pkg </> cs mod </> "module.s"
  ExtStg   pkg mod -> "haskell" </> cs pkg </> cs mod </> "module.stgbin"
  FFICStub pkg mod -> "haskell" </> cs pkg </> cs mod </> "module_stub.c"
  FFIHStub pkg mod -> "haskell" </> cs pkg </> cs mod </> "module_stub.h"
  ModInfo  pkg mod -> "haskell" </> cs pkg </> cs mod </> "module.info"
  ForeignC _pkg path -> cs path

getSourceName :: SourceCodeDescriptor -> String
getSourceName = \case
  Haskell  pkg mod -> "haskell" </> cs pkg </> cs mod <> ".hs"
  GhcCore  pkg mod -> "haskell" </> cs pkg </> cs mod <> ".ghccore"
  GhcStg   pkg mod -> "haskell" </> cs pkg </> cs mod <> ".ghcstg"
  Cmm      pkg mod -> "haskell" </> cs pkg </> cs mod <> ".cmm"
  Asm      pkg mod -> "haskell" </> cs pkg </> cs mod <> ".s"
  ExtStg   pkg mod -> "haskell" </> cs pkg </> cs mod <> ".stgbin.hs"
  FFICStub pkg mod -> "haskell" </> cs pkg </> cs mod <> "_stub.c"
  FFIHStub pkg mod -> "haskell" </> cs pkg </> cs mod <> "_stub.h"
  ModInfo  pkg mod -> "haskell" </> cs pkg </> cs mod <> ".info"
  ForeignC _pkg path -> cs path

getSourceFromSourceRefDescriptor :: DapSourceRefDescriptor -> Adaptor ESTG Source
getSourceFromSourceRefDescriptor sourceRefDesc@(SourceRef_SourceFileInFullpak srcDesc) = do
  srcDescSet <- getsApp sourceCodeSet
  extraSources <- case srcDesc of
    Haskell packageName qualModName
      | cStub <- FFICStub packageName qualModName
      , hStub <- FFIHStub packageName qualModName
      -> Just <$> sequence (
      [ getSourceFromSourceRefDescriptor (SourceRef_SourceFileInFullpak $ ExtStg   packageName qualModName)
      , getSourceFromSourceRefDescriptor (SourceRef_SourceFileInFullpak $ GhcCore  packageName qualModName)
      , getSourceFromSourceRefDescriptor (SourceRef_SourceFileInFullpak $ GhcStg   packageName qualModName)
      , getSourceFromSourceRefDescriptor (SourceRef_SourceFileInFullpak $ Cmm      packageName qualModName)
      , getSourceFromSourceRefDescriptor (SourceRef_SourceFileInFullpak $ Asm      packageName qualModName)
      , getSourceFromSourceRefDescriptor (SourceRef_SourceFileInFullpak $ ModInfo  packageName qualModName)
      ] ++
      [ getSourceFromSourceRefDescriptor (SourceRef_SourceFileInFullpak cStub)
      | Set.member cStub srcDescSet
      ] ++
      [ getSourceFromSourceRefDescriptor (SourceRef_SourceFileInFullpak hStub)
      | Set.member hStub srcDescSet
      ])

    _ -> pure Nothing

  let sourceName = cs $ getSourceName srcDesc
  sourceRef <- getSourceRef sourceRefDesc
  ESTG {..} <- getDebugSession
  pure defaultSource
    { sourceName            = Just $ sourceName -- used in source tree children
    , sourceSourceReference = Just sourceRef
    , sourcePath            = Just $ sourceName -- used in code tab title
    , sourceSources         = extraSources
    }

getFrameId :: DapFrameIdDescriptor -> Adaptor ESTG Int
getFrameId key = do
  getsApp (Bimap.lookup key . dapFrameIdMap) >>= \case
    Just frameId -> pure frameId
    Nothing -> do
      frameId <- getsApp (succ . Bimap.size . dapFrameIdMap)
      modifyApp $ \s -> s {dapFrameIdMap = Bimap.insert key frameId (dapFrameIdMap s)}
      pure frameId

getVariablesRef :: DapVariablesRefDescriptor -> Adaptor ESTG Int
getVariablesRef key = do
  getsApp (Bimap.lookup key . dapVariablesRefMap) >>= \case
    Just varRef -> pure varRef
    Nothing -> do
      varRef <- getsApp (succ . Bimap.size . dapVariablesRefMap)
      modifyApp $ \s -> s {dapVariablesRefMap = Bimap.insert key varRef (dapVariablesRefMap s)}
      pure varRef

getSourceRef :: DapSourceRefDescriptor -> Adaptor ESTG Int
getSourceRef key = do
  -- NOTE: Source code related db is populated at initialization
  getsApp (Bimap.lookup key . dapSourceRefMap) >>= \case
    Just srcRef -> pure srcRef
    Nothing     -> error $ "unknown source descriptor: " ++ show key

setVariables :: Int -> [Variable] -> Adaptor ESTG ()
setVariables variablesRef variableList = do
  modifyApp $ \s -> s {dapVariablesRefStore = IntMap.insert variablesRef variableList (dapVariablesRefStore s)}

getVariables :: Int -> Adaptor ESTG [Variable]
getVariables variablesRef = do
  ESTG {..} <- getDebugSession
  case IntMap.lookup variablesRef dapVariablesRefStore of
    Nothing       -> sendError (ErrorMessage (T.pack $ "Unknown variablesRef: " ++ show variablesRef)) Nothing
    Just varList  -> pure varList

----------------------------------------------------------------------------
-- | Note: this `Int` should act as if it were an unsigned 31-bit integer (0, 2^31).

getAllVars :: Adaptor ESTG [Variable]
getAllVars = getsApp (concat . IntMap.elems . dapVariablesRefStore)

-- | Invoked when a StepEvent has occurred
resetObjectLifetimes :: Adaptor ESTG ()
resetObjectLifetimes = do
  modifyApp $ \s -> s
    { dapFrameIdMap         = Bimap.empty
    , dapVariablesRefMap    = Bimap.empty
    , dapVariablesRefStore  = mempty
    }

getFreshBreakpointId :: Adaptor ESTG BreakpointId
getFreshBreakpointId = do
  bkpId <- getsApp nextFreshBreakpointId
  modifyApp $ \s -> s { nextFreshBreakpointId = nextFreshBreakpointId s + 1 }
  pure bkpId

type PackageName = Text
type QualifiedModuleName = Text
type BreakpointId = Int
type SourceId = Int
type ThreadId = Int

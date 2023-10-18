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
import           System.FilePath                       ( (-<.>), (</>), takeDirectory, takeFileName, takeExtension, dropExtension, splitFileName, splitPath, joinPath, splitDirectories)
import           Text.Read                             ( readMaybe )
import qualified Data.ByteString.Lazy.Char8            as BL8 ( pack, unpack, fromStrict, toStrict )
import qualified Control.Concurrent.Chan.Unagi.Bounded as Unagi
import           Control.Concurrent.MVar               ( MVar )
import qualified Control.Concurrent.MVar               as MVar
import           Control.Concurrent                    ( forkIO )
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
import           Stg.Interpreter.Debugger.TraverseState
import           Stg.Interpreter.GC.GCRef
import           Stg.IO
import           Stg.Program
import           Stg.Fullpak
import           Data.Yaml                             hiding (Array)
import qualified Text.Pretty.Simple                    as PP
----------------------------------------------------------------------------
import           DAP                                   hiding (send)
----------------------------------------------------------------------------
import           DapBase
import           CustomCommands
import           GraphProtocol.Commands
import           GraphProtocol.Server
import           SourceCode
import           SourceLocation
import           Breakpoints
import           Inspect.Value.Atom
import           Inspect.Value
import           Inspect.Stack
import           Graph
----------------------------------------------------------------------------
-- | DAP entry point
-- Extracts configuration information from the environment
-- Opens a listen socket on a port (defaulting to '4711')
-- Converts the 'Socket' to a 'Handle' for convenience
main :: IO ()
main = do
  (config, graphConfig) <- getConfig
  forkIO $ runGraphServer graphConfig
  finally (runDAPServer config talk) $ do
    putStrLn "dap finished, bye!"

----------------------------------------------------------------------------
-- | Fetch config from environment, fallback to sane defaults
getConfig :: IO (ServerConfig, GraphServerConfig)
getConfig = do
  let
    hostDefault       = "0.0.0.0"
    portDefault       = 4711
    graphPortDefault  = 4721
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
  config <- ServerConfig
    <$> do fromMaybe hostDefault <$> lookupEnv "DAP_HOST"
    <*> do fromMaybe portDefault . (readMaybe =<<) <$> do lookupEnv "DAP_PORT"
    <*> pure capabilities
    <*> pure True

  graphConfig <- GraphServerConfig
    <$> do fromMaybe hostDefault <$> lookupEnv "DAP_HOST"
    <*> do fromMaybe graphPortDefault . (readMaybe =<<) <$> do lookupEnv "DAP_GRAPH_PORT"
    <*> pure True

  pure (config, graphConfig)

findProgram :: String -> IO [FilePath]
findProgram globPattern = do
  let isPattern = any (`elem` ("[*?" :: String))
      startDir = joinPath . takeWhile (not . isPattern) . splitPath $ takeDirectory globPattern
  Glob.find Glob.always (Glob.filePath Glob.~~? globPattern) startDir

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
-- | Intialize ESTG interpreter
----------------------------------------------------------------------------
initESTG :: AttachArgs -> Adaptor ESTG ()
initESTG AttachArgs {..} = do
  programPath <- (liftIO $ findProgram program) >>= \case
    [fname] -> pure fname
    []      -> sendError (ErrorMessage (T.pack $ unlines ["No program found at:", program])) Nothing
    names   -> sendError (ErrorMessage (T.pack $ unlines $ ["Ambiguous program path:", program, "Use more specific path pattern to fix the issue.", "Multiple matches:"] ++ names)) Nothing
  fullpakPath <- case takeExtension programPath of
    ".fullpak" -> do
      -- handle .fullpak
      pure programPath
    _ -> do
      -- handle .ghc_stgapp
      let fname = programPath -<.> ".fullpak"
      liftIO $ mkFullpak programPath False False fname
      pure fname

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
  (graphAsyncI, graphAsyncO) <- liftIO (Unagi.newChan 100)
  let graphChan = GraphChan
        { graphAsyncEventIn   = graphAsyncI
        , graphAsyncEventOut  = graphAsyncO
        }
      estg = ESTG
        { debuggerChan          = dbgChan
        , fullPakPath           = fullpakPath
        , breakpointMap         = mempty
        , sourceCodeSet         = Set.fromList sourceCodeList
        , unitIdMap             = unitIdMap
        , haskellSrcPathMap     = haskellSrcPathMap
        , dapSourceNameMap      = Bimap.fromList [(cs $ getSourceName d, d) | d <- sourceCodeList]
        , dapSourceRefMap       = Bimap.fromList $ zip sourceCodeList [1..]
        , dapFrameIdMap         = Bimap.empty
        , dapVariablesRefMap    = Bimap.empty
        , dapStackFrameCache    = mempty
        , nextFreshBreakpointId = 1
        }
  flip catch handleDebuggerExceptions $ do
    registerNewDebugSession __sessionId estg
      [ \_withAdaptor -> loadAndRunProgram True True fullpakPath [] dbgChan DbgStepByStep False defaultDebugSettings
      , handleDebugEvents dbgChan
      , handleGraphEvents graphChan
      ]
    liftIO $ registerGraphChan __sessionId graphChan

----------------------------------------------------------------------------
-- | Graph Event Handler
handleGraphEvents :: GraphChan -> (Adaptor ESTG () -> IO ()) -> IO ()
handleGraphEvents GraphChan{..} withAdaptor = forever $ do
  graphEvent <- liftIO (Unagi.readChan graphAsyncEventOut)
  withAdaptor . flip catch handleDebuggerExceptions $ do
    let sendEvent ev = sendSuccesfulEvent ev . setBody
    case graphEvent of
      GraphEventShowValue nodeId
        | Just programPoint <- readMaybe $ cs nodeId
        -> do
          let getStgPointFromProgramPoint = \case
                PP_Global     -> Nothing
                PP_Apply _ pp -> getStgPointFromProgramPoint pp
                PP_StgPoint p -> Just p
          case getStgPointFromProgramPoint programPoint of
            Nothing -> pure ()
            Just stgPoint -> do
              srcLocJson <- getStgSourceLocJSON stgPoint
              sendEvent (EventTypeCustom "showCode") srcLocJson

      GraphEventShowValue nodeId
        | Just root@(ns, idx) <- readMaybe $ cs nodeId
        -> do
          atom <- valueToAtom ns idx
          var <- getVariableForAtom "" (ValueRoot_Value root) atom
          sendEvent (EventTypeCustom "showValue") $ object
            [ "variable" .= var
            ]

      GraphEventShowValue nodeId -> do
        logError $ BL8.pack ("invalid node id format: " <> cs nodeId)

----------------------------------------------------------------------------
-- | Debug Event Handler
handleDebugEvents :: DebuggerChan -> (Adaptor ESTG () -> IO ()) -> IO ()
handleDebugEvents DebuggerChan{..} withAdaptor = forever $ do
  dbgEvent <- liftIO (Unagi.readChan dbgAsyncEventOut)
  withAdaptor . flip catch handleDebuggerExceptions $ do
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
    mapM getSourceFromSourceCodeDescriptor $ filter shouldInclude $ Set.toList srcSet

----------------------------------------------------------------------------
talk (CustomCommand "getSourceLinks") = customCommandGetSourceLinks
----------------------------------------------------------------------------
talk (CustomCommand "selectVariableGraphNode") = customCommandSelectVariableGraphNode
----------------------------------------------------------------------------
talk (CustomCommand "showVariableGraphStructure") = customCommandShowVariableGraphStructure
----------------------------------------------------------------------------
talk (CustomCommand "showCallGraph") = customCommandShowCallGraph
----------------------------------------------------------------------------
talk CommandModules = do
  sendModulesResponse (ModulesResponse [] Nothing)
----------------------------------------------------------------------------
talk CommandPause = do
  sendAndWait CmdStop
  sendPauseResponse
----------------------------------------------------------------------------
talk CommandSetBreakpoints = commandSetBreakpoints
----------------------------------------------------------------------------
talk CommandStackTrace = commandStackTrace
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
talk CommandThreads = commandThreads
----------------------------------------------------------------------------
talk CommandScopes = commandScopes
----------------------------------------------------------------------------
talk CommandVariables = do
  VariablesArguments {..} <- getArguments
  getsApp (Bimap.lookupR variablesArgumentsVariablesReference . dapVariablesRefMap) >>= \case
    Just (VariablesRef_StackFrameVariables frameIdDesc) -> do
      variables <- getVariablesForStackFrame frameIdDesc
      sendVariablesResponse (VariablesResponse variables)
    Just (VariablesRef_Value valueRoot valueNameSpace addr) -> do
      variables <- getVariablesForValue valueRoot valueNameSpace addr
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


-- + on watch causes "CommandEvaluate"
-- right click - set value - [127.0.0.1:49599][INFO][GOT cmd CommandSetVariable]
-- right click - copy value - [127.0.0.1:49599][INFO][GOT cmd CommandEvaluate]
-- save breakpoints from breakpoints request into AdaptrClient set, set them on the interpreter after configuration done (not attach)

{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase          #-}
module Graph where

import           System.FilePath                       ( (-<.>), (</>), takeDirectory, takeFileName, takeExtension, dropExtension, splitFileName, splitPath, joinPath, splitDirectories)
import           Data.String.Conversions               (cs)
import           Control.Monad
import           Control.Monad.IO.Class                (liftIO)
import qualified Data.Text                             as T
import qualified Data.Map.Strict                       as Map
import qualified Data.IntMap.Strict                    as IntMap
import qualified Data.Bimap                            as Bimap
import qualified Data.Set                              as Set

import qualified Data.Map.Strict as StrictMap
import Data.List (intercalate, foldl', sortOn)
import System.IO


import           Stg.Interpreter.Base                  hiding (lookupEnv, getCurrentThreadState, Breakpoint)
import           Stg.Interpreter.GC.GCRef
import           Stg.Interpreter.Debugger.TraverseState
import           Stg.Interpreter.Debugger.Retainer
import           Stg.IRLocation

import DAP
import DapBase
import CustomCommands
import GraphProtocol.Server
import GraphProtocol.Commands
import Inspect.Value.Atom
import SharedFolder
import Region

customCommandSelectVariableGraphNode :: Adaptor ESTG ()
customCommandSelectVariableGraphNode = do
  SelectVariableGraphNodeArguments {..} <- getArguments
  getsApp (Bimap.lookupR selectVariableGraphNodeArgumentsVariablesReference . dapVariablesRefMap) >>= \case
    Just VariablesRef_StackFrameVariables{} -> do
      sendSuccesfulEmptyResponse
    Just (VariablesRef_Value _valueRoot valueNameSpace addr) -> do
      liftIO $ sendGraphCommand SelectNode
        { selectNodeRequest = "selectNode"
        , selectNodeNodeId  = cs $ show (valueNameSpace, addr)
        }
      sendSuccesfulEmptyResponse
    Nothing -> sendError (ErrorMessage (T.pack $ "Unknown variables ref: " ++ show selectVariableGraphNodeArgumentsVariablesReference)) Nothing

customCommandShowRetainerGraph :: Adaptor ESTG ()
customCommandShowRetainerGraph = do
  ShowRetainerGraphArguments {..} <- getArguments
{-
  = ShowRetainerGraphArguments
  { showRetainerGraphArgumentsVariablesReference :: Int
-}
  getsApp (Bimap.lookupR showRetainerGraphArgumentsVariablesReference . dapVariablesRefMap) >>= \case
    Just (VariablesRef_Value _valueRoot valueNameSpace addr) -> do
      stgState@StgState{..} <- getStgState
      valueSummary <- getValueSummary valueNameSpace addr

      -- generate names
      ESTG {..} <- getDebugSession
      let nodesFname = fullPakPath ++ "-graph-nodes.tsv"
          edgesFname = fullPakPath ++ "-graph-edges.tsv"
      hostNodesFname <- mapFilePathToHost nodesFname
      hostEdgesFname <- mapFilePathToHost edgesFname

      liftIO $ exportRetainerGraph nodesFname edgesFname stgState $ encodeRef addr valueNameSpace
      liftIO $ sendGraphCommand LoadGraph
        { loadGraphRequest  = "loadGraph"
        , loadGraphTitle    = cs $ show addr ++ " " ++ valueSummary
        , loadGraphNodesFilepath = Just $ cs hostNodesFname
        , loadGraphEdgesFilepath = cs hostEdgesFname
        }
      sendSuccesfulEmptyResponse
    Just v -> sendError (ErrorMessage (T.pack $ "Visualization is not yet supported for: " ++ show v)) Nothing
    Nothing -> sendError (ErrorMessage (T.pack $ "Unknown variables ref: " ++ show showRetainerGraphArgumentsVariablesReference)) Nothing

customCommandShowVariableGraphStructure :: Adaptor ESTG ()
customCommandShowVariableGraphStructure = do
  ShowVariableGraphStructureArguments {..} <- getArguments
  getsApp (Bimap.lookupR showVariableGraphStructureArgumentsVariablesReference . dapVariablesRefMap) >>= \case
    Just r@(VariablesRef_RegionInstance region idx) -> do
      stgState@StgState{..} <- getStgState
      case Map.lookup region ssRegionInstances of
        Just instanceMap
          | Just (start, end) <- IntMap.lookup idx instanceMap
          -> do
              -- generate names
              ESTG {..} <- getDebugSession
              let nodesFname = fullPakPath ++ "-graph-nodes.tsv"
                  edgesFname = fullPakPath ++ "-graph-edges.tsv"
              hostNodesFname <- mapFilePathToHost nodesFname
              hostEdgesFname <- mapFilePathToHost edgesFname

              let heap  = getRegionHeap (asNextHeapAddr start) (asNextHeapAddr end) ssHeap
                  title = case region of
                            IRRegion{}      -> "region instance " ++ show idx
                            EventRegion{..} -> cs regionName ++ " instance " ++ show idx
              liftIO $ exportHeapGraph nodesFname edgesFname heap
              liftIO $ sendGraphCommand LoadGraph
                { loadGraphRequest       = "loadGraph"
                , loadGraphTitle         = cs title
                , loadGraphNodesFilepath = Just $ cs hostNodesFname
                , loadGraphEdgesFilepath = cs hostEdgesFname
                }
              sendSuccesfulEmptyResponse

        _ -> sendError (ErrorMessage (T.pack $ "Unknown region instance: " ++ show r)) Nothing

    Just VariablesRef_StackFrameVariables{} -> do
      -- TODO: create graph from the full stack frame
      sendSuccesfulEmptyResponse
    Just (VariablesRef_Value _valueRoot valueNameSpace addr) -> do
      stgState@StgState{..} <- getStgState
      valueSummary <- getValueSummary valueNameSpace addr

      -- generate names
      ESTG {..} <- getDebugSession
      let nodesFname = fullPakPath ++ "-graph-nodes.tsv"
          edgesFname = fullPakPath ++ "-graph-edges.tsv"
      hostNodesFname <- mapFilePathToHost nodesFname
      hostEdgesFname <- mapFilePathToHost edgesFname

      liftIO $ exportReachableGraph nodesFname edgesFname stgState $ encodeRef addr valueNameSpace
      liftIO $ sendGraphCommand LoadGraph
        { loadGraphRequest  = "loadGraph"
        , loadGraphTitle    = cs $ show addr ++ " " ++ valueSummary
        , loadGraphNodesFilepath = Just $ cs hostNodesFname
        , loadGraphEdgesFilepath = cs hostEdgesFname
        }
      sendSuccesfulEmptyResponse
    Nothing -> sendError (ErrorMessage (T.pack $ "Unknown variables ref: " ++ show showVariableGraphStructureArgumentsVariablesReference)) Nothing

customCommandShowCallGraph :: Adaptor ESTG ()
customCommandShowCallGraph = do
  ESTG {..} <- getDebugSession
  let nodesFname = fullPakPath ++ "-graph-nodes.tsv"
      edgesFname = fullPakPath ++ "-graph-edges.tsv"
  hostNodesFname <- mapFilePathToHost nodesFname
  hostEdgesFname <- mapFilePathToHost edgesFname
  StgState{..} <- getStgState
  liftIO $ do
    writeCallGraphEdges edgesFname ssCallGraph
    writeCallGraphNodes nodesFname ssCallGraph
    sendGraphCommand LoadGraph
      { loadGraphRequest  = "loadGraph"
      , loadGraphTitle    = cs $ takeFileName fullPakPath ++ " call graph"
      , loadGraphNodesFilepath = Just $ cs hostNodesFname
      , loadGraphEdgesFilepath = cs hostEdgesFname
      }
  sendSuccesfulEmptyResponse

writeCallGraphEdges :: FilePath -> CallGraph -> IO ()
writeCallGraphEdges fname CallGraph{..} = do
  let showCallType = \case
        SO_CloArg         -> "unknown"
        SO_Let            -> "known"
        SO_Scrut          -> "unknown"
        SO_AltArg         -> "unknown"
        SO_TopLevel       -> "known"
        SO_Builtin        -> "known"
        SO_ClosureResult  -> "unknown"
  withFile fname WriteMode $ \h -> do
    hPutStrLn h $ intercalate "\t"
      [ "Source"
      , "Target"
      , "Label"
      , "count"
      , "static-origin"
      , "call-site-type"
      ]
    forM_ (sortOn (negate . snd) $ StrictMap.toList cgInterClosureCallGraph) $ \((so, from, to), count) -> do
      hPutStrLn h $ intercalate "\t"
        [ show from
        , show to
        , show count
        , show count
        , show so
        , showCallType so
        ]
    forM_ (sortOn (negate . snd) $ StrictMap.toList cgIntraClosureCallGraph) $ \((from, so, to), count) -> do
      hPutStrLn h $ intercalate "\t"
        [ show from
        , show to
        , show count
        , show count
        , "direct"
        , "known"
        ]
writeCallGraphNodes :: FilePath -> CallGraph -> IO ()
writeCallGraphNodes fname CallGraph{..} = do
  withFile fname WriteMode $ \h -> do
    hPutStrLn h $ intercalate "\t"
      [ "Id"
      , "Label"
--      , "package-id"
--      , "module"
      , "partition2"
      ]
    let nodes = Set.fromList . concat $
          [[from, to] | (_so, from, to) <- StrictMap.keys cgInterClosureCallGraph] ++
          [[from, to] | (from, _so, to) <- StrictMap.keys cgIntraClosureCallGraph]


    forM_ nodes $ \node ->
      hPutStrLn h $ intercalate "\t"
        [ show node
        , getLabelForProgramPoint node
        , case node of
            PP_StgPoint{} -> "PP_StgPoint"
            PP_Global{}   -> "PP_Global"
            PP_Apply{}    -> "PP_Apply"
        ]

getLabelForProgramPoint :: ProgramPoint -> String
getLabelForProgramPoint = \case
  PP_Global     -> "global scope"
  PP_Apply n pp -> "apply " ++ show n ++ " " ++ getLabelForProgramPoint pp
  PP_StgPoint p -> getLabelForStgPoint p

getLabelForStgPoint :: StgPoint -> String
getLabelForStgPoint = \case
  SP_CaseScrutineeExpr{..} -> getLabelForStgId spScrutineeResultName
  SP_LetExpr{..}           -> getLabelForStgPoint spParent
  SP_LetNoEscapeExpr{..}   -> getLabelForStgPoint spParent
  SP_RhsClosureExpr{..}    -> getLabelForStgId spRhsBinderName
  SP_AltExpr{..}           -> "alt " ++ show spAltIndex ++ ": " ++ getLabelForStgId spScrutineeResultName
  SP_RhsCon{..}            -> getLabelForStgId spRhsBinderName
  SP_Binding{..}           -> getLabelForStgId spBinderName
  SP_Tickish{..}           -> getLabelForStgPoint spParent

getLabelForStgId :: StgId -> String
getLabelForStgId StgId{..} = cs (siUnitId <> "_" <> siModuleName <> "." <> siName) <> maybe "" (\u -> "_" <> show u) siUnique

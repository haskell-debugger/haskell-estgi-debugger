{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase          #-}
module Graph where

import           System.FilePath                       ( (-<.>), (</>), takeDirectory, takeFileName, takeExtension, dropExtension, splitFileName, splitPath, joinPath, splitDirectories)
import           Data.String.Conversions               (cs)
import           Control.Monad.IO.Class                (liftIO)
import qualified Data.Text                             as T
import qualified Data.IntMap.Strict                    as IntMap
import qualified Data.Bimap                            as Bimap

import           Stg.Interpreter.Base                  hiding (lookupEnv, getCurrentThreadState, Breakpoint)
import           Stg.Interpreter.GC.GCRef
import           Stg.Interpreter.Debugger.TraverseState
import           Stg.Interpreter.Debug

import DAP
import DapBase
import           CustomCommands
import GraphProtocol.Server
import GraphProtocol.Commands
import Inspect.Value.Atom

getValueSummary _ valueNameSpace addr = Just $ "TODO: title " ++ show (valueNameSpace, addr)

customCommandShowVariableGraphStructure :: Adaptor ESTG ()
customCommandShowVariableGraphStructure = do
  ShowVariableGraphStructureArguments {..} <- getArguments
  getsApp (Bimap.lookupR showVariableGraphStructureArgumentsVariablesReference . dapVariablesRefMap) >>= \case
    Just VariablesRef_StackFrameVariables{} -> do
      -- TODO: create graph from the full stack frame
      sendSuccesfulEmptyResponse
    Just (VariablesRef_Value _valueRoot valueNameSpace addr) -> do
      stgState@StgState{..} <- getStgState
      case getValueSummary stgState valueNameSpace addr of
        Nothing -> sendError (ErrorMessage (T.pack $ "Unknown object: " ++ show (valueNameSpace, addr))) Nothing
        Just valueSummary -> do
          {-
            TODO
              - generate facts for transitive closure of reachable objects into file
              - send to graph command service
          -}
          let fname = "/home/csaba/call-graphs/q3mapviewer-call-graphXXX.tsv"
          {-
            encodeRef :: Int -> RefNamespace -> GCSymbol
          -}
          liftIO $ do
            --exportReachableGraph :: FilePath -> StgState -> GCSymbol -> IO ()
            exportReachableGraph fname stgState $ encodeRef addr valueNameSpace
          liftIO $ sendGraphCommand LoadGraph
            { loadGraphRequest  = "loadGraph"
            , loadGraphTitle    = cs $ show addr ++ " " ++ valueSummary
            , loadGraphFilepath = cs fname
            }
          sendSuccesfulEmptyResponse

{-
  { loadGraphRequest  :: Text
  , loadGraphTitle    :: Text
  , loadGraphFilepath :: Text
      , "filepath"  .= Aeson.String "/home/csaba/call-graphs/q3mapviewer-call-graph.tsv"
    | Just o <- IntMap.lookup addr ssHeap
    -> do
      varsRef <- getVariablesRef $ VariablesRef_HeapObject frameIdDesc addr
      pure ("HeapPtr", show addr ++ " " ++ getHeapObjectSummary o ++ "\n --- \n" ++ LazyText.unpack (PP.pShowNoColor o), varsRef)
      stgState <- getStgState
      ESTG {..} <- getDebugSession
      sendSuccesfulEmptyResponse
-}

customCommandShowCallGraph :: Adaptor ESTG ()
customCommandShowCallGraph = do
  --sendAndWait (CmdInternal "gc")
  {-
    TODO:
      - export call graph
      - send command to gephi
  -}
  let fname = "/home/csaba/call-graphs/dap-estgi-call-graph.tsv"
  ESTG {..} <- getDebugSession
  StgState{..} <- getStgState
  liftIO $ do
    writeCallGraph fname ssCallGraph
    sendGraphCommand LoadGraph
      { loadGraphRequest  = "loadGraph"
      , loadGraphTitle    = cs $ takeFileName fullPakPath ++ " call graph"
      , loadGraphFilepath = cs fname
      }
  sendSuccesfulEmptyResponse

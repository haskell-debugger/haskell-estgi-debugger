{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE LambdaCase #-}
module Inspect.Value.HeapObject where

import           Control.Monad

import qualified Data.Map.Strict                       as Map
import           Data.String.Conversions               (cs)

import           Stg.Interpreter.Base                  hiding (lookupEnv, getCurrentThreadState, Breakpoint)
import           Stg.Syntax                            hiding (sourceName, Scope)
import           Stg.IRLocation

import DAP
import DapBase
import Inspect.Value.Atom
import SourceLocation

getVariablesForHeapObject :: ValueRoot -> HeapObject -> Adaptor ESTG [Variable]
getVariablesForHeapObject valueRoot = \case
  Con{..} -> forM (zip [0..] hoConArgs) $ \(idx, atom) -> do
    let name = cs $ "arg" ++ show idx
    getVariableForAtom name valueRoot atom

  Closure{..} -> do
    srcLocJson <- getStgSourceLocJSONText . SP_Binding . binderToStgId $ unId hoName
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
      let name = cs $ "arg" ++ show idx
      getVariableForAtom name valueRoot atom

    envVarList <- forM (Map.toList hoEnv) $ \(Id (Binder{..}), (_, atom)) -> do
      let BinderId u = binderId
          displayName = if binderScope == ModulePublic then cs binderName else cs (show u)
      getVariableForAtom displayName valueRoot atom

    pure $ bodyVar : argVarList ++ envVarList

  BlackHole{..} -> do
    bodyVar <- case hoBHOriginalThunk of
      Closure{..} -> do
        srcLocJson <- getStgSourceLocJSONText . SP_Binding . binderToStgId $ unId hoName
        pure . pure $ defaultVariable
          { variableName = "code"
          , variableValue = cs $ show hoName
          , variableEvaluateName = cs <$> srcLocJson
          }
      _ -> pure []
    onwerVar <- getVariableForAtom "owner thread id" valueRoot $ ThreadId hoBHOwnerThreadId
    queueVarList <- forM hoBHWaitQueue $ \tid -> getVariableForAtom "waiting thread id" valueRoot $ ThreadId tid
    pure $ bodyVar ++ onwerVar : queueVarList

  ApStack{..} -> do
    resultVarList <- forM hoResult $ \atom -> do
      getVariableForAtom "latest result" valueRoot atom
      -- TODO: hoStack
    pure resultVarList

  RaiseException ex -> do
    sequence [getVariableForAtom "exception" valueRoot ex]

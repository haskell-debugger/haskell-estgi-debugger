{-# LANGUAGE RecordWildCards     #-}
module Inspect.Value where

import           Stg.Interpreter.Base                  hiding (lookupEnv, getCurrentThreadState, Breakpoint)
import           Stg.Interpreter.GC.GCRef
import qualified Data.IntMap                           as IntMap

import DAP
import DapBase
import Inspect.Value.HeapObject

getVariablesForValue :: ValueRoot -> RefNamespace -> Int -> Adaptor ESTG [Variable]
getVariablesForValue valueRoot valueNS idx = do
  StgState{..} <- getStgState
  case valueNS of
    NS_HeapPtr
      | Just v <- IntMap.lookup idx ssHeap
      -> getVariablesForHeapObject valueRoot v

    _ -> pure []

{-

data RefNamespace
  = NS_Array
  | NS_ArrayArray
  | NS_HeapPtr
  | NS_MutableArray
  | NS_MutableArrayArray
  | NS_MutableByteArray
  | NS_MutVar
  | NS_TVar
  | NS_MVar
  | NS_SmallArray
  | NS_SmallMutableArray
  | NS_StableName
  | NS_StablePointer
  | NS_WeakPointer
  | NS_Thread
-}
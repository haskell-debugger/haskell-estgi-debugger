{-# LANGUAGE RecordWildCards     #-}
module Inspect.Value where

import           Stg.Interpreter.Base                  hiding (lookupEnv, getCurrentThreadState, Breakpoint)
import           Stg.Interpreter.GC.GCRef

import DAP
import DapBase

getVariablesForValue :: ValueRoot -> RefNamespace -> Int -> Adaptor ESTG [Variable]
getVariablesForValue valueRoot valueNS idx = do
  StgState{..} <- getStgState
  --case valueNS of
  pure []

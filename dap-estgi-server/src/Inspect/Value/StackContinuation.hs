{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE LambdaCase #-}
module Inspect.Value.StackContinuation where

import           Control.Monad

import qualified Data.Map.Strict                       as Map
import           Data.String.Conversions               (cs)
import           Data.Text                             ( Text )
import qualified Data.Text                             as T

import           Stg.Interpreter.Base                  hiding (lookupEnv, getCurrentThreadState, Breakpoint)
import           Stg.Syntax                            hiding (sourceName, Scope)
import           Stg.IRLocation

import DAP
import DapBase
import Inspect.Value.Atom
import SourceLocation

showScheduleReason :: ScheduleReason -> Text
showScheduleReason = \case
  SR_ThreadFinished            -> "Thread Finished"
  SR_ThreadFinishedFFICallback -> "Thread Finished FFI Callback"
  SR_ThreadBlocked             -> "Thread Blocked"
  SR_ThreadYield               -> "Thread Yield"
  SR_ThreadFinishedMain        -> "Thread Finished Main"

getVariablesForStackContinuation :: ValueRoot -> StackContinuation -> Adaptor ESTG [Variable]
getVariablesForStackContinuation valueRoot = \case
  CaseOf _ _ env _ _ _ -> do
    forM (Map.toList env) $ \(Id (Binder{..}), (_, atom)) -> do
      -- DMJ: for now everything is local.
      -- Inspect StaticOrigin to put things top-level, or as arguments, where applicable
      let BinderId u = binderId
          displayName = if binderScope == ModulePublic then cs binderName else cs (show u)
      getVariableForAtom displayName valueRoot atom

  Update addr -> do
    sequence [getVariableForAtom "Thunk Address" valueRoot $ HeapPtr addr]

  Apply atoms -> do
    forM atoms $ \atom -> do
      getVariableForAtom "Closure argument" valueRoot atom

  Catch atom blockAsync interruptible -> do
    sequence
      [ getVariableForAtom "Exception Handler" valueRoot atom
      , pure defaultVariable
        { variableName = "BlockAsyncExceptions"
        , variableValue = T.pack (show blockAsync)
        , variableType = Just "Bool"
        }
      , pure defaultVariable
        { variableName = "Interruptible"
        , variableValue = T.pack (show interruptible)
        , variableType = Just "Bool"
        }
      ]

  RestoreExMask _ blockAsync interruptible -> do
    pure
      [ defaultVariable
        { variableName = "BlockAsyncExceptions"
        , variableValue = T.pack (show blockAsync)
        , variableType = Just "Bool"
        }
      , defaultVariable
        { variableName = "Interruptible"
        , variableValue = T.pack (show interruptible)
        , variableType = Just "Bool"
        }
      ]

  RunScheduler reason -> do
    pure
      [ defaultVariable
        { variableName = "Schedule Reason"
        , variableValue = showScheduleReason reason
        }
      ]

  Atomically atom -> do
    sequence [getVariableForAtom "STM action" valueRoot atom]

  CatchRetry primaryAction alternativeAction isRunningAlternative _tlog -> do
    sequence
      [ getVariableForAtom "First STM action" valueRoot primaryAction
      , getVariableForAtom "Second STM action" valueRoot alternativeAction
      , pure defaultVariable
        { variableName = "Is running alternative STM action"
        , variableValue = T.pack (show isRunningAlternative)
        , variableType = Just "Bool"
        }
        -- todo add tlog
      ]

  CatchSTM action handler -> do
    sequence
      [ getVariableForAtom "STM action" valueRoot action
      , getVariableForAtom "Exception Handler" valueRoot handler
      ]

  DataToTagOp -> do
    pure []

  RaiseOp atom -> do
    sequence [getVariableForAtom "Exception" valueRoot atom]

  KeepAlive atom -> do
    sequence [getVariableForAtom "Managed Object" valueRoot atom]

  DebugFrame (RestoreProgramPoint maybeId _) -> do
    pure
      [ defaultVariable
        { variableName = "DebugFrame"
        , variableValue = cs (show maybeId)
        , variableType = Just "RestoreProgramPoint"
        }
      ]

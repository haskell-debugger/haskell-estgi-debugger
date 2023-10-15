{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
module Inspect.Stack where

import           Data.Typeable                         ( typeOf )
import           Control.Monad
import           Data.String.Conversions               (cs)
import           Data.Text                             ( Text )
import qualified Data.Text                             as T
import qualified Data.Map.Strict                       as Map
import qualified Data.Bimap                            as Bimap
import qualified Data.IntMap.Strict                    as IntMap

import           Stg.Interpreter.Base                  hiding (lookupEnv, getCurrentThreadState, Breakpoint)
import           Stg.Syntax                            hiding (sourceName, Scope)
import           Stg.IRLocation

import DAP
import DapBase
import Inspect.Value.Atom
import Inspect.Value.StackContinuation
import SourceCode
import SourceLocation

{-
  TODO:
    done - refactor stack inspection to:
      getVariablesForStackContinuation :: ValueRoot -> StackContinuation -> Adaptor ESTG [Variable]
    done - store stack frames in cache/map: (ThreadId, FrameIndex) -> StackContinuation
-}

----------------------------------------------------------------------------

getVariablesForStackFrame :: DapFrameIdDescriptor -> Adaptor ESTG [Variable]
getVariablesForStackFrame frameIdDesc = do
  let valueRoot = ValueRoot_StackFrame frameIdDesc
  case frameIdDesc of
    FrameId_CurrentThreadTopStackFrame -> do
      StgState{..} <- getStgState
      forM (Map.toList ssCurrentClosureEnv) $ \(Id (Binder{..}), (_, atom)) -> do
        let BinderId u = binderId
            displayName = if binderScope == ModulePublic then cs binderName else cs (show u)
        getVariableForAtom displayName valueRoot atom

    FrameId_ThreadStackFrame _threadId _stackFrameIndex -> do
      stackCont <- getStackFrameFromCache frameIdDesc
      getVariablesForStackContinuation valueRoot stackCont

getScopesForStackContinuation
  :: DapFrameIdDescriptor
  -> StackContinuation
  -- ^ The stack instruction that we're generating Scopes for
  -> Adaptor ESTG [Scope]
  -- ^ List of Scopes for this StackFrame
getScopesForStackContinuation frameIdDesc stackCont = do
  scopeVarablesRef <- getVariablesRef $ VariablesRef_StackFrameVariables frameIdDesc
  let scope = defaultScope
        { scopeName = "Locals: " <> T.pack (showStackCont stackCont)
        , scopePresentationHint = Just ScopePresentationHintLocals
        , scopeVariablesReference = scopeVarablesRef
        }
  scopeWithSourceLoc <- case stackCont of
    CaseOf _ closureId _ _ _ _ -> do
      (source, line, column, endLine, endColumn) <- getSourceAndPositionForStgPoint (SP_RhsClosureExpr (binderToStgId . unId $ closureId))
      pure scope
        { scopeSource = source
        , scopeLine = Just line
        , scopeColumn = Just column
        , scopeEndLine = Just endLine
        , scopeEndColumn = Just endColumn
        }
    _ -> pure scope
  pure [scopeWithSourceLoc]

getScopesForTopStackFrame
  :: DapFrameIdDescriptor
  -> Id
  -> Adaptor ESTG [Scope]
getScopesForTopStackFrame frameIdDesc closureId = do
  scopeVarablesRef <- getVariablesRef $ VariablesRef_StackFrameVariables frameIdDesc
  (source, line, column, endLine, endColumn) <- getSourceAndPositionForStgPoint (SP_RhsClosureExpr . binderToStgId $ unId closureId)
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

commandThreads :: Adaptor ESTG ()
commandThreads = do
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

commandStackTrace :: Adaptor ESTG ()
commandStackTrace = do
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
          (source, line, column, endLine, endColumn) <- getSourceAndPositionForStgPoint (SP_RhsClosureExpr . binderToStgId $ unId currentClosureId)
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
      stackFrames <- forM (zip [0..] tsStack) $ \(frameIndex, stackCont) -> do
        let frameIdDesc = FrameId_ThreadStackFrame stackTraceArgumentsThreadId frameIndex
        addStackFrameToCache frameIdDesc stackCont
        frameId <- getFrameId frameIdDesc
        case stackCont of
          CaseOf _ closureId _ scrutResultId _ _ -> do
            -- HINT: use the case scrutinee result's unique binder id to lookup source location info
            (source, line, column, endLine, endColumn) <- getSourceAndPositionForStgPoint (SP_CaseScrutineeExpr . binderToStgId $ unId scrutResultId)
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

commandScopes :: Adaptor ESTG ()
commandScopes = do
  ScopesArguments {..} <- getArguments
  StgState{..} <- getStgState
  ESTG {..} <- getDebugSession
  case Bimap.lookupR scopesArgumentsFrameId dapFrameIdMap of
    Nothing -> do
      sendError (ErrorMessage (T.pack $ "Unknown frameId: " ++ show scopesArgumentsFrameId)) Nothing

    Just frameIdDescriptor@FrameId_CurrentThreadTopStackFrame
      | Just currentClosureId <- ssCurrentClosure
      -> do
        scopes <- getScopesForTopStackFrame frameIdDescriptor currentClosureId
        sendScopesResponse (ScopesResponse scopes)

    Just frameIdDescriptor@(FrameId_ThreadStackFrame _threadId _frameIndex) -> do
      stackFrame <- getStackFrameFromCache frameIdDescriptor
      scopes <- getScopesForStackContinuation frameIdDescriptor stackFrame
      sendScopesResponse (ScopesResponse scopes)

    _ -> sendScopesResponse (ScopesResponse [])

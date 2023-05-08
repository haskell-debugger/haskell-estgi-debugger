-----------------------------------------------------------------------------
-- |
-- Module      :  DAP.Response
-- Copyright   :  (C) 2023 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <djohnson.m@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
----------------------------------------------------------------------------
module DAP.Response
  ( -- * Response message API
    sendAttachResponse
  , sendBreakpointLocationsResponse
  , sendCompletionsResponse
  , sendConfigurationDoneResponse
  , sendContinueResponse
  , sendDataBreakpointInfoResponse
  , sendDisassembleResponse
  , sendDisconnectResponse
  , sendEvaluateResponse
  , sendExceptionInfoResponse
  , sendGotoResponse
  , sendGotoTargetsResponse
  , sendInitializeResponse
  , sendLaunchResponse
  , sendLoadedSourcesResponse
  , sendModulesResponse
  , sendNextResponse
  , sendPauseResponse
  , sendReadMemoryResponse
  , sendRestartResponse
  , sendRestartFrameResponse
  , sendReverseContinueResponse
  , sendScopesResponse
  , sendSetBreakpointsResponse
  , sendSetDataBreakpointsResponse
  , sendSetExceptionBreakpointsResponse
  , sendSetExpressionResponse
  , sendSetFunctionBreakpointsResponse
  , sendSetInstructionBreakpointsResponse
  , sendSetVariableResponse
  , sendSourceResponse
  , sendStackTraceResponse
  , sendStepBackResponse
  , sendStepInResponse
  , sendStepInTargetsResponse
  , sendStepOutResponse
  , sendTerminateResponse
  , sendTerminateThreadsResponse
  , sendThreadsResponse
  , sendVariablesResponse
  , sendWriteMemoryResponse
  , sendRunInTerminalResponse
  , sendStartDebuggingResponse
  ) where
----------------------------------------------------------------------------
import           DAP.Adaptor
import           DAP.Types
----------------------------------------------------------------------------
-- | AttachResponse has no body by default
sendAttachResponse :: Adaptor app ()
sendAttachResponse = sendSuccesfulEmptyResponse
----------------------------------------------------------------------------
-- | BreakpointLocationResponse has no body by default
sendBreakpointLocationsResponse
  :: [BreakpointLocation]
  -> Adaptor app ()
sendBreakpointLocationsResponse
  = sendSuccesfulResponse
  . setBody
  . Breakpoints
----------------------------------------------------------------------------
-- | 'SetDataBreakpointsResponse'
sendSetDataBreakpointsResponse
  :: [Breakpoint]
  -> Adaptor app ()
sendSetDataBreakpointsResponse
  = sendSuccesfulResponse
  . setBody
  . Breakpoints
----------------------------------------------------------------------------
-- | BreakpointResponse has no body by default
sendSetBreakpointsResponse
  :: [Breakpoint]
  -> Adaptor app ()
sendSetBreakpointsResponse
  = sendSuccesfulResponse
  . setBody
  . Breakpoints
----------------------------------------------------------------------------
-- | SetInstructionsBreakpointResponse has no body by default
sendSetInstructionBreakpointsResponse
  :: [Breakpoint]
  -> Adaptor app ()
sendSetInstructionBreakpointsResponse
  = sendSuccesfulResponse
  . setBody
  . Breakpoints
----------------------------------------------------------------------------
-- | SetFunctionBreakpointResponse has no body by default
sendSetFunctionBreakpointsResponse
  :: [Breakpoint]
  -> Adaptor app ()
sendSetFunctionBreakpointsResponse
  = sendSuccesfulResponse
  . setBody
  . Breakpoints
----------------------------------------------------------------------------
-- | SetExceptionBreakpointsResponse has no body by default
sendSetExceptionBreakpointsResponse
  :: [Breakpoint]
  -> Adaptor app ()
sendSetExceptionBreakpointsResponse
  = sendSuccesfulResponse
  . setBody
  . Breakpoints
----------------------------------------------------------------------------
-- | ContinueResponse
sendContinueResponse
  :: ContinueResponse
  -> Adaptor app ()
sendContinueResponse continueResponse = do
  sendSuccesfulResponse (setBody continueResponse)
----------------------------------------------------------------------------
-- | ConfigurationDoneResponse
sendConfigurationDoneResponse
  :: Adaptor app ()
sendConfigurationDoneResponse = do
  sendSuccesfulEmptyResponse
----------------------------------------------------------------------------
-- | LaunchResponse
sendLaunchResponse
  :: Adaptor app ()
sendLaunchResponse = sendSuccesfulEmptyResponse
----------------------------------------------------------------------------
-- | RestartResponse
sendRestartResponse
  :: Adaptor app ()
sendRestartResponse = sendSuccesfulEmptyResponse
----------------------------------------------------------------------------
-- | DisconnectResponse
sendDisconnectResponse
  :: Adaptor app ()
sendDisconnectResponse = sendSuccesfulEmptyResponse
----------------------------------------------------------------------------
-- | TerminateResponse
sendTerminateResponse
  :: Adaptor app ()
sendTerminateResponse = sendSuccesfulEmptyResponse
----------------------------------------------------------------------------
-- | NextResponse
sendNextResponse
  :: Adaptor app ()
sendNextResponse = sendSuccesfulEmptyResponse
----------------------------------------------------------------------------
-- | StepInResponse
sendStepInResponse
  :: Adaptor app ()
sendStepInResponse = sendSuccesfulEmptyResponse
----------------------------------------------------------------------------
-- | StepOutResponse
sendStepOutResponse
  :: Adaptor app ()
sendStepOutResponse = sendSuccesfulEmptyResponse
----------------------------------------------------------------------------
-- | StepBackResponse
sendStepBackResponse
  :: Adaptor app ()
sendStepBackResponse = sendSuccesfulEmptyResponse
----------------------------------------------------------------------------
-- | ReverseContinueResponse
sendReverseContinueResponse
  :: Adaptor app ()
sendReverseContinueResponse = sendSuccesfulEmptyResponse
----------------------------------------------------------------------------
-- | RestartFrameResponse
sendRestartFrameResponse
  :: Adaptor app ()
sendRestartFrameResponse = sendSuccesfulEmptyResponse
----------------------------------------------------------------------------
-- | InitializeReponse
sendInitializeResponse
  :: Adaptor app ()
sendInitializeResponse = do
  capabilities <- getServerCapabilities
  sendSuccesfulResponse (setBody capabilities)
----------------------------------------------------------------------------
-- | GotoResponse
sendGotoResponse
  :: Adaptor app ()
sendGotoResponse = sendSuccesfulEmptyResponse
----------------------------------------------------------------------------
-- | GotoTargetsResponse
sendGotoTargetsResponse
  :: Adaptor app ()
sendGotoTargetsResponse = sendSuccesfulEmptyResponse
----------------------------------------------------------------------------
-- | PauseResponse
sendPauseResponse
  :: Adaptor app ()
sendPauseResponse = sendSuccesfulEmptyResponse
----------------------------------------------------------------------------
-- | TerminateThreadsResponse
sendTerminateThreadsResponse
  :: Adaptor app ()
sendTerminateThreadsResponse = sendSuccesfulEmptyResponse
----------------------------------------------------------------------------
sendModulesResponse :: ModulesResponse -> Adaptor app ()
sendModulesResponse = sendSuccesfulResponse . setBody
----------------------------------------------------------------------------
sendStackTraceResponse :: StackTraceResponse -> Adaptor app ()
sendStackTraceResponse = sendSuccesfulResponse . setBody
----------------------------------------------------------------------------
sendSourceResponse :: SourceResponse -> Adaptor app ()
sendSourceResponse = sendSuccesfulResponse . setBody
----------------------------------------------------------------------------
sendThreadsResponse :: [Thread] -> Adaptor app ()
sendThreadsResponse = sendSuccesfulResponse . setBody . ThreadsResponse
----------------------------------------------------------------------------
sendLoadedSourcesResponse :: [Source] -> Adaptor app ()
sendLoadedSourcesResponse = sendSuccesfulResponse . setBody . LoadedSourcesResponse
----------------------------------------------------------------------------
sendWriteMemoryResponse :: WriteMemoryResponse -> Adaptor app ()
sendWriteMemoryResponse = sendSuccesfulResponse . setBody
----------------------------------------------------------------------------
sendReadMemoryResponse :: ReadMemoryResponse -> Adaptor app ()
sendReadMemoryResponse = sendSuccesfulResponse . setBody
----------------------------------------------------------------------------
sendCompletionsResponse :: CompletionsResponse -> Adaptor app ()
sendCompletionsResponse = sendSuccesfulResponse . setBody
----------------------------------------------------------------------------
sendDataBreakpointInfoResponse :: DataBreakpointInfoResponse -> Adaptor app ()
sendDataBreakpointInfoResponse = sendSuccesfulResponse . setBody
----------------------------------------------------------------------------
sendDisassembleResponse :: DisassembleResponse -> Adaptor app ()
sendDisassembleResponse = sendSuccesfulResponse . setBody
----------------------------------------------------------------------------
sendEvaluateResponse :: EvaluateResponse -> Adaptor app ()
sendEvaluateResponse = sendSuccesfulResponse . setBody
----------------------------------------------------------------------------
sendExceptionInfoResponse :: ExceptionInfoResponse -> Adaptor app ()
sendExceptionInfoResponse = sendSuccesfulResponse . setBody
----------------------------------------------------------------------------
sendScopesResponse :: ScopesResponse -> Adaptor app ()
sendScopesResponse = sendSuccesfulResponse . setBody
----------------------------------------------------------------------------
sendSetExpressionResponse :: SetExpressionResponse -> Adaptor app ()
sendSetExpressionResponse = sendSuccesfulResponse . setBody
----------------------------------------------------------------------------
sendSetVariableResponse :: SetVariableResponse -> Adaptor app ()
sendSetVariableResponse = sendSuccesfulResponse . setBody
----------------------------------------------------------------------------
sendStepInTargetsResponse :: StepInTargetsResponse -> Adaptor app ()
sendStepInTargetsResponse = sendSuccesfulResponse . setBody
----------------------------------------------------------------------------
sendVariablesResponse :: VariablesResponse -> Adaptor app ()
sendVariablesResponse = sendSuccesfulResponse . setBody
----------------------------------------------------------------------------
sendRunInTerminalResponse :: RunInTerminalResponse -> Adaptor app ()
sendRunInTerminalResponse = sendSuccesfulResponse . setBody
----------------------------------------------------------------------------
sendStartDebuggingResponse :: Adaptor app ()
sendStartDebuggingResponse = sendSuccesfulEmptyResponse
----------------------------------------------------------------------------

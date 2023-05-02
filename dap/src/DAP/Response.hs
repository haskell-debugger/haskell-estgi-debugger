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
sendAttachResponse :: AdaptorClient app ()
sendAttachResponse = sendSuccesfulEmptyResponse
----------------------------------------------------------------------------
-- | BreakpointLocationResponse has no body by default
sendBreakpointLocationsResponse
  :: [BreakpointLocation]
  -> AdaptorClient app ()
sendBreakpointLocationsResponse
  = sendSuccesfulResponse
  . setBody
  . Breakpoints
----------------------------------------------------------------------------
-- | 'SetDataBreakpointsResponse'
sendSetDataBreakpointsResponse
  :: [Breakpoint]
  -> AdaptorClient app ()
sendSetDataBreakpointsResponse
  = sendSuccesfulResponse
  . setBody
  . Breakpoints
----------------------------------------------------------------------------
-- | BreakpointResponse has no body by default
sendSetBreakpointsResponse
  :: [Breakpoint]
  -> AdaptorClient app ()
sendSetBreakpointsResponse
  = sendSuccesfulResponse
  . setBody
  . Breakpoints
----------------------------------------------------------------------------
-- | SetInstructionsBreakpointResponse has no body by default
sendSetInstructionBreakpointsResponse
  :: [Breakpoint]
  -> AdaptorClient app ()
sendSetInstructionBreakpointsResponse
  = sendSuccesfulResponse
  . setBody
  . Breakpoints
----------------------------------------------------------------------------
-- | SetFunctionBreakpointResponse has no body by default
sendSetFunctionBreakpointsResponse
  :: [Breakpoint]
  -> AdaptorClient app ()
sendSetFunctionBreakpointsResponse
  = sendSuccesfulResponse
  . setBody
  . Breakpoints
----------------------------------------------------------------------------
-- | SetExceptionBreakpointsResponse has no body by default
sendSetExceptionBreakpointsResponse
  :: [Breakpoint]
  -> AdaptorClient app ()
sendSetExceptionBreakpointsResponse
  = sendSuccesfulResponse
  . setBody
  . Breakpoints
----------------------------------------------------------------------------
-- | ContinueResponse
sendContinueResponse
  :: ContinueResponse
  -> AdaptorClient app ()
sendContinueResponse continueResponse = do
  sendSuccesfulResponse (setBody continueResponse)
----------------------------------------------------------------------------
-- | ConfigurationDoneResponse
sendConfigurationDoneResponse
  :: AdaptorClient app ()
sendConfigurationDoneResponse = do
  sendSuccesfulEmptyResponse
----------------------------------------------------------------------------
-- | LaunchResponse
sendLaunchResponse
  :: AdaptorClient app ()
sendLaunchResponse = sendSuccesfulEmptyResponse
----------------------------------------------------------------------------
-- | RestartResponse
sendRestartResponse
  :: AdaptorClient app ()
sendRestartResponse = sendSuccesfulEmptyResponse
----------------------------------------------------------------------------
-- | DisconnectResponse
sendDisconnectResponse
  :: AdaptorClient app ()
sendDisconnectResponse = sendSuccesfulEmptyResponse
----------------------------------------------------------------------------
-- | TerminateResponse
sendTerminateResponse
  :: AdaptorClient app ()
sendTerminateResponse = sendSuccesfulEmptyResponse
----------------------------------------------------------------------------
-- | NextResponse
sendNextResponse
  :: AdaptorClient app ()
sendNextResponse = sendSuccesfulEmptyResponse
----------------------------------------------------------------------------
-- | StepInResponse
sendStepInResponse
  :: AdaptorClient app ()
sendStepInResponse = sendSuccesfulEmptyResponse
----------------------------------------------------------------------------
-- | StepOutResponse
sendStepOutResponse
  :: AdaptorClient app ()
sendStepOutResponse = sendSuccesfulEmptyResponse
----------------------------------------------------------------------------
-- | StepBackResponse
sendStepBackResponse
  :: AdaptorClient app ()
sendStepBackResponse = sendSuccesfulEmptyResponse
----------------------------------------------------------------------------
-- | ReverseContinueResponse
sendReverseContinueResponse
  :: AdaptorClient app ()
sendReverseContinueResponse = sendSuccesfulEmptyResponse
----------------------------------------------------------------------------
-- | RestartFrameResponse
sendRestartFrameResponse
  :: AdaptorClient app ()
sendRestartFrameResponse = sendSuccesfulEmptyResponse
----------------------------------------------------------------------------
-- | InitializeReponse
sendInitializeResponse
  :: AdaptorClient app ()
sendInitializeResponse = do
  capabilities <- getServerCapabilities
  sendSuccesfulResponse (setBody capabilities)
----------------------------------------------------------------------------
-- | GotoResponse
sendGotoResponse
  :: AdaptorClient app ()
sendGotoResponse = sendSuccesfulEmptyResponse
----------------------------------------------------------------------------
-- | GotoTargetsResponse
sendGotoTargetsResponse
  :: AdaptorClient app ()
sendGotoTargetsResponse = sendSuccesfulEmptyResponse
----------------------------------------------------------------------------
-- | PauseResponse
sendPauseResponse
  :: AdaptorClient app ()
sendPauseResponse = sendSuccesfulEmptyResponse
----------------------------------------------------------------------------
-- | TerminateThreadsResponse
sendTerminateThreadsResponse
  :: AdaptorClient app ()
sendTerminateThreadsResponse = sendSuccesfulEmptyResponse
----------------------------------------------------------------------------
sendModulesResponse :: ModulesResponse -> AdaptorClient app ()
sendModulesResponse = sendSuccesfulResponse . setBody
----------------------------------------------------------------------------
sendStackTraceResponse :: StackTraceResponse -> AdaptorClient app ()
sendStackTraceResponse = sendSuccesfulResponse . setBody
----------------------------------------------------------------------------
sendSourceResponse :: SourceResponse -> AdaptorClient app ()
sendSourceResponse = sendSuccesfulResponse . setBody
----------------------------------------------------------------------------
sendThreadsResponse :: [Thread] -> AdaptorClient app ()
sendThreadsResponse = sendSuccesfulResponse . setBody . ThreadsResponse
----------------------------------------------------------------------------
sendLoadedSourcesResponse :: [Source] -> AdaptorClient app ()
sendLoadedSourcesResponse = sendSuccesfulResponse . setBody . LoadedSourcesResponse
----------------------------------------------------------------------------
sendWriteMemoryResponse :: WriteMemoryResponse -> AdaptorClient app ()
sendWriteMemoryResponse = sendSuccesfulResponse . setBody
----------------------------------------------------------------------------
sendReadMemoryResponse :: ReadMemoryResponse -> AdaptorClient app ()
sendReadMemoryResponse = sendSuccesfulResponse . setBody
----------------------------------------------------------------------------
sendCompletionsResponse :: CompletionsResponse -> AdaptorClient app ()
sendCompletionsResponse = sendSuccesfulResponse . setBody
----------------------------------------------------------------------------
sendDataBreakpointInfoResponse :: DataBreakpointInfoResponse -> AdaptorClient app ()
sendDataBreakpointInfoResponse = sendSuccesfulResponse . setBody
----------------------------------------------------------------------------
sendDisassembleResponse :: DisassembleResponse -> AdaptorClient app ()
sendDisassembleResponse = sendSuccesfulResponse . setBody
----------------------------------------------------------------------------
sendEvaluateResponse :: EvaluateResponse -> AdaptorClient app ()
sendEvaluateResponse = sendSuccesfulResponse . setBody
----------------------------------------------------------------------------
sendExceptionInfoResponse :: ExceptionInfoResponse -> AdaptorClient app ()
sendExceptionInfoResponse = sendSuccesfulResponse . setBody
----------------------------------------------------------------------------
sendScopesResponse :: ScopesResponse -> AdaptorClient app ()
sendScopesResponse = sendSuccesfulResponse . setBody
----------------------------------------------------------------------------
sendSetExpressionResponse :: SetExpressionResponse -> AdaptorClient app ()
sendSetExpressionResponse = sendSuccesfulResponse . setBody
----------------------------------------------------------------------------
sendSetVariableResponse :: SetVariableResponse -> AdaptorClient app ()
sendSetVariableResponse = sendSuccesfulResponse . setBody
----------------------------------------------------------------------------
sendStepInTargetsResponse :: StepInTargetsResponse -> AdaptorClient app ()
sendStepInTargetsResponse = sendSuccesfulResponse . setBody
----------------------------------------------------------------------------
sendVariablesResponse :: VariablesResponse -> AdaptorClient app ()
sendVariablesResponse = sendSuccesfulResponse . setBody
----------------------------------------------------------------------------
sendRunInTerminalResponse :: RunInTerminalResponse -> AdaptorClient app ()
sendRunInTerminalResponse = sendSuccesfulResponse . setBody
----------------------------------------------------------------------------
sendStartDebuggingResponse :: AdaptorClient app ()
sendStartDebuggingResponse = sendSuccesfulEmptyResponse
----------------------------------------------------------------------------

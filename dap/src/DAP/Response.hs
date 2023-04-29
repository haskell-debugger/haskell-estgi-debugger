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
  , sendModulesResponse
  , sendBreakpointsLocationResponse
  , sendSetDataBreakpointsResponse
  , sendSetBreakpointsResponse
  , sendSetFunctionBreakpointsResponse
  , sendSetExceptionBreakpointsResponse
  , sendSetInstructionBreakpointsResponse
  , sendContinueResponse
  , sendConfigurationDoneResponse
  , sendLaunchResponse
  , sendRestartResponse
  , sendDisconnectResponse
  , sendTerminateResponse
  , sendNextResponse
  , sendStepInResponse
  , sendStepOutResponse
  , sendStepBackResponse
  , sendReverseContinueResponse
  , sendRestartFrameResponse
  , sendGotoResponse
  , sendPauseResponse
  , sendInitializedResponse
  , sendThreadsResponse
  , sendTerminateThreadsResponse
  , sendStackTraceResponse
  , sendSourceResponse
  , sendLoadedSourcesResponse
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
sendBreakpointsLocationResponse
  :: [BreakpointLocation]
  -> AdaptorClient app ()
sendBreakpointsLocationResponse
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
-- | InitializedReponse
-- <https://microsoft.github.io/debug-adapter-protocol/specification#Requests_Initialize>
-- 'Capabilities' is present in the `ServerConfig` when the DAP server starts
-- This function reads the global 'Capabilities' from the 'ServerConfig'
sendInitializedResponse
  :: AdaptorClient app ()
sendInitializedResponse = do
  capabilities <- getServerCapabilities
  sendSuccesfulResponse (setBody capabilities)
----------------------------------------------------------------------------
-- | GotoResponse
sendGotoResponse
  :: AdaptorClient app ()
sendGotoResponse = sendSuccesfulEmptyResponse
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

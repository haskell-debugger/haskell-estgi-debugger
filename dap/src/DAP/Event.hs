-----------------------------------------------------------------------------
-- |
-- Module      :  DAP.Event
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
module DAP.Event
  ( -- * Event message API
    sendBreakpointEvent
  , sendCapabilitiesEvent
  , sendContinuedEvent
  , sendExitedEvent
  , sendInitializedEvent
  , sendInvalidatedEvent
  , sendLoadedSourceEvent
  , sendMemoryEvent
  , sendModuleEvent
  , sendOutputEvent
  , sendProcessEvent
  , sendProgressEndEvent
  , sendProgressStartEvent
  , sendProgressUpdateEvent
  , sendStoppedEvent
  , sendTerminatedEvent
  , sendThreadEvent
  ) where
----------------------------------------------------------------------------
import           DAP.Types
import           DAP.Adaptor
----------------------------------------------------------------------------
sendBreakpointEvent :: AdaptorClient app ()
sendBreakpointEvent = sendSuccesfulEvent EventTypeBreakpoint (pure ())
----------------------------------------------------------------------------
sendCapabilitiesEvent :: AdaptorClient app ()
sendCapabilitiesEvent = sendSuccesfulEvent EventTypeCapabilities (pure ())
----------------------------------------------------------------------------
sendContinuedEvent :: ContinuedEvent -> AdaptorClient app ()
sendContinuedEvent = sendSuccesfulEvent EventTypeContinued . setBody
----------------------------------------------------------------------------
sendExitedEvent :: ExitedEvent -> AdaptorClient app ()
sendExitedEvent = sendSuccesfulEvent EventTypeExited . setBody
----------------------------------------------------------------------------
sendInitializedEvent :: AdaptorClient app ()
sendInitializedEvent = sendSuccesfulEvent EventTypeInitialized (pure ())
----------------------------------------------------------------------------
sendInvalidatedEvent :: InvalidatedEvent -> AdaptorClient app ()
sendInvalidatedEvent = sendSuccesfulEvent EventTypeInvalidated . setBody
----------------------------------------------------------------------------
sendLoadedSourceEvent :: LoadedSourceEvent -> AdaptorClient app ()
sendLoadedSourceEvent = sendSuccesfulEvent EventTypeLoadedSource . setBody
----------------------------------------------------------------------------
sendMemoryEvent :: MemoryEvent -> AdaptorClient app ()
sendMemoryEvent = sendSuccesfulEvent EventTypeMemory . setBody
----------------------------------------------------------------------------
sendModuleEvent :: ModuleEvent -> AdaptorClient app ()
sendModuleEvent = sendSuccesfulEvent EventTypeModule . setBody
----------------------------------------------------------------------------
sendOutputEvent :: OutputEvent -> AdaptorClient app ()
sendOutputEvent = sendSuccesfulEvent EventTypeOutput . setBody
----------------------------------------------------------------------------
sendProcessEvent :: ProcessEvent -> AdaptorClient app ()
sendProcessEvent = sendSuccesfulEvent EventTypeProcess . setBody
----------------------------------------------------------------------------
sendProgressEndEvent :: ProgressEndEvent -> AdaptorClient app ()
sendProgressEndEvent = sendSuccesfulEvent EventTypeProgressEnd . setBody
----------------------------------------------------------------------------
sendProgressStartEvent :: ProgressStartEvent -> AdaptorClient app ()
sendProgressStartEvent = sendSuccesfulEvent EventTypeProgressStart . setBody
----------------------------------------------------------------------------
sendProgressUpdateEvent :: ProgressUpdateEvent -> AdaptorClient app ()
sendProgressUpdateEvent = sendSuccesfulEvent EventTypeProgressUpdate . setBody
----------------------------------------------------------------------------
sendStoppedEvent :: StoppedEvent -> AdaptorClient app ()
sendStoppedEvent = sendSuccesfulEvent EventTypeStopped . setBody
----------------------------------------------------------------------------
sendTerminatedEvent :: TerminatedEvent -> AdaptorClient app ()
sendTerminatedEvent = sendSuccesfulEvent EventTypeTerminated . setBody
----------------------------------------------------------------------------
sendThreadEvent :: ThreadEvent -> AdaptorClient app ()
sendThreadEvent = sendSuccesfulEvent EventTypeThread . setBody
----------------------------------------------------------------------------

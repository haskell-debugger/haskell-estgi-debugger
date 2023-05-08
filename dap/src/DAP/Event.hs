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
  -- * Defaults
  , defaultContinuedEvent
  , defaultExitedEvent
  , defaultInvalidatedEvent
  , defaultMemoryEvent
  , defaultOutputEvent
  , defaultProcessEvent
  , defaultProgressEndEvent
  , defaultProgressStartEvent
  , defaultProgressUpdateEvent
  , defaultStoppedEvent
  , defaultTerminatedEvent
  , defaultThreadEvent
  ) where
----------------------------------------------------------------------------
import           DAP.Types
import           DAP.Adaptor
----------------------------------------------------------------------------
sendBreakpointEvent :: BreakpointEvent -> Adaptor app ()
sendBreakpointEvent = sendSuccesfulEvent EventTypeBreakpoint . setBody
----------------------------------------------------------------------------
sendCapabilitiesEvent :: CapabilitiesEvent -> Adaptor app ()
sendCapabilitiesEvent = sendSuccesfulEvent EventTypeCapabilities . setBody
----------------------------------------------------------------------------
sendContinuedEvent :: ContinuedEvent -> Adaptor app ()
sendContinuedEvent = sendSuccesfulEvent EventTypeContinued . setBody
----------------------------------------------------------------------------
defaultContinuedEvent :: ContinuedEvent
defaultContinuedEvent = ContinuedEvent 0 False
----------------------------------------------------------------------------
sendExitedEvent :: ExitedEvent -> Adaptor app ()
sendExitedEvent = sendSuccesfulEvent EventTypeExited . setBody
----------------------------------------------------------------------------
defaultExitedEvent :: ExitedEvent
defaultExitedEvent = ExitedEvent 0
----------------------------------------------------------------------------
sendInitializedEvent :: Adaptor app ()
sendInitializedEvent = sendSuccesfulEvent EventTypeInitialized (pure ())
----------------------------------------------------------------------------
sendInvalidatedEvent :: InvalidatedEvent -> Adaptor app ()
sendInvalidatedEvent = sendSuccesfulEvent EventTypeInvalidated . setBody
----------------------------------------------------------------------------
defaultInvalidatedEvent :: InvalidatedEvent
defaultInvalidatedEvent = InvalidatedEvent [] Nothing Nothing
----------------------------------------------------------------------------
sendLoadedSourceEvent :: LoadedSourceEvent -> Adaptor app ()
sendLoadedSourceEvent = sendSuccesfulEvent EventTypeLoadedSource . setBody
----------------------------------------------------------------------------
sendMemoryEvent :: MemoryEvent -> Adaptor app ()
sendMemoryEvent = sendSuccesfulEvent EventTypeMemory . setBody
----------------------------------------------------------------------------
defaultMemoryEvent :: MemoryEvent
defaultMemoryEvent = MemoryEvent mempty 0 0
----------------------------------------------------------------------------
sendModuleEvent :: ModuleEvent -> Adaptor app ()
sendModuleEvent = sendSuccesfulEvent EventTypeModule . setBody
----------------------------------------------------------------------------
sendOutputEvent :: OutputEvent -> Adaptor app ()
sendOutputEvent = sendSuccesfulEvent EventTypeOutput . setBody
----------------------------------------------------------------------------
defaultOutputEvent :: OutputEvent
defaultOutputEvent = OutputEvent Nothing mempty Nothing Nothing Nothing Nothing Nothing Nothing
----------------------------------------------------------------------------
sendProcessEvent :: ProcessEvent -> Adaptor app ()
sendProcessEvent = sendSuccesfulEvent EventTypeProcess . setBody
----------------------------------------------------------------------------
defaultProcessEvent :: ProcessEvent
defaultProcessEvent = ProcessEvent mempty Nothing True Nothing Nothing
----------------------------------------------------------------------------
sendProgressEndEvent :: ProgressEndEvent -> Adaptor app ()
sendProgressEndEvent = sendSuccesfulEvent EventTypeProgressEnd . setBody
----------------------------------------------------------------------------
defaultProgressEndEvent :: ProgressEndEvent
defaultProgressEndEvent = ProgressEndEvent mempty Nothing
----------------------------------------------------------------------------
sendProgressStartEvent :: ProgressStartEvent -> Adaptor app ()
sendProgressStartEvent = sendSuccesfulEvent EventTypeProgressStart . setBody
----------------------------------------------------------------------------
defaultProgressStartEvent :: ProgressStartEvent
defaultProgressStartEvent = ProgressStartEvent mempty mempty Nothing False Nothing Nothing
----------------------------------------------------------------------------
sendProgressUpdateEvent :: ProgressUpdateEvent -> Adaptor app ()
sendProgressUpdateEvent = sendSuccesfulEvent EventTypeProgressUpdate . setBody
----------------------------------------------------------------------------
defaultProgressUpdateEvent :: ProgressUpdateEvent
defaultProgressUpdateEvent = ProgressUpdateEvent mempty Nothing Nothing
----------------------------------------------------------------------------
sendStoppedEvent :: StoppedEvent -> Adaptor app ()
sendStoppedEvent = sendSuccesfulEvent EventTypeStopped . setBody
----------------------------------------------------------------------------
defaultStoppedEvent :: StoppedEvent
defaultStoppedEvent = StoppedEvent StoppedEventReasonStep Nothing (Just 0) False Nothing False []
----------------------------------------------------------------------------
sendTerminatedEvent :: TerminatedEvent -> Adaptor app ()
sendTerminatedEvent = sendSuccesfulEvent EventTypeTerminated . setBody
----------------------------------------------------------------------------
defaultTerminatedEvent :: TerminatedEvent
defaultTerminatedEvent = TerminatedEvent False
----------------------------------------------------------------------------
sendThreadEvent :: ThreadEvent -> Adaptor app ()
sendThreadEvent = sendSuccesfulEvent EventTypeThread . setBody
----------------------------------------------------------------------------
defaultThreadEvent :: ThreadEvent
defaultThreadEvent = ThreadEvent ThreadEventReasonStarted 0
----------------------------------------------------------------------------


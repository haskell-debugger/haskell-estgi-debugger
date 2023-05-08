----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (C) 2023 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <djohnson.m@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
-- |
----------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE DerivingStrategies  #-}
----------------------------------------------------------------------------
module Main (main) where
----------------------------------------------------------------------------
import           Control.Arrow
import           Data.IORef
import qualified Data.IntMap.Strict                    as I
import qualified Data.Map.Strict                       as M
import           Control.Exception                     hiding (catch)
import           Control.Monad.IO.Class                (liftIO)
import           Control.Exception.Lifted              (catch)
import           Control.Monad
import           Data.Aeson                            ( Value(Null), FromJSON )
import           Data.Text                             ( Text )
import qualified Data.Text                             as T
import           Data.Typeable                         ( typeOf )
import           Data.Maybe                            ( fromMaybe )
import           GHC.Generics                          ( Generic )
import           System.Environment                    ( lookupEnv )
import           Text.Read                             ( readMaybe )
import qualified Data.ByteString.Lazy.Char8            as BL8 ( pack, unpack, fromStrict, toStrict )
import qualified Control.Concurrent.Chan.Unagi.Bounded as Unagi
----------------------------------------------------------------------------
import           Stg.Syntax                            hiding (sourceName, Scope)
import           Stg.Interpreter
import           Stg.Interpreter.Debug
import           Stg.Interpreter.Base                  hiding (lookupEnv, getCurrentThreadState, getCurrentThreadState)
import           Stg.Interpreter.Debugger
import           Stg.Interpreter.Debugger.UI
import           Stg.IO
import           Stg.Program
import           Data.Yaml
----------------------------------------------------------------------------
import           DAP
----------------------------------------------------------------------------
-- | DAP entry point
-- Extracts configuration information from the environment
-- Opens a listen socket on a port (defaulting to '4711')
-- Converts the 'Socket' to a 'Handle' for convenience
main :: IO ()
main = do
  config <- getConfig
  runDAPServer config talk
----------------------------------------------------------------------------
-- | Fetch config from environment, fallback to sane defaults
getConfig :: IO ServerConfig
getConfig = do
  let
    hostDefault = "127.0.0.1"
    portDefault = 4711
  ServerConfig
    <$> do fromMaybe hostDefault <$> lookupEnv "DAP_HOST"
    <*> do fromMaybe portDefault . (readMaybe =<<) <$> do lookupEnv "DAP_PORT"
    <*> pure defaultCapabilities
    <*> pure True
----------------------------------------------------------------------------
-- | VSCode arguments are custom for attach
-- > "arguments": {
-- >      "__configurationTarget": 6,
-- >      "__sessionId": "6c0ba6f8-e478-4698-821e-356fc4a72c3d",
-- >      "name": "thing",
-- >      "program": "/home/dmjio/Desktop/stg-dap/test.fullpak",
-- >      "request": "attach",
-- >      "type": "dap-extension"
-- >  }
--
data AttachArgs
  = AttachArgs
  { __sessionId :: Text
    -- ^ SessionID from VSCode
  , program :: String
    -- ^ Path to .fullpak file
  } deriving stock (Show, Eq, Generic)
    deriving anyclass FromJSON
----------------------------------------------------------------------------
-- | External STG Interpreter application internal state
data ESTG
  = ESTG
  { inChan :: Unagi.InChan DebugCommand
  , outChan :: Unagi.OutChan DebugOutput
  , fullPakPath :: String
  , frameToScopeRef :: IORef (I.IntMap Int)
  }
----------------------------------------------------------------------------
-- | Intialize ESTG interpreter
----------------------------------------------------------------------------
initESTG :: AttachArgs -> Adaptor ESTG ()
initESTG AttachArgs {..} = do
  (dbgCmdI, dbgCmdO) <- liftIO (Unagi.newChan 100)
  (dbgOutI, dbgOutO) <- liftIO (Unagi.newChan 100)
  let dbgChan = DebuggerChan (dbgCmdO, dbgOutI)
  frameRef <- liftIO (newIORef scopes')
  registerNewDebugSession __sessionId (ESTG dbgCmdI dbgOutO program frameRef)
    $ flip catch handleDebuggerExceptions
    $ do liftIO $ loadAndRunProgram True True program [] dbgChan DbgStepByStep False
         -- ^ doesn't seem to return here
         sendTerminatedEvent (TerminatedEvent False)
         sendExitedEvent (ExitedEvent 0)

scopes' = I.fromList (zip [0..100] [ 1000 .. 2000 ])
----------------------------------------------------------------------------
-- | Exception Handler
handleDebuggerExceptions :: SomeException -> Adaptor ESTG ()
handleDebuggerExceptions e | Just ThreadKilled <- fromException e = pure ()
handleDebuggerExceptions e = do
  logError $ BL8.pack ("Caught: " <> show e)
  sendTerminatedEvent (TerminatedEvent False)
  sendExitedEvent (ExitedEvent 1)

sendStop =
  sendStoppedEvent $
      StoppedEvent
           StoppedEventReasonPause
           (Just "paused")
           (Just 0)
           False
           (Just "starting now?")
           False
           []
----------------------------------------------------------------------------
-- | Main function where requests are received and Events + Responses are returned.
-- The core logic of communicating between the client <-> adaptor <-> debugger
-- is implemented in this function.
----------------------------------------------------------------------------
talk :: Command -> Adaptor ESTG ()
----------------------------------------------------------------------------
-- | Register SessionId and initialize program in the 'AppStore'
talk CommandAttach = do
  initESTG =<< getArguments
  sendAttachResponse
    where
      emitEvent :: DebugOutput -> Adaptor ESTG ()
      emitEvent cmd = logInfo $ BL8.pack (show cmd)
----------------------------------------------------------------------------
talk CommandContinue = do
  ESTG {..} <- getDebugSession
  send CmdContinue
  sendContinueResponse (ContinueResponse True)
----------------------------------------------------------------------------
talk CommandConfigurationDone = do
  sendConfigurationDoneResponse
  sendStop
----------------------------------------------------------------------------
talk CommandDisconnect = do
  destroyDebugSession
  sendExitedEvent (ExitedEvent 0)
  sendDisconnectResponse
----------------------------------------------------------------------------
talk CommandInitialize = do
  sendInitializeResponse
  sendInitializedEvent
----------------------------------------------------------------------------
talk CommandLoadedSources = do
  modules <- getModuleListFromFullPak
  requestId <- getRequestSeqNum
  sendProgressStartEvent defaultProgressStartEvent
    { progressStartEventProgressId = "10"
    , progressStartEventRequestId = Just requestId
    }
  sendLoadedSourcesResponse
    [ defaultSource
      { sourceName = Just (T.pack moduleName)
      , sourceSourceReference = Just index
      , sourcePath = Just (T.pack moduleName)
      }
    | (index, moduleName) <- zip [1000..] modules
    ]
----------------------------------------------------------------------------
talk CommandModules = do
  sendModulesResponse (ModulesResponse [] Nothing)
----------------------------------------------------------------------------
talk CommandPause = sendPauseResponse
----------------------------------------------------------------------------
talk CommandSetBreakpoints = sendSetBreakpointsResponse []
----------------------------------------------------------------------------
talk CommandStackTrace = do
  stackFrames <- interpreterFramesToDAPFrames <$> getCurrentThreadStack
  let totalFrames = Just (length stackFrames)
  sendStackTraceResponse StackTraceResponse {..}
    -- INFO: (Just stackFramesLength) = totalFrames, this can be used to enforce pagination (if Nothing)
    -- this is good if the stack is extremely large. We'll need to add an internal counter here
    -- to support that if so.
    where
      interpreterFramesToDAPFrames
        :: [StackContinuation]
        -> [StackFrame]
      interpreterFramesToDAPFrames stackFrames =
        [ defaultStackFrame
          { stackFrameId = stackId
          , stackFrameName = T.pack (showStackCont stackCont)
          , stackFrameSource = Nothing -- TODO: Get Frame to Source mapping (e.g. Source of GHC.IO.FD)
          , stackFrameModuleId = Nothing -- TODO: Get Frame to Module mapping (e.g. GHC.IO.FD)
          }
        | (stackId, stackCont) <- zip [0..] stackFrames
        ]
----------------------------------------------------------------------------
talk CommandSource = do
  SourceArguments {..} <- getArguments
  string <- pure (show sourceArgumentsSourceReference <> " test source")
  sendProgressUpdateEvent defaultProgressUpdateEvent
    { progressUpdateEventProgressId = "10"
    , progressUpdateEventMessage = Just "Updating..."
    }
  sendSourceResponse (SourceResponse (T.pack string) Nothing)
----------------------------------------------------------------------------
talk CommandThreads = do
  (threadId, threadState) <- getCurrentThreadStateWithId
  let
    threadLabel = mkThreadLabel threadState
    threadName = T.pack (show threadId <> " " <> threadLabel)
  sendThreadsResponse [ Thread {..} ]
----------------------------------------------------------------------------
talk CommandScopes = do
  ScopesArguments {..} <- getArguments
  setCurrentFrameId scopesArgumentsFrameId
  threadState <- getCurrentThreadState
  stackFrame <- getStackFrame
  scopes <- generateScopes stackFrame
  sendScopesResponse (ScopesResponse scopes)
----------------------------------------------------------------------------
talk CommandVariables = do
  VariablesArguments {..} <- getArguments
  threadState <- getCurrentThreadState
  setCurrentScopeId variablesArgumentsVariablesReference
  variables <- getVariables
  sendVariablesResponse (VariablesResponse variables)
----------------------------------------------------------------------------
talk CommandNext = do
  NextArguments {..} <- getArguments
  send CmdStep
  resetObjectLifetimes
  sendStoppedEvent defaultStoppedEvent
    { stoppedEventReason = StoppedEventReasonStep
    , stoppedEventText = Just "Stepping..."
    , stoppedEventThreadId = Just 0
    }
----------------------------------------------------------------------------
talk CommandBreakpointLocations       = sendBreakpointLocationsResponse []
talk CommandSetDataBreakpoints        = sendSetDataBreakpointsResponse []
talk CommandSetExceptionBreakpoints   = sendSetExceptionBreakpointsResponse []
talk CommandSetFunctionBreakpoints    = sendSetFunctionBreakpointsResponse []
talk CommandSetInstructionBreakpoints = sendSetInstructionBreakpointsResponse []
----------------------------------------------------------------------------
talk cmd = logInfo $ BL8.pack ("GOT cmd " <> show cmd)
----------------------------------------------------------------------------


----------------------------------------------------------------------------
-- | Retrieves list of modules from .fullpak file
getModuleListFromFullPak :: Adaptor ESTG [String]
getModuleListFromFullPak = do
  ESTG {..} <- getDebugSession
  let appName = "app.ghc_stgapp"
  bytes <- liftIO (readModpakL fullPakPath appName id)
  GhcStgApp {..} <- liftIO (decodeThrow (BL8.toStrict bytes))
  let
    unitModules :: [String]
    unitModules = concat
      [ unitExposedModules ++ unitHiddenModules
      | UnitLinkerInfo {..} <- appLibDeps
      ]
  pure (unitModules <> appModules)

----------------------------------------------------------------------------
-- | Asynchronous call to Debugger, sends message, does not wait for response
send
  :: DebugCommand
  -> Adaptor ESTG ()
send cmd = do
  ESTG {..} <- getDebugSession
  liftIO (Unagi.writeChan inChan cmd)

----------------------------------------------------------------------------
-- | Synchronous call to Debugger, sends message and waits for response
sendAndWait :: DebugCommand -> Adaptor ESTG DebugOutput
sendAndWait cmd = do
  ESTG {..} <- getDebugSession
  liftIO $ do
    Unagi.writeChan inChan cmd
    Unagi.readChan outChan

----------------------------------------------------------------------------
-- | Receive Thread Report
-- Fails if anything but 'DbgOutThreadReport' is returned
getCurrentThreadStateWithId :: Adaptor ESTG (ThreadId, ThreadState)
getCurrentThreadStateWithId = do
  sendAndWait (CmdInternal "get-current-thread-state") >>= \case
    DbgOutThreadReport tid tstate _ _ _ ->
      pure (tid, tstate)
    otherMessage -> do
      let errorMsg
            = concat
            [ "Unexpected Message received from interpreter: "
            , show otherMessage
            ]
      logInfo (BL8.pack errorMsg)
      sendError (ErrorMessage (T.pack errorMsg)) Nothing

----------------------------------------------------------------------------
-- | Receive Thread State
getCurrentThreadState :: Adaptor ESTG ThreadState
getCurrentThreadState = snd <$> getCurrentThreadStateWithId

type ThreadId = Int
type FrameId  = Int

----------------------------------------------------------------------------
-- | Receive Thread Report
-- Fails if anything but 'DbgOutThreadReport' is returned
getCurrentThreadStack :: Adaptor ESTG [StackContinuation]
getCurrentThreadStack = tsStack <$> getCurrentThreadState

----------------------------------------------------------------------------
-- | Receive Thread Report
-- Retrieve a specific stack frame
-- TODO: Error handling
getStackFrame :: Adaptor ESTG StackContinuation
getStackFrame = do
  frameId <- getCurrentFrameId
  (!! frameId) <$> getCurrentThreadStack

----------------------------------------------------------------------------
mkThreadLabel :: ThreadState -> String
mkThreadLabel = maybe "<unknown>" (BL8.unpack . BL8.fromStrict) . tsLabel

----------------------------------------------------------------------------
generateScopes
  :: StackContinuation
  -- ^ The stack instruction that we're generating Scopes for
  -> Adaptor app [Scope]
  -- ^ List of Scopes for this StackFrame
generateScopes (CaseOf _ _ env _ _ _) = do
  frameId <- getCurrentFrameId
  localsScopeIndex <- getNextScopeId
  forM_ (M.toList env) $ \(k,v) -> do
    -- DMJ: for now everything is local.
    -- Inspect StaticOrigin to put things top-level, or as arguments, where applicable
    addVariable localsScopeIndex defaultVariable
      { variableName = T.pack (show k)
      , variableValue = T.pack (show v)
      , variableType =
          case unId k of
            Binder {..} -> Just $ T.pack (show binderType)
      }
  pure [ localScope localsScopeIndex ]
    where
      localScope localsScopeIndex
        = defaultScope
        { scopeName = "Locals"
        , scopePresentationHint = Just ScopePresentationHintLocals
        , scopeVariablesReference = localsScopeIndex
        , scopeNamedVariables = Just (M.size env)
        }
generateScopes (Update addr) = do
  frameId <- getCurrentFrameId
  localsScopeIndex <- getNextScopeId
  addVariable localsScopeIndex defaultVariable
    { variableName = "Address"
    , variableValue = T.pack (show addr)
    , variableType = Just "Ptr"
    }
  pure [ localScope localsScopeIndex ]
    where
      localScope localsScopeIndex
        = defaultScope
        { scopeName = "Locals"
        , scopePresentationHint = Just ScopePresentationHintLocals
        , scopeVariablesReference = localsScopeIndex
        , scopeNamedVariables = Just 1
        }
generateScopes (Apply atoms) = do
  frameId <- getCurrentFrameId
  localsScopeIndex <- getNextScopeId
  forM_ atoms $ \atom -> do
    addVariable localsScopeIndex defaultVariable
      { variableName = "Atom" -- DMJ: Write a pretty printing Atom function (see below)
      , variableValue = T.pack (show atom)
      , variableType = Just (T.pack (show atom))
      }
  pure [ localScope localsScopeIndex ]
    where
      localScope localsScopeIndex
        = defaultScope
        { scopeName = "Locals"
        , scopePresentationHint = Just ScopePresentationHintLocals
        , scopeVariablesReference = localsScopeIndex
        , scopeNamedVariables = Just (length atoms)
        }
generateScopes (Catch atom blockAsync interruptible) = do
  frameId <- getCurrentFrameId
  localsScopeIndex <- getNextScopeId
  addVariable localsScopeIndex defaultVariable
    { variableName = "Atom"
    , variableValue = T.pack (show atom)
    , variableType = Just (T.pack (show atom))
    }
  addVariable localsScopeIndex defaultVariable
    { variableName = "BlockAsyncExceptions"
    , variableValue = T.pack (show blockAsync)
    , variableType = Just $ T.pack $ show (typeOf blockAsync)
    }
  addVariable localsScopeIndex defaultVariable
    { variableName = "Interruptible"
    , variableValue = T.pack (show interruptible)
    , variableType = Just $ T.pack $ show (typeOf interruptible)
    }
  pure [ localScope localsScopeIndex ]
    where
      localScope localsScopeIndex
        = defaultScope
        { scopeName = "Locals"
        , scopePresentationHint = Just ScopePresentationHintLocals
        , scopeVariablesReference = localsScopeIndex
        , scopeNamedVariables = Just 3
        }
generateScopes (RestoreExMask blockAsync interruptible) = do
  frameId <- getCurrentFrameId
  localsScopeIndex <- getNextScopeId
  addVariable localsScopeIndex defaultVariable
    { variableName = "BlockAsyncExceptions"
    , variableValue = T.pack (show blockAsync)
    , variableType = Just $ T.pack $ show (typeOf blockAsync)
    }
  addVariable localsScopeIndex defaultVariable
    { variableName = "Interruptible"
    , variableValue = T.pack (show interruptible)
    , variableType = Just $ T.pack $ show (typeOf interruptible)
    }
  pure [ localScope localsScopeIndex ]
    where
      localScope localsScopeIndex
        = defaultScope
        { scopeName = "Locals"
        , scopePresentationHint = Just ScopePresentationHintLocals
        , scopeVariablesReference = localsScopeIndex
        , scopeNamedVariables = Just 2
        }
generateScopes (RunScheduler reason) = do
  frameId <- getCurrentFrameId
  localsScopeIndex <- getNextScopeId
  addVariable localsScopeIndex defaultVariable
    { variableName = T.pack $ show (typeOf reason)
    , variableValue = T.pack (show reason)
    , variableType = Just $ T.pack $ show (typeOf reason)
    }
  pure [ localScope localsScopeIndex ]
    where
      localScope localsScopeIndex
        = defaultScope
        { scopeName = "Locals"
        , scopePresentationHint = Just ScopePresentationHintLocals
        , scopeVariablesReference = localsScopeIndex
        , scopeNamedVariables = Just 1
        }
generateScopes (Atomically atom) = do
  frameId <- getCurrentFrameId
  localsScopeIndex <- getNextScopeId
  addVariable localsScopeIndex defaultVariable
    { variableName = "Atom"
    , variableValue = T.pack (show atom)
    , variableType = Just (T.pack (show atom))
    }
  pure [ localScope localsScopeIndex ]
    where
      localScope localsScopeIndex
        = defaultScope
        { scopeName = "Locals"
        , scopePresentationHint = Just ScopePresentationHintLocals
        , scopeVariablesReference = localsScopeIndex
        , scopeNamedVariables = Just 1
        }
generateScopes (CatchRetry atom1 atom2 interruptible) = do
  frameId <- getCurrentFrameId
  localsScopeIndex <- getNextScopeId
  addVariable localsScopeIndex defaultVariable
    { variableName = "STM1"
    , variableValue = T.pack (show atom1)
    , variableType = Just (T.pack (show (typeOf atom1)))
    }
  addVariable localsScopeIndex defaultVariable
    { variableName = "STM2"
    , variableValue = T.pack (show atom2)
    , variableType = Just (T.pack (show (typeOf atom2)))
    }
  addVariable localsScopeIndex defaultVariable
    { variableName = "altCode"
    , variableValue = T.pack (show interruptible)
    , variableType = Just (T.pack (show (typeOf interruptible)))
    }
  pure [ localScope localsScopeIndex ]
    where
      localScope localsScopeIndex
        = defaultScope
        { scopeName = "Locals"
        , scopePresentationHint = Just ScopePresentationHintLocals
        , scopeVariablesReference = localsScopeIndex
        , scopeNamedVariables = Just 3
        }
generateScopes (CatchSTM atom1 atom2) = do
  frameId <- getCurrentFrameId
  localsScopeIndex <- getNextScopeId
  addVariable localsScopeIndex defaultVariable
    { variableName = "STM"
    , variableValue = T.pack (show atom1)
    , variableType = Just (T.pack (show (typeOf atom1)))
    }
  addVariable localsScopeIndex defaultVariable
    { variableName = "Exception Handler"
    , variableValue = T.pack (show atom2)
    , variableType = Just (T.pack (show (typeOf atom2)))
    }
  pure [ localScope localsScopeIndex ]
    where
      localScope localsScopeIndex
        = defaultScope
        { scopeName = "Locals"
        , scopePresentationHint = Just ScopePresentationHintLocals
        , scopeVariablesReference = localsScopeIndex
        , scopeNamedVariables = Just 2
        }
generateScopes DataToTagOp = do
  frameId <- getCurrentFrameId
  localsScopeIndex <- getNextScopeId
  pure [ localScope localsScopeIndex ]
    where
      localScope localsScopeIndex
        = defaultScope
        { scopeName = "Locals"
        , scopePresentationHint = Just ScopePresentationHintLocals
        , scopeVariablesReference = localsScopeIndex
        , scopeNamedVariables = Just 0
        }
generateScopes (RaiseOp atom) = do
  frameId <- getCurrentFrameId
  localsScopeIndex <- getNextScopeId
  addVariable localsScopeIndex defaultVariable
    { variableName = "Atom"
    , variableValue = T.pack (show atom)
    , variableType = Just (T.pack (show (typeOf atom)))
    }
  pure [ localScope localsScopeIndex ]
    where
      localScope localsScopeIndex
        = defaultScope
        { scopeName = "Locals"
        , scopePresentationHint = Just ScopePresentationHintLocals
        , scopeVariablesReference = localsScopeIndex
        , scopeNamedVariables = Just 1
        }
generateScopes (KeepAlive atom) = do
  frameId <- getCurrentFrameId
  localsScopeIndex <- getNextScopeId
  addVariable localsScopeIndex defaultVariable
    { variableName = "Atom"
    , variableValue = T.pack (show atom)
    , variableType = Just (T.pack (show (typeOf atom)))
    }
  pure [ localScope localsScopeIndex ]
    where
      localScope localsScopeIndex
        = defaultScope
        { scopeName = "Locals"
        , scopePresentationHint = Just ScopePresentationHintLocals
        , scopeVariablesReference = localsScopeIndex
        , scopeNamedVariables = Just 1
        }
generateScopes (DebugFrame atom) = do
  frameId <- getCurrentFrameId
  localsScopeIndex <- getNextScopeId
  addVariable localsScopeIndex defaultVariable
    { variableName = "Atom"
    , variableValue = T.pack (show atom)
    , variableType = Just (T.pack (show (typeOf atom)))
    }
  pure [ localScope localsScopeIndex ]
    where
      localScope localsScopeIndex
        = defaultScope
        { scopeName = "Locals"
        , scopePresentationHint = Just ScopePresentationHintLocals
        , scopeVariablesReference = localsScopeIndex
        , scopeNamedVariables = Just 1
        }

-- getAtomNameAndValueAndType = \case
--   HeapPtr addr -> ("HeapPtr", showText addr, showType addr)
--   Literal lit -> ("Literal", showLit lit
--   | Void
--   | PtrAtom !PtrOrigin {-# UNPACK #-}(GHC.Ptr.Ptr GHC.Word.Word8)
--   | IntAtom {-# UNPACK #-}Int
--   | WordAtom {-# UNPACK #-}Word
--   | FloatAtom {-# UNPACK #-}Float
--   | DoubleAtom {-# UNPACK #-}Double
--   | MVar {-# UNPACK #-}Int
--   | MutVar {-# UNPACK #-}Int
--   | TVar {-# UNPACK #-}Int
--   | Stg.Interpreter.Base.Array !ArrIdx
--   | MutableArray !ArrIdx
--   | SmallArray !SmallArrIdx
--   | SmallMutableArray !SmallArrIdx
--   | ArrayArray !ArrayArrIdx
--   | MutableArrayArray !ArrayArrIdx
--   | ByteArray !ByteArrayIdx
--   | MutableByteArray !ByteArrayIdx
--   | WeakPointer {-# UNPACK #-}Int
--   | StableName {-# UNPACK #-}Int
--   | ThreadId {-# UNPACK #-}Int
--   | LiftedUndefined
--   | Rubbish
--     where
--       showLit (LitChar char) = ("LitChar", T.pack [char], )
--       showType = Just . T.pack . show . typeOf
--       showText = T.pack . show

-- + on watch causes "CommandEvaluate"
-- right click - set value - [127.0.0.1:49599][INFO][GOT cmd CommandSetVariable]
-- right click - copy value - [127.0.0.1:49599][INFO][GOT cmd CommandEvaluate]
-- save breakpoints from breakpoints request into AdaptrClient set, set them on the interpreter after configuration done (not attach)

-- sourceSTG
--   = Source
--       (Just "Main.stg")
--       (Just "./exe/stg.txt")
--       (Just 1)
--       (Just SourcePresentationHintEmphasize)
--       Nothing -- (Just "STG source code")
--       (Just [ sourceHS, sourceCore ])
--       Nothing
--       Nothing

-- sourceHS
--   = Source
--       (Just "Main.hs")
--       (Just "./exe/hs.txt")
--       (Just 2)
--       (Just SourcePresentationHintNormal)
--       Nothing -- (Just "Haskell source code")
--       Nothing
--       Nothing
--       Nothing

-- sourceCore
--   = Source
--       (Just "Main.cmm")
--       (Just "./exe/cmm.txt")
--       (Just 3)
--       (Just SourcePresentationHintDeemphasize)
--       Nothing -- (Just "Core source code")
--       Nothing
--       Nothing
--       Nothing

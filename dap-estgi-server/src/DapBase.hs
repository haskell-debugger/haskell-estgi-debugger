{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module DapBase where

import           Data.Bimap                            ( Bimap )
import qualified Data.Bimap                            as Bimap
import           Data.Text                             ( Text )
import qualified Data.Text                             as T
import           Data.Map.Strict                       ( Map )
import qualified Data.Map.Strict                       as Map
import           Data.IntSet                           ( IntSet )
import           Data.Set                              ( Set )
import           Data.IntMap.Strict                    ( IntMap )
import qualified Data.IntMap.Strict                    as IntMap
import qualified Data.ByteString.Lazy.Char8            as BL8 ( pack, unpack, fromStrict, toStrict )
import qualified Control.Concurrent.MVar               as MVar
import           Control.Monad.IO.Class                (liftIO)

import           Stg.Interpreter.Base                  hiding (lookupEnv, getCurrentThreadState, Breakpoint)
import qualified Stg.Interpreter.Base                  as Stg
import           Stg.Interpreter.GC.GCRef
import           Stg.Syntax                            hiding (sourceName, Scope)
import           DAP                                   hiding (send)

type PackageName = Text
type QualifiedModuleName = Text
type BreakpointId = Int
type SourceId = Int
type ThreadId = Int

{-
  IDEA:
    pure design
      use pure and unique resource descriptors to select items from StgState
      maintain a bimap between the pure resource descriptors and DAP integer ids

    IMPORTANT: avoid use of counters
    BENEFIT:
      DAP request order independence
      no resource caching is needed
      stateless
      use of descriptive resource identification instead of integers

  IDEA:
    ResourceID ADT - structured key
    idMap :: ResourceID -> Int

    DAP request argument -> estg domian idientifiers
    request argument's id -> estg domain 

  resource ids
    threadRef     = thread id
    frameRef      = thread id + frame index
    scopeRef      = thread id + frame index + argument index
    variablesRef  = ??
    sourceRef

  HINT: VariablesRef -> [Variable]

  DAP id types:
    thread
    stack frame
    variable


  Threads             args: NONE
    StackTrace        args: threadId
      Scopes          args: frameId
        Variables     args: variablesRef
          ...
            Variables
-}

type StackFrameIndex = Int

data DapFrameIdDescriptor
  = FrameId_CurrentThreadTopStackFrame
  | FrameId_ThreadStackFrame ThreadId StackFrameIndex
  deriving (Show, Eq, Ord)

data ValueRoot
  = ValueRoot_StackFrame  DapFrameIdDescriptor
  | ValueRoot_Value       (RefNamespace, Int)
  deriving (Show, Eq, Ord)

data DapVariablesRefDescriptor
  = VariablesRef_StackFrameVariables  DapFrameIdDescriptor
--  | VariablesRef_HeapObject           DapFrameIdDescriptor Int
  | VariablesRef_Value                ValueRoot RefNamespace Int
  deriving (Show, Eq, Ord)

data SourceCodeDescriptor
  = Haskell   PackageName QualifiedModuleName
  | GhcCore   PackageName QualifiedModuleName
  | GhcStg    PackageName QualifiedModuleName
  | Cmm       PackageName QualifiedModuleName
  | Asm       PackageName QualifiedModuleName
  | ExtStg    PackageName QualifiedModuleName
  | FFICStub  PackageName QualifiedModuleName
  | FFIHStub  PackageName QualifiedModuleName
  | ModInfo   PackageName QualifiedModuleName
  | ForeignC  PackageName FilePath
  deriving (Show, Read, Eq, Ord)

data DapSourceRefDescriptor
  = SourceRef_SourceFileInFullpak SourceCodeDescriptor
  deriving (Show, Read, Eq, Ord)

----------------------------------------------------------------------------
-- | External STG Interpreter application internal state
data ESTG
  = ESTG
  { debuggerChan      :: DebuggerChan
  , fullPakPath       :: String
  , breakpointMap     :: Map Stg.Breakpoint IntSet
  , sourceCodeSet     :: Set SourceCodeDescriptor
  , unitIdMap         :: Bimap UnitId PackageName
  , haskellSrcPathMap :: Bimap Name SourceCodeDescriptor
  , dapSourceNameMap  :: Bimap Text DapSourceRefDescriptor

  -- application specific resource handling

  , dapSourceRefMap       :: !(Bimap DapSourceRefDescriptor Int)
    -- ^ Used to track source reference IDs
    --
  , dapFrameIdMap         :: !(Bimap DapFrameIdDescriptor Int)
    -- ^ Used to track stack frame IDs
    --
  , dapVariablesRefMap    :: !(Bimap DapVariablesRefDescriptor Int)
    -- ^ Used to track variablesReferences
    --
  , dapStackFrameCache    :: !(Map DapFrameIdDescriptor StackContinuation)
    -- ^ Stores the assigned StackContinuation for each DAP FrameId (Int)
    --
  , nextFreshBreakpointId :: !Int
    -- ^ monotinic counter for unique BreakpointId assignment
    --
  }

-- resource handling

getsApp f = f <$> getDebugSession
modifyApp = updateDebugSession

----------------------------------------------------------------------------
-- | Synchronous call to Debugger, sends message and waits for response
sendAndWait :: DebugCommand -> Adaptor ESTG DebugOutput
sendAndWait cmd = do
  ESTG {..} <- getDebugSession
  let DebuggerChan{..} = debuggerChan
  liftIO $ do
    MVar.putMVar dbgSyncRequest cmd
    MVar.takeMVar dbgSyncResponse

getStgState :: Adaptor ESTG StgState
getStgState = do
  sendAndWait (CmdInternal "get-stg-state") >>= \case
    DbgOutStgState stgState ->
      pure stgState
    otherMessage -> do
      let errorMsg
            = concat
            [ "Unexpected Message received from interpreter: "
            , show otherMessage
            ]
      logInfo (BL8.pack errorMsg)
      sendError (ErrorMessage (T.pack errorMsg)) Nothing

----------------------------------------------------------------------------
mkThreadLabel :: ThreadState -> String
mkThreadLabel = maybe "" (BL8.unpack . BL8.fromStrict) . tsLabel

getFrameId :: DapFrameIdDescriptor -> Adaptor ESTG Int
getFrameId key = do
  getsApp (Bimap.lookup key . dapFrameIdMap) >>= \case
    Just frameId -> pure frameId
    Nothing -> do
      frameId <- getsApp (succ . Bimap.size . dapFrameIdMap)
      modifyApp $ \s -> s {dapFrameIdMap = Bimap.insert key frameId (dapFrameIdMap s)}
      pure frameId

getVariablesRef :: DapVariablesRefDescriptor -> Adaptor ESTG Int
getVariablesRef key = do
  getsApp (Bimap.lookup key . dapVariablesRefMap) >>= \case
    Just varRef -> pure varRef
    Nothing -> do
      varRef <- getsApp (succ . Bimap.size . dapVariablesRefMap)
      modifyApp $ \s -> s {dapVariablesRefMap = Bimap.insert key varRef (dapVariablesRefMap s)}
      pure varRef

addStackFrameToCache :: DapFrameIdDescriptor -> StackContinuation -> Adaptor ESTG ()
addStackFrameToCache frameIdDesc stackCont = do
  modifyApp $ \s -> s {dapStackFrameCache = Map.insert frameIdDesc stackCont (dapStackFrameCache s)}

getStackFrameFromCache :: DapFrameIdDescriptor -> Adaptor ESTG StackContinuation
getStackFrameFromCache frameIdDesc = do
  ESTG {..} <- getDebugSession
  case Map.lookup frameIdDesc dapStackFrameCache of
    Nothing         -> sendError (ErrorMessage (T.pack $ "Unknown stack frame: " ++ show frameIdDesc)) Nothing
    Just stackCont  -> pure stackCont

-- | Invoked when a StepEvent has occurred
resetObjectLifetimes :: Adaptor ESTG ()
resetObjectLifetimes = do
  modifyApp $ \s -> s
    { dapFrameIdMap         = Bimap.empty
    , dapVariablesRefMap    = Bimap.empty
    , dapStackFrameCache    = mempty
    }

getFreshBreakpointId :: Adaptor ESTG BreakpointId
getFreshBreakpointId = do
  bkpId <- getsApp nextFreshBreakpointId
  modifyApp $ \s -> s { nextFreshBreakpointId = nextFreshBreakpointId s + 1 }
  pure bkpId

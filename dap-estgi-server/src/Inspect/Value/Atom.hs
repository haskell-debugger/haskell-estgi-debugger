{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE LambdaCase #-}
module Inspect.Value.Atom where

import           Data.List
import           Data.String.Conversions               (cs)
import           Data.IntMap.Strict                    ( IntMap )
import qualified Data.IntMap.Strict                    as IntMap
import           Data.Text                             ( Text )
import qualified Data.Text.Lazy                        as LazyText
import qualified Text.Pretty.Simple                    as PP
import           Foreign.Ptr

import           Stg.Interpreter.Base                  hiding (lookupEnv, getCurrentThreadState, Breakpoint)
import           Stg.Interpreter.GC.GCRef
import           Stg.Syntax                            hiding (sourceName, Scope)

import DAP
import DapBase

showLitNumType :: LitNumType -> String
showLitNumType = \case
  LitNumInt8   -> "Int8"
  LitNumInt16  -> "Int16"
  LitNumInt32  -> "Int32"
  LitNumInt64  -> "Int64"
  LitNumWord   -> "Word"
  LitNumWord8  -> "Word8"
  LitNumWord16 -> "Word16"
  LitNumWord32 -> "Word32"
  LitNumWord64 -> "Word64"

showElemRep :: PrimElemRep -> String
showElemRep = \case
  Int8ElemRep   -> "Int8Rep"
  Int16ElemRep  -> "Int16Rep"
  Int32ElemRep  -> "Int32Rep"
  Int64ElemRep  -> "Int64Rep"
  Word8ElemRep  -> "Word8Rep"
  Word16ElemRep -> "Word16Rep"
  Word32ElemRep -> "Word32Rep"
  Word64ElemRep -> "Word64Rep"
  FloatElemRep  -> "FloatRep"
  DoubleElemRep -> "DoubleRep"

showRubbishType :: Type -> String
showRubbishType (SingleValue primRep) = showPrimRep primRep

showRubbishType (UnboxedTuple primReps) =
  concat
  [ "(# "
  , intercalate "," (showPrimRep <$> primReps)
  , " #)"
  ]
showRubbishType PolymorphicRep = show PolymorphicRep

showPrimRep :: PrimRep -> String
showPrimRep (VecRep n primElemRep) =
  concat
  [ "<"
  , intercalate "," (replicate n (showElemRep primElemRep))
  , ">"
  ]
showPrimRep rep = show rep

getAtomTypeAndValueM
  :: ValueRoot
  -> Atom
  -> Adaptor ESTG (String, String, Int)
getAtomTypeAndValueM valueRoot atom = do
  ss@StgState{..} <- getStgState
  case atom of
    HeapPtr addr
      | Just o <- IntMap.lookup addr ssHeap
      -> do
        varsRef <- getVariablesRef $ VariablesRef_Value valueRoot NS_HeapPtr addr
        pure ("HeapPtr", show addr ++ " " ++ getHeapObjectSummary o ++ "\n --- \n" ++ LazyText.unpack (PP.pShowNoColor o), varsRef)
    _
      | (t, v) <- getAtomTypeAndValue ss atom
      -> pure (t, v, 0)

getAtomTypeAndValue
  :: StgState
  -> Atom
  -> (String, String)
getAtomTypeAndValue StgState{..} atom = case atom of
  HeapPtr addr
    | Just o <- IntMap.lookup addr ssHeap
    -> ("HeapPtr", show addr ++ "\n --- \n" ++ LazyText.unpack (PP.pShowNoColor o))
  Literal (LitChar char)                       -> ("Char", show char)
  Literal (LitString bytes)                    -> ("String", cs $ show bytes)
  Literal LitNullAddr                          -> ("Address", "0x00000000")
  Literal (LitFloat float)                     -> ("Float", show float)
  Literal (LitDouble double)                   -> ("Double", show double)
  Literal (LitLabel labelName FunctionLabel{}) -> ("Foreign Function", cs labelName)
  Literal (LitLabel labelName DataLabel)       -> ("Foreign Data", cs labelName)
  Literal (LitNumber num value)                -> (showLitNumType num, show value)
  Literal (LitRubbish rubbishType)             -> ("Rubbish", showRubbishType rubbishType)
  Void                                         -> ("Void", "()")
  PtrAtom _ x                                  -> ("Ptr", show x)
  IntAtom x                                    -> ("Int", show x)
  WordAtom x                                   -> ("Word", show x)
  FloatAtom x                                  -> ("Float", show x)
  DoubleAtom x                                 -> ("Double", show x)
  MVar x                                       -> ("MVar", show atom)
  MutVar x                                     -> ("MutVar", show atom)
  TVar x                                       -> ("TVar", show atom)
  Array idx                                    -> ("Array", show atom)
  MutableArray idx                             -> ("MutableArray", show atom)
  SmallArray idx                               -> ("SmallArray", show atom)
  SmallMutableArray idx                        -> ("SmallMutableArray", show atom)
  ArrayArray idx                               -> ("ArrayArray", show atom)
  MutableArrayArray idx                        -> ("MutableArrayArray", show atom)
  ByteArray idx                                -> ("ByteArray", show atom)
  MutableByteArray idx                         -> ("MutableByteArray", show atom)
  WeakPointer x                                -> ("WeakPoint", show atom)
  StableName x                                 -> ("StableName", show atom)
  ThreadId x                                   -> ("ThreadId", show atom)
  LiftedUndefined                              -> ("LiftedUndefined","undefined")

getHeapObjectSummary :: HeapObject -> String
getHeapObjectSummary = \case
  Con{..} -> "Con: " ++ show hoCon
  Closure{..} -> if hoCloMissing == 0
    then "Thunk: " ++ show hoName
    else "Closure: " ++ show hoName
  BlackHole{} -> "BlackHole"
  ApStack{} -> "ApStack"
  RaiseException{} -> "RaiseException"

getVariableForAtom :: Text -> ValueRoot -> Atom -> Adaptor ESTG Variable
getVariableForAtom name valueRoot atom = do
  (variableType, variableValue, varsRef) <- getAtomTypeAndValueM valueRoot atom
  pure defaultVariable
    { variableName  = name
    , variableValue = cs variableValue
    , variableType  = Just (cs variableType)
    , variableVariablesReference = varsRef
    }

valueToAtom :: RefNamespace -> Int -> Adaptor ESTG Atom
valueToAtom ns i = do
  StgState{..} <- getStgState
  pure $ case ns of
    NS_HeapPtr            -> HeapPtr i
    NS_StablePointer      -> PtrAtom (StablePtr i) (intPtrToPtr $ IntPtr i)
    NS_MVar               -> MVar i
    NS_MutVar             -> MutVar i
    NS_TVar               -> TVar i
    NS_Array              -> Array $ ArrIdx i
    NS_MutableArray       -> MutableArray $ MutArrIdx i
    NS_SmallArray         -> SmallArray $ SmallArrIdx i
    NS_SmallMutableArray  -> SmallMutableArray $ SmallMutArrIdx i
    NS_ArrayArray         -> ArrayArray $ ArrayArrIdx i
    NS_MutableArrayArray  -> MutableArrayArray $ ArrayMutArrIdx i
    NS_MutableByteArray
      | Just ByteArrayDescriptor{..} <- IntMap.lookup i ssMutableByteArrays
      -> MutableByteArray $ ByteArrayIdx
          { baId        = i
          , baPinned    = baaPinned
          , baAlignment = baaAlignment
          }
    NS_WeakPointer        -> WeakPointer i
    NS_StableName         -> StableName i
    NS_Thread             -> ThreadId i

getValueSummary :: RefNamespace -> Int -> Adaptor ESTG String
getValueSummary ns i = do
  StgState{..} <- getStgState
  pure $ case ns of
    NS_HeapPtr
      | Just o <- IntMap.lookup i ssHeap
      -> "HeapPtr " ++ getHeapObjectSummary o
    _ -> show (ns, i)

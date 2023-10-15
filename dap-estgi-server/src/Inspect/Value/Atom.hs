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
getAtomTypeAndValue StgState{..} = \case
  HeapPtr addr
    | Just o <- IntMap.lookup addr ssHeap
    -> ("HeapPtr", show addr ++ "\n --- \n" ++ LazyText.unpack (PP.pShowNoColor o))
  Literal (LitChar char)                       -> ("Char", [char])
  Literal (LitString bytes)                    -> ("String", cs bytes)
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
  MVar x                                       -> ("MVar", show x)
  MutVar x                                     -> ("MutVar", show x)
  TVar x                                       -> ("TVar", show x)
  Array idx                                    -> ("Array", show idx)
  MutableArray idx                             -> ("MutableArray", show idx)
  SmallArray idx                               -> ("SmallArray", show idx)
  SmallMutableArray idx                        -> ("SmallMutableArray", show idx)
  ArrayArray idx                               -> ("ArrayArray", show idx)
  MutableArrayArray idx                        -> ("MutableArrayArray", show idx)
  ByteArray idx                                -> ("ByteArray", show idx)
  MutableByteArray idx                         -> ("MutableByteArray", show idx)
  WeakPointer x                                -> ("WeakPoint", show x)
  StableName x                                 -> ("StableName", show x)
  ThreadId x                                   -> ("ThreadId", show x)
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

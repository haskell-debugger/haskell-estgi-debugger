{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE OverloadedStrings   #-}
module Breakpoints where

import           Text.Read                             ( readMaybe )
import           Data.Maybe                            ( fromMaybe, maybeToList )
import           Data.List                             ( sortOn )
import           Control.Monad
import           Data.String.Conversions               (cs)
import qualified Data.Text                             as T
import qualified Data.Bimap                            as Bimap
import qualified Data.IntSet                           as IntSet
import qualified Data.Map.Strict                       as Map
import qualified Stg.Interpreter.Base                  as Stg
import           Stg.Interpreter.Base                  hiding (lookupEnv, getCurrentThreadState, Breakpoint)
import           Stg.Syntax                            hiding (sourceName, Scope)
import           Stg.IRLocation

import DAP
import DapBase
import SourceCode

----------------------------------------------------------------------------
-- | Clears the currently known breakpoint set
clearBreakpoints :: Adaptor ESTG ()
clearBreakpoints = do
  updateDebugSession $ \estg -> estg {breakpointMap = mempty}

----------------------------------------------------------------------------
-- | Adds new BreakpointId for a givent StgPoint
addNewBreakpoint :: Stg.Breakpoint -> Adaptor ESTG BreakpointId
addNewBreakpoint breakpoint = do
  bkpId <- getFreshBreakpointId
  updateDebugSession $ \estg@ESTG{..} -> estg {breakpointMap = Map.insertWith mappend breakpoint (IntSet.singleton bkpId) breakpointMap}
  pure bkpId

commandSetBreakpoints :: Adaptor ESTG ()
commandSetBreakpoints = do
  SetBreakpointsArguments {..} <- getArguments
  maybeSourceRef <- getValidSourceRefFromSource setBreakpointsArgumentsSource

  -- the input SourceRef might be a remain of a previous DAP session, update it with the new valid one
  let refUpdatedSource = setBreakpointsArgumentsSource { sourceSourceReference = maybeSourceRef }

  clearBreakpoints
  {-
    supports placing breakpoint on:
      - Haskell
      - ExtStg
  -}
  ESTG {..} <- getDebugSession
  case (setBreakpointsArgumentsBreakpoints, maybeSourceRef, maybeSourceRef >>= flip Bimap.lookupR dapSourceRefMap) of
    -- HINT: breakpoint on Haskell
    (Just sourceBreakpoints, Just sourceRef, Just hsCodeDesc@(Haskell pkg mod))
      | Just extStgSourceRef <- Bimap.lookup (ExtStg pkg mod) dapSourceRefMap
      , Just hsSourceFilePath <- Bimap.lookupR hsCodeDesc haskellSrcPathMap
      -> do
      (_sourceCodeText, _locations, hsSrcLocs) <- getSourceFromFullPak extStgSourceRef
      breakpoints <- forM sourceBreakpoints $ \SourceBreakpoint{..} -> do
        -- filter all relevant ranges
        {-
          SP_RhsClosureExpr
        -}
        let onlySupported = \case
              SP_RhsClosureExpr{} -> True
              _ -> True -- TODO
        let relevantLocations = filter (onlySupported . fst . fst) $ case sourceBreakpointColumn of
              Nothing ->
                [ (p, spanSize)
                | p@(_,SourceNote RealSrcSpan'{..} _) <- hsSrcLocs
                , srcSpanFile == hsSourceFilePath
                , srcSpanSLine <= sourceBreakpointLine
                , srcSpanELine >= sourceBreakpointLine
                , let spanSize = (srcSpanELine - srcSpanSLine, srcSpanECol - srcSpanSCol)
                ]
              Just col  ->
                [ (p, spanSize)
                | p@(_,SourceNote RealSrcSpan'{..} _) <- hsSrcLocs
                , srcSpanFile == hsSourceFilePath
                , srcSpanSLine <= sourceBreakpointLine
                , srcSpanELine >= sourceBreakpointLine
                , srcSpanSCol <= col
                , srcSpanECol >= col
                , let spanSize = (srcSpanELine - srcSpanSLine, srcSpanECol - srcSpanSCol)
                ]
        debugMessage . cs . unlines $ "relevant haskell locations:" : map show relevantLocations
        -- use the first location found
        -- HINT: locations are sorted according the span size, small spans are preferred more
        case map fst . take 1 $ sortOn snd relevantLocations of
          (stgPoint@(SP_RhsClosureExpr _closureName), SourceNote RealSrcSpan'{..} _) : _ -> do
            let hitCount = fromMaybe 0 (sourceBreakpointHitCondition >>= readMaybe . T.unpack) :: Int
            sendAndWait (CmdAddBreakpoint (BkpStgPoint stgPoint) hitCount)
            bkpId <- addNewBreakpoint $ BkpStgPoint stgPoint
            pure $ defaultBreakpoint
              { breakpointVerified  = True
              , breakpointSource    = Just refUpdatedSource
              , breakpointLine      = Just srcSpanSLine
              , breakpointColumn    = Just srcSpanSCol
              , breakpointEndLine   = Just srcSpanELine
              , breakpointEndColumn = Just srcSpanECol
              , breakpointId        = Just bkpId
              }
          _ ->
            pure $ defaultBreakpoint
              { breakpointVerified  = False
              , breakpointSource    = Just refUpdatedSource
              , breakpointMessage   = Just "no hs code found"
              }
      sendSetBreakpointsResponse breakpoints

    -- HINT: breakpoint on ExtStg
    (Just sourceBreakpoints, Just sourceRef, Just ExtStg{}) -> do
      (_sourceCodeText, locations, _hsSrcLocs) <- getSourceFromFullPak sourceRef
      breakpoints <- forM sourceBreakpoints $ \SourceBreakpoint{..} -> do
        -- filter all relevant ranges
        {-
          SP_RhsClosureExpr
        -}
        let onlySupported = \case
              SP_RhsClosureExpr{} -> True
              _ -> False
        let relevantLocations = filter (onlySupported . fst) $ case sourceBreakpointColumn of
              Nothing ->
                [ p
                | p@(_,((startRow, startCol), (endRow, endCol))) <- locations
                , startRow <= sourceBreakpointLine
                , endRow >= sourceBreakpointLine
                ]
              Just col  ->
                [ p
                | p@(_,((startRow, startCol), (endRow, endCol))) <- locations
                , startRow <= sourceBreakpointLine
                , endRow >= sourceBreakpointLine
                , startCol <= col
                , endCol >= col
                ]
        debugMessage . cs $ "relevantLocations: " ++ show relevantLocations
        -- use the first location found
        case sortOn snd relevantLocations of
          (stgPoint@(SP_RhsClosureExpr _closureName), ((startRow, startCol), (endRow, endCol))) : _ -> do
            let hitCount = fromMaybe 0 (sourceBreakpointHitCondition >>= readMaybe . T.unpack) :: Int
            sendAndWait (CmdAddBreakpoint (BkpStgPoint stgPoint) hitCount)
            bkpId <- addNewBreakpoint $ BkpStgPoint stgPoint
            pure $ defaultBreakpoint
              { breakpointVerified  = True
              , breakpointSource    = Just refUpdatedSource
              , breakpointLine      = Just startRow
              , breakpointColumn    = Just startCol
              , breakpointEndLine   = Just endRow
              , breakpointEndColumn = Just endCol
              , breakpointId        = Just bkpId
              }
          _ ->
            pure $ defaultBreakpoint
              { breakpointVerified  = False
              , breakpointSource    = Just refUpdatedSource
              , breakpointMessage   = Just "no code found"
              }
      sendSetBreakpointsResponse breakpoints
    v -> do
      sendSetBreakpointsResponse []

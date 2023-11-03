{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings   #-}
module SourceLocation where

import           Data.Maybe                            ( fromMaybe, maybeToList )
import           Control.Monad.IO.Class                (liftIO)
import           Control.Monad
import           Data.String.Conversions               (cs)
import           Data.Text                             ( Text )
import qualified Data.Text                             as T
import qualified Data.Aeson                            as Aeson
import qualified Data.Bimap                            as Bimap
import qualified Data.Map.Strict                       as Map

import           Stg.Syntax                            hiding (sourceName, Scope)
import           Stg.IRLocation

import DAP
import DapBase
import SourceCode
import           CustomCommands

getUnitIdAndModuleNameForStgPoint :: StgPoint -> (UnitId, ModuleName)
getUnitIdAndModuleNameForStgPoint = \case
  SP_CaseScrutineeExpr StgId{..}  -> (UnitId siUnitId, ModuleName siModuleName)
  SP_LetExpr stgPoint             -> getUnitIdAndModuleNameForStgPoint stgPoint
  SP_LetNoEscapeExpr stgPoint     -> getUnitIdAndModuleNameForStgPoint stgPoint
  SP_RhsClosureExpr StgId{..}     -> (UnitId siUnitId, ModuleName siModuleName)
  SP_AltExpr StgId{..} _idx       -> (UnitId siUnitId, ModuleName siModuleName)
  SP_RhsCon StgId{..}             -> (UnitId siUnitId, ModuleName siModuleName)
  SP_Binding StgId{..}            -> (UnitId siUnitId, ModuleName siModuleName)
  SP_Tickish stgPoint             -> getUnitIdAndModuleNameForStgPoint stgPoint

-- TODO: precalc in a map
getSourceAndPositionForStgPoint :: StgPoint -> Adaptor ESTG (Maybe Source, Int, Int, Int, Int)
getSourceAndPositionForStgPoint stgPoint = do
  let (unitId, moduleNameBS) = getUnitIdAndModuleNameForStgPoint stgPoint
  ESTG {..} <- getDebugSession
  packageName <- case Bimap.lookup unitId unitIdMap of
    Nothing -> sendError (ErrorMessage (T.pack $ "Unknown unit id: " ++ show unitId)) Nothing
    Just v  -> pure v
  let moduleName = cs $ getModuleName moduleNameBS
  source <- getSourceFromSourceCodeDescriptor $ ExtStg packageName moduleName
  let Just sourceRef = sourceSourceReference source
  (_sourceCodeText, locations, hsSrcLocs) <- getSourceFromFullPak sourceRef
  let inModule pkg mod (_, SourceNote{..})
        | RealSrcSpan'{..} <- sourceSpan
        , Just hsSrcDesc <- Bimap.lookup srcSpanFile haskellSrcPathMap
        = hsSrcDesc == Haskell pkg mod
      inModule _ _ _ = False

      stgPointLocs  = filter ((== stgPoint) . fst) hsSrcLocs
      hsModLocs     = filter (inModule packageName moduleName) stgPointLocs
  forM_ stgPointLocs $ \(_, tickish) -> liftIO $ print tickish
  {-
    source location priorities:
      - haskell module local
      - stg
  -}
  case hsModLocs of
    (_, SourceNote{..}) : _
      | RealSrcSpan'{..} <- sourceSpan
      , Just hsSrcDesc <- Bimap.lookup srcSpanFile haskellSrcPathMap
      -> do
      sourceHs <- getSourceFromSourceCodeDescriptor hsSrcDesc
      pure (Just sourceHs, srcSpanSLine, srcSpanSCol, srcSpanELine, srcSpanECol)
    _ -> do
      case filter ((== stgPoint) . fst) locations of
        (_, ((line, column),(endLine, endColumn))) : _ -> do
          pure (Just source, line, column, endLine, endColumn)
        _ -> do
          pure (Just source, 0, 0, 0, 0)

getStgSourceLocJSONText :: StgPoint -> Adaptor ESTG (Maybe Text)
getStgSourceLocJSONText stgPoint = fmap (cs . Aeson.encode) <$> getStgSourceLocJSON stgPoint

getStgSourceLocJSON :: StgPoint -> Adaptor ESTG (Maybe Aeson.Value)
getStgSourceLocJSON stgPoint = do
  (mSource, startL, startC, endL, endC) <- getSourceAndPositionForStgPoint stgPoint
  let mkPosObject line column = Aeson.object
        [ ("line",    Aeson.Number $ fromIntegral line)
        , ("column",  Aeson.Number $ fromIntegral column)
        ]
      srcLocJson = do
        Source{..} <- mSource
        path <- sourcePath
        pure $ Aeson.object
          [ ("path",  Aeson.String path)
          , ("start", mkPosObject startL startC)
          , ("end",   mkPosObject endL endC)
          ]
  pure srcLocJson

-- TODO: precalc in a map
customCommandGetSourceLinks :: Adaptor ESTG ()
customCommandGetSourceLinks = do
  let progressId = "estgi-get-source-links"
  sendProgressStartEvent $ defaultProgressStartEvent
    { progressStartEventProgressId  = progressId
    , progressStartEventTitle       = "Running get source links..."
    }
  GetSourceLinksArguments {..} <- getArguments
  ESTG {..} <- getDebugSession
  sourceLinks <- case Bimap.lookup getSourceLinksArgumentsPath dapSourceNameMap of
    Just srcDesc@ExtStg{} -> do
      source <- getSourceFromSourceCodeDescriptor srcDesc
      let Just sourceRef = sourceSourceReference source
      (_sourceCodeText, locations, hsSrcLocs) <- getSourceFromFullPak sourceRef
      let hsTickishLocMap = Map.unionsWith mappend [Map.singleton stgPoint [tickish] | (stgPoint, tickish) <- hsSrcLocs]
          -- collect tickish locations
          estgLocMap = Map.unionsWith mappend
            [ Map.singleton stgPoint [range]
            | (SP_Tickish stgPoint, range) <- locations
            ]
      liftIO $ do
        print hsTickishLocMap
        print estgLocMap
      pure $
        [ SourceLink
          { sourceLinkSourceLine        = estgStartLine
          , sourceLinkSourceColumn      = estgStartCol
          , sourceLinkSourceEndLine     = estgEndLine
          , sourceLinkSourceEndColumn   = estgEndCol
          , sourceLinkTargetLine        = srcSpanSLine
          , sourceLinkTargetColumn      = srcSpanSCol
          , sourceLinkTargetEndLine     = srcSpanELine
          , sourceLinkTargetEndColumn   = srcSpanECol
          , sourceLinkTargetPath        = cs $ getSourceName hsCodeDesc
          }
        | (stgPoint, hsTickishList) <- Map.toList hsTickishLocMap
        , estgLocList <- maybeToList $ Map.lookup stgPoint estgLocMap
        , (((estgStartLine, estgStartCol),(estgEndLine, estgEndCol)), SourceNote{..}) <- zip estgLocList hsTickishList
        , let RealSrcSpan'{..} = sourceSpan
        , hsCodeDesc <- maybeToList $ Bimap.lookup srcSpanFile haskellSrcPathMap
        ]
    _ -> pure []
  sendSuccesfulResponse . setBody $ GetSourceLinksResponse
    { getSourceLinksResponseSourceLinks = sourceLinks
    }
  sendProgressEndEvent $ defaultProgressEndEvent
    { progressEndEventProgressId  = progressId
    , progressEndEventMessage     = Just "Get source links finished."
    }

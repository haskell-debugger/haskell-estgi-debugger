{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE LambdaCase #-}
module SourceCode where

import           Control.Monad.IO.Class                (liftIO)
import           Control.Monad
import           Data.String.Conversions               (cs)
import qualified Data.Set                              as Set
import           Data.Bimap                            ( Bimap )
import qualified Data.Bimap                            as Bimap
import           Data.Text                             ( Text )
import qualified Data.Text                             as T
import qualified Data.Text.Encoding                    as T
import qualified Data.Map.Strict                       as Map
import qualified Data.ByteString.Lazy.Char8            as BL8 ( pack, unpack, fromStrict, toStrict )
import           System.FilePath                       ( (-<.>), (</>), takeDirectory, takeFileName, takeExtension, dropExtension, splitFileName, splitPath, joinPath, splitDirectories)
import           Codec.Archive.Zip                     (withArchive, unEntrySelector, getEntries)
import           Data.Yaml                             hiding (Array)

import           Stg.Syntax                            hiding (sourceName, Scope)
import           Stg.IRLocation
import           Stg.Pretty
import           Stg.Program
import           Stg.IO
import           Stg.Tickish                           ( collectTickish )

import DAP
import DapBase


----------------------------------------------------------------------------

getSourcePath :: SourceCodeDescriptor -> FilePath
getSourcePath = \case
  Haskell  pkg mod -> "haskell" </> cs pkg </> cs mod </> "module.hs"
  GhcCore  pkg mod -> "haskell" </> cs pkg </> cs mod </> "module.ghccore"
  GhcStg   pkg mod -> "haskell" </> cs pkg </> cs mod </> "module.ghcstg"
  Cmm      pkg mod -> "haskell" </> cs pkg </> cs mod </> "module.cmm"
  Asm      pkg mod -> "haskell" </> cs pkg </> cs mod </> "module.s"
  ExtStg   pkg mod -> "haskell" </> cs pkg </> cs mod </> "module.stgbin"
  FFICStub pkg mod -> "haskell" </> cs pkg </> cs mod </> "module_stub.c"
  FFIHStub pkg mod -> "haskell" </> cs pkg </> cs mod </> "module_stub.h"
  ModInfo  pkg mod -> "haskell" </> cs pkg </> cs mod </> "module.info"
  ForeignC _pkg path -> cs path

getSourceName :: SourceCodeDescriptor -> String
getSourceName = \case
  Haskell  pkg mod -> "haskell" </> cs pkg </> cs mod <> ".hs"
  GhcCore  pkg mod -> "haskell" </> cs pkg </> cs mod <> ".ghccore"
  GhcStg   pkg mod -> "haskell" </> cs pkg </> cs mod <> ".ghcstg"
  Cmm      pkg mod -> "haskell" </> cs pkg </> cs mod <> ".cmm"
  Asm      pkg mod -> "haskell" </> cs pkg </> cs mod <> ".s"
  ExtStg   pkg mod -> "haskell" </> cs pkg </> cs mod <> ".stgbin.hs"
  FFICStub pkg mod -> "haskell" </> cs pkg </> cs mod <> "_stub.c"
  FFIHStub pkg mod -> "haskell" </> cs pkg </> cs mod <> "_stub.h"
  ModInfo  pkg mod -> "haskell" </> cs pkg </> cs mod <> ".info"
  ForeignC _pkg path -> cs path

----------------------------------------------------------------------------
-- | Retrieves list of modules from .fullpak file
getSourceCodeListFromFullPak :: FilePath -> IO ([SourceCodeDescriptor], Bimap UnitId PackageName, Bimap Name SourceCodeDescriptor)
getSourceCodeListFromFullPak fullPakPath = do
  rawEntries <- fmap unEntrySelector . Map.keys <$> withArchive fullPakPath getEntries
  let folderNames = Set.fromList (takeDirectory <$> rawEntries)
      appInfoName = "app.info"
  appInfoBytes <- readModpakL fullPakPath appInfoName id
  AppInfo{..} <- decodeThrow (BL8.toStrict appInfoBytes)
  let unitIdMap = Bimap.fromList
        [ (UnitId $ cs ciUnitId, cs ciPackageName)
        | CodeInfo{..} <- aiLiveCode
        ]
  {-
    program source content:
      haskell modules
      foreign files
  -}
  let rawEntriesSet = Set.fromList rawEntries
      moduleCodeItems pkg mod =
        [ Haskell   pkg mod
        , GhcCore   pkg mod
        , GhcStg    pkg mod
        , Cmm       pkg mod
        , Asm       pkg mod
        , ExtStg    pkg mod
        , FFICStub  pkg mod
        , FFIHStub  pkg mod
        , ModInfo   pkg mod
        ]
      haskellModuleCode :: [SourceCodeDescriptor]
      haskellModuleCode =
        [ srcDesc
        | CodeInfo{..} <- aiLiveCode
        , srcDesc <- moduleCodeItems (cs ciPackageName) (cs ciModuleName)
        , Set.member (getSourcePath srcDesc) rawEntriesSet
        ]

      cbitsSources :: [SourceCodeDescriptor]
      cbitsSources =
        [ ForeignC packageName path
        | path <- rawEntries
        , ("cbits-source" : unitIdString : _) <- [splitDirectories path]
        , Just packageName <- [Bimap.lookup (UnitId $ cs unitIdString) unitIdMap]
        ]

  hsPathList <- forM aiLiveCode $ \CodeInfo{..} -> do
    let extStgPath = getSourcePath $ ExtStg (cs ciPackageName) (cs ciModuleName)
    (_phase, _unitId, _modName, mSrcFilePath, _stubs, _hasForeignExport, _deps) <- readModpakL fullPakPath extStgPath decodeStgbinInfo
    case mSrcFilePath of
      Nothing -> pure []
      Just p  -> pure [(cs p, Haskell (cs ciPackageName) (cs ciModuleName))]
  let hsPathMap = Bimap.fromList $ concat hsPathList
  pure (haskellModuleCode ++ cbitsSources, unitIdMap, hsPathMap)

getValidSourceRefFromSource :: Source -> Adaptor ESTG (Maybe Int)
getValidSourceRefFromSource Source{..} = do
  ESTG {..} <- getDebugSession
  {-
    fallback chain:
      1. sourcePath
      2. sourceSourceReference
  -}
  let maybeSrcDesc = do
        srcName <- sourcePath
        Bimap.lookup srcName dapSourceNameMap
  case maybeSrcDesc of
    Just srcDesc  -> Just <$> getSourceRef srcDesc
    Nothing       -> case sourceSourceReference of
      Just srcRef
        | Bimap.memberR srcRef dapSourceRefMap
        -> pure sourceSourceReference
      _ -> pure Nothing

----------------------------------------------------------------------------
-- | Retrieves list of modules from .fullpak file
getSourceFromFullPak :: SourceId -> Adaptor ESTG (Text, [(StgPoint, SrcRange)], [(StgPoint, Tickish)])
getSourceFromFullPak sourceId = do
  ESTG {..} <- getDebugSession
  SourceRef_SourceFileInFullpak srcDesc <- case Bimap.lookupR sourceId dapSourceRefMap of
    Nothing     -> do
      sendError (ErrorMessage (T.pack $ "Unknown sourceId: " ++ show sourceId)) Nothing
    Just value  -> pure value
  let sourcePath = getSourcePath srcDesc
  liftIO $
    case srcDesc of
      ExtStg{} -> do
        m <- readModpakL fullPakPath sourcePath decodeStgbin
        let (stgCode, stgLocs)  = pShowWithConfig Config {cfgPrintTickish = True} $ pprModule m
            tickishList         = collectTickish m
        pure (stgCode, stgLocs, tickishList)
      _ -> do
        ir <- readModpakS fullPakPath sourcePath T.decodeUtf8
        pure (ir, [], [])

getSourceFromSourceRefDescriptor :: DapSourceRefDescriptor -> Adaptor ESTG Source
getSourceFromSourceRefDescriptor sourceRefDesc@(SourceRef_SourceFileInFullpak srcDesc) = do
  srcDescSet <- getsApp sourceCodeSet
  extraSources <- case srcDesc of
    Haskell packageName qualModName
      | cStub <- FFICStub packageName qualModName
      , hStub <- FFIHStub packageName qualModName
      -> Just <$> sequence (
      [ getSourceFromSourceRefDescriptor (SourceRef_SourceFileInFullpak $ ExtStg   packageName qualModName)
      , getSourceFromSourceRefDescriptor (SourceRef_SourceFileInFullpak $ GhcCore  packageName qualModName)
      , getSourceFromSourceRefDescriptor (SourceRef_SourceFileInFullpak $ GhcStg   packageName qualModName)
      , getSourceFromSourceRefDescriptor (SourceRef_SourceFileInFullpak $ Cmm      packageName qualModName)
      , getSourceFromSourceRefDescriptor (SourceRef_SourceFileInFullpak $ Asm      packageName qualModName)
      , getSourceFromSourceRefDescriptor (SourceRef_SourceFileInFullpak $ ModInfo  packageName qualModName)
      ] ++
      [ getSourceFromSourceRefDescriptor (SourceRef_SourceFileInFullpak cStub)
      | Set.member cStub srcDescSet
      ] ++
      [ getSourceFromSourceRefDescriptor (SourceRef_SourceFileInFullpak hStub)
      | Set.member hStub srcDescSet
      ])

    _ -> pure Nothing

  let sourceName = cs $ getSourceName srcDesc
  sourceRef <- getSourceRef sourceRefDesc
  ESTG {..} <- getDebugSession
  pure defaultSource
    { sourceName            = Just $ sourceName -- used in source tree children
    , sourceSourceReference = Just sourceRef
    , sourcePath            = Just $ sourceName -- used in code tab title
    , sourceSources         = extraSources
    }

getSourceRef :: DapSourceRefDescriptor -> Adaptor ESTG Int
getSourceRef key = do
  -- NOTE: Source code related db is populated at initialization
  getsApp (Bimap.lookup key . dapSourceRefMap) >>= \case
    Just srcRef -> pure srcRef
    Nothing     -> error $ "unknown source descriptor: " ++ show key

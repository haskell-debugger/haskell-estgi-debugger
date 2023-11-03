{-# LANGUAGE RecordWildCards #-}
module SharedFolder where

import Control.Monad.IO.Class
import System.Directory
import System.FilePath

import DAP
import DapBase

mapFilePathToHost :: FilePath -> Adaptor ESTG FilePath
mapFilePathToHost path = do
  ESTG {..} <- getDebugSession
  case sharedFolderMapping of
    Nothing -> pure path
    Just (hostAbsPath, containerAbsPath) -> do
      absPath <- liftIO $ makeAbsolute path
      pure $ hostAbsPath </> makeRelative containerAbsPath absPath

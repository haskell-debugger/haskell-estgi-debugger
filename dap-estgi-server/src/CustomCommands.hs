{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DeriveGeneric              #-}
module CustomCommands where

import           GHC.Generics                    ( Generic )

import Data.Text
import Data.Aeson
import DAP.Utils
import CustomDapTypes

data GetSourceLinksArguments
  = GetSourceLinksArguments
  { getSourceLinksArgumentsPath :: Text
  } deriving stock (Show, Eq, Generic)

instance FromJSON GetSourceLinksArguments where
  parseJSON = genericParseJSONWithModifier

------------

data GetSourceLinksResponse
  = GetSourceLinksResponse
  { getSourceLinksResponseSourceLinks :: [SourceLink]
  } deriving stock (Show, Eq, Generic)
----------------------------------------------------------------------------
instance ToJSON GetSourceLinksResponse where
  toJSON = genericToJSONWithModifier
----------------------------------------------------------------------------
data SourceLink
  = SourceLink
  { sourceLinkSourceLine :: Int
  , sourceLinkSourceColumn :: Int
  , sourceLinkSourceEndLine :: Int
  , sourceLinkSourceEndColumn :: Int
  , sourceLinkTargetLine :: Int
  , sourceLinkTargetColumn :: Int
  , sourceLinkTargetEndLine :: Int
  , sourceLinkTargetEndColumn :: Int
  , sourceLinkTargetPath :: Text
  } deriving stock (Show, Eq, Generic)
----------------------------------------------------------------------------
instance ToJSON SourceLink where
  toJSON = genericToJSONWithModifier

----------------------------------------------------------------------------
data ShowVariableGraphStructureArguments
  = ShowVariableGraphStructureArguments
  { showVariableGraphStructureArgumentsVariablesReference :: Int
  } deriving stock (Show, Eq, Generic)

instance FromJSON ShowVariableGraphStructureArguments where
  parseJSON = genericParseJSONWithModifier

----------------------------------------------------------------------------

data ShowRetainerGraphArguments
  = ShowRetainerGraphArguments
  { showRetainerGraphArgumentsVariablesReference :: Int
  } deriving stock (Show, Eq, Generic)

instance FromJSON ShowRetainerGraphArguments where
  parseJSON = genericParseJSONWithModifier

----------------------------------------------------------------------------

----------------------------------------------------------------------------
data SelectVariableGraphNodeArguments
  = SelectVariableGraphNodeArguments
  { selectVariableGraphNodeArgumentsVariablesReference :: Int
  } deriving stock (Show, Eq, Generic)

instance FromJSON SelectVariableGraphNodeArguments where
  parseJSON = genericParseJSONWithModifier

----------------------------------------------------------------------------
data RegionsResponse
  = RegionsResponse
  { regionsResponseRegions :: [Region]
  } deriving stock (Show, Eq, Generic)
----------------------------------------------------------------------------
instance ToJSON RegionsResponse where
  toJSON = genericToJSONWithModifier
----------------------------------------------------------------------------

data RegionInstancesArguments
  = RegionInstancesArguments
  { regionInstancesArgumentsRegionName :: Text
  } deriving stock (Show, Eq, Generic)

instance FromJSON RegionInstancesArguments where
  parseJSON = genericParseJSONWithModifier

----------------------------------------------------------------------------
data RegionInstancesResponse
  = RegionInstancesResponse
  { regionInstancesResponseRegionInstances :: [RegionInstance]
  } deriving stock (Show, Eq, Generic)
----------------------------------------------------------------------------
instance ToJSON RegionInstancesResponse where
  toJSON = genericToJSONWithModifier
----------------------------------------------------------------------------

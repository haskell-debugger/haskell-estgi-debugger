{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DeriveGeneric              #-}
module CustomCommands where

import           GHC.Generics                    ( Generic )

import Data.Text
import Data.Aeson
import DAP.Utils

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

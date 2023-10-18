{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DeriveGeneric              #-}
module GraphProtocol.Commands where

import GHC.Generics ( Generic )

import Data.Text
import Data.Aeson
import DAP.Utils


data LoadGraph
  = LoadGraph
  { loadGraphRequest        :: Text
  , loadGraphTitle          :: Text
  , loadGraphNodesFilepath  :: Maybe Text
  , loadGraphEdgesFilepath  :: Text
  } deriving stock (Show, Eq, Generic)
----------------------------------------------------------------------------
instance ToJSON LoadGraph where
  toJSON = genericToJSONWithModifier
----------------------------------------------------------------------------


data SelectNode
  = SelectNode
  { selectNodeRequest       :: Text
  , selectNodeNodeId        :: Text
  } deriving stock (Show, Eq, Generic)
----------------------------------------------------------------------------
instance ToJSON SelectNode where
  toJSON = genericToJSONWithModifier
----------------------------------------------------------------------------


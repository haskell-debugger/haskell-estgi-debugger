{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DeriveGeneric              #-}
module CustomDapTypes where

import           GHC.Generics                    ( Generic )

import Data.Text
import Data.Aeson
import DAP.Utils

----------------------------------------------------------------------------
data Region
  = Region
  { regionName          :: Text
  , regionInstanceCount :: Int
  } deriving stock (Show, Eq, Generic)
----------------------------------------------------------------------------
instance ToJSON Region where
  toJSON = genericToJSONWithModifier
----------------------------------------------------------------------------

data RegionInstance
  = RegionInstance
  { regionInstanceInstanceId          :: Int
  , regionInstanceObjectCount         :: Int
  , regionInstanceVariablesReference  :: Int
  } deriving stock (Show, Eq, Generic)
----------------------------------------------------------------------------
instance ToJSON RegionInstance where
  toJSON = genericToJSONWithModifier
----------------------------------------------------------------------------

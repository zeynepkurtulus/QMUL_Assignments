{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Types (StopPoint(..), StopPointResponse(..)) where

import GHC.Generics
import Data.Aeson
import Data.Aeson.Types (defaultOptions, fieldLabelModifier)

-- Represents a single stop point
data StopPoint = StopPoint
    { naptanId :: String
    , commonName :: String
    , modes :: [String]
    , lat :: Maybe Double
    , lon :: Maybe Double
    } deriving (Show, Generic)

-- | Represents a stop point in the transportation system.
-- The 'FromJSON' instance allows parsing from JSON.
-- The 'ToJSON' instance allows converting to JSON.
instance FromJSON StopPoint where
    parseJSON = genericParseJSON defaultOptions

instance ToJSON StopPoint

-- | Represents the API response that contains an array of stop points.
-- The 'stopPoints' field contains a list of 'StopPoint' objects.
-- The 'FromJSON' instance allows parsing from JSON.
-- The 'ToJSON' instance allows converting to JSON.
data StopPointResponse = StopPointResponse
    { stopPoints :: [StopPoint] -- ^ List of stop points.
    } deriving (Show, Generic)

instance FromJSON StopPointResponse where
    parseJSON = genericParseJSON defaultOptions
        { fieldLabelModifier = \key -> if key == "stopPoints" then "stopPoints" else key }

instance ToJSON StopPointResponse

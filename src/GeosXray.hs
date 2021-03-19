{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module GeosXray 
    (sourceUrl
    , getXrayEvents
    , ProductGoesXray (..)
    , XrayEvent (..)
    ) where

import Data.Time.Format
import Data.Time.Clock
import Network.Wreq
import Data.Text.Conversions
import Control.Lens
import Data.Aeson 
import Data.Text
import Data.Map as Map
import GHC.Generics
import Control.Monad
import Data.Map as Map

-- Default data source
sourceUrl :: String
sourceUrl = "https://services.swpc.noaa.gov/json/goes/primary/xrays-1-day.json"


-- Standard Solar flare classification, based on flux measured at GOES orbit.
data FlareClassification = A | B | C | D | M | X deriving (Show)

-- Converter from flux value to flare classification
toFlareClassiciation :: Double -> FlareClassification
toFlareClassiciation flux 
    |                  flux < 1.0E-7 = A 
    | flux > 1.0E-7 && flux < 1.0E-6 = B 
    | flux > 1.0E-6 && flux < 1.0E-5 = C
    | flux > 1.0E-5 && flux < 1.0E-4 = M
    | flux > 1.0E-4                  = X

-- Data source type
data ProductGoesXray = ProductGoesXray {
    time_tag :: Text,
    satellite :: Int,
    flux :: Double,
    energy :: Text
} deriving (Show, Generic)

-- Enable serialization from JSON
instance FromJSON ProductGoesXray

-- Event type
data XrayEvent = XrayEvent Double UTCTime FlareClassification deriving (Show)

-- Carrier type  analysis
data XrayEventSeq = XrayEventSeq {
    formerValue :: Double, 
    currentValue :: Double,
    xrayEvent :: [XrayEvent]
} deriving (Show)

-- Convert String-formatted time to UTCTime
getUtcTime :: String -> UTCTime
getUtcTime s = parseTimeOrError True defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" s :: UTCTime

{- 
A simple peak detection function for xray products.
Input    : List of Goes Xray products,
Output   : A list containing Xray peaks, with additional classification data.
--}
getXrayEvents :: [ProductGoesXray] -> [XrayEvent]
getXrayEvents x = xrayEvent $ Prelude.foldl chainXrayProduct (XrayEventSeq 0.0 0.0 []) x 

chainXrayProduct :: XrayEventSeq -> ProductGoesXray -> XrayEventSeq 
chainXrayProduct 
    (XrayEventSeq former current eventList) 
    (ProductGoesXray t _ flux _) = 
        addEventIfExists (XrayEventSeq current flux eventList) (isXrayEvent former current flux t)

isXrayEvent :: Double -> Double -> Double -> Text ->  Maybe XrayEvent
isXrayEvent former current flux t
    | current > former && flux < current && isEvent   = Just (XrayEvent current timestamp flareClass)
    | otherwise                                       = Nothing 
        where timestamp = (getUtcTime (convertText t :: String))
              flareClass = toFlareClassiciation current
              isEvent = filterFlareClassification flareClass

filterFlareClassification :: FlareClassification -> Bool
filterFlareClassification A = False
filterFlareClassification B = True
filterFlareClassification C = True    
filterFlareClassification M = True    
filterFlareClassification X = True    

addEventIfExists :: XrayEventSeq -> Maybe XrayEvent -> XrayEventSeq
addEventIfExists 
    (XrayEventSeq former current x4) 
    (Just (XrayEvent v s c)) = 
        XrayEventSeq former current (x4 ++ [XrayEvent v s c])
addEventIfExists 
    (XrayEventSeq former current x4) 
    Nothing = 
        XrayEventSeq former current x4 


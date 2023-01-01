{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

module DiscountListClient ( DiscountList, createDiscountList, getDiscount ) where

import Data.Aeson ( decode )
import Data.Time
import Network.HTTP.Client ( defaultManagerSettings, httpLbs, method, newManager, parseRequest, responseBody )

import Discount ( Discount )

newtype DiscountList = DiscountList {
  baseUrl :: String
}

class DiscountListClass discountList where
  getDiscount :: discountList -> Day -> String -> IO [Discount]

createDiscountList :: String -> DiscountList
createDiscountList baseURL = DiscountList { baseUrl = baseURL }

instance DiscountListClass DiscountList where
  getDiscount :: DiscountList -> Day -> String -> IO [Discount]
  getDiscount discountList date code = do
    zonedTime <- getZonedTime
    let currentLocalTime = zonedTimeToLocalTime zonedTime
    let localTime = LocalTime date $ localTimeOfDay currentLocalTime
    let utcTime = localTimeToUTC (zonedTimeZone zonedTime) localTime
    let seconds = formatTime defaultTimeLocale "%s" utcTime
    let milliseconds = show ((*) 1000 $ read seconds :: Int)
    manager <- newManager defaultManagerSettings
    initialRequest <- parseRequest $ baseUrl discountList ++ "/discount/" ++ milliseconds ++ "/" ++ code
    let request = initialRequest {method = "GET"}
    response <- httpLbs request manager
    let discount = decode $ responseBody response :: Maybe Discount
    case discount of
      Just foundDiscount -> return [foundDiscount]
      _ -> return []

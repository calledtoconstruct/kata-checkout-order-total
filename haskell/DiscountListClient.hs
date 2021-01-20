{-# LANGUAGE OverloadedStrings #-}

module DiscountListClient (DiscountList, createDiscountList, getDiscount) where

import Data.Aeson (decode)
import Data.Time
import Discount (Discount)
import Network.HTTP.Client (defaultManagerSettings, httpLbs, method, newManager, parseRequest, responseBody)

data DiscountList = DiscountList {
  baseUrl :: String
}

class DiscountListClass discountList where
  getDiscount :: discountList -> String -> Day -> IO [Discount]

createDiscountList :: String -> DiscountList
createDiscountList baseURL = DiscountList { baseUrl = baseURL }

instance DiscountListClass DiscountList where
  getDiscount discountList code date = do
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

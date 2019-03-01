
{-# LANGUAGE OverloadedStrings #-}

module DiscountListClient ( DiscountList, createDiscountList, getDiscount ) where

  import Network.HTTP.Client        (newManager, defaultManagerSettings, parseRequest, httpLbs, method, responseBody)
  import Data.Aeson                 (decode)
  import Data.Time.Format
  import Data.Time
  import Discount

  data DiscountList                     = DiscountList

  class DiscountListClass discountList where
    getDiscount :: discountList -> String -> Day -> IO [Discount]

  createDiscountList :: DiscountList
  createDiscountList                    = DiscountList

  instance DiscountListClass DiscountList where
    getDiscount discountList code date  = do
      zonedTime                           <- getZonedTime
      let currentLocalTime                = zonedTimeToLocalTime zonedTime
      let localTime                       = LocalTime date $ localTimeOfDay currentLocalTime
      let utcTime                         = localTimeToUTC (zonedTimeZone zonedTime) localTime
      let seconds                         = formatTime defaultTimeLocale "%s" utcTime
      let milliseconds                    = show $ (*) 1000 $ read seconds
      manager                             <- newManager defaultManagerSettings
      initialRequest                      <- parseRequest $ "http://localhost:8081/discount/" ++ milliseconds ++ "/" ++ code
      let request                         = initialRequest { method = "GET" }
      response                            <- httpLbs request manager
      let discount                        = decode $ responseBody response :: Maybe Discount
      case discount of
        Just discount                       -> return [discount]
        otherwise                           -> return []

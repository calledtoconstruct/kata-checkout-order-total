
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module DiscountAPI where

  import Data.Functor.Identity
  import Data.Time.Format
  import Data.Time
  import Control.Monad.Trans
  import Network.HTTP.Types.Status
  import Network.Wai.Middleware.Cors
  import Web.Scotty
  import Discount
  import DiscountList
  import ItemListClient

  parseDate :: String -> Day
  parseDate dateString = localDay localTime
    where localTime = zonedTimeToLocalTime zonedTime
          zonedTime = runIdentity maybeDate
          maybeDate = parseTimeM True defaultTimeLocale "%s" seconds
          seconds = show $ flip div 1000 $ read dateString

  main :: IO ()
  main = do
    putStrLn "Starting Server..."
    let itemList = createItemList
    discountList <- loadDiscounts (getItem itemList) "./Discounts.json" $ pure createDiscountList
    timeZone <- getCurrentTimeZone
    scotty 8081 $ do
      middleware simpleCors
      get "/discount/:date/:code" $ do
        code <- param "code"
        dateString <- param "date"
        discounts <- liftIO $ getDiscount discountList code $ parseDate dateString
        case discounts of
          []          -> status status404 *> text "Not Found"
          [discount]  -> json discount

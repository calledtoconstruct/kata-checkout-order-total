
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

  import Data.Time
  import Data.Time.LocalTime
  import Data.Time.Format ( defaultTimeLocale, parseTimeM )
  import Control.Monad.Trans ( MonadIO(liftIO) )
  import Network.HTTP.Types.Status ( status404 )
  import Network.Wai.Middleware.Cors ( simpleCors )
  import Web.Scotty ( get, json, middleware, param, scotty, status, text )
  import DiscountList ( getDiscount, createDiscountList, loadDiscounts )
  import ItemListClient ( createItemList, getItem )
  import Data.Maybe ( fromMaybe )

  parseDate :: String -> ZonedTime -> Day
  parseDate dateString defaultTime = localDay localTime
    where localTime = zonedTimeToLocalTime zonedTime
          zonedTime = fromMaybe defaultTime maybeDate
          maybeDate = parseTimeM True defaultTimeLocale "%s" seconds
          seconds = show $ flip div 1000 $ read dateString

  main :: IO ()
  main = do
    putStrLn "Starting Server..."
    let itemList = createItemList
    discountList <- loadDiscounts (getItem itemList) "./Discounts.json" $ pure createDiscountList
    timeZone <- getCurrentTimeZone
    defaultTime <- getZonedTime
    scotty 8081 $ do
      middleware simpleCors
      get "/discount/:date/:code" $ do
        code <- param "code"
        dateString <- param "date"
        discounts <- liftIO $ getDiscount discountList code $ parseDate dateString defaultTime
        case discounts of
          []          -> status status404 *> text "Not Found"
          [discount]  -> json discount

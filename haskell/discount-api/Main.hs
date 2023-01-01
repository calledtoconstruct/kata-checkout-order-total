{-# LANGUAGE OverloadedStrings #-}

module Main where

  import Control.Monad.Trans ( MonadIO(liftIO) )
  import Data.Maybe ( fromMaybe )
  import Data.Time
  import Network.HTTP.Types.Status (status500,  status404 )
  import Network.Wai.Middleware.Cors ( simpleCors )
  import Web.Scotty ( get, json, middleware, param, scotty, status, text )

  import DiscountList ( getDiscount, createDiscountList, loadDiscounts )
  import ItemListClient ( createItemList, getItem )

  parseDate :: String -> TimeZone -> ZonedTime -> Day
  parseDate dateString timeZone defaultTime = localDay localTime
    where localTime = zonedTimeToLocalTime zonedTime
          zonedTime = fromMaybe defaultTime maybeDate
          maybeDate = parseTimeM True defaultTimeLocale "%s" seconds
          seconds = show (flip div 1000 $ read dateString :: Int)

  main :: IO ()
  main = do
    putStrLn "Starting Server..."
    let itemList = createItemList "http://item-api:8082"
    discountList <- loadDiscounts (getItem itemList) "./Discounts.json" $ pure $ createDiscountList ""
    timeZone <- getCurrentTimeZone
    defaultTime <- getZonedTime
    scotty 8081 $ do
      middleware simpleCors
      get "/discount/:date/:code" $ do
        code <- param "code"
        dateString <- param "date"
        discounts <- liftIO $ getDiscount discountList (parseDate dateString timeZone defaultTime)  code
        case discounts of
          (_:_:_)     -> status status500 *> text "Internal Server Error"
          []          -> status status404 *> text "Not Found"
          [discount]  -> json discount


{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module DiscountAPI where

  import Control.Monad.Trans
  import Network.HTTP.Types.Status
  import Network.Wai.Middleware.Cors
  import Web.Scotty
  import Discount
  import DiscountList
  import ItemList

  main :: IO ()
  main = do
    putStrLn "Starting Server..."
    itemList <- loadItems "./Items.json" $ pure createItemList
    discountList <- loadDiscounts (getItem itemList) "./Discounts.json" $ pure createDiscountList
    scotty 8081 $ do
      middleware simpleCors
      get "/discount/:code" $ do
        code <- param "code"
        discounts <- liftIO $ getDiscount discountList code
        case discounts of
          []          -> status status404 *> text "Not Found"
          [discount]  -> json discount

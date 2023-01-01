{-# LANGUAGE OverloadedStrings #-}

module TestLimitedUpSalePercentDiscount where

  import Control.Monad.Trans ( MonadIO(liftIO) )
  import Data.Aeson
  import Data.ByteString.Lazy.UTF8 ( ByteString, fromString )
  import Data.Time ( Day, fromGregorian )
  import Data.Time.Calendar ( fromGregorian )

  import Discount
  import Item

  import TestHarness ( calculateTotal )

  itemName :: String
  itemName = "turkey"

  itemTurkey :: Item
  itemTurkey = ByQuantityItem {
    itemCode = itemName,
    itemDescription = "15 lbs whole turkey.",
    itemType = "by quantity",
    itemPrice = 20.00
  }

  discountTurkey :: Discount
  discountTurkey = LimitedUpSalePercentDiscount {
    discountCode = itemName,
    discountStartDate = fromGregorian 2019 1 1,
    discountEndDate = fromGregorian 2019 12 31,
    discountBulk = 2,
    discountSale = 1,
    discountPercent = 1.0,
    discountLimit = 6
  }

  -- | scanItem for turkey with limited up sale percent discount
  -- >>> limitedUpSalePercentDiscount [itemTurkey] [discountTurkey] 2019 4 30 itemName 1
  -- 20.0
  -- >>> limitedUpSalePercentDiscount [itemTurkey] [discountTurkey] 2019 4 30 itemName 2
  -- 40.0
  -- >>> limitedUpSalePercentDiscount [itemTurkey] [discountTurkey] 2019 4 30 itemName 3
  -- 40.0
  -- >>> limitedUpSalePercentDiscount [itemTurkey] [discountTurkey] 2019 4 30 itemName 4
  -- 60.0
  -- >>> limitedUpSalePercentDiscount [itemTurkey] [discountTurkey] 2019 4 30 itemName 9
  -- 140.0

  -- | scanItem for turkey with no discount
  -- >>> limitedUpSalePercentDiscount [itemTurkey] [] 2019 4 30 itemName 3
  -- 60.0
  -- >>> limitedUpSalePercentDiscount [itemTurkey] [] 2019 4 30 itemName 9
  -- 180.0

  -- | scanItem for turkey with limited up sale percent discount with a date in the future
  -- >>> limitedUpSalePercentDiscount [itemTurkey] [discountTurkey] 2018 4 30 itemName 1
  -- 20.0
  -- >>> limitedUpSalePercentDiscount [itemTurkey] [discountTurkey] 2018 4 30 itemName 2
  -- 40.0
  -- >>> limitedUpSalePercentDiscount [itemTurkey] [discountTurkey] 2018 4 30 itemName 3
  -- 60.0
  limitedUpSalePercentDiscount :: [Item] -> [Discount] -> Integer -> Int -> Int -> String -> Int -> IO Double
  limitedUpSalePercentDiscount = calculateTotal

  -- | Serialization of Limited Up Sale Percent Discount
  -- >>> serialization
  -- "{\"discountBulk\":2,\"discountCode\":\"turkey\",\"discountEndDate\":\"2019-12-31\",\"discountLimit\":6,\"discountPercent\":1,\"discountSale\":1,\"discountStartDate\":\"2019-01-01\",\"tag\":\"LimitedUpSalePercentDiscount\"}"
  serialization :: ByteString
  serialization = encode discountTurkey

  discountTurkeyJSON :: String
  discountTurkeyJSON = "{\"tag\":\"LimitedUpSalePercentDiscount\",\"discountBulk\":2,\"discountLimit\":6,\"discountPercent\":1,\"discountEndDate\":\"2019-12-31\",\"discountSale\":1,\"discountStartDate\":\"2019-01-01\",\"discountCode\":\"turkey\"}"

  -- | Deserialization of Limited Up Sale Percent Discount
  -- >>> deserialization
  -- "LimitedUpSalePercentDiscount {discountCode = \"turkey\", discountStartDate = 2019-01-01, discountEndDate = 2019-12-31, discountBulk = 2, discountSale = 1, discountPercent = 1.0, discountLimit = 6}"
  deserialization :: String
  deserialization = case (decode $ fromString discountTurkeyJSON) :: Maybe Discount of
    Just discount -> show discount
    Nothing -> "Invalid input"

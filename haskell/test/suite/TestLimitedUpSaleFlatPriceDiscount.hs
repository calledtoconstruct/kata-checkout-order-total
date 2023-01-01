{-# LANGUAGE OverloadedStrings #-}

module TestLimitedUpSaleFlatPriceDiscount where

  import Control.Monad.Trans ( MonadIO(liftIO) )
  import Data.Aeson
  import Data.ByteString.Lazy.UTF8 ( ByteString, fromString )
  import Data.Time ( Day, fromGregorian )
  import Data.Time.Calendar ( fromGregorian )

  import Discount
  import Item

  import TestHarness ( calculateTotal )

  itemName :: String
  itemName = "bologna"

  itemBologna :: Item
  itemBologna = ByQuantityItem {
    itemCode = itemName,
    itemDescription = "32 oz of premium bologna.",
    itemType = "by quantity",
    itemPrice = 3.25
  }

  discountBologna :: Discount
  discountBologna = LimitedUpSaleFlatPriceDiscount {
    discountCode = itemName,
    discountStartDate = fromGregorian 2019 1 1,
    discountEndDate = fromGregorian 2019 12 31,
    discountBulk = 2,
    discountSale = 1,
    discountPrice = 1.5,
    discountLimit = 6
  }

  -- | scanItem for bologna with limited up sale flat price discount
  -- >>> limitedUpSaleFlatPriceDiscount [itemBologna] [discountBologna] 2019 4 30 itemName 1
  -- 3.25
  -- >>> limitedUpSaleFlatPriceDiscount [itemBologna] [discountBologna] 2019 4 30 itemName 2
  -- 6.5
  -- >>> limitedUpSaleFlatPriceDiscount [itemBologna] [discountBologna] 2019 4 30 itemName 3
  -- 8.0
  -- >>> limitedUpSaleFlatPriceDiscount [itemBologna] [discountBologna] 2019 4 30 itemName 4
  -- 11.25
  -- >>> limitedUpSaleFlatPriceDiscount [itemBologna] [discountBologna] 2019 4 30 itemName 9
  -- 25.75

  -- | scanItem for bologna with no discount
  -- >>> limitedUpSaleFlatPriceDiscount [itemBologna] [] 2019 4 30 itemName 3
  -- 9.75
  -- >>> limitedUpSaleFlatPriceDiscount [itemBologna] [] 2019 4 30 itemName 4
  -- 13.0
  -- >>> limitedUpSaleFlatPriceDiscount [itemBologna] [] 2019 4 30 itemName 9
  -- 29.25

  -- | scanItem for bologna with a limited up sale flat price discount with a date in the future
  -- >>> limitedUpSaleFlatPriceDiscount [itemBologna] [discountBologna] 2018 4 30 itemName 1
  -- 3.25
  -- >>> limitedUpSaleFlatPriceDiscount [itemBologna] [discountBologna] 2018 4 30 itemName 2
  -- 6.5
  -- >>> limitedUpSaleFlatPriceDiscount [itemBologna] [discountBologna] 2018 4 30 itemName 3
  -- 9.75
  limitedUpSaleFlatPriceDiscount :: [Item] -> [Discount] -> Integer -> Int -> Int -> String -> Int -> IO Double
  limitedUpSaleFlatPriceDiscount = calculateTotal

  -- | Serialization of Limited Up Sale Flat Price Discount
  -- >>> serialization
  -- "{\"discountBulk\":2,\"discountCode\":\"bologna\",\"discountEndDate\":\"2019-12-31\",\"discountLimit\":6,\"discountPrice\":1.5,\"discountSale\":1,\"discountStartDate\":\"2019-01-01\",\"tag\":\"LimitedUpSaleFlatPriceDiscount\"}"
  serialization :: ByteString
  serialization = encode discountBologna

  discountBolognaJSON :: String
  discountBolognaJSON = "{\"tag\":\"LimitedUpSaleFlatPriceDiscount\",\"discountBulk\":2,\"discountPrice\":1.5,\"discountLimit\":6,\"discountEndDate\":\"2019-12-31\",\"discountSale\":1,\"discountStartDate\":\"2019-01-01\",\"discountCode\":\"bologna\"}"

  -- | Deserialization of Limited Up Sale Flat Price Discount
  -- >>> deserialization
  -- "LimitedUpSaleFlatPriceDiscount {discountCode = \"bologna\", discountStartDate = 2019-01-01, discountEndDate = 2019-12-31, discountBulk = 2, discountSale = 1, discountPrice = 1.5, discountLimit = 6}"
  deserialization :: String
  deserialization = case (decode $ fromString discountBolognaJSON) :: Maybe Discount of
    Just discount -> show discount
    Nothing -> "Invalid input"

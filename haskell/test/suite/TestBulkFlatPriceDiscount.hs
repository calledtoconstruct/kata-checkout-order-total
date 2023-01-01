{-# LANGUAGE OverloadedStrings #-}

module TestBulkFlatPriceDiscount where

  import Control.Monad.Trans ( MonadIO(liftIO) )
  import Data.Aeson
  import Data.ByteString.Lazy.UTF8 (ByteString, fromString)
  import Data.Time ( Day, fromGregorian )
  import Data.Time.Calendar ( fromGregorian )

  import Discount
  import Item

  import TestHarness ( calculateTotal )

  itemName :: String
  itemName = "crisps"

  itemCrisps :: Item
  itemCrisps = ByQuantityItem {
    itemCode        = itemName,
    itemDescription = "12 oz bag of salted crisps.",
    itemType        = "by quantity",
    itemPrice       = 0.55
  }

  discountCrisps :: Discount
  discountCrisps = BulkFlatPriceDiscount {
    discountCode      = itemName,
    discountStartDate = fromGregorian 2019 1 1,
    discountEndDate   = fromGregorian 2019 12 31,
    discountBulk      = 3,
    discountPrice     = 1.00
  }

  -- | scanItem for crisps with bulk flat price discount
  -- >>> bulkFlatPriceDiscount [itemCrisps] [discountCrisps] 2019 4 30 itemName 1
  -- 0.55
  -- >>> bulkFlatPriceDiscount [itemCrisps] [discountCrisps] 2019 4 30 itemName 2
  -- 1.1
  -- >>> bulkFlatPriceDiscount [itemCrisps] [discountCrisps] 2019 4 30 itemName 3
  -- 1.0
  -- >>> bulkFlatPriceDiscount [itemCrisps] [discountCrisps] 2019 4 30 itemName 4
  -- 1.55
  -- >>> bulkFlatPriceDiscount [itemCrisps] [discountCrisps] 2019 4 30 itemName 5
  -- 2.1
  -- >>> bulkFlatPriceDiscount [itemCrisps] [discountCrisps] 2019 4 30 itemName 6
  -- 2.0

  -- | scanItem for crisps with no discount
  -- >>> bulkFlatPriceDiscount [itemCrisps] [] 2019 4 30 itemName 3
  -- 1.65
  -- >>> bulkFlatPriceDiscount [itemCrisps] [] 2019 4 30 itemName 6
  -- 3.3

  -- | scanItem for crisps with bulk flat price discount with a date in the future
  -- >>> bulkFlatPriceDiscount [itemCrisps] [discountCrisps] 2018 4 30 itemName 1
  -- 0.55
  -- >>> bulkFlatPriceDiscount [itemCrisps] [discountCrisps] 2018 4 30 itemName 2
  -- 1.1
  -- >>> bulkFlatPriceDiscount [itemCrisps] [discountCrisps] 2018 4 30 itemName 3
  -- 1.65
  bulkFlatPriceDiscount :: [Item] -> [Discount] -> Integer -> Int -> Int -> String -> Int -> IO Double
  bulkFlatPriceDiscount = calculateTotal

  -- | Serialization of Bulk Flat Price Discount
  -- >>> serialization
  -- "{\"discountBulk\":3,\"discountCode\":\"crisps\",\"discountEndDate\":\"2019-12-31\",\"discountPrice\":1,\"discountStartDate\":\"2019-01-01\",\"tag\":\"BulkFlatPriceDiscount\"}"
  serialization :: ByteString
  serialization = encode discountCrisps

  discountCrispsJSON :: String
  discountCrispsJSON = "{\"tag\":\"BulkFlatPriceDiscount\",\"discountBulk\":3,\"discountPrice\":1,\"discountEndDate\":\"2019-12-31\",\"discountStartDate\":\"2019-01-01\",\"discountCode\":\"crisps\"}"

  -- | Deserialization of Bulk Flat Price Discount
  -- >>> deserialization
  -- "BulkFlatPriceDiscount {discountCode = \"crisps\", discountStartDate = 2019-01-01, discountEndDate = 2019-12-31, discountBulk = 3, discountPrice = 1.0}"
  deserialization :: String
  deserialization = case (decode $ fromString discountCrispsJSON) :: Maybe Discount of
    Just discount -> show discount
    Nothing -> "Invalid input"

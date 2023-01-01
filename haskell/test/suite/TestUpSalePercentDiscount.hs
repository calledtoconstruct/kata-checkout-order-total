{-# LANGUAGE OverloadedStrings #-}

module TestUpSalePercentDiscount where

  import Control.Monad.Trans ( MonadIO(liftIO) )
  import Data.Aeson ( encode, decode )
  import Data.ByteString.Lazy.UTF8 ( ByteString, fromString )
  import Data.Time ( Day, fromGregorian )
  import Data.Time.Calendar ( fromGregorian )

  import Discount
  import Item

  import TestHarness ( calculateTotal )

  itemName :: String
  itemName = "cat food"

  itemCatFood :: Item
  itemCatFood = ByQuantityItem {
    itemCode = itemName,
    itemDescription = "12 oz can of cat food.",
    itemType = "by quantity",
    itemPrice = 1.25
  }

  discountCatFood :: Discount
  discountCatFood = UpSalePercentDiscount {
    discountCode = itemName,
    discountStartDate = fromGregorian 2019 1 1,
    discountEndDate = fromGregorian 2019 12 31,
    discountBulk = 2,
    discountSale = 1,
    discountPercent = 0.50
  }

  -- | scanItem for cat food with up sale percent discount
  -- >>> upSalePercentDiscount [itemCatFood] [discountCatFood] 2019 4 30 itemName 1
  -- 1.25
  -- >>> upSalePercentDiscount [itemCatFood] [discountCatFood] 2019 4 30 itemName 2
  -- 2.5
  -- >>> upSalePercentDiscount [itemCatFood] [discountCatFood] 2019 4 30 itemName 3
  -- 3.12
  -- >>> upSalePercentDiscount [itemCatFood] [discountCatFood] 2019 4 30 itemName 4
  -- 4.37
  -- >>> upSalePercentDiscount [itemCatFood] [discountCatFood] 2019 4 30 itemName 9
  -- 9.37

  -- | scanItem for cat food with no discount
  -- >>> upSalePercentDiscount [itemCatFood] [] 2019 4 30 itemName 3
  -- 3.75
  -- >>> upSalePercentDiscount [itemCatFood] [] 2019 4 30 itemName 9
  -- 11.25

  -- | scanItem for cat food with an up sale percent discount with a date in the future
  -- >>> upSalePercentDiscount [itemCatFood] [discountCatFood] 2018 4 30 itemName 1
  -- 1.25
  -- >>> upSalePercentDiscount [itemCatFood] [discountCatFood] 2018 4 30 itemName 2
  -- 2.5
  -- >>> upSalePercentDiscount [itemCatFood] [discountCatFood] 2018 4 30 itemName 3
  -- 3.75
  upSalePercentDiscount :: [Item] -> [Discount] -> Integer -> Int -> Int -> String -> Int -> IO Double
  upSalePercentDiscount = calculateTotal

  -- | Serialization of Up Sale Percent Discount
  -- >>> serialization
  -- "{\"discountBulk\":2,\"discountCode\":\"cat food\",\"discountEndDate\":\"2019-12-31\",\"discountPercent\":0.5,\"discountSale\":1,\"discountStartDate\":\"2019-01-01\",\"tag\":\"UpSalePercentDiscount\"}"
  serialization :: ByteString
  serialization = encode discountCatFood

  discountCatFoodJSON :: String
  discountCatFoodJSON = "{\"tag\":\"UpSalePercentDiscount\",\"discountBulk\":2,\"discountPercent\":0.5,\"discountEndDate\":\"2019-12-31\",\"discountSale\":1,\"discountStartDate\":\"2019-01-01\",\"discountCode\":\"cat food\"}"

  -- | Deserialization of Up Sale Percent Discount
  -- >>> deserialization
  -- "UpSalePercentDiscount {discountCode = \"cat food\", discountStartDate = 2019-01-01, discountEndDate = 2019-12-31, discountBulk = 2, discountSale = 1, discountPercent = 0.5}"
  deserialization :: String
  deserialization = case (decode $ fromString discountCatFoodJSON) :: Maybe Discount of
    Just discount -> show discount
    Nothing -> "Invalid input"

{-# LANGUAGE OverloadedStrings #-}

module TestStandardDiscount where

  import Control.Monad.Trans ( MonadIO(liftIO) )
  import Data.Aeson
  import Data.ByteString.Lazy.UTF8 ( ByteString, fromString )
  import Data.Time ( Day, fromGregorian )
  import Data.Time.Calendar ( fromGregorian )

  import Discount
  import Item

  import TestHarness ( calculateTotal )

  itemName :: String
  itemName = "dog food"

  itemDogFood :: Item
  itemDogFood = ByQuantityItem {
    itemCode = itemName,
    itemDescription = "2 lbs bag of dry dog food.",
    itemType = "by quantity",
    itemPrice = 9.99
  }

  discountDogFood :: Discount
  discountDogFood = StandardDiscount {
    discountCode = itemName,
    discountStartDate = fromGregorian 2019 1 1,
    discountEndDate = fromGregorian 2019 12 31,
    discountPrice = 3.00
  }

  -- | scanItem for dog food with standard discount
  -- >>> standardDiscount [itemDogFood] [discountDogFood] 2019 4 30 itemName 1
  -- 3.0
  -- >>> standardDiscount [itemDogFood] [discountDogFood] 2019 4 30 itemName 3
  -- 9.0

  -- | scanItem for dog food with no discount
  -- >>> standardDiscount [itemDogFood] [] 2019 4 30 itemName 1
  -- 9.99
  -- >>> standardDiscount [itemDogFood] [] 2019 4 30 itemName 3
  -- 29.97

  -- | scanItem for dog food with standard discount that is in the future
  -- >>> standardDiscount [itemDogFood] [discountDogFood] 2018 4 30 itemName 1
  -- 9.99
  -- >>> standardDiscount [itemDogFood] [discountDogFood] 2018 4 30 itemName 3
  -- 29.97
  standardDiscount :: [Item] -> [Discount] -> Integer -> Int -> Int -> String -> Int -> IO Double
  standardDiscount = calculateTotal

  -- | Serialization of Standard Discount
  -- >>> serialization
  -- "{\"discountCode\":\"dog food\",\"discountEndDate\":\"2019-12-31\",\"discountPrice\":3,\"discountStartDate\":\"2019-01-01\",\"tag\":\"StandardDiscount\"}"
  serialization :: ByteString
  serialization = encode discountDogFood

  discountDogFoodJSON :: String
  discountDogFoodJSON = "{\"tag\":\"StandardDiscount\",\"discountPrice\":3,\"discountEndDate\":\"2019-12-31\",\"discountStartDate\":\"2019-01-01\",\"discountCode\":\"dog food\"}"

  -- | Deserialization of Standard Discount
  -- >>> deserialization
  -- "StandardDiscount {discountCode = \"dog food\", discountStartDate = 2019-01-01, discountEndDate = 2019-12-31, discountPrice = 3.0}"
  deserialization :: String
  deserialization = case (decode $ fromString discountDogFoodJSON) :: Maybe Discount of
      Just discount -> show discount
      Nothing -> "Invalid input"

{-# LANGUAGE OverloadedStrings #-}

module TestUpSaleFlatPriceDiscount where

  import Control.Monad.Trans ( MonadIO(liftIO) )
  import Data.Aeson
  import Data.ByteString.Lazy.UTF8 ( ByteString, fromString )
  import Data.Time ( Day, fromGregorian )
  import Data.Time.Calendar ( fromGregorian )

  import Discount
  import Item

  import TestHarness ( calculateTotal )

  itemName :: String
  itemName = "swiss cheese"

  itemSwissCheese :: Item
  itemSwissCheese = ByQuantityItem {
    itemCode = itemName,
    itemDescription = "12 oz package of swiss chese.",
    itemType = "by quantity",
    itemPrice = 2.1
  }

  discountSwissCheese :: Discount
  discountSwissCheese = UpSaleFlatPriceDiscount {
    discountCode = itemName,
    discountStartDate = fromGregorian 2019 1 1,
    discountEndDate = fromGregorian 2019 12 31,
    discountBulk = 2,
    discountSale = 1,
    discountPrice = 1.0
  }

  -- | scanItem for swiss cheese with up sale flat price discount
  -- >>> upSaleFlatPriceDiscount [itemSwissCheese] [discountSwissCheese] 2019 4 30 itemName 1
  -- 2.1
  -- >>> upSaleFlatPriceDiscount [itemSwissCheese] [discountSwissCheese] 2019 4 30 itemName 2
  -- 4.2
  -- >>> upSaleFlatPriceDiscount [itemSwissCheese] [discountSwissCheese] 2019 4 30 itemName 3
  -- 5.2
  -- >>> upSaleFlatPriceDiscount [itemSwissCheese] [discountSwissCheese] 2019 4 30 itemName 4
  -- 7.3
  -- >>> upSaleFlatPriceDiscount [itemSwissCheese] [discountSwissCheese] 2019 4 30 itemName 9
  -- 15.6

  -- | scanItem for swiss cheese with no discount
  -- >>> upSaleFlatPriceDiscount [itemSwissCheese] [] 2019 4 30 itemName 3
  -- 6.3
  -- >>> upSaleFlatPriceDiscount [itemSwissCheese] [] 2019 4 30 itemName 9
  -- 18.9

  -- | scanItem for swiss cheese with an up sale flat price discount with a date in the future
  -- >>> upSaleFlatPriceDiscount [itemSwissCheese] [discountSwissCheese] 2018 4 30 itemName 1
  -- 2.1
  -- >>> upSaleFlatPriceDiscount [itemSwissCheese] [discountSwissCheese] 2018 4 30 itemName 2
  -- 4.2
  -- >>> upSaleFlatPriceDiscount [itemSwissCheese] [discountSwissCheese] 2018 4 30 itemName 3
  -- 6.3
  upSaleFlatPriceDiscount :: [Item] -> [Discount] -> Integer -> Int -> Int -> String -> Int -> IO Double
  upSaleFlatPriceDiscount = calculateTotal

  -- | Serialization of Up Sale Flat Price Discount
  -- >>> serialization
  -- "{\"discountBulk\":2,\"discountCode\":\"swiss cheese\",\"discountEndDate\":\"2019-12-31\",\"discountPrice\":1,\"discountSale\":1,\"discountStartDate\":\"2019-01-01\",\"tag\":\"UpSaleFlatPriceDiscount\"}"
  serialization :: ByteString
  serialization = encode discountSwissCheese

  discountSwissCheeseJSON :: String
  discountSwissCheeseJSON = "{\"tag\":\"UpSaleFlatPriceDiscount\",\"discountBulk\":2,\"discountPrice\":1,\"discountEndDate\":\"2019-12-31\",\"discountSale\":1,\"discountStartDate\":\"2019-01-01\",\"discountCode\":\"swiss cheese\"}"

  -- | Deserialization of Up Sale Flat Price Discount
  -- >>> deserialization
  -- "UpSaleFlatPriceDiscount {discountCode = \"swiss cheese\", discountStartDate = 2019-01-01, discountEndDate = 2019-12-31, discountBulk = 2, discountSale = 1, discountPrice = 1.0}"
  deserialization :: String
  deserialization = case (decode $ fromString discountSwissCheeseJSON) :: Maybe Discount of
    Just discount -> show discount
    Nothing -> "Invalid input"


{-# LANGUAGE OverloadedStrings #-}

module TestUpSaleFlatPriceDiscount where

  import Data.Aeson (encode, decode)
  import Data.ByteString.Lazy.UTF8 (fromString)
  import Data.Time
  import Discount
  import Item
  import Transaction
  import ItemList
  import DiscountList

  itemSwissCheese :: Item
  itemSwissCheese = ByQuantityItem {
    itemCode = "swiss cheese",
    itemDescription = "12 oz package of swiss chese.",
    itemType = "by quantity",
    itemPrice = 2.1
  }

  discountSwissCheese :: Discount
  discountSwissCheese = UpSaleFlatPriceDiscount {
    discountCode = "swiss cheese",
    discountStartDate = fromGregorian 2019 1 1,
    discountEndDate = fromGregorian 2019 12 31,
    discountBulk = 2,
    discountSale = 1,
    discountPrice = 1.0
  }

  noDiscount :: String -> [Discount]
  noDiscount _ = []

  -- $setup
  -- >>> itemList                   = addItems createItemList $ itemSwissCheese: []
  -- >>> discountList               = addDiscounts createDiscountList (getItem itemList) $ discountSwissCheese: []
  -- >>> item                       = head (getItem itemList "swiss cheese")
  -- >>> currentTransaction         = createTransaction $ fromGregorian 2019 4 30
  -- >>> oldTransaction             = createTransaction $ fromGregorian 2018 4 30
  -- >>> discountSwissCheeseJSON    = "{\"tag\":\"UpSaleFlatPriceDiscount\",\"discountBulk\":2,\"discountPrice\":1,\"discountEndDate\":\"2019-12-31\",\"discountSale\":1,\"discountStartDate\":\"2019-01-01\",\"discountCode\":\"swiss cheese\"}"

  -- | scanItem for swiss cheese with and without up sale flat price discount
  -- >>> transactionTotal $ scanItem currentTransaction (getDiscount discountList) item
  -- 2.1
  -- >>> transactionTotal $ (!!2) $ iterate (\t -> scanItem t (getDiscount discountList) item) currentTransaction
  -- 4.2
  -- >>> transactionTotal $ (!!3) $ iterate (\t -> scanItem t (getDiscount discountList) item) currentTransaction
  -- 5.2
  -- >>> transactionTotal $ (!!4) $ iterate (\t -> scanItem t (getDiscount discountList) item) currentTransaction
  -- 7.3
  -- >>> transactionTotal $ (!!9) $ iterate (\t -> scanItem t (getDiscount discountList) item) currentTransaction
  -- 15.6
  -- >>> transactionTotal $ (!!3) $ iterate (\t -> scanItem t noDiscount item) currentTransaction
  -- 6.3

  -- | scanItem for swiss cheese with an up sale flat price discount with a date in the future
  -- >>> transactionTotal $ scanItem oldTransaction (getDiscount discountList) item
  -- 2.1
  -- >>> transactionTotal $ (!!2) $ iterate (\t -> scanItem t (getDiscount discountList) item) oldTransaction
  -- 4.2
  -- >>> transactionTotal $ (!!3) $ iterate (\t -> scanItem t (getDiscount discountList) item) oldTransaction
  -- 6.3

  -- | Serialization of Up Sale Flat Price Discount
  -- >>> encode discountSwissCheese
  -- "{\"tag\":\"UpSaleFlatPriceDiscount\",\"discountBulk\":2,\"discountPrice\":1,\"discountEndDate\":\"2019-12-31\",\"discountSale\":1,\"discountStartDate\":\"2019-01-01\",\"discountCode\":\"swiss cheese\"}"

  -- | Deserialization of Up Sale Flat Price Discount
  -- >>> case (decode $ fromString discountSwissCheeseJSON) :: Maybe Discount of Just discount -> discount; Nothing -> error "Invalid input"
  -- UpSaleFlatPriceDiscount {discountCode = "swiss cheese", discountStartDate = 2019-01-01, discountEndDate = 2019-12-31, discountBulk = 2, discountSale = 1, discountPrice = 1.0}


{-# LANGUAGE OverloadedStrings #-}

module TestLimitedUpSaleFlatPriceDiscount where

  import Data.Aeson (encode, decode)
  import Data.ByteString.Lazy.UTF8 (fromString)
  import Data.Time
  import Discount
  import Item
  import Transaction
  import ItemList
  import DiscountList

  itemBologna :: Item
  itemBologna = ByQuantityItem {
    itemCode = "bologna",
    itemDescription = "32 oz of premium bologna.",
    itemType = "by quantity",
    itemPrice = 3.25
  }

  discountBologna :: Discount
  discountBologna = LimitedUpSaleFlatPriceDiscount {
    discountCode = "bologna",
    discountStartDate = fromGregorian 2019 1 1,
    discountEndDate = fromGregorian 2019 12 31,
    discountBulk = 2,
    discountSale = 1,
    discountPrice = 1.5,
    discountLimit = 6
  }

  noDiscount :: String -> [Discount]
  noDiscount _ = []

  -- $setup
  -- >>> itemList                   = addItems createItemList $ itemBologna: []
  -- >>> discountList               = addDiscounts createDiscountList (getItem itemList) $ discountBologna: []
  -- >>> item                       = head (getItem itemList "bologna")
  -- >>> currentTransaction         = createTransaction $ fromGregorian 2019 4 30
  -- >>> oldTransaction             = createTransaction $ fromGregorian 2018 4 30
  -- >>> discountBolognaJSON        = "{\"tag\":\"LimitedUpSaleFlatPriceDiscount\",\"discountBulk\":2,\"discountPrice\":1.5,\"discountLimit\":6,\"discountEndDate\":\"2019-12-31\",\"discountSale\":1,\"discountStartDate\":\"2019-01-01\",\"discountCode\":\"bologna\"}"

  -- | scanItem for bologna with and without limited up sale flat price discount
  -- >>> transactionTotal $ scanItem currentTransaction (getDiscount discountList) item
  -- 3.25
  -- >>> transactionTotal $ (!!2) $ iterate (\t -> scanItem t (getDiscount discountList) item) currentTransaction
  -- 6.5
  -- >>> transactionTotal $ (!!3) $ iterate (\t -> scanItem t (getDiscount discountList) item) currentTransaction
  -- 8.0
  -- >>> transactionTotal $ (!!4) $ iterate (\t -> scanItem t (getDiscount discountList) item) currentTransaction
  -- 11.25
  -- >>> transactionTotal $ (!!9) $ iterate (\t -> scanItem t (getDiscount discountList) item) currentTransaction
  -- 25.75
  -- >>> transactionTotal $ (!!3) $ iterate (\t -> scanItem t noDiscount item) currentTransaction
  -- 9.75

  -- | scanItem for bologna with a limited up sale flat price discount with a date in the future
  -- >>> transactionTotal $ scanItem oldTransaction (getDiscount discountList) item
  -- 3.25
  -- >>> transactionTotal $ (!!2) $ iterate (\t -> scanItem t (getDiscount discountList) item) oldTransaction
  -- 6.5
  -- >>> transactionTotal $ (!!3) $ iterate (\t -> scanItem t (getDiscount discountList) item) oldTransaction
  -- 9.75

  -- | Serialization of Limited Up Sale Flat Price Discount
  -- >>> encode discountBologna
  -- "{\"tag\":\"LimitedUpSaleFlatPriceDiscount\",\"discountBulk\":2,\"discountPrice\":1.5,\"discountLimit\":6,\"discountEndDate\":\"2019-12-31\",\"discountSale\":1,\"discountStartDate\":\"2019-01-01\",\"discountCode\":\"bologna\"}"

  -- | Deserialization of Limited Up Sale Flat Price Discount
  -- >>> case (decode $ fromString discountBolognaJSON) :: Maybe Discount of Just discount -> discount; Nothing -> error "Invalid input"
  -- LimitedUpSaleFlatPriceDiscount {discountCode = "bologna", discountStartDate = 2019-01-01, discountEndDate = 2019-12-31, discountBulk = 2, discountSale = 1, discountPrice = 1.5, discountLimit = 6}

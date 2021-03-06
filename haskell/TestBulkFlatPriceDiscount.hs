
{-# LANGUAGE OverloadedStrings #-}

module TestBulkFlatPriceDiscount where

  import Data.Aeson (encode, decode)
  import Data.ByteString.Lazy.UTF8 (fromString)
  import Data.Time
  import Discount
  import Item
  import Transaction
  import ItemList
  import DiscountList

  itemCrisps :: Item
  itemCrisps = ByQuantityItem {
    itemCode = "crisps",
    itemDescription = "12 oz bag of salted crisps.",
    itemType = "by quantity",
    itemPrice = 0.55
  }

  discountCrisps :: Discount
  discountCrisps = BulkFlatPriceDiscount {
    discountCode = "crisps",
    discountStartDate = fromGregorian 2019 1 1,
    discountEndDate = fromGregorian 2019 12 31,
    discountBulk = 3,
    discountPrice = 1.00
  }

  noDiscount :: String -> [Discount]
  noDiscount _ = []

  -- $setup
  -- >>> itemList                   = addItems createItemList $ itemCrisps: []
  -- >>> discountList               = addDiscounts createDiscountList (getItem itemList) $ discountCrisps: []
  -- >>> item                       = head $ getItem itemList "crisps"
  -- >>> currentTransaction         = createTransaction $ fromGregorian 2019 4 30
  -- >>> oldTransaction             = createTransaction $ fromGregorian 2018 4 30
  -- >>> discountCrispsJSON         = "{\"tag\":\"BulkFlatPriceDiscount\",\"discountBulk\":3,\"discountPrice\":1,\"discountEndDate\":\"2019-12-31\",\"discountStartDate\":\"2019-01-01\",\"discountCode\":\"crisps\"}"

  -- | scanItem for crisps with and without bulk flat price discount
  -- >>> transactionTotal $ scanItem currentTransaction (getDiscount discountList) item
  -- 0.55
  -- >>> transactionTotal $ (!!2) $ iterate (\t -> scanItem t (getDiscount discountList) item) currentTransaction
  -- 1.1
  -- >>> transactionTotal $ (!!3) $ iterate (\t -> scanItem t (getDiscount discountList) item) currentTransaction
  -- 1.0
  -- >>> transactionTotal $ (!!4) $ iterate (\t -> scanItem t (getDiscount discountList) item) currentTransaction
  -- 1.55
  -- >>> transactionTotal $ (!!6) $ iterate (\t -> scanItem t (getDiscount discountList) item) currentTransaction
  -- 2.0
  -- >>> transactionTotal $ (!!6) $ iterate (\t -> scanItem t noDiscount item) currentTransaction
  -- 3.3

  -- | scanItem for crisps with bulk flat price discount in with a date in the future
  -- >>> transactionTotal $ scanItem oldTransaction (getDiscount discountList) item
  -- 0.55
  -- >>> transactionTotal $ (!!2) $ iterate (\t -> scanItem t (getDiscount discountList) item) oldTransaction
  -- 1.1
  -- >>> transactionTotal $ (!!3) $ iterate (\t -> scanItem t (getDiscount discountList) item) oldTransaction
  -- 1.65

  -- | Serialization of Bulk Flat Price Discount
  -- >>> encode discountCrisps
  -- "{\"tag\":\"BulkFlatPriceDiscount\",\"discountBulk\":3,\"discountPrice\":1,\"discountEndDate\":\"2019-12-31\",\"discountStartDate\":\"2019-01-01\",\"discountCode\":\"crisps\"}"

  -- | Deserialization of Bulk Flat Price Discount
  -- >>> case (decode $ fromString discountCrispsJSON) :: Maybe Discount of Just discount -> discount; Nothing -> error "Invalid input"
  -- BulkFlatPriceDiscount {discountCode = "crisps", discountStartDate = 2019-01-01, discountEndDate = 2019-12-31, discountBulk = 3, discountPrice = 1.0}


{-# LANGUAGE OverloadedStrings #-}

module TestStandardDiscount where

  import Data.Aeson (encode, decode)
  import Data.ByteString.Lazy.UTF8 (fromString)
  import Data.Time
  import Discount
  import Item
  import Transaction
  import ItemList
  import DiscountList

  itemDogFood :: Item
  itemDogFood = ByQuantityItem {
    itemCode = "dog food",
    itemDescription = "2 lbs bag of dry dog food.",
    itemType = "by quantity",
    itemPrice = 9.99
  }

  discountDogFood :: Discount
  discountDogFood = StandardDiscount {
    discountCode = "dog food",
    discountStartDate = fromGregorian 2019 1 1,
    discountEndDate = fromGregorian 2019 12 31,
    discountPrice = 3.00
  }

  noDiscount :: String -> [Discount]
  noDiscount _ = []

  -- $setup
  -- >>> itemList                   = addItems createItemList $ itemDogFood: []
  -- >>> discountList               = addDiscounts createDiscountList (getItem itemList) $ discountDogFood: []
  -- >>> item                       = head $ getItem itemList "dog food"
  -- >>> currentTransaction         = createTransaction $ fromGregorian 2019 4 30
  -- >>> oldTransaction             = createTransaction $ fromGregorian 2018 4 30
  -- >>> discountDogFoodJSON        = "{\"tag\":\"StandardDiscount\",\"discountPrice\":3,\"discountEndDate\":\"2019-12-31\",\"discountStartDate\":\"2019-01-01\",\"discountCode\":\"dog food\"}"

  -- | scanItem for dog food with and without standard discount
  -- >>> transactionTotal $ (!!1) $ iterate (\t -> scanItem t (getDiscount discountList) item) currentTransaction
  -- 3.0
  -- >>> transactionTotal $ (!!3) $ iterate (\t -> scanItem t (getDiscount discountList) item) currentTransaction
  -- 9.0
  -- >>> transactionTotal $ (!!3) $ iterate (\t -> scanItem t noDiscount item) currentTransaction
  -- 29.97

  -- | scanItem for dog food with standard discount that is in the future
  -- >>> transactionTotal $ (!!1) $ iterate (\t -> scanItem t (getDiscount discountList) item) oldTransaction
  -- 9.99
  -- >>> transactionTotal $ (!!3) $ iterate (\t -> scanItem t (getDiscount discountList) item) oldTransaction
  -- 29.97

  -- | Serialization of Standard Discount
  -- >>> encode discountDogFood
  -- "{\"tag\":\"StandardDiscount\",\"discountPrice\":3,\"discountEndDate\":\"2019-12-31\",\"discountStartDate\":\"2019-01-01\",\"discountCode\":\"dog food\"}"

  -- | Deserialization of Standard Discount
  -- >>> case (decode $ fromString discountDogFoodJSON) :: Maybe Discount of Just discount -> discount; Nothing -> error "Invalid input"
  -- StandardDiscount {discountCode = "dog food", discountStartDate = 2019-01-01, discountEndDate = 2019-12-31, discountPrice = 3.0}

module TestStandardDiscount where

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

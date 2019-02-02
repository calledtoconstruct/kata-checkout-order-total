module Main where

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

  main :: IO ()
  main = print $ transactionTotal transaction
    where transaction     = createTransaction $ fromGregorian 2019 4 30
          itemList        = addItems createItemList $ itemDogFood: []
          discountList    = addDiscounts createDiscountList (getItem itemList) $ discountDogFood: []

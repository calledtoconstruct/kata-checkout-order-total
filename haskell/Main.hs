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

  itemCrisps :: Item
  itemCrisps = ByQuantityItem {
    itemCode = "crisps",
    itemDescription = "12 oz bag of salted crisps.",
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

  itemCatFood :: Item
  itemCatFood = ByQuantityItem {
    itemCode = "cat food",
    itemDescription = "12 oz can of cat food.",
    itemPrice = 1.25
  }

  discountCatFood :: Discount
  discountCatFood = UpSalePercentDiscount {
    discountCode = "cat food",
    discountStartDate = fromGregorian 2019 1 1,
    discountEndDate = fromGregorian 2019 12 31,
    discountBulk = 2,
    discountSale = 1,
    discountPercent = 0.50
  }

  main :: IO ()
  main = print $ transactionTotal transaction
    where transaction     = createTransaction $ fromGregorian 2019 4 30
          itemList        = addItems createItemList $ itemDogFood: itemCrisps: itemCatFood: []
          discountList    = addDiscounts createDiscountList (getItem itemList) $ discountDogFood: discountCrisps: discountCatFood: []

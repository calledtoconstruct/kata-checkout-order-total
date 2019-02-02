module TestUpSalePercentDiscount where

  import Data.Time
  import Discount
  import Item
  import Transaction
  import ItemList
  import DiscountList

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

  noDiscount :: String -> [Discount]
  noDiscount _ = []

  -- $setup
  -- >>> itemList                   = addItems createItemList $ itemCatFood: []
  -- >>> discountList               = addDiscounts createDiscountList (getItem itemList) $ discountCatFood: []
  -- >>> item                       = head (getItem itemList "cat food")
  -- >>> currentTransaction         = createTransaction $ fromGregorian 2019 4 30
  -- >>> oldTransaction             = createTransaction $ fromGregorian 2018 4 30

  -- | scanItem for cat food with and without up sale percent discount
  -- >>> transactionTotal $ scanItem currentTransaction (getDiscount discountList) item
  -- 1.25
  -- >>> transactionTotal $ (!!2) $ iterate (\t -> scanItem t (getDiscount discountList) item) currentTransaction
  -- 2.5
  -- >>> transactionTotal $ (!!3) $ iterate (\t -> scanItem t (getDiscount discountList) item) currentTransaction
  -- 3.12
  -- >>> transactionTotal $ (!!4) $ iterate (\t -> scanItem t (getDiscount discountList) item) currentTransaction
  -- 4.37
  -- >>> transactionTotal $ (!!9) $ iterate (\t -> scanItem t (getDiscount discountList) item) currentTransaction
  -- 9.37
  -- >>> transactionTotal $ (!!3) $ iterate (\t -> scanItem t noDiscount item) currentTransaction
  -- 3.75

  -- | scanItem for cat food with an up sale percent discount with a date in the future
  -- >>> transactionTotal $ scanItem oldTransaction (getDiscount discountList) item
  -- 1.25
  -- >>> transactionTotal $ (!!2) $ iterate (\t -> scanItem t (getDiscount discountList) item) oldTransaction
  -- 2.5
  -- >>> transactionTotal $ (!!3) $ iterate (\t -> scanItem t (getDiscount discountList) item) oldTransaction
  -- 3.75

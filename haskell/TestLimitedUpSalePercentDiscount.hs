module TestLimitedUpSalePercentDiscount where

  import Data.Time
  import Discount
  import Item
  import Transaction
  import ItemList
  import DiscountList

  noDiscount :: String -> [Discount]
  noDiscount _ = []

  itemTurkey :: Item
  itemTurkey = ByQuantityItem {
    itemCode = "turkey",
    itemDescription = "15 lbs whole turkey.",
    itemType = "by quantity",
    itemPrice = 20.00
  }

  discountTurkey :: Discount
  discountTurkey = LimitedUpSalePercentDiscount {
    discountCode = "turkey",
    discountStartDate = fromGregorian 2019 1 1,
    discountEndDate = fromGregorian 2019 12 31,
    discountBulk = 2,
    discountSale = 1,
    discountPercent = 1.0,
    discountLimit = 6
  }

  -- $setup
  -- >>> itemList                   = addItems createItemList $ itemTurkey: []
  -- >>> discountList               = addDiscounts createDiscountList (getItem itemList) $ discountTurkey: []
  -- >>> item                       = head (getItem itemList "turkey")
  -- >>> currentTransaction         = createTransaction $ fromGregorian 2019 4 30
  -- >>> oldTransaction             = createTransaction $ fromGregorian 2018 4 30

  -- | scanItem for turkey with and without limited up sale percent discount
  -- >>> transactionTotal $ scanItem currentTransaction (getDiscount discountList) item
  -- 20.0
  -- >>> transactionTotal $ (!!2) $ iterate (\t -> scanItem t (getDiscount discountList) item) currentTransaction
  -- 40.0
  -- >>> transactionTotal $ (!!3) $ iterate (\t -> scanItem t (getDiscount discountList) item) currentTransaction
  -- 40.0
  -- >>> transactionTotal $ (!!4) $ iterate (\t -> scanItem t (getDiscount discountList) item) currentTransaction
  -- 60.0
  -- >>> transactionTotal $ (!!9) $ iterate (\t -> scanItem t (getDiscount discountList) item) currentTransaction
  -- 140.0
  -- >>> transactionTotal $ (!!3) $ iterate (\t -> scanItem t noDiscount item) currentTransaction
  -- 60.0

  -- | scanItem for turkey with limited up sale percent discount with a date in the future
  -- >>> transactionTotal $ scanItem oldTransaction (getDiscount discountList) item
  -- 20.0
  -- >>> transactionTotal $ (!!2) $ iterate (\t -> scanItem t (getDiscount discountList) item) oldTransaction
  -- 40.0
  -- >>> transactionTotal $ (!!3) $ iterate (\t -> scanItem t (getDiscount discountList) item) oldTransaction
  -- 60.0

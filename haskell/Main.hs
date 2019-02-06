module Main where

  import Data.Time
  import Discount
  import Item
  import Transaction
  import ItemList
  import DiscountList

  discountDogFood :: Discount
  discountDogFood = StandardDiscount {
    discountCode = "dog food",
    discountStartDate = fromGregorian 2019 1 1,
    discountEndDate = fromGregorian 2019 12 31,
    discountPrice = 3.00
  }

  discountCrisps :: Discount
  discountCrisps = BulkFlatPriceDiscount {
    discountCode = "crisps",
    discountStartDate = fromGregorian 2019 1 1,
    discountEndDate = fromGregorian 2019 12 31,
    discountBulk = 3,
    discountPrice = 1.00
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

  discountSwissCheese :: Discount
  discountSwissCheese = UpSaleFlatPriceDiscount {
    discountCode = "swiss cheese",
    discountStartDate = fromGregorian 2019 1 1,
    discountEndDate = fromGregorian 2019 12 31,
    discountBulk = 2,
    discountSale = 1,
    discountPrice = 1.0
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

  add :: DiscountList -> ItemList -> TransactionType -> [Item] -> IO Double
  add discountList itemList transaction []      = do
    putStrLn "Item not found!"
    prompt discountList itemList transaction
  add discountList itemList transaction [item]  = do
    let nextTransaction = scanItem transaction (getDiscount discountList) item
    putStrLn $ show $ transactionTotal nextTransaction
    prompt discountList itemList nextTransaction

  prompt :: DiscountList -> ItemList -> TransactionType -> IO Double
  prompt discountList itemList transaction = do
    nextCode <- putStr "Enter a product code: " *> getLine
    waitForScan discountList itemList transaction nextCode

  scan :: DiscountList -> ItemList -> TransactionType -> String -> IO Double
  scan discountList itemList transaction code = add discountList itemList transaction (getItem itemList code)

  waitForScan :: DiscountList -> ItemList -> TransactionType -> String -> IO Double
  waitForScan discountList itemList transaction code = case code of
    "done"              -> return $ transactionTotal transaction
    otherwise           -> scan discountList itemList transaction code

  console :: ItemList -> IO ()
  console itemList
    | invalidDiscount   = print "Invalid Discount"
    | invalidItem       = print "Invalid Item"
    | otherwise         = do
      total               <- prompt discountList itemList transaction
      putStr "Total: " *> print total
    where invalidDiscount = any (==False) $ flip isValidDiscount (getItem itemList) <$> (transactionDiscounts transaction)
          invalidItem     = any (==False) $ isValidItem <$> transactionItems transaction
          transaction     = createTransaction $ fromGregorian 2019 4 30
          discountList    = addDiscounts createDiscountList (getItem itemList) $ discountDogFood: discountCrisps: discountCatFood: discountTurkey: discountSwissCheese: discountBologna: []

  main :: IO ()
  main = do
    itemList            <- load "./Items.json" $ pure createItemList
    console itemList
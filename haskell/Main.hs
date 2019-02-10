module Main where

  import Data.Time
  import Discount
  import Item
  import Transaction
  import ItemList
  import DiscountList

  add :: DiscountList -> ItemList -> TransactionType -> [Item] -> IO Double
  add discountList itemList transaction []            = do
    putStrLn "Item not found!"
    prompt discountList itemList transaction
  add discountList itemList transaction [item]        = do
    let nextTransaction = scanItem transaction (getDiscount discountList) item
    putStrLn $ show $ transactionTotal nextTransaction
    prompt discountList itemList nextTransaction

  prompt :: DiscountList -> ItemList -> TransactionType -> IO Double
  prompt discountList itemList transaction            = do
    nextCode <- putStr "Enter a product code: " *> getLine
    waitForScan discountList itemList transaction nextCode

  scan :: DiscountList -> ItemList -> TransactionType -> String -> IO Double
  scan discountList itemList transaction code         = add discountList itemList transaction (getItem itemList code)

  waitForScan :: DiscountList -> ItemList -> TransactionType -> String -> IO Double
  waitForScan discountList itemList transaction code  = case code of
    "done"              -> return $ transactionTotal transaction
    otherwise           -> scan discountList itemList transaction code

  console :: DiscountList -> ItemList -> IO ()
  console discountList itemList
    | invalidDiscount   = print "Invalid Discount"
    | invalidItem       = print "Invalid Item"
    | otherwise         = do
      total               <- prompt discountList itemList transaction
      putStr "Total: " *> print total
    where invalidDiscount = any (==False) $ flip isValidDiscount (getItem itemList) <$> (transactionDiscounts transaction)
          invalidItem     = any (==False) $ isValidItem <$> transactionItems transaction
          transaction     = createTransaction $ fromGregorian 2019 4 30

  main :: IO ()
  main = do
    itemList            <- loadItems "./Items.json" $ pure createItemList
    discountList        <- loadDiscounts (getItem itemList) "./Discounts.json" $ pure createDiscountList
    console discountList itemList
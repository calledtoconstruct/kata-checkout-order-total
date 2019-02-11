module Main where

  import Data.Time
  import Discount
  import Item
  import Transaction
  import ItemListClient
  import DiscountListClient

  add :: DiscountList -> ItemList -> TransactionType -> [Item] -> IO Double
  add discountList itemList transaction []            = do
    putStrLn "Item not found!"
    prompt discountList itemList transaction
  add discountList itemList transaction [item]        = do
    nextTransaction <- scanItem transaction (getDiscount discountList) item
    putStrLn $ show $ transactionTotal nextTransaction
    prompt discountList itemList nextTransaction

  prompt :: DiscountList -> ItemList -> TransactionType -> IO Double
  prompt discountList itemList transaction            = do
    nextCode <- putStr "Enter a product code: " *> getLine
    waitForScan discountList itemList transaction nextCode

  scan :: DiscountList -> ItemList -> TransactionType -> String -> IO Double
  scan discountList itemList transaction code         = do
    item <- getItem itemList code
    add discountList itemList transaction item

  waitForScan :: DiscountList -> ItemList -> TransactionType -> String -> IO Double
  waitForScan discountList itemList transaction code  = case code of
    "done"              -> return $ transactionTotal transaction
    otherwise           -> scan discountList itemList transaction code

  console :: DiscountList -> ItemList -> IO ()
  console discountList itemList
    | otherwise         = do
      total               <- prompt discountList itemList transaction
      putStr "Total: " *> print total
    where transaction     = createTransaction $ fromGregorian 2019 4 30

  main :: IO ()
  main = do
    let itemList        = createItemList
    let discountList    = createDiscountList
    console discountList itemList

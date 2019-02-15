module Main where

  import Control.Monad.Trans
  import Data.Time
  import Discount
  import Item
  import Transaction
  import ItemListClient
  import DiscountListClient

  addItem :: DiscountList -> ItemList -> TransactionType -> Item -> IO Double
  addItem discountList itemList transaction item      = do
    nextTransaction <- scanItem transaction (getDiscount discountList) item
    putStrLn $ show $ transactionTotal nextTransaction
    prompt discountList itemList nextTransaction

  add :: DiscountList -> ItemList -> TransactionType -> [Item] -> IO Double
  add discountList itemList transaction []            = do
    putStrLn "Item not found!"
    prompt discountList itemList transaction
  add discountList itemList transaction [item]        = case item of
    ByWeightItem{}      -> case (itemWeight item) of
      Nothing             -> do
        weightString <- putStr "Enter weight: " *> getLine
        let weight        = read weightString :: Double
        let weightedItem  = ByWeightItem {
          itemCode          = itemCode item,
          itemDescription   = itemDescription item,
          itemPrice         = itemPrice item,
          itemType          = "by weight",
          itemWeight        = Just weight
        }
        addItem discountList itemList transaction weightedItem
      Just weight         -> addItem discountList itemList transaction item
    ByQuantityItem{}    -> addItem discountList itemList transaction item

  scan :: DiscountList -> ItemList -> TransactionType -> String -> IO Double
  scan discountList itemList transaction code         = do
    item <- getItem itemList code
    add discountList itemList transaction item

  waitForScan :: DiscountList -> ItemList -> TransactionType -> String -> IO Double
  waitForScan discountList itemList transaction code  = case code of
    "done"              -> return $ transactionTotal transaction
    otherwise           -> scan discountList itemList transaction code

  prompt :: DiscountList -> ItemList -> TransactionType -> IO Double
  prompt discountList itemList transaction            = do
    nextCode <- putStr "Enter a product code: " *> getLine
    waitForScan discountList itemList transaction nextCode

  console :: DiscountList -> ItemList -> IO ()
  console discountList itemList                       = do
      total <- prompt discountList itemList transaction
      putStr "Total: " *> print total
    where transaction   = createTransaction $ fromGregorian 2019 4 30

  main :: IO ()
  main                                                = do
    let itemList        = createItemList
    let discountList    = createDiscountList
    console discountList itemList

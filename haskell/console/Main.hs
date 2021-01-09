module Main where

  import Data.Time
  import Item ( Item(ByQuantityItem, ByWeightItem, itemWeight) )
  import Transaction ( createTransaction, transactionTotal, scanItem, TransactionType )
  import ItemListClient ( createItemList, ItemList, getItem )
  import DiscountListClient ( createDiscountList, DiscountList, getDiscount )

  addItem :: DiscountList -> ItemList -> TransactionType -> Item -> IO Double
  addItem discountList itemList transaction item      = do
    nextTransaction <- scanItem transaction (getDiscount discountList) item
    print $ transactionTotal nextTransaction
    prompt discountList itemList nextTransaction

  add :: DiscountList -> ItemList -> TransactionType -> [Item] -> IO Double
  add discountList itemList transaction []            = do
    putStrLn "Item not found!"
    prompt discountList itemList transaction
  add discountList itemList transaction [item]        = case item of
    ByWeightItem{}      -> case itemWeight item of
      Nothing             -> do
        weightString <- putStr "Enter weight: " *> getLine
        let weight        = read weightString :: Double
        let weightedItem  = item {
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
    _                   -> scan discountList itemList transaction code

  prompt :: DiscountList -> ItemList -> TransactionType -> IO Double
  prompt discountList itemList transaction            = do
    nextCode <- putStr "Enter a product code: " *> getLine
    waitForScan discountList itemList transaction nextCode

  console :: DiscountList -> ItemList -> Day -> IO ()
  console discountList itemList date                  = do
      total <- prompt discountList itemList transaction
      putStr "Total: " *> print total
    where transaction   = createTransaction date

  main :: IO ()
  main                                                = do
    zonedTime           <- getZonedTime
    let itemList        = createItemList
    let discountList    = createDiscountList
    console discountList itemList $ localDay $ zonedTimeToLocalTime zonedTime

{-# LANGUAGE BangPatterns #-}

module Main where

  import Data.Time
  import Item ( Item(ByQuantityItem, ByWeightItem, itemWeight) )
  import Transaction ( createTransaction, transactionTotal, scanItem, TransactionType )
  import ItemListClient ( createItemList, ItemList, getItem )
  import DiscountListClient ( createDiscountList, DiscountList, getDiscount )
  import Options.Applicative
  import System.IO ( hFlush, stdout )
  
  addItem :: DiscountList -> ItemList -> TransactionType -> Item -> IO Double
  addItem discountList itemList transaction item      = do
    !nextTransaction <- scanItem transaction (getDiscount discountList) item
    print $ transactionTotal nextTransaction
    prompt discountList itemList nextTransaction

  add :: DiscountList -> ItemList -> TransactionType -> [Item] -> IO Double
  add _ _ _ (_:_:_) = undefined
  add discountList itemList transaction []            = do
    putStrLn "Item not found!"
    prompt discountList itemList transaction
  add discountList itemList transaction [item]        = case item of
    ByWeightItem{}      -> case itemWeight item of
      Nothing             -> do
        weightString <- putStr "Enter weight: " *> hFlush stdout *> getLine
        let weight        = read weightString :: Double
        let weightedItem  = item {
          itemWeight        = Just weight
        }
        addItem discountList itemList transaction weightedItem
      Just _              -> addItem discountList itemList transaction item
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
    nextCode <- putStr "Enter a product code: " *> hFlush stdout *> getLine
    waitForScan discountList itemList transaction nextCode

  console :: DiscountList -> ItemList -> Day -> IO ()
  console discountList itemList date                  = do
      total <- prompt discountList itemList transaction
      putStr "Total: "
      print total
    where transaction   = createTransaction date

  data Options = Options {
    itemAPI :: String,
    discountAPI :: String
  }

  args :: Parser Options
  args = Options
    <$> strOption (
      long "item-api"
      <> short 'i'
      <> help "URL for the item-api"
      <> showDefault
      <> value "http://localhost:8082"
    )
    <*> strOption (
      long "discount-api"
      <> short 'd'
      <> help "URL for the discount-api"
      <> showDefault
      <> value "http://localhost:8081"
    )

  main :: IO ()
  main = start =<< execParser opts
    where opts = info (args <**> helper) (fullDesc <> progDesc "When prompted, enter one or more product codes to add items to the order.  Enter 'done' when finished." <> header "Console 'cash register' for the checkout-order-total kata.")

  start :: Options -> IO ()
  start options = do
    zonedTime           <- getZonedTime
    let itemList        = createItemList $ itemAPI options
    let discountList    = createDiscountList $ discountAPI options
    console discountList itemList $ localDay $ zonedTimeToLocalTime zonedTime

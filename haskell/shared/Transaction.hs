{-# LANGUAGE InstanceSigs #-}

module Transaction ( TransactionType (transactionDiscounts, transactionItems, transactionDate), createTransaction, transactionTotal, scanItem, scanDiscountedItem ) where

  import Data.Time ( Day )

  import Discount ( Discount(discountCode), isApplicable, getTotal )
  import Item ( Item(itemCode), itemTotal )

  data TransactionType = StandardTransaction {
    transactionDiscounts  :: [Discount],
    transactionItems      :: [Item],
    transactionDate       :: Day
  }

  createTransaction :: Day -> TransactionType
  createTransaction date = StandardTransaction {
    transactionDiscounts  = [],
    transactionItems      = [],
    transactionDate       = date
  }

  isItemCoveredByDiscount :: [Discount] -> Item -> Bool
  isItemCoveredByDiscount discounts item = all (==False) $ flip isApplicable item <$> discounts

  class Transaction transaction where
    discountTotal         :: transaction -> Double
    nonDiscountTotal      :: transaction -> Double
    transactionTotal      :: transaction -> Double
    scanItem              :: transaction -> (Day -> String -> IO [Discount]) -> Item -> IO TransactionType
    scanDiscountedItem    :: transaction -> Discount -> Item -> TransactionType

  sameCode :: Item -> Discount -> Bool
  sameCode item discount = discountCode discount == itemCode item

  instance Transaction TransactionType where

    discountTotal :: TransactionType -> Double
    discountTotal transaction                 = sum $ getTotal (transactionDate transaction) (transactionItems transaction) <$> transactionDiscounts transaction

    nonDiscountTotal :: TransactionType -> Double
    nonDiscountTotal transaction              = sum $ itemTotal <$> full_price_items
      where full_price_items                    = filter (isItemCoveredByDiscount $ transactionDiscounts transaction) $ transactionItems transaction

    transactionTotal :: TransactionType -> Double
    transactionTotal transaction              = fromIntegral ((floor $ 100 * total) :: Integer) / 100
      where total                               = discountTotal transaction + nonDiscountTotal transaction

    scanItem :: TransactionType -> (Day -> String -> IO [Discount]) -> Item -> IO TransactionType
    scanItem transaction discountLookup item  = do
      discounts <- case length matching of
        1 -> return existing
        _ -> do
          found <- discountLookup (transactionDate transaction) (itemCode item)
          case length found of
            1 -> do
              let single = head found
              return $ single: existing
            _ -> return existing
      return $ transaction {
        transactionDiscounts                = discounts,
        transactionItems                    = item: transactionItems transaction
      }
      where existing                            = transactionDiscounts transaction
            matching                            = filter (sameCode item) existing

    scanDiscountedItem :: TransactionType -> Discount -> Item -> TransactionType
    scanDiscountedItem transaction discount item = transaction {
        transactionDiscounts                = discounts,
        transactionItems                    = item: transactionItems transaction
      }
      where
        discounts = case length matching of
          1 -> existing
          _ -> discount: existing
        existing                            = transactionDiscounts transaction
        matching                            = filter (sameCode item) existing

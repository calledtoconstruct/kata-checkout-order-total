module Transaction ( TransactionType (transactionDiscounts, transactionItems), createTransaction, transactionTotal, scanItem ) where

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
    scanItem              :: transaction -> (String -> Day -> IO [Discount]) -> Item -> IO TransactionType

  sameCode :: Item -> Discount -> Bool
  sameCode item discount = discountCode discount == itemCode item

  instance Transaction TransactionType where

    discountTotal transaction                 = sum $ getTotal (transactionDate transaction) (transactionItems transaction) <$> (transactionDiscounts transaction)

    nonDiscountTotal transaction              = sum $ itemTotal <$> full_price_items
      where full_price_items                    = filter (isItemCoveredByDiscount $ transactionDiscounts transaction) $ transactionItems transaction

    transactionTotal transaction              = fromIntegral (floor $ 100 * total) / 100
      where total                               = discountTotal transaction + nonDiscountTotal transaction

    scanItem transaction discountLookup item  = do
      discounts <- case length matching of
        1 -> return existing
        _ -> do
          found <- discountLookup (itemCode item) (transactionDate transaction)
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

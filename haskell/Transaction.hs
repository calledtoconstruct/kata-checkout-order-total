module Transaction ( TransactionType (transactionDiscounts, transactionItems), createTransaction, transactionTotal, scanItem ) where

  import Data.Time
  import Discount
  import Item

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
    scanItem              :: transaction -> (String -> [Discount]) -> Item -> TransactionType

  sameCode :: Item -> Discount -> Bool
  sameCode item discount = discountCode discount == itemCode item

  instance Transaction TransactionType where

    discountTotal transaction                 = sum $ getTotal (transactionDate transaction) (transactionItems transaction) <$> (transactionDiscounts transaction)

    nonDiscountTotal transaction              = sum $ itemPrice <$> full_price_items
      where full_price_items                    = filter (isItemCoveredByDiscount $ transactionDiscounts transaction) $ transactionItems transaction

    transactionTotal transaction              = (fromIntegral $ floor $ 100 * total) / 100
      where total                               = discountTotal transaction + nonDiscountTotal transaction
      
    scanItem transaction discountLookup item  = nextTransaction
      where nextTransaction                     = StandardTransaction {
              transactionDiscounts                = discounts,
              transactionItems                    = item: transactionItems transaction,
              transactionDate                     = transactionDate transaction
            }
            found                               = discountLookup $ itemCode item
            discounts
              | length matching == 1              = existing
              | length found == 1                 = (head found): existing
              | otherwise                         = existing
            existing                            = transactionDiscounts transaction
            matching                            = filter (sameCode item) existing


module TestHarness ( calculateTotal ) where

  import Control.Monad.Trans ( MonadIO(liftIO) )
  import Data.Time ( Day, fromGregorian )

  import Discount ( Discount )
  import DiscountList ( createDiscountList, getDiscount, addDiscounts )
  import Item ( Item )
  import ItemList ( createItemList, getItem, addItems )
  import Transaction ( createTransaction, transactionTotal, scanItem, TransactionType )

  scanQuantity :: (MonadIO m, Eq a, Num a) => (Day -> String -> IO [Discount]) -> Item -> TransactionType -> a -> m TransactionType
  scanQuantity queryDiscounts item currentTransaction quantity = case quantity of
    0 -> return currentTransaction
    _ -> do
      updatedTransaction <- liftIO $ scanItem currentTransaction queryDiscounts item
      result <- scanQuantity queryDiscounts item updatedTransaction $ (-) quantity 1
      return result

  calculateTotal :: [Item] -> [Discount] -> Integer -> Int -> Int -> String -> Int -> IO Double
  calculateTotal items discounts year month day itemName quantity = do
    let transaction = createTransaction $ fromGregorian year month day
    let itemList = addItems (createItemList "") items
    matchingItems <- liftIO $ getItem itemList itemName
    discountList <- liftIO $ addDiscounts (createDiscountList "") (getItem itemList) discounts
    updatedTransaction <- scanQuantity (getDiscount discountList) (head matchingItems) transaction quantity
    return $ transactionTotal $ updatedTransaction

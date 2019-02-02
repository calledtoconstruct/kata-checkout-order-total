module Discount ( Discount (..), getTotal, isApplicable, isValidDiscount ) where

  import Data.Time
  import Item

  class DiscountClass discount where
    getTotal                :: Day -> [Item] -> discount -> Double
    isApplicable            :: discount -> Item -> Bool
    getCountOfMatchingItems :: discount -> [Item] -> Int
    isValidDiscount         :: discount -> (String -> [Item]) -> Bool

  data Discount = StandardDiscount {
    discountCode :: String,
    discountStartDate :: Day,
    discountEndDate :: Day,
    discountPrice :: Double
  } | BulkFlatPriceDiscount {
    discountCode :: String,
    discountStartDate :: Day,
    discountEndDate :: Day,
    discountBulk :: Int,
    discountPrice :: Double
  }

  calculateStandard :: Discount -> [Item] -> Double
  calculateStandard discount []                               = 0
  calculateStandard discount list                             = sale_cost
    where sale_cost           = (*) (discountPrice discount) $ fromIntegral item_quantity
          item_quantity       = getCountOfMatchingItems discount list

  calculateBulkFlatPrice :: Discount -> [Item] -> Double
  calculateBulkFlatPrice discount []                          = 0
  calculateBulkFlatPrice discount list@(item: _)              = regular_price + sale_price
    where sale_price          = (*) (discountPrice discount) $ fromIntegral $ div item_quantity $ discountBulk discount
          regular_price       = (*) (itemPrice item) $ fromIntegral $ mod item_quantity $ discountBulk discount
          item_quantity       = getCountOfMatchingItems discount list

  instance DiscountClass Discount where
    getTotal _ [] _                             = 0
    getTotal currentDate list@(item: items) discount
      | sameCode && saleBegan && saleNotEnded   = case discount of
        StandardDiscount{}                        -> calculateStandard discount list
        BulkFlatPriceDiscount{}                   -> calculateBulkFlatPrice discount list
      | sameCode                                = (itemPrice item) + getTotal currentDate items discount
      | otherwise                               = getTotal currentDate items discount
      where sameCode                              = itemCode item == discountCode discount
            saleBegan                             = discountStartDate discount <= currentDate
            saleNotEnded                          = discountEndDate discount >= currentDate
    isApplicable discount item                  = itemCode item == discountCode discount
    getCountOfMatchingItems discount list       = length $ filter (isApplicable discount) list
    isValidDiscount discount itemLookup         = case discount of
      StandardDiscount{}                          -> hasCode && hasItem && hasStartDate && hasEndDate && hasPrice
      BulkFlatPriceDiscount{}                     -> hasCode && hasItem && hasStartDate && hasEndDate && hasPrice && hasBulk
      where hasCode                               = length (discountCode discount) > 0
            hasItem                               = length (itemLookup (discountCode discount)) == 1
            hasStartDate                          = True
            hasEndDate                            = True
            hasPrice                              = (discountPrice discount) > 0.0
            hasBulk                               = (discountBulk discount) > 0

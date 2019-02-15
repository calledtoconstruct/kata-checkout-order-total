
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Discount ( Discount (..), getTotal, isApplicable, isValidDiscount ) where

  import Data.Aeson (FromJSON, ToJSON)
  import GHC.Generics
  import Data.Time
  import Data.Sort
  import Item

  class DiscountClass discount where
    getTotal                :: Day -> [Item] -> discount -> Double
    isApplicable            :: discount -> Item -> Bool
    getCountOfMatchingItems :: discount -> [Item] -> Int
    isValidDiscount         :: discount -> (String -> IO [Item]) -> IO Bool

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
  } | UpSalePercentDiscount {
    discountCode :: String,
    discountStartDate :: Day,
    discountEndDate :: Day,
    discountBulk :: Int,
    discountSale :: Int,
    discountPercent :: Double
  } | LimitedUpSalePercentDiscount {
    discountCode :: String,
    discountStartDate :: Day,
    discountEndDate :: Day,
    discountBulk :: Int,
    discountSale :: Int,
    discountPercent :: Double,
    discountLimit :: Int
  } | UpSaleFlatPriceDiscount {
    discountCode :: String,
    discountStartDate :: Day,
    discountEndDate :: Day,
    discountBulk :: Int,
    discountSale :: Int,
    discountPrice :: Double
  } | LimitedUpSaleFlatPriceDiscount {
    discountCode :: String,
    discountStartDate :: Day,
    discountEndDate :: Day,
    discountBulk :: Int,
    discountSale :: Int,
    discountPrice :: Double,
    discountLimit :: Int
  } | UpSalePercentDiscountByWeight {
    discountCode :: String,
    discountStartDate :: Day,
    discountEndDate :: Day,
    discountBulk :: Int,
    discountSale :: Int,
    discountPercent :: Double
  } deriving (Generic, Show)

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

  calculateUpSalePercent :: Discount -> [Item] -> Double
  calculateUpSalePercent discount []                          = 0
  calculateUpSalePercent discount list@(item: _)              = (bundles * bundle_cost) + extra_price
    where bulk_cost           = (*) (itemPrice item) $ fromIntegral $ discountBulk discount
          sale_price          = (*) (itemPrice item) $ 1.0 - discountPercent discount
          sale_cost           = (*) sale_price $ fromIntegral $ discountSale discount
          extra_quantity      = fromIntegral $ mod item_quantity bundle_quantity
          extra_price         = extra_quantity * itemPrice item
          bundle_quantity     = discountBulk discount + discountSale discount
          bundle_cost         = bulk_cost + sale_cost
          bundles             = fromIntegral $ div item_quantity bundle_quantity
          item_quantity       = getCountOfMatchingItems discount list

  calculateLimitedUpSalePercent :: Discount -> [Item] -> Double
  calculateLimitedUpSalePercent discount []                   = 0
  calculateLimitedUpSalePercent discount list@(item: _)       = (bundles * bundle_cost) + over_cost
    where over                = max 0 $ item_quantity - (discountLimit discount)
          under               = item_quantity - over
          over_cost           = (*) (itemPrice item) $ fromIntegral $ (mod under bundle_quantity) + over
          bulk_cost           = (*) (itemPrice item) $ fromIntegral $ discountBulk discount
          sale_price          = (*) (itemPrice item) $ 1.0 - discountPercent discount
          sale_cost           = (*) sale_price $ fromIntegral $ discountSale discount
          bundle_quantity     = discountBulk discount + discountSale discount
          bundle_cost         = bulk_cost + sale_cost
          bundles             = fromIntegral (div under bundle_quantity)
          item_quantity       = getCountOfMatchingItems discount list

  calculateUpSaleFlatPrice :: Discount -> [Item] -> Double
  calculateUpSaleFlatPrice discount []                        = 0
  calculateUpSaleFlatPrice discount list@(item: _)            = (bundles * bundle_cost) + regular_cost
    where regular_cost        = (*) (itemPrice item) $ fromIntegral $ mod item_quantity bundle_quantity
          bulk_cost           = (*) (itemPrice item) $ fromIntegral $ discountBulk discount
          sale_cost           = (*) (discountPrice discount) $ fromIntegral $ discountSale discount
          bundle_quantity     = discountBulk discount + discountSale discount
          bundle_cost         = bulk_cost + sale_cost
          bundles             = fromIntegral $ div item_quantity bundle_quantity
          item_quantity       = getCountOfMatchingItems discount list

  calculateLimitedUpSaleFlatPrice :: Discount -> [Item] -> Double
  calculateLimitedUpSaleFlatPrice discount []                 = 0
  calculateLimitedUpSaleFlatPrice discount list@(item: _)     = (bundles * bundle_cost) + over_cost
    where over                = max 0 $ item_quantity - (discountLimit discount)
          under               = item_quantity - over
          over_cost           = (*) (itemPrice item) $ fromIntegral $ (mod under bundle_quantity) + over
          bulk_cost           = (*) (itemPrice item) $ fromIntegral $ discountBulk discount
          sale_cost           = (*) (discountPrice discount) $ fromIntegral $ discountSale discount
          bundle_quantity     = discountBulk discount + discountSale discount
          bundle_cost         = bulk_cost + sale_cost
          bundles             = fromIntegral $ div under bundle_quantity
          item_quantity       = getCountOfMatchingItems discount list

  calculateUpSalePercentDiscountByWeight :: Discount -> [Item] -> Double
  calculateUpSalePercentDiscountByWeight discount []          = 0
  calculateUpSalePercentDiscountByWeight discount list        = sum costs
    where pattern_start       = replicate (discountBulk discount) 1
          pattern_end         = replicate (discountSale discount) (discountPercent discount) 
          totals              = reverse . sort $ itemTotal <$> list
          costs               = map (uncurry (*)) $ zip totals $ cycle $ pattern_start ++ pattern_end

  instance DiscountClass Discount where
    getTotal _ [] _                             = 0
    getTotal currentDate list@(item: items) discount
      | sameCode && saleBegan && saleNotEnded   = case discount of
        StandardDiscount{}                        -> calculateStandard discount list
        BulkFlatPriceDiscount{}                   -> calculateBulkFlatPrice discount list
        UpSalePercentDiscount{}                   -> calculateUpSalePercent discount list
        LimitedUpSalePercentDiscount{}            -> calculateLimitedUpSalePercent discount list
        UpSaleFlatPriceDiscount{}                 -> calculateUpSaleFlatPrice discount list
        LimitedUpSaleFlatPriceDiscount{}          -> calculateLimitedUpSaleFlatPrice discount list
        UpSalePercentDiscountByWeight{}           -> calculateUpSalePercentDiscountByWeight discount list
      | sameCode                                = (itemPrice item) + getTotal currentDate items discount
      | otherwise                               = getTotal currentDate items discount
      where sameCode                              = itemCode item == discountCode discount
            saleBegan                             = discountStartDate discount <= currentDate
            saleNotEnded                          = discountEndDate discount >= currentDate
    isApplicable discount item                  = itemCode item == discountCode discount
    getCountOfMatchingItems discount list       = length $ filter (isApplicable discount) list
    isValidDiscount discount itemLookup         = do
      items <- itemLookup $ discountCode discount
      let hasItem = 1 == length items
      let item = head items
      let hasByQuantityItem = isByQuantityItem item
      let hasByWeightItem = isByWeightItem item
      return $ case discount of
        StandardDiscount{}                          -> hasCode && hasItem && hasByQuantityItem && hasStartDate && hasEndDate && hasPrice
        BulkFlatPriceDiscount{}                     -> hasCode && hasItem && hasByQuantityItem && hasStartDate && hasEndDate && hasBulk && hasPrice
        UpSalePercentDiscount{}                     -> hasCode && hasItem && hasByQuantityItem && hasStartDate && hasEndDate && hasBulk && hasSale && hasPercent
        LimitedUpSalePercentDiscount{}              -> hasCode && hasItem && hasByQuantityItem && hasStartDate && hasEndDate && hasBulk && hasSale && hasPercent && hasLimit
        UpSaleFlatPriceDiscount{}                   -> hasCode && hasItem && hasByQuantityItem && hasStartDate && hasEndDate && hasBulk && hasSale && hasPrice
        LimitedUpSaleFlatPriceDiscount{}            -> hasCode && hasItem && hasByQuantityItem && hasStartDate && hasEndDate && hasBulk && hasSale && hasPrice && hasLimit
        UpSalePercentDiscountByWeight{}             -> hasCode && hasItem && hasByWeightItem && hasStartDate && hasEndDate && hasBulk && hasSale && hasPercent
      where hasCode                               = not $ null $ discountCode discount
            hasStartDate                          = True
            hasEndDate                            = True
            hasPrice                              = (discountPrice discount) > 0.0
            hasBulk                               = (discountBulk discount) > 0
            hasSale                               = (discountSale discount) > 0
            hasPercent                            = (discountPercent discount) > 0.0 && (discountPercent discount) <= 1.0
            hasLimit                              = (discountLimit discount) > 0

  instance ToJSON Discount where

  instance FromJSON Discount where

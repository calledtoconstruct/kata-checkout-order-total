module Item ( Item (..), isByQuantityItem, isByWeightItem, isValidItem ) where

  class ItemClass item where
    isByQuantityItem  :: item -> Bool
    isByWeightItem    :: item -> Bool
    isValidItem       :: item -> Bool

  data Item = ByQuantityItem {
    itemCode          :: String,
    itemDescription   :: String,
    itemPrice         :: Double
  }

  instance ItemClass Item where
    isByQuantityItem item   = case item of
      ByQuantityItem{}        -> True
    isByWeightItem item     = case item of
      ByQuantityItem{}        -> False
    isValidItem item        = hasCode && hasDescription && hasPrice
      where hasCode           = not $ null $ itemCode item
            hasDescription    = not $ null $ itemDescription item
            hasPrice          = (itemPrice item) > 0.0

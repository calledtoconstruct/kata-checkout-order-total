
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Item ( Item (..), isByQuantityItem, isByWeightItem, isValidItem ) where

  import Data.Aeson (FromJSON, ToJSON)
  import GHC.Generics

  class ItemClass item where
    isByQuantityItem  :: item -> Bool
    isByWeightItem    :: item -> Bool
    isValidItem       :: item -> Bool

  data Item = ByQuantityItem {
    itemCode          :: String,
    itemDescription   :: String,
    itemType          :: String,
    itemPrice         :: Double
  } deriving (Generic)

  instance ItemClass Item where
    isByQuantityItem item   = case item of
      ByQuantityItem{}        -> True
    isByWeightItem item     = case item of
      ByQuantityItem{}        -> False
    isValidItem item        = hasCode && hasDescription && hasPrice
      where hasCode           = not $ null $ itemCode item
            hasDescription    = not $ null $ itemDescription item
            hasPrice          = (itemPrice item) > 0.0

  instance FromJSON Item where

  instance ToJSON Item where

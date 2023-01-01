{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}

module Item ( Item (..), isByQuantityItem, isByWeightItem, isValidItem, itemTotal ) where

  import Data.Aeson   (FromJSON, ToJSON)
  import GHC.Generics ( Generic )

  class ItemClass item where
    isByQuantityItem  :: item -> Bool
    isByWeightItem    :: item -> Bool
    isValidItem       :: item -> Bool
    itemTotal         :: item -> Double

  data Item = ByQuantityItem {
    itemCode          :: String,
    itemDescription   :: String,
    itemType          :: String,
    itemPrice         :: Double
  } | ByWeightItem {
    itemCode          :: String,
    itemDescription   :: String,
    itemType          :: String,
    itemPrice         :: Double,
    itemWeight        :: Maybe Double
  } deriving (Generic, Show)

  instance ItemClass Item where
    itemTotal :: Item -> Double
    itemTotal item          = case item of
      ByQuantityItem{}        -> itemPrice item
      ByWeightItem{}          -> case itemWeight item of
        Nothing                 -> 0
        Just weight             -> itemPrice item * weight

    isByQuantityItem :: Item -> Bool
    isByQuantityItem item   = case item of
      ByQuantityItem{}        -> True
      ByWeightItem{}          -> False

    isByWeightItem :: Item -> Bool
    isByWeightItem item     = case item of
      ByQuantityItem{}        -> False
      ByWeightItem{}          -> True

    isValidItem :: Item -> Bool
    isValidItem item        = hasCode && hasDescription && hasPrice
      where hasCode           = not $ null $ itemCode item
            hasDescription    = not $ null $ itemDescription item
            hasPrice          = itemPrice item > 0.0

  instance ToJSON Item where

  instance FromJSON Item where

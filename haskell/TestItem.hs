
{-# LANGUAGE OverloadedStrings #-}

module TestItem where

  import Data.Aeson (encode, decode)
  import Data.ByteString.Lazy.UTF8 (fromString)
  import Item

  dogFood :: Item
  dogFood = ByQuantityItem {
    itemCode = "dog food",
    itemDescription = "2 lbs bag of dry dog food.",
    itemType = "by quantity",
    itemPrice = 9.99
  }

  groundBeef :: Item
  groundBeef = ByWeightItem {
    itemCode = "ground beef",
    itemDescription = "Package of premium Ground Beef.",
    itemType = "by weight",
    itemPrice = 2.49,
    itemWeight = Nothing
  }

  -- $setup
  -- >>> dogFoodJSON = fromString "{\"tag\":\"ByQuantityItem\",\"itemType\":\"by quantity\",\"itemCode\":\"dog food\",\"itemDescription\":\"2 lbs bag of dry dog food.\",\"itemPrice\":9.99}"
  -- >>> groundBeefJSON = fromString "{\"tag\":\"ByWeightItem\",\"itemType\":\"by weight\",\"itemCode\":\"ground beef\",\"itemDescription\":\"Package of premium Ground Beef.\",\"itemWeight\":null,\"itemPrice\":2.49}"

  -- | Serialization of Standard By Quantity Item
  -- >>> encode dogFood
  -- "{\"tag\":\"ByQuantityItem\",\"itemType\":\"by quantity\",\"itemCode\":\"dog food\",\"itemDescription\":\"2 lbs bag of dry dog food.\",\"itemPrice\":9.99}"

  -- | Deserialization of Standard Quantity Item
  -- >>> case (decode dogFoodJSON) :: Maybe Item of Just item -> item; Nothing -> error "Invalid input"
  -- ByQuantityItem {itemCode = "dog food", itemDescription = "2 lbs bag of dry dog food.", itemType = "by quantity", itemPrice = 9.99}

  -- | Serialization of Standard By Weight Item
  -- >>> encode groundBeef
  -- "{\"tag\":\"ByWeightItem\",\"itemType\":\"by weight\",\"itemCode\":\"ground beef\",\"itemDescription\":\"Package of premium Ground Beef.\",\"itemWeight\":null,\"itemPrice\":2.49}"

  -- | Deserialization of Standard By Weight Item
  -- >>> case (decode groundBeefJSON) :: Maybe Item of Just item -> item; Nothing -> error "Invalid input"
  -- ByWeightItem {itemCode = "ground beef", itemDescription = "Package of premium Ground Beef.", itemType = "by weight", itemPrice = 2.49, itemWeight = Nothing}

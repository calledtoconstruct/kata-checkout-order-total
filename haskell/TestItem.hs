
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

  -- $setup
  -- >>> dogFoodJSON = fromString "{\"tag\":\"ByQuantityItem\",\"itemType\":\"by quantity\",\"itemCode\":\"dog food\",\"itemDescription\":\"2 lbs bag of dry dog food.\",\"itemPrice\":9.99}"

  -- | Serialization of Standard Item
  -- >>> encode dogFood
  -- "{\"tag\":\"ByQuantityItem\",\"itemType\":\"by quantity\",\"itemCode\":\"dog food\",\"itemDescription\":\"2 lbs bag of dry dog food.\",\"itemPrice\":9.99}"

  -- | Deserialization of Standard Item
  -- >>> case (decode dogFoodJSON) :: Maybe Item of Just item -> item; Nothing -> error "Invalid input"
  -- ByQuantityItem {itemCode = "dog food", itemDescription = "2 lbs bag of dry dog food.", itemType = "by quantity", itemPrice = 9.99}
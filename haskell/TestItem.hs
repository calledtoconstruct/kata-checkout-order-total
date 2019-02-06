
{-# LANGUAGE OverloadedStrings #-}

module TestItem where

  import Data.Aeson (decode)
  import Data.ByteString.Lazy.UTF8 (fromString)
  import Item

  -- $setup
  -- >>> dogFood = fromString "{\"tag\":\"ByQuantityItem\",\"itemType\":\"by quantity\",\"itemCode\":\"dog food\",\"itemDescription\":\"2 lbs bag of dry dog food.\",\"itemPrice\":9.99}"

  -- | Deserialization of Standard Item
  -- >>> case (decode dogFood) :: Maybe Item of Just item -> item; Nothing -> error "Invalid input"
  -- ByQuantityItem {itemCode = "dog food", itemDescription = "2 lbs bag of dry dog food.", itemType = "by quantity", itemPrice = 9.99}
{-# LANGUAGE OverloadedStrings #-}

module TestItem where

  import Data.Aeson
  import Data.ByteString.Lazy.UTF8 (ByteString, fromString)

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

  -- | Serialization of Standard By Quantity Item
  -- >>> serializationOfDogFood
  -- "{\"itemCode\":\"dog food\",\"itemDescription\":\"2 lbs bag of dry dog food.\",\"itemPrice\":9.99,\"itemType\":\"by quantity\",\"tag\":\"ByQuantityItem\"}"
  serializationOfDogFood :: ByteString
  serializationOfDogFood = encode dogFood

  dogFoodJSON :: ByteString
  dogFoodJSON = fromString "{\"tag\":\"ByQuantityItem\",\"itemType\":\"by quantity\",\"itemCode\":\"dog food\",\"itemDescription\":\"2 lbs bag of dry dog food.\",\"itemPrice\":9.99}"

  -- | Deserialization of Standard Quantity Item
  -- >>> deserializationOfDogFood
  -- "ByQuantityItem {itemCode = \"dog food\", itemDescription = \"2 lbs bag of dry dog food.\", itemType = \"by quantity\", itemPrice = 9.99}"
  deserializationOfDogFood :: String
  deserializationOfDogFood = case (decode dogFoodJSON) :: Maybe Item of
    Just item -> show item
    Nothing -> "Invalid input"

  -- | Serialization of Standard By Weight Item
  -- >>> serializationOfGroundBeef
  -- "{\"itemCode\":\"ground beef\",\"itemDescription\":\"Package of premium Ground Beef.\",\"itemPrice\":2.49,\"itemType\":\"by weight\",\"itemWeight\":null,\"tag\":\"ByWeightItem\"}"
  serializationOfGroundBeef :: ByteString
  serializationOfGroundBeef = encode groundBeef

  groundBeefJSON :: ByteString
  groundBeefJSON = fromString "{\"tag\":\"ByWeightItem\",\"itemType\":\"by weight\",\"itemCode\":\"ground beef\",\"itemDescription\":\"Package of premium Ground Beef.\",\"itemWeight\":null,\"itemPrice\":2.49}"

  -- | Deserialization of Standard By Weight Item
  -- >>> deserializationOfGroundBeef
  -- "ByWeightItem {itemCode = \"ground beef\", itemDescription = \"Package of premium Ground Beef.\", itemType = \"by weight\", itemPrice = 2.49, itemWeight = Nothing}"
  deserializationOfGroundBeef :: String
  deserializationOfGroundBeef = case (decode groundBeefJSON) :: Maybe Item of
    Just item -> show item
    Nothing -> "Invalid input"
  
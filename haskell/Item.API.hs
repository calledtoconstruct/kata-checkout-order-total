
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module ItemAPI where

  import Network.HTTP.Types.Status
  import Network.Wai.Middleware.Cors
  import Web.Scotty
  import Item

  itemDogFood :: Item
  itemDogFood = ByQuantityItem {
    itemCode = "dog food",
    itemDescription = "2 lbs bag of dry dog food.",
    itemType = "by quantity",
    itemPrice = 9.99
  }

  itemCrisps :: Item
  itemCrisps = ByQuantityItem {
    itemCode = "crisps",
    itemDescription = "12 oz bag of salted crisps.",
    itemType = "by quantity",
    itemPrice = 0.55
  }

  itemCatFood :: Item
  itemCatFood = ByQuantityItem {
    itemCode = "cat food",
    itemDescription = "12 oz can of cat food.",
    itemType = "by quantity",
    itemPrice = 1.25
  }

  itemTurkey :: Item
  itemTurkey = ByQuantityItem {
    itemCode = "turkey",
    itemDescription = "15 lbs whole turkey.",
    itemType = "by quantity",
    itemPrice = 20.00
  }

  itemSwissCheese :: Item
  itemSwissCheese = ByQuantityItem {
    itemCode = "swiss cheese",
    itemDescription = "12 oz package of swiss chese.",
    itemType = "by quantity",
    itemPrice = 2.1
  }

  itemBologna :: Item
  itemBologna = ByQuantityItem {
    itemCode = "bologna",
    itemDescription = "32 oz of premium bologna.",
    itemType = "by quantity",
    itemPrice = 3.25
  }

  items :: [Item]
  items = itemDogFood: itemCrisps: itemCatFood: itemTurkey: itemSwissCheese: itemBologna: []

  matchesCode :: String -> Item -> Bool
  matchesCode code item = code == itemCode item

  main = do
    putStrLn "Starting Server..."
    scotty 3000 $ do
      middleware simpleCors
      get "/item/:code" $ do
          code <- param "code"
          case (filter (matchesCode code) items) of
            []      -> status status404 *> text "Not Found"
            [item]  -> json item

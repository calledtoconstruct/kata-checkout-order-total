
{-# LANGUAGE OverloadedStrings #-}

module ItemListClient ( ItemList, createItemList, getItem ) where

  import Data.Aeson               (decode)
  import Network.HTTP.Client      (newManager, defaultManagerSettings, parseRequest, httpLbs, method, responseBody)
  import Item ( Item )

  data ItemList                   = ItemList

  class ItemListClass itemList where
    getItem :: itemList -> String -> IO [Item]

  createItemList :: ItemList
  createItemList                  = ItemList

  instance ItemListClass ItemList where
    getItem itemList code         = do
      manager                       <- newManager defaultManagerSettings
      initialRequest                <- parseRequest $ "http://item-api:8082/item/" ++ code
      let request                   = initialRequest { method = "GET" }
      response                      <- httpLbs request manager
      let item                      = decode $ responseBody response :: Maybe Item
      case item of
        Just item                     -> return [item]
        _                             -> return []

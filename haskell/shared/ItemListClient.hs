{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

module ItemListClient ( ItemList, createItemList, getItem ) where

  import Data.Aeson               (decode)
  import Network.HTTP.Client      (newManager, defaultManagerSettings, parseRequest, httpLbs, method, responseBody)

  import Item ( Item )

  newtype ItemList = ItemList { baseUrl :: String }

  class ItemListClass itemList where
    getItem :: itemList -> String -> IO [Item]

  createItemList :: String -> ItemList
  createItemList baseURL = ItemList { baseUrl = baseURL }

  instance ItemListClass ItemList where
    getItem :: ItemList -> String -> IO [Item]
    getItem itemList code = do
      manager                       <- newManager defaultManagerSettings
      initialRequest                <- parseRequest $ baseUrl itemList ++ "/item/" ++ code
      let request                   = initialRequest { method = "GET" }
      response                      <- httpLbs request manager
      let item                      = decode $ responseBody response :: Maybe Item
      case item of
        Just foundItem                -> return [foundItem]
        _                             -> return []

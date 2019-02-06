
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module ItemAPI where

  import Network.HTTP.Types.Status
  import Network.Wai.Middleware.Cors
  import Web.Scotty
  import Item
  import ItemList

  main :: IO ()
  main = do
    putStrLn "Starting Server..."
    itemList <- load "./Items.json" $ pure createItemList
    scotty 3000 $ do
      middleware simpleCors
      get "/item/:code" $ do
        code <- param "code"
        let items = getItem itemList code
        case items of
          []      -> status status404 *> text "Not Found"
          [item]  -> json item

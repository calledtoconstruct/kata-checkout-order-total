
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module ItemAPI where

  import Control.Monad.Trans
  import Network.HTTP.Types.Status
  import Network.Wai.Middleware.Cors
  import Web.Scotty
  import Item
  import ItemList

  main :: IO ()
  main = do
    putStrLn "Starting Server..."
    itemList <- loadItems "./Items.json" $ pure createItemList
    scotty 8082 $ do
      middleware simpleCors
      get "/item/:code" $ do
        requestedCode <- param "code"
        foundItems <- liftIO $ getItem itemList requestedCode
        case foundItems of
          []      -> status status404 *> text "Not Found"
          [item]  -> json item
          list    -> status status500 *> text "Non-unique Code"


{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

  import Control.Monad.Trans ( MonadIO(liftIO) )
  import Network.HTTP.Types.Status ( status500, status404 )
  import Network.Wai.Middleware.Cors ( simpleCors )
  import Web.Scotty ( get, json, middleware, param, scotty, status, text )
  import ItemList ( getItem, createItemList, loadItems )

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

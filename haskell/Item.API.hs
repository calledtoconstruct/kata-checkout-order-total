
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module ItemAPI where

  import Data.ByteString.Lazy.UTF8 (fromString)
  import Data.Aeson (decode)
  import Network.HTTP.Types.Status
  import Network.Wai.Middleware.Cors
  import Web.Scotty
  import System.IO
  import Item
  import ItemList

  loadItem :: Handle -> IO ItemList -> IO ItemList
  loadItem handle itemList = do
    eof <- hIsEOF handle
    updated <- case eof of
      False -> do
        line <- hGetLine handle
        updated <- case (decode $ fromString line) :: Maybe Item of
          Just item -> do
            loadItem handle $ pure . flip addItem item =<< itemList
          Nothing -> do
            hClose handle
            itemList
        return updated
      True -> do
        hClose handle
        itemList
    return updated

  load :: IO ItemList -> IO ItemList
  load itemList = do
    handle <- openFile "./Items.json" ReadMode
    loadItem handle itemList

  main :: IO ()
  main = do
    putStrLn "Starting Server..."
    itemList <- load $ pure createItemList
    scotty 3000 $ do
      middleware simpleCors
      get "/item/:code" $ do
        code <- param "code"
        let items = getItem itemList code
        case items of
          []      -> status status404 *> text "Not Found"
          [item]  -> json item

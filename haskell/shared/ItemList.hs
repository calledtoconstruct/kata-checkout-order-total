{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use if" #-}

module ItemList (ItemList, createItemList, getItem, addItems, loadItems) where

import Data.Aeson (decode)
import Data.ByteString.Lazy.UTF8 (fromString)
import System.IO ( Handle, IOMode(ReadMode), hClose, hIsEOF, hGetLine, openFile )

import Item ( Item(itemCode), isValidItem )

newtype ItemList = ItemList { items :: [Item] }

class ItemListClass itemList where
  getItem :: itemList -> String -> IO [Item]
  addItem :: itemList -> Item -> itemList
  addItems :: itemList -> [Item] -> itemList

createItemList :: String -> ItemList
createItemList _ = ItemList { items = [] }

sameCode :: String -> Item -> Bool
sameCode code item = code == itemCode item

instance ItemListClass ItemList where
  getItem :: ItemList -> String -> IO [Item]
  getItem itemList code = return $ filter (sameCode code) $ items itemList

  addItem :: ItemList -> Item -> ItemList
  addItem itemList item
    | isValidItem item = itemList {items = item : items itemList}
    | otherwise = itemList

  addItems :: ItemList -> [Item] -> ItemList
  addItems itemList [] = itemList
  addItems itemList (item : remaining)
    | isValidItem item = addItems nextList remaining
    | otherwise = addItems itemList remaining
    where
      nextList = itemList {items = item : items itemList}

loadItem :: Handle -> IO ItemList -> IO ItemList
loadItem handle itemList = do
  eof <- hIsEOF handle
  case eof of
    False -> do
      line <- hGetLine handle
      case (decode $ fromString line) :: Maybe Item of
        Just item -> do
          loadItem handle $ flip addItem item <$> itemList
        Nothing -> do
          hClose handle
          itemList
    True -> do
      hClose handle
      itemList

loadItems :: String -> IO ItemList -> IO ItemList
loadItems fileName itemList = do
  handle <- openFile fileName ReadMode
  loadItem handle itemList

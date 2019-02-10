module ItemList ( ItemList, createItemList, getItem, addItems, loadItems ) where

  import Data.ByteString.Lazy.UTF8 (fromString)
  import Data.Aeson (decode)
  import System.IO
  import Item

  data ItemList   = ItemList {
    items           :: [Item]
  }

  class ItemListClass itemList where
    getItem         :: itemList -> String -> IO [Item]
    addItem         :: itemList -> Item -> itemList
    addItems        :: itemList -> [Item] -> itemList

  createItemList :: ItemList
  createItemList              = ItemList { items = [] }

  sameCode :: String -> Item -> Bool
  sameCode code item          = code == itemCode item

  instance ItemListClass ItemList where

    getItem itemList code     = return $ filter (sameCode code) $ items itemList

    addItem itemList item
      | isValidItem item      = ItemList { items = item: items itemList }
      | otherwise             = itemList

    addItems itemList []      = itemList
    addItems itemList (item: remaining)
      | isValidItem item        = addItems nextList remaining
      | otherwise               = addItems itemList remaining
      where nextList            = ItemList { items = item: items itemList }

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

  loadItems :: String -> IO ItemList -> IO ItemList
  loadItems fileName itemList = do
    handle <- openFile fileName ReadMode
    loadItem handle itemList

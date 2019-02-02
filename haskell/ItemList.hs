module ItemList ( ItemList, createItemList, getItem, addItem, addItems ) where

  import Item

  data ItemList   = ItemList {
    items           :: [Item]
  }

  class ItemListClass itemList where
    getItem         :: itemList -> String -> [Item]
    addItem         :: itemList -> Item -> ItemList
    addItems        :: itemList -> [Item] -> ItemList

  createItemList :: ItemList
  createItemList              = ItemList { items = [] }

  sameCode :: String -> Item -> Bool
  sameCode code item          = code == itemCode item

  instance ItemListClass ItemList where

    getItem itemList code     = filter (sameCode code) $ items itemList

    addItem itemList item
      | isValidItem item      = ItemList { items = item: items itemList }
      | otherwise             = itemList

    addItems itemList []      = itemList
    addItems itemList (item: remaining)
      | isValidItem item        = addItems nextList remaining
      | otherwise               = addItems itemList remaining
      where nextList            = ItemList { items = item: items itemList }

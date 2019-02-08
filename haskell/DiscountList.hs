module DiscountList ( DiscountList, createDiscountList, getDiscount, addDiscounts, loadDiscounts ) where

  import Data.ByteString.Lazy.UTF8 (fromString)
  import Data.Aeson (decode)
  import System.IO
  import Discount
  import Item

  data DiscountList = DiscountList {
    discounts         :: [Discount]
  }

  class DiscountListClass discountList where
    getDiscount       :: discountList -> String -> [Discount]
    addDiscount       :: discountList -> (String -> [Item]) -> Discount -> DiscountList
    addDiscounts      :: discountList -> (String -> [Item]) -> [Discount] -> DiscountList

  createDiscountList :: DiscountList
  createDiscountList                            = DiscountList { discounts = [] }

  sameCode :: String -> Discount -> Bool
  sameCode code discount                        = code == discountCode discount

  instance DiscountListClass DiscountList where

    getDiscount discountList code               = filter (sameCode code) (discounts discountList)

    addDiscount discountList itemLookup discount
      | isValidDiscount discount itemLookup     = DiscountList { discounts = discount: discounts discountList }
      | otherwise                               = discountList

    addDiscounts discountList _ []              = discountList
    addDiscounts discountList itemLookup (discount: remaining)
      | isValidDiscount discount itemLookup     = addDiscounts nextList itemLookup remaining
      | otherwise                               = addDiscounts discountList itemLookup remaining
      where nextList                              = DiscountList { discounts = discount: discounts discountList }

  loadDiscount :: Handle -> (String -> [Item]) -> IO DiscountList -> IO DiscountList
  loadDiscount handle itemLookup discountList = do
    eof <- hIsEOF handle
    updated <- case eof of
      False -> do
        line <- hGetLine handle
        updated <- case (decode $ fromString line) :: Maybe Discount of
          Just discount -> do
            dl <- discountList
            loadDiscount handle itemLookup $ pure $ addDiscount dl itemLookup discount
          Nothing -> do
            hClose handle
            discountList
        return updated
      True -> do
        hClose handle
        discountList
    return updated

  loadDiscounts :: (String -> [Item]) -> String -> IO DiscountList -> IO DiscountList
  loadDiscounts itemLookup fileName discountList = do
    handle <- openFile fileName ReadMode
    loadDiscount handle itemLookup discountList

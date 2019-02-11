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
    getDiscount       :: discountList -> String -> IO [Discount]
    addDiscount       :: discountList -> (String -> IO [Item]) -> Discount -> IO DiscountList
    addDiscounts      :: discountList -> (String -> IO [Item]) -> [Discount] -> IO DiscountList

  createDiscountList :: DiscountList
  createDiscountList                            = DiscountList { discounts = [] }

  sameCode :: String -> Discount -> Bool
  sameCode code discount                        = code == discountCode discount

  instance DiscountListClass DiscountList where

    getDiscount discountList code               = return $ filter (sameCode code) (discounts discountList)

    addDiscount discountList itemLookup discount = do
      valid <- isValidDiscount discount itemLookup
      return $ case valid of
        True -> DiscountList { discounts = discount: discounts discountList }
        False -> discountList

    addDiscounts discountList _ []              = return discountList
    addDiscounts discountList itemLookup (discount: remaining) = do
      valid <- isValidDiscount discount itemLookup
      case valid of
        True -> addDiscounts nextList itemLookup remaining
        False -> addDiscounts discountList itemLookup remaining
      where nextList                              = DiscountList { discounts = discount: discounts discountList }

  loadDiscount :: Handle -> (String -> IO [Item]) -> IO DiscountList -> IO DiscountList
  loadDiscount handle itemLookup discountList = do
    eof <- hIsEOF handle
    updated <- case eof of
      False -> do
        line <- hGetLine handle
        updated <- case (decode $ fromString line) :: Maybe Discount of
          Just discount -> do
            dl <- discountList
            loadDiscount handle itemLookup $ addDiscount dl itemLookup discount
          Nothing -> do
            hClose handle
            discountList
        return updated
      True -> do
        hClose handle
        discountList
    return updated

  loadDiscounts :: (String -> IO [Item]) -> String -> IO DiscountList -> IO DiscountList
  loadDiscounts itemLookup fileName discountList = do
    handle <- openFile fileName ReadMode
    loadDiscount handle itemLookup discountList

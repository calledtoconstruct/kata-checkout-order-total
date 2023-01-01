{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use if" #-}
  
module DiscountList ( DiscountList, createDiscountList, getDiscount, addDiscounts, loadDiscounts ) where

import Data.Aeson ( decode )
import Data.ByteString.Lazy.UTF8 ( fromString )
import Data.Time ( Day )
import System.IO ( Handle, hClose, hIsEOF, openFile, hGetLine, IOMode(ReadMode) )

import Discount ( Discount(discountStartDate, discountEndDate, discountCode), isValidDiscount )
import Item ( Item )

newtype DiscountList = DiscountList { discounts :: [Discount] }

class DiscountListClass discountList where
  getDiscount :: discountList -> Day -> String -> IO [Discount]
  addDiscount :: discountList -> (String -> IO [Item]) -> Discount -> IO DiscountList
  addDiscounts :: discountList -> (String -> IO [Item]) -> [Discount] -> IO DiscountList

createDiscountList :: String -> DiscountList
createDiscountList _ = DiscountList {discounts = []}

sameDate :: Day -> Discount -> Bool
sameDate date discount = date >= discountStartDate discount && date <= discountEndDate discount

sameCode :: String -> Discount -> Bool
sameCode code discount = code == discountCode discount

instance DiscountListClass DiscountList where
  getDiscount :: DiscountList -> Day -> String -> IO [Discount]
  getDiscount discountList date code = return $ filter (sameDate date) $ filter (sameCode code) (discounts discountList)

  addDiscount :: DiscountList -> (String -> IO [Item]) -> Discount -> IO DiscountList
  addDiscount discountList itemLookup discount = do
    valid <- isValidDiscount discount itemLookup
    return $ case valid of
      True -> discountList {discounts = discount : discounts discountList}
      False -> discountList

  addDiscounts :: DiscountList -> (String -> IO [Item]) -> [Discount] -> IO DiscountList
  addDiscounts discountList _ [] = return discountList
  addDiscounts discountList itemLookup (discount : remaining) = do
    valid <- isValidDiscount discount itemLookup
    case valid of
      True -> addDiscounts nextList itemLookup remaining
      False -> addDiscounts discountList itemLookup remaining
    where
      nextList = discountList {discounts = discount : discounts discountList}

loadDiscount :: Handle -> (String -> IO [Item]) -> IO DiscountList -> IO DiscountList
loadDiscount handle itemLookup discountList = do
  eof <- hIsEOF handle
  case eof of
    False -> do
      line <- hGetLine handle
      case (decode $ fromString line) :: Maybe Discount of
        Just discount -> do
          discountList' <- discountList
          loadDiscount handle itemLookup $ addDiscount discountList' itemLookup discount
        Nothing -> do
          hClose handle
          discountList
    True -> do
      hClose handle
      discountList

loadDiscounts :: (String -> IO [Item]) -> String -> IO DiscountList -> IO DiscountList
loadDiscounts itemLookup fileName discountList = do
  handle <- openFile fileName ReadMode
  loadDiscount handle itemLookup discountList

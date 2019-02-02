module DiscountList ( DiscountList, createDiscountList, getDiscount, addDiscount, addDiscounts ) where

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

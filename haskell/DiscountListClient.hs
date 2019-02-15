
{-# LANGUAGE OverloadedStrings #-}

module DiscountListClient ( DiscountList, createDiscountList, getDiscount ) where

  import Network.HTTP.Client        (newManager, defaultManagerSettings, parseRequest, httpLbs, method, responseBody)
  import Data.Aeson                 (decode)
  import Discount

  data DiscountList                 = DiscountList

  class DiscountListClass discountList where
    getDiscount :: discountList -> String -> IO [Discount]

  createDiscountList :: DiscountList
  createDiscountList                = DiscountList

  instance DiscountListClass DiscountList where
    getDiscount discountList code   = do
      manager                         <- newManager defaultManagerSettings
      initialRequest                  <- parseRequest $ "http://localhost:8081/discount/" ++ code
      let request                     = initialRequest { method = "GET" }
      response                        <- httpLbs request manager
      let discount                    = decode $ responseBody response :: Maybe Discount
      case discount of
        Just discount                   -> return [discount]
        otherwise                       -> return []

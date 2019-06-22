FROM haskell AS kcot-base

WORKDIR /opt/server
RUN cabal update
COPY ./kata-checkout-order-total.cabal /opt/server/kata-checkout-order-total.cabal
RUN cabal install --only-dependencies
COPY . /opt/server

FROM kcot-base

RUN ghc Item.API.hs -main-is ItemAPI -o Item.API
EXPOSE 8082
CMD ["/opt/server/Item.API"]

FROM kcot-base

RUN ghc Discount.API.hs -main-is DiscountAPI -o Discount.API
EXPOSE 8081
CMD ["/opt/server/Discount.API"]

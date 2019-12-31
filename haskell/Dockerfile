FROM haskell:8.6.5 AS kcot-base

WORKDIR /opt/server
RUN cabal update
COPY ./kata-checkout-order-total.cabal /opt/server/kata-checkout-order-total.cabal
COPY ./*.hs /opt/server/
COPY ./*.md /opt/server/
COPY ./LICENSE /opt/server/
RUN cabal install --only-dependencies

FROM kcot-base as item-api

COPY ./Items.json /opt/server/
RUN ghc Item.API.hs -main-is ItemAPI -o Item.API
EXPOSE 8082
CMD ["/opt/server/Item.API"]

FROM kcot-base as discount-api

COPY ./Discounts.json /opt/server/
RUN ghc Discount.API.hs -main-is DiscountAPI -o Discount.API
EXPOSE 8081
CMD ["/opt/server/Discount.API"]

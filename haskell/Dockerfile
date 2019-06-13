FROM haskell

RUN apt -y install zlib1g zlib1g-dev

WORKDIR /opt/server

COPY ./kata-checkout-order-total.cabal /opt/server/kata-checkout-order-total.cabal

RUN cabal update
RUN cabal install --only-dependencies

COPY . /opt/server

RUN ghc Item.API.hs -main-is ItemAPI -o Item.API

EXPOSE 8082

CMD ["Item.API"]


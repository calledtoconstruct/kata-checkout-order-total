FROM haskell

#RUN apt -y install zlib1g zlib1g-dev

WORKDIR /opt/server

RUN cabal update

COPY ./kata-checkout-order-total.cabal /opt/server/kata-checkout-order-total.cabal

RUN cabal install --only-dependencies

COPY . /opt/server

RUN ghc Item.API.hs -main-is ItemAPI -o Item.API

EXPOSE 8082

CMD ["/opt/server/Item.API"]


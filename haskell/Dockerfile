FROM haskell:8 AS kcot-base

WORKDIR /opt/server
RUN cabal update
COPY ./kata-checkout-order-total.cabal /opt/server/kata-checkout-order-total.cabal
RUN cabal v1-install --dependencies-only
COPY . /opt/server/

FROM kcot-base as item-api

EXPOSE 8082
RUN cabal v1-install
CMD ["item-api"]

FROM kcot-base as discount-api

EXPOSE 8081
RUN cabal v1-install
CMD ["discount-api"]

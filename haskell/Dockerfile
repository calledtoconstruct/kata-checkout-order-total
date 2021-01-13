FROM haskell:8 AS kcot-base

WORKDIR /opt/server
RUN cabal update
COPY . /opt/server/
#COPY ./kata-checkout-order-total.cabal /opt/server/kata-checkout-order-total.cabal
#COPY ./*.hs /opt/server/ --recursive
#COPY ./*.md /opt/server/
#COPY ./LICENSE /opt/server/
RUN cabal install

FROM kcot-base as item-api

#COPY ./Items.json /opt/server/
EXPOSE 8082
CMD ["item-api-app"]

FROM kcot-base as discount-api

#COPY ./Discounts.json /opt/server/
EXPOSE 8081
CMD ["discount-api-app"]

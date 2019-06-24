FROM node:alpine AS kcot-base

WORKDIR /usr/src/app
COPY package*.json ./
RUN npm install
RUN npm audit fix

COPY . .

FROM kcot-base
EXPOSE 8082
CMD [ "npm", "run", "host-item-api" ]

FROM kcot-base
EXPOSE 8081
CMD [ "npm", "run", "host-discount-api" ]

FROM node:alpine AS kcot-base

WORKDIR /usr/src/app
COPY package*.json ./
RUN npm install
RUN npm audit fix

COPY . .

FROM kcot-base
EXPOSE 8080
CMD [ "npm", "run", "host-application" ]

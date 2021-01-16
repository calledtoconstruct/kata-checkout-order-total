#!/bin/bash
docker ps -q --filter "name=item-api" | grep -q . && docker stop item-api
docker ps -aq --filter "name=item-api" | grep -q . && docker rm item-api
docker ps -q --filter "name=discount-api" | grep -q . && docker stop discount-api
docker ps -aq --filter "name=discount-api" | grep -q . && docker rm discount-api
docker network ls | grep -q . && docker network rm services
docker network create services
docker run --detach --network services --name item-api --publish 8082:8082 item-api:v1.0.0
docker run --detach --network services --name discount-api --publish 8081:8081 discount-api:v1.0.0
cabal run console-app

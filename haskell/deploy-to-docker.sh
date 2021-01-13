#!/bin/bash
sudo docker ps -q --filter "name=item-api" | grep -q . && sudo docker stop item-api
sudo docker ps -aq --filter "name=item-api" | grep -q . && sudo docker rm item-api
sudo docker ps -q --filter "name=discount-api" | grep -q . && sudo docker stop discount-api
sudo docker ps -aq --filter "name=discount-api" | grep -q . && sudo docker rm discount-api
sudo docker run --detach --network services --name item-api --publish 8082:8082 item-api:v1.0.0
sudo docker run --detach --network services --name discount-api --publish 8081:8081 discount-api:v1.0.0
cabal run console-app

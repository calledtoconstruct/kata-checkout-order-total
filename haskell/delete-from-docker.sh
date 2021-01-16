#!/bin/bash
docker stop item-api
docker rm item-api
docker stop discount-api
docker rm discount-api
docker network rm services


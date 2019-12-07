sudo docker network create -d bridge --subnet 172.25.0.0/16 isolated_network
sudo docker run --detach --network isolated_network --name item-api item-api:v1.0.0
sudo docker run --detach --network isolated_network --name discount-api discount-api:v1.0.0
sudo docker run --detach --network isolated_network --name application --publish 32780:8080 application:v1.0.0
sleep 2
curl http://localhost:32780/verify
sudo docker stop application
sudo docker rm application
sudo docker stop discount-api
sudo docker rm discount-api
sudo docker stop item-api
sudo docker rm item-api
sudo docker network rm isolated_network
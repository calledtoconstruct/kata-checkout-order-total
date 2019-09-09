dockenv="eval $(sudo minikube docker-env)"
dockdo="sudo docker --host="$DOCKER_HOST" --tlsverify=1 --tlscacert=\"$DOCKER_CERT_PATH/ca.pem\" --tlscert=\"$DOCKER_CERT_PATH/cert.pem\" --tlskey=\"$DOCKER_CERT_PATH/key.pem\""
$dockenv
$dockdo rmi item-api:v1.0.0
cat Dockerfile.base Dockerfile.item-api > Dockerfile
$dockdo build . --tag item-api:v1.0.0
$dockdo rmi discount-api:v1.0.0
cat Dockerfile.base Dockerfile.discount-api > Dockerfile
$dockdo build . --tag discount-api:v1.0.0
$dockdo rmi application:v1.0.0
cat Dockerfile.base Dockerfile.application > Dockerfile
$dockdo build . --tag application:v1.0.0

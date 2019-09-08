dockenv="eval $(sudo minikube docker-env)"
dockdo="sudo docker --host="$DOCKER_HOST" --tlsverify=1 --tlscacert=\"$DOCKER_CERT_PATH/ca.pem\" --tlscert=\"$DOCKER_CERT_PATH/cert.pem\" --tlskey=\"$DOCKER_CERT_PATH/key.pem\""
$dockenv
$dockdo rmi item-api:v1.0.0
$dockdo rmi discount-api:v1.0.0
$dockdo rmi application:v1.0.0
$dockdo build . --file item-api.Dockerfile --tag item-api:v1.0.0
$dockdo build . --file discount-api.Dockerfile --tag discount-api:v1.0.0
$dockdo build . --file application.Dockerfile --tag application:v1.0.0

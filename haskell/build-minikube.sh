dockenv="eval $(sudo minikube docker-env)"
dockdo="sudo docker --host="$DOCKER_HOST" --tlsverify=1 --tlscacert=\"$DOCKER_CERT_PATH/ca.pem\" --tlscert=\"$DOCKER_CERT_PATH/cert.pem\" --tlskey=\"$DOCKER_CERT_PATH/key.pem\""
$dockenv
$dockdo rmi item-api:v1.0.0
$dockdo rmi discount-api:v1.0.0
$dockdo build --tag item-api:v1.0.0 --target item-api .
$dockdo build --tag discount-api:v1.0.0 --target discount-api .
cat kubernetes.yaml | sed 's/image-prefix\///g' | linkerd inject - | minikube kubectl apply -- -f -

#cat kubernetes.yaml | sed 's/image-prefix\///g' | linkerd inject - | minikube kubectl apply -- -f -

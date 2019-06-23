dockenv="eval $(sudo minikube docker-env)"
dockdo="sudo docker --host="$DOCKER_HOST" --tlsverify=1 --tlscacert=\"$DOCKER_CERT_PATH/ca.pem\" --tlscert=\"$DOCKER_CERT_PATH/cert.pem\" --tlskey=\"$DOCKER_CERT_PATH/key.pem\""
$dockenv
$dockdo build .
image_name=$($dockdo images | grep -P "^(\<none\>\s*){2}([a-z0-9]*).*$" | tail -1)
echo $image_name
image_id=`expr match "$image_name" "^<none>\s*<none>\s*\([a-z0-9]*\).*$"`
echo $image_id
$dockdo tag $image_id item-api:v1.0.0
image_name=$($dockdo images | grep -P "^(\<none\>\s*){2}([a-z0-9]*).*$" | tail -1)
image_id=`expr match "$image_name" "^<none>\s*<none>\s*\([a-z0-9]*\).*$"`
$dockdo tag $image_id discount-api:v1.0.0

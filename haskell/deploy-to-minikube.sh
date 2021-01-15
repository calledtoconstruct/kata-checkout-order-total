# --------------------------------------
# Move minikube home to a writable drive
# --------------------------------------
# export MINIKUBE_HOME=/media/joseph/data/minikube

# --------------
# Start minikube
# --------------
# minikube start --driver=docker
# eval $(minikube -p minikube docker-env)

# --------------------------
# Build api container images
# --------------------------
# docker build --tag item-api:v1.0.0 --target item-api .
# docker build --tag discount-api:v1.0.0 --target discount-api .

# ----------------
# Install linkerd2
# ----------------
# sudo apt -y install curl
# curl -sL https://run.linkerd.io/install | sh
# export PATH=$PATH:/home/joseph/.linkerd2/bin
# --------------
# Start linkerd2
# --------------
# linkerd check --pre
# linkerd install | kubectl apply -f -
# linkerd check

# ----------------------------------
# Start apis and console application
# ----------------------------------
cat kubernetes.yaml | sed 's/image-prefix\///g' | linkerd inject - | kubectl apply -f -
kubectl port-forward service/item-api 8082 &
kubectl port-forward service/discount-api 8081 &
cabal run console-app

# minikube status
# linkerd check
# kubectl get pods

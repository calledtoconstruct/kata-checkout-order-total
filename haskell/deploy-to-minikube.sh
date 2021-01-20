# --------------------------------------
# Move minikube home to a writable drive
# --------------------------------------
# export MINIKUBE_HOME=/media/joseph/data/minikube

# Configure Docker to run off external drive
# Create /etc/docker/daemon.json with:
# {
#   "data-root": "/media/username/data/docker",
#   "storage-driver": "overlay2"
# }
# Copy the docker folder to the external drive:
# sudo cp /var/lib/docker /media/username/data/docker --recursive
# Also, execute the following:
# sudo mount -n -o remount,suid /media/username/data/

# --------------
# Start minikube
# --------------
# minikube start --driver=docker --disk-size=20GB
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
sudo minikube tunnel &
cabal run console -- --item-api $(minikube service --url item-api) --discount-api $(minikube service --url discount-api)

# minikube status
# linkerd check
# kubectl get pods

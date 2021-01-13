if [ "$(gcloud config get-value core/account 2>&1 >/dev/tty)" = "(unset)" ]; then
  echo "Please sign into your google cloud account using:"
  echo "$ gcloud init"
else
  echo "Starting kubernetes cluster on Google Cloud"
  export PATH=$PATH:$HOME/.linkerd2/bin
  gcloud beta container clusters create "default" --project "$(gcloud config get-value core/project)" --zone "$(gcloud config get-value compute/zone)" --no-enable-basic-auth --release-channel "regular" --machine-type "e2-small" --image-type "COS" --disk-type "pd-standard" --disk-size "100" --metadata disable-legacy-endpoints=true --scopes "https://www.googleapis.com/auth/devstorage.read_only","https://www.googleapis.com/auth/logging.write","https://www.googleapis.com/auth/monitoring","https://www.googleapis.com/auth/servicecontrol","https://www.googleapis.com/auth/service.management.readonly","https://www.googleapis.com/auth/trace.append" --max-pods-per-node "50" --num-nodes "2" --enable-stackdriver-kubernetes --enable-ip-alias --network "projects/$(gcloud config get-value core/project)/global/networks/default" --subnetwork "projects/$(gcloud config get-value core/project)/regions/$(gcloud config get-value compute/region)/subnetworks/default" --default-max-pods-per-node "50" --enable-autoscaling --min-nodes "2" --max-nodes "6" --addons HorizontalPodAutoscaling,HttpLoadBalancing --enable-autoupgrade --enable-autorepair
  gcloud beta container node-pools create "application" --project "$(gcloud config get-value core/project)" --cluster "default" --zone "$(gcloud config get-value compute/zone)" --machine-type "e2-micro" --image-type "COS" --disk-type "pd-standard" --disk-size "100" --metadata disable-legacy-endpoints=true --scopes "https://www.googleapis.com/auth/devstorage.read_only","https://www.googleapis.com/auth/logging.write","https://www.googleapis.com/auth/monitoring","https://www.googleapis.com/auth/servicecontrol","https://www.googleapis.com/auth/service.management.readonly","https://www.googleapis.com/auth/trace.append" --max-pods-per-node "8" --num-nodes "2" --enable-autoscaling --min-nodes "2" --max-nodes "6" --enable-autoupgrade --enable-autorepair
  echo "Deploying linkerd2 Service Mesh"
  linkerd install | sed 's/\([[:space:]]*\)nodeSelector:/\1nodeSelector:\n\1  cloud.google.com\/gke-nodepool: default-pool/g' | kubectl apply -f -
  linkerd check
  echo "Deploying application components"
  sudo docker build --tag us.gcr.io/$(gcloud config get-value core/project)/item-api:v1.0.0 --target item-api .
  sudo docker push us.gcr.io/$(gcloud config get-value core/project)/item-api:v1.0.0
  sudo docker build --tag us.gcr.io/$(gcloud config get-value core/project)/discount-api:v1.0.0 --target discount-api .
  sudo docker push us.gcr.io/$(gcloud config get-value core/project)/discount-api:v1.0.0
  cat kubernetes.yaml | sed 's/image-prefix\//us.gcr.io\/$(gcloud config get-value core\/project)\//g' | sed 's/\([[:space:]]*\)nodeSelector:/\1nodeSelector:\n\1  cloud.google.com\/gke-nodepool: application/g' | linkerd inject - | kubectl apply -f -
  echo "Deployment complete"
fi

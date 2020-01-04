export PATH=$PATH:$HOME/.linkerd2/bin
gcloud beta container clusters create "default" --project "enlightenment-today" --zone "us-central1-a" --no-enable-basic-auth --release-channel "regular" --machine-type "e2-small" --image-type "COS" --disk-type "pd-standard" --disk-size "100" --metadata disable-legacy-endpoints=true --scopes "https://www.googleapis.com/auth/devstorage.read_only","https://www.googleapis.com/auth/logging.write","https://www.googleapis.com/auth/monitoring","https://www.googleapis.com/auth/servicecontrol","https://www.googleapis.com/auth/service.management.readonly","https://www.googleapis.com/auth/trace.append" --max-pods-per-node "50" --num-nodes "2" --enable-stackdriver-kubernetes --enable-ip-alias --network "projects/enlightenment-today/global/networks/default" --subnetwork "projects/enlightenment-today/regions/us-central1/subnetworks/default" --default-max-pods-per-node "50" --enable-autoscaling --min-nodes "2" --max-nodes "6" --addons HorizontalPodAutoscaling,HttpLoadBalancing --enable-autoupgrade --enable-autorepair
gcloud beta container node-pools create "application" --project "enlightenment-today" --cluster "default" --zone "us-central1-a" --machine-type "e2-micro" --image-type "COS" --disk-type "pd-standard" --disk-size "100" --metadata disable-legacy-endpoints=true --scopes "https://www.googleapis.com/auth/devstorage.read_only","https://www.googleapis.com/auth/logging.write","https://www.googleapis.com/auth/monitoring","https://www.googleapis.com/auth/servicecontrol","https://www.googleapis.com/auth/service.management.readonly","https://www.googleapis.com/auth/trace.append" --max-pods-per-node "8" --num-nodes "2" --enable-autoscaling --min-nodes "2" --max-nodes "6" --enable-autoupgrade --enable-autorepair
linkerd install | sed 's/\([[:space:]]*\)nodeSelector:/\1nodeSelector:\n\1  cloud.google.com\/gke-nodepool: default-pool/g' | kubectl apply -f -
linkerd check
docker build --tag us.gcr.io/enlightenment-today/item-api:v1.0.0 --target item-api .
docker push us.gcr.io/enlightenment-today/item-api:v1.0.0
docker build --tag us.gcr.io/enlightenment-today/discount-api:v1.0.0 --target discount-api .
docker push us.gcr.io/enlightenment-today/discount-api:v1.0.0
cat kubernetes.yaml | sed 's/image-prefix\//us.gcr.io\/enlightenment-today\//g' | sed 's/\([[:space:]]*\)nodeSelector:/\1nodeSelector:\n\1  cloud.google.com\/gke-nodepool: application/g' | linkerd inject - | kubectl apply -f -

#cat kubernetes.yaml | linkerd inject - | kubectl delete -f -
#linkerd install --ignore-cluster | kubectl delete -f -
#gcloud beta container --project "enlightenment-today" clusters delete "default" --zone "us-central1-a"

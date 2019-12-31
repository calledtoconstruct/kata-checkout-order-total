docker build --tag us.gcr.io/enlightenment-today/item-api:v1.0.0 --target item-api .
docker build --tag us.gcr.io/enlightenment-today/discount-api:v1.0.0 --target discount-api .
#docker tag item-api:v1.0.0 us.gcr.io/project/item-api:v1.0.0
#docker tag discount-api:v1.0.0 us.gcr.io/project/discount-api:v1.0.0
docker push us.gcr.io/enlightenment-today/item-api:v1.0.0
docker push us.gcr.io/enlightenment-today/discount-api:v1.0.0

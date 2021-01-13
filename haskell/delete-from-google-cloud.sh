if [ "$(gcloud config get-value core/account 2>&1 >/dev/tty)" = "(unset)" ]; then
  echo "Please sign into your google cloud account using:"
  echo "$ gcloud init"
else
  echo "Deleting 'default' cluster"
  gcloud container clusters delete "default" --quiet
  echo "Cluster deleted"
fi

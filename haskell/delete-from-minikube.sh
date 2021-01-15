# -------------
# Shutdown apis
# -------------
cat kubernetes.yaml | sed 's/image-prefix\///g' | linkerd inject - | kubectl delete -f -

# -----------------
# Shutdown linkerd2
# -----------------
linkerd uninstall | kubectl delete -f -

# -----------------------
# Delete minikube cluster
# -----------------------
minikube delete

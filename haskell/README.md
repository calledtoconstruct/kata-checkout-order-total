# kata-checkout-order-total :: Haskell #
##### The checkout order total solution written in Haskell #####

### Interoperability ###

This version is designed to seamlessly interoperate with the Typescript version.

### Building ###

To build all packages of the solution, execute the following:

```bash
$ cabal build all
```

To run the unit test suite, execute the following:

```bash
$ cabal test all
```

When using the vscode devcontainer, for the haskell language server plugin to work correctly, you mean need to first execute:

```bash
$ cabal update && cabal build all
```

(prior to opening a .hs file in the editor)

### Notes on using Docker ###

The scripts that accompany the code are intended to be executed by a user account
that is already granted permission to run Docker.  In other words, *sudo* is
not prepended to the docker commands in the script.  To grant your user account 
access to run docker commands without *sudo*, use the following:

```bash
$ sudo usermod -aG docker $USER
$ newgrp docker
```

The following command should show *docker* in the list of groups:

```bash
$ id
```

Note: This will only add the docker group to the user for the current shell (terminal).  
The user must logout and login again to cause the group membership to apply globally.

### Notes on using Minikube ###

There are notes included in *deploy-to-minikube.sh* for running on minikube in addition
to the commands to start the application components.  Those notes suggest running minikube
using the Docker driver.  Keep in mind, this creates a dual-layer of docker usage.  Before
minikube is started, docker will be running on the local machine.  Once minikube starts, 
environment variables are set to instruct docker to write images inside the cluster.  If
you attempt to start the application components and they fail with image pull errors or
image pull backoff, then delete the deployment, run the command to set the environment 
variables, and recreate the deployment.

At this time, it is not possible (or, better said, feasible) to detect whether minikube
is running or to script the minikube start-up.  The minikube project could assist with
that by providing commands that output simple status messages (ex. minikube status --up
could write True or False to the output).  In the meantime, I tried to document the
steps for manual start-up.

### Kubernetes on Google Cloud ###

The script *deploy-to-google-cloud.sh* is more robust in that it will detect whether
you are not logged in and provide the command to do so.  It assumes that the cluster
does not exist.

The cluster will be created with the minimum resources necessary to run the service 
mesh and solution components.  This configuration is not high performance as the goal
was to minimize cost.  You may find other configurations to be more efficient or to
provide higher performance when that is your goal.


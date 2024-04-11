FROM haskell:9.8.2
# FROM debian:bookworm

# This Dockerfile adds a non-root user with sudo access. Use the "remoteUser"
# property in devcontainer.json to use it. On Linux, the container user's GID/UIDs
# will be updated to match your local UID/GID (when using the dockerFile property).
# See https://aka.ms/vscode-remote/containers/non-root-user for details.
ARG USERNAME=vscode
ARG USER_UID=1000
ARG USER_GID=$USER_UID

ARG GHCUP_VERSION=0.1.9
ARG PROC_ARCH=x86_64

# Set to false to skip installing zsh and Oh My ZSH!
ARG INSTALL_ZSH="false"

# Location and expected SHA for common setup script - SHA generated on release
ARG COMMON_SCRIPT_SOURCE="https://raw.githubusercontent.com/microsoft/vscode-dev-containers/master/script-library/common-debian.sh"
ARG COMMON_SCRIPT_SHA="dev-mode"

# Configure apt and install packages
RUN apt update \
    && apt --yes install wget coreutils ca-certificates   build-essential  \
    && wget -q -O /tmp/common-setup.sh $COMMON_SCRIPT_SOURCE \
    && if [ "$COMMON_SCRIPT_SHA" != "dev-mode" ]; then echo "$COMMON_SCRIPT_SHA /tmp/common-setup.sh" | sha256sum -c - ; fi \
    && /bin/bash /tmp/common-setup.sh "$INSTALL_ZSH" "$USERNAME" "$USER_UID" "$USER_GID" \
    && rm /tmp/common-setup.sh

USER $USERNAME

# sudo apt --yes install build-essential libgmp3-dev zlib1g-dev libtinfo-dev libtinfo5 

RUN mkdir $HOME/.ghcup \
    && mkdir $HOME/.ghcup/bin \
    && wget -q -O $HOME/.ghcup/bin/ghcup https://downloads.haskell.org/~ghcup/$GHCUP_VERSION/$PROC_ARCH-linux-ghcup-$GHCUP_VERSION \
    && chmod +x $HOME/.ghcup/bin/ghcup \
    && export PATH=$HOME/.ghcup/bin:$PATH \
    && ghcup upgrade \
    && ghcup upgrade \
    && ghcup upgrade \
    && ghcup install cabal 3.10.3.0 \
    && ghcup install hls 2.7.0.0 \
    && ghcup install ghc 9.8.2 \
    && ghcup install stack 2.15.5 \
    && ghcup set ghc 9.8.2 

# RUN cabal update \
#     && cabal build --dependencies-only

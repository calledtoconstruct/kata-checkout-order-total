
sudo apt install cabal-install
cabal update
sudo apt install libghc-zlib-dev
cabal install --dependencies-only --allow-newer=base

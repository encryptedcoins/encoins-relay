#!/bin/bash

cabal build exe:encoins-ipfs

cp /home/metaxis/sources/organisations/encoins/encoins-relay/dist-newstyle/build/x86_64-linux/ghc-8.10.7/encoins-relay-apps-0.1.0.0/build/encoins-ipfs/encoins-ipfs ~/.local/bin/encoins-ipfs
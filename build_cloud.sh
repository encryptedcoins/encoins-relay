#!/bin/bash

cabal build exe:encoins-cloud
cabal build exe:encoins-cloud-clean

cp dist-newstyle/build/x86_64-linux/ghc-8.10.7/encoins-relay-apps-0.1.0.0/build/encoins-cloud/encoins-cloud ~/.local/bin/encoins-cloud
cp dist-newstyle/build/x86_64-linux/ghc-8.10.7/encoins-relay-apps-0.1.0.0/build/encoins-cloud-clean/encoins-cloud-clean ~/.local/bin/encoins-cloud-clean
#!/bin/bash

CLOUD_VERSION=$(./scripts/get_project_version.sh "encoins-relay-cloud/encoins-relay-cloud.cabal")

cabal build exe:encoins-cloud
cabal build exe:encoins-cloud-clean

cp dist-newstyle/build/x86_64-linux/ghc-9.6.3/encoins-relay-cloud-"$CLOUD_VERSION"/build/encoins-cloud/encoins-cloud ~/.local/bin/encoins-cloud
cp dist-newstyle/build/x86_64-linux/ghc-9.6.3/encoins-relay-cloud-"$CLOUD_VERSION"/build/encoins-cloud-clean/encoins-cloud-clean ~/.local/bin/encoins-cloud-clean
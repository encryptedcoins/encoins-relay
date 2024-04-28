#!/bin/bash

APPS_VERSION=$(./scripts/get_project_version.sh "encoins-relay-apps/encoins-relay-apps.cabal")

cabal build exe:encoins-cloud
cabal build exe:encoins-cloud-clean

cp dist-newstyle/build/x86_64-linux/ghc-8.10.7/encoins-relay-apps-"$APPS_VERSION"/build/encoins-cloud/encoins-cloud ~/.local/bin/encoins-cloud
cp dist-newstyle/build/x86_64-linux/ghc-8.10.7/encoins-relay-apps-"$APPS_VERSION"/build/encoins-cloud-clean/encoins-cloud-clean ~/.local/bin/encoins-cloud-clean
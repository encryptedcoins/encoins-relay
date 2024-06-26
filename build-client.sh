#!/bin/bash

cabal new-build exe:encoins-relay-client

CLIENT_VERSION=$(./scripts/get_project_version.sh "encoins-relay-client/encoins-relay-client.cabal")
cp dist-newstyle/build/x86_64-linux/ghc-9.6.3/encoins-relay-client-"$CLIENT_VERSION"/x/encoins-relay-client/build/encoins-relay-client/encoins-relay-client ~/.local/bin/encoins-client

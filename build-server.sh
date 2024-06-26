#!/bin/bash

cabal new-build exe:encoins-relay-server

SERVER_VERSION=$(./scripts/get_project_version.sh "encoins-relay-server/encoins-relay-server.cabal")
cp dist-newstyle/build/x86_64-linux/ghc-9.6.3/encoins-relay-server-"$SERVER_VERSION"/build/encoins-relay-server/encoins-relay-server ~/.local/bin/encoins
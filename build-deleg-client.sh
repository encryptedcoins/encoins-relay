#!/bin/bash

DELEGATION_VERSION=$(./scripts/get_project_version.sh "encoins-relay-delegation/encoins-relay-delegation.cabal")
cabal new-build exe:encoins-relay-delegation-client

cp dist-newstyle/build/x86_64-linux/ghc-9.6.3/encoins-relay-delegation-"$DELEGATION_VERSION"/build/encoins-relay-delegation-client/encoins-relay-delegation-client ~/.local/bin/encoins-delegation-client
#!/bin/bash

cabal new-build exe:encoins-relay-verifier

VERIFIER_VERSION=$(./scripts/get_project_version.sh "encoins-relay-verifier/encoins-relay-verifier.cabal")
cp dist-newstyle/build/x86_64-linux/ghc-9.6.3/encoins-relay-verifier-"$VERIFIER_VERSION"/build/encoins-relay-verifier/encoins-relay-verifier ~/.local/bin/encoins-verifier
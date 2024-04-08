#!/bin/bash

cabal new-build all

SERVER_VERSION=$(./scripts/get_project_version.sh "encoins-relay-server/encoins-relay-server.cabal")
cp dist-newstyle/build/x86_64-linux/ghc-8.10.7/encoins-relay-server-"$SERVER_VERSION"/build/encoins-relay-server-lightweight/encoins-relay-server-lightweight ~/.local/bin/encoins
cp dist-newstyle/build/x86_64-linux/ghc-8.10.7/encoins-relay-server-"$SERVER_VERSION"/build/encoins-relay-server-full/encoins-relay-server-full ~/.local/bin/encoins-full

VERIFIER_VERSION=$(./scripts/get_project_version.sh "encoins-relay-verifier/encoins-relay-verifier.cabal")
cp dist-newstyle/build/x86_64-linux/ghc-8.10.7/encoins-relay-verifier-"$VERIFIER_VERSION"/build/encoins-relay-verifier/encoins-relay-verifier ~/.local/bin/encoins-verifier

CLIENT_VERSION=$(./scripts/get_project_version.sh "encoins-relay-client/encoins-relay-client.cabal")
cp dist-newstyle/build/x86_64-linux/ghc-8.10.7/encoins-relay-client-"$CLIENT_VERSION"/x/encoins-relay-client/build/encoins-relay-client/encoins-relay-client ~/.local/bin/encoins-client

APPS_VERSION=$(./scripts/get_project_version.sh "encoins-relay-apps/encoins-relay-apps.cabal")
cp dist-newstyle/build/x86_64-linux/ghc-8.10.7/encoins-relay-apps-"$APPS_VERSION"/build/encoins-relay-poll/encoins-relay-poll ~/.local/bin/encoins-poll

cp dist-newstyle/build/x86_64-linux/ghc-8.10.7/encoins-relay-apps-"$APPS_VERSION"/build/encoins-relay-delegation/encoins-relay-delegation ~/.local/bin/encoins-delegation

cp dist-newstyle/build/x86_64-linux/ghc-8.10.7/encoins-relay-apps-"$APPS_VERSION"/build/encoins-relay-delegation-client/encoins-relay-delegation-client ~/.local/bin/encoins-delegation-client

cp dist-newstyle/build/x86_64-linux/ghc-8.10.7/encoins-relay-apps-"$APPS_VERSION"/build/encoins-ipfs/encoins-ipfs ~/.local/bin/encoins-ipfs

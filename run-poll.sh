#!/bin/bash

# run mainnet encoins-relay-poll
POLL_NUM="$1"

./scripts/checkout_last_release.sh
cabal build encoins-relay-apps:exe:encoins-relay-poll
APPS_VERSION=$(./scripts/get_project_version.sh "encoins-relay-apps/encoins-relay-apps.cabal")
cp dist-newstyle/build/x86_64-linux/ghc-8.10.7/encoins-relay-apps-"$APPS_VERSION"/build/encoins-relay-poll/encoins-relay-poll ~/.local/bin/encoins-poll

cd "mainnet/apps/encoins" || exit

encoins-poll "$POLL_NUM"
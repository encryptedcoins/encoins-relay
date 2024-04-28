#!/bin/bash

# run mainnet encoins-relay-server

./scripts/checkout_last_release.sh
./build-server.sh

cd "mainnet/apps/encoins" || exit

encoins
#!/bin/bash

# run mainnet encoins-relay-delegation
./scripts/checkout_last_release.sh
./build-deleg.sh

cd "mainnet/apps/encoins" || exit

encoins-delegation
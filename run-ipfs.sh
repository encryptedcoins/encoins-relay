#!/bin/bash

# run mainnet encoins-relay-ipfs
./scripts/checkout_last_release.sh
./build-ipfs.sh

cd "mainnet/apps/encoins" || exit

encoins-ipfs
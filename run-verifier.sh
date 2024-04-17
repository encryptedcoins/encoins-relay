#!/bin/bash

# run mainnet encoins-relay-verifier

./scripts/checkout_last_release.sh
./build-verifier.sh

cd "mainnet/apps/verifier" || exit

encoins-verifier
#!/bin/bash

# run mainnet encoins-relay-verifier

./scripts/checkout_last_release.sh

cd "mainnet/apps/verifier" || exit

encoins-verifier
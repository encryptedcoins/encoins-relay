#!/bin/bash

# run mainnet encoins-relay-poll
POLL_NUM="$1"

./scripts/checkout_last_release.sh

cd "mainnet/apps/encoins" || exit

encoins-poll "$POLL_NUM"
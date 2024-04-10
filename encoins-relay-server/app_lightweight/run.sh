#!/bin/bash

cd ../.. || exit

./scripts/checkout_last_release.sh

cd "encoins-relay-server/app_lightweight/mainnet" || exit

encoins
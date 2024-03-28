#!/bin/bash

node_metrics_port=12798
kupo_port=1442
relay_staus_addr="{$1}/status"

connErr="ConnectionError"
check_service()
{
    if [[ $1 == "$connErr" ]]; then
        echo "$2 is offline."
    else
        echo "$2 is ok."
    fi
}

node_check=$(curl "localhost:${node_metrics_port}/metrics") || node_check=$connErr
kupo_check=$(curl "localhost:${kupo_port}/health") || kupo_check=$connErr
wallet_check=$(cardano-wallet network information) || wallet_check=$connErr
relay_check=$(curl "$relay_staus_addr" -X POST -H "Content-Type: application/json" --data '"MaxAdaWithdraw"')

check_service "$node_check" "Cardano-node"
check_service "$kupo_check" "Kupo"
check_service "$wallet_check" "Cardano-wallet"

if [[ $relay_check  == "{\"contents"* ]]; then
    echo "Relay is ok."
else
    echo "Relay repsonse to MaxAdaWithdraw status reuest:"
    echo "$relay_check"
fi
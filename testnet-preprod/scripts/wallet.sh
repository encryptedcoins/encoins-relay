#!/bin/bash

cardano-wallet serve --testnet ../apps/cardano-node/byron-genesis.json --node-socket ../apps/cardano-node/node.sock --database ../data/wallet

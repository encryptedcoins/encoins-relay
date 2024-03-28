#!/bin/bash
export CARDANO_NODE_SOCKET_PATH=node.sock

cardano-cli query protocol-parameters --mainnet --out-file protocol-parameters.json

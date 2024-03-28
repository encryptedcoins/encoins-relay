#!/bin/bash

cardano-node run \
    --config        ../apps/cardano-node/config.json \
    --topology      ../apps/cardano-node/topology.json \
    --database-path ../data/chain \
    --socket-path   ../apps/cardano-node/node.sock \
    --port          3003

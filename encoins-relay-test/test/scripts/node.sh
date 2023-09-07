#!/bin/bash

cardano-node run \
    --config        ../configuration/node/config.json \
    --topology      ../configuration/node/topology.json \
    --database-path "$1" \
    --socket-path   "$2" \
    --port          3003
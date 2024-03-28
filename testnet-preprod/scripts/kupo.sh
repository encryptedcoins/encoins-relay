#!/bin/bash

kupo \
  --node-socket ../apps/cardano-node/node.sock \
  --node-config ../apps/cardano-node/config.json \
  --since origin \
  --match "*" \
  --workdir ../data

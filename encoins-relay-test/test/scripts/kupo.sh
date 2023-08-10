#!/bin/bash

kupo \
  --node-socket="$1" \
  --node-config=../configuration/node/config.json \
  --since=origin \
  --match="*" \
  --workdir="$2"
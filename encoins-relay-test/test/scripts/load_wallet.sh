#!/bin/bash

curl -H "content-type: application/json" -XPOST -d @"$1" localhost:8090/v2/wallets
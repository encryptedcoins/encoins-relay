#!/bin/bash

new_string="$1"

if [ -z "$1" ]; then
    echo "Error: The argument must be non-empty."
    exit 1
fi

aws apigatewayv2 delete-api --api-id $1

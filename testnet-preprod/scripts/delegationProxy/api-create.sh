#!/bin/bash

old_string="<HTTP_ADDRESS>"
new_string="$1"

if [ -z "$new_string" ]; then
    echo "Error: The argument must be non-empty."
    exit 1
fi

sed -i s@${old_string}@${new_string}@g api.json

aws apigatewayv2 import-api --body file://api.json > result.json

sed -i s@${new_string}@${old_string}@g api.json

api_id=$(jq -r '.ApiId' result.json)

aws apigatewayv2 create-stage --api-id $api_id --auto-deploy --stage-name '$default' > /dev/null

echo $api_id

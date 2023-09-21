#!/bin/bash

unset GTK_PATH

NODE_SOCK_PATH=$(jq -r '.node_file_path' ../configuration/config.json)
NODE_DB_PATH=$(jq -r '.node_db_file_path' ../configuration/config.json)
KUPO_DB_PATH=$(jq -r '.kupo_db_file_path' ../configuration/config.json)
WALLET_PATH=$(jq -r '.wallet_file' ../configuration/config.json)

# Run the scripts
gnome-terminal --tab --title="cardano-node" -- bash -c "./node.sh $NODE_DB_PATH $NODE_SOCK_PATH;$SHELL"
gnome-terminal --tab --title="cardano-wallet" -- bash -c "./wallet.sh $NODE_SOCK_PATH;$SHELL"
gnome-terminal --tab --title="kupo" -- bash -c "./kupo.sh $NODE_SOCK_PATH $KUPO_DB_PATH;$SHELL"


cd ../../..

# Create the wallet file if it doesn't exist
DEFAULT_WALLET="{\"name\":\"test-wallet\",\"mnemonic_sentence\":[\"cry\",\"risk\",\"kangaroo\",\"setup\",\"thought\",\"asset\",\"blame\",\"spot\",\"chunk\",\"force\",\"beauty\",\"laptop\",\"silk\",\"chief\",\"jewel\",\"want\",\"ranch\",\"awkward\",\"venue\",\"warfare\",\"pill\",\"usage\",\"case\",\"foster\"],\"passphrase\":\"1234567890\"}"
DEFAULT_WALLET_PATH="encoins-relay-test/test/configuration/wallets/test-wallet.json"
if [ ! -f "$WALLET_PATH" ]; then
   echo "$DEFAULT_WALLET" > $DEFAULT_WALLET_PATH
   WALLET_PATH=$DEFAULT_WALLET_PATH
fi

# Load the wallet
sleep 10
sh ./encoins-relay-test/test/scripts/load_wallet.sh "$WALLET_PATH"
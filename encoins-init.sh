#!/bin/bash

# Get latest tag name
latestTag=$(git ls-remote --tags https://github.com/encryptedcoins/encoins-relay | sort -t '/' -k 3 -V | awk -F/ '{ print $3 }' | awk '!/\^\{\}/' | tail -n 1)

# Download binaries
wget "https://github.com/encryptedcoins/encoins-relay/releases/download/${latestTag}/encoins"
chmod +x encoins
cp encoins ~/.local/bin/encoins

# Download and extract configs
git clone https://github.com/encryptedcoins/encoins-relay
cd encoins-relay || exit

if [[ "${latestTag}" == "v1.2.5.1" ]]
then
    git checkout Encoins-tools-merge
else
    git checkout "${latestTag}"
fi

mv  -v  mainnet ../
cd .. || exit

# Remove trash
rm encoins
rm -r -f encoins-relay
rm -r -f mainnet/apps/verifier
rm -r -f mainnet/apps/encoins/schema
rm mainnet/apps/encoins/cloud_config.json
rm mainnet/apps/encoins/delegConfig.json
rm mainnet/apps/encoins/distribution.json
rm mainnet/apps/encoins/poll*onfig.json

# Run app
cd mainnet/apps/encoins || exit
encoins
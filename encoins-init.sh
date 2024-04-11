#!/bin/bash

# Get new tags from remote
git fetch --tags

# Get latest tag name
GITHUB_TAG=$(git describe --tags "$(git rev-list --tags --max-count=1)")

# Download binaries
wget "https://github.com/encryptedcoins/encoins-relay/releases/download/${GITHUB_TAG}/encoins"
chmod +x encoins
cp encoins ~/.local/bin/encoins

# Download and extract configs
git clone https://github.com/encryptedcoins/encoins-relay
cd encoins-relay || exit

if [[ "${GITHUB_TAG}" == "v1.2.5.1" ]]
then
    git checkout 46608dd67ce79597c2ae4c110cb8411af2d8f7cf
else
    git checkout "${GITHUB_TAG}"
fi

ls

mv  -v  encoins-relay-server/app_lightweight/mainnet/* ../
cd .. || exit

ls

# Remove trash
rm encoins
rm -r -f encoins-relay

# Run app
encoins
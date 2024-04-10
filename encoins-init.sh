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
echo "https://github.com/encryptedcoins/encoins-relay/archive/${GITHUB_TAG}.tar.gz"
wget "https://github.com/encryptedcoins/encoins-relay/archive/${GITHUB_TAG}.tar.gz"
tar -xf "${GITHUB_TAG}.tar.gz"

# Run app
cd "encoins-relay-${GITHUB_TAG}/encoins-relay-server/app_lightweight" || exit
./run.sh

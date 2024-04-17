#!/bin/bash

# Get new tags from remote
git fetch --tags

# Get latest tag name
latestTag=$(git describe --tags "$(git rev-list --tags --max-count=1)")

# Checkout latest tag
if [[ "${latestTag}" == "v1.2.5.1" ]]
then
    git checkout f934a80c8fcda657bdb259f0a5d72a15ebe90365
else
    git checkout "${latestTag}"
fi
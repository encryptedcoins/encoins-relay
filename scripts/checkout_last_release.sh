#!/bin/bash

# Get new tags from remote
git fetch --tags

# Get latest tag name
latestTag=$(git describe --tags "$(git rev-list --tags --max-count=1)")

# Checkout latest tag
if [[ "${latestTag}" == "v1.2.5.1" ]]
then
    git checkout a13656c9107ffcbacd161f48762c26a30fc9efc3
else
    git checkout "${latestTag}"
fi
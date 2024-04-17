#!/bin/bash

# Get new tags from remote
git fetch --tags

# Get latest tag name
latestTag=$(git describe --tags "$(git rev-list --tags --max-count=1)")

# Checkout latest tag
if [[ "${latestTag}" == "v1.2.5.1" ]]
then
    git checkout 77aa006c07b18f4794cf9ec34fef1f8edbfbe51a
else
    git checkout "${latestTag}"
fi
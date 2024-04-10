#!/bin/bash

# Get new tags from remote
git fetch --tags

# Get latest tag name
latestTag=$(git describe --tags "$(git rev-list --tags --max-count=1)")

# Checkout latest tag
git checkout "$latestTag"
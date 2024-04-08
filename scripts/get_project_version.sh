#!/bin/bash

# Specify the path to the .cabal file
CABAL_FILE="$1"

# Get the version number from the .cabal file
VERSION=$(grep -E '^version:' "$CABAL_FILE" | awk '{print $2}')

echo "$VERSION"
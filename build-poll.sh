#!/bin/bash

POLL_VERSION=$(./scripts/get_project_version.sh "encoins-relay-poll/encoins-relay-poll.cabal")
cabal new-build exe:encoins-relay-poll

cp dist-newstyle/build/x86_64-linux/ghc-8.10.7/encoins-relay-poll-"$POLL_VERSION"/build/encoins-relay-poll/encoins-relay-poll ~/.local/bin/encoins-poll

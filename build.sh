#!/bin/bash

cabal new-build
cp dist-newstyle/build/x86_64-linux/ghc-8.10.7/encoins-relay-0.1.0.0/x/encoinsRelayServer/build/encoinsRelayServer/encoinsRelayServer ~/.local/bin/encoins-relay

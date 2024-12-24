cabal update

cd dist-newstyle/src/dhall-has_-8284154889994081 

git submodule sync

git submodule init

git submodule update --recursive

cabal update 

cabal install
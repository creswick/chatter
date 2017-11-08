#!/bin/bash
# 'unofficial bash strict mode'
set -euo pipefail
IFS=$'\n\t'

# Path settings for jenkins
export PATH=${HOME}/.cabal/bin:${HOME}/tools/ghc/ghc-7.6.3/bin:${HOME}/android-sdk-linux/platform-tools/:$PATH

cabal sandbox init
cabal clean
cabal install --enable-tests --only-dependencies
cabal configure --enable-tests
cabal build

echo "Running benchmarks"
./dist/build/bench/bench -o bench.html


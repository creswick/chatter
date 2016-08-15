#!/bin/bash
# exit on /any/ error:
set -e

# Path settings for jenkins
export PATH=${HOME}/.cabal/bin:${HOME}/tools/ghc/ghc-7.6.3/bin:${HOME}/android-sdk-linux/platform-tools/:$PATH

cabal sandbox delete || true
cabal sandbox init
cabal clean
cabal install --enable-tests --only-dependencies
cabal configure --enable-tests
cabal build
cabal install || true

echo "Running tests"
./dist/build/tests/tests --xml=./dist/test-results.xml || true

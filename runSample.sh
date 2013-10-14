#!/bin/bash
# exit on /any/ error:
set -e

# Path settings for jenkins
export PATH=${HOME}/.cabal/bin:${HOME}/tools/ghc/ghc-7.6.3/bin:${HOME}/android-sdk-linux/platform-tools/:$PATH

cabal sandbox init
cabal clean
cabal install --enable-tests --only-dependencies --enable-library-profiling --enable-executable-profiling
cabal configure --enable-tests --enable-library-profiling --enable-executable-profiling
cabal build

echo "Running sample"
./dist/build/hpostag/hpostag +RTS -hy -p -K64M -RTS
hp2ps -c hpostag.hp


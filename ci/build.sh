#!/usr/bin/env bash

set -e

STACK="stack --no-terminal --jobs=1"

# Install dependencies, aborting if it takes too long
ret=0
$TIMEOUT 40m $STACK --install-ghc build --only-dependencies --test --haddock || ret=$?
case "$ret" in
0) # All good
    ;;
124)
    echo "Timed out while installing dependencies."
    echo "Try pushing a new commit to build again."
    exit 1
    ;;
*)
    echo "Failed to install dependencies."
    exit 1
    ;;
esac

STACK_EXTRA_FLAGS=""
if [ -z "$TRAVIS_TAG" ]; then
    # On non-release builds, disable optimizations
    STACK_EXTRA_FLAGS="--fast"
else
    STACK_EXTRA_FLAGS="--flag elm-smuggle:release"
fi

BUILD_COMMAND="$STACK build --pedantic --test $STACK_EXTRA_FLAGS"
echo "$BUILD_COMMAND"
$BUILD_COMMAND

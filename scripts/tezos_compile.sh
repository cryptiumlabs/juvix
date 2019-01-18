#!/bin/bash

set -e

ARGS=$@
mkdir -p tmp
cd tmp
find . -type f -name "*.ibc" | xargs -n 1 -I % rm %
cd ..

set -x

stack exec -- idris --interface --noprelude -p tezos --ibcsubdir tmp --codegen juvix $ARGS

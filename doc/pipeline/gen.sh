#!/bin/sh

set -xe

TMP=$(mktemp).dot
node index.js > $TMP
dot -Tpng $TMP -o output.png
rm -f $TMP

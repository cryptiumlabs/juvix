#!/bin/bash

set -e

files=$(find ./examples -type f -name "*.idr" | sort)
count=$(echo "$files" | wc -l)

echo "Found $count example contracts"

for file in ${files[@]}; do
  echo "Typechecking $file..."
  stack exec -- idris --noprelude -p tezos --check $file
done

TMP=$(mktemp -d)

for file in ${files[@]}; do
  echo "Compiling $file..."
  ./scripts/tezos_compile.sh $file -o $TMP/test.tz
  test -f $TMP/test.tz
done

rm -rf $TMP

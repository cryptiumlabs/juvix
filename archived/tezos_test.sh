#!/bin/bash

set -e

files=$(find ./examples -type f -name "*.idr" | sort)
count=$(echo "$files" | wc -l)

echo "Found $count example contracts"

for file in ${files[@]}; do
  echo "Typechecking $file..."
  stack exec -- idris --noprelude -p tezos --check $file
done

ok=0
exit=0

for file in ${files[@]}; do
  OUT=$(mktemp).tz
  OUTPUT=$(./scripts/tezos_compile.sh $file -o $OUT)
  if test -f $OUT; then
    ok=`expr $ok + 1`
  else
    echo "Compilation of $file failed!"
    echo -e $OUTPUT
    exit=1
  fi
done

echo "$ok out of $count contracts compiled successfully!"

rm -rf $TMP

exit $exit

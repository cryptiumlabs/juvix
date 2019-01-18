#!/bin/bash

set -e

CLIENT=$(which tezos-client)
if [ -z "$CLIENT" ]; then
  echo "No tezos-client binary found!"
  exit 1
fi

CONTRACT=$1
STORAGE=$2
INPUT=$3
AMOUNT=$4

if [ -z "$CONTRACT" ] || [ -z "$STORAGE" ] || [ -z "$INPUT" ]; then
  echo "Usage: ./tezos_run.sh FILENAME STORAGE INPUT [AMOUNT]"
  exit 1
fi

if [ -z "$AMOUNT" ]; then
  set -x
  $CLIENT run script $CONTRACT on storage $STORAGE and input $INPUT
else
  set -x
  $CLIENT run script $CONTRACT on storage $STORAGE and input $INPUT -amount "$AMOUNT"
fi

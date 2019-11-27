#!/bin/sh

set -xe

make format

# disabled for now
# make lint

cd scripts && sbcl --script "./run-generate.lisp"

exit 0

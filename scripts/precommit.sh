#!/bin/sh

set -xe

make format

# disabled for now
# make lint

cd ~/scripts && ./run-generate.lisp

exit 0

#!/bin/sh

set -xe

make format

# disabled for now
# make lint

make org-gen

exit 0

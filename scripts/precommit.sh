#!/bin/sh

set -xe

make format

# disabled for now
# make lint

org-generation app/ doc/Code/App.org test/ doc/Code/Test.org src/ doc/Code/Juvix.org bench/ doc/Code/Bench.org

exit 0

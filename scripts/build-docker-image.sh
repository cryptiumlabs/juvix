#!/bin/sh

set -xe

time docker build --squash -t cryptiumlabs/juvix-ci .
docker push cryptiumlabs/juvix-ci

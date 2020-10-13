#!/bin/sh

TAG=cryptiumlabs/juvix-ci-2

set -xe

time docker build --squash -t $TAG .
docker push $TAG

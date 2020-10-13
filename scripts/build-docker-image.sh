#!/bin/sh

TAG=cryptiumlabs/juvix-ci

set -xe

time docker build --squash -t $TAG .
docker push $TAG

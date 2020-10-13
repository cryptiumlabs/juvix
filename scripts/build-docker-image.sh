#!/bin/sh

TAG=cryptiumlabs/juvix-ci

set -xe

time docker build -t $TAG .
docker save $TAG > /tmp/image.tar
TMPDIR=/var/run/shm docker-squash -i /tmp/image.tar -o /tmp/squashed.tar -t $TAG
rm /tmp/image.tar
cat /tmp/squashed.tar | docker load
rm /tmp/squashed.tar
docker push $TAG

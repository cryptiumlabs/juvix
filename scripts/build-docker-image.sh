#!/bin/sh

TAG=cryptiumlabs/juvix-ci

set -xe

time docker build -t $TAG .
docker save $TAG > /var/run/shm/image.tar
TMPDIR=/var/run/shm docker-squash -i /var/run/shm/image.tar -o /var/run/shm/squashed.tar -t $TAG
rm /var/run/shm/image.tar
rm /var/run/shm/squashed.tar
cat /var/run/shm/squashed.tar | docker load
docker push $TAG

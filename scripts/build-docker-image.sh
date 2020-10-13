#!/bin/sh

set -xe

time docker build -t cryptiumlabs/juvix-ci .
docker save cryptiumlabs/juvix-ci > /tmp/image.tar
docker-squash -i /tmp/image.tar -o /tmp/squashed.tar -t cryptiumlabs/juvix-ci
cat /tmp/squashed.tar | docker load
docker push cryptiumlabs/juvix-ci

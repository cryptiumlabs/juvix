#!/bin/sh

set -xe

time docker build -t cryptiumlabs/juvix-ci .
docker push cryptiumlabs/juvix-ci

#!/bin/sh

time docker build -t cryptiumlabs/juvix-ci .
docker push cryptiumlabs/juvix-ci

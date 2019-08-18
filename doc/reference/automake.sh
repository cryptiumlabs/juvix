#!/bin/sh

while true; do
inotifywait --quiet -e modify -r ./src && \
  make language-reference && echo "Updated!"
done

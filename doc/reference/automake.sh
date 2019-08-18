#!/bin/sh

while true; do
inotifywait --quiet -e modify -r ./src && \
  make && echo "Updated!"
done

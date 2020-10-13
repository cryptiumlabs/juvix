#!/bin/sh

make format

if [ -z "$(git status --untracked-files=no --porcelain)" ]; then
  exit 0
else
  git status
  exit 1
fi

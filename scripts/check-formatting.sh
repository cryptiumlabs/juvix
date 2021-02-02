#!/bin/sh

make format

if [ -z "$(git status src/ library/ test/ --untracked-files=no --porcelain)" ]; then
  exit 0
else
  ormolu --version
  git status
  exit 1
fi

#!/bin/sh

make format

if [ -z "$(git status src/ library/ test/ --untracked-files=no --porcelain)" ]; then
  exit 0
else
  # ormolu --version
  # git status
  git add -u
  git commit -m "[CI] Run code formatting"
  git push
  exit 0
fi

#!/bin/sh

git status src/ library/ test/ --untracked-files=no --porcelain

make format

if [ -z "$(git status src/ library/ test/ --untracked-files=no --porcelain)" ]; then
  exit 0
else
  ormolu --version
  git status src/ library/ test/ --untracked-files=no --porcelain
  git diff
  exit 1
fi
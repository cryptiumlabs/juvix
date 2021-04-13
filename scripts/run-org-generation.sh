#!/bin/sh

PATH=~/.roswell/bin:$PATH make org-gen

if [ -z "$(git status doc/Code --porcelain)" ]; then
  exit 0
else
  git add -u
  git commit -m "[CI] Run org generation"
  git push
  exit 0
fi
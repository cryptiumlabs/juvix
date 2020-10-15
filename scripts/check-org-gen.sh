#!/bin/sh

PATH=~/.roswell/bin:$PATH make org-gen

if [ -z "$(git status --porcelain)" ]; then
  exit 0
else
  git status
  exit 1
fi

#!/bin/sh

PATH=~/.roswell/bin:$PATH make org-gen

git config user.email "ci@heliax.dev"
git config user.name "Drone CI"

REMOTE=$(git remote get-url origin | cut -c 9-)
PUSH_URL="https://${GITHUB_TOKEN}@${REMOTE}.git"

if [ -z "$(git status doc/Code --porcelain)" ]; then
  exit 0
else
  echo "Committing differences..."
  git add -u
  git commit -m "[skip ci] Run org generation"
  git push $PUSH_URL
  exit 0
fi
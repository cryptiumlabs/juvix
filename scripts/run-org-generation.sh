#!/bin/sh

PATH=~/.roswell/bin:$PATH make org-gen

git config user.email "gianmarco@heliax.dev"
git config user.name "Drone CI"

REMOTE=$(git remote get-url origin | cut -c 9-)
PUSH_URL="https://${GITHUB_TOKEN}@${REMOTE}"

if [ -z "$(git status doc/Code --porcelain)" ]; then
  exit 0
else
  echo "Committing differences..."
  git remote set-url origin $PUSH_URL
  git checkout $DRONE_SOURCE_BRANCH
  git add -u
  git commit -m "run org-generation [CI SKIP]"
  git push --verbose
  exit $?
fi
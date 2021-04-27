git config user.email "gianmarco@heliax.dev"
git config user.name "Drone CI"

if [ -z "$(git status --porcelain)" ]; then 
    echo "No changes to commit."
    exit 78
fi

REMOTE=$(git remote get-url origin | cut -c 9-)
PUSH_URL="https://${GITHUB_TOKEN}@${REMOTE}"

git fetch --all
git checkout $DRONE_SOURCE_BRANCH

git status

git remote set-url origin $PUSH_URL

git add -A
git commit -m "[CI SKIP] changes from CI"

git push

exit $?

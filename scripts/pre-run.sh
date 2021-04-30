echo "e6ea3496b27ecdc2912b7866c65a93e2907a410fedf00b6af4215355a38a14fe  scripts/push-changes.sh" | sha256sum -c -
echo "a169df616d5cdf363cb7aaaf218a979d66c8c0a8c24e6c7333483dc933138354  scripts/format-and-org-gen.sh" | sha256sum -c -
echo "4438c2dfcd3aa0e4a3700fb5865c9b8e9bd208c38b1cb52b91b5393f56571a03  scripts/check-formatting.sh" | sha256sum -c -
echo "13f9fae7f558567336505324e4c54dabe978ba7441617854dd31d9f9e9c85c60  scripts/check-org-gen.sh" | sha256sum -c -

echo $COMMIT_MESSAGE
echo $DRONE_BRANCH
echo $DRONE_BUILD_EVENT

# we don't want to skip pipeline execution if event is push
if [[ "push" == "$DRONE_BUILD_EVENT" ]]; then
    exit 0
fi

COMMIT_MESSAGE=$(git show -s --format=%B ${DRONE_COMMIT_SHA})
CHECK="[ci]"

# check commit comment. If contains [ci], then exit pipeline sucessfully
if [[ "$COMMIT_MESSAGE" == *"$CHECK"* ]]; then
    echo "Skipping pipeline."
    exit 78
fi
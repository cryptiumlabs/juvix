echo "e6ea3496b27ecdc2912b7866c65a93e2907a410fedf00b6af4215355a38a14fe  scripts/push-changes.sh" | sha256sum -c -
echo "d04e32c725b9316805e85d2e8d26d9aaa7010f00e98cd933e4a16c64c0533a6f  scripts/format-and-org-gen.sh" | sha256sum -c -
echo "275e4e243a1197bfba6076e0df9df956d05ad3232ee97f78f00b4c1cce1ccb9d  scripts/check-formatting.sh" | sha256sum -c -
echo "4b2678ee3159c1ee1d4879384163af17d388f0ce1f716bbc6f891e2b32483d3e  scripts/check-org-gen.sh" | sha256sum -c -

echo $DRONE_BRANCH
echo $DRONE_BUILD_EVENT

# we don't want to skip pipeline execution if event is push
if [[ "push" == "$DRONE_BUILD_EVENT" ]]; then
    exit 0
fi

COMMIT_MESSAGE=$(git show -s --format=%B ${DRONE_COMMIT_SHA})
CHECK="[ci]"

echo $COMMIT_MESSAGE

# check commit comment. If contains [ci], then exit pipeline sucessfully
if [[ "$COMMIT_MESSAGE" == *"$CHECK"* ]]; then
    echo "Skipping pipeline."
    exit 78
fi
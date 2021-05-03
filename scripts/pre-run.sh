echo "7b1480fbe1ab0779c25d27c5515fa3a33d22b8f273008196676fe7e1d26eadf8  scripts/push-changes.sh" | sha256sum -c -
echo "d04e32c725b9316805e85d2e8d26d9aaa7010f00e98cd933e4a16c64c0533a6f  scripts/format-and-org-gen.sh" | sha256sum -c -
echo "4438c2dfcd3aa0e4a3700fb5865c9b8e9bd208c38b1cb52b91b5393f56571a03  scripts/check-formatting.sh" | sha256sum -c -
echo "13f9fae7f558567336505324e4c54dabe978ba7441617854dd31d9f9e9c85c60  scripts/check-org-gen.sh" | sha256sum -c -

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
echo "2a75d94d23168136b3870a3a480388a7a5fddbf55ae9c57c6dca14c33ab43097  scripts/push-changes.sh" | sha256sum -c -
echo "a169df616d5cdf363cb7aaaf218a979d66c8c0a8c24e6c7333483dc933138354  scripts/format-and-org-gen.sh" | sha256sum -c -
echo "4438c2dfcd3aa0e4a3700fb5865c9b8e9bd208c38b1cb52b91b5393f56571a03  scripts/check-formatting.sh" | sha256sum -c -
echo "13f9fae7f558567336505324e4c54dabe978ba7441617854dd31d9f9e9c85c60  scripts/check-org-gen.sh" | sha256sum -c -

# check commit comment. If contains $CHECK, than exit pipeline sucessfully
COMMIT_MESSAGE=$(git show -s --format=%B ${DRONE_COMMIT_SHA})
CHECK="[CI SKIP]"

if [[ "$CHECK" == *"$COMMIT_MESSAGE"* ]]; then
    echo "Skipping pipeline."
    exit 78
fi
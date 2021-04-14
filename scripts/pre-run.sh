# echo "bbd7240242839ed5051ba48ebb3b2e66a4cdeb50945ff96e09c0869dc219ceef  scripts/run-formatter.sh" | sha256sum -c -
# echo "a2cb891077e670ccf9546df3ad1e7629e9e8858680a281bc836abfcef414641a  scripts/run-org-generation.sh" | sha256sum -c -
# echo "4438c2dfcd3aa0e4a3700fb5865c9b8e9bd208c38b1cb52b91b5393f56571a03  scripts/check-formatting.sh" | sha256sum -c -
# echo "13f9fae7f558567336505324e4c54dabe978ba7441617854dd31d9f9e9c85c60  scripts/check-org-gen.sh" | sha256sum -c -

# check commit comment. If contains $CHECK, than exit pipeline sucessfully
COMMIT_MESSAGE=$(git show -s --format=%B ${DRONE_COMMIT_SHA})
CHECK="[CI SKIP]"
if [[ $CHECK == *"${COMMIT_MESSAGE}"* ]]; then
    exit 78
fi
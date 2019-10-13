# Contributing

Contributions are welcome! Please consider the following guidelines.

## Code formatting

- Run `make format` to automatically format your code.
- Any changes to code style should be submitted as pull requests to https://github.com/cryptiumlabs/ormolu

## Branch usage

The mainline branch is `develop` (this will change post-1.0.0). Feature development should be done on feature branches.

## Branch naming

Name your feature branches with a name or handle of the branch owner as a prefix, followed by a brief description preceded by a slash, e.g. `cwgoes/eal-inference-bugfix`.

Pushing minor changes (typo fixes) to another person's branch is fine. Ask before pushing major changes.

## Reviews & merging

Before 1.0.0, you may merge your own PRs, review is not required (but feel free to request a review if you would like one, Github has a button to do so).

## Pre-commit hooks

Please put the following in `.git/hooks/pre-commit` and run `chmod +x .git/hooks/pre-commit`.
```bash
#!/bin/sh

./scripts/precommit.sh
```
Ensure that it passes before you submit a pull request.

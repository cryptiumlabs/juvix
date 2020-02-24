# Contributing

Contributions are welcome! Please follow the [style guide](https://github.com/cryptiumlabs/juvix/blob/develop/doc/STYLEGUIDE.md) and consider the following guidelines.

## Code formatting

- Run `make format` to automatically format your code.
- Any changes to code style should be submitted as pull requests to https://github.com/cryptiumlabs/ormolu

## Documentation

Add a brief description of any module you create before the module definition.
This can be placed before or after (more idiomatically) after file specific pragmas.
Much like haddock, the comments must begin with `--|` as the first comment and `--` for the consecutive lines.

The comment formatting follows [org formatting](http://ergoemacs.org/emacs/emacs_org_markup.html)

Adding headlines to code is also allowed, nesting the headline inside the current headline level while also putting `Relies on`
before any nested directories

Also try to keep each comment line below 82 characters long, breaking the line into a second line at the same indentation level
and on the next line will not newline any generation from the org file itself.

An example of this generation [can be seen here](https://github.com/cryptiumlabs/juvix/blob/15ca9e5e602d24cf09fe87fc059e3e0ee78ad6db/src/Juvix/Encoding/Encoding.hs#L3).

The best way to write the documentation is by getting an org mode extension and writing the comments in said extension.
- [atom](https://atom.io/packages/org-mode)
- [vim](https://github.com/jceb/vim-orgmode)
- [vscode](https://marketplace.visualstudio.com/items?itemName=tootone.org-mode)
- [emacs](https://orgmode.org/)
  + [emacs-in-buffer](http://pragmaticemacs.com/emacs/write-code-comments-in-org-mode-with-poporg/)

## Branch usage

The mainline branch is `develop` (this will change post-1.0.0). Feature development should be done on feature branches.

## Branch naming

Name your feature branches with a name or handle of the branch owner as a prefix, followed by a brief description preceded by a slash, e.g. `cwgoes/eal-inference-bugfix`.

Pushing minor changes (typo fixes) to another person's branch is fine. Ask before pushing major changes.

## Reviews & merging

Before 1.0.0, you may merge your own PRs, review is not required (but feel free to request a review if you would like one, Github has a button to do so).

## Editing Environments

- __Intero + Emacs__
  - Sadly by default intero does not work out of the box for code in the `test/` directory
  - To allow intero to integrate nicely with this project please type `M-x intero-targets` and select the following
    ```
    [x] juvix:lib
    [ ] juvix:exe:juvix
    [x] juvix:test:juvix-test
    ```
    - This should write the elisp file `.dir-locals.el`
    ```elisp
    ;;; Directory Local Variables
    ;;; For more information see (info "(emacs) Directory Variables")

    ((haskell-mode
       (intero-targets "juvix:lib" "juvix:test:juvix-test")))
    ```
    - Upon further uses, emacs will ask about unsafe variable values, allow it to cache in your .emacs that the code is safe
  - You may have to open emacs in the directory of Juvix for emacs + intero to work properly

## Pre-commit hooks

Please put the following in `.git/hooks/pre-commit` and run `chmod +x .git/hooks/pre-commit`.
```bash
#!/bin/sh

./scripts/precommit.sh
```
Ensure that it passes before you submit a pull request.

Juvix's Haskell Style Guide
===========================

This document describes the preferred coding style for Juvix.
When something isn't covered by this guide you should stay
consistent with the existing code.
See also [CONTRIBUTING.md](https://github.com/cryptiumlabs/juvix/blob/develop/doc/CONTRIBUTING.md).

HLint
-----

Set up [HLint](https://github.com/ndmitchell/hlint)
in your code environment and follow its code suggestions
as much as you can.

Formatting
----------

Our formatter is [Ormolu](https://github.com/tweag/ormolu) (the latest version
on Stack). The main reason we chose Ormolu is
that it works for all the extensions we use. See this [post](https://www.tweag.io/posts/2019-05-27-ormolu.html)
and this [post](https://www.tweag.io/posts/2019-10-11-ormolu-first-release.html)
for more motivations and rationale. 

Unless otherwise specified below, follow
[Tweag's](https://github.com/tweag/guides/blob/master/style/Haskell.md) style
guide.


### Line Length

Maximum line length is *80 characters*.
There should be no trailing whitespace anywhere in your code.

- In Emacs, you can add the following code to your `init.el` file to
enforce this:

```elisp
(add-hook 'haskell-mode-hook (lambda () (set-fill-column 80)))
(add-hook 'haskell-mode-hook
          (lambda ()
             (add-hook 'before-save-hook 'delete-trailing-whitespace t t)))
```
- In VScode, use the [Rewrap](https://github.com/stkb/Rewrap) extension to
  enable hard word wrapping.

- In Vim, follow [this](https://vim.fandom.com/wiki/Automatic_word_wrapping) to
  enable hard word wrapping.

### Whitespace

Surround binary operators with a single space on either side.  Use
your better judgement for the insertion of spaces around arithmetic
operators but always be consistent about whitespace on either side of
a binary operator.  Don't insert a space after a lambda.  For example:

```haskell
plus4 n = n + 4  -- whitespace on either side of `+`

(\x -> x + 4)  -- no space after the lambda
```

Imports
-------

Imports should be grouped in the following order:

1. standard library imports
2. related third party imports
3. local application/library specific imports

Put a blank line between each group of imports.  The imports in each
group should be sorted alphabetically, by module name.

Always use explicit import lists or `qualified` imports for standard
and third party libraries.  The *Juvix Standard Library* is an exception.

Qualified imports help two fold:

1. easily determine what code code comes from where and refactoring dependencies
2. Enforce consistency between different Haskell modules. For example:

Before:
```haskell
module Usage where

data UsageType = ...

usage = ...

```
```haskell
module Nat where

data NatType = ...

nat = ...

```
After:

```haskell
module Usage where

data Type = ...

t = ...

```

```haskell
module Nat where

data Type = ...

t = ...

```

Warnings
--------

`-Wall` is turned on. Keep warnings to the minimum.
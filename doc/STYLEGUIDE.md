Juvix's Haskell Style Guide
===========================

This document describes the preferred coding style for Juvix. When something isn't covered by this guide you should stay
consistent with the existing code. See also  [CONTRIBUTING.md](https://github.com/cryptiumlabs/juvix/blob/develop/doc/CONTRIBUTING.md).

HLint
-----

Set up [HLint](https://github.com/ndmitchell/hlint) in your code environment and follow its code suggestions
as much as you can.

Formatting
----------

### Line Length

Maximum line length is *80 characters*. There should be no trailing whitespace anywhere in your
code.

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


### Indentation

Use spaces for indenting.  Indent your code blocks
with *2 spaces*.  

### Blank Lines

One blank line between top-level definitions.  No blank lines between
type signatures and function definitions.  Add one blank line between
functions in a type class instance declaration if the function bodies
are large.  Use your judgement.

### Whitespace

Surround binary operators with a single space on either side.  Use
your better judgement for the insertion of spaces around arithmetic
operators but always be consistent about whitespace on either side of
a binary operator.  Don't insert a space after a lambda. For example:

```haskell
plus4 n = n + 4  -- whitespace on either side of `+`

(\x -> x + 4)  -- no space after the lambda 
```

### Data Declarations

Align the constructors in a data type definition.  Example:

```haskell
data Tree a = Branch !a !(Tree a) !(Tree a)
            | Leaf
```

For long type names the following formatting is also acceptable:

```haskell
data HttpException
    = InvalidStatusCode Int
    | MissingContentHeader
```

Format records as follows:

```haskell
data Person = Person
    { firstName :: !String  -- ^ First name
    , lastName  :: !String  -- ^ Last name
    , age       :: !Int     -- ^ Age
    } deriving (Eq, Show)
```

### List Declarations

Align the elements in the list.  Example:

```haskell
exceptions =
    [ InvalidStatusCode
    , MissingContentHeader
    , InternalServerError
    ]
```

Optionally, you can skip the first newline.  Use your judgement.

```haskell
directions = [ North
             , East
             , South
             , West
             ]
```

### Export Lists

Format export lists as follows:

```haskell
module Data.Set
    (
      -- * The @Set@ type
      Set
    , empty
    , singleton

      -- * Querying
    , member
    ) where
```

### If-then-else clauses

Generally, guards and pattern matches should be preferred over if-then-else
clauses, where possible.  Short cases should usually be put on a single line
(when line length allows it).


### Case expressions

The alternatives in a case expression can be indented as follows:

```haskell
foobar = case something of
  Just j  -> foo
  Nothing -> bar
```

Align the `->` arrows when it helps readability.

Imports
-------

Imports should be grouped in the following order:

1. standard library imports
2. related third party imports
3. local application/library specific imports

Put a blank line between each group of imports.  The imports in each
group should be sorted alphabetically, by module name.

Always use explicit import lists or `qualified` imports for standard
and third party libraries. The *Prelude* is an exception.

Comments
--------

### Punctuation

Write proper sentences; start with a capital letter and use proper
punctuation. Make sure there are no typos.

### Top-Level Definitions

Comment every top level function (particularly exported functions),
and provide a type signature; use Haddock syntax in the comments.
Comment every exported data type.  Function example:

```haskell
-- | Send a message on a socket.  The socket must be in a connected
-- state.  Returns the number of bytes sent.  Applications are
-- responsible for ensuring that all data has been sent.
send :: Socket      -- ^ Connected socket
     -> ByteString  -- ^ Data to send
     -> IO Int      -- ^ Bytes sent
```

For functions the documentation should give enough information to
apply the function without looking at the function's definition.

Record example:

```haskell
-- | Bla bla bla.
data Person = Person
    { age  :: !Int     -- ^ Age
    , name :: !String  -- ^ First name
    }
```

For fields that require longer comments format them like so:

```haskell
data Record = Record
    { -- | This is a very very very long comment that is split over
      -- multiple lines.
      field1 :: !Text

      -- | This is a second very very very long comment that is split
      -- over multiple lines.
    , field2 :: !Int
    }
```

### End-of-Line Comments

Separate end-of-line comments from the code using 2 spaces.  Align
comments for data type definitions.  Some examples:

```haskell
data Parser = Parser
    !Int         -- Current position
    !ByteString  -- Remaining input

foo :: Int -> Int
foo n = salt * 32 + 9
  where
    salt = 453645243  -- Magic hash salt.
```

### Links

Use in-line links economically.  You are encouraged to add links for
API names.  It is not necessary to add links for all API names in a
Haddock comment.  We therefore recommend adding a link to an API name
if:

* The user might actually want to click on it for more information (in
  your judgment), and

* Only for the first occurrence of each API name in the comment (don't
  bother repeating a link)

Naming
------

Use camel case (e.g. `functionName`) when naming functions and upper
camel case (e.g. `DataType`) when naming data types.

For readability reasons, don't capitalize all letters when using an
abbreviation.  For example, write `HttpServer` instead of
`HTTPServer`.  Exception: Two letter abbreviations, e.g. `IO`.

### Modules

Use singular when naming modules e.g. use `Data.Map` and
`Data.ByteString.Internal` instead of `Data.Maps` and
`Data.ByteString.Internals`.

Misc
----

### Point-free style ###

Avoid over-using point-free style.  For example, this is hard to read:

```haskell
-- Bad:
f = (g .) . h
```

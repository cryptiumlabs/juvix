# juvix Language support


Support for the juvix programming language in Visual Studio Code.

## Syntax highlighting

Adds syntax highlighting support for juvix (.ju and .lju). This is a (now heavily) modified version of
the syntax file from the [juvix TextMate bundle](https://github.com/textmate/juvix.tmbundle).
Additionally there is support for Cabal files (.cabal) via a concoction of my own.

Also adds automatic indentation after `where`, `do`, `->` etc. and surrounding brackets (`[]`, `{}` etc)

![Screenshot juvix](/images/screenshot1.png?raw=true)

![Screenshot Cabal](/images/screenshot-cabal1.png?raw=true)

## Bugs

If you happen to notice bugs or have suggestions for improvements visit the [issue
section](https://github.com/metastatedev/juvix/issues) of the
[repository](https://github.com/metastatedev/juvix).

## Themes

This extension provides TextMate scopes for use in syntax highlighting, but the colours displayed
depend on the theme being used.    
Unfortunately many themes have incomplete support for the different TextMate scopes, and as a
result different tokens can end up with identical colours.    

For a theme that supports all the scopes provided by this extension, see the
[Groovy Lambda theme](https://github.com/sheaf/groovy-lambda).

## Theme authors

I recently realized that I am woefully unaware of whether there are any themes with juvix-specific
rules and how changes to this extension affect such themes. If you are a theme author that wishes to
use juvix specific rules, or are aware of a theme with juvix specific rules, feel free to get in
touch.

With version `3.0.0` some new tm scopes were added, such that now record and GADT definitions can be
distinguished. Let me know if there are any questions about the scope assignment in this
extension or if there are further scope assigments you'd like to see added.

We now publish an automatically generated, complete list of the textmate scopes
used in our grammars. You can find the lists of scopes in the
[scope-lists](/scope-lists) directory.

all: setup build

setup:
	stack build --only-dependencies

build:
	stack build --copy-bins --fast

build-watch:
	stack build --copy-bins --fast --file-watch

build-opt: clean
	stack build --copy-bins --ghc-options "-O3"

lint:
	stack exec -- hlint app src test

test:
	stack test --fast

repl-lib:
	stack ghci juvix:lib

repl-exe:
	stack ghci juvix:exe

clean:
	stack clean

clean-full:
	stack clean --full

.PHONY: all setup build build-watch build-opt lint test repl-lib repl-exe clean clean-full

all: setup build

setup:
	stack build --only-dependencies

build-z3:
	cd z3 && python scripts/mk_make.py && cd build && make

build: build-z3
	stack build --copy-bins --fast

build-watch:
	stack build --copy-bins --fast --file-watch

build-opt: clean
	stack build --copy-bins --ghc-options "-O3 -fllvm"

lint:
	stack exec -- hlint app src test

format:
	find . -type f -name "*.hs" -exec stylish-haskell -i {} \;

test:
	stack test --fast

repl-lib:
	stack ghci juvix:lib

repl-exe:
	stack ghci juvix:exe:juvix

clean:
	stack clean

clean-full:
	stack clean --full

.PHONY: all setup build build-watch build-opt lint format test repl-lib repl-exe clean clean-full

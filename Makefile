PWD=$(CURDIR)
PREFIX="$(PWD)/.stack-work/prefix"

all: setup build

setup:
	stack build --only-dependencies

build-z3:
	mkdir -p $(PREFIX)
	cd z3 && test -f build/Makefile || python scripts/mk_make.py
	cd z3/build && make -j $(shell nproc)
	cd z3/build && PREFIX=$(PREFIX) make install

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

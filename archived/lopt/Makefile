all: setup build

setup:
	stack build --only-dependencies

build:
	stack build --copy-bins --fast

repl:
	stack ghci

clean:
	stack clean

.PHONY: all setup build repl clean

all: compile

compile:
	rm -f Test Test.hi Test.o
	stack exec -- ghc Test.hs -O0 -ddump-stg -ddump-asm

.PHONY: compile

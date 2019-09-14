all: map church

clean:
	rm -f *.hi *.ho
	rm -rf Map Church

map: clean
	stack exec -- ghc Map.hs -O0 -ddump-stg -ddump-asm

church: clean
	stack exec -- ghc Church.hs -O0 -ddump-stg -ddump-asm

.PHONY: clean map church

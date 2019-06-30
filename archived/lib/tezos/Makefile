all: install

PKG=tezos.ipkg
IDRIS=stack exec idris --

build: clean
	${IDRIS} --build ${PKG}

install: build
	${IDRIS} --install ${PKG}

clean:
	${IDRIS} --clean ${PKG}

.PHONY: all build install clean

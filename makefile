.PHONY: all build test

all: build test

build:
	cabal install all --install-method copy --overwrite-policy=always --installdir .

test:
	cabal test all

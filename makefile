.PHONY: all build test

all: test build

build:
	cabal install all --install-method copy --overwrite-policy=always --installdir .

test:
	cabal test all

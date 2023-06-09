.PHONY: all build test

all: test build

build:
	cabal build
	cp "$$(cabal list-bin jsonsrt)" .

test:
	cabal test

install:
	cabal install --overwrite-policy always --install-method copy

default:

all: configure install-dependencies build doc test

example:
	cabal run spreto -- examples/alice.txt

configure:
	cabal configure --enable-tests -f development

install-dependencies:
	cabal install --enable-tests --dependencies-only

build:
	cabal build

test:
	cabal test --show-details=always

doc: dist/doc/html/spreto dist/doc/html/README.html

dist/doc/html/spreto: src/*.hs
	cabal haddock --executables
	touch $@

dist/doc/html/README.html: README.md
	mkdir -p $(dir $@)
	pandoc $< -o $@ --css pandoc.css --standalone --from gfm --to html --metadata=title:README

.PHONY: default all example install-dependencies build doc test

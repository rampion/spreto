default:

all: configure install-dependencies build doc test

run-example:
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

dist/doc/html/README.html: README.md dist/doc/html/gfm.css
	pandoc $< -o $@ --css gfm.css --standalone --from gfm --to html --metadata=title:README

dist/doc/html/gfm.css: gfm.css
	mkdir -p $(dir $@)
	cp $< $@

.PHONY: default all example install-dependencies build doc test

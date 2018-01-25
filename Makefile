default:

all: doc build test

doc: README.html

build:
	cabal build

test:
	cabal test

run:
	cabal run spreto -- examples/alice.txt

README.html: README.md
	pandoc -f markdown_github -i $< -o $@ --css pandoc.css -s

.PHONY: default all doc build test

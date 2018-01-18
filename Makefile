default:

all: doc

doc: README.html

README.html: README.md
	pandoc -f markdown_github -i $< -o $@ --css pandoc.css -s

.PHONY: default doc

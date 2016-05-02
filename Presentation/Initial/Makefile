all: $(patsubst %.md, %.pdf, $(wildcard *.md))

%.pdf: %.md
	pandoc -t beamer -V theme:shadow $< -o $@

%.tex: %.md
	pandoc -t beamer -V theme:shadow $< -o $@

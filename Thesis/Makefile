all: $(patsubst %.md, %.pdf, $(wildcard *.md))

%.pdf: %.md
	pandoc --toc -V documentclass:scrreprt -H header.tex $< -o $@

%.tex: %.md
	pandoc --toc $< -o $@

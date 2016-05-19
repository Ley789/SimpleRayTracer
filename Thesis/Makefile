all: $(patsubst %.md, %.pdf, $(wildcard *.md))

%.pdf: %.md
	pandoc --toc -V documentclass:scrartcl -H header.tex $< -o $@

%.tex: %.md
	pandoc --toc $< -o $@

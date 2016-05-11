all: $(patsubst %.md, %.pdf, $(wildcard *.md))

%.pdf: %.md
	pandoc --toc $< -o $@

%.tex: %.md
	pandoc --toc $< -o $@

all: $(patsubst %.md, %.tex, $(wildcard *.md))

%.pdf: %.md
	pandoc --filter pandoc-citeproc -V documentclass:clbthesis -H header.tex $< -o $@

%.tex: %.md
	rm thesis.tex; \
	rm thesis.pdf; \
	pandoc --chapter $< -o $@; \
	pdflatex --shell-escape  clb.tex; \
	bibtex clb;       \
	pdflatex --shell-escape clb.tex; \
	pdflatex --shell-escape clb.tex; \

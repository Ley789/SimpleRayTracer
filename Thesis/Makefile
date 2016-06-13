all: clb.pdf

%.pdf: %.md
	pandoc --filter pandoc-citeproc -V documentclass:clbthesis -H header.tex $< -o $@

%.tex: %.md
	pandoc --chapter $< -o $@

clb.pdf: clb.tex thesis.tex
	pdflatex --shell-escape $<
	bibtex clb
	pdflatex --shell-escape $<
	pdflatex --shell-escape $<

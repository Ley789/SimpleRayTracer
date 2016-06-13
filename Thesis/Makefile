all: thesis.pdf

%.tex: %.md metadata.yaml template.latex
	pandoc metadata.yaml --template template.latex -H header.tex --chapters --toc -s $< -o $@

%.pdf: %.tex
	pdflatex --shell-escape $<
	bibtex thesis
	pdflatex --shell-escape $<
	pdflatex --shell-escape $<

all: thesis.pdf

clean:
	rm *.aux *.bbl *.blg *.log *.out *.toc

%.tex: %.md metadata.yaml template.latex
	pandoc metadata.yaml --template template.latex -H header.tex --chapters --toc -s $< -o $@

%.pdf: %.tex
	pdflatex $<
	bibtex $*
	pdflatex $<
	pdflatex $<

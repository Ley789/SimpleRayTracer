all: thesis.pdf

clean:
	rm *.aux *.bbl *.blg *.log *.out *.toc

%.tex: %.md biblio.bib metadata.yaml template.latex
	pandoc --natbib --bibliography biblio.bib \
	  metadata.yaml --template template.latex -H header.tex \
	  --chapters --toc -s $< -o $@

%.pdf: %.tex
	pdflatex $<
	bibtex $*
	pdflatex $<
	pdflatex $<

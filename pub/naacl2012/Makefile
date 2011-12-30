
naacl2012.pdf: $(wildcard *.tex) ref.bib Makefile
	@pdflatex naacl2012
	@bibtex naacl2012
	@pdflatex naacl2012
	@pdflatex naacl2012

clean:
	rm -f *.aux *.log *.bbl *.blg *.pdf

default: naacl2012.pdf


naacl2012.pdf: $(wildcard *.tex) ref.bib Makefile fig/gauss.plot fig/figs.rb
	@cd fig && make all || true
	@pdflatex naacl2012
	@bibtex naacl2012
	@pdflatex naacl2012
	@pdflatex naacl2012

clean:
	rm -f *.aux *.log *.bbl *.blg *.pdf

default: naacl2012.pdf

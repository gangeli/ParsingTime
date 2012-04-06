
naacl2012.pdf: $(wildcard *.tex) ref.bib Makefile fig/gauss.plot fig/figs.rb fig/pr.plot fig/pr.dat
	@cd fig && make
	@pdflatex naacl2012
	@bibtex naacl2012
	@pdflatex naacl2012
	@pdflatex naacl2012

clean:
	rm -f *.aux *.log *.bbl *.blg *.pdf *.bak

default: naacl2012.pdf

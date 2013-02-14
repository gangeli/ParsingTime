
angeli_time.pdf: $(wildcard *.tex) $(wildcard fig/*.eps) fig/figs.rb fig/lib.rb ref.bib Makefile
	@cd fig && make
	@pdflatex angeli_time
	@bibtex angeli_time
	@pdflatex angeli_time
	@pdflatex angeli_time

clean:
	rm -f *.aux *.log *.bbl *.blg *.pdf *.bak *.ps *.dvi

default: angeli_time.pdf

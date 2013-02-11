
angeli_time.pdf: $(wildcard *.tex) $(wildcard fig/*.eps) ref.bib Makefile
	@latex angeli_time
	@bibtex angeli_time
	@latex angeli_time
	@latex angeli_time
	@dvips angeli_time.dvi
	@ps2pdf angeli_time.ps angeli_time.pdf

clean:
	rm -f *.aux *.log *.bbl *.blg *.pdf *.bak *.ps *.dvi

default: angeli_time.pdf

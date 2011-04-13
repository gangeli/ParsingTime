default: all

spec.pdf: spec.tex
	@latex spec
	@latex spec
	@dvips spec.dvi > /dev/null
	@ps2pdf spec.ps > /dev/null

clean:
	rm -f *.log *.aux
	rm -f spec.pdf spec.ps spec.dvi

all: spec.pdf

.SUFFIXES: .PS .dvi .tex

all: libfxcl.PS

.tex.dvi:
	latex $*

.dvi.PS:
	dvi2ps -r $*.dvi > $@

.tex.PS:
	latex $*
	dvi2ps -r $*.dvi > $@

clean:
	rm -f *~ *.PS *.log *.dvi

SUBDIR=	animator autopull beeper bgcolor bigcur blankscreen btol calendar \
	circularities class-browser clocks colormenu colors colortable \
	colortool colour cyber distillery dragcanvas drawer dumb dumper \
	duper enhanced exporttonews graph gravity icongravity icons \
	invertmatrix journalpanel keypad keysee knobs litedialog logo \
	lps mac mag mail-popup mail-sender mchoose mousee mult \
	netsecurity newsbiff newsbomb newsmon object-browser pacman \
	patternfill piemenus plane popup print-hier prompt ps2bits \
	ps3d psbiff psexec psgraph pshexec pspp rpsterm ruler \
	save-proc scrollclient scrolldata scrollwin softmenu sorts \
	stickem term textfield tooltool trackstack vectorops \
	winwin xfonts zap
all:
	-for i in ${SUBDIR} ; do (echo Making $$i; cd $$i; make -k); done

clean:
	rm -f core *~ *.BAK
	-for i in ${SUBDIR} ; do (echo Cleaning $$i; cd $$i; make clean); done


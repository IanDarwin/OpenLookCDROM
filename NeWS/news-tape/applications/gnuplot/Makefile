# where to install executable on 'make install'
DESTDIR=/usr/local/bin
# where to install man page on 'make man_install'
MANDESTDIR=/usr/man/manl

# -DFORK if you have vfork()
# -DBCOPY if your memcpy() is called bcopy() (a Berkeleyism, right?)
# -DNOCOPY if you don't have a memcpy() by any name
# -DGAMMA if you've got gamma(3)
# -O if you trust your compiler's optimizer
CFLAGS = -DVFORK -DBCOPY  -g #-gx -O

# -D<terminal> in TERMFLAGS iff you wish to support <terminal>
# -DAED         AED 512 and AED 767
# -DBITGRAPH    BBN BitGraph
# -DHP26        HP2623A and maybe others
# -DHP75        HP7580, and probably other HPs
# -DNEWS	SUN Newtwork Window System (Postscript based)
# -DPOSTSCRIPT  Postscript
# -DQMS         QMS/QUIC laserprinter (Talaris 1200 and others)
# -DREGIS       ReGis graphics (vt125, vt220, vt240, Gigis...)
# -DSELANAR     Selanar
# -DTEK         Tektronix 4010, and probably others
# -DUNIXPLOT    unixplot
# -DV384        Vectrix 384 and tandy color printer

TERMFLAGS = -DAED -DBITGRAPH -DHP26 -DNEWS -DPOSTSCRIPT -DQMS  -DTEK -DUNIXPLOT -I/usr/NeWS/include

.SUFFIXES:	.cps
.cps.h:
	cps $*.cps

OBJS = 	command.o eval.o graphics.o internal.o misc.o parse.o plot.o scanner.o\
    standard.o term.o util.o

CSOURCE1 = command.c eval.c graphics.c internal.c misc.c parse.c
CSOURCE2 = plot.c scanner.c standard.c term.c util.c version.c
# not C code, but still needed
ETC = README gnuplot.1 Makefile plot.h term.cps simple.demo 1.dat 2.dat 3.dat \
	  controls.demo
# VMS and PC-specific files
VMSPC = compile.com link.com \
        buildhlp.c corgraph.asm corplot.c header.mac help.c help.hlm \
		hrcgraph.asm lineproc.mac link.opt make.msc pcgraph.asm

# -lplot if you have -DUNIXPLOT
LIBS = -lm -lplot $(NEWSHOME)/lib/libcps.a

all:	gnuplot help

help:	help.o
	${CC} ${CFLAGS} help.o -o help

help.o: help.h /usr/include/sys/file.h /usr/include/sys/param.h

install: help

gnuplot: $(OBJS) version.o
	cc $(CFLAGS) $(OBJS) version.o $(LIBS) -o gnuplot

install: gnuplot
	cp gnuplot $(DESTDIR)/gnuplot
	strip $(DESTDIR)/gnuplot
	install -s -c help $(DESTDIR)/help

man_install: gnuplot.1
	cp gnuplot.1 $(MANDESTDIR)

clean:
	rm -f *.o gnuplot term.h help core *~ *.BAK

term.o: term.c term.h
	cc $(CFLAGS) $(TERMFLAGS) -c term.c

$(OBJS): plot.h
term.h:	term.cps

lint:
	lint -hx $(CSOURCE1) $(CSOURCE2)

vax:
	uucp -m $(CSOURCE1) $(CSOURCE2) plot.h excalibur!m1:\[esl.source.esl.gnuplot\]

archive: $(CSOURCE1) $(CSOURCE2) $(ETC)
	tar cf /tmp/gnuplot.tar $(ETC) $(CSOURCE1) $(CSOURCE2) docs help

shar: gnuplot.shar.1 gnuplot.shar.2 gnuplot.shar.3 gnuplot.shar.4 \
	  gnuplot.shar.5 gnuplot.shar.6

gnuplot.shar.1: $(ETC)
	shar -vc $(ETC) > gnuplot.shar.1

gnuplot.shar.2: $(CSOURCE1)
	shar -vc $(CSOURCE1) > gnuplot.shar.2

gnuplot.shar.3: $(CSOURCE2)
	shar -vc $(CSOURCE2) > gnuplot.shar.3

gnuplot.shar.4: docs/*
	cd docs; make clean
	shar -vc docs > gnuplot.shar.4

gnuplot.shar.5: help/gnuplot/*
	(cd help; shar -vc gnuplot) > gnuplot.shar.5

gnuplot.shar.6: pc/*
	(cd pc; shar -vc $(VMSPC) ) >gnuplot.shar.6

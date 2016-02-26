BINDIR=${INSDIR}/bin
DATADIR=${INSDIR}/data
PSDIR=${INSDIR}/NeWS

# modify this and build
CFLAGS= -O

LIBS=$(NEWSHOME)/lib/libcps.a
INCLUDE=$(NEWSHOME)/include

.SUFFIXES:	.cps

.cps.h:	; cps $*.cps

#all:	mandel mandel_fpa mandel_68881
all:	mandel

mandel_fpa:	mandel.c mandel.h
	cc $(CFLAGS) -I${INCLUDE}  -ffpa -o mandel_fpa mandel.c $(LIBS)

mandel_68881:	mandel.c mandel.h
	cc $(CFLAGS) -I${INCLUDE}  -f68881 -o mandel_68881 mandel.c $(LIBS)

# Add your special screaming floating-point performance options here.
mandel:	mandel.c mandel.h
	cc $(CFLAGS) -I${INCLUDE} -o mandel mandel.c $(LIBS)

install:  all
	cp mandel ${BINDIR}/mandel

clean:	; rm -f *.o *.BAK mandel.h mandel mandel_* mandel.shar core

shar:	; shar -v README Makefile mandel.c mandel.cps > mandel.shar

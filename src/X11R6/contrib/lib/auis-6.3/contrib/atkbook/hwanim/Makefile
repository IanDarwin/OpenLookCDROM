ANDREWDIR = /usr/andrew

INCLUDES = -I. -I${ANDREWDIR}/include -I${ANDREWDIR}/include/atk

.SUFFIXES: .ih .eh .ch .do

.ch.ih:
	${ANDREWDIR}/bin/class ${INCLUDES} $*.ch
.ch.eh:
	${ANDREWDIR}/bin/class ${INCLUDES} $*.ch
.c.o:
	cc -c ${INCLUDES} $*.c
.o.do:
	${ANDREWDIR}/bin/makedo -g $<

all:  hanim hwarg hw

hanim: hanimapp.do hwapp.do
	-ln -s ${ANDREWDIR}/bin/runapp hanim

hanimapp.do:  hanimapp.eh hwargapp.ih hwapp.ih hanimapp.o

hwarg: hwargapp.do hwapp.do
	-ln -s ${ANDREWDIR}/bin/runapp hwarg

hwargapp.do:  hwargapp.eh hwapp.ih hwargapp.o

hw: hwapp.do
	-ln -s ${ANDREWDIR}/bin/runapp hw

hwapp.do:  hwapp.eh hwapp.o

clean:
	rm -f *.o *.do *.eh *.ih *.BAK *.CKP hw hwarg hanim core *.dog

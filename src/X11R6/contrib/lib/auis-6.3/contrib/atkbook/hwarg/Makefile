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
	${ANDREWDIR}/bin/makedo $<

all:  hwarg hw

hwarg: hwargapp.do hwapp.do
	-ln -s ${ANDREWDIR}/bin/runapp hwarg

hw: hwapp.do
	-ln -s ${ANDREWDIR}/bin/runapp hw

hwapp.do:  hwapp.o

hwargapp.do:  hwargapp.o

hwargapp.o:  hwargapp.eh hwargapp.c

hwapp.o:  hwapp.eh hwapp.c

hwargapp.eh:  hwargapp.ch hwapp.ih

hwargapp.ih:  hwargapp.ch hwapp.ih

hwapp.eh:  hwapp.ch 

hwapp.ih:  hwapp.ch

clean:
	rm -f *.o *.do *.eh *.ih *.BAK *.CKP hw hwarg core

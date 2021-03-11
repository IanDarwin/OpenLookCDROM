ANDREWDIR=/usr/andrew

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

hw: hwapp.do
	-ln -s ${ANDREWDIR}/bin/runapp hw

hwapp.do:  hwapp.eh hwapp.o

hwapp.o: hwapp.eh

clean:
	rm -f *.o *.do *.eh *.ih *.BAK *.CKP hw core

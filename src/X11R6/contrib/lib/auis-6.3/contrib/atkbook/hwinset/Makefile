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

all: hw.do hwview.do

hw.o: hw.eh

hwview.o: hwview.eh

hw.do: hw.o

hwview.do: hwview.o

clean:
	rm -f *.o *.do *.eh *.ih *.BAK *.CKP core

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

all: switcher.do switview.do

switcher.o: switcher.eh

switview.o: switview.eh switcher.ih

switcher.do: switcher.o

switview.do: switview.o

clean:
	rm -f *.o *.do *.eh *.ih *.BAK *.CKP core

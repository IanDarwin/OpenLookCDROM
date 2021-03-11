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

all: flex.do flexview.do

flex.o: flex.eh

flexview.o: flexview.eh flex.ih

flex.do: flex.o

flexview.do: flexview.o

clean:
	rm -f *.o *.do *.eh *.ih *.BAK *.CKP core

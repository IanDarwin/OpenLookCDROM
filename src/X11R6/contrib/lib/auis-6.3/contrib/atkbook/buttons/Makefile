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

all: butt.do buttview.do

butt.o: butt.eh

buttview.o: buttview.eh butt.ih

butt.do: butt.o

buttview.do: buttview.o

clean:
	rm -f *.o *.do *.eh *.ih *.BAK *.CKP core

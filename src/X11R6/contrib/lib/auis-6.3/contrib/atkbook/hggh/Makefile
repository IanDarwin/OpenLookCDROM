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

hggh: hgghapp.do butt.do
	-ln -s ${ANDREWDIR}/bin/runapp hggh

butt.o: butt.eh

butt.do: butt.o

hgghapp.o: hgghapp.eh butt.ih

hgghapp.do:  hgghapp.o 

clean:
	rm -f *.o *.do *.eh *.ih *.BAK *.CKP hggh core

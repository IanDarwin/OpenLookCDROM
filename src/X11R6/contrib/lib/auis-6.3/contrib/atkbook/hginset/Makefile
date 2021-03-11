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

all: buttlinks hggh.do hgghview.do hgghapp.do

buttlinks:
	-ln -s ../buttons/butt.ih
	-ln -s ../buttons/buttview.ih
	-ln -s ../buttons/butt.do
	-ln -s ../buttons/buttview.do

hgghapp.o: hgghapp.eh hgghview.ih hggh.ih

hgghapp.do:  hgghapp.o

hggh.o: hggh.eh

hgghview.o: hgghview.eh hggh.ih buttview.ih

hggh.do: hggh.o

hgghview.do: hgghview.o

clean:
	rm -f *.o *.do *.eh *.ih *.BAK *.CKP core

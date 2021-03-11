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

all: fancy.do fancier.do fontanim.do

fancy.do:  fancy.eh fancy.o

fancy.o: fancy.eh

fontanim.do:  fontanim.eh fontanim.o

fontanim.o: fontanim.eh

fancier.do:  fancier.eh fancier.o

fancier.o: fancier.eh fancy.ih

clean:
	rm -f *.o *.do *.eh *.ih *.BAK *.CKP hw core

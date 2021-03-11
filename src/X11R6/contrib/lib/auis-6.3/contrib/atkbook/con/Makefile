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

all: getstats clockob.do     clocksec.do    conob.do       conview.do   contimer.do statsob.do ekgview.do  clogview.do trouble.do  

clockob.do: clockob.o
clockob.eh: conob.ih clockob.ch
clockob.o: clockob.eh contimer.ih conob.ih clockob.c
clocksec.do: clocksec.o
clocksec.eh:  clockob.ih conob.ih clocksec.ch
clocksec.o: clocksec.eh clockob.ih contimer.ih conob.ih  clocksec.c
conob.do: conob.o
conob.o: conob.eh conob.c
conview.do: conview.o
conview.o: conview.eh conob.ih conview.c
contimer.do: contimer.o
contimer.o: contimer.eh conob.ih contimer.c
ekgview.do: ekgview.o
ekgview.eh:  conview.ih ekgview.ch 
ekgview.o:  ekgview.eh conview.ih conob.ih ekgview.c
getstats.o: getstats.h
statsob.do: statsob.o
statsob.eh: conob.ih statsob.ch
statsob.o: statsob.eh conob.ih statsob.c getstats.h
clogview.do: clogview.o
clogview.eh: conview.ih clogview.ch
clogview.o:  clogview.eh conob.ih clogview.c
trouble.do: trouble.o
trouble.eh: conob.ih trouble.ch
trouble.o:  trouble.eh conob.ih getstats.h statsob.ih contimer.ih trouble.c

getstats: getstats.o 
	${CC} -g getstats.o  -o getstats

clean:
	rm -f *.o *.do *.eh *.ih *.BAK *.CKP getstats core

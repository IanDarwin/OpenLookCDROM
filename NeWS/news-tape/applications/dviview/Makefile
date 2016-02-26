NEWSHOME = /usr/NeWS
CFLAGS = -O -I${NEWSHOME}/include
all: psview psmung dviview

clean:
	rm -f core *~ *.BAK *.o psview.h psview psmung

psview: psview.c psview.h
	cc ${CFLAGS} -o psview psview.c ${NEWSHOME}/lib/libcps.a

psview.h: psview.cps
	cps psview.cps

psmung: psmung.c
	cc ${CFLAGS} -o psmung psmung.c


mchoose: mchoose.c mchoose.h
	cc -o mchoose mchoose.c /usr/NeWS/lib/libcps.a -I/usr/NeWS/include -I.

mchoose.h: mchoose.cps
	cps mchoose.cps

clean:
	rm -f mchoose mchoose.o mchoose.h core *~ *.BAK

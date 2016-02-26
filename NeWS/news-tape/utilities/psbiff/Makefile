psbiff: psbiff.c psbiff.h
	cc -o psbiff psbiff.c /usr/NeWS/lib/libcps.a -I/usr/NeWS/include -I.

psbiff.h: psbiff.cps
	cps psbiff.cps

clean:
	rm -f psbiff *.o psbiff.h core *~ *.BAK

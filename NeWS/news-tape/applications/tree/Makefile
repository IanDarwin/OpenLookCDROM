treeedit: treeedit.c treeedit.h
	cc -o treeedit treeedit.c /usr/NeWS/lib/libcps.a -I/usr/NeWS/include

treeedit.h: treeedit.cps
	cps treeedit.cps

clean:
	rm -f treeedit treeedit.o treeedit.h core *~ *.BAK

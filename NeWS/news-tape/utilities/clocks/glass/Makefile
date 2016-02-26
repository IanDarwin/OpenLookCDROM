glass: glass.c glass.h
	cc -o glass glass.c /usr/NeWS/lib/libcps.a -I/usr/NeWS/include -I.

glass.h: glass.cps
	cps glass.cps

clean:
	rm -f glass glass.o glass.h core *~ *.BAK

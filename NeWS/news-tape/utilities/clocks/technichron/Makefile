technichron: technichron.c technichron.h
	cc -o technichron technichron.c /usr/NeWS/lib/libcps.a -I/usr/NeWS/include -I.

technichron.h: technichron.cps
	cps technichron.cps

clean:
	rm -f technichron technichron.o technichron.h core *~ *.BAK

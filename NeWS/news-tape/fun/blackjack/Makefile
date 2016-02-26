blackjack: black.c black.h
	cc -o blackjack black.c /usr/NeWS/lib/libcps.a -I/usr/NeWS/include -I.

black.h: black.cps
	cps black.cps

clean:
	rm -f blackjack black.o black.h core *~ *.BAK

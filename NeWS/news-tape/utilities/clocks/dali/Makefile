CLSRC=/usr/NeWS/clientsrc/client

.SUFFIXES: .h .cps .diff

.cps.h:
	cps $*.cps

CFLAGS = -O -I/usr/NeWS/include

ALL: daliclock

daliclock: daliclock.o
	cc -o $@ daliclock.o /usr/NeWS/lib/libcps.a

daliclock.o: daliclock.c daliclock.h

daliclock.cps:
	cat $(CLSRC)/roundclock.cps >daliclock.cps
	ed daliclock.cps <cps.diff

daliclock.c:
	cat $(CLSRC)/roundclock.c >daliclock.c
	ed daliclock.c <c.diff
	
shar: Makefile daliclock.c daliclock.cps
	{ { diff -e $(CLSRC)/roundclock.cps daliclock.cps||true;};echo w;} >cps.diff
	{ { diff -e $(CLSRC)/roundclock.c daliclock.c||true;};echo w;} >c.diff
	{ echo "#!/bin/sh"; \
	  echo CLSRC="$(CLSRC)"; \
	  for F in Makefile c.diff cps.diff; do \
	    echo "if test -f $$F; then echo "File $$F exists!"; exit 1; fi"; \
	    echo "sed -e 's/^X//' <<'EOF' >$$F"; \
	    sed -e 's/^/X/' <$$F; \
	    echo "EOF"; \
	  done; \
	} > shar

clean:
	rm -f daliclock daliclock.o daliclock.h core *~ *.BAK

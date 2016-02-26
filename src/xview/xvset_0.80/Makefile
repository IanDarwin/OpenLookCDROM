PROGNAME = xvset
VERSION = 0.80
# MUST BE an ANSI C compiler
CC = cc
#CC = gcc
#CC = acc
CFLAGS = -O -g
# SYSDEP_CFLAGS = -I/usr/openwin/include
# SYSDEP_CFLAGS = -m486
SYSDEP_CFLAGS = -I/usr/openwin/include -m486
DEFINES =
LIBS= -lxview -lolgx -lX11
OPENWINHOME=/usr/openwin
BINDIR=$(OPENWINHOME)/bin
MANDIR=$(OPENWINHOME)/man/man1
HELPDIR=$(OPENWINHOME)/lib/help

all: linux

linux:
	$(CC) $(CFLAGS) $(SYSDEP_CFLAGS) $(PROGNAME).c -L$(OPENWINHOME)/lib $(LIBS) -o $(PROGNAME)

sunos:
	$(CC) $(CFLAGS) $(PROGNAME).c -I$(OPENWINHOME)/include -L$(OPENWINHOME)/lib $(LIBS) -o $(PROGNAME)

clean:
	rm -f core $(PROGNAME)

dist: clean
	(cd .. && tar zcf $(PROGNAME)_$(VERSION).tar.gz $(PROGNAME)_$(VERSION))

install: all
	install -m 755 $(PROGNAME) $(BINDIR)

install.man:
	[ -f $(PROGNAME).1 ] && install -m 444 $(PROGNAME).1 $(MANDIR)
	[ -f $(PROGNAME).info ] && install -m 444 $(PROGNAME).info $(HELPDIR)

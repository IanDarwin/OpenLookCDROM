# This is the Makefile for et2hku

CC = cc
CFLAGS = -O

INSTALL = cp

BINDIR = /usr/local/bin
MANDIR = /usr/local/man/man1

all: et2hku

et2hku: et2hku.c
	$(CC) $(CFLAGS) -o et2hku et2hku.c

install:
	$(INSTALL) et2hku $(BINDIR)

install.man:
	$(INSTALL) et2hku.1 $(MANDIR)

clean:
	rm -f et2hku core a.out *BAK *CKP

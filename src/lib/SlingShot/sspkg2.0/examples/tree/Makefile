 
#       @(#) Makefile 1.2 92/06/24 

SSPKGHOME	= ../..

LIBDIR		= $(OPENWINHOME)/lib
LIBS		= -lxview -lolgx -lX11
SSPKGLIB	= $(SSPKGHOME)/lib/libsspkg.a

.KEEP_STATE:

DBXFLAGS	=  -g 
CFLAGS		= $(DBXFLAGS) -I$(SSPKGHOME)/include -I$(OPENWINHOME)/include 
LDFLAGS		= -L$(LIBDIR) $(LIBS) $(SSPKGLIB) -lm

all: tlstool treetool

lex.yy.c: treetool.l
	lex treetool.l

lex.yy.o: lex.yy.c

treetool: lex.yy.o $(SSPKGLIB)
	cc -o $@ lex.yy.o $(LDFLAGS)

tlstool: tlstool.o $(SSPKGLIB)
	cc -o $@ tlstool.o $(LDFLAGS)


clean:
	-sccs clean
	rm -f *.o tlstool treetool lex.yy.c


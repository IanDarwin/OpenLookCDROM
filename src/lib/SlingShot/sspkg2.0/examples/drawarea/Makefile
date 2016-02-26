 
#       @(#) Makefile 1.4 92/10/21 

SSPKGHOME	= ../..

LIBDIR		= $(OPENWINHOME)/lib
LIBS		= -lxview -lolgx -lX11
SSPKGLIB	= $(SSPKGHOME)/lib/libsspkg.a

.KEEP_STATE:

DBXFLAGS	=  -g 
CFLAGS		= $(DBXFLAGS) -I$(SSPKGHOME)/include -I$(OPENWINHOME)/include 
LDFLAGS		= -L$(LIBDIR) $(LIBS) $(SSPKGLIB) -lm

all: darea1 darea2 hist hist_stdin custom

darea1: darea1.o
	cc -o $@ darea1.o $(LDFLAGS)

darea2: darea2.o
	cc -o $@ darea2.o $(LDFLAGS)

hist: hist.o
	cc -o $@ hist.o $(LDFLAGS)

hist_stdin: hist_stdin.o
	cc -o $@ hist_stdin.o $(LDFLAGS)

custom: custom.o
	cc -o $@ custom.o $(LDFLAGS)

clean:
	-sccs clean
	rm -f *.o darea1 darea2 hist hist_stdin custom


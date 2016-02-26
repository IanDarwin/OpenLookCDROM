 
#       @(#) Makefile 1.4 92/11/12 

SSPKGHOME	= ../..

LIBDIR		= $(OPENWINHOME)/lib
LIBS		= -lxview -lolgx -lX11
SSPKGLIB	= $(SSPKGHOME)/lib/libsspkg.a


DBXFLAGS	=  -g 
CFLAGS		= $(DBXFLAGS) -I$(SSPKGHOME)/include -I$(OPENWINHOME)/include 
LDFLAGS		= -L$(LIBDIR) $(LIBS) $(SSPKGLIB) -lm

all: drawrect example1 

drawrect: drawrect.o
	cc -o $@ drawrect.o $(LDFLAGS)

example1: example1.o
	cc -o $@ example1.o $(LDFLAGS)

clean:
	-sccs clean
	rm -f *.o drawrect example1

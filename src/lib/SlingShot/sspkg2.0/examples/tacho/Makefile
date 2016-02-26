 
#       @(#) Makefile 1.3 92/10/22 

SSPKGHOME	= ../..

LIBDIR		= $(OPENWINHOME)/lib
LIBS		= -lxview -lolgx -lX11
SSPKGLIB	= $(SSPKGHOME)/lib/libsspkg.a


DBXFLAGS	=  -g 
CFLAGS		= $(DBXFLAGS) -I$(SSPKGHOME)/include -I$(OPENWINHOME)/include 
LDFLAGS		= -L$(LIBDIR) $(LIBS) $(SSPKGLIB) -lm

all: tacho

tacho: tacho.o
	cc -o $@ tacho.o $(LDFLAGS)

clean:
	-sccs clean
	rm -f *.o tacho

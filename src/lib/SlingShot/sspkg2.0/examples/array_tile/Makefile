 
#       @(#) Makefile 1.14 92/10/15 

SSPKGHOME	= ../..

LIBDIR		= $(OPENWINHOME)/lib
LIBS		= -lxview -lolgx -lX11
SSPKGLIB	= $(SSPKGHOME)/lib/libsspkg.a

.KEEP_STATE:

DBXFLAGS	=  -g 
CFLAGS		= $(DBXFLAGS) -I$(SSPKGHOME)/include -I$(OPENWINHOME)/include 
LDFLAGS		= -L$(LIBDIR) $(LIBS) $(SSPKGLIB) -lm

all: array_ex

array_ex: array_ex.o
	cc -o $@ array_ex.o $(LDFLAGS)

clean:
	-sccs clean
	rm -f *.o array_ex


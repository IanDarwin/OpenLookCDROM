 
#       @(#) Makefile 1.2 92/10/21 

SSPKGHOME	= ../..

LIBDIR		= $(OPENWINHOME)/lib
LIBS		= -lxview -lolgx -lX11
SSPKGLIB	= $(SSPKGHOME)/lib/libsspkg.a

.KEEP_STATE:

DBXFLAGS	=  -g 
CFLAGS		= $(DBXFLAGS) -I$(SSPKGHOME)/include -I$(OPENWINHOME)/include 
LDFLAGS		= -L$(LIBDIR) $(LIBS) $(SSPKGLIB) -lm

all: drop_ex drag_ex

drop_ex: drop_ex.o
	cc -o $@ drop_ex.o $(LDFLAGS)

drag_ex: drag_ex.o
	cc -o $@ drag_ex.o $(LDFLAGS)
clean:
	-sccs clean
	rm -f *.o drop_ex drag_ex


 
#       @(#) Makefile 1.5 92/10/27 

SSPKGHOME	= ../..

LIBDIR		= $(OPENWINHOME)/lib
LIBS		= -lxview -lolgx -lX11
SSPKGLIB	= $(SSPKGHOME)/lib/libsspkg.a

.KEEP_STATE:

DBXFLAGS	=  -g 
CFLAGS		= $(DBXFLAGS) -I$(SSPKGHOME)/include -I$(OPENWINHOME)/include 
LDFLAGS		= -L$(LIBDIR) $(LIBS) $(SSPKGLIB) -lm -lce 

include ../../Make.config

all: icon_dnd 

icon_dnd: icon_dnd.o cei.o
	cc -o $@ icon_dnd.o cei.o $(LDFLAGS) $(NSLLIB)

test_cei:	cei.c cei.h
	$(CC) -o $@ cei.c -DTEST $(CFLAGS) $(LDFLAGS)

icon_dnd.o: icon_dnd.c cei.h
	$(CC) $(CFLAGS) $(SVR4FLAG) -c icon_dnd.c -o $@

clean:
	-sccs clean
	rm -f *.o icon_dnd


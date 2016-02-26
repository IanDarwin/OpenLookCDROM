 
#       @(#) Makefile 1.2 92/06/22 

SSPKGHOME	= ../..

LIBDIR		= $(OPENWINHOME)/lib
LIBS		= -lxview -lolgx -lX11
SSPKGLIB	= $(SSPKGHOME)/lib/libsspkg.a

.KEEP_STATE:

DBXFLAGS	=  -g 
CFLAGS		= $(DBXFLAGS) -I$(SSPKGHOME)/include -I$(OPENWINHOME)/include 
LDFLAGS		= -L$(LIBDIR) $(LIBS) $(SSPKGLIB) -lm

all:	amper.icon banana.icon bananamask.icon \
	bully.icon.aa bully.icon.ab bully.icon.ac \
	bully.icon.ad bully.icon.ae bully.icon.af \
	demo1.icon demo2.icon demo3.icon demomask.icon \
	smiley.icon frown.icon puzzle_demo.icon

#	sccs get $@

clean:
	-sccs clean


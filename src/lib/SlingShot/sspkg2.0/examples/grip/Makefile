 
#       @(#) Makefile 1.6 92/11/12 

SSPKGHOME	= ../..

LIBDIR		= $(OPENWINHOME)/lib
LIBS		= -lxview -lolgx -lX11
SSPKGLIB	= $(SSPKGHOME)/lib/libsspkg.a


DBXFLAGS	=  -g 
CFLAGS		= $(DBXFLAGS) -I$(SSPKGHOME)/include -I$(OPENWINHOME)/include 
LDFLAGS		= -L$(LIBDIR) $(LIBS) $(SSPKGLIB) -lm

all: gripdemo puzzdemo nonlin temp_grip

gripdemo: gripdemo.o
	cc -o $@ gripdemo.o $(LDFLAGS)

puzzdemo: puzzdemo.o
	cc -o $@ puzzdemo.o $(LDFLAGS)

nonlin: nonlin.o
	cc -o $@ nonlin.o $(LDFLAGS)

temp_grip: temp_grip.o
	cc -o $@ temp_grip.o $(LDFLAGS)

clean:
	-sccs clean
	rm -f *.o puzzdemo gripdemo nonlin temp_grip


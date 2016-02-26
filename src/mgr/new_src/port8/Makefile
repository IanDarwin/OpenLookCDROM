#                        Copyright (c) 1988,1989 Bellcore
#                            All Rights Reserved
#       Permission is granted to copy or use this program, EXCEPT that it
#       may not be sold for profit, the copyright notice must be reproduced
#       on copies, and credit should be given to Bellcore where it is due.
#       BELLCORE MAKES NO WARRANTY AND ACCEPTS NO LIABILITY FOR THIS PROGRAM.

#	$Header: /home/sau/mgr/nsrc/port8/RCS/Makefile,v 1.2 91/03/01 11:55:15 sau Exp Locker: sau $
#	$Source: /home/sau/mgr/nsrc/port8/RCS/Makefile,v $

#	makefile for portable blit library (S. A. Uhler)

#	Compile flags
#	  INVERT	- invert the sense of black and white
#	  UNROLL - Unroll the inner loops
#	  NOCLIP	- Don't do clipping on bit-blits

FLAG=-g
START=.
CFLAGS= $(FLAG) -I. -DUNROLL
CFLAGS= $(FLAG) -I.
CPP=/lib/cpp -P -C

#	generic routines
GENERICOBJS= blit.o line.o pixel.o bitmap.o
GENERICSRCS= blit.C line.c pixel.c bitmap.c

# device dependent routines for memory only versions
DEVOBJS= generic.o

#device dependent routines for suns
DEVOBJS= sun.o

# all of the device specific srcs
DEVSRCS = sun.c generic.c

# Other files needed for the source distribution
OTHER= Semantics README Makefile bitmap.h

##############################################################33

LIBOBJS= $(GENERICOBJS) $(DEVOBJS)


#  Blit.C contains the templates for building the bitblt code.  It is
#  run through cpp twice, once to expand the templates, again as part of
#  the normal compilation process.  The expansion level picks the amount
#  of macro expansion that is done in the 1st cpp pass, to aid in debugging

#	The template expansion level is specified here for building blit.c
#	0			leave all the templates intact
#	1			expand the switches
#	12			expand the bitblt templates
#	123		expand the inner loops
#	1234		expand the bit shifts
#	12345		expand the bitwise operators
#	%			Normal CPP defines/includes
#	#			These always get expanded on the first pass
PASS1=0

INDENT=indent -st
#	Indent doesn't always generate valid C code
INDENT = cat

blitlib.a:		$(LIBOBJS)
					rm -f blitlib.a
					ar r blitlib.a $(LIBOBJS)
					ranlib blitlib.a

rops:		rops.o bitmap.o blit.o
					$(CC) $(CFLAGS) -o rops rops.o blit.o bitmap.o

test_shrink:		test_shrink.o  blitlib.a
					$(CC) $(CFLAGS) -o test_shrink test_shrink.o blitlib.a

test_rop:		test_rop.o bitmap.o blit.o
					$(CC) $(CFLAGS) -o test_rop test_rop.o blit.o bitmap.o

blit.c:		blit.C
				< blit.C sed "s/^[$(PASS1)]/#/" | \
					$(CPP) | \
					sed 's/^[1-9%]/#/' | \
					$(INDENT) | \
					cat -s \
				> blit.c

$(LIBOBJS):	bitmap.h

tex:		tex.o expand.o blitlib.a ../bitmaphead.o ../bitmapread.o
				$(CC) $(CFLAGS) -o tex tex.o expand.o ../bitmaphead.o \
					../bitmapread.o blitlib.a

test1:		test1.o blitlib.a
				$(CC) $(CFLAGS) -o test1 test1.o blitlib.a

list:
	@for i in $(GENERICSRCS) $(DEVSRCS) $(OTHER) ; do \
		echo "${START}/$$i"; \
	done	

clean:
	-rm -f *.o core blit.c

clobber:	clean
	-rm -f blitlib.a

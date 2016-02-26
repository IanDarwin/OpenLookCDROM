#                        Copyright (c) 1988,1989 Bellcore
#                            All Rights Reserved
#       Permission is granted to copy or use this program, EXCEPT that it
#       may not be sold for profit, the copyright notice must be reproduced
#       on copies, and credit should be given to Bellcore where it is due.
#       BELLCORE MAKES NO WARRANTY AND ACCEPTS NO LIABILITY FOR THIS PROGRAM.

#	$Header: Makefile,v 1.2 89/05/23 08:33:45 sau Exp $
#	$Source: /mnt/sau/bitblt/DEC/RCS/Makefile,v $

#	makefile for portable blit library (S. A. Uhler)

#	Compile flags
#	  INVERT	- invert the sense of black and white
#	  UNROLL - Unroll the inner loops
#	  NOCLIP	- Don't do clipping on bit-blits
#	  MOVIE	- do mgr movie logging stuff

PASS=-DMOVIE
FLAG=-g
START=.
CFLAGS= $(FLAG) $(PASS) -I. -DUNROLL
CPP=/lib/cpp -P -C
INSDIR=../bin

LIBOBJS=bitmap.o blit.o line.o pixel.o share.o timestamp.o

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
					-rm -f blitlib.a
					ar r blitlib.a $(LIBOBJS)
					ranlib blitlib.a

rops:		rops.o bitmap.o blit.o
					$(CC) $(CFLAGS) -o rops rops.o blit.o bitmap.o

shrink_map:		shrink_map.o
					$(CC) $(CFLAGS) -DTEST -o shrink_map shrink_map.o  \
						../bitmapread.o ../bitmaphead.o ../port/blitlib.a

test_rop:		test_rop.o bitmap.o blit.o
					$(CC) $(CFLAGS) -o test_rop test_rop.o blit.o bitmap.o

blit.c:		blit.C
				< blit.C sed "s/^[$(PASS1)]/#/" | \
					$(CPP) | \
					sed 's/^[1-9%]/#/' | \
					$(INDENT) | \
					cat -s \
				> blit.c

bitmap.o blit.o line.o pixel.o:	bitmap.h

list:
	@for i in blit.C bitmap.h bitmap.c rops.c test_rop.c Makefile line.c \
			 pixel.c share.h share.c timestamp.c README \
			 ; do \
		echo "${START}/$$i"; \
	done	

clean:
	-rm -f *.o core blit.c

clobber:
	-rm -f blitlib.a

$(LIBOBJS):		share.h bitmap.h

install:

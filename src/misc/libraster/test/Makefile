# Makefile for raster test programs
#
# Copyright (C) 1991, 1993 by Jef Poskanzer <jef@netcom.com>.
#
# Permission to use, copy, modify, and distribute this software and its
# documentation for any purpose and without fee is hereby granted, provided
# that the above copyright notice appear in all copies and that both that
# copyright notice and this permission notice appear in supporting
# documentation.  This software is provided "as is" without express or
# implied warranty.

# CONFIGURE: location of the raster include files
RASTER_INC_DIR =	..
#RASTER_INC_DIR =	/usr/include
#RASTER_INC_DIR =	/usr/local/include

# CONFIGURE: location of the raster library
RASTER_LIB_DIR =	..
#RASTER_LIB_DIR =	/usr/lib
#RASTER_LIB_DIR =	/usr/local/lib

# CONFIGURE: your favorite C compiler
CC =		cc
#CC =		gcc -W -ansi -pedantic -fpcc-struct-return
#CC =		gcc -W -ansi -pedantic -fpcc-struct-return -fstrength-reduce -finline-functions

# CONFIGURE: your favorite C flags
#CFLAGS =	-g
CFLAGS =	-O
#CFLAGS =	-g -O

# CONFIGURE: your favorite loader flags
LDFLAGS =	-s
#LDFLAGS =	

# CONFIGURE: your favorite lint flags
LINTFLAGS =	-hxz

# CONFIGURE: uncomment one of the following defines according to your
# operating system type.
#
# For SunOS 3.x:
#SYSDEFS =	-DOLD_FBIO_H_LOC -DNO_POLL -DNO_STDLIB_H
# For SunOS 4.x (Solaris 1.x):
#SYSDEFS =	-DOLD_FBIO_H_LOC
# For Solaris 2.x (SunOS 5.x):
SYSDEFS =	

# End of configurable definitions.

INCLUDES =	-I$(RASTER_INC_DIR)
DEFINES =	$(SYSDEFS)
LIBRASTER =	-L$(RASTER_LIB_DIR) -lraster
DEP_LIBRASTER =	$(RASTER_LIB_DIR)/libraster.a

SRCS =		memcpy.c chars.c dumpfont.c dumprast.c fbtype.c melt.c nosrc.c overlap.c rot.c scroll.c splines.c squares.c text.c ttext.c
EXES =		memcpy chars dumpfont dumprast fbtype melt nosrc overlap rot scroll splines squares text ttext

ALLCFLAGS =	$(CFLAGS) $(INCLUDES) $(DEFINES)
ALLLINTFLAGS =	$(LINTFLAGS) $(INCLUDES) $(DEFINES)

all:		$(EXES)

.c.o:
	$(CC) $(ALLCFLAGS) -c $*.c

memcpy:		memcpy.o
	$(CC) $(CFLAGS) $(LDFLAGS) memcpy.o -o memcpy
memcpy.o:	memcpy.c

chars:		chars.o $(DEP_LIBRASTER)
	$(CC) $(CFLAGS) $(LDFLAGS) chars.o $(LIBRASTER) -o chars
chars.o:	chars.c $(RASTER_INC_DIR)/raster.h

dumpfont:	dumpfont.o $(DEP_LIBRASTER)
	$(CC) $(CFLAGS) $(LDFLAGS) dumpfont.o $(LIBRASTER) -o dumpfont
dumpfont.o:	dumpfont.c $(RASTER_INC_DIR)/raster.h

dumprast:	dumprast.o $(DEP_LIBRASTER)
	$(CC) $(CFLAGS) $(LDFLAGS) dumprast.o $(LIBRASTER) -o dumprast
dumprast.o:	dumprast.c $(RASTER_INC_DIR)/raster.h

fbtype:		fbtype.o
	$(CC) $(CFLAGS) $(LDFLAGS) fbtype.o -o fbtype
fbtype.o:	fbtype.c

melt:		melt.o $(DEP_LIBRASTER)
	$(CC) $(CFLAGS) $(LDFLAGS) melt.o $(LIBRASTER) -o melt
melt.o:		melt.c $(RASTER_INC_DIR)/raster.h

nosrc:		nosrc.o $(DEP_LIBRASTER)
	$(CC) $(CFLAGS) $(LDFLAGS) nosrc.o $(LIBRASTER) -o nosrc
nosrc.o:	nosrc.c $(RASTER_INC_DIR)/raster.h

overlap:	overlap.o $(DEP_LIBRASTER)
	$(CC) $(CFLAGS) $(LDFLAGS) overlap.o $(LIBRASTER) -o overlap
overlap.o:	overlap.c $(RASTER_INC_DIR)/raster.h

rot:		rot.o $(DEP_LIBRASTER)
	$(CC) $(CFLAGS) $(LDFLAGS) rot.o $(LIBRASTER) -o rot
rot.o:		rot.c $(RASTER_INC_DIR)/raster.h

scroll:		scroll.o $(DEP_LIBRASTER)
	$(CC) $(CFLAGS) $(LDFLAGS) scroll.o $(LIBRASTER) -o scroll
scroll.o:	scroll.c $(RASTER_INC_DIR)/raster.h

splines:	splines.o $(DEP_LIBRASTER)
	$(CC) $(CFLAGS) $(LDFLAGS) splines.o $(LIBRASTER) -o splines
splines.o:	splines.c $(RASTER_INC_DIR)/raster.h

squares:	squares.o $(DEP_LIBRASTER)
	$(CC) $(CFLAGS) $(LDFLAGS) squares.o $(LIBRASTER) -o squares
squares.o:	squares.c $(RASTER_INC_DIR)/raster.h

text:		text.o $(DEP_LIBRASTER)
	$(CC) $(CFLAGS) $(LDFLAGS) text.o $(LIBRASTER) -o text
text.o:		text.c $(RASTER_INC_DIR)/raster.h

ttext:		ttext.o $(DEP_LIBRASTER)
	$(CC) $(CFLAGS) $(LDFLAGS) ttext.o $(LIBRASTER) -o ttext
ttext.o:	ttext.c $(RASTER_INC_DIR)/raster.h

install:
	# Nothing - these are just test programs.

lint:
	for i in $(SRCS) ; do \
	    echo $$i ; \
	    lint $(ALLLINTFLAGS) $$i | sed -e '/but never used or defined/d' -e '/but not defined/d' -e '/but never used/d' ; \
	done

clean:
	rm -f $(EXES) *.o a.out core

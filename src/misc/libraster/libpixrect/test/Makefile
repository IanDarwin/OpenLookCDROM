# Makefile for pixrect test programs
#
# Copyright (C) 1991, 1993 by Jef Poskanzer <jef@netcom.com>.
#
# Permission to use, copy, modify, and distribute this software and its
# documentation for any purpose and without fee is hereby granted, provided
# that the above copyright notice appear in all copies and that both that
# copyright notice and this permission notice appear in supporting
# documentation.  This software is provided "as is" without express or
# implied warranty.

# CONFIGURE: location of the dir above the pixrect include files
PIXRECT_INC_DIR =	..
#PIXRECT_INC_DIR =	/usr/include
#PIXRECT_INC_DIR =	/usr/local/include

# CONFIGURE: location of the pixrect library
PIXRECT_LIB_DIR =	..
#PIXRECT_LIB_DIR =	/usr/lib
#PIXRECT_LIB_DIR =	/usr/local/lib

# CONFIGURE: location of the raster library
RASTER_LIB_DIR =        ../..
#RASTER_LIB_DIR =       /usr/lib
#RASTER_LIB_DIR =       /usr/local/lib

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

INCLUDES =	-I$(PIXRECT_INC_DIR)
DEFINES =	$(SYSDEFS)
LIBPIXRECT =	-L$(PIXRECT_LIB_DIR) -lpixrect
DEP_LIBPIXRECT = $(PIXRECT_LIB_DIR)/libpixrect.a
LIBRASTER =	-L$(RASTER_LIB_DIR) -lraster
DEP_LIBRASTER = $(RASTER_LIB_DIR)/libraster.a

SRCS =		meltdown.c pchars.c prot.c pscroll.c ptext.c
EXES =		meltdown pchars prot pscroll ptext

ALLCFLAGS =	$(CFLAGS) $(INCLUDES) $(DEFINES)
ALLLINTFLAGS =	$(LINTFLAGS) $(INCLUDES) $(DEFINES)

all:		$(EXES)

.c.o:
	$(CC) $(ALLCFLAGS) -c $*.c

meltdown:		meltdown.o $(DEP_LIBPIXRECT) $(DEP_LIBRASTER)
	$(CC) $(CFLAGS) $(LDFLAGS) meltdown.o $(LIBPIXRECT) $(LIBRASTER) -o meltdown
meltdown.o:	meltdown.c $(PIXRECT_INC_DIR)/pixrect/pixrect.h

pchars:		pchars.o $(DEP_LIBPIXRECT) $(DEP_LIBRASTER)
	$(CC) $(CFLAGS) $(LDFLAGS) pchars.o $(LIBPIXRECT) $(LIBRASTER) -o pchars
pchars.o:	pchars.c $(PIXRECT_INC_DIR)/pixrect/pixrect.h $(PIXRECT_INC_DIR)/pixrect/pixfont.h

prot:		prot.o $(DEP_LIBPIXRECT) $(DEP_LIBRASTER)
	$(CC) $(CFLAGS) $(LDFLAGS) prot.o $(LIBPIXRECT) $(LIBRASTER) -o prot
prot.o:		prot.c $(PIXRECT_INC_DIR)/pixrect/pixrect.h

pscroll:	pscroll.o $(DEP_LIBPIXRECT) $(DEP_LIBRASTER)
	$(CC) $(CFLAGS) $(LDFLAGS) pscroll.o $(LIBPIXRECT) $(LIBRASTER) -o pscroll
pscroll.o:	pscroll.c $(PIXRECT_INC_DIR)/pixrect/pixrect.h

ptext:		ptext.o $(DEP_LIBPIXRECT) $(DEP_LIBRASTER)
	$(CC) $(CFLAGS) $(LDFLAGS) ptext.o $(LIBPIXRECT) $(LIBRASTER) -o ptext
ptext.o:	ptext.c $(PIXRECT_INC_DIR)/pixrect/pixrect.h $(PIXRECT_INC_DIR)/pixrect/pixfont.h

install:
	# Nothing - these are just test programs.

lint:
	for i in $(SRCS) ; do \
	    echo $$i ; \
	    lint $(ALLLINTFLAGS) $$i | sed -e '/but never used or defined/d' -e '/but not defined/d' -e '/but never used/d' ; \
	done

clean:
	rm -f $(EXES) *.o a.out core

# Makefile for spf3
#
# Copyright (C) 1993 by Jef Poskanzer <jef@netcom.com>.
#
# Permission to use, copy, modify, and distribute this software and its
# documentation for any purpose and without fee is hereby granted, provided
# that the above copyright notice appear in all copies and that both that
# copyright notice and this permission notice appear in supporting
# documentation.  This software is provided "as is" without express or
# implied warranty.

# CONFIGURE: Set this to the directory where you want executables to go.
BININSTDIR =		/usr/local/bin

# CONFIGURE: Set this to the manual area you want.  Standard subdirectory
# organization is assumed - section 1 for executables, section 3 for libraries.
#MANINSTDIR =		/usr/man
MANINSTDIR =		/usr/local/man

# CONFIGURE: location of the raster include file
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
#CC =		gcc -W -ansi -pedantic -fpcc-struct-return -fcombine-regs -fstrength-reduce -finline-functions

# CONFIGURE: your favorite C flags
#CFLAGS =	-g
CFLAGS =	-O
#CFLAGS =	-g -O

# CONFIGURE: your favorite loader flags
LDFLAGS =	-s
#LDFLAGS =	

# CONFIGURE: your favorite lint flags.
LINTFLAGS =	-hxz

# CONFIGURE: uncomment one of the following defines according to your
# operating system type.
#
# For SunOS 3.x:
#SYSDEFS =	-DNO_STDLIB_H
# For SunOS 4.x (Solaris 1.x):
#SYSDEFS =
# For Solaris 2.x (SunOS 5.x):
SYSDEFS =

# End of configurable definitions.

INCLUDES =	-I$(RASTER_INC_DIR)
DEFINES =	$(SYSDEFS)
ALLCFLAGS =	$(CFLAGS) $(INCLUDES) $(DEFINES)
ALLLINTFLAGS =	$(LINTFLAGS) $(INCLUDES) $(DEFINES)
LIBRASTER =	-L$(RASTER_LIB_DIR) -lraster
DEP_LIBRASTER =	$(RASTER_LIB_DIR)/libraster.a

all:		spf3

spf3:		spf3.o $(DEP_LIBRASTER)
	$(CC) $(ALLCFLAGS) $(LDFLAGS) spf3.o $(LIBRASTER) -o spf3
spf3.o:		spf3.c $(RASTER_INC_DIR)/raster.h
	$(CC) -c $(ALLCFLAGS) spf3.c

install:	all
	install -c spf3 $(BININSTDIR)/spf3
	install -c spf3.1 $(MANINSTDIR)/man1/spf3.1

lint:
	lint $(ALLLINTFLAGS) spf3.c | sed -e '/but never used or defined/d' -e '/but not defined/d' -e '/defined.* but never used/d'

clean:
	rm -f spf3 *.o a.out core

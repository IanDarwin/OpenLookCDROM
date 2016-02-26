# Makefile for libraster
#
# Copyright (C) 1991, 1993 by Jef Poskanzer <jef@netcom.com>.
#
# Permission to use, copy, modify, and distribute this software and its
# documentation for any purpose and without fee is hereby granted, provided
# that the above copyright notice appear in all copies and that both that
# copyright notice and this permission notice appear in supporting
# documentation.  This software is provided "as is" without express or
# implied warranty.

# CONFIGURE: Set this to the directory where you want libraries to go.
#LIBINSTDIR =		/usr/lib
LIBINSTDIR =		/usr/local/lib

# CONFIGURE: Set this to the directory where you want include files to go.
#INCINSTDIR =		/usr/include
INCINSTDIR =		/usr/local/include

# CONFIGURE: Set this to the directory where you want executables to go.
BININSTDIR =		/usr/local/bin

# CONFIGURE: Set this to the manual area you want.  Standard subdirectory
# organization is assumed - section 1 for executables, section 3 for libraries.
#MANINSTDIR =		/usr/man
MANINSTDIR =		/usr/local/man

# CONFIGURE: your favorite C compiler.
CC =		cc
#CC =		gcc -W -ansi -pedantic -fpcc-struct-return
#CC =		gcc -W -ansi -pedantic -fpcc-struct-return -fstrength-reduce -finline-functions

# CONFIGURE: your favorite C flags.
#CFLAGS =	-g
CFLAGS =	-O
#CFLAGS =	-g -O

# CONFIGURE: your favorite loader flags.
LDFLAGS =	-s
#LDFLAGS =	

# CONFIGURE: your favorite lint flags.
LINTFLAGS =	-hxz

# CONFIGURE: uncomment one of the following defines according to your
# operating system type.
#
# For SunOS 3.x:
#SYSDEFS =	-DOLD_FBIO_H_LOC -DOLD_MMAP -DNO_POLL -DNO_STDLIB_H -DNO_SYSCONF -DNO_UNISTD_H
# For SunOS 4.x (Solaris 1.x):
#SYSDEFS =	-DOLD_FBIO_H_LOC -DNO_SYSCONF
# For Solaris 2.x (SunOS 5.x):
SYSDEFS =	

# CONFIGURE: define or comment out for your machine's byte order.
MSBYTE_DEF =	-DMSBYTE_FIRST

# CONFIGURE: define or comment out for your frame buffer's bit order.
MSBIT_DEF =	-DMSBIT_FIRST

# CONFIGURE: The text routines can optionally keep a cache of 8-bit
# characters.  This uses about 30K, but makes text on a color screen
# go three times faster.
CFC_DEF =	-DCOLORFONT_CACHE

# CONFIGURE: To save on executable size, you can configure out the seldom-used
# logical operations.  With this variable set, the only operations implemented
# are: RAS_SRC, RAS_CLEAR, RAS_SET, RAS_INVERT, RAS_XOR, RAS_INVERTSRC.
#PARTOP_DEF =	-DPARTIAL_LOGICAL_OPS

# CONFIGURE: memcpy() is supposed to be the ultimately fastest way to move
# bytes, overlapping or not, ignoring the startup cost.  Unfortunately
# this is not true on some systems.  For example, on a Sun 3 running
# SunOS 3.5, memcpy() is about one-fifth as fast as a simple for-loop
# on overlapping copies.  And on a 4.1.1 SPARC, memcpy() is about 2/3rds
# as fast on backwards overlaps.  So, only define this if your memcpy is
# known to be good.  Use test/memcpy to check this.  If unsure, leave it
# commented out - the for-loop should never be much worse than even
# the best memcpy().
#MEMCPY_DEF =	-DMEMCPY_FASTER

# End of configurable definitions.

MAKE =		make

INCLUDES =	
DEFINES =	$(SYSDEFS) $(MSBYTE_DEF) $(MSBIT_DEF) $(CFC_DEF) $(PARTOP_DEF) $(MEMCPY_DEF)
ALLSRCS =	raster_batch.c raster_dfont.c raster_dump.c \
		raster_file.c raster_font.c raster_getput.c \
		raster_line.c raster_misc.c raster_op.c raster_repl.c \
		raster_spline.c raster_sten.c raster_sys.c raster_text.c
LIBOBJS =	raster_batch.o raster_dfont.o raster_dump.o \
		raster_file.o raster_font.o raster_getput.o \
		raster_line.o raster_misc.o raster_op.o raster_repl.o \
		raster_spline.o raster_sten.o raster_sys.o raster_text.o
LIBS =		libraster.a

ALLCFLAGS =	$(CFLAGS) $(INCLUDES) $(DEFINES)
ALLLINTFLAGS =	$(LINTFLAGS) $(INCLUDES) $(DEFINES)

SUBDIRS =	test screendump screenload spf3 squig libpixrect rconsole

all:		$(LIBS) allsubdirs

libraster.a:	$(LIBOBJS)
	rm -f libraster.a
	ar rc libraster.a $(LIBOBJS)
	-ranlib libraster.a

.c.o:
	$(CC) $(ALLCFLAGS) -c $*.c

raster_batch.o:		raster_batch.c raster.h
raster_dfont.o:		raster_dfont.c raster.h gallant19.h
raster_dump.o:		raster_dump.c raster.h
raster_file.o:		raster_file.c raster.h
raster_font.o:		raster_font.c raster.h
raster_getput.o:	raster_getput.c raster.h
raster_line.o:		raster_line.c raster.h
raster_misc.o:		raster_misc.c raster.h
raster_op.o:		raster_op.c raster.h
raster_repl.o:		raster_repl.c raster.h
raster_spline.o:	raster_spline.c raster.h
raster_sten.o:		raster_sten.c raster.h
raster_sys.o:		raster_sys.c raster.h
raster_text.o:		raster_text.c raster.h

allsubdirs:
	for i in $(SUBDIRS) ; do \
	    ( cd $$i ; pwd ; $(MAKE) $(MFLAGS) 'CC=$(CC)' 'CFLAGS=$(CFLAGS)' 'LDFLAGS=$(LDFLAGS)' 'SYSDEFS=$(SYSDEFS)' all ) ; \
	done

install:        all installsubdirs
	install -c libraster.a $(LIBINSTDIR)/libraster.a
	install -c raster.3 $(MANINSTDIR)/man3/raster.3
	install -c raster.h $(INCINSTDIR)/raster.h

installsubdirs:
	for i in $(SUBDIRS) ; do \
	    ( cd $$i ; pwd ; $(MAKE) $(MFLAGS) 'CC=$(CC)' 'CFLAGS=$(CFLAGS)' 'LDFLAGS=$(LDFLAGS)' 'SYSDEFS=$(SYSDEFS)' 'LIBINSTDIR=$(LIBINSTDIR)' 'INCINSTDIR=$(INCINSTDIR)' 'BININSTDIR=$(BININSTDIR)' 'MANINSTDIR=$(MANINSTDIR)' install ) ; \
	done

lint:
	for i in $(ALLSRCS) ; do \
	    echo $$i ; \
	    lint $(ALLLINTFLAGS) $$i | sed -e '/but never used or defined/d' -e '/but not defined/d' -e '/but never used/d' ; \
	done

clean:		cleansubdirs
	rm -f $(LIBS) *.o a.out core

cleansubdirs:
	for i in $(SUBDIRS) ; do \
	    ( cd $$i ; pwd ; $(MAKE) $(MFLAGS) clean ) ; \
	done

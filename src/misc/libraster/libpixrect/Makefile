# Makefile for libpixrect
#
# Copyright (C) 1993 by Jef Poskanzer <jef@netcom.com>.
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

# CONFIGURE: location of the raster include files
RASTER_INC_DIR =	..
#RASTER_INC_DIR =	/usr/include
#RASTER_INC_DIR =	/usr/local/include

# CONFIGURE: uncomment one of the following defines according to your
# operating system type.
#
# For SunOS 3.x:
#SYSDEFS =	-DOLD_FBIO_H_LOC -DOLD_MMAP -DNO_POLL -DNO_STDLIB_H -DNO_SYSCONF -DNO_UNISTD_H
# For SunOS 4.x (Solaris 1.x):
#SYSDEFS =	-DOLD_FBIO_H_LOC
# For Solaris 2.x (SunOS 5.x):
SYSDEFS =	

# End of configurable definitions.

MAKE =		make

INCLUDES =	-I. -I$(RASTER_INC_DIR)
DEFINES =	
ALLSRCS =	pixrect_batch.c pixrect_file.c pixrect_font.c pixrect_getput.c \
		pixrect_line.c pixrect_maps.c pixrect_misc.c pixrect_repl.c \
		pixrect_rop.c pixrect_sten.c pixrect_text.c
LIBOBJS =	pixrect_batch.o pixrect_file.o pixrect_font.o pixrect_getput.o \
		pixrect_line.o pixrect_maps.o pixrect_misc.o pixrect_repl.o \
		pixrect_rop.o pixrect_sten.o pixrect_text.o
LIBS =		libpixrect.a

ALLCFLAGS =	$(CFLAGS) $(INCLUDES) $(DEFINES)
ALLLINTFLAGS =	$(LINTFLAGS) $(INCLUDES) $(DEFINES)

SUBDIRS =	test crumble zot

all:		$(LIBS) allsubdirs

libpixrect.a:	$(LIBOBJS)
	rm -f libpixrect.a
	ar rc libpixrect.a $(LIBOBJS)
	-ranlib libpixrect.a

.c.o:
	$(CC) $(ALLCFLAGS) -c $*.c

pixrect_batch.o:	pixrect_batch.c pixrect/pixrect.h $(RASTER_INC_DIR)/raster.h
pixrect_file.o:		pixrect_file.c pixrect/pixrect.h pixrect/pr_io.h $(RASTER_INC_DIR)/raster.h
pixrect_font.o:		pixrect_font.c pixrect/pixrect.h pixrect/pixfont.h $(RASTER_INC_DIR)/raster.h
pixrect_getput.o:	pixrect_getput.c pixrect/pixrect.h $(RASTER_INC_DIR)/raster.h
pixrect_line.o:		pixrect_line.c pixrect/pixrect.h $(RASTER_INC_DIR)/raster.h
pixrect_maps.o:		pixrect_maps.c pixrect/pixrect.h $(RASTER_INC_DIR)/raster.h
pixrect_misc.o:		pixrect_misc.c pixrect/pixrect.h $(RASTER_INC_DIR)/raster.h
pixrect_repl.o:		pixrect_repl.c pixrect/pixrect.h $(RASTER_INC_DIR)/raster.h
pixrect_rop.o:		pixrect_rop.c pixrect/pixrect.h $(RASTER_INC_DIR)/raster.h
pixrect_sten.o:		pixrect_sten.c pixrect/pixrect.h $(RASTER_INC_DIR)/raster.h
pixrect_text.o:		pixrect_text.c pixrect/pixrect.h pixrect/pixfont.h $(RASTER_INC_DIR)/raster.h

allsubdirs:
	for i in $(SUBDIRS) ; do \
	    ( cd $$i ; pwd ; $(MAKE) $(MFLAGS) 'CC=$(CC)' 'CFLAGS=$(CFLAGS)' 'LDFLAGS=$(LDFLAGS)' 'SYSDEFS=$(SYSDEFS)' all ) ; \
	done

install:        all installsubdirs
	install -c libpixrect.a $(LIBINSTDIR)/libpixrect.a
	install -c pixrect.3 $(MANINSTDIR)/man3/pixrect.3
	-mkdir $(INCINSTDIR)/pixrect
	install -c pixrect/pixrect.h $(INCINSTDIR)/pixrect/pixrect.h
	install -c pixrect/pixfont.h $(INCINSTDIR)/pixrect/pixfont.h
	install -c pixrect/pr_io.h $(INCINSTDIR)/pixrect/pr_io.h
	install -c pixrect/memvar.h $(INCINSTDIR)/pixrect/memvar.h
	install -c pixrect/pixrect_hs.h $(INCINSTDIR)/pixrect/pixrect_hs.h

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

# Makefile for rconsole
#
# Copyright (C) 1991, 1993 by Jef Poskanzer <jef@netcom.com>.
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
#CC =		gcc -W -ansi -pedantic -fpcc-struct-return -fstrength-reduce -finline-functions
# (Doesn't work if compiled with gcc, at least on Sun3's.)

# CONFIGURE: your favorite C flags
#CFLAGS =	-g
CFLAGS =	-O
#CFLAGS =	-g -O

# CONFIGURE: your favorite loader flags
LDFLAGS =	-s
#LDFLAGS =	

# CONFIGURE: your favorite lint flags.
LINTFLAGS =	-hxz

# End of configurable definitions.

INCLUDES =	-I$(RASTER_INC_DIR)
DEFINES =
ALLCFLAGS =	$(CFLAGS) $(DEFINES) $(INCLUDES)
ALLLINTFLAGS =	$(LINTFLAGS) $(INCLUDES) $(DEFINES)
LIBRASTER =	-L$(RASTER_LIB_DIR) -lraster
DEP_LIBRASTER =	$(RASTER_LIB_DIR)/libraster.a

SRCS =		main.c options.c setup_tty.c util.c rcons_subr.c rcons_font.c
OBJS =		main.o options.o setup_tty.o util.o rcons_subr.o rcons_font.o

all:		rconsole

# Load with cc to get shared library support under SunOS 4
rconsole:	$(OBJS) $(DEP_LIBRASTER)
	cc $(ALLCFLAGS) $(LDFLAGS) $(OBJS) -lm $(LIBRASTER) -o rconsole

main.o:		main.c global.h ascii.h get_char.h myfbdevice.h
	$(CC) $(ALLCFLAGS) -c main.c

options.o:	options.c global.h
	$(CC) $(ALLCFLAGS) -c options.c

setup_tty.o:	setup_tty.c global.h
	$(CC) $(ALLCFLAGS) -c setup_tty.c

util.o:		util.c global.h ascii.h get_char.h
	$(CC) $(ALLCFLAGS) -c util.c

rcons_subr.o:	rcons_subr.c myfbdevice.h $(RASTER_INC_DIR)/raster.h
	$(CC) $(ALLCFLAGS) -c rcons_subr.c

rcons_font.o:	rcons_font.c myfbdevice.h $(RASTER_INC_DIR)/raster.h
	$(CC) $(ALLCFLAGS) -c rcons_font.c

install:	all
	install -c rconsole $(BININSTDIR)/rconsole
	install -c rconsole.1 $(MANINSTDIR)/man1/rconsole.1

lint:
	for i in $(SRCS) ; do \
	    echo $$i ; \
	    lint $(ALLLINTFLAGS) $$i | sed -e '/but never used or defined/d' -e '/but not defined/d' -e '/but never used/d' -e '/constant in conditional context/d' ; \
	done

clean:
	rm -f rconsole *.o a.out core

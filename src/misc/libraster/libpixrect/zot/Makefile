# Makefile for zot

# CONFIGURE: Set this to the directory where you want executables to go.
BININSTDIR =		/usr/local/bin

# CONFIGURE: Set this to the manual area you want.  Standard subdirectory
# organization is assumed - section 1 for executables, section 3 for libraries.
#MANINSTDIR =		/usr/man
MANINSTDIR =		/usr/local/man

# CONFIGURE: location of the dir above the pixrect include files
PIXRECT_INC_DIR =	..
#PIXRECT_INC_DIR =	/usr/include
#PIXRECT_INC_DIR =	/usr/local/include

# CONFIGURE: location of the pixrect library
PIXRECT_LIB_DIR =	..
#PIXRECT_LIB_DIR =	/usr/lib
#PIXRECT_LIB_DIR =	/usr/local/lib

# CONFIGURE: location of the raster library
RASTER_LIB_DIR =	../..
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

# End of configurable definitions.

INCLUDES =	-I$(PIXRECT_INC_DIR)
DEFINES =
ALLCFLAGS =	$(CFLAGS) $(INCLUDES) $(DEFINES)
ALLLINTFLAGS =	$(LINTFLAGS) $(INCLUDES) $(DEFINES)
LIBPIXRECT =	-L$(PIXRECT_LIB_DIR) -lpixrect
DEP_LIBPIXRECT = $(PIXRECT_LIB_DIR)/libpixrect.a
LIBRASTER =	-L$(RASTER_LIB_DIR) -lraster
DEP_LIBRASTER =	$(RASTER_LIB_DIR)/libraster.a

all:		zot

zot:		zot.o $(DEP_LIBPIXRECT) $(DEP_LIBRASTER)
	$(CC) $(ALLCFLAGS) $(LDFLAGS) zot.o $(LIBPIXRECT) $(LIBRASTER) -o zot
zot.o:		zot.c $(PIXRECT_INC_DIR)/pixrect/pixrect.h $(PIXRECT_INC_DIR)/pixrect/memvar.h
	$(CC) -c $(ALLCFLAGS) zot.c

install:	all
	install -c zot $(BININSTDIR)/zot
	install -c zot.1 $(MANINSTDIR)/man1/zot.1

lint:
	lint $(ALLLINTFLAGS) zot.c | sed -e '/but never used or defined/d' -e '/but not defined/d' -e '/defined.* but never used/d'

clean:
	rm -f zot *.o a.out core

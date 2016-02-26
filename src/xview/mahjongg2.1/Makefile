#
# Modified for XView version, 1991, Stan Tazuma
#
# $Header: /home/sirius/skt/cmds/src/sun/mj/RCS/Makefile,v 2.3 91/12/20 14:32:54 skt Exp $
#
#/*
# *	Copyright 1988, Mark Holm
# *			Exceptions
# *
# *	Acknowledgments to Dorothy Robinson for her artistic
# *	 abilities in drawing the icons and to Jim Batch for
# *	 technical support and graphical concepts (which I abandoned in favor
# *       of the easy way out).
# *
# *	Permission is given to copy and distribute for non-profit purposes.
# *
# */
#	Removed the DISTFILES? entries as they didn't match the
#	net distributed version and I didn't want to put in 37 entries.
#		Bill Randle, 6/17/88
#

#ifndef lint
# place to put the manual page
MANLOC = /usr/public/man/manp/mahjongg.p

# Place to put the executable
DEST = /usr/public/bin/mahjongg

# Root directory of XView developers package
# For OpenWindows users, this is the same as $OPENWINHOME.
XVIEWROOT = ${OPENWINHOME}

# If not in OpenWindows, need to specify directory where libxview
# and libolgx are.
# For OpenWindows users, this is taken care of via $LD_LIBRARY_PATH.
XVIEWLIBDIR =
# e.g., XVIEWLIBDIR = -L/usr/openwin/lib
# This -L... stuff may only be appropriate in a SunOS environment,
# so do what's appropriate in your environment.  The -L<pathname>
# adds <pathname> to the list of directories which will be searched
# when a -l<libname> command line option is seen.

#Target machine - -DSWAP only valid define
# currently only necessary when compiling icons.o
TARGET=

CFLAGS = -O -I${XVIEWROOT}/include

SWOBJS = icons.o mahjongg.o event.o
#SWLIBS= ${XVIEWLIBDIR} -lxview -lolgx -lX11 /stage/lib.linux/uflow.o
SWLIBS= ${XVIEWLIBDIR} -lxview -lolgx -lX11 

mahjongg: $(SWOBJS)
	cc $(CFLAGS) -o mahjongg $(SWOBJS) $(SWLIBS)

mahjongg.swap:
	-mv icons.o icons.o.prev
	make TARGET=-DSWAP images/swap/s_winter mahjongg

install: mahjongg
	cp mahjongg $(DEST)
	cp mahjongg.6 $(MANLOC)
	@echo "Don't forget to install the on-line help file mahjongg.info"
	@echo "Installation of this will depend on your environment."

mahjongg.o: mahjongg.h

event.o: mahjongg.h

icons.o: icons.c images/color/s_winter
	cc -c $(CFLAGS) $(TARGET) icons.c

images/color/s_winter:
	cd image-stuff; make images

images/swap/s_winter: images/color/s_winter
	cd image-stuff; make images.swap

clean:
	/bin/rm -f core mahjongg *.o swshar*

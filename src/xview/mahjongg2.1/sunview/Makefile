# 12/19/91 - skt
# modifed for SunView/Xview combined release

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
MANLOC = /usr/public/man/manp/mahjongg_sv.p

# Place to put the executable
DEST = /usr/public/bin/mahjongg_sv

#Target machine - -DSWAP only valid define
# currently only necessary when compiling icons.o
TARGET=

CFLAGS = -O -DSUNVIEW

SWOBJS = icons.o mahjongg.o event.o
SWLIBS= -lsuntool -lsunwindow -lpixrect

mahjongg: $(SWOBJS)
	cc $(CFLAGS) -o mahjongg $(SWOBJS) $(SWLIBS)

mahjongg.swap:
	-mv icons.o icons.o.prev
	make TARGET=-DSWAP images/swap/s_winter mahjongg

install: mahjongg
	cp mahjongg $(DEST)
	cp mahjongg.6 $(MANLOC)

mahjongg.o: mahjongg.h color.h

event.o: mahjongg.h

icons.o: icons.c images/color/s_winter
	cc -c $(CFLAGS) $(TARGET) icons.c

icons.c: ../icons.c
	ln -s ../$@ .

images/color/s_winter: images
	cd ../image-stuff; make images

images/swap/s_winter: images/color/s_winter
	cd ../image-stuff; make images.swap

images:
	ln -s ../images .

clean:
	/bin/rm -f core mahjongg *.o swshar*

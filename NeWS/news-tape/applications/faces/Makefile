#
#  Makefile for faces, an icon face server, plus associated software.
#
#  @(#)Makefile 1.1 88/12/04
#
#  Copyright (c) Rich Burridge.
#                Sun Microsystems, Australia - All rights reserved.
#
#  Permission is given to distribute these sources, as long as the
#  copyright messages are not removed, and no monies are exchanged. 
#
#  Version 1.3 - December 1988.
#
#  No responsibility is taken for any errors inherent either
#  to the comments or the code of this program, but if reported
#  to me then an attempt will be made to fix them.
#
#------------------------------------------------------------------
#  There are various command line options with the faces program.
#  They can also be defined at compile time. If they are not present
#  here, then sensible default values are used. The values given
#  below are the defaults, except for the spoolfile name where the
#  default is obtained from the users' passwd entry.
#
#
#  Monitor type (0 = monitor all, 1 = monitor new, 2 = monitor printer).
#
FMONTYPE         = -DFMONTYPE=1
#
#  Alternate background pattern instead of root gray.
#
BACKGROUND       = -DBACKGROUND=\"\"
#
#  Different directory for face icons,
#
FACEDIR          = /usr/NeWS/faces
FACEPARAM        = -DFACEDIR=\"$(FACEDIR)\"
#
#  If nonzero, then display face images in reverse video.
#
INVERT           = -DINVERT=0
#
#  Period in seconds for checking for new mail or print jobs,
#
PERIOD           = -DPERIOD=60
#
#  If nonzero, do not show number of messages on the face icon,
#
DONTSHOWNO       = -DDONTSHOWNO=0
#
#  Alternative spoolfile to monitor,
#  eg:     SPOOLFILE  = -DSPOOLFILE=\"/usr/richb/Mail/pending\"
#
#SPOOLFILE       =
#
#  If nonzero, do not show timestamp on the face icon.
#
DONTSHOWTIME     = -DDONTSHOWTIME=0
#
#  If nonzero, do not show username on the face icon.
#
DONTSHOWUSER     = -DDONTSHOWUSER=0
#-------------------------------------------------------------------

VARIABLES       = $(FMONTYPE) $(BACKGROUND) $(FACEPARAM) \
                  $(INVERT) $(PERIOD) $(DONTSHOWNO) $(SPOOLFILE) \
		  $(DONTSHOWTIME) $(DONTSHOWUSER)
BINARIES        = ps_faces sv_faces
BINDIR          = /usr/NeWS/bin
FACES           = facedir
LIBDIR          = /usr/NeWS/lib
NEWSFILE        = -DNEWSGIVEN -DNEWSFILE=\"$(LIBDIR)/faces.ps\"
MANDIR          = /usr/man/man$(MANSECT)
MANSECT         = l

#
#  If you are compiling faces under v3.x of SunOS, then uncomment this line.
#OSTYPE          = -DSUNOS3.x

CFLAGS          = -g $(NEWSFILE) $(OSTYPE) $(VARIABLES)
HDRS            = extern.h faces.h patchlevel.h
IMAGES          = noface.icon nomail.icon noprint.icon nopaper.icon

PSLIBS          = $$NEWSHOME/lib/libcps.a
PSOBJS          = get.o main.o mon.o news.o rec.o
PSSRCS          = get.c main.c mon.c news.c rec.c

SVLIBS          = -lsuntool -lsunwindow -lpixrect
SVOBJS          = get.o main.o mon.o rec.o sunview.o
SVSRCS          = get.c main.c mon.c rec.c sunview.c

OTHERS          = README faces.1 faces.ps machine.tab people.tab Makefile
SRCS            = get.c main.c mon.c news.c rec.c sunview.c

all:            sunview news

news:           ps_faces
		cp ps_faces faces

sunview:        sv_faces
		cp sv_faces faces

ps_faces:       $(PSOBJS)
		cc -o ps_faces $(CFLAGS) $(PSOBJS) $(PSLIBS)

sv_faces:       $(SVOBJS)
		cc -o sv_faces $(CFLAGS) $(SVOBJS) $(SVLIBS)

install:        $(BINARIES)
		install -s -m 751 faces $(BINDIR)
		install -c -m 644 faces.ps $(LIBDIR)
		-mkdir $(FACEDIR)
		install -c -m 644 machine.tab $(FACEDIR)
		install -c -m 644 people.tab $(FACEDIR)
		install -c -m 644 faces.1 $(MANDIR)/faces.$(MANSECT)

backup:;	cp -r $(SRCS) $(HDRS) $(IMAGES) $(OTHERS) $(FACES) backdir

clean:;         rm -f *.o $(BINARIES) faces core *.BAK *~

lint:;          lint -DNEWSGIVEN -DNEWSFILE=\"$(LIBDIR)/faces.ps\" $(PSSRCS)
		lint $(SVSRCS) $(SVLIBS)

shar:;          shar.script $(OTHERS) $(HDRS) $(IMAGES) > archive.1
		shar.script $(SRCS) > archive.2
		shar.script $(FACES) > archive.3

create:         SCCS
		-sccs create $(SRCS) $(HDRS) $(IMAGES) $(OTHERS)

SCCS:
		mkdir SCCS
		chmod 755 SCCS

get.o:          get.c faces.h extern.h patchlevel.h
main.o:         main.c faces.h Makefile
mon.o:          mon.c faces.h extern.h
news.o:         news.c faces.h extern.h
rec.o:          rec.c faces.h extern.h
sunview.o:      sunview.c extern.h $(IMAGES)

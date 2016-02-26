#
#  Makefile for calctool, a calculator program.
#
#  Copyright (c) Rich Burridge - May 1988.
#                Sun Microsystems, Australia - All rights reserved.
#
#  Version 2.2.
#
#  No responsibility is taken for any errors inherent either in the comments
#  or the code of this program, but if reported to me then an attempt will
#  be made to fix them.
#

BINARIES        = ps_calctool
BINDIR          = /usr/local/bin
LIBDIR          = /usr/local/lib
NEWSLIBDIR	= /usr/NeWS/lib
#HELPNAME        = -DHELPGIVEN -DHELPNAME=\"$(LIBDIR)/calctool.help\"
HELPNAME        = -DHELPGIVEN -DHELPNAME=\"$(NEWSLIBDIR)/calctool.help\"
#NEWSFILE        = -DNEWSGIVEN -DNEWSFILE=\"$(LIBDIR)/news.ps\"
NEWSFILE        = -DNEWSGIVEN -DNEWSFILE=\"$(NEWSLIBDIR)/news.ps\"
MANDIR          = /usr/man/man$(MANSECT)
MANSECT         = l
CFLAGS          = -g $(HELPNAME) $(NEWSFILE)
HDRS            = calctool.h color.h extern.h
IMAGES          = calctool.icon help.cursor

PSLIBS          = -lm $$NEWSHOME/lib/libcps.a
PSOBJS          = calctool.o functions.o graphics.o news.o
PSSRCS          = calctool.c functions.c graphics.c news.c

SVLIBS          = -lm -lsuntool -lsunwindow -lpixrect
SVOBJS          = calctool.o functions.o graphics.o sunview.o
SVSRCS          = calctool.c functions.c graphics.c sunview.c

OTHERS          = Makefile README calctool.help calctool.1 news.ps patchlevel.h
SRCS            = calctool.c functions.c graphics.c news.c sunview.c

all:            $(BINARIES)

sunview:        sv_calctool
		cp sv_calctool calctool

news:           ps_calctool
		cp ps_calctool calctool

sv_calctool:    $(SVOBJS)
		cc -o sv_calctool $(CFLAGS) $(SVOBJS) $(SVLIBS)

ps_calctool:    $(PSOBJS)
		cc -o ps_calctool $(CFLAGS) $(PSOBJS) $(PSLIBS)

install:        $(BINARIES)
		install -s -m 751 calctool $(BINDIR)
		install -c -m 644 calctool.help $(LIBDIR)
		install -c -m 644 calctool.1 $(MANDIR)/calctool.$(MANSECT)

clean:;         rm -f *.o calctool core *~ *.BAK ps_calctool sv_calctool

lint:;          lint $(SVSRCS) $(SVLIBS)
		lint $(PSSRCS) $(PSLIBS)

shar:;          shar.script $(SRCS)  > archive.1
		shar.script $(HDRS) $(IMAGES) $(OTHERS) > archive.2

calctool.o:     calctool.c $(HDRS) patchlevel.h
functions.o:    functions.c $(HDRS)
graphics.o:     graphics.c $(HDRS)
news.o:         news.c $(HDRS)
sunview.o:      sunview.c $(HDRS)

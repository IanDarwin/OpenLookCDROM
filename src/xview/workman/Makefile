# @(#)Makefile	1.63 04 Jun 1995

#
# If you want to use gcc, tell make about it here.
#

#CC = gcc
CC = cc

#
# Uncomment the following if you're compiling on Solaris 2.x.
#
# If your system needs additional libraries, specify them on the "EXTRA_LIBS"
# line.
#
# If you don't want WorkMan to try to activate the SPARCstation 5 internal
# audio input so you get sound from the workstation, take -DCODEC off the
# SOL2 line.
#

#SOL2 = -DSYSV # -DCODEC
#LIBLOC = -R$(LIBDIR)
#EXTRA_LIBS = -lsocket

#
# Uncomment the following if you want fast database lookups using the
# Berkeley libdb code.  You can find libdb on ftp.cs.berkeley.edu in the
# /ucb/4bsd directory.  Make sure you adjust the last part of DBCFLAGS
# to match your system type as listed in libdb's PORT directory.
#

# DBCFLAGS = -DLIBDB -Idb.1.85/PORT/include -Idb.1.85/PORT/sunos.5.2
# DBLIB = db.1.85/PORT/sunos.5.2/libdb.a

#
# Set these to the locations of your XView include files and libraries.
#

INCDIR = $(OPENWINHOME)/include
LIBDIR = $(OPENWINHOME)/lib

#
# Set these to the directory names in which to install the software and
# help files.
#

BINDIR = $(OPENWINHOME)/bin
HLPDIR = $(OPENWINHOME)/lib/help

#
# Set this to the directory containing manual pages.  Manual pages will only
# be installed if you "make install.man".
#

MANDIR = $(OPENWINHOME)/man

#
# Set these to the manual sections (e.g. 1, 4, 5, l) for the program and
# database file manual pages, respectively.
#

PEXT = 1
DEXT = 5

# You shouldn't have to worry about anything from this line on.

############################################################################
# Parameters.

PROGRAM = workman
SOURCES.c = workman_stubs.c database.c cdinfo.c display.c setup.c scsi.c \
	ui_db_prefs.c index.c \
	ui_cdinfo.c plat_sun.c plat_hpux.c plat_linux.c plat_news.c \
	plat_bsd386.c plat_ultrix.c cdrom.c drv_toshiba.c drv_sony.c \
	plat_svr4.c plat_freebsd.c plat_osf1.c
SOURCES.h = struct.h workman_ui.h

OBJECTS = \
	$(SOURCES.c:%.c=%.o)

# Compiler flags.

CFLAGS = -g
CPPFLAGS = -I$(INCDIR) $(SOL2)
LDFLAGS = -L$(LIBDIR) $(LIBLOC)
LDLIBS = -lxview -lolgx -lX11 $(EXTRA_LIBS) $(DBLIB) -ldl

# Standard targets.

all:	 $(PROGRAM) $(PROGRAM).doc workmandb.doc

$(PROGRAM): $(SOURCES.c) $(OBJECTS)
	$(CC) $(LDFLAGS) -o $@ $(OBJECTS) $(LDLIBS)

buildindex: buildindex.c
	$(CC) $(LDFLAGS) $(DBCFLAGS) buildindex.c $(DBLIB) -o $@

clean:
	$(RM) $(OBJECTS) *.BAK *.delta core

install: $(PROGRAM)
	cp $(PROGRAM) $(BINDIR)
	chmod 755 $(BINDIR)/$(PROGRAM)
	cp $(PROGRAM).info $(HLPDIR)
	chmod 644 $(HLPDIR)/$(PROGRAM).info

install.man: workman.man workmandb.man $(MANDIR)/man$(PEXT) $(MANDIR)/man$(DEXT)
	cp workman.man $(MANDIR)/man$(PEXT)/workman.$(PEXT)
	chmod 644 $(MANDIR)/man$(PEXT)/workman.$(PEXT)
	cp workmandb.man $(MANDIR)/man$(DEXT)/workmandb.$(DEXT)
	chmod 644 $(MANDIR)/man$(DEXT)/workmandb.$(DEXT)

display.o: display.c workman_ui.h
setup.o: setup.c workman_ui.h
workman_stubs.o: workman_stubs.c workman_ui.h
ui_cdinfo.o: ui_cdinfo.c workman_ui.h
# This next one isn't completely true, but close enough
$(OBJECTS): struct.h

index.o: index.c
	$(CC) $(CFLAGS) $(DBCFLAGS) -c $<

$(PROGRAM).doc: $(PROGRAM).man
	nroff -man $(PROGRAM).man > $(PROGRAM).doc
workmandb.doc: workmandb.man
	nroff -man workmandb.man > workmandb.doc

#
# Build the DOCS subdirectory from the HTML sources.
#
docs:
	@cd HTML; for i in *.html; do \
		lynx -dump file://localhost`pwd`/$$i > ../DOCS/`basename $$i .html`; \
		echo Converted $$i; \
	done

#
# These targets are for building the shar distribution.
#
shar: part01
tar: WorkMan.tar

ALLSOURCES= README $(SOURCES.h) \
	$(PROGRAM).man $(PROGRAM).doc $(SOURCES.c) workmandb.man workmandb.doc \
	bitmaps Makefile.svr4 HTML DOCS \
	$(PROGRAM).info Makefile Makefile.hpux Makefile.linux Imakefile \
	WorkMan.tmpl Makefile.osf1 buildindex.c
ALLFILES= $(ALLSOURCES) \
	bitmaps/loud.icon bitmaps/loud0.icon \
	bitmaps/loud1.icon bitmaps/loud2.icon bitmaps/loud3.icon \
	bitmaps/loud4.icon bitmaps/loud5.icon bitmaps/loud6.icon \
	bitmaps/phones bitmaps/phones0 bitmaps/phonesl1 bitmaps/phonesl2 \
	bitmaps/phonesl3 bitmaps/phonesr1 bitmaps/phonesr2 bitmaps/phonesr3 \
	bitmaps/sink0 bitmaps/sink1 bitmaps/sink2 bitmaps/sink3 \
	bitmaps/sink4 bitmaps/sink5 bitmaps/sink6 bitmaps/sink7 \
	bitmaps/eject.button bitmaps/ff.button \
	bitmaps/eject.button.small bitmaps/ff.button.small \
	bitmaps/icon bitmaps/iconmask bitmaps/pause.button \
	bitmaps/pause.button.small bitmaps/play.button.small \
	bitmaps/rew.button.small bitmaps/stop.button.small \
	bitmaps/play.button bitmaps/rew.button bitmaps/stop.button

part01: $(ALLFILES)
	makekit -s 50k -n part $(ALLFILES)
WorkMan.tar: $(ALLSOURCES)
	make docs
	(cd ..; tar cvFf workman/WorkMan.tar `(for i in $(ALLSOURCES); do \
		echo $$i; done) | sed 's@^@workman/@' | sort`)

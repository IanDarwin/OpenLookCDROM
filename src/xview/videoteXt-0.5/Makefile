# GNUmakefile for VideoteXt
#
# Copyright (c) 1994-95 by Martin Buck

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.


# Set the installation-directories & permissions here (these directories must
# exist before you install VideoteXt!):

BINDIR = /usr/local/bin
XBINDIR = /usr/local/bin/X11
BINPERM = 755
BINGRP =
FONTDIR = /usr/lib/X11/fonts/local
FONTPERM = 644
FONTGRP =
CONFDIR = /usr/local/etc
CONFPERM = 644
CONFGRP =

STRIP = -s


# Compiler options

INCLUDEDIRS = -I/usr/openwin/include

# If you use XFree 3.x you probably have to use the second
# LIBDIRS-line

LIBDIRS = -L/usr/openwin/lib
#LIBDIRS = -L/usr/openwin/lib -L/usr/X11R6/lib

CC = gcc
CCFLAGS = -O2 -fomit-frame-pointer -Wall #-Wstrict-prototypes

LDFLAGS =
CLIBS =
XLIBS = -lxview -lolgx -lXext -lX11 -lm


# Uncomment the following line if you don't have the videotext-driver
# installed (e.g. if you want to use VideoteXt just for displaying
# INtv-pages or for testing purposes)

# DRIVER_KLUGE = -Idriver


# Uncomment the following line if you want to get coredumps when an
# assertion fails. You should only do this if you want to debug VideoteXt
# (see the comment in vtx_assert.h)

# ASSERT = -DASSERT_DUMP_CORE


#####################################################################
# No user-servicable parts beyond this line!
#####################################################################

DEFINES = -DVTX_CONFDIR=\"$(CONFDIR)\" $(ASSERT)
CFLAGS = $(DEFINES) $(CCFLAGS) $(INCLUDEDIRS) $(DRIVER_KLUGE)

OBJS = cct.o vtxdecode.o vtxtools.o toptext.o safe_malloc.o postscript.o
XOBJS = hotlist.o vtxqueue.o fileio.o config.o dialog.o xevents.o xinit.o
COBJS = vtxget.o batch.o
SRC = vtxget.c cct.c vtxdecode.c batch.c vtxtools.c safe_malloc.c toptext.c hotlist.c \
      vtxqueue.c fileio.c config.c dialog.c xevents.c xinit.c postscript.c
HEADERS = vtxget.h cct.h vtxdecode.h batch.h vtxtools.h safe_malloc.h toptext.h hotlist.h \
          vtxqueue.h fileio.h config.h dialog.h xevents.h xinit.h postscript.h misc.h vtx_assert.h
FONTS = fonts/vtx10x12.pcf fonts/vtx15x18.pcf fonts/vtx20x24.pcf
MISC = empty.xbm checkmark_sheet.xbm checkmark_sheets.xbm clock.xbm eyes.xbm sheet.xbm vtx.xbm \
       sheets.xbm eyes_sheet.xbm \
       titlepage.raw psheader.ps .depend mkvtxfont mktitlepage mkpsheader Makefile
DOCS = README LIESMICH HOWTOGET ChangeLog VideoteXt.lsm VTXCARD-VENDORS COPYING
DISTDIR = VideoteXt-0.5

ifneq ($(BINGRP),)
_BINGRP = -g $(BINGRP)
endif
ifneq ($(FONTGRP),)
_FONTGRP = -g $(FONTGRP)
endif
ifneq ($(CONFGRP),)
_CONFGRP = -g $(CONFGRP)
endif


all: videotext vtxget fonts

videotext: $(OBJS) $(XOBJS) titlepage.h psheader.h
	$(CC) $(LDFLAGS) -o videotext $(OBJS) $(XOBJS) $(LIBDIRS) $(XLIBS)

vtxget: $(OBJS) $(COBJS) psheader.h
	$(CC) $(LDFLAGS) -o vtxget $(OBJS) $(COBJS) $(LIBDIRS) $(CLIBS)

titlepage.h: titlepage.raw
	./mktitlepage < $< > $@

psheader.h: psheader.ps
	./mkpsheader < $< > $@

fonts: $(FONTS)

install: install.videotext install.vtxget install.fonts install.config

install.videotext: videotext
	install $(STRIP) $(_BINGRP) -m $(BINPERM) videotext $(XBINDIR)

install.vtxget: vtxget
	-rm -f $(XBINDIR)/vtxget		# Remove symlink of old version
	install $(STRIP) $(_BINGRP) -m $(BINPERM) vtxget $(BINDIR)

install.fonts: fonts
	install $(_FONTGRP) -m $(FONTPERM) fonts/*.pcf $(FONTDIR)
	mkfontdir $(FONTDIR)
	chmod $(FONTPERM) $(FONTDIR)/fonts.dir
ifneq ($(FONTGRP),)
	chgrp $(FONTGRP) $(FONTDIR)/fonts.dir
endif

install.config:
	install $(_CONFGRP) -m $(CONFPERM) examples/vtxrc examples/vtx-hotlist $(CONFDIR)

clean:
	rm -f $(OBJS) $(COBJS) $(XOBJS) titlepage.h psheader.h core gmon.out *~

distclean: clean
	rm -f videotext vtxget fonts/*.pcf

dist: depend
	-rm -fr $(DISTDIR)
	mkdir $(DISTDIR) $(DISTDIR)/fonts $(DISTDIR)/examples $(DISTDIR)/driver
	cp -a $(SRC) $(HEADERS)  $(MISC) $(DOCS) examples $(DISTDIR)
	cp -a fonts/*.pbm $(DISTDIR)/fonts
	make -C driver DISTDIR=$(DISTDIR) sub-dist
	chmod -R u=rwX,go=rX $(DISTDIR)/
	tar cf - $(DISTDIR) | gzip -9 > $(DISTDIR).tar.gz
	rm -rf $(DISTDIR)

rotw:
	make DISTDIR=VideoteXt-$(shell date '+%d%m%y') dist

hotlist: $(HOME)/.vtx-hotlist
	grep '^[0-9]\+$$' $(HOME)/.vtx-hotlist -v > examples/vtx-hotlist

depend: titlepage.h psheader.h
	$(CC) -c -MM $(CFLAGS) $(SRC) > .depend


%.pcf: %.pbm
	./mkvtxfont $*.pbm $*.bdf $*d.bdf
	bdftopcf $*.bdf > $*.pcf
	bdftopcf $*d.bdf > $*d.pcf
	rm $*.bdf $*d.bdf


ifeq (.depend,$(wildcard .depend))
include .depend
endif

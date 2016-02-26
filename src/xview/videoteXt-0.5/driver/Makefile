# GNUmakefile for videotext-driver
#
# Copyright (c) 1994-95 Martin Buck  <martin.buck@student.uni-ulm.de>

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


# Uncomment the suitable line for your VTX-interface
# IF_CT       is an internal VTX-decoder-card equipped with a SAA5246 as
#             published in the german computer-magazine c't 11/91, p. 228
#             and c't 7/92, p. 176 (only the internal version with a 5246
#             is supported at the moment; the 5244 will _never_ be supported)
# IF_CTSERIAL is the same c't-interface connected to a serial port (thanks to
#             Carsten Bobek for patches)
# IF_VTX2000  is an internal VTX-decoder-card with a SAA5246. It should
#             be compatible with the internal videodat-cards (VD3000)
#             made by Wiegand Datentechnik/Videodat Medien GmbH (thanks to
#             Volker Schuermann for info about these cards)
# IF_PCVT7000 is an external decoder with an internal interface-card which
#             was published in ELV journal 2/90, p. 54, 3/90, p. 54
#             and 4/90, p. 16. This is untested, please tell me if it
#             works for you
# IF_SATCOM   is an external VTX-decoder connected to a serial port made
#             by KDK Satcom (thanks to Martin Höppner for info and
#             patches)
# IF_HOMEBREW is my own interface attached to a parallel-port (maybe not
#             useful for anybody but me)

VTXIF = IF_CT
#VTXIF = IF_CTSERIAL
#VTXIF = IF_VTX2000
#VTXIF = IF_PCVT7000
#VTXIF = IF_SATCOM
#VTXIF = IF_HOMEBREW


# Define the base-IO-port of your I²C-/VTX-interface here (can be overridden
# on the insmod-commandline with base_io=0x???):

VTX_DEFAULT_IO_BASE = 0x100	# IF_CT with S3/S2/S1 set to 000
#VTX_DEFAULT_IO_BASE = 0x1f0	# IF_CT with S3/S2/S1 set to 001
#VTX_DEFAULT_IO_BASE = 0x200	# IF_CT with S3/S2/S1 set to 010
#VTX_DEFAULT_IO_BASE = 0x220	# IF_CT with S3/S2/S1 set to 011
#VTX_DEFAULT_IO_BASE = 0x300	# IF_CT with S3/S2/S1 set to 100
#VTX_DEFAULT_IO_BASE = 0x3e0	# IF_CT with S3/S2/S1 set to 101

#VTX_DEFAULT_IO_BASE = 0x3f8	# IF_CTSERIAL/IF_SATCOM attached to ttyS0
#VTX_DEFAULT_IO_BASE = 0x2f8	# IF_CTSERIAL/IF_SATCOM attached to ttyS1
#VTX_DEFAULT_IO_BASE = 0x3e8	# IF_CTSERIAL/IF_SATCOM attached to ttyS2
#VTX_DEFAULT_IO_BASE = 0x2e8	# IF_CTSERIAL/IF_SATCOM attached to ttyS3

# The next four values can also be used for a VD3000; 0 means jumper open,
# 1 means jumper closed.
#VTX_DEFAULT_IO_BASE = 0x3e0	# IF_VTX2000 with JP1/JP2 set to 00
#VTX_DEFAULT_IO_BASE = 0x300	# IF_VTX2000 with JP1/JP2 set to 01
#VTX_DEFAULT_IO_BASE = 0x330	# IF_VTX2000 with JP1/JP2 set to 10
#VTX_DEFAULT_IO_BASE = 0x250	# IF_VTX2000 with JP1/JP2 set to 11
#VTX_DEFAULT_IO_BASE = 0x238	# IF_VTX2000 (SL) with JP4/JP3/JP2 set to 000
#VTX_DEFAULT_IO_BASE = 0x278    # IF_VTX2000 (SL) with JP4/JP3/JP2 set to 001
#VTX_DEFAULT_IO_BASE = 0x2b8    # IF_VTX2000 (SL) with JP4/JP3/JP2 set to 010
#VTX_DEFAULT_IO_BASE = 0x2f8    # IF_VTX2000 (SL) with JP4/JP3/JP2 set to 011
#VTX_DEFAULT_IO_BASE = 0x338    # IF_VTX2000 (SL) with JP4/JP3/JP2 set to 100
#VTX_DEFAULT_IO_BASE = 0x378    # IF_VTX2000 (SL) with JP4/JP3/JP2 set to 101
#VTX_DEFAULT_IO_BASE = 0x3b8    # IF_VTX2000 (SL) with JP4/JP3/JP2 set to 110
#VTX_DEFAULT_IO_BASE = 0x3f8    # IF_VTX2000 (SL) with JP4/JP3/JP2 set to 111

# The PC-VT 7000 card be be set to any port you like. BR0-BR9 correspond to
# A0-A9. A closed jumper sets the corresponding bit to 0.
#VTX_DEFAULT_IO_BASE = 0x300    # IF_PCVT7000 with default-settings


# Uncomment the following line if you get I/O-Errors when using the driver
# (can be overridden on the insmod-commandline with slow_if=?; possible
# values are 0 (fast) or 1 (slow)):

#I2C_SPEED = -DVTX_CHIPSET_IS_DAMN_SLOW


# Choose your favourite major-number here (can be overridden on the insmod-
# commandline with major=??):

VTX_DEFAULT_MAJOR = 28


# To get debugging messages, uncomment the following line and use debug=x
# (with x > 0) when loading the driver (WARNING: x >= 2 is *very* verbose, you
# definitely want to kill your klogd before you do this)

#DEBUG = -DDEBUG_VTX


# Set the installation-directories & permissions here:
# $(KERNEL_VERSION) will be automagically set to you currently installed (not
# necessarily running!) kernel-version

DRVDIR = /lib/modules/$(KERNEL_VERSION)
DRVSUFFIX = .drv
DRVPERM = 644
DRVGRP =
DEVPERM = 666
DEVGRP =
HDRDIR = /usr/include/linux
HDRPERM = 644
HDRGRP =


# Compiler options

INCLUDEDIRS =

CC = gcc
CCFLAGS = -O2 -fomit-frame-pointer -Wall -Wstrict-prototypes


#####################################################################
# No user-serviceable parts beyond this line!
#####################################################################

DEFINES = -D$(VTXIF) $(DEBUG) -DVTX_DEFAULT_IO_BASE=$(VTX_DEFAULT_IO_BASE) $(I2C_SPEED) \
          -DVTX_DEFAULT_MAJOR=$(VTX_DEFAULT_MAJOR) -DKERNEL_VERSION=\"$(KERNEL_VERSION)\"
CFLAGS = $(DEFINES) $(CCFLAGS) $(INCLUDEDIRS)

DEBUGLEVEL = 1
KERNEL_VERSION = \
  $(shell { grep "define.*UTS_RELEASE" $(HDRDIR)/version.h 2>/dev/null | cut -d\" -f2; uname -r } \
    | head -n 1)

SRC = vtx.c
OBJS = vtx.o
HEADERS = vtx.h
DOCS = README LIESMICH programming.txt ChangeLog

ifneq ($(DRVGRP),)
_DRVGRP = -g $(DRVGRP)
endif
ifneq ($(HDRGRP),)
_HDRGRP = -g $(HDRGRP)
endif


all: vtx.o

test: rmmod all insmod

rmmod:
	-rmmod vtx
	-rmmod vtx.drv

insmod:
	insmod vtx.o debug=$(DEBUGLEVEL)
	lsmod

install: all
	-rm -f /dev/vtx
	mknod -m $(DEVPERM) /dev/vtx c $(VTX_DEFAULT_MAJOR) 0
ifneq ($(DEVGRP),)
	chgrp $(DEVGRP) /dev/vtx
endif
	install $(_DRVGRP) -m $(DRVPERM) vtx.o $(DRVDIR)/vtx$(DRVSUFFIX)
	install $(_HDRGRP) -m $(HDRPERM) vtx.h $(HDRDIR)

clean distclean:
	rm -f $(OBJS) *~

sub-dist: depend
	cp -a $(SRC) $(HEADERS) .depend Makefile $(DOCS) linux ../$(DISTDIR)/driver

depend:
	$(CC) -c -MM $(CFLAGS) $(SRC) > .depend


ifeq (.depend,$(wildcard .depend))
include .depend
endif

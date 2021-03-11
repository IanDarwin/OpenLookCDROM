 /* Copyright IBM Corporation 1988,1991 - All Rights Reserved */
/* For full copyright information see:'andrew/config/COPYRITE' */
/* $Disclaimer: 
# Permission to use, copy, modify, and distribute this software and its 
# documentation for any purpose is hereby granted without fee, 
# provided that the above copyright notice appear in all copies and that 
# both that copyright notice, this permission notice, and the following 
# disclaimer appear in supporting documentation, and that the names of 
# IBM, Carnegie Mellon University, and other copyright holders, not be 
# used in advertising or publicity pertaining to distribution of the software 
# without specific, written prior permission.
# 
# IBM, CARNEGIE MELLON UNIVERSITY, AND THE OTHER COPYRIGHT HOLDERS 
# DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
# ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.  IN NO EVENT 
# SHALL IBM, CARNEGIE MELLON UNIVERSITY, OR ANY OTHER COPYRIGHT HOLDER 
# BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
# DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, 
# WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS 
# ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
# OF THIS SOFTWARE.
#  $ */
#define In_Imake 1
/* The next two lines need to be kept in sync */
#include <i386_bsdi/system.h>
        SYSTEM_H_FILE = i386_bsdi/system.h
#undef In_Imake

/* These next three lines help configure the embedded machine-dependent
    directories overhead/class/machdep, atk/console/stats, and
    atk/console/stats/common. */
        SYS_IDENT = i386_bsdi
        SYS_OS_ARCH = i386_bsdi

/* Get parent inclusions */
#include <allsys.mcr>

STD_DEFINES = -DUCB
COMPILERFLAGS=

/* Now for the system-dependent information. */
CC = cc
GNULIB = /usr/lib/libgcc.a
XUTILDIR = /usr/X11/bin
XLIBDIR = /usr/X11/lib
XLIB = -L$(XLIBDIR) -lX11
LEX = flex
CLIB=/usr/lib/libc.a
CRT0PATH=/usr/lib/crt0.o
XBASEDIR=/usr/X11
XINCDIR=/usr/X11/include
XLIBDIR=/usr/X11/lib
XBINDIR=/usr/X11/bin
XUTILDIR=/usr/X11/bin
XLIB=/usr/X11/lib/libX11.a
IMAKE=/usr/X11/bin/imake
XMAKEDEPEND=/usr/X11/bin/makedepend
XMKFONTDIR=/usr/X11/bin/mkfontdir
XFC=/usr/X11/bin/bdftopcf
INSTALL=/usr/bin/install
SHELL=/usr/contrib/bin/bash 
SUPLIBS = $(BASEDIR)/lib/libatkos.a $(BASEDIR)/lib/libossup.a

/* Get site-specific inclusions */
#include <site.mcr>
SYS_CONFDIR = i386_bsdi

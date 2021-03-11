/* Copyright IBM Corporation 1988,1991 - All Rights Reserved */

/*
	$Disclaimer: 
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
#  $
*/

/* For full copyright information see:'andrew/config/COPYRITE' */

#define In_Imake 1
/* The next two lines need to be kept in sync */
#include <sun4_51/system.h>
SYSTEM_H_FILE = sun4_51/system.h
#undef In_Imake

/* These next two lines help configure the embedded machine-dependent
    directories overhead/class/machdep, atk/console/stats, and
    atk/console/stats/common. */
SYS_IDENT = sun4_51
SYS_OS_ARCH = sun_sparc_51

/* Get parent inclusions */
#include <allsys.mcr>

#ifdef OPENWINDOWS_ENV
CONVERTFONT = /usr/openwin/bin/convertfont
BLDFAMILY = /usr/openwin/bin/bldfamily
#endif /*OPENWINDOWS_ENV */

AR = /usr/ccs/bin/ar cq
NM = /usr/ccs/bin/nm
YACC = /usr/ccs/bin/yacc
LEX = /usr/ccs/bin/lex
MAKE = /usr/ccs/bin/make
IMAKE = imake -DSOLARIS
RANLIB = echo ranlib is not needed this system
CPP = /usr/ccs/lib/cpp
MAKEFLAGS = S

RESOLVLIB = -lresolv
XUTILDIR = /usr/openwin/bin
XBINDIR = /usr/openwin/bin
XINCDIR = /usr/openwin/include
XLIBDIR = /usr/openwin/lib
XLIB = -L$(XLIBDIR)  -lX11
CLASSLIB = -L$(BASEDIR)/lib -lclass

STD_DEFINES = -DSOLARIS -DSYSV

ASMPP_CC = $(CC) -D_ASM
#ifdef GNU_ENV
CC = gcc -traditional
GCCLIB = "fill me in with the path to libgcc.a for example:/usr/local/lib/gcc-lib/sparc-sunos5/2.4.3/libgcc.a"
/* Flag to compile position-independent code. */
PICFLAG = 
#else
CC = cc -Xs
/* Apparently no PIC flag is necessary at least under Solaris 2.3*/
PICFLAG=
GCCLIB=
#endif

/* Libraries that are used for curses apps, like vui */
CURSESLIBS = -L/usr/ccs/lib -lcurses  -ltermcap

NETLIBS	= -lsocket -lnsl -lelf
SUPLIBS = $(BASEDIR)/lib/libossup.a	/* Berkeley-support hacks to avoid libucb.a */

SHARED_LIB_PATH = -R${BASEDIR}/lib

MAKEDOFLAGS = ${MAKEDODEBUG} -d ${BASEDIR}/lib -b ${BASEDIR}/bin ${SHARED_LIB_PATH}

/* Now for the system-dependent information. */

MALLOCALIGNMENT = 8

DYN_LINK_LIB = -ldl
SHLIBLDFLAGS = -G

/* Get site-specific inclusions */
#include <site.mcr>
SYS_CONFDIR = sun4_51

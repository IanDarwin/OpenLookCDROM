/* $Id: system.mcr,v 1.20 1994/02/13 17:42:25 rr2b Exp $ */

/*
	$Disclaimer: 
Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose is hereby granted without fee, 
provided that the above copyright notice appear in all copies and that 
both that copyright notice, this permission notice, and the following 
disclaimer appear in supporting documentation, and that the names of 
IBM, Carnegie Mellon University, and other copyright holders, not be 
used in advertising or publicity pertaining to distribution of the software 
without specific, written prior permission.

IBM, CARNEGIE MELLON UNIVERSITY, AND THE OTHER COPYRIGHT HOLDERS 
DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.  IN NO EVENT 
SHALL IBM, CARNEGIE MELLON UNIVERSITY, OR ANY OTHER COPYRIGHT HOLDER 
BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, 
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS 
ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
OF THIS SOFTWARE.
 $
*/

SHELL=/bin/sh
 
/* Copyright IBM Corporation 1988,1991 - All Rights Reserved */
/* For full copyright information see:'andrew/config/COPYRITE' */
 
#define In_Imake 1
/* The next two lines need to be kept in sync */
#include <rs_aix3/system.h>
        SYSTEM_H_FILE = rs_aix3/system.h
#undef In_Imake
 
/* These next two lines help configure the embedded machine-dependent
    directories overhead/class/machdep, atk/console/stats, and atk/console/stats/common. */
        SYS_IDENT = rs_aix3
        SYS_OS_ARCH = aix_3
 
/* Get parent inclusions */
#include <allsys.mcr>
 
/* Now for the system-dependent information. */
#ifdef GNU_ENV
CC = gcc
#else
CC = cc
#endif
STD_DEFINES = -DAIX -D_POSIX_SOURCE -D_NO_PROTO -D_ALL_SOURCE
 
RANLIB = echo ranlib is not needed this system


/* Installed exports file for users of libclass.a */
IBMEXPFILE = $(BASEDIR)/lib/libclass.exp
IBMEXP = -bE:$(IBMEXPFILE)

MAKEDOFLAGS = ${MAKEDODEBUG} -d ${BASEDIR}/lib -b ${BASEDIR}/bin ${SHARED_LIB_PATH}
 
/* The following macros are included in LDFLAGS, which is used when linking executables */
SUPLIBS = $(BASEDIR)/lib/libossup.a
CLASSLIB = -L$(BASEDIR)/lib -lclass
XLIB = -L$(XLIBDIR) -lX11
BSDLIB = -lbsd
SHARED_LIB_PATH = -L${BASEDIR}/lib

#define ConstructMFLAGS 1
 
/* Get site-specific inclusions */
 
#include <site.mcr>
 
SYS_CONFDIR = rs_aix3

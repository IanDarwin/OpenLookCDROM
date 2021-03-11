/* Copyright IBM Corporation 1988,1991 - All Rights Reserved */

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

/* For full copyright information see:'andrew/config/COPYRITE' */

#define In_Imake 1
/* The next two lines need to be kept in sync */
#include <sco_i386/system.h>
        SYSTEM_H_FILE = sco_i386/system.h
#undef In_Imake

/* These next three lines help configure the embedded machine-dependent
    directories overhead/class/machdep, atk/console/stats, and
    atk/console/stats/common. */
        SYS_IDENT = sco_i386
        SYS_OS_ARCH = sco_i386

/* Get parent inclusions */
#include <allsys.mcr>


/* Now for the system-dependent information. */
        CDEBUGFLAGS = -O
        STD_DEFINES = -DM_UNIX
        ConstructMFLAGS =
        XUTILDIR = /usr/bin/X11
        LN = ln
        DEPENDSCRIPT = $(CSHELL) $(TOP)/config/depend.csh
        RANLIB = echo ranlib is not required on this system
        NETLIBS = -lsocket
        MMDFLIBS = ${BASEDIR}/lib/libmmdf.a
        AUTHLIBS = -lcrypt -lprot
        SUPLIBS = $(BASEDIR)/lib/libatkos.a ${UTILLIB} -lX11

/* Get site-specific inclusions */
#include <site.mcr>
SYS_CONFDIR = sco_i386

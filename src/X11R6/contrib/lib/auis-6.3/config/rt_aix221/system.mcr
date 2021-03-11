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
#include <rt_aix221/system.h>
SYSTEM_H_FILE = rt_aix221/system.h
#undef In_Imake

/* These next two lines help configure the embedded machine-dependent
    directories overhead/class/machdep, atk/console/stats, and
    atk/console/stats/common. */
SYS_IDENT = rt_aix221
SYS_OS_ARCH = aix_rt

/* Get parent inclusions */
#include <allsys.mcr>


/* Now for the system-dependent information. */
STD_DEFINES = -Nn6000 -Nd8000 -Np2400 -Nt4000 \
	    -DBSD_INCLUDES -DBSD_REMAP_SIGNAL_TO_SIGVEC \
	    -U_C_func -a 

RANLIB = echo ranlib is not needed this system
XFC = /usr/lpp/X11/bin/bdftortx
XMKFONTDIR = /usr/lpp/X11/bin/mkfontdir
CURSESLIB = -lcurses -ltermcap

#define ConstructMFLAGS 1

CURSESLIBS = -lcurses
SUPLIBS = $(BASEDIR)/lib/libatkos.a

/* Get site-specific inclusions */
#include <site.mcr>
SYS_CONFDIR = rt_aix221

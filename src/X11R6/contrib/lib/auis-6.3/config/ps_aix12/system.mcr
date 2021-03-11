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
#include <ps_aix12/system.h>
        SYSTEM_H_FILE = ps_aix12/system.h
#undef In_Imake

/* These next two lines help configure the embedded machine-dependent
    directories overhead/class/machdep, atk/console/stats, and
    atk/console/stats/common. */
        SYS_IDENT = ps_aix12
        SYS_OS_ARCH = aix_i386

/* Get parent inclusions */
#include <allsys.mcr>


/* Now for the system-dependent information. */
        XSRCDIR = /
        INCLUDES =  -I${BASEDIR}/include/atk -I${BASEDIR}/include -I${XBASEDIR}/usr/include

        CC = cc
        STD_DEFINES =  -D_BSD
#define ConstructMFLAGS 1
        RANLIB = echo ranlib is not needed on this system

#ifdef LINKINSTALL_ENV
         RM = rm -fs
#else
          RM = rm -f
#endif /* LINKINSTALL_ENV */

          NM = /bin/nm
SUPLIBS = $(BASEDIR)/lib/libatkos.a

/* Get site-specific inclusions */
#include <site.mcr>
SYS_CONFDIR = ps_aix12

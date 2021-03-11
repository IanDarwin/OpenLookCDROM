/* Copyright IBM Corporation 1988,1989 - All Rights Reserved */

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

/* The next two lines need to be kept in sync */

#define In_Imake 1
#include <sgi_4d/system.h>
        SYSTEM_H_FILE = sgi_4d/system.h
#undef In_Imake

/* These next two lines help configure the embedded machine-dependent
    directories overhead/class/machdep, atk/console/stats, and
    atk/console/stats/common. */
        SYS_IDENT = sgi_4d
        SYS_OS_ARCH = sgi_mips

/* Get parent inclusions */
#include <allsys.mcr>


/* Now for the system-dependent information. */
        XSRCDIR = /usr/src/X11R4/mit/
        XBASEDIR =
        XUTILDIR = /usr/bin/X11
        RANLIB = echo ranlib is not needed on this system

/* MIPS' compiler seems to have the standard set of PCC bugs dealing with 
 *     void...
 * The -G 0 is to prevent dynamically loadable modules from having global
 *     area sections.
 * The -Wl,D,8000000 switch is to move the data area down and
 * the -Wl,T,4000000 switch is to move the text area up so that
 *     dynamically loaded routines are addressible within the 28 bit jump
 *     offset limit of the MIPS architecture.
 */
#ifdef ReadObjects
              CC = cc -G 0 -cckr "-Wl,-D,8000000" "-Wl,-T,4000000"
              CCNOGO = cc -cckr "-Wl,-D,8000000" "-Wl,-T,4000000"
#else /* Do mmap instead */
              CC = cc -G 0 -cckr
              CCNOG0 = cc -cckr
#endif

     STD_DEFINES = -DSYSV -D_BSD_SIGNALS
     SYS_LIBRARIES = -lsun
     MMDFLIBS = -lmld

    MKSDPOOL = ${BASEDIR}/etc/mksdpool

/* Set path to the standard libc.a and crt0.o files */
        CLIB = /usr/lib/libc_s.a
        CRT0PATH = /usr/lib/crt1.o /usr/lib/crtn.o
SUPLIBS = $(BASEDIR)/lib/libatkos.a

/* Get site-specific inclusions */
#include <site.mcr>
SYS_CONFDIR = sgi_4d

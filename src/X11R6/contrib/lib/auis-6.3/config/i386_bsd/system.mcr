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
#include <i386_bsd/system.h>
        SYSTEM_H_FILE = i386_bsd/system.h
#undef In_Imake

/* These next two lines help configure the embedded machine-dependent
    directories overhead/class/machdep, atk/console/stats, and
    atk/console/stats/common. */
        SYS_IDENT = i386_bsd
        SYS_OS_ARCH = i386_bsd


/* Get parent inclusions */
#include <allsys.mcr>

/* Now for the system-dependent information. */

        CDEBUGFLAGS = -O -fwritable-strings      
        SUPLIBS = $(BASEDIR)/lib/libatkos.a $(BASEDIR)/lib/libossup.a

        /* 386BSD/Xfree86 location of X directory tree */
        XBASEDIR = 
        XINCDIR = /usr/X386/include
        XLIBDIR = /usr/X386/lib
        XBINDIR = /usr/X386/bin
        XUTILDIR = /usr/X386/bin

	/* where this install program is */        
        INSTALL = /usr/bin/install

	/* where 386bsd things it's libraries are */
        CRT0PATH = /usr/lib/crt0.o
        CLIB = /usr/lib/libc.a
        LEXLIB= -ll

/* Get site-specific inclusions */
#include <site.mcr>
        SYS_CONFDIR = i386_bsd


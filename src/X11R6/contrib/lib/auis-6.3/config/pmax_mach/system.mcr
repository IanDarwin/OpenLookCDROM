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
#include <pmax_mach/system.h>
        SYSTEM_H_FILE = pmax_mach/system.h
#undef In_Imake

/* These next two lines help configure the embedded machine-dependent
    directories overhead/class/machdep, atk/console/stats, and
    atk/console/stats/common. */
        SYS_IDENT = pmax_mach
        SYS_OS_ARCH = dec_mach

/* Get parent inclusions */
#include <allsys.mcr>

/* Now for the system-dependent information. */
/* (currently none) */

/* MIPS' compiler seems to have the standard set of PCC bugs dealing with 
 *     void...
 * The -G 0 is to prevent dynamically loadable modules from having global
 *     area sections.
 * The -Wl,D,1000000 switch is to move the data area down so that
 *     dynamically loaded routines are addressible within the 28 bit jump
 *     offset limit of the MIPS architecture.
 */
        CC = cc -Dvoid=int -G 0
        CCNOG0 = cc -Dvoid=int
        COMPILERFLAGS =  -Wl,-D,1000000
        MIPS_GS_FILE = $(BASEDIR)/etc/mips_global_space.c
        MIPS_GSIZE_SCRIPT = $(BASEDIR)/etc/mips_gsize.awk

/* Get site-specific inclusions */
#include <site.mcr>

SYS_CONFDIR = pmax_mach

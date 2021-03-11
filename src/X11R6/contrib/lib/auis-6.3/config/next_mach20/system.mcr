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

/* The next two lines need to be kept in sync */
#include <next_mach20/system.h>
        SYSTEM_H_FILE = next_mach20/system.h

/* These next two lines help configure the embedded machine-dependent
    directories overhead/class/machdep, atk/console/stats, and
    atk/console/stats/common. */
        SYS_IDENT = next_mach20
      SYS_OS_ARCH = next_mach
   CONSOLESYSLIBS = -lmach

/* Get parent inclusions */
#include <allsys.mcr>


/* Now for the system-dependent information.  -Dconst is needed because some include files in system directories aren't properly ifdefed for STDC, -traditional is needed to avoid incompatibilities, -D__STRICT_BSD__ is needed to get BSDisms to work. */
COMPILERFLAGS= -Dconst= -traditional -D__STRICT_BSD__

/* We'll strip class programs manually in the InstallClassProgram macro defined in andrew.rls */
INSTCLASSPROGFLAGS= -ns -c -m 0555
SUPLIBS = $(BASEDIR)/lib/libatkos.a

/* Get site-specific inclusions */
#include <site.mcr>
SYS_CONFDIR = next_mach20

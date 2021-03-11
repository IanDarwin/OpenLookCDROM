/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */

/*
	$Disclaimer: 
 * Permission to use, copy, modify, and distribute this software and its 
 * documentation for any purpose is hereby granted without fee, 
 * provided that the above copyright notice appear in all copies and that 
 * both that copyright notice, this permission notice, and the following 
 * disclaimer appear in supporting documentation, and that the names of 
 * IBM, Carnegie Mellon University, and other copyright holders, not be 
 * used in advertising or publicity pertaining to distribution of the software 
 * without specific, written prior permission.
 * 
 * IBM, CARNEGIE MELLON UNIVERSITY, AND THE OTHER COPYRIGHT HOLDERS 
 * DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
 * ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.  IN NO EVENT 
 * SHALL IBM, CARNEGIE MELLON UNIVERSITY, OR ANY OTHER COPYRIGHT HOLDER 
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
 * DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, 
 * WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS 
 * ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
 * OF THIS SOFTWARE.
 *  $
*/

#ifndef	SYSTEM_H
#define	SYSTEM_H
 
/* Get common definitions */
 
#include <allsys.h>
 
#define	OPSYSNAME	"risc_aix3"
#define	sys_rt_aix3	1
#define	SYS_NAME	"rs_aix3"

#define ANSI_COMPILER 1
#define HAVE_SHARED_LIBRARIES 1

#ifndef _IBMR2
#define _IBMR2 1
#endif /* _IBMR2 */ 

/* These are here for AIX support. */
 
#undef SY_AIX3
#define SY_AIX3 1	/* define for AIX 3.1 */
#undef POSIX_ENV
#define POSIX_ENV 1	/* This is a Posix system. */

#ifndef In_Imake

#include <sys/types.h>
#include <stdlib.h>
#include <strings.h>
#include <unistd.h>

/* Get open(2) constants */
#include <sys/file.h>
#include <sys/select.h>

#include <signal.h>
#undef SIGSET_TYPE
#define SIGSET_TYPE sigset_t

#include <dirent.h>
#define DIRENT_TYPE struct dirent
#define DIRENT_NAMELEN(d) (strlen((d)->d_name))
#define NEWPGRP() setpgrp(0, getpid())

/* Get struct timeval */
#include <time.h>
#include <sys/time.h>

/* include path for syslog.h */
#include <sys/syslog.h>

#define OSI_HAS_SYMLINKS 1
/* If OSI_HAS_SYMLINKS is not defined, osi_readlink is present in libutil. */
#define osi_readlink(PATH,BUF,SIZE) readlink((PATH),(BUF),(SIZE))
#include <sys/flock.h>
#define osi_ExclusiveLockNoBlock(fid)	lockf((fid), F_TLOCK, 0)
#define osi_UnLock(fid)			lockf((fid), F_ULOCK, 0)

#define osi_O_READLOCK			O_RDWR
#define osi_F_READLOCK			"r+"

/* handle (BSD) vfork for (AIX) which only knows fork */
#define	osi_vfork()			fork()

/* Handle the absence of _setjmp and _longjmp on AIX. */
#define	osi_setjmp  setjmp
#define	osi_longjmp longjmp

/* Make a time standard. */
struct osi_Times {unsigned long int Secs; unsigned long int USecs;};
/* Set one of the above with a call to osi_GetTimes(&foo) */
#define osi_GetSecs() time((long int *) 0)
#define osi_SetZone() tzset()
#define osi_ZoneNames tzname
#define osi_SecondsWest timezone
#define osi_IsEverDaylight daylight

/* More BSD-isms */
#define setlinebuf(file) setvbuf(file, NULL, _IOLBF, BUFSIZ)

/*
 * Put system-specific definitions here
 */
#define HAS_SYSEXITS 1
#define BIT_ZERO_ON_LEFT 1

#define setreuid(r,e) atk_setuid(r)

#endif /* !In_Imake */

#define GETDOMAIN_ENV 1
#undef ANDREW_MALLOC_ENV

/* Now follow the site-specific customizations. */
 
#include <site.h>

#endif	/* SYSTEM_H */
 

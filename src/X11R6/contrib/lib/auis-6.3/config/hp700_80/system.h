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

#define	OPSYSNAME	"HPUX_700"
#define	sys_hpux_700	1
#define	SYS_NAME	"hp700"
#define	HP700_ENV
#define	HP_OS		80

#define HAVE_SHARED_LIBRARIES 1

/* Here follow the overrides for this system. */
#undef	SY_U52
#define	SY_U52	1   /* HP700 is most like SysVR2. */

#ifndef In_Imake

#include <signal.h>
#undef SIGSET_TYPE
#define SIGSET_TYPE sigset_t

#ifndef XLIB_ILLEGAL_ACCESS
#define XLIB_ILLEGAL_ACCESS 1
#endif

#ifndef CLASS_CTRAMPOLINE_ENV
#define CLASS_CTRAMPOLINE_ENV 1
#endif

#ifndef SYSV
#define SYSV	1
#endif SYSV

#ifndef VMUNIX
#define VMUNIX 1
#endif

#ifndef POSIX_ENV
#define POSIX_ENV 1
#endif

/* Get major data types (esp. caddr_t) */
#include <sys/types.h>

#define SYSV_STRINGS
#include <string.h>
#define index strchr
#define rindex strrchr

/* Get open(2) constants */
#include <fcntl.h>
#include <sys/file.h>

#include <dirent.h>
#define DIRENT_TYPE struct dirent
#define DIRENT_NAMELEN(d) (strlen((d)->d_name))
#define NEWPGRP() setpgrp()

/* Get struct timeval */
#include <time.h>

/* More BSDisms */

#include <syslog.h>

/* getdtablesize() and an errno which does not seem to be defined for SYSV */
#define EDOM				33
#define getdtablesize()			_NFILE
#define setpriority(which,who,prio) (nice((prio)-nice(0)))

#define OSI_HAS_SYMLINKS 0
/* If OSI_HAS_SYMLINKS is not defined, osi_readlink is present in libutil. */
extern int osi_readlink();

#include <unistd.h>
#define osi_ExclusiveLockNoBlock(fid)	hp_lockf(fid)
#define osi_UnLock(fid)			hp_unlockf(fid)
#define osi_O_READLOCK			O_RDWR
#define osi_F_READLOCK			"r+"

#define	osi_vfork()			vfork()

#define	osi_setjmp  _setjmp
#define	osi_longjmp _longjmp

/* Make a time standard. */
struct osi_Times {unsigned long int Secs; unsigned long int USecs;};
/* Set one of the above with a call to osi_GetTimes(&foo) */
#define osi_GetSecs() time((long int *) 0)
#define osi_SetZone() tzset()
#define osi_ZoneNames tzname
#define osi_SecondsWest timezone
#define osi_IsEverDaylight daylight

/* More BSD-isms */
#define setreuid(r,e) setuid(r)
#define setlinebuf(file) setvbuf(file, NULL, _IOLBF, BUFSIZ)

/*
 * Put system-specific definitions here
 */

#undef setreuid
#define setreuid(r,e) setresuid(r,e,-1)

#define sigvec sigvector

#define random rand
#define srandom srand
#define initstate(a,b,c) srand(a)
#define bcopy(src, dst, length) memmove(dst, src, length)
#define bzero(b, length) memset(b, 0, length)
#define bcmp(region1, region2, length) memcmp(region1, region2, length)
#define killpg(pgid, signal) kill(-(pgid), signal)
#undef MAX
#undef MIN
#include <sys/param.h>
#define getwd(pathname) getcwd(pathname, MAXPATHLEN)

/* hpux has no sysexits.h file anywhere. */
#ifndef EX__BASE
#define EX_OK 0
#define EX__BASE 64
#define EX_USAGE 64
#define EX_DATAERR 65
#define EX_NOINPUT 66
#define EX_NOUSER 67
#define EX_NOHOST 68
#define EX_UNAVAILABLE 69
#define EX_SOFTWARE 70
#define EX_OSERR 71
#define EX_OSFILE 72
#define EX_CANTCREAT 73
#define EX_IOERR 74
#define EX_TEMPFAIL 75
#define EX_PROTOCOL 76
#define EX_NOPERM 77
#endif /* #ifndef EX__BASE */

#endif /* !In_Imake */

#define GETDOMAIN_ENV 1
#define	BUILDANDREWINSTALL_ENV	1
#ifndef ANDREW_MALLOC_ENV
#define ANDREW_MALLOC_ENV 1
#endif

#define NEED_UTIMES 1
#define NEED_RANDOM_MAPPED_TO_RAND48 1
#define NEED_INSREMQUE 1
/* Now follow the site-specific customizations. */
#include <site.h>
#ifdef CMU_ENV
#undef AMS_SUBSCRIPTIONMAPFILE
#define  AMS_SUBSCRIPTIONMAPFILE ".SubscriptionMap"
#endif

#endif	/* SYSTEM_H */

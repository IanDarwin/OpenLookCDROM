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

#define	OPSYSNAME	"ps2_aix12"
#define	sys_ps_aix12	1
#define	SYS_NAME	"ps_aix12"
#ifndef	i386_ENV
#define	i386_ENV
#endif /* i386_ENV */

/* Here follow the overrides for this system. */
#undef	SY_AIX12
#define	SY_AIX12	1 /* This system is most like AIX 1.2 */
#undef POSIX_ENV
#define POSIX_ENV	1 /* This is a Posix system. */

#ifndef In_Imake

#include <signal.h>
#define SIGSET_TYPE sigset_t

/* Get major data types (esp. caddr_t) */
#include <sys/types.h>

#ifndef BSD
#define BSD
#endif /* BSD */
#include <strings.h>

/* Get open(2) constants */
#include <sys/file.h>

#include <dirent.h>
#define DIRENT_TYPE struct dirent
#define DIRENT_NAMELEN(d) (strlen((d)->d_name))
#define NEWPGRP() setpgrp(0, getpid())

/* Get struct timeval */
#include <sys/time.h>

/* include path for syslog.h BSD vs SYSV */
#include <sys/syslog.h>

#define OSI_HAS_SYMLINKS 1
#define osi_readlink(PATH,BUF,SIZE) readlink((PATH),(BUF),(SIZE))

#include <sys/lockf.h>
#if SY_AIX12 && defined(AFS_ENV) /* hack for BSD NFS/AFS bridge flock bug */
#define osi_ExclusiveLockNoBlock(fid)	(0)
#define osi_UnLock(fid)			(0)
#else
#define osi_ExclusiveLockNoBlock(fid)	lockf((fid), F_TLOCK, 0)
#define osi_UnLock(fid)			lockf((fid), F_ULOCK, 0)
#endif /* SY_AIX12 && defined(AFS_ENV) */
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
#define setreuid(r,e) setuid(r)
#define setlinebuf(file) setvbuf(file, NULL, _IOLBF, BUFSIZ)

/*
 * Put system-specific definitions here
 */

#define HAS_SYSEXITS 1
#define NEED_ANSI_TMPFILES 1
#ifndef IN_ATKOS_LIB
#include <atkos.h>
#endif

#endif /* !In_Imake */

#undef RESOLVER_ENV

/* Now follow the site-specific customizations. */
#include <site.h>

#endif	/* SYSTEM_H */

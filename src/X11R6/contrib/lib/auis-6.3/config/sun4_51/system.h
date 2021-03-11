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

#define	OPSYSNAME	"SUN4_SVR4"
#define	sys_sun4_51	1
#define	SYS_NAME	"sun4_51"
#define	SUN_ENV

/* SunOS 5.1 has the "-ldl" library which contains dlopen et al. */
#ifndef LIBDL_ENV
#define LIBDL_ENV 1
#endif /* LIBDL_ENV */

#define SUN_OS 51

#undef OPENWINDOWS_ENV

#undef SOLARIS
#define SOLARIS 1

#define NO_SHARED_DIR 1

#undef ANSI_COMPILER 

#undef POSIX_ENV
#define POSIX_ENV 1

/* Here follow the overrides for this system. */
#undef	SY_U54
#define	SY_U54  1 /* This system is most like SVR4 */
#undef SY_U5x
#define SY_U5x 1

#define HAVE_SHARED_LIBRARIES 1

#ifndef In_Imake
/* ugly gross blecherous hack, this ensures that #including curses.h won't screw up _ctype for isspace and friends. */
#define _EUC_H 1 
#undef TMACMANFILE
#define TMACMANFILE "/usr/lib/tmac/an"
#undef TMACPREFIX
#define TMACPREFIX "/usr/lib/tmac/"

#undef BSD_COMP
#define BSD_COMP	/* for networking */

#ifndef SYSV
#define SYSV	1
#endif /* SYSV */

#undef USG

/* Get major data types (esp. caddr_t) */
#include <sys/types.h>

#include <dirent.h>
#define DIRENT_TYPE struct dirent
#define DIRENT_NAMELEN(d) (strlen((d)->d_name))
#define NEWPGRP() setpgrp()

#define SYSV_STRINGS
#include <string.h>

/* Get open(2) constants */
#include <fcntl.h>
#include <sys/file.h>

/* Get struct timeval */
#include <time.h>
#include <sys/time.h>

/* include path for syslog.h */
#include <syslog.h>

#ifndef VMUNIX
#define	VMUNIX	1
#endif /* VMUNIX */

#define OSI_HAS_SYMLINKS 1

#define osi_readlink(PATH,BUF,SIZE) readlink((PATH),(BUF),(SIZE))

#include <unistd.h>
#if 0
#define osi_ExclusiveLockNoBlock(fid)	lockf((fid), F_TLOCK, 0)
#define osi_UnLock(fid)			lockf((fid), F_ULOCK, 0)
#endif
#define osi_O_READLOCK			O_RDWR
#define osi_F_READLOCK			"r+"

#define	osi_vfork()			vfork()

#define	osi_setjmp  _setjmp
#define	osi_longjmp _longjmp

#define HAS_GETTIMEOFDAY 1
/* Make a time standard. */
struct osi_Times {unsigned long int Secs; unsigned long int USecs;};
/* Set one of the above with a call to osi_GetTimes(&foo) */
#define osi_GetSecs() time((long int *) 0)
#define osi_SetZone() tzset()
#define osi_ZoneNames tzname
#define osi_SecondsWest timezone
#define osi_IsEverDaylight daylight

/*
 * Put system-specific definitions here
 */

#define setpriority(which, who, prio) (nice((prio)-nice(0)))

#define random rand
#define srandom srand
#define initstate(a,b,c) srand(a)

#include <signal.h>
#ifdef SIGSET_TYPE
#undef SIGSET_TYPE
#endif
#define SIGSET_TYPE sigset_t

#define killpg(pgid, signal) kill(-(pgid), signal)

#include <sys/param.h>
#define getwd(pathname) getcwd(pathname, MAXPATHLEN)

#ifdef L_INCR
#undef L_INCR
#endif
#define L_INCR	SEEK_CUR

#ifdef L_XTND
#undef L_XTND
#endif
#define L_XTND	SEEK_END

#ifdef HAS_SYSEXITS
#undef HAS_SYSEXITS
#endif
# define EX_OK          0       /* successful termination */
# define EX__BASE       64      /* base value for error messages */
# define EX_USAGE       64      /* command line usage error */
# define EX_DATAERR     65      /* data format error */
# define EX_NOINPUT     66      /* cannot open input */
# define EX_NOUSER      67      /* addressee unknown */
# define EX_NOHOST      68      /* host name unknown */
# define EX_UNAVAILABLE 69      /* service unavailable */
# define EX_SOFTWARE    70      /* internal software error */
# define EX_OSERR       71      /* system error (e.g., can't fork) */
# define EX_OSFILE      72      /* critical OS file missing */
# define EX_CANTCREAT   73      /* can't create (user) output file */
# define EX_IOERR       74      /* input/output error */
# define EX_TEMPFAIL    75      /* temp failure; user is invited to retry */
# define EX_PROTOCOL    76      /* remote error in protocol */
# define EX_NOPERM      77      /* permission denied */

/* We define gethostname here rather than using bsd compat
 library because readdir there conflicts with readdir in libc
 and you lose your butt if EVERYTHING is not exactly right. */
#include <sys/systeminfo.h>
#define gethostname(name,namelen) \
  sysinfo(SI_HOSTNAME,(name),(namelen))

/* double sigh, the preprocessor is broken or I'd use macros for this -rr2b */
extern char *index();
extern char *rindex();

#endif /* !In_Imake */

#define	NDBM_ENV	1

#ifdef GETDOMAIN_ENV
#undef GETDOMAIN_ENV
#endif

#ifdef ANDREW_MALLOC_ENV
#undef ANDREW_MALLOC_ENV
#endif

#define BUILDANDREWINSTALL_ENV 1

/* Now follow the site-specific customizations. */
#include <site.h>

#endif	/* SYSTEM_H */

/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */
#ifndef SYSTEM_H
#define SYSTEM_H

/* Get common definitions */
#include <allsys.h>

#define OPSYSNAME       "i386_Linux"
#define sys_i386_linux  1
#define SYS_NAME        "i386_Linux"
#define i386_ENV        1

/* Here follow the overrides for this system. */
#undef  SY_U53
#define SY_U53  1 /* This system is most like SVR3 */
#undef SY_U5x
#define SY_U5x 1

#define HAVE_SHARED_LIBRARIES 1
#define HAS_GETTIMEOFDAY 1

#ifndef In_Imake

#undef FILE_NEEDS_FLUSH
#define FILE_NEEDS_FLUSH(f) (1)

#undef SIGSET_TYPE
#define SIGSET_TYPE sigset_t

#undef TMACMANFILE
#define TMACMANFILE "/usr/lib/groff/tmac/tmac.an"
#undef TMACPREFIX
#define TMACPREFIX "/usr/lib/groff/tmac/tmac."

#undef FILE_HAS_IO
#define FILE_HAS_IO(f) ((f)->_IO_read_end - (f)->_IO_read_ptr)

#define NDEBUG			/* some places use asserts()'s, but */
				/* linux doesn't have ___eprintf() in */
				/* the shared libs for some reason. So */
				/* we just turn them off. */

#ifndef X_NOT_STDC_ENV
#define X_NOT_STDC_ENV 1
#endif

#ifndef SYSV
#define SYSV    1
#endif /* SYSV */
#ifndef USG
#define USG 1
#endif /* USG */

#include <unistd.h>
#include <sys/time.h>   /* for timeval, timercmp */

/* Get major data types (esp. caddr_t) */
#include <sys/types.h>

#define SYSV_STRINGS
#include <string.h>

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

#define OSI_HAS_SYMLINKS 1

#define osi_readlink(PATH,BUF,SIZE) readlink((PATH),(BUF),(SIZE))

#define osi_ExclusiveLockNoBlock(fid)   flock((fid), LOCK_EX | LOCK_NB)
#define osi_UnLock(fid)                 flock((fid), LOCK_UN)
#define osi_O_READLOCK                  O_RDONLY
#define osi_F_READLOCK                  "r"

#define osi_vfork()                     fork()

#define osi_setjmp  _setjmp
#define osi_longjmp _longjmp

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


#include <sys/param.h>
#define getwd(pathname) getcwd(pathname, MAXPATHLEN)

#ifndef FNDELAY
#define FNDELAY O_NONBLOCK
#endif

#endif /* !In_Imake */

#ifndef FLEX_ENV
#define FLEX_ENV 1
#endif

#ifndef POSIX_ENV
#define POSIX_ENV       1
#endif /* POSIX_ENV */

#define USE_VARARGS
#define ANSI_COMPILER 1

/* Now follow the site-specific customizations. */
#include <site.h>

#endif  /* SYSTEM_H */

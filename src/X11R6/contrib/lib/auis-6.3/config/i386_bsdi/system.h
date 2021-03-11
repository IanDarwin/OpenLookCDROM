/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */

/* $Disclaimer: 
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
 *  $ */
#ifndef SYSTEM_H
#define SYSTEM_H

/* Get common definitions */
#include <allsys.h>

#define OPSYSNAME       "i386_bsdi"
#define sys_i386_bsdi  1
#define SYS_NAME        "i386_bsdi"

/* Here follow the overrides for this system. */
#undef  SY_B43
#define SY_B43  1 /* This system is most like bsd 4.3 */

#ifndef In_Imake

/*
#include <sys/types.h>

#include <string.h>
*/
#include <strings.h>

/*  Get open(2) constants */
/*
#include <sys/file.h>
*/

/*
#include <dirent.h>
*/

#undef SIGSET_TYPE
#define SIGSET_TYPE sigset_t

#undef TMACMANFILE
#define TMACMANFILE "/usr/share/tmac/tmac.an"
#undef TMACPREFIX
#define TMACPREFIX "/usr/share/tmac/tmac."

#undef FILE_HAS_IO
#define FILE_HAS_IO(f) ((f)->_bf._size)
#undef FILE_NEEDS_FLUSH
#define FILE_NEEDS_FLUSH(f) (1)

#ifndef USG
#define USG 1
#endif /* USG */

#ifdef POSIX_ENV
#undef POSIX_ENV
#endif /* POSIX_ENV */
#define POSIX_ENV 1

#ifndef VMUNIX
#define VMUNIX  1
#endif /* VMUNIX */

#include <unistd.h>
#include <sys/time.h>   /* for timeval, timercmp */

/* Get major data types (esp. caddr_t) */
#include <sys/types.h>

#include <string.h>

/* Get open(2) constants */
#include <fcntl.h>
#include <sys/file.h>

#include <dirent.h>
#define DIRENT_TYPE struct dirent
#define DIRENT_NAMELEN(d) (strlen((d)->d_name))
#define NEWPGRP() setpgrp(0, getpid())

/* Get struct timeval */
#include <time.h>

/* helps reduce errors */
#undef MAX
#undef MIN

/* More BSDisms */

#define OSI_HAS_SYMLINKS 1

#define osi_readlink(PATH,BUF,SIZE) readlink((PATH),(BUF),(SIZE))

#define osi_ExclusiveLockNoBlock(fid)   flock((fid), LOCK_EX | LOCK_NB)
#define osi_UnLock(fid)                 flock((fid), LOCK_UN)
#define osi_O_READLOCK                  O_RDONLY
#define osi_F_READLOCK                  "r"

#define osi_vfork()                     vfork()

#define osi_setjmp  _setjmp
#define osi_longjmp _longjmp

/* Make a time standard. */
struct osi_Times {unsigned long int Secs; unsigned long int USecs;};
/* Set one of the above with a call to osi_GetTimes(&foo) */
#define osi_GetSecs() time((long int *) 0)
/*
#define osi_SetZone() tzset()
#define osi_ZoneNames tzname
#define osi_SecondsWest timezone
*/

extern void osi_SetZone();
extern char *osi_ZoneNames[];
extern long int osi_SecondsWest;
extern int osi_IsEverDaylight;
/*
 * Put system-specific definitions here
 */

#define HAS_SYSEXITS 1

#include <sys/param.h>

#ifndef FNDELAY
#define FNDELAY O_NONBLOCK
#endif

#define stty(fd,buf) ioctl((fd), TIOCSETP, (buf))
#define gtty(fd,buf) ioctl((fd), TIOCGETP, (buf))

#endif /* !In_Imake */

#ifndef FLEX_ENV
#define FLEX_ENV 1
#endif

#define USE_VARARGS
#define ANSI_COMPILER 1

/* no ditroff */
#undef DITROFF_ENV

/* Now follow the site-specific customizations. */
#include <site.h>

#endif  /* SYSTEM_H */

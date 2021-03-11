/* ********************************************************************** *\
 *         Copyright TELMAT Informatique 1991,1993 - All Rights Reserved  *
 *         For full copyright information see:'andrew/config/COPYRITE'    *
\* ********************************************************************** */

#ifndef TELMATSVR4_SYSTEM_H
#define TELMATSVR4_SYSTEM_H

/* Get common definitions */
#include <allsys.h>

#define	OPSYSNAME	"Unix System V 4.0 R40V1 mc88100"
#define	sys_telmat	1
#define	SYS_NAME	"telmat_svr4"

#ifdef LIBDL_ENV
#undef LIBDL_ENV
#endif
#define LIBDL_ENV 1

#define NO_SHARED_DIR 1
#define SHARED_LIBCLASS libclass.a
#define NO_ALT_SHARED_LIBCLASS 1
#define CLASS_CTRAMPOLINE_ENV 1

#ifndef In_Imake

#undef TMACMANFILE
#define TMACMANFILE "/usr/ucblib/doctools/tmac/an"
#undef TMACPREFIX
#define TMACPREFIX "/usr/ucblib/doctools/tmac/"
#undef MANDIR
#define MANDIR "/usr/share/man"

#ifdef NDBM_ENV
#undef NDBM_ENV
#endif

#ifndef POSIX_ENV
#define POSIX_ENV 1
#endif

#ifndef SYSV
#define SYSV	1
#endif /* SYSV */

#ifdef USG
#undef USG
#endif
#define USG 1

#ifdef SY_U54
#undef SY_U54
#endif /* SY_U54 */
#define SY_U54	1

#ifdef VMUNIX
#undef VMUNIX
#endif
#define VMUNIX

#undef vfork
#define vfork _abi_vfork
#undef gettimeofday
#define gettimeofday _abi_gettimeofday
#undef sigsetjmp
#define sigsetjmp _abi_sigsetjmp
#undef sigaction
#define sigaction _abi_sigaction
#undef sysinfo
#define sysinfo _abi_sysinfo
#undef fcvt
#define fcvt _abi_fcvt
#undef setlogmask
#define setlogmask _abi_setlogmask
#undef truncate
#define truncate _abi_truncate
#undef sigset
#define sigset _abi_sigset
#undef openlog
#define openlog _abi_openlog
#undef siglongjmp
#define siglongjmp _abi_siglongjmp
#undef tcsetattr
#define tcsetattr _abi_tcsetattr
#undef syslog
#define syslog _abi_syslog
#undef hrtcntl
#define hrtcntl _abi_hrtcntl
#undef cfgetospeed
#define cfgetospeed _abi_cfgetospeed
#undef ftruncate
#define ftruncate _abi_ftruncate
#undef gethz
#define gethz _abi_gethz
#undef cfsetospeed
#define cfsetospeed _abi_cfsetospeed
#undef getdents
#define getdents _abi_getdents
#undef settimeofday
#define settimeofday _abi_settimeofday
#undef signal
#define signal _abi_signal
#undef utime
#define utime _abi_utime
#undef vsyslog
#define vsyslog _abi_vsyslog
#undef cfgetispeed
#define cfgetispeed _abi_cfgetispeed
#undef tcgetattr
#define tcgetattr _abi_tcgetattr
#undef cfsetispeed
#define cfsetispeed _abi_cfsetispeed
#undef ecvt
#define ecvt _abi_ecvt
#undef select
#define select _abi_select
#undef ttyslot
#define ttyslot _abi_ttyslot
#undef longjmp
#define longjmp _abi_longjmp
#undef ptrace
#define ptrace _abi_ptrace
#undef syscall
#define syscall _abi_syscall
#undef closelog
#define closelog _abi_closelog
#undef setjmp
#define setjmp _abi_setjmp

/* Get major data types (esp. caddr_t) */
#include <sys/types.h>
#include <sys/param.h>
#include <string.h>

/*  Get open(2) constants */
#include <fcntl.h>
#include <sys/file.h>

#include <dirent.h>
#define DIRENT_TYPE struct dirent
#define DIRENT_NAMELEN(d) (strlen((d)->d_name))
#define NEWPGRP() setpgrp()

/*  Get struct timeval */
#include <sys/time.h>

/* include path for syslog.h */
#include <syslog.h>

#ifndef PMAX_ENV
#define PMAX_ENV 1
#endif /* PMAX_ENV */

#ifndef SOCKET
#define SOCKET
#endif /* SOCKET */

#define OSI_HAS_SYMLINKS 1

#include <errno.h>
#include <unistd.h>

#ifdef OSI_HAS_SYMLINKS
#define osi_readlink(PATH,BUF,SIZE) readlink((PATH),(BUF),(SIZE))
#endif /* OSI_HAS_SYMLINKS */

#define osi_ExclusiveLockNoBlock(fid)	lockf((fid), F_TLOCK, 0)
#define osi_UnLock(fid)			lockf((fid), F_ULOCK, 0)
#define osi_O_READLOCK			O_RDWR
#define osi_F_READLOCK			"r+"

#define	osi_fork			fork
#define	osi_vfork			vfork

#define	osi_setjmp  setjmp
#define	osi_longjmp longjmp

/* Make a time standard. */
#include <time.h>
struct osi_Times {time_t Secs; time_t USecs;};
/* Set one of the above with a call to osi_GetTimes(&foo) */
#define osi_GetSecs() time((time_t *) 0)
#define osi_SetZone   tzset
#define osi_ZoneNames tzname
#define osi_SecondsWest timezone
#define osi_IsEverDaylight daylight
#define setreuid(r,e) setuid(r)

/*
 * Put system-specific definitions here
 */

#ifdef  HAS_SYSEXITS
#undef  HAS_SYSEXITS
/* hpux, for example, has no sysexits.h file anywhere. */
#endif  /* HAS_SYSEXITS */

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


#ifdef index
#undef index
#endif
#define index(s, c) strchr(s, c)

#ifdef rindex
#undef rindex
#endif
#define rindex(s, c) strrchr(s, c)

#ifdef bcopy
#undef bcopy
#endif
#define bcopy(src, dest, len) memmove(dest, src, len)

#ifdef bcmp
#undef bcmp
#endif
#define bcmp(s1, s2, len) memcmp(s1, s2, len)

#ifdef bzero
#undef bzero
#endif
#define bzero(s, len) memset(s, 0, len)

#define killpg(pgid,signal) kill(-(pgid),(signal))

#define getdtablesize() _NFILE
#define getwd(pathname) getcwd(pathname, MAXPATHLEN)

#define random rand
#define srandom srand

#define initstate(a,b,c) srand(a)
#include <sys/mntent.h>
#define setlinebuf(file) setvbuf((file), NULL, _IOLBF, BUFSIZ)

#endif /* !In_Imake */


#define BUILDANDREWINSTALL_ENV 1

#ifdef ANDREW_MALLOC_ENV
#undef ANDREW_MALLOC_ENV
#endif

#ifdef FONTS_TO_BDF_ENV
#undef FONTS_TO_BDF_ENV
#endif

#define CONTRIB_ENV 1

#define MK_BASIC_UTILS 1
#define MK_AUTHORING 1
#define MK_AUX_UTILS 1
#define MK_INSETS 1
#define MK_EXAMPLES 1
#define MK_METAMAIL 1
#define MK_CALC 1
#define MK_CHAMP 1
#define MK_ZIP 1

#undef RESOLVER_ENV
/*
#define AMS_DELIVERY_ENV 1
#define SNAP_ENV 1
#define WHITEPAGES_ENV 1
#define RUN_AMDS_ENV 1
*/
#define RTF_ENV 1

#define SCRIBETEXT_ENV 1

#define GETDOMAIN_ENV 1

/* Now follow the site-specific customizations. */
#include <site.h>

#endif /* TELMATSVR4_SYSTEM_H */









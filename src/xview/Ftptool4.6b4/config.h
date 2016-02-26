
#pragma ident   "@(#)config.h 1.6     93/11/05"

/*
 * If you use Sun Consulting's Proxy FTP, set this up appropriately
 *	DEFAULT_PROXY is the ftp gateway
*/
#define	DEFAULT_HOST 	"yavin.Eng"
#define	DEFAULT_PROXY	"sun-barr.EBay"
#define	DEFAULT_ALIAS	"Engineering"
#define	DEFAULT_COMMENT "Home of Ftptool"

#define	UNIX_DIR_PATTERN "PERMS LINKS USER GROUP SIZE MONTH DAY TIME NAME"

/* define this to be a global .ftptoolrc */
/* ftptool will load this if $HOME/.ftptoolrc does not exist */
#ifndef GLOBAL_FTPTOOLRC
#define	GLOBAL_FTPTOOLRC "/usr/local/lib/ftptoolrc"
#endif

/* The following are the filenames that ftptool will create in the */
/* users home directory. You probably don't want to change this. */
#define	FTPTOOL_RC			".ftptoolrc"
#define	FTPTOOL_LAYOUT		".ftptoollayout"
#define	FTPTOOL_DEFAULTS	".ftptooldefaults"
#define	FTPTOOL_TYPES		".ftptooltypes"

/* This is the maximum size the magic number of the file can be */
/* Actually an old term, but this is really how much ftptool will */
/* try to read in to determine the type of file when starting a */
/* viewer. */
#define	MAX_MAGIC_SIZE		10

#if defined(SYSV) || defined(SYSV386)

#include <memory.h>
#include <sys/utsname.h>
#include <sys/systeminfo.h>


#define	bzero(b, length)	memset(b, '\0', length)
#define	bcopy(b1, b2, length)	memcpy(b2, b1, length)

#else

void bzero();
void bcopy();

#endif

/* USE_PROTOTYPES also implies you use <stdarg.h> and not <varargs.h> */
#ifndef USE_PROTOTYPES
#if __STDC__ == 1 || defined(SYSV) || defined(SVR4) || defined(SYSV386)
#define	USE_PROTOTYPES
#endif
#endif

/* prototypes that may be necessary */

#ifdef USE_PROTOTYPES

int strcasecmp(const char *s1, const char *s2);

#else

int strcasecmp();

#endif


/* Define NEED_STRCASECMP if you don't have it */
#ifndef NEED_STRCASECMP
#if defined(SYSV386)
#define	NEED_STRCASECMP
#endif
#endif

/* Inside Sun, our NIS domains are of the form 'something.realdomain.Sun.COM' */
/* The 'something' needs to be stripped and replaced with the hostname. */
/* Real domainnames do not need this. If you do things the proper way, comment */
/* out the line below. This only affects how ftptool determines your default */
/* password the first time you run it. */
/* #define FIX_DOMAIN */

/* The following are used to determine the max size of ftptoolrc */
/* fields that ftptool can read in */
#define	MAXLOGINLEN		64
#define	MAXPASSWORDLEN	1024
#define	MAXCOMMENTLEN	1024
#define	MAXALIASLEN		1024

/* The following are the default port numbers to use if ftptool cannot */
/* find the 'ftp,tcp' service or 'ftp-passthru, tcp service'. */
/* ftp is usually 21, ftp-passthru can vary */
#define	FTP_PORT	21
#define	FTP_PASSTHRU_PORT	4666

#ifndef	S_ISLNK
#define	S_ISLNK(mode)	((mode & S_IFMT) == S_IFLNK)
#endif

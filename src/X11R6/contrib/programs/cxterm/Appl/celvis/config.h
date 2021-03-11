/*
 * vi configuration file
 * We try to automatically configure to various compilers and operating
 * systems. Extend the autoconf section as needed.
 */

/*************************** autoconf section ************************/

/* standard unix V (?) */
#ifdef	M_SYSV
# define UNIXV		1
#endif

/* xelos system, University of Ulm */
#ifdef	xelos
# define UNIXV		1
#endif

/* Microsoft C: sorry, Watcom does the same thing */
#ifdef	M_I86
# ifndef M_SYSV
#  define MSDOS		1
#  define MICROSOFT	1
#  define COMPILED_BY	"Microsoft C 5.10"
# endif
#endif

/* Borlands Turbo C */
#ifdef	__TURBOC__
# define MSDOS		1
# define TURBOC		1
# define COMPILED_BY	"Turbo C 2.00"
#endif

/* Tos Mark-Williams */
#ifdef	M68000
# define TOS 1
# define COMPILED_BY	"Mark Williams C"
#endif

/* OS-9 requires an explicit "-DOS9" in CFLAGS */

/* TOS requires an explicit "-DTOS" in CFLAGS */

/*************************** end of autoconf section ************************/

/* All undefined symbols are defined to zero here, to allow for older    */
/* compilers which dont understand #if defined() or #if UNDEFINED_SYMBOL */

/*************************** operating systems *****************************/
 
#ifndef	BSD
# define BSD	0		/* UNIX - Berkeley 4.x */
#endif

#ifndef	UNIXV
# define UNIXV	0		/* UNIX - AT&T SYSV */
#endif

#ifndef	UNIX7
# define UNIX7	0		/* UNIX - version 7 */
#endif

#ifndef	MSDOS
# define MSDOS	0		/* PC		*/
#endif

#ifndef	TOS
# define TOS	0		/* Atari ST	*/
#endif

#ifndef	AMIGA
# define AMIGA	0		/* Commodore Amiga */
#endif

#ifndef OS9
# define OS9	0		/* OS-9 / 68k */
#endif

				/* Minix has no predefines */
#if !BSD && !UNIXV && !UNIX7 && !MSDOS && !TOS && !AMIGA && !OS9
# define MINIX	1
#else
# define MINIX	0
#endif

				/* generic combination of Unices */
#if UNIXV || UNIX7 || BSD || MINIX
# define ANY_UNIX 1
#else
# define ANY_UNIX 0
#endif

/*************************** compilers **************************************/
 
#ifndef	MICROSOFT
# define MICROSOFT	0
#endif

#ifndef	TURBOC
# define TURBOC		0
#endif

/******************************* Credit ************************************/

#if MSDOS
# define CREDIT "Ported to MS-DOS by Guntram Blohm & Martin Patzel"
#endif

#if TOS
# define CREDIT "Ported to Atari/TOS by Guntram Blohm & Martin Patzel"
#endif

/*************************** functions depending on OS *********************/

/* Only MSDOS and TOS need a special function for reading from the keyboard.
 * All others just read from file descriptor 0.
 */
#if !MSDOS && !TOS
# define ttyread(buf, len)	read(0, buf, len)	/* raw read */
#endif
#if !TOS
# define ttywrite(buf, len)	write(1, buf, len)	/* raw write */
#endif

/* The strchr() function is an official standard now, so everybody has it
 * except Unix version 7 (which is old) and BSD Unix (which is academic).
 * Those guys use something called index() to do the same thing.
 */
#if BSD || UNIX7 || MINIX
# define strchr	index
#endif

/* BSD uses bcopy() instead of memcpy() */
#if BSD
#define memcpy(dest, src, siz)	bcopy(src, dest, siz)
#endif

/* text versa binary mode for read/write */
#if !TOS
#define	tread	read
#define twrite  write
#endif

/**************************** Compiler quirks *********************************/

/* The Microsoft Xenix cross compiler for DOS doesn't support "volatile" */
#if MICROSOFT
# define volatile
#endif

/* the UNIX version 7, OS-9, and (some) TOS compilers, don't allow "void" */
#if UNIX7 || TOS || OS9
# define void int
#endif

/* In OS9, argv[0] never changes */
#if OS9
# define NO_ARGV0
#endif

/* as far as I know, all compilers except version 7 support unsigned char */
/* NEWFLASH: the Minix-ST compiler has subtle problems with unsigned char */
#if UNIX7 || MINIX
# define UCHAR(c)	((c) & 0xff)
#else
# define UCHAR(c)	((unsigned char)(c))
#endif

/* Some compilers prefer to have malloc declared as returning a (void *) */
#if BSD
extern void *malloc();
#else
extern char *malloc();
#endif

/******************* Names of files and environment vars **********************/

#if ANY_UNIX
# define TMPDIR		"/usr/tmp"	/* directory where temp files live */
# define TMPNAME	"%s/elvt%04x%03x" /* temp file */
# define CUTNAME	"%s/elvc%04x%03x" /* cut buffer's temp file */
# define EXRC		".exrc"		/* init file in current directory */
# define KEYWORDPRG	"/usr/bin/ref"	/* keyword "help" program */
# define SCRATCHOUT	"%s/soXXXXXX"	/* temp file used as input to filter */
# define SLASH		'/'
# define EXINIT		"EXINIT"
#endif

#if MSDOS || TOS
/* do not change TMPNAME, CUTNAME and SCRATCH*: they MUST begin with '%s\\'! */
# define TMPDIR		"C:\\tmp"	/* directory where temp files live */
# define TMPNAME	"%s\\elvt%04x.%03x" /* temp file */
# define CUTNAME	"%s\\elvc%04x.%03x" /* cut buffer's temp file */
# define EXRC		"elvis.rc"	/* init file in current directory */
# if TOS
# define KEYWORDPRG	"ref.ttp"	/* keyword "help" program */
# else
# define KEYWORDPRG	"ref.exe"	/* keyword "help" program */
# endif
# define SCRATCHIN	"%s\\siXXXXXX"	/* DOS ONLY - output of filter program */
# define SCRATCHOUT	"%s\\soXXXXXX"	/* temp file used as input to filter */
# define SLASH		'\\'
# define EXINIT		"EXINIT"        /* MSDOS also, kjh@usc.edu, 2 Sept 90 */

#endif
#define BUGFIXES "Some bugs fixed by Kenneth J. Hendrickson"

#if OS9
# define TMPDIR		"/dd/tmp"	/* directory where temp files live */
# define TMPNAME	"%s/elvt%04x%03x" /* temp file */
# define CUTNAME	"%s/elvc%04x%03x" /* cut buffer's temp file */
# define EXRC		".exrc"		/* init file in current directory */
# define KEYWORDPRG	"/dd/cmds/ref"	/* keyword "help" program */
# define SCRATCHOUT	"%s/soXXXXXX"	/* temp file used as input to filter */
# define SLASH		'/'
# define EXINIT		"EXINIT"
#endif

#ifndef	TAGS
# define TAGS		"tags"		/* tags file */
#endif

#ifndef TMPNAME
# define TMPNAME	"%s/elvt%04x.%03x"	/* temp file */
#endif

#ifndef CUTNAME
# define CUTNAME	"%s/elvc%04x.%03x"	/* cut buffer's temp file */
#endif

#ifndef	EXRC
# define EXRC		"elvis.rc"
#endif

#ifndef HMEXRC
# if !MSDOS && !TOS
#  define HMEXRC	EXRC
# endif
#endif

#ifndef	KEYWORDPRG
# define KEYWORDPRG	"ref"
#endif

#ifndef	SCRATCHOUT
# define SCRATCHIN	"%s/SIXXXXXX"
# define SCRATCHOUT	"%s/SOXXXXXX"
#endif

#ifndef	SLASH
# define SLASH		'/'
#endif

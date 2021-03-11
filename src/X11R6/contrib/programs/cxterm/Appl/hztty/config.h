/* $Id: config.h,v 1.6 1993/07/15 02:16:30 ygz Exp $ */

#include <sys/ioctl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/signal.h>
#include <fcntl.h>
#include <sys/wait.h>
#include <sys/time.h>
#include <pwd.h>
#include <stdio.h>
#include <errno.h>

#ifdef __STDC__
# include <stdlib.h>
#endif

#ifdef UNISTD
# include <unistd.h>
#endif

#ifdef	SVR4
# define USE_SYS_UTMP
# define USE_TERMIOS
# define ATT
# define SYSV
# ifdef sun			/* Solaris 2 */
#  include <sys/strredir.h>
#  define HAS_UTMPX
# endif
#endif

#ifdef	SYSV386
# define USE_SYS_UTMP
# define ATT
#endif

#ifdef	POSIX
# define USE_TERMIOS
#endif

#ifdef TERMIO
# define USE_SYSV_TERMIO
#endif

#ifdef USE_TERMIOS
# include <termios.h>
# define USE_SYSV_TERMIO
# ifndef _SEQUENT_
#  define termio termios
#  undef TCGETA
#  define TCGETA TCGETS
#  undef TCSETA
#  define TCSETA TCSETS
# endif /* _SEQUENT_ */
#else /* USE_TERMIOS */
# ifdef SYSV
#  define USE_SYSV_TERMIO
# endif 
# ifdef USE_SYSV_TERMIO
#  include <sys/termio.h>
# else
#  include <sgtty.h>
# endif
#endif /* USE_TERMIOS */

#ifdef	SVR4
# undef TIOCSLTC			/* defined, but not useable */
#endif

#ifdef	ATT
# define USE_PTYS
#endif

#ifndef	SYSV                            /* BSD systems */
# include <sys/resource.h>
#endif  /* !SYSV */

#ifdef	SYSV
# define bcopy(a,b,c)		memcpy(b,a,c)
# define bzero(poi,len)		memset(poi,0,len)
# define killpg(pgrp,sig)	kill(-(pgrp),sig)
#endif

#ifdef	hpux
# include <sys/ptyio.h>
# include <sys/bsdtty.h>
#endif

#ifdef  sgi
# include <sys/sysmacros.h>
#endif

#ifdef	macII
# include <sys/ttychars.h>
# define setpgrp2 setpgrp
# include <sgtty.h>
# include <sys/resource.h>
#endif

#if (! defined(SYSV)) || defined(sequent) || defined(pyr)
# include <strings.h>
#else /* BSD */
# include <string.h>
#endif /* BSD */

#if (defined(TIOCGWINSZ) || defined(TIOCSWINSZ)) && defined(M_UNIX)
# include <sys/stream.h>
# include <sys/ptem.h>
#endif

#ifdef	USE_PTYS
#include <sys/stropts.h>		/* for I_PUSH */
#endif


#ifndef SIG_RETURNS_INT
#define SIGNAL_T void
#define SIGNAL_RETURN return
#else
#define	SIGNAL_T int
#define SIGNAL_RETURN return 0
#endif

#ifdef	TIOCSWINSZ
#define		WINSIZE		winsize
#define		GETWINSZ	TIOCGWINSZ
#define		SETWINSZ	TIOCSWINSZ
#else   /* TIOCSWINSZ */
#if defined(sun) && !defined(SVR4)
#ifdef	    TIOCSSIZE
#define	 	WINSIZE		ttysize
#define		GETWINSZ	TIOCGSIZE
#define		SETWINSZ	TIOCSSIZE
#endif      /* TIOCSSIZE */
#endif    /* sun */
#endif  /* TIOCSWINSZ */

struct term_mode {
#ifdef	TIOCSLTC
		struct ltchars ltc;
#endif
#ifdef	TIOCLSET
		int lmode;
#endif
#ifdef	USE_SYSV_TERMIO
		struct termio tio;
#else	/* USE_SYSV_TERMIO */
		struct sgttyb sb;
		struct tchars tc;
		int ldis;
#endif	/* USE_SYSV_TERMIO */
};


/* $Id: tty.c,v 1.9 1993/08/04 05:12:41 ygz Exp $ */

/*
 * TTY.C ------ Routines for pseudo terminal allocation.
 *		Various platforms are supported.
 *
 * Copyright (C) 1992, 1993  Yongguang Zhang
 * All right reserved.		(ygz@cs.purdue.edu)
 */

#ifndef lint
static char *rcs_id="$Id: tty.c,v 1.9 1993/08/04 05:12:41 ygz Exp $";
#endif /* lint */

#include "config.h"

/* 
 * Functions get_pty() and pty_search() in this file are adopted from
 * X11R5 xterm, copyrighted by DEC & MIT.
 */ 

#ifdef hpux
# define	PTYCHAR1	"zyxwvutsrqp"
# define	PTYCHAR2	"fedcba9876543210"
# define	PTYDEV		"/dev/ptym/ptyxx"
# define	TTYDEV		"/dev/pty/ttyxx"
#else   /* !hpux */
# define	PTYCHAR1	"pqrstuvwxyzPQRSTUVWXYZ"
# define	PTYCHAR2	"0123456789abcdef"
# define	PTYDEV		"/dev/ptyxx"
# define	TTYDEV		"/dev/ttyxx"
#endif  /* !hpux */

#ifdef	SYSV
extern char *ptsname();
#endif

#ifdef hpux
# include <sys/utsname.h>
#endif

static char ptydev[32];
static char ttydev[32];
static int pty_search();

/* This function opens up a pty master and stuffs its value into pty.
 * If it finds one, it returns a value of 0.  If it does not find one,
 * it returns a value of !0.  This routine is designed to be re-entrant,
 * so that if a pty master is found and later, we find that the slave
 * has problems, we can re-enter this function and get another one.
 */

int get_pty (pty)
    int *pty;
{
	strcpy (ptydev, PTYDEV);
	strcpy (ttydev, TTYDEV);

#if defined(SYSV) && defined(SYSV386)
        if (pty_search(pty) == 0)
	    return 0;
#endif /* SYSV && SYSV386 */

#if (defined(att) || defined(ATT)) && (!defined(_SEQUENT_))
	{
	    if ((*pty = open ("/dev/ptmx", O_RDWR)) < 0)
		return 1;
# if defined(SVR4) || defined(SYSV386)
	    strcpy(ttydev, ptsname(*pty));
# endif
	    return 0;
	}
# define GET_PTY_DONE
#endif /* ATT */

#if defined(AIXV3) || defined(_IBMR2)
	{
	    if ((*pty = open ("/dev/ptc", O_RDWR)) < 0) 
		return 1;
	    strcpy(ttydev, (char *)ttyname(*pty));
	    return 0;
	}
# define GET_PTY_DONE
#endif

#if defined(sgi) && defined(IRIX4)
	{
	    char    *tty_name;

	    tty_name = _getpty (pty, O_RDWR, 0622, 0);
	    if (tty_name == 0)
		return 1;
	    strcpy (ttydev, tty_name);
	    return 0;
	}
# define GET_PTY_DONE
#endif

#ifdef __convex__
        {
	    char *pty_name, *getpty();

	    while ((pty_name = getpty()) != NULL) {
		if ((*pty = open (pty_name, O_RDWR)) >= 0) {
		    strcpy(ptydev, pty_name);
		    strcpy(ttydev, pty_name);
		    ttydev[5] = 't';
		    return 0;
		}
	    }
	    return 1;
	}
# define GET_PTY_DONE
#endif /* __convex__ */

#if defined(sequent) || defined(_SEQUENT_)
	{
	    char *sl[2], *ms[2];
	    *pty = getpseudotty (sl, ms);
	    strcpy(ptydev, ms[0]);
	    strcpy(ttydev, sl[0]);
	    return (*pty >= 0 ? 0 : 1);
	}
# define GET_PTY_DONE
#endif /* sequent */

#if (defined(sgi) && defined(IRIX3)) || (defined(umips) && defined (SYSTYPE_SYSV))
	{
	    struct stat fstat_buf;
	    int tty;

	    *pty = open ("/dev/ptc", O_RDWR);
	    if (*pty < 0 || (fstat (*pty, &fstat_buf)) < 0) {
	      return(1);
	    }
	    sprintf (ttydev, "/dev/ttyq%d", minor(fstat_buf.st_rdev));
# ifndef sgi
	    sprintf (ptydev, "/dev/ptyq%d", minor(fstat_buf.st_rdev));
	    if ((*tty = open (ttydev, O_RDWR)) < 0) {
	      close (*pty);
	      return(1);
	    }
# endif /* !sgi */
	    /* got one! */
	    return(0);
	}
# define GET_PTY_DONE
#endif /* sgi or umips */

#ifndef	GET_PTY_DONE
	return pty_search(pty);
#endif

}

/*
 * Called from get_pty to iterate over likely pseudo terminals
 * we might allocate.  Used on those systems that do not have
 * a functional interface for allocating a pty.
 * Returns 0 if found a pty, 1 if fails.
 */
static int pty_search(pty)
    int *pty;
{
    static int devindex, letter = 0;

#ifdef	O_NOCTTY
# define	O_RDWRPTY	(O_RDWR | O_NOCTTY)
#else
# define	O_RDWRPTY	(O_RDWR)
#endif

#ifdef CRAY
    for (; devindex < 256; devindex++) {
	sprintf (ttydev, "/dev/ttyp%03d", devindex);
	sprintf (ptydev, "/dev/pty/%03d", devindex);

	if ((*pty = open (ptydev, O_RDWRPTY)) >= 0) {
	    /* We need to set things up for our next entry
	     * into this function!
	     */
	    (void) devindex++;
# ifdef	TIOCEXCL
	    ioctl (*pty, TIOCEXCL, (char *)0);
# endif
	    return 0;
	}
    }
#else /* CRAY */
    while (PTYCHAR1[letter]) {
	ttydev [strlen(ttydev) - 2] = PTYCHAR1 [letter];
	ptydev [strlen(ptydev) - 2] = PTYCHAR1 [letter];

	while (PTYCHAR2[devindex]) {
	    ttydev [strlen(ttydev) - 1] = PTYCHAR2 [devindex];
	    ptydev [strlen(ptydev) - 1] = PTYCHAR2 [devindex];

	    if ((*pty = open (ptydev, O_RDWRPTY)) >= 0) {
		/* We need to set things up for our next entry
		 * into this function!
		 */
# ifdef sun
		int dummy;

		ptydev[5] = 't';
		chown(ptydev, 0, 0);
		chmod(ptydev, 0600);
		ptydev[5] = 'p';
		if ((ioctl(*pty, TIOCGPGRP, &dummy) == -1) && errno == EIO) {
		    (void) devindex++;
		    return 0;
		}
		close(*pty);
# else
		(void) devindex++;
#  ifdef TIOCEXCL
		ioctl (*pty, TIOCEXCL, (char *)0);
#  endif
		return 0;
# endif
	    }
	    devindex++;
	}
	devindex = 0;
	letter++;
    }
#endif /* CRAY else */
    /*
     * We were unable to allocate a pty master!  Return an error
     * condition and let our caller terminate cleanly.
     */
    return 1;
}

/* given master fildes, get the slave side tty */
int get_tty (master, tty)
    int master;
    int *tty;
{
  int t;
#if defined(POSIX) || defined(SVR4) || defined(__convex__)
  int pgrp = setsid();
#else
  int pgrp = getpid();
#endif

#ifdef	USE_PTYS

	setpgrp();
	grantpt (master);
	unlockpt (master);
	if ((*tty = open (ptsname(master), O_RDWR)) < 0)
		return 1;	/* error */

	if (ioctl (*tty, I_PUSH, "ptem") < 0)
		return 1;	/* error */
# if !defined(SVR4) && !defined(SYSV386)
	if (!getenv("CONSEM") && ioctl (*tty, I_PUSH, "consem") < 0) {
		return 1;
# endif
	if (ioctl (*tty, I_PUSH, "ldterm") < 0)
		return 1;
# ifdef SVR4
	if (ioctl (*tty, I_PUSH, "ttcompat") < 0)
		return 1;
# endif

#else /* ! USE_PTYS */

# ifdef	TIOCNOTTY
	t = open ("/dev/tty", O_RDWR);
	if (t >= 0) {
		(void) ioctl(t, TIOCNOTTY, (char *)0);
		(void) close(t);
	}
# endif

	*tty = open(ttydev, O_RDWR);
	if (*tty < 0) {
		perror(ttydev);
		return 1;
	}

# ifdef SYSV
	setpgrp();
# else
#  ifdef TIOCSCTTY
	(void) setsid();
	(void) ioctl (*tty, TIOCSCTTY, 0);
#  endif
#  ifdef TIOCSPGRP
	(void) ioctl (*tty, TIOCSPGRP, (char *)&pgrp);
#  endif
	setpgrp (0, 0);
	close (open (ttydev, O_WRONLY, 0));
	setpgrp (0, pgrp);
# endif /* SYSV */

#endif /* USE_PTYS */

	return 0;	/* OK */
}


/**************************** TTY MODE stuffs ****************************/

void get_term_mode(tty, termptr)
     int tty;
     struct term_mode *termptr;
{
#ifdef	TIOCSLTC
	(void) ioctl (tty, TIOCGLTC, (char *)&(termptr->ltc));
#endif
#ifdef	  TIOCLSET
	(void) ioctl (tty, TIOCLGET, (char *)&(termptr->lmode));
#endif
#ifdef	USE_SYSV_TERMIO
	(void) ioctl (tty, TCGETA,   (char *)&(termptr->tio));
#else	/* USE_SYSV_TERMIO */
	(void) ioctl (tty, TIOCGETP, (char *)&(termptr->sb));
	(void) ioctl (tty, TIOCGETC, (char *)&(termptr->tc));
	(void) ioctl (tty, TIOCGETD, (char *)&(termptr->ldis));
#endif	/* USE_SYSV_TERMIO */
}

void set_term_mode(tty, termptr)
     int tty;
     struct term_mode *termptr;
{
#ifdef	TIOCSLTC
	(void) ioctl (tty, TIOCSLTC, (char *)&(termptr->ltc));
#endif
#ifdef	  TIOCLSET
	(void) ioctl (tty, TIOCLSET, (char *)&(termptr->lmode));
#endif
#ifdef	USE_SYSV_TERMIO
# ifdef	POSIX
	(void) tcsetattr (fd, TCSAFLUSH, (char *)&(termptr->tio));
# else
	(void) ioctl (tty, TCSETA,   (char *)&(termptr->tio));
# endif
#else	/* USE_SYSV_TERMIO */
	(void) ioctl (tty, TIOCSETP, (char *)&(termptr->sb));
	(void) ioctl (tty, TIOCSETC, (char *)&(termptr->tc));
	(void) ioctl (tty, TIOCSETD, (char *)&(termptr->ldis));
#endif	/* USE_SYSV_TERMIO */
}

void make_raw(oldtermptr, newtermptr)
     struct term_mode *oldtermptr, *newtermptr;
{
	*newtermptr = *oldtermptr;

#ifdef	TIOCSLTC
	newtermptr->ltc.t_suspc = 0377;
	newtermptr->ltc.t_dsuspc = 0377;
	newtermptr->ltc.t_flushc = 0377;
	newtermptr->ltc.t_lnextc = 0377;
#endif
#if  defined(TIOCLSET) && defined(LPASS8)
	newtermptr->lmode |= LPASS8;
#endif

#ifdef	USE_SYSV_TERMIO

	newtermptr->tio.c_iflag &= ~(ICRNL|IXON);
# ifdef	ISTRIP
	newtermptr->tio.c_iflag &= ~ISTRIP;
# endif
# ifdef ONLCR
	newtermptr->tio.c_oflag &= ~ONLCR;
# endif
	newtermptr->tio.c_cflag |= CS8;
	newtermptr->tio.c_cflag &= ~(PARENB);
	newtermptr->tio.c_lflag &= ~(ICANON|ISIG|ECHO);
	newtermptr->tio.c_cc[VMIN] = 1;
	newtermptr->tio.c_cc[VTIME] = 0;
	newtermptr->tio.c_cc[VINTR] = 0377;
	newtermptr->tio.c_cc[VQUIT] = 0377;
	newtermptr->tio.c_cc[VINTR] = 0377;
# ifdef VSTART
	newtermptr->tio.c_cc[VSTART] = 0377;
# endif
# ifdef VSTOP
	newtermptr->tio.c_cc[VSTOP] = 0377;
# endif
# ifdef VDISCARD
	newtermptr->tio.c_cc[VDISCARD] = 0377;
# endif
# ifdef VSUSP
 	newtermptr->tio.c_cc[VSUSP] = 0377;
# endif
# ifdef VDSUSP
	newtermptr->tio.c_cc[VDSUSP] = 0377;
# endif

#else	/* USE_SYSV_TERMIO */

	newtermptr->sb.sg_flags &= ~(CRMOD | ECHO);
	newtermptr->sb.sg_flags |= CBREAK;
	newtermptr->tc.t_quitc = 0377;
	newtermptr->tc.t_intrc = 0377;
	newtermptr->tc.t_startc = 0377;
	newtermptr->tc.t_stopc = 0377;

#endif	/* USE_SYSV_TERMIO */
}

/******************************* UTMP stuffs *******************************/

#include <utmp.h>
#ifdef	HAS_UTMPX

# include <utmpx.h>
# define USE_SYSV_UTMP

  /* utmpx is very similar to utmp */
# define utmp		utmpx
# define ut_time	ut_xtime
# define setutent	setutxent
# define endutent	endutxent
# define getutid	getutxid
# define pututline	pututxline

#endif

#ifdef USE_SYSV_UTMP

# ifndef SVR4			/* otherwise declared in utmp.h */
extern struct utmp *getutid();
extern void pututline();
extern void setutent();
extern void endutent();
# endif /* !SVR4 */

# ifndef SYSV386
extern struct passwd *getpwuid();
# endif

extern time_t time();

#else   /* not USE_SYSV_UTMP */

# define UTMP_FILENAME	"/etc/utmp"

#endif

#ifdef	CRAY
# define PTYCHLEN	4
#else
# define PTYCHLEN	2
#endif

/*
 * addutmp() and rmutmp() will run in different processes --
 * one in subsubprocess, and the other in subprocess.  There is no way to
 * pass values or the status to each other by means of global variables.
 */

/* addutmp() -- add a record to the utmp file */
void addutmp()
{
  struct utmp utmp;
  struct passwd *pw;
#ifndef	USE_SYSV_UTMP
  int utmpf;
  int tslot;
#endif

	pw = getpwuid(getuid());

#ifdef	USE_SYSV_UTMP

	(void) setutent ();

	/* set up entry to search for */
	(void) bzero ((char *)&utmp, sizeof(struct utmp));
	utmp.ut_type = DEAD_PROCESS;
	(void) strncpy ((char *)utmp.ut_id, ttydev + strlen(ttydev) - PTYCHLEN,
			sizeof (utmp.ut_id));

	/* position to entry in utmp file */
	(void) getutid(&utmp);

	/* set up the new entry */
	(void) bzero ((char *)&utmp, sizeof(struct utmp));
	utmp.ut_type = USER_PROCESS;
# ifdef	HAS_UTMPX
	utmpx.ut_exit.e_exit = 0;
	utmpx.ut_exit.e_termination = 0;
# else
	utmp.ut_exit.e_exit = 2;
# endif
	(void) strncpy ((char *)utmp.ut_user,
			(pw && pw->pw_name) ? pw->pw_name : "???",
			sizeof(utmp.ut_user));
	(void) strncpy ((char *)utmp.ut_id, ttydev + strlen(ttydev) - PTYCHLEN,
			sizeof(utmp.ut_id));
	utmp.ut_pid = getpid();

# ifdef	HAS_UTMPX
	utmpx.ut_syslen = 1;
	utmpx.ut_session = getsid(0);
	utmpx.ut_tv.tv_usec = 0;
# endif

#else	/* USE_SYSV_UTMP */

# ifndef apollo
	tslot = ttyslot();
# else
	tslot = 1;
# endif

	utmpf = open (UTMP_FILENAME, O_WRONLY);
	if (utmpf < 0)
		return;
	bzero((char *)&utmp, sizeof(struct utmp));
#endif	/* USE_SYSV_UTMP */

	/* common utmp fields */
	(void) strncpy (utmp.ut_line, ttydev + strlen("/dev/"),
			sizeof (utmp.ut_line));
	(void) strncpy (utmp.ut_name,
			(pw && pw->pw_name) ? pw->pw_name : "???",
			sizeof(utmp.ut_name));
	utmp.ut_time = time ((time_t *) 0);

#ifdef	USE_SYSV_UTMP
	(void) pututline(&utmp);
	(void) endutent();
#else
	lseek (utmpf, (long)(tslot * sizeof(struct utmp)), 0);
	write (utmpf, (char *)&utmp, sizeof(struct utmp));
	close (utmpf);
#endif
}

void rmutmp(child_pid)
     int child_pid;
{
  struct utmp utmp;
#ifdef	USE_SYSV_UTMP
  struct utmp *utptr;
#else
  int utmpf;
  int tslot;
#endif

#ifdef	USE_SYSV_UTMP
	(void) setutent();

	utmp.ut_type = USER_PROCESS;
	(void) bzero (utmp.ut_id, sizeof(utmp.ut_id));
	(void) strncpy ((char *)utmp.ut_id, ttydev + strlen(ttydev) - PTYCHLEN,
			sizeof(utmp.ut_id));
	utptr = getutid(&utmp);

	/* write it out only if it exists, and the pid's match */
	if (utptr && (utptr->ut_pid == child_pid)) {
		utptr->ut_type = DEAD_PROCESS;
		utptr->ut_time = time ((time_t *) 0);
		(void) pututline(utptr);
	}
	(void) endutent();

#else	/* USE_SYSV_UTMP */

	utmpf = open (UTMP_FILENAME, O_RDWR);
	if (utmpf < 0)
		return;
	tslot = 0;
	while (read(utmpf, (char *)&utmp, sizeof (utmp)) == sizeof (utmp)) {
		if (strncmp (utmp.ut_line, ttydev + strlen("/dev/"),
				sizeof (utmp.ut_line)) != 0) {
			tslot ++;
			continue;
		}

		/* find the right slot */
		utmp.ut_name[0] = '\0';
		utmp.ut_time = time ((time_t *) 0);
		lseek (utmpf, (long)(tslot * sizeof(struct utmp)), 0);
		write (utmpf, (char *)&utmp, sizeof(struct utmp));
		close (utmpf);
		break;
	}

#endif	/* USE_SYSV_UTMP */
	return;
}

/*
 *	$Id: hzimpath.c,v 3.0 1994/06/04 07:50:36 ygz Exp $
 */

/***********************************************************
Copyright 1991,1994 by Yongguang Zhang.  All Rights Reserved

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, 
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in 
supporting documentation, and that the names of the authors not
used in advertising or publicity pertaining to distribution of the
software without specific, written prior permission.

THE AUTHORS DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

******************************************************************/

#include <X11/Xos.h>
#include <stdio.h>
#include <ctype.h>

#if defined(att) || (defined(SYSV) && defined(SYSV386))
#define ATT
#endif

#ifdef SVR4
#define SYSV
#define ATT
#endif

#ifdef ATT
#define USE_USG_PTYS
#endif

#ifdef APOLLO_SR9
#define CANT_OPEN_DEV_TTY
#endif

#ifdef macII
#define USE_SYSV_TERMIO
#undef SYSV				/* pretend to be bsd */
#endif /* macII */

#ifdef SYSV
#define USE_SYSV_TERMIO
#endif /* SYSV */

#include <sys/ioctl.h>
#ifdef USE_SYSV_TERMIO
#include <sys/termio.h>
#else /* else not USE_SYSV_TERMIO */
#include <sgtty.h>
#endif	/* USE_SYSV_TERMIO */

#ifdef USE_USG_PTYS
#include <sys/stream.h>
#include <sys/ptem.h>
#endif

#include <signal.h>

#ifdef SIGNALRETURNSINT
#define SIGNAL_T int
#else
#define SIGNAL_T void
#endif

#ifndef X_NOT_STDC_ENV
#include <stdlib.h>
#else
char *getenv();
#endif

#ifdef USE_SYSV_TERMIO
struct termio tioorig;
#else /* not USE_SYSV_TERMIO */
struct sgttyb sgorig;
#endif /* USE_SYSV_TERMIO */
int tty;
FILE *ttyfp;

SIGNAL_T onintr();

char *myname;
char *setdirs = "\033]160;%s\007";	/* esc seq for set hzinputdir */
char *setim = "\033]161;%s\007";	/* esc seq for set hz input method */

/*
   set input method 
 */
main (argc, argv)
    int argc;
    char **argv;
{
	register char *ptr;
	int setd = 0;		/* by default, set input method */
#ifdef USE_SYSV_TERMIO
	struct termio tio;
#else /* not USE_SYSV_TERMIO */
	struct sgttyb sg;
#endif /* USE_SYSV_TERMIO */
	char buf[BUFSIZ];
	char *name_of_tty;
#ifdef CANT_OPEN_DEV_TTY
	extern char *ttyname();
#endif

	if(ptr = strrchr(myname = argv[0], '/'))
		myname = ptr + 1;
	for(argv++, argc-- ; argc > 0 && **argv == '-' ; argv++, argc--) {
		switch((*argv)[1]) {
		  case 'd':	/* hzinputdir */
			if (argc < 2)
				Usage();	/* Never returns */
			setd = 1;
			break;
		  default:
			Usage();		/* Never returns */
		}
	}
	if (argc != 1)
		Usage();


#ifdef CANT_OPEN_DEV_TTY
	if ((name_of_tty = ttyname(fileno(stderr))) == NULL) 
#endif
	  name_of_tty = "/dev/tty";

	if ((ttyfp = fopen (name_of_tty, "w")) == NULL) {
	    fprintf (stderr, "%s:  can't open terminal %s\n",
		     myname, name_of_tty);
	    exit (1);
	}
	tty = fileno(ttyfp);

#ifdef USE_SYSV_TERMIO
	ioctl (tty, TCGETA, &tioorig);
	tio = tioorig;
	tio.c_iflag &= ~(ICRNL | IUCLC);
	tio.c_lflag &= ~(ICANON | ECHO);
	tio.c_cflag |= CS8;
	tio.c_cc[VMIN] = 6;
	tio.c_cc[VTIME] = 1;
#else	/* else not USE_SYSV_TERMIO */
 	ioctl (tty, TIOCGETP, &sgorig);
	sg = sgorig;
	sg.sg_flags |= RAW;
	sg.sg_flags &= ~ECHO;
#endif	/* USE_SYSV_TERMIO */
	signal(SIGINT, onintr);
	signal(SIGQUIT, onintr);
	signal(SIGTERM, onintr);
#ifdef USE_SYSV_TERMIO
	ioctl (tty, TCSETAW, &tio);
#else	/* not USE_SYSV_TERMIO */
	ioctl (tty, TIOCSETP, &sg);
#endif	/* USE_SYSV_TERMIO */

	sprintf (buf, (setd ? setdirs : setim), *argv);
	write(tty, buf, strlen(buf));

#ifdef USE_SYSV_TERMIO
	ioctl (tty, TCSETAW, &tioorig);
#else	/* not USE_SYSV_TERMIO */
	ioctl (tty, TIOCSETP, &sgorig);
#endif	/* USE_SYSV_TERMIO */
	signal(SIGINT, SIG_DFL);
	signal(SIGQUIT, SIG_DFL);
	signal(SIGTERM, SIG_DFL);

	exit(0);
}

Usage()
{
	fprintf(stderr, "Usage: %s [-d hzinputdir | hzinputmethod]\n", myname);
	exit(1);
}

/* ARGSUSED */
SIGNAL_T
onintr(sig)
    int sig;
{
#ifdef USE_SYSV_TERMIO
	ioctl (tty, TCSETAW, &tioorig);
#else	/* not USE_SYSV_TERMIO */
	ioctl (tty, TIOCSETP, &sgorig);
#endif	/* USE_SYSV_TERMIO */
	exit(1);
}

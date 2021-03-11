/* $Id: main.c,v 1.12 1993/08/04 05:12:14 ygz Exp $ */

/*
 * hztty -- version 1.1
 *
 * This program creates a pseudo terminal on top of cxterm that
 * facilitates you to read/write Chinese characters in different
 * encoding or different representation, such as the zW/HZ standard.
 * It turns the current cxterm session into the "new encoding aware"
 * cxterm and creates a new shell session for you.  
 *
 * This program must be run on top of cxterm.  (It's possible to run
 * in a terminal that supports Chinese, like kermit on CCDOS, ET, KC,
 * or Chinese Windows, etc.) 
 *
 * The conversion is implemented in a configurable I/O stream style.
 * Conversion modules are specified in command line options "-O" (for
 * output) and "-I" (for input).  In each direction, conversion modules
 * can be piped one to one using connection character ':'.  For example,
 * specifying "hz2gb:gb2big" in output stream translate the cxterm screen
 * output from zW/HZ to GB, then from GB to Big5.  
 * 
 *	Yongguang ZHANG 		(ygz@cs.purdue.edu)
 *	Purdue University		August 04, 1993
 */

/*
 * Copyright 1992 by Yongguang Zhang
 */

#ifndef lint
static char *rcs_id="$Id: main.c,v 1.12 1993/08/04 05:12:14 ygz Exp $";
#endif /* lint */

#include "config.h"
#include "io.h"

extern int get_pty();
extern int get_tty();
extern void get_term_mode();
extern void set_term_mode();
extern void make_raw();
extern void addutmp();
extern void rmutmp();

static int loginsh = 0;
static int utmp = 1;			/* update utmp by default */
static char *term = "vt100";

int	master;
int	slave;
int	child;
int	subchild = 0;
int	in_raw = 0;
char	*i_stream = NULL;
char	*o_stream = NULL;
int	debug = 0;		/* the debug flag */

static char *shell[] = { "sh", "-i", (char *)0 };
static char **cmdargv;
static char *cmd;
static char *progname;

static struct term_mode defmode, rawmode;

static void usage();
static void getmaster();
static void getslave();
static void doinput();
static void dooutput();
static void doshell();

#ifdef WINSIZE
static struct WINSIZE winsz;
static SIGNAL_T sigwinch ();
#endif
static SIGNAL_T finish ();
static void fail();
static void done();


main(argc, argv)
     int argc;
     char *argv[];
{
  extern int getopt();
  extern char *optarg;
  extern int optind;
  int ch;

	progname = *argv;
	while ((ch = getopt(argc, argv, "I:O:T:lud")) != EOF)
		switch(ch) {
		case 'I':
			i_stream = optarg;
			break;
		case 'O':
			o_stream = optarg;
			break;
		case 'T':
			term = optarg;
			break;
		case 'l':
			loginsh = 1;
			break;
		case 'u':
			utmp = 0;	/* disable utmp */
			break;
		case 'd':
			debug = 1;
			break;
		case '?':
		default:
			usage();
		}
	argc -= optind;
	argv += optind;

	if ((! i_stream) && (! o_stream)) {
		i_stream = DEF_ISTREAM;
		o_stream = DEF_OSTREAM;
	}

	if (argc == 0) {
		cmdargv = shell;
		cmd = (char *)getenv("SHELL");
		if (! cmd)  cmd = "/bin/sh";
	} else {
		cmdargv = argv;
		cmd = *argv;
	}
	if (loginsh)
		*cmdargv = "-";

	getmaster();

	get_term_mode(0, &defmode);
	make_raw(&defmode, &rawmode);
	set_term_mode(0, &rawmode);
	in_raw = 1;

	(void) signal(SIGCHLD, finish);
	child = fork();
	if (child < 0) {
		perror("fork");
		fail();
	}
	if (child == 0) {
		(void) signal(SIGCHLD, finish);
		subchild = child = fork();
		if (child < 0) {
			perror("fork");
			fail();
		}
		if (child == 0) {
			doshell();
		}
#if defined(SIGWINCH) && defined(WINSIZE)
		(void) signal(SIGWINCH, sigwinch);
#endif
		dooutput();
	}
	doinput();
	exit (0);
}

static void usage()
{
	(void)fprintf(stderr,
	    "usage: %s [ -lu ] [-T term] [-I input_steams] [-O output_streams] [command ...]\n",
	    progname);
	exit (1);
}


/* stdin --> pty */
static void doinput()
{
	register int cc;
	char ibuf[BUFSIZ];

	if (in_stream_setup (i_stream) < 0)
		fail ();
	for (;;) {
		cc = stream_read(0, ibuf, BUFSIZ);
		if (cc < 0)
			break;
		if (cc > 0)
			(void) write(master, ibuf, cc);
	}
	done();
}

/* pty --> stdout */
static void dooutput()
{
	register int cc;
	char obuf[BUFSIZ];

	if (out_stream_setup (o_stream) < 0)
		fail ();
	(void) close(0);
	for (;;) {
		cc = read(master, obuf, sizeof (obuf));
		if (cc <= 0)
			break;
		(void) stream_write(1, obuf, cc);
	}
	done();
}

/* tty <-> shell */
static void doshell()
{
  char envbuf[20];

	getslave();
	(void) close(master);
	(void) dup2(slave, 0);
	(void) dup2(slave, 1);
	(void) dup2(slave, 2);
	(void) close(slave);
	if (utmp)
		addutmp ();
#if defined(sequent) || defined(__convex__)
	setenv ("TERM", term, 1);
#else
	sprintf (envbuf, "TERM=%s", term);
	putenv (envbuf);
#endif
	printf ("[%s started]\n", progname);
	sleep(1);

	/* now execute the shell command */
	execvp (cmd, cmdargv);

	perror (cmd);
	fail();
}

static SIGNAL_T
finish()
{
#if defined(SYSV) || defined(POSIX)
	int status;
#else
	union wait status;
#endif
	register int pid;
	register int die = 0;

#if defined(SYSV) || defined(POSIX)
	while ((pid = waitpid(-1, &status, WNOHANG|WUNTRACED)) > 0)
#else
	while ((pid = wait3(&status, WNOHANG|WUNTRACED, (struct rusage *)0))>0)
#endif
		if (pid == child)
			die = 1;

	fflush (stdout);
	if (die)
		done();

	SIGNAL_RETURN;
}

static void fail()
{
	if (child) {
		kill (child, SIGTERM);
		sleep (1);	/* wait for the child to die */
		kill (child, SIGKILL);
	}
	done();
}

static void done()
{
	if (subchild) {
		(void) close(master);
		if (utmp)
			rmutmp (subchild);
	} else {
		if (in_raw)
			set_term_mode (0, &defmode);
		printf ("\n[%s exited]\n", progname);
	}
	exit (0);
}

static void getmaster()
{
	if (get_pty (&master) != 0) {
		fprintf(stderr, "Out of pty's\n");
		fail();
	}
#ifdef	GETWINSZ
	(void) ioctl(0, GETWINSZ, &winsz);
#endif
}

static void getslave()
{
	if (get_tty (master, &slave) != 0) {
		fprintf(stderr, "Fail to open tty\n");
		fail();
	}
#ifdef	SETWINSZ
	(void) ioctl (slave, SETWINSZ, &winsz);
#endif
	set_term_mode(slave, &defmode);
}

#ifdef WINSIZE
static SIGNAL_T
sigwinch ()
{
  struct WINSIZE ws;

	if (ioctl(1, GETWINSZ, &ws) != 0)
		SIGNAL_RETURN;
	(void) ioctl(master, SETWINSZ, &ws);
#ifdef notdef	/* SIGWINCH */
	{ int pgrp;
		if (ioctl (master, TIOCGPGRP, (char *)&pgrp))
			killpg (pgrp, SIGWINCH);
	}
#endif
	SIGNAL_RETURN;
}
#endif


/* -*-Mode: C; -*- */
#ifndef _PROCESS_CLIX_INCLUDE
#define _PROCESS_CLIX_INCLUDE

/* proc_clix.i,v 1.2 1994/05/27 06:21:40 me Exp */

/*
 * This file is part of the Emu system.
 *
 * Copyright 1990 by PCS Computer Systeme, GmbH. Munich, West Germany.
 * 
 * Copyright 1994 by Jordan K. Hubbard and Michael W. Elbel
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject to
 * the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
 * OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL PCS, THE AUTHORS, OR THEIR HOUSEPETS BE LIABLE FOR
 * ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
 * TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
 * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. SO DON'T SUE US.
 * THANK YOU.
 */

/* Mandatory */
#ifndef _PROCESS_INCLUDED
#define _PROCESS_INCLUDED
#endif

/*
 * Various process manipulation routines specific to Intergraph CLIX systems.
 *
 * Author: Steven S. Crooks
 * Date: December 25th, 1991
 * Description: Here are all the CLIX specific routines for frobbing the
 *	 	state of of the main process.
 *
 */

#include <stdlib.h>
#include <errno.h>
#include <grp.h>
#include <sys/param.h>
#include <sys/wait.h>
#include <sys/time.h>
#include <sys/resource.h>

/* What our system wait() function expects */
#define WAIT_STATUS_TYPE	union wait

Import struct group *getgrnam();
Local TermWidget clix_w;
Local void clix_exit();

Import char clix_ptyname[20];


/* Set various important environment variables */
Local void
clix_set_environment(w)
TermWidget w;
{
	char envstr[255];
	Widget canvas = CANVAS_W(w);
	Dimension lines = 24, columns = 80;

	/*
	 * Note: We duplicate (then save) all environment variables here before
	 * putenv()'ing them since putenv() treats values passed to it as
	 * forever after sacrosanct.
	 */

	/*
	 * TERM
	 */
	sprintf(envstr, "TERM=%s", w->term.term_type);
	if (putenv(XtNewString(envstr)))
		warn("putenv() failed in %s, line %d", __FILE__, __LINE__);
     
	/*
	 * Find out lines and columns of canvas so we can set environment.
	 */
	XtVaGetValues(canvas, XpNlines, &lines,
		      XpNcolumns, &columns, NULL);

	/*
	 * LINES
	 */
	sprintf(envstr, "LINES=%d", lines);
	if (putenv(XtNewString(envstr)))
		warn("putenv() failed in %s, line %d", __FILE__, __LINE__);

	/*
	 * COLUMNS
	 */
	sprintf(envstr, "COLUMNS=%d", columns);
	if (putenv(XtNewString(envstr)))
		warn("putenv() failed in %s, line %d", __FILE__, __LINE__);

	/*
	 * WINDOWID
	 */
	sprintf(envstr, "WINDOWID=%lu", (unsigned long)XtWindow(XtParent(w)));
	if (putenv(XtNewString(envstr)))
		warn("putenv() failed in %s, line %d", __FILE__, __LINE__);

	/*
	 * DISPLAY
	 */
	sprintf(envstr, "DISPLAY=%s",
		XDisplayString(XtDisplayOfObject((Widget)w)));
	if (putenv(XtNewString(envstr)))
		warn("putenv() failed in %s, line %d", __FILE__, __LINE__);
}


/*
 * These next three Exported routines, process_wait(), process_init() and
 * process_cleanup() are expected of us.  The rest is up to our personal
 * tastes in how we want to organize the work procedures.
 */

/*
 * Fire one process torpedo
 */
Local void
clix_fork_process(w)
TermWidget w;
{
#ifdef DEBUG
	debug("-- Entering clix_fork_process() --");
#endif
	/*
	 * We don't want the signal yet, since SYSV syscalls don't restart
	 */
	if (sighold(SIGCHLD) == -1)
		warn("sighold(SIGCHLD) failed in %s line %d: %s", __FILE__,
		     __LINE__, strerror(errno));

	switch(w->term.pid = fork()) {
	case -1:
		fatal("fork failed: %s", strerror(errno));

	case 0:
	{
		/*
		 * Now in child process.
		 */
		char *args[2], **argp;
		int fd, i;
		struct group *ttygrp;

		/*
		 * Set some signals to their defaults.
		 */
		if (sigset(SIGCHLD, SIG_DFL) == SIG_ERR)
			warn("sigset() failed in %s line %d: %s", __FILE__,
			     __LINE__, strerror(errno));
		if (signal(SIGINT, SIG_DFL) == SIG_ERR)
			warn("signal() failed in %s line %d: %s", __FILE__,
			     __LINE__, strerror(errno));
		if (signal(SIGQUIT, SIG_DFL) == SIG_ERR)
			warn("signal() failed in %s line %d: %s", __FILE__,
			     __LINE__, strerror(errno));
		if (signal(SIGTERM, SIG_DFL) == SIG_ERR)
			warn("signal() failed in %s line %d: %s", __FILE__,
			     __LINE__, strerror(errno));
		if (signal(SIGHUP, SIG_DFL) == SIG_ERR)
			warn("signal() failed in %s line %d: %s", __FILE__,
			     __LINE__, strerror(errno));
			
		/*
		 * First time for luck.
		 */
		if (setpgrp() == -1)
			warn("setpgrp() failed in %s line %d: %s", __FILE__,
			     __LINE__, strerror(errno));

		/*
		 * Close everything in sight.
		 */
		for (i = 0; i < NOFILES_MAX; i++)
			(void)close(i);

		/*
		 * Make the pty be the controlling terminal of the process.
		 */
		if (setpgrp() == -1)
			warn("setpgrp() failed in %s line %d: %s", __FILE__,
			     __LINE__, strerror(errno));

		/*
		 * Open the slave side for stdin.
		 */
		if ((w->term.slave = open(w->term.tname, O_RDWR, 0)) == -1)
			fatal("open of slave side failed: %s",
			      strerror(errno));

		fd = w->term.slave;

		/*
		 * Make sure we're the process group leader.
		 */
		if ((i = open("/dev/tty", O_RDWR, 0)) == -1)
			warn("not process group leader: %s", strerror(errno));
		else
			(void)close(i);

#ifdef DEBUG
		debug("Opened slave side %s on fd %d", w->term.tname,
		      w->term.slave);
#endif

		/*
		 * Set up stdin, stdout, stderr.
		 */
		for (i = 0; i <= 2; i++)
			if (i != fd) {
				(void)close(i);
				if (dup(fd) == -1)
					warn("couldn't dup slave fd: %s",
					     strerror(errno));
#ifdef DEBUG
				else
					debug("clix_fork_process: dup'ed fd %d to fd %d",
					      fd, i);
#endif			
			}

		/*
		 * And then close the tty if needed.
		 */
		if (fd > 2)
			if (close(fd) == -1)
				warn("couldn't close slave fd: %s",
				     strerror(errno));

		/*
		 * It's MINE, I tell you! Mine! Mine! Mine!
		 */
		if ((ttygrp = getgrnam("tty")) != NULL) {
			chown(w->term.tname, w->term.uid, ttygrp->gr_gid);
			chmod(w->term.tname, 0620);
		} else {
			chown(w->term.tname, w->term.uid, w->term.gid);
			chmod(w->term.tname, 0622);
		}

		/*
		 * Now is the time to add the utmp entry.
		 */
		tty_add_utmp(w);

		/*
		 * Since we may be setuid root (for utmp), be safe.
		 */
		if (setuid(w->term.uid) == -1)
			warn("setuid() failed in %s line %d: %s", __FILE__,
			     __LINE__, strerror(errno));
		if (setgid(w->term.gid) == -1)
			warn("getgid() failed in %s line %d: %s", __FILE__,
			     __LINE__, strerror(errno));

#ifdef DEBUG
		debug("child: opened %s for stdin stdout stderr",
		      w->term.tname);
#endif
		args[1] = NULL;
		argp = w->term.command_args ? w->term.command_args : args;

		if (w->term.login_shell) {
			char *lname, *sname;
			if (sname = rindex(w->term.command, '/'))
				sname++;
			else
				sname = w->term.command;
			if ((lname = (char *)alloca(strlen(sname) + 2)) ==
			    NULL)
				fatal("alloca failed: %s", strerror(errno));
			(void)strcpy(lname, "-");
			(void)strcat(lname, sname);
			argp[0] = lname;
		} else	
			argp[0] = w->term.command;
#ifdef DEBUG
		debug("child: about to exec %s as %s.", w->term.command,
		      argp[0]);
#endif
		execvp(w->term.command, argp);

		/*
		 * This will show up in the client window (hopefully), so
		 * sleep for some time before exiting to let the user read
		 * the message
		 */
		pmessage("Couldn't exec %s: %s", args[0], strerror(errno));
		sleep(5);
		exit(PROC_EXEC_FAILED);
	}

	default:
#ifdef DEBUG
		debug("parent:  term.pid is %d", w->term.pid);
#endif
		/*
		 * In the parent it's now safe to get this...
		 */
		if (sigrelse(SIGCHLD) == -1)
			warn("sigrelse() failed in %s line %d: %s", __FILE__,
			     __LINE__, strerror(errno));

		/*
		 * Set various signals to non annoying (for SYSV) values.
		 */
		if (sigset(SIGHUP, SIG_IGN) == SIG_ERR)
			warn("sigset() failed in %s line %d: %s", __FILE__,
			     __LINE__, strerror(errno));
		if (getpgrp() == getpid()) {
			if (signal(SIGINT, clix_exit) == SIG_ERR)
				warn("signal() failed in %s line %d: %s",
				     __FILE__, __LINE__, strerror(errno));
			if (signal(SIGQUIT, clix_exit) == SIG_ERR)
				warn("signal() failed in %s line %d: %s",
				     __FILE__, __LINE__, strerror(errno));
			if (signal(SIGTERM, clix_exit) == SIG_ERR)
				warn("signal() failed in %s line %d: %s",
				     __FILE__, __LINE__, strerror(errno));
		} else {
			if (signal(SIGINT, SIG_IGN) == SIG_ERR)
				warn("signal() failed in %s line %d: %s",
				     __FILE__, __LINE__, strerror(errno));
			if (signal(SIGQUIT, SIG_IGN) == SIG_ERR)
				warn("signal() failed in %s line %d: %s",
				     __FILE__, __LINE__, strerror(errno));
			if (signal(SIGTERM, SIG_IGN) == SIG_ERR)
				warn("signal() failed in %s line %d: %s",
				     __FILE__, __LINE__, strerror(errno));
		}

		/*
		 * Set up some stuff for clix_exit.
		 */
		clix_w = w;
	}
}


Export void
process_init(w)
TermWidget w;
{
#ifdef DEBUG
	debug("-- Entering process_init() --");
#endif
	/*
	 * Record uid and gid values for later use.
	 */
	w->term.uid = getuid();
	w->term.gid = getgid();

	clix_set_environment(w);
	clix_fork_process(w);
	XpNrememberWidget(w);
}


Export int
process_wait(fc)
WAIT_STATUS_TYPE *fc;
{
	int pid;

#ifdef DEBUG
	debug("-- Entering process_wait() --");
#endif

	if ((pid = wait3((WAIT_STATUS_TYPE *)fc, WNOHANG,
			 (struct rusage *)NULL)) == -1)
		warn("wait3() failed in %s line %d: %s", __FILE__, __LINE__,
		     strerror(errno));
#ifdef DEBUG
	debug("process_wait: pid = %d", pid);
#endif

	return pid;
}


Export void
process_cleanup(w)
TermWidget w;
{
#ifdef DEBUG
	debug("-- Entering process_cleanup() --");
#endif
	/*
	 * Just remove the utmp entry
	 */
	tty_remove_utmp(w);

	/*
	 * Restore modes of tty and pty.
	 */
	if (chmod(w->term.tname, 0666) == -1)
		warn("chmod() failed in %s line %d: %s", __FILE__, __LINE__,
		     strerror(errno));
	if (chmod(clix_ptyname, 0666) == -1)
		warn("chmod() failed in %s line %d: %s", __FILE__, __LINE__,
		     strerror(errno));

	/*
	 * Restore ownership of tty and pty.
	 */
	if (chown(w->term.tname, 0, 0) == -1)
		warn("chown() failed in %s line %d: %s", __FILE__, __LINE__,
		     strerror(errno));
	if (chown(clix_ptyname, 0, 0) == -1)
		warn("chown() failed in %s line %d: %s", __FILE__, __LINE__,
		     strerror(errno));
}


/*ARGSUSED*/
Local void
clix_exit(sig)
int sig;
{
#ifdef DEBUG
	debug("-- Entering clix_exit() --");
#endif
	tty_close(clix_w);
	process_cleanup(clix_w);
	process_destroy(clix_w, 0);
	(void)exit(0);
}

#endif	/* _PROCESS_CLIX_INCLUDE */

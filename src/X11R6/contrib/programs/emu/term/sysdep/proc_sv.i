/* -*-Mode: C; -*- */

#ifndef _PROCESS_SYSV_INCLUDE
#define _PROCESS_SYSV_INCLUDE

/* proc_sv.i,v 1.2 1994/05/27 06:22:00 me Exp */

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

/*
 * Various process manipulation routines specific to V, The System.
 *
 * Author: Jordan K. Hubbard
 * Date: Nov 1st, 1990.
 * Description: Here are some System V specific routines for frobbing the
 *	 	state of of the main process.
 *
 * proc_sv.i,v
 * Revision 1.2  1994/05/27  06:22:00  me
 * New copyright message
 *
 * Revision 1.1.1.1  1994/05/22  11:22:46  me
 * Initial import into CVS
 *
 * Revision 1.6  93/10/25  14:06:57  me
 * Removed unnecessary signal(SIGHUP, SIG_IGN)
 * 
 * Revision 1.5  92/05/16  06:15:13  jkh
 * Reorganized basic driver structure somewhat
 * 
 * Revision 1.4  92/02/26  11:45:00  me
 * Steve Crooks' clix port and general code cleanup
 * 
 * Revision 1.2  1991/08/17  10:16:00  jkh
 * General reorganization, changed declarations for TAGS.
 */

/* Mandatory */
#ifndef _PROCESS_INCLUDED
#define _PROCESS_INCLUDED
#endif

/* SYSV was a UNIX variant last time we checked */
#include "sysdep/proc_unix.i"

/* Set various important environment variables */
Local void
sysv_set_environment(w)
TermWidget w;
{
     char envstr[255];
     Widget canvas = CANVAS_W(w);
     Arg args[8];
     int i;
     Dimension lines = 24, columns = 80;
     Import void putenv();

     /*
      * Note: We duplicate (then save) all environment variables here before
      * putenv()'ing them since putenv() treats values passed to it as forever
      * after sacrosanct.
      */
     /* TERM */
     sprintf(envstr, "TERM=%s", w->term.term_type);
     putenv(XtNewString(envstr));
     
     /* Find out lines and columns of canvas so we can set environment */
     i = 0;
     XtSetArg(args[i], XpNlines, &lines);		i++;
     XtSetArg(args[i], XpNcolumns, &columns);		i++;
     XtGetValues(canvas, args, i);
     
     /* LINES */
     sprintf(envstr, "LINES=%d", lines);
     putenv(XtNewString(envstr));

     /* COLUMNS */
     sprintf(envstr, "COLUMNS=%d", columns);
     putenv(XtNewString(envstr));
}

/* Fire one process torpedo */
Local void
sysv_fork_process(w)
TermWidget w;
{
     int i;
     Import void exit();
#ifndef hpux
     Import int ioctl(), setpgrp(), vfork(), open(), close(), dup();
     Import int chown(), chmod();
     Import void execve();
     Import int sighold(), sigrelse();
#endif 

     /* We don't want the signal yet, since SYSV syscalls don't restart */
     sighold(SIGCHLD);

     if (!(w->term.pid = fork())) {
	  /* Now in child process */
	  char *args[2], **argp;
	  int fd;

	  /* We're not particularly interested */
	  signal(SIGCHLD, SIG_DFL);

	  /* First time for luck */
	  setpgrp();

	  /* Close everything in sight */
	  for (i = 0; i < NFILES; i++)
	       close(i);

	  /* Open /dev/tty and disassociate ourselves from it */
#ifdef TIOCNOTTY
	  if ((fd = open("/dev/tty", O_RDWR, 0)) >= 0) {
	       ioctl(fd, TIOCNOTTY, 0);
	       close(fd);
	  }
#endif
          /* Make the pty be the controlling terminal of the process. */
	  setpgrp();

	  /*
	   * Open the slave side for stdin and dup twice for stdout and stderr.
	   * We need to open all fd's read/write since some weird applications
	   * (like "more") READ from stdout/stderr or WRITE to stdin!
	   * Ack, bletch, barf.
	   *
	   * This should also take care of the controlling TTY. I hope.
	   */
          fd = open(w->term.tname, O_RDWR, 0);		/* 0 */
          dup(fd);					/* 1 */
          dup(fd);					/* 2 */

	  /* Now open the tty for our new process group */
	  if ((fd = open("/dev/tty", O_RDWR, 0)) >= 0)
	       close(fd);

	  /* It's MINE, I tell you! Mine! Mine! Mine! */
	  chown(w->term.tname, w->term.uid, w->term.gid);
	  chmod(w->term.tname, 0622);

	  /* set various signals to non annoying (for SYSV) values */
	  if (getpgrp() == getpid()) {
	       signal(SIGINT, exit);
	       signal(SIGQUIT, exit);
	       signal(SIGTERM, exit);
	  }
	  else {
	       signal(SIGINT, SIG_IGN);
	       signal(SIGQUIT, SIG_IGN);
	       signal(SIGTERM, SIG_IGN);
	  }
	  signal(SIGPIPE, exit);

	  /* Now is the time to add the utmp entry */
	  tty_add_utmp(w);

	  /* since we may be setuid root (for utmp), be safe */
	  setuid(w->term.uid);
	  setgid(w->term.gid);

#ifdef DEBUG
          debug("child: opened %s for stdin stdout stderr", w->term.tname);
          debug("child: about to exec %s.", w->term.command);
#endif
	  args[1] = NULL;
	  argp = w->term.command_args ? w->term.command_args : args;
	  if (w->term.login_shell)
	       argp[0] = "-";
	  else	
	       argp[0] = w->term.command;
	  execvp(w->term.command, argp);
	  /*
	   * This will show up in the client window (hopefully), so
	   * sleep for some time before exiting to let the user read
	   * the message
	   */
	  pmessage("Couldn't exec %s", args[0]);
	  sleep(5);
	  exit(PROC_EXEC_FAILED);
     }
     /* In the parent it's now safe to get this.. */
     sigrelse(SIGCHLD);
}

Local void
sysv_process_init(w)
TermWidget w;
{
     sysv_set_environment(w);
     sysv_fork_process(w);
     XpNrememberWidget(w);
}

#endif	/* _PROCESS_SYSV_INCLUDE */

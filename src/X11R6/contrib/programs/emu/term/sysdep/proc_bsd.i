/* -*-Mode: C; -*- */

#ifndef _PROCESS_BSD_INCLUDE
#define _PROCESS_BSD_INCLUDE

/* proc_bsd.i,v 1.3 1994/05/29 10:40:49 me Exp */

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
 * Various process manipulation routines specific to BSD systems.
 *
 * Author: Jordan K. Hubbard
 * Date: March 20th, 1990.
 * Description: Here are some routines that generic BSD systems should be
 *	 	able to use.
 *
 *
 * proc_bsd.i,v
 * Revision 1.3  1994/05/29  10:40:49  me
 * bsd_fork_process now sets a couple of signals to default values. This
 * fixes the problem that e.g. SIGINT was sometimes not caught.
 *
 * Revision 1.2  1994/05/27  06:21:37  me
 * New copyright message
 *
 * Revision 1.1.1.1  1994/05/22  11:22:47  me
 * Initial import into CVS
 *
 * Revision 1.3  92/05/16  06:14:42  jkh
 * Reorganized basic driver structure somewhat
 * 
 * Revision 1.2  1991/10/01  18:27:46  jkh
 * Moved over some things from process_sun.i that were
 * BSD generic.
 */

/* Mandatory */
#ifndef _PROCESS_INCLUDED
#define _PROCESS_INCLUDED
#endif

/* BSD was a UNIX variant last time we checked */
#include "sysdep/proc_unix.i"

/* Needed for TIOCSCTTY */
#include "sys/ioctl.h"

/* Fire one process torpedo */
Local void
bsd_fork_process(w)
TermWidget w;
{
     int i;
     int omask;
     int fd;
     void exit(int);

     /* Make sure we don't get the signal just yet */
     omask = sigblock(sigmask(SIGCHLD));
     
     if (!(w->term.pid = fork())) {	/* now running in junior */
	  char *args[2], **argp;

	  /* Begin our long decent into madness */
      	  if (setsid() < 0)
               pmessage("failed to set process group");

	  /* close everything in sight */
	  for (i = 0; i < NFILES; i++)
	       close(i);

	  /*
	   * Open the slave side for stdin and dup twice for stdout and stderr.
	   * We need to open all fd's read/write since some weird applications
	   * (like "more") READ from stdout/stderr or WRITE to stdin!
	   * Ack, bletch, barf.
	   *
	   */
          fd = open(w->term.tname, O_RDWR);		/* 0 */
#ifdef TIOCSCTTY
	  if (ioctl(fd, TIOCSCTTY, (char *)NULL) == -1)
		exit(PROC_EXEC_FAILED);
#endif
          dup(fd);					/* 1 */
          dup(fd);					/* 2 */

	  signal(SIGCHLD, SIG_DFL);
	  
	  /* Time to release the weasels */
	  sigsetmask(omask);
	  
	  /* It's MINE, I tell you! Mine mine mine! */
	  fchown(fd, w->term.uid, w->term.gid);
	  fchmod(fd, 0622);

	  /* set various signals to non annoying values */
	  signal(SIGINT, exit);
	  signal(SIGQUIT, exit);
	  signal(SIGTERM, exit);
	  signal(SIGPIPE, exit);

	  /* since we may be setuid root (for utmp), be safe */
	  setuid(w->term.uid);
	  setgid(w->term.gid);

#ifdef DEBUG
          debug("child: opened %s for stdin stdout stderr", w->term.tname);
          debug("child: about to exec %s.", w->term.command);
#endif
	  args[1] = NULL;
	  argp = w->term.command_args ? w->term.command_args : args;
	  argp[0] = w->term.command;
	  execvp(w->term.command, argp);
	  /*
	   * This will show up in the client window (hopefully), so
	   * sleep for some time before exiting, to let the user read
	   * the message
	   */
	  pmessage("Couldn't exec %s", args[0]);
	  sleep(5);
	  exit(PROC_EXEC_FAILED);
     }
     /* Now is the time to add the utmp entry */
     tty_add_utmp(w);

     /*
      * Now that we have stored the widget ID, we can enable SIGCHLD
      * and deal with a possible early failure. 
      */
     sigsetmask(omask);
}

Local void
bsd_process_init(w)
TermWidget w;
{
     char envterm[2048];
     String res;

     Import uid_t getuid();
     Import gid_t getgid();

     /* record uid and gid values for later use */
     w->term.uid = getuid();
     w->term.gid = getgid();

     /* TERM */
     sprintf(envterm, "TERM=%s", w->term.term_type);
     putenv(envterm);

     /* TERMCAP */
     if (res = get_sub_resource_string((Widget)w, w->term.term_type,
				       "termcap", "Termcap")) {
	  strcpy(envterm, "TERMCAP=");
	  strcat(envterm, res);
	  putenv(envterm);
     }

     bsd_fork_process(w);
     XpNrememberWidget(w);
}

#ifdef WAIT_STATUS_TYPE
Local int
bsd_process_wait(fc)
WAIT_STATUS_TYPE *fc;
{
     int pid;
     
     if ((pid = wait3((WAIT_STATUS_TYPE *)fc, WNOHANG,
		      (struct rusage *)NULL)) == -1)
	  perror("wait");
     return pid;
}
#endif	/* WAIT_STATUS_TYPE */

#endif	/* _PROCESS_BSD_INCLUDE */

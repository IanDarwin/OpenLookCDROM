/* -*-Mode: C; -*- */

#ifndef _TTY_POSIX_INCLUDE
#define _TTY_POSIX_INCLUDE

/* tty_linux.i,v 1.2 1994/05/27 06:22:15 me Exp */

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
 * Tty service routines for linux systems
 *
 * Author: Michael Elbel
 * Date: Jan. 4th 1994
 *
 * tty_linux.i,v
 * Revision 1.2  1994/05/27  06:22:15  me
 * New copyright message
 *
 */

/* Mandatory */
#ifndef _TTY_INCLUDED
#define _TTY_INCLUDED
#endif

/*
 * These next three routines complement eachother by getting and setting
 * some number of tty values using the TtyStuff structure.
 */

#include <termios.h>
#include <utmp.h>
#include <pwd.h>
#include "tty_unix.i"

typedef struct termios TtyStuff;

/* File descriptors to try */
Local int Fd_try[] = { 0, 1, 2, -1, 0, -1 };

/* If w is NULL, get tty values for invoking tty, not pty */
Generic
tty_get_values(w)
TermWidget w;
{
     register int i, *try;
     static TtyStuff ret;
     Import int errno;

     if (w == NULL)
	  try = Fd_try;
     else {
	  try = Fd_try + 4;
	  try[0] = w->term.slave;
     }
     for (i = 0; try[i] >= 0; i++) {
	  int r;

	  r = tcgetattr(try[i], &ret);
	  if (!r)	/* success? */
	       break;
     }

     if (try[i] == -1)
	  warn("posix_tty_get_values: Couldn't tcgetattr, errno = %d", errno);
     return (Generic)&ret;
}

/* Must return some "sane" set of hardwired values */
Generic
tty_get_sane(w)
TermWidget w;
{
     static struct termios sane;
     static char cchars[] = {
	  VINTR, VQUIT, VERASE, VKILL, VEOF, VEOL, VSTART, VSTOP, VSUSP,
     };

     /*
      * load values individually rather than in static struct since
      * the order of the members *doesn't* seem to be something that
      * all posix systems agree on.
      */
     sane.c_iflag = IGNPAR|ICRNL|BRKINT|ISTRIP|IXON;
     sane.c_oflag = OPOST|ONLCR;
     sane.c_cflag = B9600|CS7|PARENB|CREAD;
     sane.c_lflag = ISIG|ICANON|ECHO|ECHOE|ECHOK|ECHONL;
     sane.c_line = 2;
     bcopy(cchars, sane.c_cc, sizeof(cchars));
     return (Generic)&sane;
}

/* Whap some saved values onto the tty */
void
tty_set_values(w, val)
TermWidget w;
Generic val;
{

     register int i, *try;
     Import int errno;

     if (w == NULL)
	  try = Fd_try;
     else {
	  try = Fd_try + 4;
	  try[0] = w->term.slave;
     }
     for (i = 0; try[i] >= 0; i++) {
	  int r;
	  r = tcsetattr(try[i], TCSADRAIN, (struct termios *)val);
	  if (!r)	/* success? */
	       break;
     }
     if (try[i] == -1)
	  warn("posix_tty_set_values: Couldn't tcsetattr, errno = %d.", errno);
}

/* Set a new tty size */
void
tty_set_size(w, rows, cols, width, height)
TermWidget w;
int rows, cols, width, height;
{
     struct winsize wz;
     int pgrp;

     wz.ws_row = rows;
     wz.ws_col = cols;
     wz.ws_xpixel = width;
     wz.ws_ypixel = height;
     ioctl(w->term.slave, TIOCSWINSZ, &wz);
     if (ioctl(w->term.slave, TIOCGPGRP, &pgrp) != -1)
	  killpg(pgrp, SIGWINCH);
}

/* Add a utmp/wtmp entry for a tty */
void
tty_add_utmp(w)
TermWidget w;
{
     struct passwd *pw;
     struct utmp utmp;
     char *name;

     /* Reset before searching */
     (void)setutent();
     bzero(&utmp, sizeof(struct utmp));

     /*
      * Copy the last two chars over (e.g. "ttyp0" -> "p0") for
      * /etc/inittab lookup.
      */
     (void)strncpy(utmp.ut_id, w->term.tname + (strlen(w->term.tname) - 2),
		   sizeof(utmp.ut_id));

     /* who's there? */
     pw = getpwuid(w->term.uid);

     /* we're somebody.. */
     utmp.ut_type = USER_PROCESS;

     /* or are we? */
     name = (pw && (int) pw->pw_name) ? pw->pw_name : "(unknown)";

     /* copy over name information */
     (void)strncpy(utmp.ut_user, name, sizeof(utmp.ut_user));
     (void)strncpy(utmp.ut_line, w->term.tname + 5, /* 5 is len of "/dev/" */
		   sizeof(utmp.ut_line));

     /* pid and current time */
     utmp.ut_pid = w->term.pid;
     utmp.ut_time = time((long *)0);

     /* Display connection */
     (void)strncpy(utmp.ut_host, DisplayString(XtDisplay((Widget)w)), sizeof(utmp.ut_host));

#ifdef DEBUG
     printf("utptr: type = %d, pid = %d, line = %s, id = %c%c, time = %d, user = %s, host = %s, addr = %d\n", utmp.ut_type, utmp.ut_pid, utmp.ut_line, utmp.ut_id[0], utmp.ut_id[1], utmp.ut_time, utmp.ut_user, utmp.ut_host, utmp.ut_addr);
#endif

     /* write it out */
     (void)pututline(&utmp);

     /* close up shop */
     (void)endutent();
}

/* Remove a utmp/wtmp entry for a tty */
void
tty_remove_utmp(w)
TermWidget w;
{
     struct utmp utmp, *utptr;
     bzero(&utmp, sizeof(struct utmp));

     /* time to clean up the old utmp entry we made */
     utmp.ut_type = USER_PROCESS;
     (void)strncpy(utmp.ut_id, w->term.tname + strlen(w->term.tname) - 2,
		   sizeof(utmp.ut_id));
     (void)strncpy(utmp.ut_line, w->term.tname + 5,
		   sizeof(utmp.ut_line));
#ifdef DEBUG
     debug("Trying to remove utmp entry for /dev/%s.", utmp.ut_line);
#endif
     (void)setutent();
     utptr = getutid(&utmp);
#ifdef DEBUG
     printf("utptr = 0x%xd\n", utptr);
     printf("utptr: type = %d, pid = %d, line = %s, id = %c%c, time = %d, user = %s, host = %s, addr = %d\n", utptr->ut_type, utptr->ut_pid, utptr->ut_line, utptr->ut_id[0], utptr->ut_id[1], utptr->ut_time, utptr->ut_user, utptr->ut_host, utptr->ut_addr);
#endif

     /* write it out only if it exists, and the pid's match */
     if (utptr && (utptr->ut_pid == w->term.pid)) {
	  
	  /* write obituary for the deceased */
	  utptr->ut_type = DEAD_PROCESS;
	  utptr->ut_time = time((long *) 0);
	  /* inform the grieving relatives */
	  (void)pututline(utptr);
     }
     /* close the casket and go home (jeeze this is morbid!) */
     (void)endutent();
}

Export String
tty_find_pty(w)
TermWidget w;
{
     /* general unix code should work */
     unix_tty_find_pty(w);
}

#endif	/* _TTY_POSIX_INCLUDE */

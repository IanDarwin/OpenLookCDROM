/* -*-Mode: C; -*- */

#ifndef _TTY_BSD_INCLUDE
#define _TTY_BSD_INCLUDE

/* tty_bsd.i,v 1.2 1994/05/27 06:22:05 me Exp */

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
 * Tty service routines for BSD systems.
 *
 * Author: Jordan K. Hubbard
 * Date: March 20th, 1990.
 * Description: Various routines for dealing with ttys that are generic to
 *		BSD systems (in theory).
 *
 * Revision History:
 *
 * tty_bsd.i,v
 * Revision 1.2  1994/05/27  06:22:05  me
 * New copyright message
 *
 * Revision 1.1.1.1  1994/05/22  11:22:46  me
 * Initial import into CVS
 *
 * Revision 1.4  92/05/16  06:15:13  jkh
 * Reorganized basic driver structure somewhat
 */

/* Mandatory */
#ifndef _TTY_INCLUDED
#define _TTY_INCLUDED
#endif

/* BSD is a variant of unix, yup */
#include "sysdep/tty_unix.i"

/* And also reasonably POSIX compliant these days */
#include "sysdep/tty_posix.i"

#include <utmp.h>

/*
 * If _PATH_UTMP isn't defined, try to guess it
 */
#ifndef _PATH_UTMP
# ifdef UTMP_FILE
#  define _PATH_UTMP UTMP_FILE
# else 
#  define _PATH_UTMP "/etc/utmp"
# endif 
#endif 

#include <pwd.h>
#include <sys/stat.h>

/* Sets no-blocking I/O on a file descriptor */
Local void
bsd_tty_set_nonblocking(TermWidget w, int fd)
{
     int mode = 1;

     if (ioctl(fd, FIONBIO, &mode) == -1)
	  perror("Couldn't FIONBIO pty");
}

/* Set a new tty size */
Local void
bsd_tty_set_size(TermWidget w, int rows, int cols, int width, int height)
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
Local void
bsd_tty_add_utmp(w)
TermWidget w;
{
     struct utmp utmp;
     struct passwd *pw;
     char *name;
     int fd;
     int tty;

     /* who's there? */
     pw = getpwuid(w->term.uid);

     /* we're somebody.. */
     /* or are we? */
     name = (pw && (int) pw->pw_name) ? pw->pw_name : "(unknown)";

     /* copy over name information */
     (void)strncpy(utmp.ut_name, name, sizeof(utmp.ut_name));
     (void)strncpy(utmp.ut_line, w->term.tname + 5, /* 5 is len of "/dev/" */
		   sizeof(utmp.ut_line));

     /* current time */
     (void)time(&utmp.ut_time);

     /* Display connection */
     (void)strncpy(utmp.ut_host, DisplayString(XtDisplay((Widget)w)), 
		   sizeof(utmp.ut_host));

     /* 
      * The rest of the function is shamelessly stolen from 
      * FreeBSD's /usr/src/libutil/login.c
      * I hope it's reasonably portable to other bsd derived systems.
      */

     if ((fd = open(_PATH_UTMP, O_RDWR|O_CREAT, 0644)) >= 0) {
	  if ((tty = ttyslot()) > 0) {
	       (void)lseek(fd, (long)(tty * sizeof(struct utmp)), L_SET);
	       (void)write(fd, (char *)&utmp, sizeof(struct utmp));
	       (void)close(fd);
	  } else {
	       setttyent();
	       for (tty = 0; getttyent(); tty++)
		    ;
	       endttyent();
	       (void)lseek(fd, (long)(tty * sizeof(struct utmp)), L_SET);
	       while (read(fd, (char *)&utmp,
			   sizeof(struct utmp)) == sizeof(struct utmp)) {
		    if (!utmp.ut_name[0]) {
			 (void)lseek(fd, -(long)sizeof(struct utmp), L_INCR);
			 break;
		    }
	       }
	       (void)write(fd, (char *)&utmp, sizeof(struct utmp));
	       (void)close(fd);
	  }
     }
#if 0
     /*
      * I'm not sure, wtmp handling us such a good idea, if you don't want
      * to clog your wtmp with loads of entries
      */
     if ((fd = open(_PATH_WTMP, O_WRONLY|O_APPEND, 0)) >= 0) {
	  (void)write(fd, (char *)&utmp, sizeof(struct utmp));
	  (void)close(fd);
     }
#endif
}

/* Remove a utmp/wtmp entry for a tty */
Local void
bsd_tty_remove_utmp(w)
TermWidget w;
{
     /*
      * Again, about everything is stolen from FreeBSD
      * /usr/src/libutil/logout.c and logwtmp.c
      */
     int fd;
     struct utmp ut;
     off_t lseek();
     time_t time();
     struct stat buf;
     
     if ((fd = open(_PATH_UTMP, O_RDWR)) < 0)
	  return;
     while (read(fd, (char *)&ut, 
		 sizeof(struct utmp)) == sizeof(struct utmp)) {
	  if (!ut.ut_name[0] || strncmp(ut.ut_line, w->term.tname + 5,
					sizeof(ut.ut_line)))
	       continue;
	  bzero(ut.ut_name, sizeof(ut.ut_name));
	  bzero(ut.ut_host, sizeof(ut.ut_host));
	  (void)time(&ut.ut_time);
	  (void)lseek(fd, -(long)sizeof(struct utmp), L_INCR);
	  (void)write(fd, (char *)&ut, sizeof(struct utmp));
     }
     (void)close(fd);

#if 0
     /*
      * wtmp handling commented out for now
      */
     if ((fd = open(_PATH_WTMP, O_WRONLY|O_APPEND, 0)) < 0)
	  return;
     if (fstat(fd, &buf) == 0) {
	  if (write(fd, (char *)&ut, sizeof(struct utmp)) !=
	      sizeof(struct utmp))
	       (void) ftruncate(fd, buf.st_size);
     }
     (void) close(fd);
#endif
     
     return;
}

/* If w is NULL, get tty values for invoking tty, not pty */
Local Generic
bsd_tty_get_values(TermWidget w)
{
     return posix_tty_get_values(w);
}

/* Must return some "sane" set of hardwired values */
Local Generic
bsd_tty_get_sane(TermWidget w)
{
     return posix_tty_get_sane(w);
}

/* Whap some saved values onto the tty */
Local void
bsd_tty_set_values(TermWidget w, Generic val)
{
     posix_tty_set_values(w, val);
}

#endif	/* _TTY_BSD_INCLUDE */

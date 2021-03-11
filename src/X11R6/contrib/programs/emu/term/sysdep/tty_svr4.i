/* -*-Mode: C; -*- */

#ifndef _TTY_SYSVR4_INCLUDE
#define _TTY_SYSVR4_INCLUDE

/* tty_svr4.i,v 1.2 1994/05/27 06:22:31 me Exp */

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
 * Tty manipulation routines for System V revision 4 machines.
 *
 * Author: Steven Farmwald
 * Date: March 20th, 1990.
 * Description: Various System Vr4 dependent routines for dealing with ttys
 *		that the Emu system expects us to declare.
 *
 * Revision History:
 *
 * tty_svr4.i,v
 * Revision 1.2  1994/05/27  06:22:31  me
 * New copyright message
 *
 * Revision 1.1.1.1  1994/05/22  11:22:47  me
 * Initial import into CVS
 */

#include <utmp.h>
#include <pwd.h>
#include <termios.h>
#include <sys/stropts.h>


Import struct utmp *getutent();
Import struct utmp *getutid();
Import struct utmp *getutline();

Import void setutent();
Import void endutent();


/* Sets no-blocking I/O on a file descriptor */
Local void
sysvr4_tty_set_nonblocking(w, fd)
TermWidget w;
int fd;
{
     int mode = 1;

     if ((mode = fcntl(fd, F_GETFL, 0)) == -1) {
	  perror("fcntl(fd, F_GETFL, 0)");
	  fatal("Couldn't get initial pty mode");
     }
     mode |= O_NDELAY;
     if (fcntl(fd, F_SETFL, mode) == -1)
	  perror("fcntl(fd, F_SETFL, 0)");
}



Export String
sysvr4_tty_find_pty(w)
TermWidget w;
{
  char *slaveName, *ptsname() ;
  int master, slave ;
  
  /* /dev/ptmx gives us the fd to the master side, if there are any free */

  if ((master = open("/dev/ptmx", O_RDWR, 0)) < 0) { /* no pty's left */
    pmessage("No free ptys.  Sorry mate.");
    return NULL;
  }

  /* change permissions, remove the lock, etc. */
  grantpt(master);
  unlockpt(master);

  slaveName = ptsname(master) ;	/* get the slave-side name */

  if ((slave = open(slaveName, O_RDWR, 0)) < 0) { /* shouldn't happen,
						     but I'm careful */
    close(master) ;
    return(NULL) ;
  }

  if (ioctl (slave, I_PUSH, "ptem") < 0) {
       perror("ioctl(I_PUSH, 'ptem')") ;
       exit(1) ;
  }
  if (ioctl (slave, I_PUSH, "ldterm") < 0) {
       perror("ioctl(I_PUSH, 'ldterm')") ;
       exit(1) ;
  }
  if (ioctl (slave, I_PUSH, "ttcompat") < 0) {
       perror("ioctl(I_PUSH, 'ttcompat')") ;
       exit(1) ;
  }

  sysvr4_tty_set_nonblocking(w, slave) ;
  
  w->term.slave  = slave ;
  w->term.master = master ;

  return(XtNewString(slaveName)) ;
  
  
}

/* Add a utmp/wtmp entry for a tty */
Local void
sysvr4_tty_add_utmp(w)
TermWidget w;
{
     struct passwd *pw;
     struct utmp utmp;
     char *name;

     /* Reset before searching */
     (void)setutent();

     /*
      * Copy the last two chars over (e.g. "ttyp0" -> "p0") for
      * /etc/inittab lookup.
      */
     (void)strncpy(utmp.ut_id, w->term.tname + (strlen(w->term.tname) - 2),
		   sizeof(utmp.ut_id));

     /* look for slot of former process */
     utmp.ut_type = DEAD_PROCESS;

     /* position us at the entry */
     (void)getutid(&utmp);

     /* build a new entry there */

     /* who's there? */
     pw = getpwuid(w->term.uid);

     /* we're somebody.. */
     utmp.ut_type = USER_PROCESS;
     utmp.ut_exit.e_exit = 2;

     /* or are we? */
     name = (pw && pw->pw_name) ? pw->pw_name : "(unknown)";

     /* copy over name information */
     (void)strncpy(utmp.ut_user, name, sizeof(utmp.ut_user));
     (void)strncpy(utmp.ut_id, w->term.tname + (strlen(w->term.tname) - 2),
		   sizeof(utmp.ut_id));
     (void)strncpy(utmp.ut_line, w->term.tname + 5, /* 5 is len of "/dev/" */
		   sizeof(utmp.ut_line));
     (void)strncpy(utmp.ut_name, name, sizeof(utmp.ut_name));

     /* pid and current time */
     utmp.ut_pid = w->term.pid;
     utmp.ut_time = time((long *)0);

     /* write it out */
     (void)pututline(&utmp);

     /* close up shop */
     (void)endutent();
}

/* Remove a utmp/wtmp entry for a tty */
Local void
sysvr4_tty_remove_utmp(w)
TermWidget w;
{
     struct utmp utmp, *utptr;

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

/* Set the row and column values for a tty */
Local void
sysvr4_tty_set_size(w, rows, cols, width, height)
TermWidget w;
int rows, cols, width, height;
{
     struct winsize wz;
     int pgrp;
     Import int kill();

     wz.ws_row = rows;
     wz.ws_col = cols;
     wz.ws_xpixel = width;
     wz.ws_ypixel = height;
/*      ioctl(w->term.slave, TIOCSWINSZ, &wz); */
     ioctl(w->term.master, TIOCSWINSZ, &wz);
     if (ioctl(w->term.slave, TIOCGPGRP, &pgrp) != -1)
	  kill(-pgrp, SIGWINCH);
}

/*
 * These next three routines complement eachother by getting and setting
 * some number of tty values using a private (and probably machine dependent)
 * format. It is up to the implementor of these routines to devise some
 * suitable data structure for getting, setting and properly storing said
 * values for their particular operating system. The term widget will
 * encode these values as "generic" and leave it to the implementor to keep
 * them straight.
 *
 */

/* What sort of structure we'll be passing around as "values" */
typedef struct termios TtyStuff;

/* File descriptors to try */
Local int Fd_try[] = { 0, 1, 2, -1, 0, -1 };

/* If w is NULL, get tty values for invoking tty, not pty */
Local Generic
sysvr4_tty_get_values(w)
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
	  warn("sysvr4_tty_get_values: Couldn't tcgetattr, errno = %d", errno);

     return (Generic)&ret;
}

Local void
sysvr4_tty_set_values(w, val)
TermWidget w;
Generic val;
{
     TtyStuff master, slave;
     
/*      cfsetospeed((struct termios *)val, B9600); */
     
/*      tcsetattr(w->term.master, TCSADRAIN, (struct termios *)val); */
     tcsetattr(w->term.slave, TCSADRAIN, (struct termios *)val);
     
/*      tcdrain(w->term.master); */
/*      tcdrain(w->term.slave); */
     
}

/* Return some "sane" set of hardwired values for a generic sysv box */
Local Generic
sysvr4_tty_get_sane(w)
TermWidget w;
{
/*     Local TtyStuff tio; */
     static struct termios tio ;
     
     /* Grab current values */
     bcopy(tty_get_values(w), &tio, sizeof(tio));

     /* Mung the important ones into a known state */
     /* input: nl->nl, don't ignore cr, cr->nl */
     tio.c_iflag &= ~(INLCR|IGNCR);
     tio.c_iflag |= ICRNL;
     /* ouput: cr->cr, nl is not return, no delays, ln->cr/nl */
     tio.c_oflag &= ~(OCRNL|ONLRET|NLDLY|CRDLY|TABDLY|BSDLY|VTDLY|FFDLY);
     tio.c_oflag |= ONLCR;
     /* enable signals, canonical processing (erase, kill, etc), echo */
     tio.c_lflag &= ~(ECHOCTL);
     tio.c_lflag |= ISIG|ICANON|ECHO;
     /* reset EOL to defalult value */
     tio.c_cc[VEOL] = '@' & 0x3f;		/* '^@'	*/
     /* certain shells (ksh & csh) change EOF as well */
     tio.c_cc[VEOF] = 'D' & 0x3f;		/* '^D'	*/
     
     cfsetospeed(&tio, B9600);
     cfsetispeed(&tio, B9600);
     
     return (Generic)&tio;
}

#endif	/* _TTY_SYSVR4_INCLUDE */

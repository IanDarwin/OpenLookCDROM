/* -*-Mode: C; -*- */

#ifndef _TTY_POSIX_INCLUDE
#define _TTY_POSIX_INCLUDE

/* tty_posix.i,v 1.2 1994/05/27 06:22:23 me Exp */

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
 * Tty service routines for generic POSIX.1 compliant systems.
 *
 * Author: Jordan K. Hubbard
 * Date: August 26th, 1991
 * Description: Various routines for dealing with ttys that are generic to
 *		POSIX compliant systems (in theory).
 *
 * Revision History:
 *
 * tty_posix.i,v
 * Revision 1.2  1994/05/27  06:22:23  me
 * New copyright message
 *
 * Revision 1.1.1.1  1994/05/22  11:22:46  me
 * Initial import into CVS
 *
 * Revision 1.3  92/05/16  06:15:13  jkh
 * Reorganized basic driver structure somewhat
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

typedef struct termios TtyStuff;

/* File descriptors to try */
Local int Fd_try[] = { 0, 1, 2, -1, 0, -1 };

/* If w is NULL, get tty values for invoking tty, not pty */
Local Generic
posix_tty_get_values(w)
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

/* Some termios.h files don't define this, and we need it for a fall-back */
#ifndef CNUL
#define CNUL	'\0'
#endif

/* Must return some "sane" set of hardwired values */
Local Generic
posix_tty_get_sane(w)
TermWidget w;
{
     static struct termios sane;
     static char cchars[] = {
	  CINTR, CQUIT, CERASE, CKILL, CEOF,
#ifdef CEOL
	  CEOL,
#else
	  CNUL,
#endif 
#ifdef CEOL2
	  CEOL2,
#else
	  CNUL,
#endif
#ifdef CSWTCH
	  CSWTCH,
#else
	  CNUL,
#endif
#ifdef	  CSTART
	  CSTART,
#else
	  CNUL,
#endif
#ifdef CSTOP
	  CSTOP,
#else
	  CNUL,
#endif
#ifdef CSUSP
	  CSUSP,
#else
	  CNUL,
#endif
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
#ifdef SYSV
     /* This is not really honest POSIX, it's a SYSV system masquerading */
     sane.c_line = 0;
#endif
     bcopy(cchars, sane.c_cc, sizeof(cchars));
     return (Generic)&sane;
}

/* Whap some saved values onto the tty */
Local void
posix_tty_set_values(w, val)
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

#endif	/* _TTY_POSIX_INCLUDE */

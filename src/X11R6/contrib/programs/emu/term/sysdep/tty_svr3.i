/* -*-Mode: C; -*- */

#ifndef _TTY_SYSVR3_INCLUDE
#define _TTY_SYSVR3_INCLUDE

/* tty_svr3.i,v 1.2 1994/05/27 06:22:29 me Exp */

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
 * Tty manipulation routines for System V revision 3 machines.
 *
 * Author: Jordan K. Hubbard
 * Date: March 20th, 1990.
 * Description: Various System Vr3 dependent routines for dealing with ttys
 *		that the Emu system expects us to declare.
 *
 * Revision History:
 *
 * tty_svr3.i,v
 * Revision 1.2  1994/05/27  06:22:29  me
 * New copyright message
 *
 * Revision 1.1.1.1  1994/05/22  11:22:47  me
 * Initial import into CVS
 */

/* Mandatory */
#ifndef _TTY_INCLUDED
#define _TTY_INCLUDED
#endif

/* Include the generic sysv code. */
#include "sysdep/tty_sv.i"

/*
 * Get the tty values for a revision 3 system, the TtyStuff and try
 * structures being the same for all revisions of sysv, and so declared
 * by the generic sysv code.
 *
 * As per convention, if w is NULL, get tty values for invoking tty, not pty.
 */
Local Generic
sysvr3_tty_get_values(w)
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

	  r = ioctl(try[i], TCGETA, (char *)&ret);
	  if (!r)	/* success? */
	       break;
     }

     if (try[i] == -1)
	  warn("sysvr3_tty_get_values: Can't get terminal values, errno = %d",
	       errno);
     return (Generic)&ret;
}

/* Whap some values onto the tty */
Local void
sysvr3_tty_set_values(w, val)
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
          r = ioctl(try[i], TCSETA, (char *)val);
	  if (!r)	/* success? */
	       break;
     }
     if (try[i] == -1)
	  warn("tty_set_values: Couldn't TCSETS, errno = %d.", errno);
}

Local Generic
sysvr3_tty_get_sane(w)
TermWidget w;
{
     Local TtyStuff tio;
     
     /* Grab current values */
     bcopy(sysvr3_tty_get_values(w), &tio, sizeof(tio));

     /* Mung the important ones into a known state */
     /* input: nl->nl, don't ignore cr, cr->nl */
     tio.c_iflag &= ~(INLCR|IGNCR);
     tio.c_iflag |= ICRNL;
     /* ouput: cr->cr, nl is not return, no delays, ln->cr/nl */
     tio.c_oflag &= ~(OCRNL|ONLRET|NLDLY|CRDLY|TABDLY|BSDLY|VTDLY|FFDLY);
     tio.c_oflag |= ONLCR;
#ifdef BAUD_0
     /* baud rate is 0 (don't care) */
     tio.c_cflag &= ~(CBAUD);
#else	/* !BAUD_0 */
     /* baud rate is 9600 (nice default) */
     tio.c_cflag &= ~(CBAUD);
     tio.c_cflag |= B9600;
#endif	/* !BAUD_0 */
     /* enable signals, canonical processing (erase, kill, etc), echo */
     tio.c_lflag |= ISIG|ICANON|ECHO;
     /* reset EOL to defalult value */
     tio.c_cc[VEOL] = '@' & 0x3f;		/* '^@'	*/
     /* certain shells (ksh & csh) change EOF as well */
     tio.c_cc[VEOF] = 'D' & 0x3f;		/* '^D'	*/
     return (Generic)&tio;
}

#endif	/* _TTY_SYSVR3_INCLUDE */

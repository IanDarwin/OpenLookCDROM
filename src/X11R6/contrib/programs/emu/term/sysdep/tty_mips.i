/* -*-Mode: C; -*- */

#ifndef _TTY_MIPS_INCLUDE
#define _TTY_MIPS_INCLUDE

/* tty_mips.i,v 1.2 1994/05/27 06:22:19 me Exp */

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
 * Tty allocation/deallocation routines for MIPS systems.
 *
 * Author: Jordan K. Hubbard
 * Date: August 22nd, 1991
 * Description: Here are all the routines for getting and setting tty
 *		parameters on MIPS and PMAX systems.
 *
 * tty_mips.i,v
 * Revision 1.2  1994/05/27  06:22:19  me
 * New copyright message
 *
 * Revision 1.1.1.1  1994/05/22  11:22:46  me
 * Initial import into CVS
 *
 * Revision 1.2  92/05/16  06:15:13  jkh
 * Reorganized basic driver structure somewhat
 */

/* Mandatory */
#ifndef _TTY_INCLUDED
#define _TTY_INCLUDED
#endif

/* Sort of BSD, sort of POSIX, mostly neither. */
#include "sysdep/tty_bsd.i"
#include "sysdep/tty_posix.i"

Export void
tty_set_nonblocking(w, fd)
TermWidget w;
int fd;
{
     /* Generic BSD does everything we need */
     bsd_tty_set_nonblocking(w, fd);
}

Export String
tty_find_pty(w)
TermWidget w;
{
     /* Generic unix code does what we want */
     return unix_tty_find_pty(w);
}

/* Set a new tty size */
Export void
tty_set_size(w, rows, cols, width, height)
TermWidget w;
int rows, cols, width, height;
{
     /* Generic BSD does what we want */
     bsd_tty_set_size(w, rows, cols, width, height);
}

/* Add a utmp/wtmp entry for a tty */
Export void
tty_add_utmp(w)
TermWidget w;
{
     /* Generic BSD does what we want */
     bsd_tty_add_utmp(w);
}

/* Remove a utmp/wtmp entry for a tty */
Export void
tty_remove_utmp(w)
TermWidget w;
{
     /* Generic BSD does what we want */
     bsd_tty_remove_utmp(w);
}

/* If w is NULL, get tty values for invoking tty, not pty */
Export Generic
tty_get_values(w)
TermWidget w;
{
     /* Use the posix tty handling */
     return posix_tty_get_values(w);
}

/* Must return some "sane" set of hardwired values */
Export Generic
tty_get_sane(w)
TermWidget w;
{
     /* Use the posix sane values */
     return posix_tty_get_sane(w);
}

/* Whap some saved values onto the tty */
Export void
tty_set_values(w, val)
TermWidget w;
Generic val;
{
     /* Use the posix tty handling */
     posix_tty_set_values(w, val);
}

#endif	/* _TTY_MIPS_INCLUDE */

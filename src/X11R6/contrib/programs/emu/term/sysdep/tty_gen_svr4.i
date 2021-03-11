/* -*-Mode: C; -*- */

#ifndef _TTY_GEN_SYSVR4_INCLUDE
#define _TTY_GEN_SYSVR4_INCLUDE

#define _TTY_INCLUDED /* mandatory */

/* tty_gen_svr4.i,v 1.2 1994/05/27 06:22:10 me Exp */

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
 * Tty manipulation routines for generic SVR4 machines (I hope).
 *
 * Author: Michael Elbel
 * Date: May 1993
 *
 * tty_gen_svr4.i,v
 * Revision 1.2  1994/05/27  06:22:10  me
 * New copyright message
 *
 * Revision 1.1.1.1  1994/05/22  11:22:47  me
 * Initial import into CVS
 *
 */

/* Include the sysv revision 4 stuff */
#include "sysdep/tty_svr4.i"

Local void
tty_set_nonblocking(w, fd)
TermWidget w;
int fd;
{
     /* Generic SYSVR4 does what we want - just inherit it */
     sysvr4_tty_set_nonblocking(w, fd);
}

/*
 * Search for an available pty, putting descriptor in passed "descr" and
 * returning name, if found. Since we're only called once (better be), it's
 * OK to stick this name in static storage.
 *
 * The process code is expected to take care of the hairy mode/process
 * group/controlling tty/etc. setup stuff later when the child is forked
 * off, so we don't do it now.
 *
 */
Export String
tty_find_pty(w)
TermWidget w;
{
     /* Generic sysvr4 does what we want */
     return sysvr4_tty_find_pty(w);
}

/* Set a new tty size */
Export void
tty_set_size(w, rows, cols, width, height)
TermWidget w;
int rows, cols, width, height;
{
     /* Generic sysvr4 does what we want */
     sysvr4_tty_set_size(w, rows, cols, width, height);
}

/* Add a utmp/wtmp entry for a tty */
Export void tty_add_utmp(w)
TermWidget w;
{
     /* Generic SYSVR4 does what we want */
     sysvr4_tty_add_utmp(w);
}

/* Remove a utmp/wtmp entry for a tty */
Export void tty_remove_utmp(w)
TermWidget w;
{
     /* Generic SYSVR4 does what we want */
     sysvr4_tty_remove_utmp(w);
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

/* Get the current settings for tty */
Export Generic
tty_get_values(w)
TermWidget w;
{
     /* sysvr4 code does what we want */
     return sysvr4_tty_get_values(w);
}

/* Must return some "sane" set of hardwired values */
Export Generic
tty_get_sane(w)
TermWidget w;
{
     /* sysvr4 code does what we want */
     return sysvr4_tty_get_sane(w);
}

/* Whap some settings onto the tty */
Export void
tty_set_values(w, val)
TermWidget w;
Generic val;
{
     /* sysvr4 code does what we want */
     sysvr4_tty_set_values(w, val);
}

#endif	/* _TTY_GEN_SYSVR4_INCLUDE */

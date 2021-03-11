/* -*-Mode: C; -*- */

#ifndef _TTY_PCS_INCLUDE
#define _TTY_PCS_INCLUDE

/* tty_pcs.i,v 1.2 1994/05/27 06:22:21 me Exp */

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
 * Tty manipulation routines for PCS machines.
 *
 * Author: Jordan K. Hubbard
 * Date: March 20th, 1990.
 * Description: Various routines for tweaking ttys on PCS/Cadmus systems
 *		that the Emu system expects us to declare.
 *
 * Revision History:
 *
 * tty_pcs.i,v
 * Revision 1.2  1994/05/27  06:22:21  me
 * New copyright message
 *
 * Revision 1.1.1.1  1994/05/22  11:22:46  me
 * Initial import into CVS
 *
 * Revision 1.4  92/05/16  06:15:13  jkh
 * Reorganized basic driver structure somewhat
 * 
 * Revision 1.3  90/11/13  14:59:31  jkh
 * Double descriptor version.
 */

/* Mandatory */
#ifndef _TTY_INCLUDED
#define _TTY_INCLUDED
#endif

/* Include the sysv revision 3 stuff */
#include "sysdep/tty_svr3.i"

Local void
tty_set_nonblocking(w, fd)
TermWidget w;
int fd;
{
     /* Generic SYSV does what we want - just inherit it */
     sysv_tty_set_nonblocking(w, fd);
}

/*
 * Search for an available pty, putting descriptor in passed "descr" and
 * returning name, if found. Since we're only called once (better be), it's
 * OK to stick this name in static storage.
 *
 * Provided that the #defines for PTY_ITERATION/PTYCHAR1/.. are done
 * properly for your system in os.h, this should be reasonably system
 * independent, meaning you may be able to just grab this intact for your
 * system. It is system dependent nonetheless, so it goes in this file
 * rather than tty.c.
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
     /* Generic unix pty finding code works for us */
     return unix_tty_find_pty(w);
}

/* Set a new tty size */
Export void
tty_set_size(w, rows, cols, width, height)
TermWidget w;
int rows, cols, width, height;
{
     /* Generic sysv does what we want */
     sysv_tty_set_size(w, rows, cols, width, height);
}

/* Add a utmp/wtmp entry for a tty */
Export void tty_add_utmp(w)
TermWidget w;
{
     /* Generic SYSV does what we want */
     sysv_tty_add_utmp(w);
}

/* Remove a utmp/wtmp entry for a tty */
Export void tty_remove_utmp(w)
TermWidget w;
{
     /* Generic SYSV does what we want */
     sysv_tty_remove_utmp(w);
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
     /* sysvr3 code does what we want */
     return sysvr3_tty_get_values(w);
}

/* Must return some "sane" set of hardwired values */
Export Generic
tty_get_sane(w)
TermWidget w;
{
     /* sysvr3 code does what we want */
     return sysvr3_tty_get_sane(w);
}

/* Whap some settings onto the tty */
Export void
tty_set_values(w, val)
TermWidget w;
Generic val;
{
     /* sysvr3 code does what we want */
     sysvr3_tty_set_values(w, val);
}

#endif	/* _TTY_PCS_INCLUDE */

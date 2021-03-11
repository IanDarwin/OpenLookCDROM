/* -*-Mode: C; -*- */

#ifndef _PROCESS_GEN_BSD_INCLUDE
#define _PROCESS_GEN_BSD_INCLUDE

#define _PROCESS_INCLUDED /* mandatory */

/* proc_gen_bsd.i,v 1.2 1994/05/27 06:21:42 me Exp */

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
 *
 * Author: Jordan K. Hubbard
 * Date: Dec 23, 1993.
 * Description: Here are all the "generic BSD" routines for frobbing the
 *	 	state of the main process.
 *
 *
 * proc_gen_bsd.i,v
 * Revision 1.2  1994/05/27  06:21:42  me
 * New copyright message
 *
 * Revision 1.1.1.1  1994/05/22  11:22:45  me
 * Initial import into CVS
 *
 */

/*
 * These next three Exported routines, process_wait(), process_init() and
 * process_cleanup() are expected of us.  The rest is up to our personal
 * tastes in how we want to organize the work procedures.
 */

#define WAIT_STATUS_TYPE int

/* Inherit the generic BSD stuff */
#include "sysdep/proc_bsd.i"

Export int
process_wait(fc)
int *fc;
{
     /* generic unix routine does what we want */
     return bsd_process_wait(fc);
}

Export void
process_cleanup(w)
TermWidget w;
{
     /* Just remove the utmp entry */
     tty_remove_utmp(w);
}

Export void
process_init(w)
TermWidget w;
{
     /* record uid and gid values for later use */
     w->term.uid = getuid();
     w->term.gid = getgid();

     /* generic BSD does what we want */
     bsd_process_init(w);
}

#endif	/* _PROCESS_GEN_BSD_INCLUDE */

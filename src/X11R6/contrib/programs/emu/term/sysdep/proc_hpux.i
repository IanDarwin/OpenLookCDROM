/* -*-Mode: C; -*- */

/* proc_hpux.i,v 1.2 1994/05/27 06:21:46 me Exp */

#ifndef _PROCESS_HPUX_INCLUDE
#define _PROCESS_HPUX_INCLUDE

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
 * Various process manipulation routines specific to HPUX systems.
 * Taken as is from process_pcs.i
 *
 * Author: Avishaiy A. Yacobi
 * Date: Aug 10th, 1993.
 * Description: Here are all the HPUX specific routines for frobbing the
 *      state of of the main process.
 *
 * Revision History:
 *
 * proc_hpux.i,v
 * Revision 1.2  1994/05/27  06:21:46  me
 * New copyright message
 *
 * Revision 1.1.1.1  1994/05/22  11:22:46  me
 * Initial import into CVS
 */

/* Mandatory */
#ifndef _PROCESS_INCLUDED
#define _PROCESS_INCLUDED
#endif

/*
 * These next three Exported routines, process_wait(), process_init() and
 * process_cleanup() are expected of us.  The rest is up to our personal
 * tastes in how we want to organize the work procedures.
 */

/* What our wait() routine expects */
#define WAIT_STATUS_TYPE int

/* Inherit the generic SYSV stuff */
#include "sysdep/proc_sv.i"

Export int
process_wait(fc)
WAIT_STATUS_TYPE *fc;
{
	/* generic unix routine does what we want */
	return unix_process_wait(fc);
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
	Import uid_t getuid();
	Import gid_t getgid();

	/* record uid and gid values for later use */
	w->term.uid = getuid();
	w->term.gid = getgid();

	/* generic sysv does what we want */
	sysv_process_init(w);
}

#endif /* _PROCESS_HPUX_INCLUDE */

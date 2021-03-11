/* tty.c,v 1.2 1994/05/27 06:21:01 me Exp */

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
 * Tty allocation/deallocation routines.
 *
 * Author: Jordan K. Hubbard
 * Date: March 20th, 1990.
 * Description: This file conditionally includes various machine specific
 *		chunks of code for dealing with the tty for the tty
 *
 * Revision History:
 *
 * tty.c,v
 * Revision 1.2  1994/05/27  06:21:01  me
 * New copyright message
 *
 * Revision 1.1.1.1  1994/05/22  11:22:44  me
 * Initial import into CVS
 *
 * Revision 1.14  92/05/16  06:11:44  jkh
 * Reorganization for new process driver structure.
 * 
 * Revision 1.13  92/02/26  11:40:00  me
 * Steve Crooks' clix port and general cleanup
 * 
 * Revision 1.7  90/11/13  14:54:22  jkh
 * double fd version.
 */

#include "TermP.h"

/*
 * Since different systems vary widely in their handling of getting and
 * setting tty attributes, each system type has its own file. If you
 * can get by with making only subtle tweaks to an existing file, then
 * by all means #ifdef them in, but DO NOT make major changes in this way -
 * create another file instead. I am trying to avoid the crazed proliferation
 * of #ifdefs that makes so much of the original xterm code unreadable.
 * Thanks. -jkh
 *
 * I use "#if defined(xxx)" instead of "#ifdef xxx" so that you can easily
 * append new conditionals for systems that share existing .i files. Sticking
 * to this convention is encouraged.
 *
 * In order to avoid #ifdefs that walk across the screen (as was before the
 * case), I've changed things so that the pre-processor symbol is assumed
 * to be unique (if there is a clash, && together some combination of
 * symbols that are unique for your system) and only one file will be
 * included.  sysdep/<file>_tty.i is now expected to define the
 * preprocessor symbol _TTY_INCLUDED.
 *
 * Note: Do not simply say something like "#if defined(BSD) ..." since
 * that will almost certainly break other systems that are merely derived
 * from BSD and need their own specific initialization files to be included
 * instead.  Always include a unique file for your system and then inherit
 * those routines from bsd/sysv/posix (or whatever) when and where you can.
 * Some of the existing configurations do this, so look to them for working
 * examples. - Jordan
 *
 */

/* Define these if you support the newer POSIX tc{get,set}attr() calls */
#ifdef _POSIX_SOURCE
#define TCGETATTR
#define TCSETATTR
#endif

#if defined(PCS)
#     include "sysdep/tty_pcs.i"
#endif

#if defined(mips) && !defined(PCS)
#     include "sysdep/tty_mips.i"
#endif

#if defined(sun)
#     include "sysdep/tty_sun.i"
#endif

#if defined(__clipper__)
#     include "sysdep/tty_clix.i"
#endif

#if defined(MACH)
#     include "sysdep/tty_mach.i"
#endif
 
#if defined(hpux)
#     include "sysdep/tty_hpux.i"
#endif
 
#if defined(__BSD_NET2__)
#     include "sysdep/tty_gen_bsd.i"
#endif

#if defined(linux)
#     include "sysdep/tty_linux.i"
#endif

#if defined(SVR4) 
#     include "sysdep/tty_gen_sysvr4.i"
#endif

#ifndef _TTY_INCLUDED
#     include "sysdep/unknown.i"
#endif

/*
 * What follows should be reasonably system independent. If there's something
 * here that needs radical munging for your system, move it into a .i file
 * and tell me why you needed to do it; I'll then see whether or not there's
 * a more general solution for more than just your machine. -jkh
 */

/* Open a PTY and initialize it */
Export Boolean
tty_open(TermWidget w)
{
     if (!(w->term.tname = tty_find_pty(w)))
	  return FALSE;
     else {
	  /* We know by now that there is at least one input callback */
	  w->term.xi = XtAddInput((int)w->term.master,
				  (XtPointer)XtInputReadMask,
				  (XtInputCallbackProc)iparse,
				  (XtPointer)w);
	  /* Output callbacks are always optional */
	  if (w->term.out_parse)
	       w->term.xo = XtAddInput((int)w->term.master,
				       (XtPointer)XtInputWriteMask,
				       (XtInputCallbackProc)oparse,
				       (XtPointer)w);
     }
     return TRUE;
}

Export void
tty_close(TermWidget w)
{
     if (w->term.xi)
	  XtRemoveInput(w->term.xi);
     if (w->term.xo)
	  XtRemoveInput(w->term.xo);

     if (w->term.master >= 0)
	  close(w->term.master);

     if (w->term.slave >= 0)
	  close(w->term.slave);

     w->term.xi = w->term.xo = (XtInputId)NULL;
     w->term.master = w->term.slave = -1;
}

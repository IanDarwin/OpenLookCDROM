/* -*-Mode: C; -*- */

#ifndef _TTY_UNIX_INCLUDE
#define _TTY_UNIX_INCLUDE

/* tty_unix.i,v 1.2 1994/05/27 06:22:33 me Exp */

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
 * Tty manipulation routines for Generic UNIX machines.
 *
 * Author: Jordan K. Hubbard
 * Date: March 20th, 1990.
 * Description: Various UNIX dependent routines for dealing with ttys.
 *		These routines are typically used by more specific tty_<mx>.i
 *		files, where <mx> is some machine running a reasonably
 *		capable version of UNIX.
 *
 * Revision History:
 *
 * tty_unix.i,v
 * Revision 1.2  1994/05/27  06:22:33  me
 * New copyright message
 *
 * Revision 1.1.1.1  1994/05/22  11:22:47  me
 * Initial import into CVS
 */


/*
 * Search for an available pty, putting descriptor in passed "descr" and
 * returning name, if found.
 *
 * Provided that the #defines for PTY_ITERATION/PTYCHAR1/.. are done
 * properly for your system in os.h, this should be reasonably system
 * independent, meaning you may be able to just grab this intact for your
 * machine. It is UNIX dependent nonetheless, so it goes in this file
 * rather than tty.c.
 *
 * The process code is expected to take care of the hairy mode/process
 * group/controlling tty/etc. setup stuff later when the child is forked
 * off, so we don't do it now.
 *
 */

Export String
unix_tty_find_pty(w)
TermWidget w;
{
     char ttyname[80];
     struct stat st;
     register char *c1, *c2;
     Import void tty_set_nonblocking();
 
     /* Use user provided search loop or our own to grub through ptys */
#ifdef PTY_ITERATION
     PTY_ITERATION
#else
     for (c1 = PTYCHAR1; *c1; ++c1) {
	  for (c2 = PTYCHAR2; *c2; ++c2) {
#endif
	       /* Go for the master side */
	       sprintf(ttyname, PTYDEV, *c1, *c2);
#ifdef DEBUG
	       debug("unix_tty_find_pty: Trying to open %s", ttyname);
#endif
	       if (stat(ttyname, &st) < 0)
		    continue;
	       if ((w->term.master = open(ttyname, O_RDWR, 0)) >= 0) {
		    /*
		     * Now make sure that the slave side is available.
		     * This allows for a bug in some versions of rlogind
		     * and gives us a handle for ioctl() ops.
		     */
		    sprintf(ttyname, TTYDEV, *c1, *c2);
#ifdef DEBUG
		    debug("unix_tty_find_pty: Checking slave side %s",
			  ttyname);
#endif
		    if ((w->term.slave = open(ttyname, O_RDWR, 0)) >= 0) {
			 /* All is casual, dude. */
#ifdef DEBUG
			 debug("unix_tty_find_pty: Found pty %s", ttyname);
#endif
#ifdef NOTDEF
			 /*
			  * This probably was the single reason, emu
			  * didn't work on BSD systems
			  */
			 tty_set_nonblocking(w, w->term.slave);
#endif
			 return XtNewString(ttyname);
                    }
		    else
			 close(w->term.master);
	       }
	  }
     }
     pmessage("No free ptys.  Sorry mate.");
     return NULL;
}

#endif	/* _TTY_UNIX_INCLUDE */

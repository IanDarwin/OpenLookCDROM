#if !defined(lint) && !defined(__clipper__)
static char *rcsid = "parse_dumb.c,v 1.2 1994/05/27 06:21:21 me Exp";
#endif

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
 * This file implements some simple minded hard parsers.
 *
 * Author: Jordan K. Hubbard
 * Date: October 7th, 1990.
 * Description: Low intellect hard parsers for emulating dumb terminals.
 * all hard-coded parsers.
 *
 * Revision History:
 *
 * parse_dumb.c,v
 * Revision 1.2  1994/05/27  06:21:21  me
 * New copyright message
 *
 * Revision 1.1.1.1  1994/05/22  11:22:45  me
 * Initial import into CVS
 *
 * Revision 1.2  92/02/26  11:42:12  me
 * Steve Crooks' clix port and general code cleanup
 */

#include "TermP.h"

/*
 * Do a simple "tty" style parser. Right now it assumes that it's supposed
 * to handle everything and just inserts what it doesn't understand. If you
 * wanted it use it as a base layer for a more sophisticated emulation,
 * however, you could simply make it return when it found something it didn't
 * know directly about.
 */
/*ARGSUSED*/
void parseDumb(TermWidget w, caddr_t client_data, caddr_t call_data)
{
     register int i, nread = w->term.chars_to_parse;
     register String buf = (String)call_data;

     for (i = 0; i < nread; i++) {
	  int j;

	  /* First see how much we can insert in one go */
	  for (j = i; j < nread && !iscntrl(buf[j]); j++)
	       w->term.chars_to_parse--;
	  if (i - j) {
	       XpTermInsertText(w, buf + i, i - j);
	       i = j;
	       continue;
	  }
	  else if (iscntrl(buf[i])) {
	       --w->term.chars_to_parse;
	       /* Check for known control characters next */
	       switch (buf[i]) {
	       case '\r':	/* Carriage return */
		    XpTermSetRegister(w, 'x', CB_INT_TYPE, (caddr_t)0);
		    XpTermDispatchRequest(w, OP_MOVE_ABS_COLUMN);
		    break;

	       case '\n':	/* Newline */
		    XpTermSetRegister(w, 'y', CB_INT_TYPE, (caddr_t)1);
		    XpTermDispatchRequest(w, OP_MOVE_REL_ROW_SCROLLED);
		    break;

	       case '\b':	/* Backspace */
		    XpTermSetRegister(w, 'x', CB_INT_TYPE, (caddr_t)-1);
		    XpTermDispatchRequest(w, OP_MOVE_REL_COLUMN);
		    break;

	       case '\007':	/* Bell */
		    XpTermDispatchRequest(w, OP_RING_BELL);
		    break;
	       default:
		    /* Don't know what to do with it, insert it I guess */
		    XpTermInsertText(w, buf + i, 1);
	       }
	  }
     }
}

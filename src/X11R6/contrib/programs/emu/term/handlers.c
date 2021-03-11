#if !defined(lint) && !defined(__clipper__)
static char *rcsid = "handlers.c,v 1.2 1994/05/27 06:20:56 me Exp";
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
 * Interface forwarders.
 *
 * Author: Jordan K. Hubbard
 * Date: March 20th, 1990.
 * Description: All sections comprising the term widget are designed so
 *		that data is passed in and out via rigidly defined "contact"
 *		procedures. These routines in this file act as intermediaries
 *		to "glue" various modules together.
 *
 * Revision History:
 *
 * handlers.c,v
 * Revision 1.2  1994/05/27  06:20:56  me
 * New copyright message
 *
 * Revision 1.1.1.1  1994/05/22  11:22:44  me
 * Initial import into CVS
 *
 * Revision 1.10  92/02/26  11:39:53  me
 * Steve Crooks' clix port and general cleanup
 * 
 * Revision 1.9  91/09/30  21:40:16  jkh
 * Checkin prior to DECWRL merge.
 */

#include "TermP.h"

Local void
copy_comblock(ComBlockPtr from, ComBlockPtr to)
{
     if ((cb_opcode(to) = cb_opcode(from)) != OP_INSERT)
	  bcopy(cb_regs(from), cb_regs(to), sizeof(Register) * CB_NREGS);
     else
	  bcopy(cb_buffer(from), cb_buffer(to),
		(cb_nbytes(to) = cb_nbytes(from)));
}

/*
 * Dispatch an event to the canvas, attempting to do various intelligent
 * things to adjacent events along the way. These include bundling like
 * requests together into single requests, collapsing "delta" requests into
 * single ones and discarding requests who's effects have been negated by
 * later ones. This routine is registered as the trie parser's external data
 * contact.
 */
Export void
dispatch(TermWidget w, ComBlockPtr cb)
{
     register ComBlockPtr old, new;
     Widget canvas = w->composite.children[0];

     old = w->term.save;
     new = cb;

     /*
      * If this event is to be dispatched IMMEDIATELY, flush saved
      * event (if there) and send this one directly out, not saving
      * it.
      */
     if (w->term.immediate) {
	  if (cb_opcode(old) != OP_NOP) {
	       /* No need to put the cursor back yet */
	       XpTermCanvasDispatch(canvas, old, False);
	       cb_opcode(old) = OP_NOP;
	  }
	  /*
	   * We don't know if another request will follow,
	   * so better put the cursor back
	   */
	  XpTermCanvasDispatch(canvas, new, True);
	  w->term.immediate = FALSE;
	  return;
     }

     if (cb_opcode(old) == OP_INSERT && cb_opcode(new) == OP_INSERT) {
	  /*
	   * If we can't combine, dispatch old first,
	   *  no need to put the cursor back here.
	    */
	  if (cb_nbytes(new) + cb_nbytes(old) >= BUF_SZ) {
	       XpTermCanvasDispatch(canvas, old, False);
	       cb_nbytes(old) = 0;
	  }
	  bcopy(cb_buffer(new), cb_buffer(old) + cb_nbytes(old),
		cb_nbytes(new));
	  cb_nbytes(old) += cb_nbytes(new);
     }
     /* If it's an outdated move, nuke it */
     else if (OP_IS_MOVE(cb_opcode(old)) && cb_opcode(new) == OP_MOVE_ABS)
	  copy_comblock(new, old);
     /* If it's something about to be obviated by a clear screen, punt */
     else if (cb_opcode(old) == OP_INSERT && cb_opcode(new) == OP_CLEAR_SCREEN)
	  copy_comblock(new, old);
     else {
	  /*
	   * Can't do anything cute, dump old (if existant) and reset,
	   * but don't draw the cursor yet
	   */
	  if (cb_opcode(old) != OP_NOP)
	       XpTermCanvasDispatch(canvas, old, False);
	  copy_comblock(new, old);
     }
     if (!w->term.chars_to_parse) {
	  /* Here we have to put the cursor back */
	  XpTermCanvasDispatch(canvas, old, True);
	  cb_opcode(old) = OP_NOP;
	  cb_nbytes(old) = 0;
     }
}

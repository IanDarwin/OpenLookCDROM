#if !defined(lint) && !defined(__clipper__)
static char *rcsid = "new.c,v 1.2 1994/05/27 06:21:18 me Exp";
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
 * Allocation routines for trie parser.
 *
 * Author: Terry Jones and Jordan K. Hubbard
 * Date: June 1st, 1990.
 * Description: Safely allocate data for various trie structures, possibly
 *		keeping tabs on what's allocated.
 *
 * Revision History:
 *
 * new.c,v
 * Revision 1.2  1994/05/27  06:21:18  me
 * New copyright message
 *
 * Revision 1.1.1.1  1994/05/22  11:22:45  me
 * Initial import into CVS
 *
 * Revision 2.7  92/05/16  06:18:49  jkh
 * Checkin cleanup.
 * 
 * Revision 2.6  92/02/26  11:42:10  me
 * Steve Crooks' clix port and general code cleanup
 * 
 * Revision 2.4  90/10/12  12:48:41  jkh
 * Changed #includes for Term widget.
 * 
 * Revision 2.0  90/04/25  13:40:08  terry
 * Changed new_rparse to actually return an Rparse *...
 */

#include "TermP.h"

#if defined(TRIE_TESTING) || defined(TRACK_MALLOC)
Export int trans_count = 0;
Export int state_count = 0;
Export int iop_count  = 0;
Export int req_count  = 0;
Export int rparse_count = 0;
Export int possible_state_count = 0;
#endif

/*
 * new_transition()
 *
 * Return a pointer to a new Transition struct.
 */
Export Inline Transition *
new_transition()
{
    Transition *new = Fresh(Transition);

#if defined(TRIE_TESTING) || defined(TRACK_MALLOC)
    new->id = trans_count++;
    debug("new: transition_count now %d", trans_count);
#endif
    return new;
}

/*
 * new_state()
 *
 * Return a pointer to a new State struct.
 */
Export Inline State *
new_state()
{
    State *new = Fresh(State);

#if defined(TRIE_TESTING) || defined(TRACK_MALLOC)
    new->id = state_count++;
    debug("new: state_count now %d", state_count);
#endif
    return new;
}

/*
 * new_iop()
 *
 * Return a pointer to a new Iop struct. Since we initialize all fields
 * anyway, we don't need to bzero() it.
 *
 */
Export Inline Iop *
new_iop(t, v)
int t, v;
{
    Iop *new = XtNew(Iop);

#if defined(TRIE_TESTING) || defined(TRACK_MALLOC)
    iop_count++;
    debug("new: iop_count now %d", iop_count);
#endif
    new->type = t;
    new->value = v;
    new->next = NULL;
#ifdef IOP_DEBUG
    new->break = 0;
#endif
    return new;
}

/*
 * new_request()
 *
 * Return a pointer to a new Request struct.
 */
Export Inline Request *
new_request()
{
    Request *new = Fresh(Request);

#if defined(TRIE_TESTING) || defined(TRACK_MALLOC)
    req_count++;
    debug("new: request_count now %d", req_count);
#endif
    return new;
}

/*
 * new_rparse()
 *
 * Return a pointer to a new Rparse struct.
 */
Export Inline Rparse *
new_rparse()
{
    Rparse *new = Fresh(Rparse);

#if defined(TRIE_TESTING) || defined(TRACK_MALLOC)
    rparse_count++;
    debug("new: rparse_count now %d", rparse_count);
#endif
    return new;
}

/*
 * new_possible_state()
 *
 * Return a pointer to a new Possible_state struct.
 */
Export Inline Possible_state *
new_possible_state()
{
    Possible_state *new = Fresh(Possible_state);

#if defined(TRIE_TESTING) || defined(TRACK_MALLOC)
    possible_state_count++;
    debug("new: possible_state_count now %d", possible_state_count);
#endif
    return new;
}

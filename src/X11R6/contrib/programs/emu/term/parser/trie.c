#if !defined(lint) && !defined(__clipper__)
static char *rcsid = "trie.c,v 1.2 1994/05/27 06:21:27 me Exp";
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
 * The reTRIEval parser.
 *
 * Author: Terry Jones and Jordan K. Hubbard
 * Date: March 14th, 1990.
 * Description: This file contains the code for a dynamic escape sequence
 *		parser. Refer to the README file for details as it is far
 *		too complicated to document here.
 *
 * Revision History:
 *
 * trie.c,v
 * Revision 1.2  1994/05/27  06:21:27  me
 * New copyright message
 *
 * Revision 1.1.1.1  1994/05/22  11:22:45  me
 * Initial import into CVS
 *
 * Revision 2.24  92/02/26  11:42:16  me
 * Steve Crooks' clix port and general code cleanup
 * 
 * Revision 2.22  1991/08/09  16:17:10  jkh
 * Added iop stuff for setting icons.
 *
 * Revision 2.20  91/07/26  17:04:46  jkh
 * Added code to allow duplicate sequences.
 * 
 * Revision 2.18  90/11/13  15:00:32  jkh
 * Double descriptor version.
 * 
 * Revision 2.17  90/10/12  12:54:58  jkh
 * Be polite to new widget.
 * 
 * Revision 2.16  90/08/18  13:03:18  terry
 * Fixed a problem I created in handle_as_if - separated the handling of
 * an exact transition match with that of READ_CHAR_TRAN since each must
 * create a new possible state.
 * 
 * Revision 2.15  90/08/18  04:14:22  terry
 * Added META_READ_CHAR which is a new state that will accept any char
 * and generate an iop to put it on the stack in parsing. Specified by
 * %c in the escape sequence. Removed the need_sep and need_normal shit
 * from add_string() with two consequnces: 
 *    1) %s%s%d etc is now legal (but will be parsed slowly so should be
 *       avoided - and it will never occur anyway).
 *    2) an escape sequence (any one) is no longer forced to end with a 
 *       normal character (i.e. a character that must be present in the
 *       input). For example, E[%dJpx is now legal. This was done so the 
 *       new %c specifier could be followed by an action (i.e. putting the
 *       char value in a register), but I made it general and now anyone
 *       can do it.
 * 
 * Optimised add_transition and has_transition to store transition arcs
 * in increasing order so has_transition's average search time is reduced.
 */

#include "TermP.h"
#include <errno.h>

Local int handle(TermWidget, char);
Local Request *add_request(int, Request *);
Local Request *get_request(String, char **, char **);
Local Request *get_request_list(String);
Local State *add_transition(TermWidget, State *, int, IopChain *);
Local Transition *has_transition(State *, int);
Local int handle_as_if(TermWidget, Possible_state *, int);
Local int same_requests(Request *, Request *);
Local void handle_init(TermWidget);
Local void dispatch_requests(TermWidget, Request *);
Local Possible_state *clean_state_list(Possible_state *);
Local char *read_int(String, int *);

/*
 * If we don't need a generic parser, just stub out the parse_init()
 * and parseGeneric() routines (actually, parseGeneric() becomes a
 * fall-through insert routine).
 */
#ifdef NO_GENERIC_PARSER

Export void
parse_init(TermWidget w)
{}

Export void
parseGeneric(TermWidget w, caddr_t client_data, caddr_t call_data)
{
     int nread;
     String buf;
     ComBlock tmp;

     nread = ((XpParseDescriptor *)call_data)->byte_count;
     buf = ((XpParseDescriptor *)call_data)->buffer;

     /* Update fields to reflect total insert */
     ((XpParseDescriptor *)call_data)->byte_count = 0;
     w->term.chars_to_parse = 0;

     if (nread >= BUF_SZ)
	  fatal("Write size > BUF_SZ is illegal.");
     else {
	  strncpy(cb_buffer(&tmp), buf, nread);
	  cb_nbytes(&tmp) = nread;
	  cb_opcode(&tmp) = OP_INSERT;
	  dispatch(w, &tmp);
     }
}

#else	/* !NO_GENERIC_PARSER */

#ifdef TRACK_MALLOC
static int handle_read_str_count = 0;
#endif

Local void add_string(TermWidget, String, Generic, int);
Local void reverse_add(TermWidget, String, Request *);
Local Inline void iopl_incr(Possible_state *, Iop *, Boolean);
Local Inline void iopl_copy(Possible_state *, Possible_state *);

/*
 * parse_init(w)
 *
 * Initialise (or re-initialise) the state of the widget's parser.
 */
Export void
parse_init(TermWidget w)
{
     if (w->term.rparse_list) {
	  destroy_rparse_list(w->term.rparse_list);
	  w->term.rparse_list = NULL;
     }

     if (w->term.zero_state)
	  destroy_state(w->term.zero_state);
     w->term.zero_state = new_state();

     /* Clear things to copacetic values */
     cb_nbytes(w->term.cb) = 0;
     w->term.chars_to_parse = 0;
     w->term.save_pos = 0;
     w->term.rparse_list = NULL;
     handle_init(w);
}

/*
 * handle_init(w)
 *
 * Initialise the state of the thing that actually does the parsing.
 * Set the stack pointer to -1 here so we clear the stack every time 
 * we reset the handler. This makes sense because if anything is left
 * on the stack we wont want it since something went wrong to get us here.
 */
Local void
handle_init(TermWidget w)
{
     register Possible_state *tmp;

     w->term.current_states = new_possible_state();
     tmp = w->term.current_states;
     tmp->state = w->term.zero_state;
     tmp->handle_state = HANDLE_NORMAL;
     return;
}

/*
 * parseGeneric(w, parse_desc)
 *
 * A dynamically configurable parser that understands how to walk TRIE
 * parser descriptions (already read in by parser_install() if we're
 * being called).
 *
 * We call handle() to take care of each character, look at what it
 * returns, and act accordingly. If it tells us that the parse of a
 * full sequence has been done, then we reset our save buffer. If it
 * tells us that the parse of a sequence is in progress, we save the char
 * in case handle later cannot figure out what to do (and then we
 * flush all the chars in the save buffer). If we run out of save room,
 * we flush the buffer ourselves and reset the handler.
 */

/*ARGSUSED*/
Export void
parseGeneric(TermWidget w, caddr_t client_data, caddr_t call_data)
{
     register int i, nread;
     register String buf;
     Boolean did_last;

     nread = w->term.chars_to_parse;
     buf = (String)call_data;

     for (i = 0; i < nread; i++) {
	  /* Update widget hint field */
	  w->term.chars_to_parse--;

	  /* See what we can do with it */
	  switch (handle(w, buf[i])) {
	     case RECOGNISED:
	       /* The handler has recognised it all. */
	       w->term.save_pos = 0;
	       break;
	       
	     case IN_PROGRESS:
	       /*
		* The handler took the char but it wasn't the end of
		* a sequence.
		*/
	       if (w->term.save_pos == BUF_SZ) {
		    ComBlock tmp;
		    
		    /*
		     * We can't save any more. Get rid of it all as a
		     * self-inserts.
		     */
		    strncpy((char *)cb_buffer(&tmp), w->term.save_buf, BUF_SZ);
		    cb_nbytes(&tmp) = BUF_SZ;
		    cb_opcode(&tmp) = OP_INSERT;
		    dispatch(w, &tmp);
		    w->term.save_pos = 0;
		    handle_init(w);
	       }
	       else
		    w->term.save_buf[w->term.save_pos++] = buf[i];
	       break;
	       
	     case FLUSH:
	       /* 
		* The handler is in trouble, flush the previous
		* characters as a self-insert. 
		* Try to put this character into the save buffer so we
		* only have to call dispatch() once. If that doesn't
		* work though, we do it separately *after* sending off
		* the save buffer.
		*/
	       if (w->term.save_pos != BUF_SZ) {
		    /* tack on the last seen character. */
		    w->term.save_buf[w->term.save_pos++] = buf[i];
		    did_last = TRUE;
	       }
	       else
		    did_last = FALSE;
	       /* save old buffer contents */
	       {
		    ComBlock tmp;
		    
		    strncpy((char *)cb_buffer(&tmp), w->term.save_buf,
			    w->term.save_pos);
		    cb_nbytes(&tmp) = w->term.save_pos;
		    cb_opcode(&tmp) = OP_INSERT;
		    dispatch(w, &tmp);
		    if (!did_last) {
			 /*
			  * We were up against the wall for storage
			  * and couldn't do the last char. Do it now.
			  */
			 cb_buffer(&tmp)[0] = buf[i];
			 cb_nbytes(&tmp) = 1;
			 dispatch(w, &tmp);
		    }
		    w->term.save_pos = 0;
	       }
	       /* 
		* We don't have to call handle_init() since the handler
		* told *us* it was in trouble and so should have reset
		* itself.
		*/
	       break;
	       
	       default:
	       /* Huh? */
	       fatal("handler returns unrecognised value.");
	  }
     }
     return;
}

/*
 * handle(w, ch)
 *
 * Run through the list of current possible states and call handle_as_if()
 * on each. This adds new states to our list, etc. If we can get away with
 * not calling handle_as_if() (in the cases where the character will instantly
 * "fall out" of the trie), we also do that instead.
 *
 * We return RECOGNISED if handle_as_if gets into a final state from
 * one of the states in the current_states, FLUSH if we run out
 * of possible states we could be in, and IN_PROGRESS otherwise.
 *
 */
Local int
handle(TermWidget w, char ch)
{
     Possible_state *state = w->term.current_states;

     /*
      * Try a quick and dirty optimization first:
      *
      * If we've just begun accumulating (I.E. there's only one possible
      * state; the zero state), and there's no way to get more states added
      * for the current character (no transitions), take a shortcut and
      * leave the parser alone.
      */
     if (!state->next && state->state == w->term.zero_state &&
	 !has_transition(state->state, ch))
	  return FLUSH;

     while (state) {
	  switch (handle_as_if(w, state, ch)) {
	  case HANDLE_READ_INT:
	  case HANDLE_READ_STR:
	       /* Go on to the next state, we don't have to do anything. */
	       break;
	  
	  case DELETE_STATE:
	       /*
		* Mark this state as finished with, though first make sure
		* to free the temporary storage if it used to be a string
		* state that failed.
		*/
	       if (state->handle_state == HANDLE_READ_STR) {
		    XtFree(state->value);
#ifdef TRACK_MALLOC
		    debug("Free'd READ_STR value %d", --handle_read_str_count);
#endif
	       }
	       state->handle_state = DELETE_STATE;
	       break;
	  
	  case RECOGNISED:
	       /* 
		* Aha! handle_as_if will have taken care of the iop 
		* stuff and also dispatched the requests.
		*/
	       destroy_current_states(w);
	       handle_init(w);
	       return RECOGNISED;
	  }
	  state = state->next;
     }
     if (w->term.current_states = clean_state_list(w->term.current_states))
	  return IN_PROGRESS;
     else
	  handle_init(w);
     return FLUSH;
}

/*
 * handle_as_if(w, state, ch)
 *
 * Handle the character 'ch' as though we were in the possible state 'state'.
 * If we come to the end of a sequence, return RECOGNISED.
 * If everything appears ok, return IN_PROGRESS. If we don't figure out what
 * to do with the character, re-initialise ourselves and return FLUSH. The
 * calling routine (parse) will then send the characters since we last
 * returned RECOGNISED as self-insert characters.
 *
 * When we detect that the transition to follow is ambiguous, we add the 
 * new states to the *FRONT* (or else we hose handle()), of the current
 * state list. This means that those new states will be processed next time
 * around by handle.
 *
 */

Local int
handle_as_if(TermWidget w, Possible_state *state, int ch)
{
     Transition *this_trans;

     switch (state->handle_state) {

     case HANDLE_READ_INT:
	  if (isdigit(ch)) {
	       state->value = (Generic)((int)(state->value) * 10 + ch - '0');
	       return HANDLE_READ_INT;
	  }

	  if (state->aux)	/* aux is sign flag for READ_INT */
	       state->value = (Generic)((int)(state->value) * -1);

	  iopl_incr(state, state->trans->iop.head, FALSE);
	  iopl_incr(state, new_iop(META_INT_PUSH, state->value), TRUE);
	  
	  if (state->trans->state->final) {
	       /* Concatenate terminating iops (if there are any). */
	       if (state->trans->state->transition_list) {
		    iopl_incr(state, state->trans->state->transition_list->iop.head, FALSE);
	       }
	       handle_iops(w, state->iopl, w->term.cb, state->niopl);
	       dispatch_requests(w, state->trans->state->request_list);
	       return RECOGNISED;
	  }
	  
	  state->state = state->trans->state;
	  state->handle_state = HANDLE_NORMAL;
	  state->trans = NULL;
	  state->value = 0;
	  
	  /*
	   * We call ourselves to handle this character. This can
	   * only return RECOGNISED or DELETE since things like
	   * %d%d and %d%s are illegal.
	   */
	  if (handle_as_if(w, state, ch) == RECOGNISED)
	       return RECOGNISED;

	  /* This state is no longer needed */
	  return DELETE_STATE;
	  
     case HANDLE_READ_STR:
	  /* Have we hit the maximum for a string sequence? */
	  if (state->aux == MAX_STR_SEQ) {
	       state->aux = 0;
	       return FLUSH;
	  }
	  else
	       state->value[state->aux++] = ch;

	  if ((this_trans = has_transition(state->trans->state, ch))) {
	       /* Add a new state, but leave this one intact too. */
	       Possible_state *new = new_possible_state();

	       /* transfer the iop list intact */
	       iopl_copy(state, new);

	       /* properly terminate str */
	       state->value[state->aux - 1] = '\0';
#ifdef TRACK_MALLOC
	       debug("Free'd READ_STR value %d", --handle_read_str_count);
#endif
	       iopl_incr(new, new_iop(META_STR_PUSH, state->value), TRUE);
	       iopl_incr(new, this_trans->iop.head, FALSE);
	       new->state = this_trans->state;
	       new->trans = NULL;
	       new->value = 0;
	       new->handle_state = HANDLE_NORMAL;
	       new->aux = 0;
	       new->next = w->term.current_states;
	       w->term.current_states = new;
	       if (this_trans->state->final) {
		   /* Concatenate terminating iops (if there are any). */
		   if (this_trans->state->transition_list) {
			iopl_incr(new, this_trans->state->transition_list->iop.head, FALSE);
		   }
		   handle_iops(w, new->iopl, w->term.cb, new->niopl);
		   dispatch_requests(w, this_trans->state->request_list);
		   /*
		    * Tweak the state so that we don't acciently free its
		    * string storage later in destroy_possible_states()
		    */
		   state->handle_state = HANDLE_NORMAL;
		   return RECOGNISED;
	       }
	  }
	  /* Just proceed in the same state. */
	  return HANDLE_READ_STR;

     case HANDLE_NORMAL: 
	  /*
	   * We check to see which outgoing transitions from this
	   * state we might follow. Under normal circumstances there will
	   * be only the one, but it is possible for there to be two, three or
           * even four. This can happen for instance if we have a '3' character
	   * and this state has outgoing transitions labelled with '3',
	   * READ_INT, READ_STR and READ_CHAR_TRAN. We don't know which to take
	   * and so we add the new states to the front of the current state
	   * list. That way, next around in handle, we will call handle_as_if
	   * we each of the possible states we might have wound up in.
	   *
	   * In each case we check to see if we have wound up in a final 
	   * state, and if so we execute the iops associated with this
           * escape sequence, call dispatch_requests() and then return 
	   * RECOGNISED.
	   *
	   * Despite the fact that the code for the exact match and for
	   * READ_CHAR_TRAN are similar (the else clause is identical)
	   * we handle them separately since each must result in a new
	   * possible state being created and added to the list.
	   */

	  /* There is a transition labeled with the character on input. */
	  if ((this_trans = has_transition(state->state, ch))) {
	       iopl_incr(state, this_trans->iop.head, FALSE);
	       if (this_trans->state->final) {
		   /* Concatenate terminating iops (if there are any). */
		   if (this_trans->state->transition_list) {
			iopl_incr(state,
				  this_trans->state->transition_list->iop.head,
				  FALSE);
		   }
		   handle_iops(w, state->iopl, w->term.cb, state->niopl);
		   dispatch_requests(w, this_trans->state->request_list);
		   return RECOGNISED;
	       }
	       else {
		    /* Add a new state to the front of the list. */
		    Possible_state *new;

		    new = new_possible_state();
		    new->state = this_trans->state;
		    new->trans = NULL;
		    new->value = 0;
		    new->handle_state = HANDLE_NORMAL;
		    iopl_copy(state, new);
		    new->aux = 0;
		    new->next = w->term.current_states;
		    w->term.current_states = new;
	       }
	  }

	  /* There is a transition labeled with READ_CHAR_TRAN. */
	  if ((this_trans = has_transition(state->state, READ_CHAR_TRAN))) {
	       /* Add an iop to push this char's value. */
	       iopl_incr(state, new_iop(META_CHAR_PUSH, ch), TRUE);
	       iopl_incr(state, this_trans->iop.head, FALSE);
	       if (this_trans->state->final) {
		   /* Concatenate terminating iops (if there are any). */
		   if (this_trans->state->transition_list) {
			iopl_incr(state,
				  this_trans->state->transition_list->iop.head,
				  FALSE);
		   }
		   handle_iops(w, state->iopl, w->term.cb, state->niopl);
		   dispatch_requests(w, this_trans->state->request_list);
		   return RECOGNISED;
	       }
	       else {
		    /* Add a new state to the front of the list. */
		    Possible_state *new;

		    new = new_possible_state();
		    new->state = this_trans->state;
		    new->trans = NULL;
		    new->value = 0;
		    new->handle_state = HANDLE_NORMAL;
		    iopl_copy(state, new);
		    new->aux = 0;
		    new->next = w->term.current_states;
		    w->term.current_states = new;
	       }
	  }

	  /* This is a digit or +/- and there is a transition labeled 
	     with READ_INT_TRAN. */
	  if ((isdigit(ch) || ch == '-' || ch == '+') &&
	      (this_trans = has_transition(state->state, READ_INT_TRAN))) {
	       Possible_state *new = new_possible_state();

	       new->state = state->state;
	       new->trans = this_trans;
	       new->handle_state = HANDLE_READ_INT;
	       iopl_copy(state, new);
	       new->next = w->term.current_states;
	       w->term.current_states = new;

	       new->value = 0;
	       new->aux = 0;
	       if (ch == '-')
		    new->aux = 1;

	       else if (ch != '+') {
		    new->value = (Generic)(ch - '0');
		    new->aux = 0;
	       }
	  }

	  /* There is a transition labeled with READ_STR_TRAN. */
	  if ((this_trans = has_transition(state->state, READ_STR_TRAN))) {
	       Possible_state *new = new_possible_state();

	       new->state = state->state;
	       new->trans = this_trans;
	       new->handle_state = HANDLE_READ_STR;
	       iopl_copy(state, new);
	       new->value = XtMalloc(MAX_STR_SEQ);
#ifdef TRACK_MALLOC
		    debug("Malloc READ_STR value %d", ++handle_read_str_count);
#endif
	       new->value[0] = ch;
	       new->aux = 1;
	       new->next = w->term.current_states;
	       w->term.current_states = new;
	  }

	  /* The current possible state is no longer necessary */
	  return DELETE_STATE;

     default:
	  fatal("handle_as_if() in unknown state (%d).", state->handle_state);
     }

     /* NOTREACHED */
     fatal("handle() fell off the end of the world! aaaaahhhh.");
     return FLUSH;   /* Keep gcc quiet.... :-(  */
}

/*
 * rparse(w, cb)
 *
 * Reverse "parse" the string in s and send the result to stdout (if testing)
 * or to the widget's output fd.
 */
Export void
rparse(TermWidget w, ComBlockPtr cb)
{
     Rparse *r;
     register int id;
#ifdef TRIE_TESTING
     int fd = 1;
#else
     int fd = w->term.master;
#endif
     id = cb_opcode(cb);

     if (id < 0)
	  fatal("rparse called with illegal opcode (%d).", id);

     /* Special case for insert op */
     if (id == OP_INSERT)
	  write(fd, cb_buffer(cb), cb_nbytes(cb));
     else {
	  if ((r = rparse_find(w, id))) {
	       static Iop *joe[2];

	       joe[0] = r->ops.head;
	       joe[1] = NULL;
	       handle_iops(w, joe, cb, 1);
	  }
     }
}

/*
 * Do the actual work of adding sequences to the trie or rparse
 * list.
 */
Local void
do_add(TermWidget w, String str, int type)
{
     char *new;
     Request *request_list;
     
     while ((request_list = get_request(str, &str, &new))) {
	  /* 
	   * str now points to the start of the sequence, (after the '<') and
	   * is NUL terminated. new now points to the first char after the '>'
	   * We check to make sure all the request IDs were of the same sign,
	   * and then add or subtract the list according to what we find in
	   * what.
	   */
	  
	  /* If type is TRIE, act on the parser. */
	  if (type == TRIE_T) {
	       add_string(w, str, (Generic)request_list, FALSE);

	       /* Don't destroy the requests list - some state is using it! */
	  }
	  /* Otherwise, act on the reverse parser. */
	  else if (type == RPARSE_T) {
	       reverse_add(w, str, request_list);

	       /* Destroy the request list, no-one is using it now. */
	       destroy_requests(request_list);
	  }
	  else
	       fatal("Bogus type to do_add()\n");
	  str = new;
     }
     return;
}

/*
 * rparse_add(w, s)
 *
 * Add command sequences in 's' to the widget's rparse list.
 */
Export void
rparse_add(TermWidget w, String s)
{
     do_add(w, s, RPARSE_T);
}

/*
 * parse_add(w, s)
 *
 * Add input sequences in 's' to or from the widget's trie.
 */
Export void
parse_add(TermWidget w, String s)
{
     do_add(w, s, TRIE_T);
}

/*
 * add_transition(w, state, label, iopc)
 *
 * Add an outgoing arc labeled 'label' to the state 'state' with
 * the iop action list 'iopc'.
 * If the arc already exists then we just return whatever state
 * it points to. 
 * If not, we create a new transition and a new
 * state for it to point to, and fill in the blanks. Then return
 * the new state. The transition list is built up in order of
 * increasing labels. So the first transition in the list is the 
 * one with the lowest label number. This facilitates faster average
 * case linear searching in has_transition.
 *
 * We have an error condition if the state is a final state, since
 * then the string we are adding has a prefix which is another string.
 * This is not true if the transition is an EPSILON_TRAN since then 
 * we are just adding some terminating actions and will never actually
 * follow the transition to another state. In this case we don't even
 * allocate a further state, just make the transition's state pointer
 * be NULL.
 */
Local State *
add_transition(TermWidget w, State *state, int label, IopChain *iopc)
{
     Transition *t;
     Transition *prev = NULL;

     /* The first ever transition, create the zero_state. */
     if (!state)
	  state = w->term.zero_state = new_state();
     /* Error! (one is a prefix of another). */
     else if (label != EPSILON_TRAN && state->final)
	  fatal("found an escape sequence which is the prefix of another.");
     
     t = state->transition_list;
     
     /* Walk the transition list and see if the transition exists. */
     while (t) {
	  /*
	   * If the existing arc and the planned arc have iop lists,
	   * we just have hope they're identical (or we're in trouble).
	   */
	  if (t->label == label)
	       return t->state;
	  else {
	       if (t->label > label)
		    break;
	       prev = t;
	  }
	  t = t->next;
     }
     
     /* Create the new transition. */
     t = new_transition();
     
     /* 
      * Insert the new one, either after 'prev' or at the front if there 
      * is no previous one.
      */
     if (prev) {
	  t->next = prev->next;
	  prev->next = t;
     }
     else {
	  t->next = state->transition_list;
	  state->transition_list = t;
     }

     /* Now fill in the blanks in the new transition. */
     t->label = label;
     t->iop.head = iopc->head;
     t->iop.tail = iopc->tail;
     
     /* Create and point at the next state if we're not an EPSILON_TRAN */
     if (label != EPSILON_TRAN)
	 t->state = new_state();
     else
	 t->state = NULL;

     /* 
      * Return the state we arrive in, this is NULL if it was an
      * EPSILON_TRAN.
      */
     return t->state;
}

/*
 * has_transition(state, label)
 *
 * Return a pointer to the transition if the state 'state' has a label 'label'
 * and (Transition *)0 otherwise. The labels are sorted and so we can stop
 * the search (unsuccessfully) if we encounter a label larger than that
 * which we seek.
 *
 * This function is very heavily used and some care has been taken to make
 * it reasonably efficient. See the comments below.
 */
Local Transition *
has_transition(State *state, int label)
{
     register Transition *t;
     
     if (!state)
	  return NULL;
     
     t = state->transition_list;

     /*
      * This loop is written so as to execute the < test first. That's
      * because it is the test that will cause us to get on with the job
      * most often. In any search, we will have a series of tests against
      * items that are smaller than the searched for value, and then a 
      * final test against an item that is either the one we want or which
      * is too big (in either case the search terminates). So we'd be doing
      * extra work if we tested every candidate for equality (or
      * excessiveness) before testing for them being too small. Make sense?
      * [ Sure, Terry! Too bad it didn't work.. -jkh ]
      *
      * Additionally, although t->label is dereferenced twice in the loop,
      * it would be incorrect to firstly assign it to a register variable
      * and then use that in the two comparisons. This is because the two
      * comparisons will only both be done ONCE. Every other time around the
      * loop the continue statement will be hit and so the assignment would
      * simply waste time.
      *
      * [ snide note: Jeeze. Try and get Terry to document some of his insane
      * state machine code and you get 3 lines saying "gee, wish I could
      * document this but I have no time." For an 8 line loop, however, I
      * get 2 paragraphs of explicit details.. Sigh.. I can't win. -jkh ]
      *
      */

     while (t) {
	  /* Not there yet? Keep going. */
	  if (t->label < label) {
	      t = t->next;
	      continue;
	  }
	  
	  /* We found it? */
	  if (t->label == label)
	       return t;
	  
	  /* Too big, quit now. */
	  return NULL;
     }
     
     /* We ran off the end of the list (worst case) */
     return NULL;
}

/*
 * get_request(s, start, end)
 *
 * read the next ID,ID,ID<sequence> string.
 * return a list of the request IDs, set *start to point to
 * the char after the <, set the > char to be a '\0' and *end to
 * point after it.
 *
 */
Local Request *
get_request(String s, char **start, char **end)
{
     register char *left_angle_bracket;
     register char *right_angle_bracket;

#ifdef TRIE_ARG_CHECKING
     if (!s)
	  fatal("get_request called with null string.");
#endif
     
     if (!(left_angle_bracket = index(s, '<')))
	  return NULL;
     
     right_angle_bracket = left_angle_bracket;
     *left_angle_bracket = '\0';
     *start = left_angle_bracket + 1;
     do {
	  ++right_angle_bracket;
	  if (!(right_angle_bracket = index(right_angle_bracket, '>')))
	       return NULL;
     } while ((*(right_angle_bracket - 1) == '\\' &&
	       *(right_angle_bracket - 2) != '\\') ||
	      *(right_angle_bracket - 1) == '%');
     
     *right_angle_bracket = '\0';
     *end = right_angle_bracket + 1;
     
     return get_request_list(s);
}


/*
 * get_request_list(s)
 *
 * Read the request list ID,ID,ID... and form it into
 * a list of Request structs. Use strtok to make things easy.
 * 
 */
Local Request *
get_request_list(String s)
{
     Request *r = NULL;
     char *id;
     
#ifdef TRIE_ARG_CHECKING
     if (!s)
	  fatal("get_request_list called with null string.");
#endif
     
#define SEP_STR " \t\n,"
     id = strtok(s, SEP_STR);
     
     while (id) {
	  int n;
	  id = read_int(id, &n);
	  if (*id != '\0')
	       fatal("found unexpected char '%c' in request ID list", *id);
	  r = add_request(n, r);
	  id = strtok(NULL, SEP_STR);
     }
     
     return r;
}
#undef SEP_STR

/* Maximum iop stack size */
#define IFWD_SZ		64

/*
 *
 * add_string(w, s, data, ioponly)
 *
 * This routine will either:
 *
 * 1. add the NULL terminated string s to the trie. When we get to the end make
 * the state we have reached a final state and tack on the request list in
 * requests.
 *
 * 2. If ioponly is TRUE, simply parse out only the iop sequences (converting
 * normal characters to insertion sequences) and store the resulting chain
 * in *data.
 *
 * Iop action designators fall into two classes:
 *
 *        1) %d, %s, %c and %%
 *        2) all other % actions.
 *
 * The first four (%d, %s, %c and %%) will correspond to some input when we are
 * parsing. The others all call for actions to be taken on the stack and
 * registers. The first four are considered transitions in their own right,
 * the others are attached to a transition and are executed before that
 * transition is followed. 
 *
 * There is, however, one exception to this, and that occurs when a
 * sequence ends with an action list (e.g. E[36;45%cpx). Here the "px"
 * command is done AFTER the %c character (whatever it is) is read. There
 * is no new state to move into, we have recognised the sequence, but the
 * "px" still needs to be done. Here we simply add an outgoing
 * transition, but label the state we are in when the %c is taken as
 * being final. So when we encounter final states in parsing, this is
 * checked for.  The outgoing transition arc is given a label of
 * EPSILON_TRAN (like epsilon transitions in automata theory, it corresponds 
 * to no input).
 */

Local void
add_string(TermWidget w, String s, Generic data, int ioponly)
{
     static IopChain tchain;
     State *state = w->term.zero_state;
     Request *requests = NULL;
     IopChain *chain;
     Iop *itmp;
     Generic gen;
     char *orig = s;	  /* For error messages */

     int numeric_sign = 1;/* For numeric constants */
     int seen_bs = 0;     /* True if we have just seen a backslash. */
     int seen_pc = 0;     /* True if we have just seen a percent sign. */
     int literal = 0;	  /* Literal numeric constant */
     int sticky = 0;      /* Are we in the middle of a {} sequence? */
     Iop *fwd[IFWD_SZ];	  /* For forward branches */
     int curr_fwd = 0;	  /* current branch */

#ifdef TRIE_ARG_CHECKING
     if (!s || !data)
	 fatal("parse_add called with null argument.");
#endif

     /*
      * If we're not doing only IOPS, the data is the current request list
      * for this string. Otherwise, it's a pointer to an iop chain head.
      */
     if (!ioponly) {
	  requests = (Request *)data;
	  chain = &tchain;
     }
     else
	  chain = (IopChain *)data;

     while (*s) {
	  register int reg, loc;

	  if (seen_bs) {
	       /* '>' is the only meta character. */
	       if (*s != '>') {
		   /* for backslashes in escape sequences
		    * to be parse properly */
		    /* This section should probably be changed into a switch
		       statement, but I didn't want to make the code gross with
		       ifdef's */
		    /* "\\" (double backslash) represents single backslash. */
		    if (*s == '\\') {
			 if (ioponly)
			      add_iop(chain, META_CHAR, *s);
		         else {
			      /* I don't know if this is right -- I copied. */
			      state = add_transition(w, state, *s, chain);
			      tchain.head = tchain.tail = NULL;
		         }
		    } else
		    if (ioponly) {
			 add_iop(chain, META_CHAR, '\\');
			 add_iop(chain, META_CHAR, *s);
		    }
		    else {
			 state = add_transition(w, state, '\\', chain);
			 tchain.head = tchain.tail = NULL;
			 state = add_transition(w, state, *s, chain);
		    }
	       }
	       else {
		    if (ioponly)
			 add_iop(chain, META_GT, 0);
		    else {
			 state = add_transition(w, state, '>', chain);
			 tchain.head = tchain.tail = NULL;
		    }
	       }
	       seen_bs = FALSE;
	       ++s;
	       continue;
	  }
	  
	  /* Did we just see a % sign, or are we 'sticky'? */
	  if (seen_pc || sticky) {
	       switch (*s) {
	       case '%':
		    /* Is it a normal percent sign? */
		    if (sticky)
			 fatal("'%s' contains '%%' inside {} brackets", orig);
		    else {
			 if (ioponly)
			      add_iop(chain, META_CHAR, '%');
			 else {
			      state = add_transition(w, state, '%', chain);
			      tchain.head = tchain.tail = NULL;
			 }
		    }
		    break;

		    /* A numeric constant */
	       case '0': case '1': case '2': case '3': case '4':
	       case '5': case '6': case '7': case '8': case '9':
		    /* It's a numeric constant */
		    s = read_int(s, &literal);
		    literal *= numeric_sign;
		    add_iop(chain, META_INT_PUSH, literal);
		    numeric_sign = 1, literal = 0;
		    /*
		     * We decrement here to make up for a later increment
		     * that works in all cases but this one.
		     */
		    --s;
		    break;

		    /* A string constant */
	       case '"':
		    gen = (Generic)++s;
		    while (*s && *s != '"')
			 ++s;
		    if (!*s)
			 fatal("Unterminated string constant: '%s'", gen);
		    else {
			 /* Nuke the closing quote */
			 *s = '\0';
			 add_iop(chain, META_STR_PUSH, gen);
		    }
		    break;

		    /* A character constant */
	       case '\'':
		    if (*(s + 2) != '\'')
			 fatal("Illegal character constant: %s", s);
		    else {
			 add_iop(chain, META_CHAR_PUSH, *(s + 1));
			 s += 2;
		    }
		    break;

	       case META_INT:
		    /* An integer transition or output iop */
		    if (ioponly)
			 add_iop(chain, META_INT, 0);
		    else {
			state = add_transition(w, state, READ_INT_TRAN,
						chain);
			tchain.head = tchain.tail = NULL;
		    }
		    break;

	       case META_STR:
		    /* A string transition or output iop */
		    if (ioponly)
			 add_iop(chain, META_STR, 0);
		    else {
			 state = add_transition(w, state, READ_STR_TRAN,
						chain);
			 tchain.head = tchain.tail = NULL;
		    }
		    break;

	       case META_CHAR:
		    /* A character transition or output iop */
		    if (ioponly) /* only support output for now */
			 add_iop(chain, META_CHAR, 0);
		    else {
			state = add_transition(w, state, READ_CHAR_TRAN, 
					       chain);
			tchain.head = tchain.tail = NULL;
		    }
		    break;

	       case '{':
		    /* turn on sticky mode */
		    if (sticky)
			 fatal("Illegally nested '{' seen");
		    else {
			 sticky = TRUE;
		    }
		    break;

	       case '}':
		    if (!sticky)
			fatal("'}' without matching '{' in '%s'", orig);
		    else
			sticky = FALSE;
		    break;

	       case META_ADD:
		    /* Either addition or start of positive integer */
		    if (isdigit(*(s + 1))) {	/* It's a number */
			 numeric_sign = 1;
			 ++s;
			 continue;
		    }
		    else			/* It's addition */
			 add_iop(chain, META_ADD, 0);
		    break;

	       case META_SUB:
		    /* Either subtraction or start of negative integer */
		    if (isdigit(*(s + 1))) {	/* It's a number */
			 numeric_sign = -1;
			 ++s;
			 continue;
		    }
		    else			/* It's subtraction */
			 add_iop(chain, META_SUB, 0);
		    break;

	       case META_AND:
	       case META_CALL:
	       case META_CAST:
	       case META_COMP:
	       case META_DIV:
	       case META_DROP:
	       case META_DUP:
	       case META_EQ:
	       case META_FREE:
	       case META_GT:
	       case META_IGET:
	       case META_IPOP_TO_REG:
	       case META_LAND:
	       case META_LNOT:
	       case META_LOR:
	       case META_LT:
	       case META_MENU:
	       case META_MOD:
	       case META_MUL:
	       case META_NOT:
	       case META_OR:
	       case META_ROP:
	       case META_SIGNAL:
	       case META_STRLEN:
	       case META_SWAP:
	       case META_TITLE:
	       case META_ICON:
	       case META_TOKEN:
	       case META_UP:
		    add_iop(chain, *s, 0);
		    break;

	       case META_POP_TO_REG:
		    /* Pop into X (X given by next char). */
		    /* Allow lower 128 chars to index register */
		    reg = *(++s) & 0x7f;
		    if (reg < 0 || reg >= CB_NREGS)
			 fatal("Invalid register p'%c'(%d) specified",
			       *s, *s);
		    add_iop(chain, META_POP_TO_REG, reg);
		    break;

	       case META_GET:
		    /* Get from X and push. (X given by next char). */
		    /* Allow lower 128 chars to index register */
		    reg = *(++s) & 0x7f;
		    if (reg < 0 || reg >= CB_NREGS)
			 fatal("Invalid register g'%c'(%d) specified",
			       *s, *s);
		    add_iop(chain, META_GET, reg);
		    break;

	       case META_BUFFER:
		    /* Some buffer operation */
		    ++s;
		    if (*s == 'a' || *s == 's')
			 add_iop(chain, META_BUFFER, *s);
		    else
			 fatal("Illegal buffer op '%c' in '%s'", *s, s);
		    break;

	       case META_GOTO:
		    loc = *(++s);
		    for (itmp = chain->head; itmp; itmp = itmp->next) {
			 if (itmp->type == META_LABEL && itmp->value == loc) {
			      add_iop(chain, META_GOTO, itmp);
			      break;
			 }
		    }
		    /* Didn't find the label, make an undefined goto */
		    if (!itmp)
			 add_iop(chain, META_GOTOU, loc);
		    break;

	       case META_LABEL:
		    loc = *(++s);
		    /*
		     * Here we do two things: 1. Make sure a label by this
		     * name doesn't already exist. 2. Satisfy any forward
		     * gotos that might have been made to us.
		     */
		    for (itmp = chain->head; itmp; itmp = itmp->next) {
			 if (itmp->value == loc && itmp->type == META_LABEL)
			      fatal("Label '%c' specified twice in '%s'",
				    loc, orig);
		    }
		    add_iop(chain, META_LABEL, loc);
		    for (itmp = chain->head; itmp; itmp = itmp->next) {
			 if (itmp->value == loc && itmp->type == META_GOTOU) {
			      itmp->type = META_GOTO;
			      itmp->value = (int)chain->tail;
			 }
		    }
		    break;

	       case META_TTY:
		    loc = *(++s);
		    switch (loc) {
		    case TTY_IGET:
		    case TTY_SGET:
		    case TTY_GET:
		    case TTY_SET:
			 add_iop(chain, META_TTY, loc);
			 break;

		    default:
			 fatal("Bad %%%c selector: %c", *(s - 1), loc);
		    }
		    break;

	       case META_IF:
		    add_iop(chain, META_IF, 0);
		    if (curr_fwd < IFWD_SZ)
			 fwd[curr_fwd++] = chain->tail;
		    else
			 fatal("Nested %%? overflow");
		    break;

	       case META_ELSE:
		    if (!curr_fwd)
			 fatal("%%e without %%? encountered.");
		    /* save the place to patch the %? */
		    itmp = fwd[--curr_fwd];
		    add_iop(chain, META_GOTO, 0);
		    /* put in our own address for the goto */
		    fwd[curr_fwd++] = chain->tail;
		    add_iop(chain, META_NOP, 0);
		    itmp->value = (int)chain->tail;
		    break;

	       case META_ENDIF:
		    if (!curr_fwd)
			 fatal("%%; without %%? encountered.");
		    add_iop(chain, META_NOP, 0);
		    fwd[--curr_fwd]->value = (int)chain->tail;
		    break;

	       case ',':	/* comma */
	       case '\n':	/* newline */
	       case '\t':	/* tab */
	       case ' ':	/* or space are valid iop separators */
		    if (!sticky && *s == ',')
			 fatal("Extraneous separator seen in: '%s'", orig);
		    break;

	       default:
		    /* Unrecognised - complain. */
		    fatal("Non-meta-character '%c' in '%s'", *s, orig);
		    break;
	       }
	       seen_pc = FALSE;
	       ++s;
	       continue;
	  }
	  /* A normal character, or the start of a iop sequence. */
	  if (!sticky) {
	       switch (*s) {
	       case '\\':
		    seen_bs = TRUE;
		    break;
		    
	       case '%':
		    seen_pc = TRUE;
		    break;
		    
	       default:
		    /* A plain old character, add it. */
		    if (ioponly)
			 add_iop(chain, META_CHAR, *s);
		    else {
			 state = add_transition(w, state, *s, chain);
			 tchain.head = tchain.tail = NULL;
		    }
		    break;
	       }
	  }
	  s++;
     }

     /*
      * Do a couple of sanity checks and then add the requests info
      * to the state.
      * Add the request ID list to the state.
      * Make sure the state was not already a final state or
      * the prefix of some other state.
      * If there is any terminating sequence of actions to be done, add
      * an EPSILON_TRAN and attatch them to it.
      */
     
     if (sticky)
	  fatal("add_string: missing '}' in sequence '%s'", s);
     if (!ioponly) {
	  
	  /* Prefix error. */
	  if (state->transition_list)
	       fatal("add_string: sequence '%s' is a prefix of another", orig);
	  
	  /* 
	   * Check for terminating actions and add an EPSILON_TRAN if 
	   * there are any. We ignore the return value of add_transition
	   * since it will be giving us NULL since no new state is created
	   * by it, just a new (special) final transition that can hold 
	   * these last iops.
	   */
	  if (tchain.head) {
	      (void) add_transition(w, state, EPSILON_TRAN, chain);
	      tchain.head = tchain.tail = NULL;
	  }
     
	  /*
	   * If this is already a final state, the request lists are
	   * different, and we're not allowing this sort of thing, complain.
	   */
	  if (state->final) {
	       if (!same_requests(state->request_list, requests) &&
		   !cb_reg_data(w->term.cb, 'z'))
		    fatal("add_string() sequences with differing ID's match.");
	       else {
		    /*
		     * Throw one set away. Below we assign request_list to
		     * state->request_list, so all's well.
		     */
		    destroy_requests(state->request_list);
	       }
	  }
	  state->final = 1;
	  state->request_list = requests;
     }
     if (curr_fwd)
	  warn("%%%c or %%%c IOP without %%%c terminator in sequence '%s",
	       META_IF, META_ELSE, META_ENDIF, s);
     return;
}

/*
 * add_request(r, request_list)
 *
 * Add a new request node to the end of the request list.
 * Return the new list. Watch out for a 0 argument - in which we
 * create the list.
 *
 * Also catch the illegal request OP_INSERT
 */
Local Request *
add_request(int r, Request *request_list)
{
     Request *tmp;
     
     if (!request_list) {
	  request_list = new_request();
	  request_list->request_id = r;
	  request_list->next = NULL;
	  return request_list;
     }
     
     tmp = request_list;
     
     while (tmp->next)
	  tmp = tmp->next;
     
     tmp->next = new_request();
     tmp = tmp->next;
     tmp->request_id = r;
     tmp->next = NULL;
     return request_list;
}

Local void
reverse_add(TermWidget w, String sequence, Request *requests)
{
     /*
      * Add the sequence to the reverse parser for the IDs in requests.
      * The IDs are all negative.
      */
     
#ifdef TRIE_ARG_CHECKING
     if (!requests || !sequence)
	  fatal("reverse_add() called with bad pointer.");
#endif
     
     while (requests) {
	  register int id = requests->request_id;
	  Rparse *r;
	  
	  if (id < 0)
	       fatal("reverse_add() called with illegal request id = %d.", id);
	  
	  if ((r = rparse_find(w, id)) != NULL)
	       fatal("reverse_add(): request #%d already in rparse list", id);
	  else {
	       /* Make a new Rparse and adjust the rparse_list ptr. */
	       r = new_rparse();
	       r->id = id;
	       add_string(w, sequence, (Generic)&(r->ops), TRUE);
	       r->next = w->term.rparse_list;
	       w->term.rparse_list = r;
	  }
	  requests = requests->next;
     }
     return;
}

/*
 * rparse_find(w, x)
 *
 * Walk the rparse_list and try to find a reverse parse ID the same as x.
 *
 */
Export Rparse *
rparse_find(TermWidget w, int x)
{
     Rparse *r;
     
     if (!(r = w->term.rparse_list))
	  return NULL;

     while (r) {
	  if (r->id == x)
	       return r;
	  r = r->next;
     }
     return NULL;
}

/*
 * same_requests(a, b)
 *
 * Return 1 or 0 according to whether the request lists a and b are identical.
 */
Local int
same_requests(Request *a, Request *b)
{
#ifdef TRIE_ARG_CHECKING
     if (!a || !b)
	  fatal("same_requests called with bad pointer.");
#endif
     
     while (a && b) {
	  if (a->request_id != b->request_id)
	       return 0;
	  a = a->next;
	  b = b->next;
     }
     
     return (!a && !b) ? 1 : 0;
}

/*
 * read_int(s, x)
 *
 * Read a (possibly signed) integer starting at s, put the value
 * into *x and return the address of the next char.
 */
Local char *
read_int(String s, int *x)
{
     register int val = 0;
     int negative = 0;
     
     if (!s || !x)
	  fatal("read_int called with NULL argument.");
     
     if (*s == '-') {
	  negative = 1;
	  s++;
     }
     else if (*s == '+')
	  s++;
     
     while (isdigit(*s)) {
	  val = val * 10 + *s - '0';
	  s++;
     }
     
     if (negative)
	  val *= -1;
     
     *x = val;
     return s;
}

Local void
dispatch_requests(TermWidget w, Request *r)
{
    while (r) {
	 cb_opcode(w->term.cb) = r->request_id;
	 dispatch(w, w->term.cb);
	 r = r->next;
    }
}

/*
 * Remove all the states from the list that are marked as DELETE_STATE
 * in their handle_state int.
 */
Local Possible_state *
clean_state_list(Possible_state *list)
{
     Possible_state *head, *next;

     /* get rid of all the ones at the start that are to be deleted. */
     while (list) {
          if (list->handle_state == DELETE_STATE) {
               next = list->next;
               destroy_possible(list);
               list = next;
          }
          else
               break;
     }
     
     if (list == NULL)
	  return NULL;

     head = list;

     while (list->next) {
	  if (list->next->handle_state == DELETE_STATE) {
	       next = list->next->next;
	       destroy_possible(list->next);
	       list->next = next;
	  }
	  else
	       list = list->next;
     }
     return head;
}

/*
 * destroy_current_states(w)
 *
 * Kill off the current_states, including the iop lists that
 * each possible state has. If "all" is FALSE, leave the top
 * state around for re-use.
 */
Export void
destroy_current_states(TermWidget w)
{
     while (w->term.current_states) {
	  Possible_state *next = w->term.current_states->next;

	  if (w->term.current_states->handle_state == HANDLE_READ_STR) {
	       XtFree(w->term.current_states->value);
#ifdef TRACK_MALLOC
	       debug("Free'd READ_STR value %d", --handle_read_str_count);
#endif
          }
	  destroy_possible(w->term.current_states);
	  w->term.current_states = next;
     }
}

/*
 * Append an IOP pointer to the end of an IOP list. We go through all this
 * handwaving to avoid massive run-time duplication of IOP chains which would
 * be even more expensive (I.E. We just keep a list of pointers rather than
 * a copy of the whole list). The flag is to catch the occasional special-case
 * IOP for %d, %s or %c which *is* allocated and must be destroyed. If the
 * corresponding bit is set for a particular iopl array entry, that entry will
 * be free'd.
 */
Local Inline void
iopl_incr(Possible_state *st, Iop *iop, Boolean flag)
{
     if (iop) {
	  st->iopl[st->niopl] = iop;
	  if (flag)
	       bit_set(st->mask, st->niopl);
	  if (++st->niopl == MAX_IOPL)
	       fatal("IOP list overflow! Increase MAX_IOPL and recompile.");
     }
}

/*
 * Transfer an IOP list (and all its various anciallary components) from
 * one possible_state to another.
 */
Local Inline void
iopl_copy(Possible_state *from, Possible_state *to)
{
     register int i, lim;

     lim = to->niopl = from->niopl;
     for (i = 0; i < lim; i++) {
	  if (bit_test(from->mask, i)) {
	       bit_set(to->mask, i);
	       to->iopl[i] = new_iop(from->iopl[i]->type,
				     from->iopl[i]->value);
	  }
	  else
	       to->iopl[i] = from->iopl[i];
     }
}

#endif	/* NO_GENERIC_PARSER */


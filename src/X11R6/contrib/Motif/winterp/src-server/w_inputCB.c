/* -*-C-*-
********************************************************************************
*
* File:         w_inputCB.c
* RCS:          $Header: /users/npm/src/winterp/src-server/RCS/w_inputCB.c,v 2.7 1994/06/06 15:40:58 npm Exp $
* Description:  WINTERP interfaces to XtAppAddInput() and XtRemoveInput()
*		code originally from w_timeouts.c, but highly mutated by now.
* Author:       Niels Mayer
* Created:      Tue Jul  9 00:00:00 1991
* Modified:     Sun Jun  5 14:47:37 1994 (Niels Mayer) npm@indeed
* Language:     C
* Package:      N/A
* Status:       X11r6 contrib release
*
* Copyright (C) 1994, Enterprise Integration Technologies Corp. and Niels Mayer.
* WINTERP 1.15-1.99, Copyright (c) 1993, Niels P. Mayer.
* WINTERP 1.0-1.14, Copyright (c) 1989-1992 Hewlett-Packard Co. and Niels Mayer.
* 
* Permission to use, copy, modify, distribute, and sell this software and its
* documentation for any purpose is hereby granted without fee, provided that
* the above copyright notice appear in all copies and that both that
* copyright notice and this permission notice appear in supporting
* documentation, and that the name of Enterprise Integration Technologies,
* Hewlett-Packard Company, or Niels Mayer not be used in advertising or
* publicity pertaining to distribution of the software without specific,
* written prior permission. Enterprise Integration Technologies, Hewlett-Packard
* Company, and Niels Mayer makes no representations about the suitability of
* this software for any purpose.  It is provided "as is" without express or
* implied warranty.
* 
* ENTERPRISE INTEGRATION TECHNOLOGIES, HEWLETT-PACKARD COMPANY AND NIELS MAYER
* DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL IMPLIED
* WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL ENTERPRISE
* INTEGRATION TECHNOLOGIES, HEWLETT-PACKARD COMPANY OR NIELS MAYER BE LIABLE
* FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER
* RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF
* CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
* CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
*
********************************************************************************
*/
static char rcs_identity[] = "@(#)$Header: /users/npm/src/winterp/src-server/RCS/w_inputCB.c,v 2.7 1994/06/06 15:40:58 npm Exp $";

/*
*------------------------------------------------------------------------------
* See ./winterp/COPYRIGHT for information on contacting the authors.
* Please e-mail comments, modifications, questions, improvements and
* bugfixes to the WINTERP mailing list winterp@netcom.com. Please send 
* mailing list subscribe/unsubscribe notices to winterp-request@netcom.com .
* Post XLISP-specific questions/information to the USENET newsgroup
* comp.lang.lisp.x.
*------------------------------------------------------------------------------
*/

#include <stdio.h>
#include <errno.h>
#include <Xm/Xm.h>		/* Xm/Xm.h only needed for "winterp.h"*/
#include "winterp.h"

extern char *sys_errlist[];	/* part of <errno.h>, but not defined by it. */
extern int sys_nerr;		/* part of <errno.h>, but not defined by it. */
extern int errno;		/* some but not all systems require this */

static LVAL s_FDINPUTCB_OBJ=NIL, s_FDINPUTCB_FILE=NIL, s_FDINPUTCB=NIL,
  s_FDINPUTCB_STRING=NIL, s_FDINPUTCB_USTREAM;

static LVAL k_READ=NIL, k_WRITE=NIL, k_EXCEPT=NIL,
  k_READ_LINE_TO_STRING=NIL, k_READ_SEXP_TO_USTREAM;

#ifdef THE_FOLLOWING_CODE_IS_COMMENTED_OUT
static LVAL k_none=NIL;
#endif /* THE_FOLLOWING_CODE_IS_COMMENTED_OUT */


/******************************************************************************
 * This is called whenever we trace input-callbacks, e.g. (trace 'FDINPUTCB).
 * For inputcbhandlers only, this proc is passed in as the 'trace_enter_proc' 
 * in Wcb_Meta_Callbackproc().
 ******************************************************************************/
static void Wicb_InputCB_Handler_Trace_Proc(tracing, client_data)
     LVAL tracing;		/* SYMBOL */
     LVAL client_data;		/* LVAL(FDINPUTCBOBJ) */
{
  extern int xltrcindent;
  int i;

  /* indent to the current trace level */
  for (i = 0; i < xltrcindent; ++i)
    trcputstr(" ");
  ++xltrcindent;
  /* display the callback call... */
  trcputstr("Entering: ");
  trcprin1(tracing);		/* print "FDINPUTCB" */
  trcputstr(" ");
  trcprin1(client_data);	/* print <FDINPUTCBOBJ> */
  trcputstr(" file = ");
  trcprin1(get_fdinputcb_file(client_data)); /* print <STREAM> */
  sprintf(temptext, " XtInputId = 0x%lx\n",
	  (unsigned long) get_fdinputcb_id(client_data));
  trcputstr(temptext);
}

/******************************************************************************
 * For code specified in XT_ADD_INPUT, Bind FDINPUTCBOBJ the <fdinputcb-object>
 * to locally referrable lexical var FDINPUTCB_OBJ; bind the STREAM to
 * FDINPUTCB_FILE.
 * -- see also xlabind().
 * For inpubcbhandlers only, this proc is passed in as the
 * 'bind_call_data_values_proc' in Wcb_Meta_Callbackproc().
 ******************************************************************************/
static void Wicb_Lexical_Bindings_For_InputCB_Handler(bindings_list,
						     lexical_env,
						     call_data,
						     client_data)
     LVAL bindings_list;	/* == NIL */
     LVAL lexical_env;
     XtPointer call_data;	/* not used == NULL */
     LVAL client_data;		/* LVAL(FDINPUTCBOBJ) */
{
  xlpbind(s_FDINPUTCB_OBJ, client_data, lexical_env);
  xlpbind(s_FDINPUTCB_FILE, get_fdinputcb_file(client_data), lexical_env);
}

/******************************************************************************
 * For code specified in XT_ADD_INPUT, Bind FDINPUTCBOBJ the <fdinputcb-object>
 * to locally referrable lexical var FDINPUTCB_OBJ; bind the STREAM to
 * FDINPUTCB_FILE.
 * -- see also xlabind().
 * For inpubcbhandlers only, this proc is passed in as the
 * 'bind_call_data_values_proc' in Wcb_Meta_Callbackproc().
 ******************************************************************************/
static void Wicb_Lexical_Bindings_For_READ_LINE_TO_STRING_InputCB_Handler(bindings_list,
									  lexical_env,
									  call_data,
									  client_data)
     LVAL bindings_list;	/* == NIL */
     LVAL lexical_env;
     XtPointer call_data;	/* char* -- string rep. the line read, bind to FDINPUTCB_STRING */
     LVAL client_data;		/* LVAL(FDINPUTCBOBJ) */
{
  xlpbind(s_FDINPUTCB_OBJ, client_data, lexical_env);
  xlpbind(s_FDINPUTCB_STRING, cvstring((char*) call_data), lexical_env);
}

/******************************************************************************
 * For code specified in XT_ADD_INPUT, Bind FDINPUTCBOBJ the <fdinputcb-object>
 * to locally referrable lexical var FDINPUTCB_OBJ; bind the STREAM to
 * FDINPUTCB_FILE.
 * -- see also xlabind().
 * For inpubcbhandlers only, this proc is passed in as the
 * 'bind_call_data_values_proc' in Wcb_Meta_Callbackproc().
 ******************************************************************************/
static void Wicb_Lexical_Bindings_For_READ_SEXP_TO_USTREAM_InputCB_Handler(bindings_list,
									   lexical_env,
									   call_data,
									   client_data)
     LVAL bindings_list;	/* == NIL */
     LVAL lexical_env;
     XtPointer call_data;	/* LVAL(USTREAM) bind to FDINPUTCB_USTREAM */
     LVAL client_data;		/* LVAL(FDINPUTCBOBJ) */
{
  xlpbind(s_FDINPUTCB_OBJ, client_data, lexical_env);
  xlpbind(s_FDINPUTCB_USTREAM, call_data, lexical_env);
}


/******************************************************************************
 * This is called indirectly via XtAppAddInput() in
 *   Wicb_Prim_XT_ADD_INPUT().
 *-----------------------------------------------------------------------------
 * typedef void (*XtInputCallbackProc)();
 *     XtPointer  closure,
 *     int*       source,
 *     XtInputId* id,
 ******************************************************************************/
static void Wicb_XtInputCallbackProc(client_data, source, id)
     XtPointer     client_data;	/* LVAL(FDINPUTCBOBJ) */
     int*	   source;	/* not used below */
     XtInputId*    id;		/* not used below */
{
  Wcb_Meta_Callbackproc(get_fdinputcb_closure((LVAL) client_data),
			(LVAL) client_data,
			NULL,	/* call_data -- not used */
			Wicb_Lexical_Bindings_For_InputCB_Handler,
			NULL,
			Wicb_InputCB_Handler_Trace_Proc);
}


/******************************************************************************
 * a wrapper to read(2) which calls xlfail() printing errno information
 * about why the read failed.
 * Used in Wicb_READ_LINE_TO_STRING_XtInputCallbackProc() and
 *         Wicb_READ_SEXP_TO_USTREAM_XtInputCallbackProc()
 ******************************************************************************/
static int Wicb_Read_Else_Xlfail(fildes, buf, nbyte)
     int fildes;
     char *buf;
     unsigned nbyte;
{
  int len;

  if ((len = read(fildes, buf, nbyte)) < 0) {
    if (errno < sys_nerr)
      (void) sprintf(temptext, "Error in read(2): %s", sys_errlist[errno]);
    else
      (void) strcpy(temptext, "Error in read(2): unknown error.");
    xlfail(temptext);
  }
  else
    return (len);
}


/******************************************************************************
 * This is called indirectly via XtAppAddInput() in
 *   Wicb_Prim_XT_ADD_INPUT().
 *-----------------------------------------------------------------------------
 * typedef void (*XtInputCallbackProc)();
 *     XtPointer  closure,
 *     int*       source,
 *     XtInputId* id,
 ******************************************************************************/
static void Wicb_READ_LINE_TO_STRING_XtInputCallbackProc(client_data, source, id)
     XtPointer     client_data;	/* LVAL(FDINPUTCBOBJ) */
     int*	   source;
     XtInputId*    id;		/* not used below */
{
  int nbytes;
  char readbuf[BUFSIZ];
  char linebuf[BUFSIZ];
  char* buf;
  int buf_end_idx;
  int prev_buf_end_idx;
  int prev_buf_idx;
  register int buf_idx;
  register int linebuf_idx;

  buf         = get_fdinputcb_readbuf((LVAL) client_data); /* allocated array[2*BUFSIZ] in XT_ADD_INPUT/:READ_LINE_TO_STRING */
  buf_end_idx = get_fdinputcb_bufendidx((LVAL) client_data); /* get the index to the end of the buffer above */

  nbytes = Wicb_Read_Else_Xlfail(*source, readbuf, BUFSIZ); /* read(2) nbytes characters into readbuf */

  /* debug: readbuf[nbytes]='\000'; fprintf(stdout, "readbuf='%s'\n", readbuf); */

  if ((buf_end_idx + nbytes) >= (2*BUFSIZ))
    xlfail("XT_ADD_INPUT/:READ_LINE_TO_STRING -- input line too long!");

  memcpy(&(buf[buf_end_idx]), readbuf, nbytes);	/* append new input to buf */
  buf_end_idx += nbytes;
  buf[buf_end_idx] = '\000';	/* NULL marks end of buf */
  prev_buf_end_idx = buf_end_idx;

  buf_idx = 0; 
  while (1) {			/* process lines till none left: exits w/ break */
    linebuf_idx = 0;
    prev_buf_idx = buf_idx;

    /* try to copy a line of characters to linebuf, quitting on \000 set above */
    while ((buf[buf_idx] != '\n') && (buf[buf_idx] != '\000'))
      linebuf[linebuf_idx++] = buf[buf_idx++];

    if (buf[buf_idx] == '\n') { /* copy to linebuf stopped at LF */

      linebuf[linebuf_idx] = '\000'; /* replace LF with NULL terminator */

      /* replace potential CR preceding LF with NULL terminator */
      if ((linebuf_idx >= 1) && (linebuf[linebuf_idx - 1] == '\r'))
	linebuf[linebuf_idx - 1] = '\000';

      /* fire the callback w/ a line of input in 'linebuf' */
      Wcb_Meta_Callbackproc(get_fdinputcb_closure((LVAL) client_data),
			    (LVAL) client_data,
			    &(linebuf[0]), /* call_data -- bind to FDINPUTCB_STRING */
			    Wicb_Lexical_Bindings_For_READ_LINE_TO_STRING_InputCB_Handler,
			    NULL,
			    Wicb_InputCB_Handler_Trace_Proc);

      buf_idx++;		/* skip over \n in buf */
      buf_end_idx -= linebuf_idx + 1; /* shorten by len(linebuf)+len(\n) */
    }
    else {			/* copy to linebuf stopped at \000 (end of buf) */
      if (prev_buf_end_idx != buf_end_idx) /* IF characters were transferred from buf to linebuf... */
	memccpy(&(buf[0]), &(buf[prev_buf_idx]), '\000', 2*BUFSIZ); /* ...THEN shiftLeft characters that weren't transferred. */
      /*
       * Buf_end_idx is now really end of buf: on the next call to this proc,
       * new input will append after remaining unprocessed input in buf.
       */
      set_fdinputcb_bufendidx((LVAL) client_data, buf_end_idx);
      break;			/*@@@---EXIT THE WHILE LOOP---@@@*/
    }
  }
}


/* read state enumerations used in Wicb_READ_SEXP_TO_USTREAM_XtInputCallbackProc() */
typedef enum {
  READING_NORMAL, READING_ESCAPE, READING_STRING, READING_STRING_ESCAPED,
  READING_VBAR_DELIM, READING_VBAR_DELIM_ESCAPED, READING_COMMENT_LINE,
  READING_HASH, READING_CMT_BRACE, READING_CMT_BRACE_END
  } WINTERP_READER_STATE;


/******************************************************************************
 *
 ******************************************************************************/
#define WRITE_CHAR_TO_USTREAM()								\
{											\
  new_elt = cons(cvchar(c), NIL);							\
  if (*last_elt != NIL) {	 /* if we've already created the head of the stream */	\
    rplacd(*last_elt, new_elt);	 /* add new_elt to the tail of the list */		\
    *last_elt = new_elt;	 /* increment last_elt pointer */			\
  }											\
  else {			 /* else create the head of the stream */		\
    sethead(*sexp_stream, new_elt);							\
    *last_elt = new_elt;								\
  }											\
}


/******************************************************************************/
void Wicb_Read_Sexp_Proc(client_data, rdbuf, len, paren_count, comment_count, read_state, sexp_stream, last_elt, protect_sexp_proc, process_sexp_proc)
     LVAL	client_data;	/* IN ... client_data==NULL means we funcall *process_sexp_proc() and *protect_sexp_proc() args below */
     char*	rdbuf;		/* IN/OUT */
     int	len;		/* IN */
     int*	paren_count;	/* IN/OUT */
     int*	comment_count;	/* IN/OUT */
     int*	read_state;	/* IN/OUT (enum type WINTERP_READER_STATE) */
     LVAL*	sexp_stream;	/* IN/OUT */
     LVAL*	last_elt;	/* IN/OUT */
     void	(*protect_sexp_proc)( /* LVAL sexp_stream; */ ); /* function pointer -- only used when client_data==NULL */
     void	(*process_sexp_proc)( /* LVAL sexp_stream; */ ); /* function pointer -- only used when client_data==NULL */
{
  int i, j, c;
  LVAL new_elt;

  j = 0;
  do {
    /*
     * when *sexp_stream==NIL, that means that the last call to this procedure had just read an S-expression.
     */
    if (null(*sexp_stream)) {	/* we need to create a new ustream... */
      *sexp_stream = newustream(); /* create it: note - stream obj has ptrs for head and tail*/    
      if (client_data != NULL)
	set_fdinputcb_ustream(client_data, *sexp_stream); /* protect new ustream from GC across calls to Wicb_READ_SEXP_TO_USTREAM_XtInputCallbackProc()
							     as xlsave/xlpop won't work... This works because below we put all FDINPUTCBOBJs onto savedobjs[] */
      else 
	(*protect_sexp_proc)(*sexp_stream);
      *last_elt = NIL;
      *paren_count = -1;	/* special init-value forcing (*paren_count!=0)==TRUE for first time thru for-loop below */
      *read_state = READING_NORMAL;
    }
    /*
     * otherwise, ustream was created in prev call, or loop... continue adding chars
     */

    /* foreach character received, stuff it into an xlisp unnamed stream */
    for (i = j; ((i < len) && (*paren_count != 0)) ; i++) {
      c = rdbuf[i];

      switch (*read_state) {
      case READING_ESCAPE:	/* previously read a \ */
	*read_state = READING_NORMAL;
	WRITE_CHAR_TO_USTREAM();
	break;
      case READING_STRING:	/* "...." */
	switch (c) {
	case '"':
	  *read_state = READING_NORMAL;
	  if (*paren_count == -1) /* at 'top paren level' */
	    *paren_count = 0;	/* THEN those characters are a STRING 'token', call the callback below */
	  break;
	case '\\':
	  *read_state = READING_STRING_ESCAPED;
	  break;
	default:
	  *read_state = READING_STRING;
	  break;
	}
	WRITE_CHAR_TO_USTREAM();
	break;
      case READING_STRING_ESCAPED: /* "...\..." */
	*read_state = READING_STRING;
	WRITE_CHAR_TO_USTREAM();
	break;
      case READING_VBAR_DELIM:	/* |...| */
	switch (c) {
	case '|':
	  *read_state = READING_NORMAL;
	  break;
	case '\\':
	  *read_state = READING_VBAR_DELIM_ESCAPED;
	  break;
	default:
	  *read_state = READING_VBAR_DELIM;
	  break;
	}
	WRITE_CHAR_TO_USTREAM();
	break;
      case READING_VBAR_DELIM_ESCAPED: /* |...\...| */
	*read_state = READING_VBAR_DELIM;
	WRITE_CHAR_TO_USTREAM();
	break;
      case READING_COMMENT_LINE: /* ;...\n */
	/* don't worry about escaped \n in comment */
	if (c == '\n')
	  *read_state = READING_NORMAL;
	/* don't call WRITE_CHAR_TO_USTREAM() -- reader will just ignore anyways... */
	break;
      case READING_HASH:	/* #?.. */
	if (c == '|') {		/* handle comment-brace #|, note if prev state was READING_CMT_BRACE, we handle recursive comment-brace by incrementint *comment_count */
	  (*comment_count)++;
	  *read_state = READING_CMT_BRACE;
	}
	else if (*comment_count == 0) {	/* if previously wasn't READING_CMT_BRACE (in READING_NORMAL mode), then need to start up paren matching if char after # is a '(', and we end up continuing in READING_NORMAL mode... */
	  if (c == '(')
	    *paren_count = (*paren_count == -1) ? 1 : *paren_count + 1;
	  *read_state = READING_NORMAL;
	}
	else			/* otherwise since *comment_count != 0, we know we were reading comment brace */
	  *read_state = READING_CMT_BRACE;
	WRITE_CHAR_TO_USTREAM(); /* must call this since we already "wrote" '#'... */
	break;
      case READING_CMT_BRACE:	/* #|? */
	switch (c) {
	case '#':
	  *read_state = READING_HASH; /* potential recursive comment '...#?', where ? could be '|' */
	  break;
	case '|':
	  *read_state = READING_CMT_BRACE_END; /* potential end of comment brace '...|?' where ? could be '#' */
	  break;
	default:		/* just reading chars inside the comment... */
	  *read_state = READING_CMT_BRACE;
	  break;
	}
	WRITE_CHAR_TO_USTREAM(); /* must call this since we already "wrote" '#'... */
	break;
      case READING_CMT_BRACE_END: /* #|..|? */
	if (c == '#') {
	  (*comment_count)--;
	  if (*comment_count < 0) {
	    fprintf(stderr, "Winterp:Wicb_Read_Sexp_Proc(): Warning -- ignoring unmatched '|#' in comment...\n");
	    *comment_count = 0;
	    *read_state = READING_NORMAL;
	  }
	  else if (*comment_count == 0)
	    *read_state = READING_NORMAL;
	  else
	    *read_state = READING_CMT_BRACE;
	}
	else
	  *read_state = READING_CMT_BRACE;
	WRITE_CHAR_TO_USTREAM(); /* must call this since we already "wrote" '#'... */
	break;
      case READING_NORMAL:
	switch (c) {
	case '(':		/******************** Read a open-paren ********************/
	  *paren_count = (*paren_count == -1) ? 1 : *paren_count + 1;
	  WRITE_CHAR_TO_USTREAM(); /* see macro above */
	  break;
	case ')':		/******************** Read a close-paren ********************/
	  if (*paren_count > 0)
	    (*paren_count)--;
	  else if (*paren_count <= 0)
	    fprintf(stderr, "Winterp:Wicb_Read_Sexp_Proc(): Warning -- ignoring unmatched ')'.\n");
	  WRITE_CHAR_TO_USTREAM(); /* see macro above */
	  break;
	case ' ':		/******************** read whitespace... ********************/
	case '\t':
	case '\r':
	case '\n':
	case '\f':
	case '\v':
	  if (*paren_count == -1) { /* at 'top paren level', and looking-at white-space */
	    if (*last_elt == NIL) /* IF no other characters read */
	      {}		/* THEN don't do anything (skip leading whitespace) */
	    else {
	      *paren_count = 0;	/* ELSE those characters are a 'token', call the callback below */  
	      WRITE_CHAR_TO_USTREAM(); /* see macro above */
	    }
	  }
	  else			/* in nested paren level */
	    WRITE_CHAR_TO_USTREAM(); /* see macro above */
	  break;
	case '\\':		/******************** read escaped char ********************/
	  *read_state = READING_ESCAPE;
	  WRITE_CHAR_TO_USTREAM(); /* see macro above */
	  break;
	case '"':		/******************** read string: "..." ********************/
	  *read_state = READING_STRING;
	  WRITE_CHAR_TO_USTREAM(); /* see macro above */
	  break;
	case ';':		/******************** read comment: ";.....\n" ********************/
	  *read_state = READING_COMMENT_LINE;
	  /* don't call WRITE_CHAR_TO_USTREAM() -- reader will just ignore anyways... */
	  break;
	case '|':		/********************        read "|...|"      ********************/
	  *read_state = READING_VBAR_DELIM;
	  WRITE_CHAR_TO_USTREAM(); /* see macro above */
	  break;
	case '#':		/********************         read "#..."       ********************/
	  *read_state = READING_HASH;
	  WRITE_CHAR_TO_USTREAM(); /* see macro above */
	  break;
	default:		/* read anything else... */
	  WRITE_CHAR_TO_USTREAM(); /* see macro above */
	  break;
	}			/* end: switch (c) */
	break;
      }				/* end: switch (*read_state) */
    }				/* end: for (i = j; ((i < len) && (*paren_count != 0)) ; i++) */

    if (*paren_count == 0) {
      /*
       * Fire callback, with Wicb_Lexical_Bindings_For_READ_SEXP_TO_USTREAM_InputCB_Handler()
       * binding sexp-stream to FDINPUTCB_USTREAM in the callback. Since
       * *paren_count is 0, current sexp-stream holds a sexp... Since we may
       * still have a rdbuf full of characters, we continue thru do() loop
       * creating next sexp-stream and eventually firing callback again...
       */
      if (*last_elt == NIL)
	*sexp_stream = NIL;	/* loop never executed, no characters read. */
      else
	settail(*sexp_stream, *last_elt); /* streams are cdr-coded -- give ptr to tail */

      if (client_data != NULL)
	Wcb_Meta_Callbackproc(get_fdinputcb_closure(client_data),
			      client_data,
			      (XtPointer) *sexp_stream,	/* call_data -- LVAL(USTREAM) bind to FDINPUTCB_USTREAM */
			      Wicb_Lexical_Bindings_For_READ_SEXP_TO_USTREAM_InputCB_Handler,
			      NULL,
			      Wicb_InputCB_Handler_Trace_Proc);
      else
	(*process_sexp_proc)(*sexp_stream);

      *sexp_stream = NIL;
      *last_elt = NIL;
      *paren_count = -1;	/* special init-value forcing (*paren_count!=0)==TRUE for first time thru for-loop below */
      *read_state = READING_NORMAL;
      j = i;
    }
    else
      j = i + 1;
  } while (j <= len);		/* end: do ... */
  

  if (*sexp_stream != NIL)
    settail(*sexp_stream, *last_elt); /* update cdr-coded ustream's tail slot... */
}


/******************************************************************************
 * This is called indirectly via XtAppAddInput() in
 *   Wicb_Prim_XT_ADD_INPUT().
 *-----------------------------------------------------------------------------
 * typedef void (*XtInputCallbackProc)();
 *     XtPointer  closure,
 *     int*       source,
 *     XtInputId* id,
 ******************************************************************************/
static void Wicb_READ_SEXP_TO_USTREAM_XtInputCallbackProc(client_data, source, id)
     XtPointer     client_data;	/* LVAL(FDINPUTCBOBJ) */
     int*	   source;
     XtInputId*    id;		/* not used below */
{
  int   len;
  LVAL  sexp_stream, last_elt;
  int   paren_count, comment_count;
  int   read_state;		/* actually type enum WINTERP_READER_STATE */
  char* rdbuf;

  rdbuf		= get_fdinputcb_readbuf((LVAL) client_data); /* allocated array[BUFSIZ] in XT_ADD_INPUT/:READ_SEXP_TO_USTREAM */
  paren_count	= get_fdinputcb_parencnt((LVAL) client_data); /* get the paren-nesting level from a previous invocation */
  comment_count	= get_fdinputcb_commtcnt((LVAL) client_data); /* get the comment-nesting level from a previous invocation */
  read_state	= get_fdinputcb_readstate((LVAL) client_data); /* get the state of the reader from a previous invocation */
  sexp_stream	= get_fdinputcb_ustream((LVAL) client_data); /* get the value of the partially read sexp from a previous invocation */
  if (sexp_stream != NIL)
    last_elt = gettail(sexp_stream);
  else
    last_elt = NIL;

  len = Wicb_Read_Else_Xlfail(*source, rdbuf, BUFSIZ); /* read(2) len characters into rdbuf */

  Wicb_Read_Sexp_Proc(client_data,
		      rdbuf, len,
		      &paren_count, &comment_count,
		      &read_state,
		      &sexp_stream, &last_elt,
		      NULL, NULL);

  set_fdinputcb_ustream((LVAL) client_data, sexp_stream);
  set_fdinputcb_parencnt((LVAL) client_data, paren_count);
  set_fdinputcb_commtcnt((LVAL) client_data, comment_count);
  set_fdinputcb_readstate((LVAL) client_data, read_state);
}


/*****************************************************************************
 * This primitive takes two forms of arguments:
 *      (XT_ADD_INPUT <file> <condition> <code>) or
 *      (XT_ADD_INPUT <fdinputcb-obj>)
 * returns: <fdinputcb_object>
 *
 * <file> is a XLISP stream (not a u-stream tho).
 *
 * There are three possibilities for <condition>: 
 *  (1) <condition> == {:READ, :WRITE, :EXCEPT}
 *	These correspond to XtAppAddInput() options 
 *	XtInputReadMask, XtInputWriteMask, XtInputExceptMask options:
 *		:READ   -- input-fd is ready to be read. The reader must be a
 *			nonblocking read, e.g. 'read-char', 'read-byte',
 *			'fscanf-string'. WARNING: do not use 'read-line' or
 *			'read' as these will cause WINTERP to deadlock,
 *			requiring you to kill the process!
 *		:WRITE  -- input-fd is ready to be written (this is almost
 *			always true -- there's really no reason to use
 *			XT_ADD_INPUT/:write).
 *		:EXCEPT -- an exception condition has occured on input-fd.
 *  (2) <condition> == :READ_LINE_TO_STRING
 *	In this case, <code> will get called with symbol FDINPUTCB_STRING
 *	bound to a string representing a line of text. Use this to
 *	read text from a stream line-by-line, calling <code> only once per line
 *	read from <file>. Note: this routine will signal an error if
 *	the line being read ends up being longer than 1023 characters.
 *  (3) <condition> == :READ_SEXP_TO_USTREAM
 *	In this case, <code> will get called with symbol FDINPUTCB_USTREAM bound
 *	to an unnamed stream representing the s-expression just read. This option
 *	may be used to read-in parentheses-bounded expressions, calling <code>
 *	only once per read s-expression. Within <code> one can pass the value of 
 *	FDINPUTCB_USTREAM on to 'READ' for parsing, 'GET-OUTPUT-STREAM-STRING' to
 *	return a string representation of the s-expr, etc.
 *
 * <code> is a list of lisp expressions that are evaluated when the fdinputcb
 * occurs. During the fdinputcb, the lexical environment that existed for
 * the call to XT_ADD_INPUT will be used for value and functional bindings.
 * Additionally, the symbol FDINPUTCB_OBJ is bound to the <fdinputcb-obj> that
 * caused the fdinputcb. FDINPUTCB_FILE is bound to <file> argument from the
 * call to XT_ADD_INPUT. Additionally, FDINPUTCB_STRING or FDINPUTCB_USTREAM
 * may get bound if <condition> is :READ_LINE_TO_STRING or
 * :READ_SEXP_TO_USTREAM.
 *
 * The form (XT_ADD_INPUT <fdinputcb-obj>) may be used to more
 * efficiently reschedule previously removed fdinputcbs. Instead of creating a
 * new closure around the same <code> each time a recurrent fdinputcb is
 * rescheduled, this second form for XT_ADD_INPUT allows you to take the
 * <fdinputcb-obj> from a previously removed fdinputcb and reschedule a new
 * fdinputcb using the closure setup by the initial call to
 *		(XT_ADD_INPUT <file> <mask> <code>).
 * During the execution of <code>, the symbol FDINPUTCB_OBJ is bound to 
 * <fdinputcb-obj> so that you don't need to keep around a global variable
 * for each recurrent fdinputcb.
 *
 * The returned <fdinputcb-obj> may be passed into the  functions
 * (XT_REMOVE_INPUT <fdinputcb-obj>), or (XT_ADD_INPUT <fdinputcb-obj>).
 *
 *----------------------------------------------------------------------------
 * extern XtInputId XtAppAddInput();
 *     XtAppContext    app;
 *     int             source;
 *     XtPointer       condition;
 *     XtInputCallbackProc proc;
 *     XtPointer 	closure;
 ****************************************************************************/
#define FDINPUTCB_NORMAL		0
#define FDINPUTCB_READ_LINE_TO_STRING	1
#define FDINPUTCB_READ_SEXP_TO_USTREAM	2
LVAL Wicb_Prim_XT_ADD_INPUT()
{
  extern XtAppContext app_context; /* winterp.c */
  extern LVAL s_lambda, xlenv, xlfenv;
  LVAL fdinputcb_obj=NIL;
  LVAL lval_code, lval_file;
  int fdinputcb_type = FDINPUTCB_NORMAL; /* FDINPUTCB_NORMAL, FDINPUTCB_READ_LINE_TO_STRING, FDINPUTCB_READ_SEXP_TO_USTREAM */
  XtPointer condition;
  FILE* fp;
  
  /* protect some pointers */
  xlsave1(fdinputcb_obj);

  /* get <fdinputcb_obj>, else .... */
  if (moreargs() && fdinputcbobj_p(*xlargv)) {
    fdinputcb_obj = nextarg();
    if (get_fdinputcb_id(fdinputcb_obj) != (XtInputId) NULL)
      xlerror("Attempt to add an already active input source.", fdinputcb_obj);

    /* get <file> ---> fp */
    lval_file      = get_fdinputcb_file(fdinputcb_obj);
    if ((fp = getfile(lval_file)) == NULL)
      xlerror("File not opened.", lval_file);

    /* get <mask> ---> condition */
    condition      = get_fdinputcb_condition(fdinputcb_obj);
    fdinputcb_type = get_fdinputcb_type(fdinputcb_obj);
  }
  else {			/* get <file> <mask> <code> */
    LVAL lval_condition;
    /* get <file> ---> fp */
    lval_file = xlgastream();
    if ((fp = getfile(lval_file)) == NULL)
      xlerror("File not opened.", lval_file);

    /* get <mask> ---> condition, fdinputcb_type */
    lval_condition = xlgasymbol();
    if (lval_condition == k_READ) {
      condition = (XtPointer) XtInputReadMask;
      fdinputcb_type = FDINPUTCB_NORMAL;
    }
    else if (lval_condition == k_EXCEPT) {
      condition = (XtPointer) XtInputExceptMask;
      fdinputcb_type = FDINPUTCB_NORMAL;
    }
    else if (lval_condition == k_WRITE) {
      condition = (XtPointer) XtInputWriteMask;
      fdinputcb_type = FDINPUTCB_NORMAL;
    }
#ifdef THE_FOLLOWING_CODE_IS_COMMENTED_OUT
    else if (lval_condition == k_none) {
      condition = (XtPointer) XtInputNoneMask;
      fdinputcb_type = FDINPUTCB_NORMAL;
    }
#endif /* THE_FOLLOWING_CODE_IS_COMMENTED_OUT */
    else if (lval_condition == k_READ_LINE_TO_STRING) {
      condition = (XtPointer) XtInputReadMask;
      fdinputcb_type = FDINPUTCB_READ_LINE_TO_STRING;
    }
    else if (lval_condition == k_READ_SEXP_TO_USTREAM) {
      condition = (XtPointer) XtInputReadMask;
      fdinputcb_type = FDINPUTCB_READ_SEXP_TO_USTREAM;
    }
    else {
      sprintf(temptext,
	      "Invalid input condition keyword, should be one of [%s, %s, %s %s %s].",
	      getstring(getpname(k_READ)),
	      getstring(getpname(k_WRITE)),
	      getstring(getpname(k_EXCEPT)),
	      getstring(getpname(k_READ_LINE_TO_STRING)),
	      getstring(getpname(k_READ_SEXP_TO_USTREAM)));
      xlerror(temptext, lval_condition);
    }

    /* get <code> */
    lval_code = xlgalist();
  }

  xllastarg();

  /* 
   * create the client_data to be sent to Wicb_XtInputCallbackProc. 
   * That procedure takes the client_data==fdinputcb_obj, extracts the closure,
   * and uses this to execute the fdinputcb callback. We also need to retain
   * the fdinputcb identifier for use in RemoveFdinputcb. For simplicity, we save
   * in lisp object fdinputcb_obj.
   */

  /* if args gave <code>, need to create fdinputcb-obj and closure around <code> */
  if (fdinputcb_obj == NIL) {
    fdinputcb_obj = new_fdinputcbobj();
    /* set_fdinputcb_id(fdinputcb_obj, NULL) */
    set_fdinputcb_readstate(fdinputcb_obj, READING_NORMAL);
    set_fdinputcb_parencnt(fdinputcb_obj, 0);
    set_fdinputcb_commtcnt(fdinputcb_obj, 0);
    set_fdinputcb_readbuf(fdinputcb_obj, (char*) NULL);
    set_fdinputcb_condition(fdinputcb_obj, condition);
    set_fdinputcb_type(fdinputcb_obj, fdinputcb_type);
    set_fdinputcb_ustream(fdinputcb_obj, NIL);
    set_fdinputcb_closure(fdinputcb_obj,
			  xlclose(s_FDINPUTCB, s_lambda, NIL, lval_code, xlenv, xlfenv));
    set_fdinputcb_file(fdinputcb_obj, lval_file);
  }

  switch (fdinputcb_type) {
  case FDINPUTCB_NORMAL:
    set_fdinputcb_id(fdinputcb_obj,
		     XtAppAddInput(app_context,
				   fileno(fp),
				   condition,
				   Wicb_XtInputCallbackProc,
				   (XtPointer) fdinputcb_obj));
    break;
  case FDINPUTCB_READ_LINE_TO_STRING:
    /*
     * This buffer gets XtFree()'d on GC of FDINPUTCBOBJ
     * -- see xlisp/xldmem.c:sweep()
     */
    set_fdinputcb_readbuf(fdinputcb_obj,
			  (char*) XtMalloc((unsigned) (2 * BUFSIZ * sizeof(char)))); 
    set_fdinputcb_id(fdinputcb_obj,
		     XtAppAddInput(app_context,
				   fileno(fp),
				   condition,
				   Wicb_READ_LINE_TO_STRING_XtInputCallbackProc,
				   (XtPointer) fdinputcb_obj));

    /* if rescheduling this input-cb from a previous invocation, reinitialize local state vars */
    set_fdinputcb_bufendidx(fdinputcb_obj, 0);
    break;
  case FDINPUTCB_READ_SEXP_TO_USTREAM:
    /*
     * This buffer gets XtFree()'d on GC of FDINPUTCBOBJ
     * -- see xlisp/xldmem.c:sweep()
     */
    set_fdinputcb_readbuf(fdinputcb_obj,
			  (char*) XtMalloc((unsigned) (BUFSIZ * sizeof(char)))); 
    set_fdinputcb_id(fdinputcb_obj,
		     XtAppAddInput(app_context,
				   fileno(fp),
				   condition,
				   Wicb_READ_SEXP_TO_USTREAM_XtInputCallbackProc,
				   (XtPointer) fdinputcb_obj));

    /* if rescheduling this input-cb from a previous invocation, reinitialize local state vars */
    set_fdinputcb_parencnt(fdinputcb_obj, 0);
    set_fdinputcb_commtcnt(fdinputcb_obj, 0);
    set_fdinputcb_readstate(fdinputcb_obj, READING_NORMAL);
    set_fdinputcb_ustream(fdinputcb_obj, NIL);
    break;
  }

  /*
   * Put fdinputcb_obj in savedobjs so that it gets marked during gc. In that way, we
   * know that the fdinputcb closure (i.e. the callback code, and it's lexical
   * environment) and fdinputcb identifier won't get garbage collected while the
   * fdinputcb-object is "referenced" inside Xt&Motif. The fdinputcb_obj gets
   * removed from savedobjs when XT_REMOVE_INPUT gets called.
   */
  { 
    int  i = Wso_Hash(fdinputcb_obj);
    LVAL l_hbucket;
    
    xlsave1(l_hbucket);
    l_hbucket = cons(fdinputcb_obj, getelement(v_savedobjs, i));
    setelement(v_savedobjs, i, l_hbucket);
    xlpop();
  }

  /* resore the stack */
  xlpop();

  return (fdinputcb_obj);
}


/******************************************************************************
 * extern void XtRemoveInput();
 *      XtInputId id;      
 * 
 * Lisp:   (xt_remove_input <fdinputcb_obj>)
 *         where <fdinputcb_obj> is the value returned by xt_add_input.
 ******************************************************************************/
LVAL Wicb_Prim_XT_REMOVE_INPUT()
{
  extern LVAL true;
  LVAL fdinputcb_obj;
  
  fdinputcb_obj = xlga_fdinputcbobj();
  xllastarg();

  if (get_fdinputcb_id(fdinputcb_obj) == (XtInputId) NULL)
    xlerror("Attempt to remove a previously removed input source.", fdinputcb_obj);
  
  XtRemoveInput(get_fdinputcb_id(fdinputcb_obj));

  /* the fdinputcb-object is no longer active, so indicate that by nulling the fdinputcb-id */
  set_fdinputcb_id(fdinputcb_obj, NULL);

  /* remove <fdinputcb_obj> from v_savedobjs allowing it to be garbage collected */
  {
    int i = Wso_Hash(fdinputcb_obj);
    LVAL l_hbucket = getelement(v_savedobjs, i);
    LVAL l_prev = NIL;

    while ((l_hbucket != NIL) && (car(l_hbucket) != fdinputcb_obj)) {
      l_prev = l_hbucket;
      l_hbucket = cdr(l_hbucket);
    }
    if (l_hbucket == NIL)
      xlerror("Internal error in Wicb_Prim_XT_REMOVE_INPUT -- couldn't remove <fdinputcb-obj> from v_savedobjs. Hash error?",
	      fdinputcb_obj);
    if (l_prev == NIL)		/* first elt matched */
      setelement(v_savedobjs, i, cdr(l_hbucket));
    else
      rplacd(l_prev, cdr(l_hbucket));
  }

  return (true);
}


/******************************************************************************
 * (INPUT_ACTIVE_P <fdinputcb_obj>)
 * 	--> returns T if <fdinputcb_obj> is still scheduled, returns
 *	    NIL if <fdinputcb_obj> input source was inactivated by
 *          XT_REMOVE_INPUT.
 *
 *  <fdinputcb_obj> is the value returned by XT_ADD_INPUT.
 ******************************************************************************/
LVAL Wicb_Prim_INPUT_ACTIVE_P()
{
  extern LVAL true;
  LVAL fdinputcb_obj;
  
  fdinputcb_obj = xlga_fdinputcbobj();
  xllastarg();
  
  return (get_fdinputcb_id(fdinputcb_obj) ? true : NIL);
}


/******************************************************************************
 *
 ******************************************************************************/
Wicb_Init()
{
  s_FDINPUTCB_OBJ	= xlenter("FDINPUTCB_OBJ");
  s_FDINPUTCB_FILE	= xlenter("FDINPUTCB_FILE");
  s_FDINPUTCB		= xlenter("FDINPUTCB");
  s_FDINPUTCB_STRING	= xlenter("FDINPUTCB_STRING");
  s_FDINPUTCB_USTREAM	= xlenter("FDINPUTCB_USTREAM");

#ifdef THE_FOLLOWING_CODE_IS_COMMENTED_OUT
  k_none           = xlenter(":NONE");
#endif /* THE_FOLLOWING_CODE_IS_COMMENTED_OUT */
  k_READ			= xlenter(":READ");
  k_WRITE			= xlenter(":WRITE");
  k_EXCEPT			= xlenter(":EXCEPT"); 
  k_READ_LINE_TO_STRING		= xlenter(":READ_LINE_TO_STRING"); 
  k_READ_SEXP_TO_USTREAM	= xlenter(":READ_SEXP_TO_USTREAM"); 
}

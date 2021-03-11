/* -*-C-*-
********************************************************************************
*
* File:         w_timeout.c
* RCS:          $Header: /users/npm/src/winterp/src-server/RCS/w_timeouts.c,v 2.8 1994/06/06 15:40:53 npm Exp $
* Description:  WINTERP interfaces to XtAppAddTimeOut() and XtRemoveTimeout().
* Author:       Niels Mayer and Bob Leichner (Hewlett-Packard Laboratories).
* Created:      Sat Aug 26 07:44:17 1989
* Modified:     Sun Jun  5 14:52:53 1994 (Niels Mayer) npm@indeed
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
static char rcs_identity[] = "@(#)$Header: /users/npm/src/winterp/src-server/RCS/w_timeouts.c,v 2.8 1994/06/06 15:40:53 npm Exp $";

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
#include <Xm/Xm.h>		/* Xm/Xm.h only needed for "winterp.h"*/
#include "winterp.h"

static LVAL s_TIMEOUT_OBJ=NIL, s_TIMEOUT=NIL;


/******************************************************************************
 * This is called whenever we trace timeouts, e.g. (trace 'XT_TIMEOUT).
 * For timeouthandlers only, this proc is passed in as the 'trace_enter_proc' 
 * in Wcb_Meta_Callbackproc().
 ******************************************************************************/
static void Wto_Timeout_Handler_Trace_Proc(tracing, client_data)
     LVAL tracing;		/* SYMBOL */
     LVAL client_data;		/* XLTYPE_TIMEOUTOBJ */
{
  extern int xltrcindent;
  int i;

  /* indent to the current trace level */
  for (i = 0; i < xltrcindent; ++i)
    trcputstr(" ");
  ++xltrcindent;
  /* display the callback call... */
  trcputstr("Entering: ");
  trcprin1(tracing);		/* print "XT_TIMEOUT" */
  trcputstr(" ");
  trcprin1(client_data);	/* print <TIMEOUTOBJ> */
  trcputstr("\n");
}


/******************************************************************************
 * Bind TIMEOUT_OBJ locally for code specified in XT_ADD_TIMEOUT.
 * -- see also xlabind().
 * For timeouthandlers only, this proc is passed in as the
 * 'bind_call_data_values_proc' in Wcb_Meta_Callbackproc().
 ******************************************************************************/
static void Wto_Lexical_Bindings_For_Timeout_Handler(bindings_list,
						     lexical_env,
						     call_data,
						     client_data)
     LVAL bindings_list;	/* == NIL */
     LVAL lexical_env;
     XtPointer call_data;	/* not used == NULL */
     LVAL client_data;		/* XLTYPE_TIMEOUTOBJ */
{ 
  /* bind the <timeout-object> to locally referrable lexical var TIMEOUT_OBJ */
  xlpbind(s_TIMEOUT_OBJ, client_data, lexical_env);
}


/******************************************************************************
 * This is a timeout procedure that is called by the XtAppAddTimeOut() 
 * interface in this file -- Wto_Prim_XT_ADD_TIMEOUT(). It serves to call the
 * lisp evaluator on the lexical closure that was created by XT_ADD_TIMEOUT.
 * It will bind the symbol TIMEOUT_OBJ to the TIMEOUTOBJ returned by the call
 * to XT_ADD_TIMEOUT that caused the timeout handler to fire.
******************************************************************************/
static void Winterp_TimeoutProc(client_data, id)
     XtPointer     client_data;	/* XLTYPE_TIMEOUTOBJ */
     XtIntervalId* id;		/* not used below */
{
  LVAL timeout_obj;

  xlsave1(timeout_obj);		/* save the timeout obj since it gets removed from v_savedobjs while it is still to be ref'd in this proc (and conses happen in between removal and use. */
  timeout_obj = (LVAL) client_data;

  /* the timeout-object is no longer active, so indicate that by nulling the timeout-id */
  set_timeout_id(timeout_obj, NULL);

  /* remove <timeout_obj> from v_savedobjs allowing it to be garbage collected */
  {
    int i = Wso_Hash(timeout_obj);
    LVAL l_hbucket = getelement(v_savedobjs, i);
    LVAL l_prev = NIL;

    while ((l_hbucket != NIL) && (car(l_hbucket) != timeout_obj)) {
      l_prev = l_hbucket;
      l_hbucket = cdr(l_hbucket);
    }
    if (l_hbucket == NIL)
      xlerror("Internal error in Winterp_TimeoutProc -- couldn't remove <timeout-obj> from v_savedobjs. Hash error?",
	      timeout_obj);
    if (l_prev == NIL)		/* first elt matched */
      setelement(v_savedobjs, i, cdr(l_hbucket));
    else
      rplacd(l_prev, cdr(l_hbucket));
  }

  /*
   * Now evaluate code associated with timeout 
   */
  Wcb_Meta_Callbackproc(get_timeout_closure(timeout_obj),
			timeout_obj,
			NULL,	/* call_data -- not used */
			Wto_Lexical_Bindings_For_Timeout_Handler,
			NULL,
			Wto_Timeout_Handler_Trace_Proc);

  xlpop(/* timeout_obj */);
}


/*****************************************************************************
 * This primitive takes two forms of arguments:
 *      (XT_ADD_TIMEOUT <interval> <code>) or
 *      (XT_ADD_TIMEOUT <interval> <timeout-obj>)
 * returns: <timeout_object>
 *
 * <interval> of the timer in milliseconds.
 *
 * <code> is a list of lisp expressions that are evaluated when the timeout
 * occurs. During the timeout, the lexical environment that existed for
 * the call to Xt_Add_Timeout will be used for value and functional bindings.
 * Additionally, the symbol TIMEOUT_OBJ is bound to the <timeout-obj> that caused
 * the timeout.
 *
 * The form (XT_ADD_TIMEOUT <interval> <timeout-obj>) may be used to more
 * efficiently schedule recurrent timeouts. Instead of creating a new closure
 * around the same <code> each time a recurrent timeout is rescheduled,
 * this second form for XT_ADD_TIMEOUT allows you to take the <timeout-obj>
 * from a previously expired timeout and reschedule a new timeout using the 
 * closure setup by the initial call to (XT_ADD_TIMEOUT <interval> <code>).
 * During the execution of <code>, the symbol TIMEOUT_OBJ is bound to 
 * <timout-obj> so that you don't need to keep around a global variable
 * for each recurrent timeout.
 *
 * The returned <timeout-obj> may be passed into the  functions
 * (XT_REMOVE_TIMEOUT <timeout-obj>), or (XT_ADD_TIMEOUT <interval> <timeout-obj>).
 ****************************************************************************/
LVAL Wto_Prim_XT_ADD_TIMEOUT()
{
  extern XtAppContext app_context; /* winterp.c */
  extern LVAL s_lambda, xlenv, xlfenv;
  LVAL arg, l_code, timeout_obj=NIL;
  long i;
  unsigned long interval;
  
  /* protect some pointers */
  xlsave1(timeout_obj);

  /* get interval */
  if ((i = getfixnum(xlgafixnum())) < 0L)
    xlfail("Timeout interval must be a positive integer.");
  else
    interval = (unsigned long) i;

  /* get <code> or <timeout_obj> */
  switch (ntype(arg = xlgetarg())) {
  case XLTYPE_TIMEOUTOBJ:
    timeout_obj = arg;
    if (get_timeout_id(timeout_obj) != (XtIntervalId) NULL)
      xlerror("Attempt to schedule a recurrent timeout before previous timeout's expiration.", timeout_obj);
    break;
  case CONS:
    l_code = arg;
    break;
  default:
    xlerror("Bad Argument Type, expected <timeout-code> or <timeout-object>.", arg);
    break;
  }

  xllastarg();

  /* 
   * create the client_data to be sent to Winterp_TimeoutProc. 
   * That procedure takes the client_data==timeout_obj, extracts the closure,
   * and uses this to execute the timeout callback. We also need to retain
   * the timeout identifier for use in RemoveTimeout. For simplicity, we save
   * in lisp object timeout_obj.
   */

  /* if args gave <code>, need to create timeout-obj and closure around <code> */
  if (timeout_obj == NIL) {		
    timeout_obj = new_timeoutobj();
    set_timeout_closure(timeout_obj,
			xlclose(s_TIMEOUT, s_lambda, NIL, l_code, xlenv, xlfenv));
  }

  set_timeout_id(timeout_obj,
		 XtAppAddTimeOut(app_context, interval, Winterp_TimeoutProc, (XtPointer) timeout_obj));  

  /*
   * Put timeout_obj in savedobjs so that it gets marked during gc. In that way, we
   * know that the timeout closure (i.e. the callback code, and it's lexical
   * environment) and timeout identifier won't get garbage collected while the
   * timeout-object is "referenced" inside Motif. The timeout_obj gets removed from
   * savedobjs each time the timeout expires, or when xt_remove_timeout gets called.
   */
  { 
    int  i = Wso_Hash(timeout_obj);
    LVAL l_hbucket;
    
    xlsave1(l_hbucket);
    l_hbucket = cons(timeout_obj, getelement(v_savedobjs, i));
    setelement(v_savedobjs, i, l_hbucket);
    xlpop();
  }

  /* resore the stack */
  xlpop();

  return (timeout_obj);
}


/******************************************************************************
 * extern void XtRemoveTimeout();
 *      XtIntervalId timer;      
 * 
 * Lisp:   (xt_remove_timeout <timeout_obj>)
 *         where <timeout_obj> is the value returned by xt_add_timeout.
 ******************************************************************************/
LVAL Wto_Prim_XT_REMOVE_TIMEOUT()
{
  extern LVAL true;
  LVAL timeout_obj;
  
  timeout_obj = xlga_timeoutobj();
  xllastarg();

  if (get_timeout_id(timeout_obj) == (XtIntervalId) NULL)
    xlerror("Attempt to remove an expired/removed timeout.", timeout_obj);
  
  XtRemoveTimeOut(get_timeout_id(timeout_obj));

  /* the timeout-object is no longer active, so indicate that by nulling the timeout-id */
  set_timeout_id(timeout_obj, NULL);

  /* remove <timeout_obj> from v_savedobjs allowing it to be garbage collected */
  {
    int i = Wso_Hash(timeout_obj);
    LVAL l_hbucket = getelement(v_savedobjs, i);
    LVAL l_prev = NIL;

    while ((l_hbucket != NIL) && (car(l_hbucket) != timeout_obj)) {
      l_prev = l_hbucket;
      l_hbucket = cdr(l_hbucket);
    }
    if (l_hbucket == NIL)
      xlerror("Internal error in Wto_Prim_XT_REMOVE_TIMEOUT -- couldn't remove <timeout-obj> from v_savedobjs. Hash error?",
	      timeout_obj);
    if (l_prev == NIL)		/* first elt matched */
      setelement(v_savedobjs, i, cdr(l_hbucket));
    else
      rplacd(l_prev, cdr(l_hbucket));
  }

  return (true);
}


/******************************************************************************
 * (TIMEOUT_ACTIVE_P <timeout_obj>)
 * 	--> returns T if <timeout_obj> is still scheduled, returns
 *	    NIL if <timeout_obj> has expired or if the timeout was
 *	    inactivated by XT_REMOVE_TIMEOUT.
 *
 *  <timeout_obj> is the value returned by XT_ADD_TIMEOUT.
 ******************************************************************************/
LVAL Wto_Prim_TIMEOUT_ACTIVE_P()
{
  extern LVAL true;
  LVAL timeout_obj;
  
  timeout_obj = xlga_timeoutobj();
  xllastarg();
  
  return (get_timeout_id(timeout_obj) ? true : NIL);
}


/******************************************************************************
 *
 ******************************************************************************/
Wto_Init()
{
  s_TIMEOUT_OBJ = xlenter("TIMEOUT_OBJ");
  s_TIMEOUT     = xlenter("XT_TIMEOUT");
}

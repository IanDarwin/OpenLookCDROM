/* -*-C-*-
********************************************************************************
*
* File:         w_callbacks.c
* RCS:          $Header: /users/npm/src/winterp/src-server/RCS/w_callbacks.c,v 2.7 1994/06/06 15:41:01 npm Exp $
* Description:  Define WINTERP's callback facilities. Wcb_Meta_Callbackproc()
*		is a generalized callback facility used in event-handlers, widget
*		callbacks, input callbacks, timeouts, etc.
* Author:       Niels Mayer
* Created:      Sat Aug 26 07:44:17 1989
* Modified:     Sun Jun  5 14:34:15 1994 (Niels Mayer) npm@indeed
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
static char rcs_identity[] = "@(#)$Header: /users/npm/src/winterp/src-server/RCS/w_callbacks.c,v 2.7 1994/06/06 15:41:01 npm Exp $";

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
#include <Xm/Xm.h>
#include "winterp.h"

#ifdef HP_GRAPH_WIDGET
#include "widgets/Graph.h"	/* needed for graph widget's added callback reasons XmCR_* */
#endif /* HP_GRAPH_WIDGET */

#ifdef SGI_DROP_POCKET_WIDGET	/* only for Irix 5.1 and IndigoMagic desktop */
#include <Sgm/DropPocket.h>
#endif /* SGI_DROP_POCKET_WIDGET */

/* Symbols init'd in Wcb_Init() */
LVAL s_CALLBACK_WIDGET, s_CALLBACK_REASON, s_CALLBACK_XEVENT, s_CALLBACK_WINDOW, s_CALLBACK_VALUE, s_CALLBACK_LENGTH, s_CALLBACK_DOIT;
#ifdef WINTERP_MOTIF_11
LVAL s_CALLBACK_CLICK_COUNT;
#endif				/* WINTERP_MOTIF_11 */


/* 
 * To prevent garbage collection of callback-objects, we store the objects in
 * the hashtable w_savedobjs.c:v_savedobjs after hashing on the address of the
 * widget-object LVAL. Since callbacks occur on a per-widget basis, we use the
 * widget as the hash-key and then search through the hashbucket looking for
 * LVALs that are callback-objects, have the appropriate widget-object, and
 * have the right callback name.
 * 
 * Garbage collection of lisp-objects will occur to any object not referenced
 * as symbol values, inside lexical environments saved as functional closures,
 * as elements of the evaluation or argument stack. In the case of callbacks,
 * the callback closure, the callback's lexical environment, and the callback
 * widget are implicitly referenced inside Motif/Xtoolkit code. As long as the
 * widget on which these callbacks are placed still exists, we must prevent
 * callback-objects from being garbage collected. 
 * 
 * This is done by:
 *    1) storing them in v_savedobjs when the callback is added
 *       (note that v_savedobjs is a hashtable hashed by the LVAL's pointer,
 *        for callbacks, we store all callbacks on a particular widget in the
 *        hashbucket associated with hashing on that widget-object. this makes
 *        it easier to implement :set_callback. and :remove_all_callbacks.)
 *    2) removing them when the callback is removed
 *       (as in methods :set_callback :remove_all_callbacks and function remove_callback)
 *    3) removing them when the widget is destroyed.
 *       (requires a destroy callback to be added onto each widget)
 */


/******************************************************************************
 * Wcb_Meta_Callbackproc() -- this is a generalized callback procedure that
 * is used for widget callbacks (wc_*.c), widget event handlers (w_evnthndlr.c),
 * Xt Timeouts (w_timeouts.c), and Xt Input Callbacks (w_inputCB.c).
 * 
 * Note that w_txlations.c:Wtx_Winterp_Lisp_Action_Proc() uses it's own version
 * of the code below, that is mostly similar. If modifying the code below, 
 * see if modifications also apply to Wtx_Winterp_Lisp_Action_Proc().
 ******************************************************************************/
void Wcb_Meta_Callbackproc(lval_closure, client_data, call_data,
			   bind_call_data_values_proc, /* a funct ptr, required */
			   set_call_data_values_proc,  /* optional func ptr, may be NULL */
			   trace_enter_proc)           /* a func ptr, required */
     LVAL      lval_closure;	/* CLOSURE taken from XLTYPE_CALLBACKOBJ,
				   XLTYPE_TIMEOUTOBJ, XLTYPE_EVHANDLEROBJ,
				   XLTYPE_FDINPUTCBOBJ */
     LVAL      client_data;	/* XLTYPE_CALLBACKOBJ, XLTYPE_TIMEOUTOBJ,
				   XLTYPE_EVHANDLEROBJ, XLTYPE_FDINPUTCBOBJ */
     XtPointer call_data;	/* a pointer to a structure that is accessed via
				   (*bind_call_data_values_proc)() */
     void      (*bind_call_data_values_proc)( /* LVAL bindings_list;
						 LVAL lexical_env;
						 XtPointer call_data;
						 LVAL client_data */ );
     void      (*set_call_data_values_proc)(  /* LVAL lexical_env;
						 XtPointer call_data */);
     void      (*trace_enter_proc)(           /* LVAL tracing;
						 LVAL client_data */);
{
  extern int winterp_caught_signal; /* winterp.c */
  LVAL l_val;
  LVAL tracing;
  int  mask;

  xlsave1(l_val);

  /*
   * Reset winterp_caught_signal so that winterp.c:oscheck() doesn't trigger
   * off of a ^C typed in to terminal prior to some GUI action. Since this
   * procedure is called from a GUI action, we clear the flag; ^C should
   * trigger an abort only once an XLISP evaluation has started.
   * See also winterp.c:Read_From_Stream_Eval_And_Print(),
   * w_txlations.c:Wtx_Winterp_Lisp_Action_Proc().
   */
  winterp_caught_signal = FALSE;

  /*
   * Most of the following looks like the path taken by evaluating
   * a functional closure, i.e.
   * xleval(form)-->
   *		X-- check for control codes	(not needeed for callbacks?)
   *		X-- check for *evalhook*	(not desired for callbacks?)
   *		--  evform(form)-->
   *			X-- check if this fn/method is being traced
   *			X-- switch(ntype(car(form)...case(CLOSURE)-->
   *			X-- check for applyhook
   *			X-- evpushargs(car(form),NIL)
   *				{FRAMEP newfp = xlsp
   *				 -- Note: pusharg() =~= "*xlsp++ = (x)"
   *				 pusharg(cvfixnum((FIXTYPE)(newfp - xlfp)))
   *				 pusharg(fun)
   *				 pusharg(NIL) -- will be argc --
   *				 -- establish the new stack frame --
   *				 newfp[2] = cvfixnum((FIXTYPE)argc)
   *				 xlfp = newfp;
   *				 }
   *			X-- trenter(tracing,0,xlfp+3) -- note no args, therefore 0,NIL.
   *				-- if (!null(tracing)) doenter(tracing,0,NIL)
   *					-- indent to cur trc level, displ funct call
   *			X-- val = evfun(fun,0,xlfp+3)
   *			X-- trexit(tracing,val)
   *				-- if (!null(tracing)) doexit(tracing,val)
   *					-- indent to cur trc level, print return(fn).
   *			X-- xlsp = xlfp; -- remove the call frame
   *			X -- xlfp = xlfp - (int)getfixnum(*xlfp);
   */

#if 0				/* from xleval(): do we want to do this in callbacks?? */
  {extern LVAL xlsample;
   /* check for control codes */
   if (--xlsample <= 0) {
     xlsample = SAMPLE;
     oscheck();
   }}
#endif

#if 0				/* from xleval(): can't use applyhook here because we'd have to teach it about callbacks */
  {
    extern LVAL s_evalhook;
    /* from xleval.c:xleval() -- check for *evalhook* */
    if (!null(getvalue(s_evalhook)))
      return (evalhook(expr));
  }
#endif

  /*
   * mutated from xleval.c:evform():
   * check if callback-name (e.g. :XMN_ACTIVATE_CALLBACK) is
   * on trace list, cause entry/exit printout if so...
   *
   * TODO: in the future we may want to allow for a WIDGETOBJ to be placed on
   * the tracelist such that all callbacks, evhandlers, etc w/r/t the widget
   * get traced. That may be more useful than tracing, say, all
   * :XMN_ACTIVATE_CALLBACKs.
   */
  {
    extern LVAL s_tracelist;	/* xlisp/xlglob.c */
    if ((getvalue(s_tracelist) != NIL)
	&& xlmember(getname(lval_closure), getvalue(s_tracelist)))
      tracing = getname(lval_closure);
    else
      tracing = NIL;
  }

#if 0				/* from xleval.c:evform(): can't use applyhook here because we'd have to teach it about callbacks */
#ifdef APPLYHOOK
  {
    extern LVAL s_applyhook;
    /* check for *applyhook* */
    if (!null(getvalue(s_applyhook))) {
      l_val = (applyhook(lval_closure, args));
    }
  }
#endif				/* APPLYHOOK */
#endif

  /*
   * mutated from xleval.c:evform():
   * argc = evpushargs(lval_closure,NIL) = 0
   * argv = xlfp + 3;
   */
  {				/* NPM: txform of evpusharg() for args=NIL */
    FRAMEP newfp;
    /* build a new argument stack frame */
    newfp = xlsp;
    /* Note: pusharg() =~= "*xlsp++ = (x)" */
    pusharg(cvfixnum((FIXTYPE)(newfp - xlfp)));
    pusharg(lval_closure);
    pusharg(cvfixnum((FIXTYPE)0));
    xlfp = newfp;
  }

  /*
   * from xleval.c:evform():
   * trenter(tracing,argc,argv)
   */
  if (tracing != NIL)
    (*trace_enter_proc)(tracing, client_data);

  /*
   * Mutated from xleval.c:evfun():
   * The main difference is that instead of calling xlabind() to bind the
   * formal parameter symbols of a function to their values in the new
   * lexical environment frame returned by xlframe(getenvi(fun)), we look
   * at the formal args from the callback's closure and bind these to the
   * appropriate values.
   */
  {
    extern LVAL		xlenv, xlfenv, xldenv, xlvalue, s_unbound;
    CONTEXT		cntxt;
    LVAL		oldenv, oldfenv, l_evalforms;
#ifdef SPECIALS
    LVAL		olddenv=xldenv;
#endif
  
    /* from xleval.c:evfun(): protect some pointers */
    xlstkcheck(3);
    xlsave(oldenv);
    xlsave(oldfenv);
    xlsave(l_evalforms);

    /* from xleval.c:evfun(): create a new environment frame */
    oldenv = xlenv;
    oldfenv = xlfenv;
    /* TODO: if ntype(client_data) is XLTYPE_CALLBACKOBJ or XLTYPE_EVHANDLEROBJ
       then call "xlenv = cons(cons(obj,msgcls),getenvi(lval_closure)); xlenv = xlframe(xlenv);"
       in place of "xlenv = xlframe(getenvi(lval_closure));"
       with obj=get_callback_widget() or get_evhandler_widget() and msgcls=class(obj).
       This should allow callbacks on subclassed widgets to access instance variables of subclasses directly... */
    xlenv = xlframe(getenvi(lval_closure));
    xlfenv = getfenv(lval_closure);

    /* 
     * get the list of bindings and bind (locally) the symbols to values retrieved from
     * call_data. Similar to call to xlabind() in evfun().
     */
    (*bind_call_data_values_proc)(getargs(lval_closure), /* a list of symbols to which values from call_data struct are locally bound */
				  xlenv, /* the lexical environment in which the bindings are made */
				  call_data, /* for callbacks, this is a pointer to a Xm*CallbackStruct, for evhandlers, this is an XEvent*, etc... */
				  client_data);	/* XLTYPE_CALLBACKOBJ, XLTYPE_TIMEOUTOBJ, XLTYPE_EVHANDLEROBJ, XLTYPE_FDINPUTCBOBJ */
  
    /* mutated from xleval.c:evfun() -- setup the implicit block */
    xlbegin(&cntxt, CF_TOPLEVEL|CF_CLEANUP|CF_BRKLEVEL|CF_RETURN, getname(lval_closure)); /* NPM: added CF_TOPLEVEL|CF_CLEANUP|CF_BRKLEVEL to make this work like a XLISP top-level loop, even though event-driven... */
  
    /* mutated from xleval.c:evfun() -- execute the block */
    if ((mask = setjmp(cntxt.c_jmpbuf)) != 0) {
      if (mask & CF_RETURN)
	l_val = xlvalue;
      else {			/* CF_TOPLEVEL|CF_CLEANUP|CF_BRKLEVEL occured out of breakloop(), so just return out of callback. */
	extern LVAL s_evalhook,s_applyhook; /* from xlisp/xlglob.c */
	extern int xltrcindent, xldebug; /* from xlisp/xlglob.c */
	l_val = s_unbound;
	setvalue(s_evalhook, NIL);
	setvalue(s_applyhook, NIL);
	xltrcindent = 0;
	xldebug = 0;
	Winterp_Print_Newline(); /* xlflush() must occur after this in order to get new-line (as spec'd by arg 'TRUE') to occur */
	xlflush();		/* needed if using (read)/(read-line) from stdin */ 
      }
    }
    else {
      for (l_val = NIL, l_evalforms = getbody(lval_closure);
	   consp(l_evalforms); 
	   l_evalforms = cdr(l_evalforms))
	l_val = xleval(car(l_evalforms));
      /*
       * if this is a callback in which elements of the call_data structure must
       * be set before the callback returns (as in the XmText or the XmGraph widgets),
       * then set_call_data_values_proc is a pointer to the function that does this
       * optional dirty deed. Note that this never gets called if error occured.
       */
      if (set_call_data_values_proc && call_data) /* don't call if call_data==NULL (e.g. during :XMN_DESTROY_CALLBACK) */
	(*set_call_data_values_proc)(xlenv, /* the lexical environment in which the bindings are made  */
				     call_data); /* a pointer to a Xm*CallbackStruct (the structure used depends on the widget-class of the widget containing the callback */
    }

    /* from xleval.c:evfun(): finish the block context */
    xlend(&cntxt);
  
    /* from xleval.c:evfun(): restore the environment */
    xlenv = oldenv;
    xlfenv = oldfenv;
#ifdef SPECIALS
    xlunbind(olddenv);
#endif

    /* from xleval.c:evfun(): restore the stack */
    xlpopn(3);			/* oldenv, oldfenv, l_evalforms */

    /* xleval.c:evfun() had return (l_val) but callbacks don't return a value */
  }

  /*
   * from xleval.c:evfun():
   * trexit(tracing,val);
   */
  if (tracing != NIL) {
    extern int xltrcindent;
    int i;

    /* indent to the current trace level */
    --xltrcindent;
    for (i = 0; i < xltrcindent; ++i)
      trcputstr(" ");
    /* display the function value */
    trcputstr("Exiting: ");
    trcprin1(tracing);		/* print :XMN_*_CALLBACK, EVHANDLER, XT_TIMEOUT, FDINPUTCB or whatever */
    trcputstr(", Value: ");    
    trcprin1(l_val);
    trcputstr("\n");
  }

  /*
   * from xleval.c:evfun() -- remove the call frame
   */
  xlsp = xlfp;
  xlfp = xlfp - (int)getfixnum(*xlfp);

  /* restore the stack */
  xlpop();			/* l_val */
}


/******************************************************************************
 * This is called whenever we trace a callback, e.g. 
 * (trace :xmn_activate_callback).
 * For callbacks only, this proc is passed in as the 'trace_enter_proc' 
 * in Wcb_Meta_Callbackproc().
 ******************************************************************************/
void Wcb_Callback_Trace_Proc(tracing, client_data)
     LVAL tracing;		/* SYMBOL */
     LVAL client_data;		/* XLTYPE_CALLBACKOBJ */
{
  extern int xltrcindent;
  int i;

  /* indent to the current trace level */
  for (i = 0; i < xltrcindent; ++i)
    trcputstr(" ");
  ++xltrcindent;
  /* display the callback call... */
  trcputstr("Entering: ");
  trcprin1(tracing);		/* print :XMN_*_CALLBACK or whatever */
  trcputstr(" -- widget = ");
  trcprin1(get_callback_widget(client_data)); /* print WIDGETOBJ */
  trcputstr("; args = ");		
  trcprin1(getargs(get_callback_closure(client_data))); /* print args for callback */
  trcputstr("\n");
}


/******************************************************************************
 * Are we having fun yet?
 ******************************************************************************/
static LVAL s_CR_NONE, s_CR_HELP, s_CR_VALUE_CHANGED, s_CR_INCREMENT, /* Symbols init'd in Wcb_Init() */
  s_CR_DECREMENT, s_CR_PAGE_INCREMENT, s_CR_PAGE_DECREMENT, s_CR_TO_TOP,
  s_CR_TO_BOTTOM, s_CR_DRAG, s_CR_ACTIVATE, s_CR_ARM, s_CR_DISARM, s_CR_MAP,
  s_CR_UNMAP, s_CR_FOCUS, s_CR_LOSING_FOCUS, s_CR_MODIFYING_TEXT_VALUE,
  s_CR_MOVING_INSERT_CURSOR, s_CR_EXECUTE, s_CR_SINGLE_SELECT,
  s_CR_MULTIPLE_SELECT, s_CR_EXTENDED_SELECT, s_CR_BROWSE_SELECT,
  s_CR_DEFAULT_ACTION, s_CR_CLIPBOARD_DATA_REQUEST, s_CR_CLIPBOARD_DATA_DELETE,
  s_CR_CASCADING, s_CR_OK, s_CR_CANCEL, s_CR_APPLY, s_CR_NO_MATCH,
  s_CR_COMMAND_ENTERED, s_CR_COMMAND_CHANGED, s_CR_EXPOSE, s_CR_RESIZE, s_CR_INPUT;
#ifdef WINTERP_MOTIF_11
static LVAL s_CR_GAIN_PRIMARY, s_CR_LOSE_PRIMARY;
#ifdef XmCR_CREATE		/* added in Motif 1.1.3 */
static LVAL s_CR_CREATE;
#endif /* XmCR_CREATE */
#ifdef XmCR_PROTOCOLS		/* added in Motif 1.1.3 */
static LVAL s_CR_PROTOCOLS;
#endif /* XmCR_PROTOCOLS */
#endif /* WINTERP_MOTIF_11 */
#ifdef WINTERP_MOTIF_12		/* added in Motif 1.2 */
static LVAL s_CR_TEAR_OFF_ACTIVATE, s_CR_TEAR_OFF_DEACTIVATE, s_CR_OBSCURED_TRAVERSAL;
#endif /* WINTERP_MOTIF_12 */

#ifdef HP_GRAPH_WIDGET
static LVAL s_CR_NEW_ARC, s_CR_NEW_NODE, s_CR_NODE_MOVED, s_CR_ARC_MOVED, s_CR_SUBGRAPH_MOVED, /* Symbols init'd in Wcb_Init() */
  s_CR_ARC_EDITED, s_CR_SELECT_NODE, s_CR_SELECT_ARC, s_CR_SELECT_SUBGRAPH, s_CR_DELETE_NODE,
  s_CR_DELETE_ARC, s_CR_SELECT, s_CR_RELEASE, s_CR_NODE_DOUBLE_CLICK, s_CR_ARC_DOUBLE_CLICK,
  s_CR_DOUBLE_CLICK, s_CR_DESELECT_ARC, s_CR_DESELECT_NODE, s_CR_DESELECT, s_CR_NODES_MOVED, 
  s_CR_SELECT_NODES, s_CR_SELECT_ARCS, s_CR_SELECT_ARCS_AND_NODES;
#endif /* HP_GRAPH_WIDGET */

#ifdef SGI_DROP_POCKET_WIDGET	/* only for Irix 5.1 and IndigoMagic desktop */
static LVAL s_CR_ICON_CHANGE;
#endif /* SGI_DROP_POCKET_WIDGET */

LVAL Wcb_Get_Callback_Reason_Symbol(cb_reason)
     int cb_reason;
{
  switch (cb_reason) {
  case XmCR_NONE:
    return (s_CR_NONE);
  case XmCR_HELP:
    return (s_CR_HELP);
  case XmCR_VALUE_CHANGED:
    return (s_CR_VALUE_CHANGED);
  case XmCR_INCREMENT:
    return (s_CR_INCREMENT);
  case XmCR_DECREMENT:
    return (s_CR_DECREMENT);
  case XmCR_PAGE_INCREMENT:
    return (s_CR_PAGE_INCREMENT);
  case XmCR_PAGE_DECREMENT:
    return (s_CR_PAGE_DECREMENT);
  case XmCR_TO_TOP:
    return (s_CR_TO_TOP);
  case XmCR_TO_BOTTOM:
    return (s_CR_TO_BOTTOM);
  case XmCR_DRAG:
    return (s_CR_DRAG);
  case XmCR_ACTIVATE:
    return (s_CR_ACTIVATE);
  case XmCR_ARM:
    return (s_CR_ARM);
  case XmCR_DISARM:
    return (s_CR_DISARM);
  case XmCR_MAP:
    return (s_CR_MAP);
  case XmCR_UNMAP:
    return (s_CR_UNMAP);
  case XmCR_FOCUS:
    return (s_CR_FOCUS);
  case XmCR_LOSING_FOCUS:
    return (s_CR_LOSING_FOCUS);
  case XmCR_MODIFYING_TEXT_VALUE:
    return (s_CR_MODIFYING_TEXT_VALUE);
  case XmCR_MOVING_INSERT_CURSOR:
    return (s_CR_MOVING_INSERT_CURSOR);
  case XmCR_EXECUTE:
    return (s_CR_EXECUTE);
  case XmCR_SINGLE_SELECT:
    return (s_CR_SINGLE_SELECT);
  case XmCR_MULTIPLE_SELECT:
    return (s_CR_MULTIPLE_SELECT);
  case XmCR_EXTENDED_SELECT:
    return (s_CR_EXTENDED_SELECT);
  case XmCR_BROWSE_SELECT:
    return (s_CR_BROWSE_SELECT);
  case XmCR_DEFAULT_ACTION:
    return (s_CR_DEFAULT_ACTION);
  case XmCR_CLIPBOARD_DATA_REQUEST:
    return (s_CR_CLIPBOARD_DATA_REQUEST);
  case XmCR_CLIPBOARD_DATA_DELETE:
    return (s_CR_CLIPBOARD_DATA_DELETE);
  case XmCR_CASCADING:
    return (s_CR_CASCADING);
  case XmCR_OK:
    return (s_CR_OK);
  case XmCR_CANCEL:
    return (s_CR_CANCEL);
  case XmCR_APPLY:
    return (s_CR_APPLY);
  case XmCR_NO_MATCH:
    return (s_CR_NO_MATCH);
  case XmCR_COMMAND_ENTERED:
    return (s_CR_COMMAND_ENTERED);
  case XmCR_COMMAND_CHANGED:
    return (s_CR_COMMAND_CHANGED);
  case XmCR_EXPOSE:
    return (s_CR_EXPOSE);
  case XmCR_RESIZE:
    return (s_CR_RESIZE);
  case XmCR_INPUT:
    return (s_CR_INPUT);
#ifdef WINTERP_MOTIF_11
  case XmCR_GAIN_PRIMARY:
    return (s_CR_GAIN_PRIMARY);
  case XmCR_LOSE_PRIMARY:
    return (s_CR_LOSE_PRIMARY);
#ifdef XmCR_CREATE		/* added in Motif 1.1.3 */
  case XmCR_CREATE:
    return (s_CR_CREATE);
#endif /* XmCR_CREATE */
#ifdef XmCR_PROTOCOLS		/* added in Motif 1.1.3 */
  case XmCR_PROTOCOLS:
    return (s_CR_PROTOCOLS);
#endif /* XmCR_PROTOCOLS */
#endif /* WINTERP_MOTIF_11 */
#ifdef WINTERP_MOTIF_12		/* added in Motif 1.2 */
  case XmCR_TEAR_OFF_ACTIVATE:
    return (s_CR_TEAR_OFF_ACTIVATE);
  case XmCR_TEAR_OFF_DEACTIVATE:
    return (s_CR_TEAR_OFF_DEACTIVATE);
  case XmCR_OBSCURED_TRAVERSAL:
    return (s_CR_OBSCURED_TRAVERSAL);
#endif /* WINTERP_MOTIF_12 */

#ifdef HP_GRAPH_WIDGET
  case XmCR_NEW_ARC:
    return (s_CR_NEW_ARC);
  case XmCR_NEW_NODE:
    return (s_CR_NEW_NODE);
  case XmCR_NODE_MOVED:
    return (s_CR_NODE_MOVED);
  case XmCR_ARC_MOVED:
    return (s_CR_ARC_MOVED);
  case XmCR_SUBGRAPH_MOVED:
    return (s_CR_SUBGRAPH_MOVED);
  case XmCR_ARC_EDITED:
    return (s_CR_ARC_EDITED);
  case XmCR_SELECT_NODE:
    return (s_CR_SELECT_NODE);
  case XmCR_SELECT_ARC:
    return (s_CR_SELECT_ARC);
  case XmCR_SELECT_SUBGRAPH:
    return (s_CR_SELECT_SUBGRAPH);
  case XmCR_DELETE_NODE:
    return (s_CR_DELETE_NODE);
  case XmCR_DELETE_ARC:
    return (s_CR_DELETE_ARC);
  case XmCR_SELECT:
    return (s_CR_SELECT);
  case XmCR_RELEASE:
    return (s_CR_RELEASE);
  case XmCR_NODE_DOUBLE_CLICK:
    return (s_CR_NODE_DOUBLE_CLICK);
  case XmCR_ARC_DOUBLE_CLICK:
    return (s_CR_ARC_DOUBLE_CLICK);
  case XmCR_DOUBLE_CLICK:
    return (s_CR_DOUBLE_CLICK);
  case XmCR_DESELECT_ARC:
    return (s_CR_DESELECT_ARC);
  case XmCR_DESELECT_NODE:
    return (s_CR_DESELECT_NODE);
  case XmCR_DESELECT:
    return (s_CR_DESELECT);
  case XmCR_NODES_MOVED:
    return (s_CR_NODES_MOVED);
  case XmCR_SELECT_NODES:
    return (s_CR_SELECT_NODES);
  case XmCR_SELECT_ARCS:
    return (s_CR_SELECT_ARCS);
  case XmCR_SELECT_ARCS_AND_NODES:
    return (s_CR_SELECT_ARCS_AND_NODES);
#endif /* HP_GRAPH_WIDGET */

#ifdef SGI_DROP_POCKET_WIDGET	/* only for Irix 5.1 and IndigoMagic desktop */
  case SgCR_ICON_CHANGE:
    return (s_CR_ICON_CHANGE);
#endif /* SGI_DROP_POCKET_WIDGET */

  default:
    xlfail("Internal error in Wcb_Get_Callback_Reason_Symbol() -- unknown callback reason.");
  }
}


/*******************************************************************************
 *
 ******************************************************************************/
LVAL Wcb_Meta_Method_Add_Callback(callback_proc, one_callback_per_name_p)
     XtCallbackProc callback_proc;
     Boolean        one_callback_per_name_p;
{
  extern LVAL s_lambda, xlenv, xlfenv;
  LVAL   o_self, l_fargs, l_code;
  LVAL   xtr_name, s_callback, callback_obj;
  Widget widget_id;
  char*  name;
  
  /* get <widget_instance> */
  widget_id = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&o_self);
				    
  /* get <name> */
  xtr_name = xlgetarg();
  if (xtresource_p(xtr_name) && Wres_Is_Callback_P(xtr_name)) {
    name = Wres_Get_Name(xtr_name);
    s_callback = Wres_Get_Symbol(xtr_name);
  }
  else if (symbolp(xtr_name)) {	
    xtr_name = getvalue(xtr_name);
    if (xtresource_p(xtr_name) && Wres_Is_Callback_P(xtr_name)) {
      name = Wres_Get_Name(xtr_name);
      s_callback = Wres_Get_Symbol(xtr_name);
    }
    else
      xlerror("Invalid callback name symbol", xtr_name);
  }
  else
    xlerror("Invalid callback name argument", xtr_name);
      
  /* get <call_data_binding_names_list> -- args to be bound at call time.
     NOTE: may want to check that these args are valid for the particular widget
     class that this "meta-method" is used in. No biggie though -- they'll get
     caught at runtime, when the callback fires. */
  l_fargs = xlgalist();		

  /* get <code> */
  l_code = xlgalist();
  xllastarg();

  /* 
   * if this procedure is being called from the :set_callback method 
   * (indicated by one_callback_per_name_p = TRUE), 
   * then remove all callbacks on <widget_inst> matching <name>
   */
  if (one_callback_per_name_p) { 
    int  i = Wso_Hash(o_self);
    LVAL l_hbucket = getelement(v_savedobjs, i); /* a list of objects, including all callback-objs on this widget */
    LVAL obj;
    LVAL l_prev = NIL;
    while (l_hbucket != NIL) {	/* while there are elements in the hashbucket */
      obj = car(l_hbucket);	/* obj points to cur elt */
      if (callbackobj_p(obj) && (get_callback_name(obj) == name) && (get_callback_widget(obj) == o_self)) {
	XtRemoveCallback(widget_id, name, get_callback_proc(obj), (XtPointer) obj);
	l_hbucket = cdr(l_hbucket); /* l_hbucket now points to next elt or NIL */
	if (l_prev == NIL)
	  setelement(v_savedobjs, i, l_hbucket); /* remove first, head is now next elt */
	else
	  rplacd(l_prev, l_hbucket); /* remove cur, point previous to next */
      }
      else {
	l_prev = l_hbucket;
	l_hbucket = cdr(l_hbucket);
      }
    }
  }

  /* 
   * create the client_data to be sent to (*callback_proc)()
   * That procedure takes the client_data and extracts the widget-object,
   * and the closure, and uses these to execute the callback.
   */
  xlsave1(callback_obj);	/* protect some pointers */
  callback_obj = new_callbackobj();
  set_callback_widget(callback_obj, o_self);
  set_callback_name(callback_obj, name);
  set_callback_proc(callback_obj, callback_proc);
  set_callback_closure(callback_obj,
		       xlclose(s_callback, s_lambda, l_fargs, l_code, xlenv, xlfenv));
  
  XtAddCallback(widget_id, name, callback_proc, (XtPointer) callback_obj);

  /*
   * Enter the callback_obj in v_savedobjs, so that it gets marked.
   * This way, it won't be garbage collected while the callback is
   * active. :set_callback, xt_remove_callback, and :remove_all_callbacks
   * may remove and destroy the callback_obj created here. Destroying the
   * widget will result in the callbackobj getting garbage collected 
   * -- see Wcls_Widget_Destroy_Callback()
   */
  { 
    int  i = Wso_Hash(o_self);
    LVAL l_hbucket;
    
    xlsave1(l_hbucket);
    l_hbucket = cons(callback_obj, getelement(v_savedobjs, i));
    setelement(v_savedobjs, i, l_hbucket);
    xlpop();			/* l_hbucket */
  }

  /* resore the stack */
  xlpop();			/*callback_obj*/

  return (callback_obj);
}


/*******************************************************************************
 *
 ******************************************************************************/
void Wcb_Callback_Missing_Call_Data_Error(client_data, lval_symbol)
     LVAL client_data;		/* XLTYPE_CALLBACKOBJ */
     LVAL lval_symbol;		/* SYMBOL */
{
  sprintf(temptext,
	  "Warning: in %s, ignored retrieving callback value %s.\n\tNULL call_data structure passed - ",
	  get_callback_name(client_data),
	  symbolp(lval_symbol) ? getstring(getpname(lval_symbol)) : "<bad-symbol>");
  errputstr(temptext);
  errprint(get_callback_widget(client_data));
}



/******************************************************************************
 * lisp: (SEND <widget> :REMOVE_ALL_CALLBACKS <name>)
 * returns T.
 *
 * <name> is a resource keyword of type XmRCallback, eg,
 * :XMN_ACTIVATE_CALLBACK, :XMN_ARM_CALLBACK, :XMN_DISARM_CALLBACK.
 *
 * This procedure removes all callbacks matching <name> for <widget>.
 ******************************************************************************/
LVAL Widget_Class_Method_REMOVE_ALL_CALLBACKS()
{
  LVAL   o_self;
  LVAL   xtr_name;
  Widget widget_id;
  char*  name;
  
  /* get <widget_instance> */
  widget_id = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&o_self);
				    
  /* get <name> */
  xtr_name = xlgetarg();
  if (xtresource_p(xtr_name) && Wres_Is_Callback_P(xtr_name)) {
    name = Wres_Get_Name(xtr_name);
  }
  else if (symbolp(xtr_name)) {	
    xtr_name = getvalue(xtr_name);
    if (xtresource_p(xtr_name) && Wres_Is_Callback_P(xtr_name)) {
      name = Wres_Get_Name(xtr_name);
    }
    else
      xlerror("Invalid callback name symbol", xtr_name);
  }
  else
    xlerror("Invalid callback name argument", xtr_name);
      
  xllastarg();

  /* remove from v_savedobjs all callback-objs on <widget_inst> matching <name> */
  {
    int  i = Wso_Hash(o_self);
    LVAL l_hbucket = getelement(v_savedobjs, i); /* a list of objects, including all callback-objs on this widget  */
    LVAL obj;
    LVAL l_prev = NIL;
    while (l_hbucket != NIL) {	/* while there are elements in the hashbucket */
      obj = car(l_hbucket);	/* obj points to cur elt */
      if (callbackobj_p(obj)
	  && (get_callback_name(obj) == name)
	  && (get_callback_widget(obj) == o_self)) {
	l_hbucket = cdr(l_hbucket); /* l_hbucket now points to next elt or NIL */
	if (l_prev == NIL)
	  setelement(v_savedobjs, i, l_hbucket); /* remove first, head is now next elt */
	else
	  rplacd(l_prev, l_hbucket); /* remove cur, point previous to next */
      }
      else {
	l_prev = l_hbucket;
	l_hbucket = cdr(l_hbucket);
      }
    }
  }

  XtRemoveAllCallbacks(widget_id, name);

  return (o_self);
}


/******************************************************************************
 * lisp: (XT_REMOVE_CALLBACK <callback-obj>)
 * 
 * where <callback-obj> is the value returned by methods :set_callback or
 * :add_callback.
 ******************************************************************************/
LVAL Wcb_Prim_XT_REMOVE_CALLBACK()
{
  LVAL callback_obj;
  LVAL o_widget;
  Widget widget_id;
  extern LVAL true;

  callback_obj = xlga_callbackobj();
  xllastarg();

  /* check if this callback hasn't already been removed */
  if ((o_widget = get_callback_widget(callback_obj)) == NIL)
    xlerror("Callback associated with <callback-obj> has already been removed.", callback_obj);
  
  /* mark the callback_obj as being removed */
  set_callback_widget(callback_obj, NIL);

  if (!(widget_id = get_widgetobj_widgetID(o_widget)))
    xlerror("widget object not properly initialized by :isnew.", o_widget);
  
  XtRemoveCallback(widget_id,
		   get_callback_name(callback_obj),
		   get_callback_proc(callback_obj), /* note that there are difft callbackproc's for difft widgetclasses */
		   (XtPointer) callback_obj);

  /* remove <callback_obj> from v_savedobjs allowing it to be garbage collected */
  {
    int i = Wso_Hash(o_widget); /* note that we hash all callbacks on the same widget to the same hashbucket */
    LVAL l_hbucket = getelement(v_savedobjs, i);
    LVAL l_prev = NIL;

    while ((l_hbucket != NIL) && (car(l_hbucket) != callback_obj)) {
      l_prev = l_hbucket;
      l_hbucket = cdr(l_hbucket);
    }
    if (l_hbucket == NIL)
      xlerror("Internal error in XtRemoveCallback -- couldn't remove <callback-obj> from v_savedobjs. Hash error?",
	      callback_obj);
    if (l_prev == NIL)		/* first elt matched */
      setelement(v_savedobjs, i, cdr(l_hbucket));
    else
      rplacd(l_prev, cdr(l_hbucket));
  }
  
  return (true);
}


/******************************************************************************
 * lisp: (SEND <widget> :HAS_CALLBACKS <name>)
 * 		returns CALLBACK_NO_LIST -- no such callback list
 *		returns CALLBACK_HAS_NONE -- has no callbacks on list
 *		returns CALLBACK_HAS_SOME -- has some callbacks on list
 *
 * <name> is a resource keyword of type XmRCallback, eg,
 * :XMN_ACTIVATE_CALLBACK, :XMN_ARM_CALLBACK, :XMN_DISARM_CALLBACK.
 *
 *
 * XtCallbackStatus XtHasCallbacks(
 *     Widget  widgetm
 *     CONST String callback_name);
 ******************************************************************************/
static LVAL s_CALLBACK_NO_LIST, s_CALLBACK_HAS_NONE, s_CALLBACK_HAS_SOME; /* Symbols init'd in Wcb_Init() */
LVAL Widget_Class_Method_HAS_CALLBACKS()
{
  LVAL   o_self;
  LVAL   xtr_name;
  Widget widget_id;
  char*  name;
  
  /* get <widget_instance> */
  widget_id = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&o_self);
				    
  /* get <name> */
  xtr_name = xlgetarg();
  if (xtresource_p(xtr_name) && Wres_Is_Callback_P(xtr_name)) {
    name = Wres_Get_Name(xtr_name);
  }
  else if (symbolp(xtr_name)) {	
    xtr_name = getvalue(xtr_name);
    if (xtresource_p(xtr_name) && Wres_Is_Callback_P(xtr_name)) {
      name = Wres_Get_Name(xtr_name);
    }
    else
      xlerror("Invalid callback name symbol", xtr_name);
  }
  else
    xlerror("Invalid callback name argument", xtr_name);
      
  xllastarg();

  switch (XtHasCallbacks(widget_id, name)) {
  case XtCallbackNoList:
    return (s_CALLBACK_NO_LIST);
  case XtCallbackHasNone:
    return (s_CALLBACK_HAS_NONE);
  case XtCallbackHasSome:
    return (s_CALLBACK_HAS_SOME);
  }
}


/******************************************************************************
 *
 ******************************************************************************/
Wcb_Init()
{
  /*
   * Define shared callback call-data symbols used by various
   * Lexical_Bindings_For_Xm*CallbackStruct() procs in noted files.
   */
  s_CALLBACK_WIDGET		= xlenter("CALLBACK_WIDGET"); /* wc_*.c */
  s_CALLBACK_REASON		= xlenter("CALLBACK_REASON"); /* wc_*.c */
  s_CALLBACK_XEVENT		= xlenter("CALLBACK_XEVENT"); /* wc_*.c */
#ifdef WINTERP_MOTIF_11
  s_CALLBACK_CLICK_COUNT	= xlenter("CALLBACK_CLICK_COUNT"); /* wc_ArrowB.c wc_DrawnB.c wc_PushB.c */
#endif /* WINTERP_MOTIF_11 */
  s_CALLBACK_WINDOW		= xlenter("CALLBACK_WINDOW"); /* wc_DrawingA.c wc_DrawnB.c */
  s_CALLBACK_VALUE		= xlenter("CALLBACK_VALUE"); /* wc_Command.c wc_FileSB.c wc_Scale.c wc_ScrollBar.c wc_SelectioB.c */
  s_CALLBACK_LENGTH		= xlenter("CALLBACK_LENGTH"); /* wc_Command.c wc_FileSB.c wc_SelectioB.c */
  s_CALLBACK_DOIT		= xlenter("CALLBACK_DOIT"); /* wc_Text.c wc_XmGraph.c */

  s_CR_NONE = xlenter("CR_NONE");
  s_CR_HELP = xlenter("CR_HELP");
  s_CR_VALUE_CHANGED = xlenter("CR_VALUE_CHANGED");
  s_CR_INCREMENT = xlenter("CR_INCREMENT");
  s_CR_DECREMENT = xlenter("CR_DECREMENT");
  s_CR_PAGE_INCREMENT = xlenter("CR_PAGE_INCREMENT");
  s_CR_PAGE_DECREMENT = xlenter("CR_PAGE_DECREMENT");
  s_CR_TO_TOP = xlenter("CR_TO_TOP");
  s_CR_TO_BOTTOM = xlenter("CR_TO_BOTTOM");
  s_CR_DRAG = xlenter("CR_DRAG");
  s_CR_ACTIVATE = xlenter("CR_ACTIVATE");
  s_CR_ARM = xlenter("CR_ARM");
  s_CR_DISARM = xlenter("CR_DISARM");
  s_CR_MAP = xlenter("CR_MAP");
  s_CR_UNMAP = xlenter("CR_UNMAP");
  s_CR_FOCUS = xlenter("CR_FOCUS");
  s_CR_LOSING_FOCUS = xlenter("CR_LOSING_FOCUS");
  s_CR_MODIFYING_TEXT_VALUE = xlenter("CR_MODIFYING_TEXT_VALUE");
  s_CR_MOVING_INSERT_CURSOR = xlenter("CR_MOVING_INSERT_CURSOR");
  s_CR_EXECUTE = xlenter("CR_EXECUTE");
  s_CR_SINGLE_SELECT = xlenter("CR_SINGLE_SELECT");
  s_CR_MULTIPLE_SELECT = xlenter("CR_MULTIPLE_SELECT");
  s_CR_EXTENDED_SELECT = xlenter("CR_EXTENDED_SELECT");
  s_CR_BROWSE_SELECT = xlenter("CR_BROWSE_SELECT");
  s_CR_DEFAULT_ACTION = xlenter("CR_DEFAULT_ACTION");
  s_CR_CLIPBOARD_DATA_REQUEST = xlenter("CR_CLIPBOARD_DATA_REQUEST");
  s_CR_CLIPBOARD_DATA_DELETE = xlenter("CR_CLIPBOARD_DATA_DELETE");
  s_CR_CASCADING = xlenter("CR_CASCADING");
  s_CR_OK = xlenter("CR_OK");
  s_CR_CANCEL = xlenter("CR_CANCEL");
  s_CR_APPLY = xlenter("CR_APPLY");
  s_CR_NO_MATCH = xlenter("CR_NO_MATCH");
  s_CR_COMMAND_ENTERED = xlenter("CR_COMMAND_ENTERED");
  s_CR_COMMAND_CHANGED = xlenter("CR_COMMAND_CHANGED");
  s_CR_EXPOSE = xlenter("CR_EXPOSE");
  s_CR_RESIZE = xlenter("CR_RESIZE");
  s_CR_INPUT = xlenter("CR_INPUT");
#ifdef WINTERP_MOTIF_11
  s_CR_GAIN_PRIMARY = xlenter("CR_GAIN_PRIMARY");
  s_CR_LOSE_PRIMARY = xlenter("CR_LOSE_PRIMARY");
#ifdef XmCR_CREATE		/* added in Motif 1.1.3 */
  s_CR_CREATE = xlenter("CR_CREATE");
#endif /* XmCR_CREATE */
#ifdef XmCR_PROTOCOLS		/* added in Motif 1.1.3 */
  s_CR_PROTOCOLS = xlenter("CR_PROTOCOLS");
#endif /* XmCR_PROTOCOLS */
#endif /* WINTERP_MOTIF_11 */
#ifdef WINTERP_MOTIF_12		/* added in Motif 1.2 */
  s_CR_TEAR_OFF_ACTIVATE   = xlenter("CR_TEAR_OFF_ACTIVATE");
  s_CR_TEAR_OFF_DEACTIVATE = xlenter("CR_TEAR_OFF_DEACTIVATE");
  s_CR_OBSCURED_TRAVERSAL  = xlenter("CR_OBSCURED_TRAVERSAL");
#endif /* WINTERP_MOTIF_12 */

#ifdef HP_GRAPH_WIDGET
  s_CR_NEW_ARC = xlenter("CR_NEW_ARC");
  s_CR_NEW_NODE = xlenter("CR_NEW_NODE");
  s_CR_NODE_MOVED = xlenter("CR_NODE_MOVED");
  s_CR_ARC_MOVED = xlenter("CR_ARC_MOVED");
  s_CR_SUBGRAPH_MOVED = xlenter("CR_SUBGRAPH_MOVED");
  s_CR_ARC_EDITED = xlenter("CR_ARC_EDITED");
  s_CR_SELECT_NODE = xlenter("CR_SELECT_NODE");
  s_CR_SELECT_ARC = xlenter("CR_SELECT_ARC");
  s_CR_SELECT_SUBGRAPH = xlenter("CR_SELECT_SUBGRAPH");
  s_CR_DELETE_NODE = xlenter("CR_DELETE_NODE");
  s_CR_DELETE_ARC = xlenter("CR_DELETE_ARC");
  s_CR_SELECT= xlenter("CR_SELECT");
  s_CR_RELEASE= xlenter("CR_RELEASE");
  s_CR_NODE_DOUBLE_CLICK= xlenter("CR_NODE_DOUBLE_CLICK");
  s_CR_ARC_DOUBLE_CLICK= xlenter("CR_ARC_DOUBLE_CLICK");
  s_CR_DOUBLE_CLICK= xlenter("CR_DOUBLE_CLICK");
  s_CR_DESELECT_NODE = xlenter("CR_DESELECT_NODE");
  s_CR_DESELECT_ARC = xlenter("CR_DESELECT_ARC");
  s_CR_DESELECT = xlenter("CR_DESELECT");
  s_CR_NODES_MOVED = xlenter("CR_NODES_MOVED");
  s_CR_SELECT_NODES = xlenter("CR_SELECT_NODES");
  s_CR_SELECT_ARCS = xlenter("CR_SELECT_ARCS");
  s_CR_SELECT_ARCS_AND_NODES = xlenter("CR_SELECT_ARCS_AND_NODES");
#endif /* HP_GRAPH_WIDGET */

#ifdef SGI_DROP_POCKET_WIDGET	/* only for Irix 5.1 and IndigoMagic desktop */
  s_CR_ICON_CHANGE = xlenter("CR_ICON_CHANGE");
#endif /* SGI_DROP_POCKET_WIDGET */

  s_CALLBACK_NO_LIST = xlenter("CALLBACK_NO_LIST");
  s_CALLBACK_HAS_NONE = xlenter("CALLBACK_HAS_NONE");
  s_CALLBACK_HAS_SOME = xlenter("CALLBACK_HAS_SOME");
}

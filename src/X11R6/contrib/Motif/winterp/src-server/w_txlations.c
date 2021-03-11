/* -*-C-*-
********************************************************************************
*
* File:         w_txlations.c
* RCS:          $Header: /users/npm/src/winterp/src-server/RCS/w_txlations.c,v 2.7 1994/06/06 15:40:52 npm Exp $
* Description:  Interfaces to Xtoolkit Translation Management Facilities
* Author:       Niels Mayer
* Created:      Wed Nov 22 02:08:31 1989
* Modified:     Sun Jun  5 14:53:25 1994 (Niels Mayer) npm@indeed
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
static char rcs_identity[] = "@(#)$Header: /users/npm/src/winterp/src-server/RCS/w_txlations.c,v 2.7 1994/06/06 15:40:52 npm Exp $";

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

static LVAL s_ACTION_WIDGET, s_ACTION_XEVENT, s_LISP_ACTION_PROC;

/******************************************************************************
 * Actions in winterp are added through this single action proc which will call
 * the lisp evaluator upon invocation of this action.
 *
 * A translation/accelerator table using this proc may look like
 * "<EnterWindow>: Lisp(lisp-function arg1 arg2 arg3)" in which case the
 * form '(lisp-function arg1 arg2 arg3)' will be passed on to the evaluator.
 * 
 * In the lexical scope of the action invocation, ACTION_WIDGET gets bound  to
 * the widget causing the action, and ACTION_XEVENT gets bound to the event.
 *
 * BUG: I haven't found a way of escaping characters passed on to the
 * translation/action table parsers to allow lisp strings or lips lists to
 * be passed on to the lisp evaluator that is called by this function. Using
 * the evaluator through an action proc is still useful, just don't try to pass
 * in very complex lisp forms. Use symbolic or functional indirection to 
 * pass in complex forms.
 ******************************************************************************/
void Wtx_Winterp_Lisp_Action_Proc(widget, event, params, num_params)
     Widget widget;
     XEvent *event;
     String *params;
     Cardinal *num_params;
{
  extern int winterp_caught_signal; /* winterp.c */
  LVAL sexp_stream;
  LVAL l_val;
  LVAL tracing;
  int  mask;

  xlstkcheck(2);
  xlsave(sexp_stream);
  xlsave(l_val);

  {
    char* par;			/* points to current parameter */
    int param_count = *num_params;
    String* param_list = params;
    register char c;
    register LVAL new_elt, last_elt;

    sexp_stream = newustream();	/* note - stream obj has ptrs for head and tail*/
    last_elt = cons(cvchar('('), NIL); /* tack on an opening paren to beginning of form*/
    sethead(sexp_stream, last_elt); /* make it the head of the stream */
  
    /* for each character in each param, append it onto sexp_stream */
    while (param_count--) {	/* while there are more parameters */
      par = *param_list++;	/* get current parameter string and increment to next */
      while (c = *par++) {	/* go through each parameter char by char turning into a stream */
	new_elt = cons(cvchar(c), NIL);
	rplacd(last_elt, new_elt); /* add new elt to tail of list */
	last_elt = new_elt;
      }
      new_elt = cons(cvchar(' '), NIL);	/* put spaces between each param */
      rplacd(last_elt, new_elt);
      last_elt = new_elt;
    }
    new_elt = cons(cvchar(')'), NIL); /* tack on a closing paren to end of form */  
    rplacd(last_elt, new_elt);
    settail(sexp_stream, new_elt); /* streams are cdr-coded -- give ptr to tail */
  }

  /*
   * mutated from xleval.c:evform():
   * If (trace 'LISP_ACTION_PROC) was done, print trace info.
   */
  {
    extern LVAL s_tracelist;	/* xlisp/xlglob.c */
    if ((getvalue(s_tracelist) != NIL)
	&& xlmember(s_LISP_ACTION_PROC, getvalue(s_tracelist)))
      tracing = s_LISP_ACTION_PROC;
    else
      tracing = NIL;
  }

  if (tracing != NIL) {
    extern int xltrcindent;
    int i;
    int param_count = *num_params;
    String* param_list = params;

    /* indent to the current trace level */
    for (i = 0; i < xltrcindent; ++i)
      trcputstr(" ");
    ++xltrcindent;
    /* display the actionproc call... */
    trcputstr("Entering: ");
    trcprin1(tracing);		/* print "LISP_ACTION_PROC" */
    trcputstr(" -- widget: ");
    trcprin1(Wcls_WidgetID_To_WIDGETOBJ(widget));
    trcputstr("; code: (");
    /* for each param, print to trace strm */
    while (param_count--) {	/* while there are more parameters */
      trcputstr(*param_list++);	/* get current parameter string and increment to next */
      trcputstr(" ");
    }
    trcputstr(")\n");
  }

  /*
   * Reset winterp_caught_signal so that winterp.c:oscheck() doesn't trigger
   * off of a ^C typed in to terminal prior to some GUI action. Since this
   * procedure is called from a GUI action, we clear the flag; ^C should
   * trigger an abort only once an XLISP evaluation has started.
   * See also winterp.c:Read_From_Stream_Eval_And_Print(),
   * w_callbacks.c:Wcb_Meta_Callbackproc().
   */
  winterp_caught_signal = FALSE; 

  /*
   * Most of the rest of this procedure looks alot like xleval.c:evfun(), which
   * is what the evaluator calls when a functional form is to be evaluated. 
   * The main difference is that instead of evaling each form in the callback
   * code with xleval(), we must xlread(), then xleval() the result...
   * Also, instead of calling xlabind() to bind the formal parameter symbols
   * of a function to their values in the new lexical environment frame returned
   * by xlframe(getenvi(fun)), we just bind ACTION_WIDGET and ACTION_XEVENT.
   */
  {
    extern LVAL		xlenv, xlfenv, xldenv, xlvalue, s_unbound;
    CONTEXT		cntxt;
    LVAL		parsed_sexp;
    LVAL		oldenv, oldfenv;
#ifdef SPECIALS
    LVAL		olddenv=xldenv;
#endif

    /* protect some pointers */
    xlstkcheck(3);
    xlsave(parsed_sexp);
    xlsave(oldenv);
    xlsave(oldfenv);

    /* establish a global environment -- stolen from xlsys.c:xload(). */
    oldenv = xlenv;
    oldfenv = xlfenv;
    xlenv = xlfenv = NIL;

    /* create a new environment frame */
    xlenv = xlframe(xlenv);

    /*
     * lexically bind ACTION_WIDGET and ACTION_EVENT to the widget/event that
     * caused the callback. Similar to call to xlabind() in evfun().
     */
    xlpbind(s_ACTION_WIDGET, Wcls_WidgetID_To_WIDGETOBJ(widget), xlenv);
    xlpbind(s_ACTION_XEVENT, (event) ? cv_xevent(event) : NIL, xlenv);

    /* mutated from xleval.c:evfun() -- setup the implicit block */
    xlbegin(&cntxt, CF_TOPLEVEL|CF_CLEANUP|CF_BRKLEVEL|CF_RETURN, s_LISP_ACTION_PROC);	/* NPM: added CF_TOPLEVEL|CF_CLEANUP|CF_BRKLEVEL to make this work like a XLISP top-level loop, even though event-driven... */

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
      if (xlread(sexp_stream, &parsed_sexp)) /* if didn't hit EOF during read */
	l_val = xleval(parsed_sexp); /* then evaluate it -- we ignore evaluation result */
      else
	l_val = s_unbound;
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
    xlpopn(3);

    /* xleval.c:evfun() had return (l_val) but actionprocs don't return a value */
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
    trcprin1(tracing);		/* print "LISP_ACTION_PROC" */
    trcputstr(", Value: ");    
    trcprin1(l_val);
    trcputstr("\n");
  }

  /* restore the stack */
  xlpopn(2);			/* sexp_stream, l_val */
}


/******************************************************************************
 * (XT_PARSE_TRANSLATION_TABLE <string>)
 * ==> returns a node of type XT_TRANSLATIONS, which is a compiled form of the
 * <string> translation table given as argument.
 *
 * XtTranslations XtParseTranslationTable(source)
 *     String source;
 *
 * NOTE: the memory allocated by XtParseTranslationTable() isn't freed anywhere
 * yet -- I can't find  any documentation that says you're supposed to free
 * this. For now this may be a memory leak.
 ******************************************************************************/
LVAL Wtx_Prim_XT_PARSE_TRANSLATION_TABLE()
{
  XtTranslations txl;
  char* source = getstring(xlgastring());
  xllastarg();
  
  txl = XtParseTranslationTable(source);

  return ( (txl) ? cv_xttranslations(txl) : NIL );
}

/******************************************************************************
 * (XT_PARSE_ACCELERATOR_TABLE <string>)
 * ==> returns a node of type XT_ACCELERATORS, which is a compiled form of the
 * <string> accelerator table given as argument.
 *
 * XtAccelerators XtParseAcceleratorTable (source)
 *     String   source;
 *
 * NOTE: the memory allocated by XtParseAcceleratorTable() isn't freed anywhere
 * yet -- I can't find  any documentation that says you're supposed to free
 * this. For now this may be a memory leak.
 ******************************************************************************/
LVAL Wtx_Prim_XT_PARSE_ACCELERATOR_TABLE()
{
  XtAccelerators axl;
  char* source = getstring(xlgastring());
  xllastarg();
  
  axl = XtParseAcceleratorTable(source);

  return ( (axl) ? cv_xtaccelerators(axl) : NIL );
}


/******************************************************************************
 * (send <widget> :OVERRIDE_TRANSLATIONS <translations>)
 * ==> returns <widget>
 * 
 * This method destructively merges the new <translations> into <widget>'s
 * existing translations -- event sequences in <translations> that already exist
 * a previous translation will override.
 *
 * <translations> can be a string, in which case it
 * is compiled into a translation table. Otherwise, we expect a node of
 * type XT_TRANSLATIONS.
 *
 * void XtOverrideTranslations(widget, new)
 *     Widget widget;
 *     XtTranslations new;
 ******************************************************************************/
LVAL Widget_Class_Method_OVERRIDE_TRANSLATIONS()
{
  LVAL self, lval_txl;
  Widget widget_id;

  widget_id = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&self);
  Wxt_Error_If_Gadget(widget_id, self); /* prevent coredump on gadgets */
  lval_txl = xlgetarg();
  xllastarg();
 
  switch (ntype(lval_txl)) {
  case STRING:
    XtOverrideTranslations(widget_id,
			   XtParseTranslationTable(getstring(lval_txl)));
    break;
  case XLTYPE_XtTranslations:
    XtOverrideTranslations(widget_id,
			   get_xttranslations(lval_txl));
    break;
  default:
    xlerror("Bad argument type -- expected a string or XT_TRANSLATIONS.", lval_txl);
    break;
  }
  
  return (self);
}

/******************************************************************************
 * (send <widget> :AUGMENT_TRANSLATIONS <translations>) 
 * ==> returns <widget>.
 *
 * This method nondestructively merges the new <translations> into <widget>'s
 * existing translations -- event sequences in <translations> that already exist
 * a previous translation will be ignored.
 *
 * <translations> can be a string, in which case it
 * is compiled into a translation table. Otherwise, we expect a node of
 * type XT_TRANSLATIONS.
 *
 * void XtAugmentTranslations(widget, new)
 *     Widget widget;
 *     XtTranslations new;
 ******************************************************************************/
LVAL Widget_Class_Method_AUGMENT_TRANSLATIONS()
{
  LVAL self, lval_txl;
  Widget widget_id;

  widget_id = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&self);
  Wxt_Error_If_Gadget(widget_id, self); /* prevent coredump on gadgets */
  lval_txl = xlgetarg();
  xllastarg();
 
  switch (ntype(lval_txl)) {
  case STRING:
    XtAugmentTranslations(widget_id,
			  XtParseTranslationTable(getstring(lval_txl)));
    break;
  case XLTYPE_XtTranslations:
    XtAugmentTranslations(widget_id,
			  get_xttranslations(lval_txl));
    break;
  default:
    xlerror("Bad argument type -- expected a string or XT_TRANSLATIONS.", lval_txl);
    break;
  }

  return (self);
}

/******************************************************************************
 * (send <widget> :UNINSTALL_TRANSLATIONS)
 * ==>  returns <widget>
 *
 * This method removes all translations from <widget>.
 *
 * void XtUninstallTranslations(widget)
 *     Widget widget;
 ******************************************************************************/
LVAL Widget_Class_Method_UNINSTALL_TRANSLATIONS()
{
  LVAL self;
  Widget widget_id;

  widget_id = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&self);
  Wxt_Error_If_Gadget(widget_id, self); /* prevent coredump on gadgets */
  xllastarg();
  
  XtUninstallTranslations(widget_id);
  return (self);
}

/******************************************************************************
 * (send <destination> :INSTALL_ACCELERATORS <source>)
 * ==>  returns <destination>
 * 
 * This method installs the accelerators from widget <source> onto widget
 * <destination> by augmenting the destination translations with the source
 * accelerators.
 * 
 * void XtInstallAccelerators(destination,source)
 *     Widget destination,source;
 ******************************************************************************/
LVAL Widget_Class_Method_INSTALL_ACCELERATORS()
{
  LVAL self, lval_src;
  Widget widget_id, src;

  widget_id = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&self);
  Wxt_Error_If_Gadget(widget_id, self); /* prevent coredump on gadgets */
  src = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&lval_src);
  xllastarg();
  
  XtInstallAccelerators(widget_id, src);
  return (self);
}


/******************************************************************************
 * (send <destination> :INSTALL_ALL_ACCELERATORS <source>)
 * ==>  returns <destination>
 * 
 * This method installs the accelerators from widget <source> and all it's 
 * children onto widget <destination> by augmenting the destination translations
 * with the source accelerators.
 *
 * void XtInstallAllAccelerators(destination,source)
 *    Widget destination,source;
 ******************************************************************************/
LVAL Widget_Class_Method_INSTALL_ALL_ACCELERATORS()
{
  LVAL self, lval_src;
  Widget widget_id, src;

  widget_id = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&self);
  Wxt_Error_If_Gadget(widget_id, self); /* prevent coredump on gadgets */
  src = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&lval_src);
  xllastarg();
  
  XtInstallAllAccelerators(widget_id, src);
  return (self);
}


#ifdef WINTERP_MOTIF_11
/******************************************************************************
 * (send <widget> :CALL_ACTION_PROC <action> <event> [<param0> [<param1> [...] ]])
 * ==>  returns <widget>
 * 
 *
 * void XtCallActionProc(Widget		-* widget *-,
 *			 CONST String	-* action *-,
 *			 XEvent*	-* event *-,
 *			 String*	-* params *-,
 *			 Cardinal	-* num_params *-);
 ******************************************************************************/
LVAL Widget_Class_Method_CALL_ACTION_PROC()
{
#define PARAMS_SIZE_INCREMENT 5
  LVAL self;
  Widget widget_id;
  String action;
  XEvent* event;
  String* params	= (String*) NULL;
  Cardinal num_params	= 0;
  Cardinal params_size	= 0;

  widget_id = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&self); /* get <widget> */
  Wxt_Error_If_Gadget(widget_id, self); /* prevent coredump on gadgets */
  action = getstring(xlgastring()); /* get <action> */
  event = get_xevent(xlga_xevent()); /* get <xevent> */

  /* get optional  [<param0> [<param1> [...] ]] */
  while (moreargs()) {
    if (num_params >= params_size) { /* make sure it'll fit into allocated table */
      params_size += PARAMS_SIZE_INCREMENT;
      params
	= (String*) XtRealloc((char*) params, (unsigned) (params_size * sizeof(String)));
    }
    params[num_params++] = getstring(xlgastring());
  }
  xllastarg();
  
  XtCallActionProc(widget_id, action, event, params, num_params);

  if (params)
    XtFree((char*) params);

  return (self);
#undef PARAMS_SIZE_INCREMENT
}
#endif /* WINTERP_MOTIF_11 */


/******************************************************************************
 *
 ******************************************************************************/
Wtx_Init()
{
  s_ACTION_WIDGET = xlenter("ACTION_WIDGET");
  s_ACTION_XEVENT = xlenter("ACTION_XEVENT");
  s_LISP_ACTION_PROC = xlenter("LISP_ACTION_PROC");
}

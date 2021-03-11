/* -*-C-*-
********************************************************************************
*
* File:         w_evnthndlr.c
* RCS:          $Header: /users/npm/src/winterp/src-server/RCS/w_evnthndlr.c,v 2.5 1994/06/06 15:41:00 npm Exp $
* Description:  Interfaces to Xtoolkit Event Handler Routines
* Author:       Niels Mayer
* Created:      Thu Nov 23 06:11:39 1989
* Modified:     Sun Jun  5 14:45:30 1994 (Niels Mayer) npm@indeed
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
static char rcs_identity[] = "@(#)$Header: /users/npm/src/winterp/src-server/RCS/w_evnthndlr.c,v 2.5 1994/06/06 15:41:00 npm Exp $";

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

static LVAL k_RAW, k_NONMASKABLE,
  s_EVHANDLER_WIDGET, s_EVHANDLER_XEVENT, s_EVHANDLER_BUTTON, s_EVHANDLER_TIME,
  s_EVHANDLER;

#define RAW_OPTION         (1L<<0)
#define NONMASKABLE_OPTION (1L<<1)


/******************************************************************************
 * This is called whenever we trace an eventhandler, e.g. 
 * (trace 'EVHANDLER).
 * For eventhandlers only, this proc is passed in as the 'trace_enter_proc' 
 * in Wcb_Meta_Callbackproc().
 ******************************************************************************/
static void Weh_Event_Handler_Trace_Proc(tracing, client_data)
     LVAL tracing;		/* SYMBOL */
     LVAL client_data;		/* EVHANDLEROBJ */
{
  extern int xltrcindent;
  int i;

  /* indent to the current trace level */
  for (i = 0; i < xltrcindent; ++i)
    trcputstr(" ");
  ++xltrcindent;
  /* display the callback call... */
  trcputstr("Entering: ");
  if (get_evhandler_options(client_data) & RAW_OPTION)
    trcputstr("RAW ");
  if (get_evhandler_options(client_data) & NONMASKABLE_OPTION)
    trcputstr("NON-MASKABLE ");
  trcprin1(tracing);		/* print "EVHANDLER" */
  trcputstr(" -- widget = ");
  trcprin1(get_evhandler_widget(client_data)); /* print WIDGETOBJ */
  sprintf(temptext, "; mask = 0x%lx\n",
	  (unsigned long) get_evhandler_mask(client_data));
  trcputstr(temptext);
}


/******************************************************************************
 * Bind local variables specified during call to :SET/:ADD_EVENT_HANDLER
 * -- see also xlabind().
 * For eventhandlers only, this proc is passed in as the
 * 'bind_call_data_values_proc' in Wcb_Meta_Callbackproc().
 ******************************************************************************/
static void Weh_Lexical_Bindings_For_Event_Handler(bindings_list,
						   lexical_env,
						   call_data,
						   client_data)
     LVAL bindings_list;
     LVAL lexical_env;
     XtPointer call_data;	/* XEvent* */
     LVAL client_data;		/* EVHANDLEROBJ */
{ 
  register LVAL s_bindname;

  for ( ; consp(bindings_list); bindings_list = cdr(bindings_list)) {

    s_bindname = car(bindings_list);

    if (s_bindname == s_EVHANDLER_WIDGET) {
      xlpbind(s_bindname, get_evhandler_widget(client_data), lexical_env);
    }
    else if (s_bindname == s_EVHANDLER_BUTTON) {
      xlpbind(s_bindname, (call_data) ? cvfixnum((FIXTYPE) ((XButtonEvent*) call_data)->button) : NIL, lexical_env);
    }
    else if (s_bindname == s_EVHANDLER_TIME) {
      xlpbind(s_bindname, (call_data) ? cvfixnum((FIXTYPE) ((XButtonEvent*) call_data)->time) : NIL, lexical_env);
    }
    else if (s_bindname == s_EVHANDLER_XEVENT) {
      xlpbind(s_bindname, (call_data) ? cv_xevent((XEvent*) call_data) : NIL, lexical_env);
    }
    else {
      errputstr("Warning: in widget event-handler, ignored unknown binding name - ");
      errprint(s_bindname);
      sprintf(temptext,
	      "\tValid event-handler symbols are [%s %s %s %s] - ",
	      getstring(getpname(s_EVHANDLER_WIDGET)),
	      getstring(getpname(s_EVHANDLER_BUTTON)),
	      getstring(getpname(s_EVHANDLER_TIME)),
	      getstring(getpname(s_EVHANDLER_XEVENT)));
      errputstr(temptext);
      errprint(get_evhandler_widget(client_data));
    }
  }
}


/******************************************************************************
 * This is an XtEventHandler procedure that is called by the various add-
 * event-handler methods in this file. It serves to call the lisp evaluator
 * on the lexical closure that was created by the add-event-handler-methods.
 * It will bind the symbols EVHANDLER_WIDGET EVHANDLER_XEVENT to
 * the widget and xevent that caused the event handler to fire.
******************************************************************************/
#ifdef WINTERP_MOTIF_11		/* change is really due to X11r4... */
static void Winterp_Event_Handler_Proc(widget_id, client_data, event, continue_to_dispatch)
     Widget    widget_id;
     XtPointer client_data;	/* XLTYPE_EVHANDLEROBJ */
     XEvent*   event;
     Boolean*  continue_to_dispatch; /* currently not supported... */
#else /* MOTIF 1.0 and X11r3 */
static void Winterp_Event_Handler_Proc(widget_id, client_data, event)
     Widget    widget_id;
     XtPointer client_data;	/* XLTYPE_EVHANDLEROBJ */
     XEvent*   event;
#endif /* WINTERP_MOTIF_11 */
{
  Wcb_Meta_Callbackproc(get_evhandler_closure((LVAL) client_data),
			(LVAL) client_data,
			(XtPointer) event, /* call_data */
			Weh_Lexical_Bindings_For_Event_Handler,
			NULL, /* TODO -- add proc to change 'continue_to_dispatch' if assoc'd-symbol's value changed */
			Weh_Event_Handler_Trace_Proc);
}


/******************************************************************************
 * (send <widget> :ADD_EVENT_HANDLER [:RAW] [:NONMASKABLE] <event_mask>
 *                                   <event_data_bindings_list> <code>)
 * (send <widget> :SET_EVENT_HANDLER [:RAW] [:NONMASKABLE] <event_mask>
 *                                   <event_data_bindings_list> <code>)
 * ==> returns an <EVHANDLEROBJ> which identifies the handler.
 *
 * Optional keyword :RAW indicates that XtAddRawEventHandler() will
 * be invoked so as to not affect the widget's input mask nor for
 * it to select for events. Otherwise XtAddEventHandler() will get 
 * called.
 *
 * Optional keyword :NONMASKABLE indicates that the handler should
 * be called for nonmaskable events.
 * 
 * <event_mask> is a FIXNUM which is the value of the an event mask.
 * event masks may be combined by doing a bitwise or via the xlisp
 * 'logior' function. The following event mask constants have been
 * defined: NO_EVENT_MASK, KEY_PRESS_MASK, KEY_RELEASE_MASK,
 * BUTTON_PRESS_MASK, BUTTON_RELEASE_MASK, ENTER_WINDOW_MASK,
 * LEAVE_WINDOW_MASK, POINTER_MOTION_MASK, POINTER_MOTIONHINT_MASK,
 * BUTTON1_MOTION_MASK, BUTTON2_MOTION_MASK, BUTTON3_MOTION_MASK,
 * BUTTON4_MOTION_MASK, BUTTON5_MOTION_MASK, BUTTON_MOTION_MASK,
 * KEYMAP_STATE_MASK, EXPOSURE_MASK,  VISIBILITY_CHANGE_MASK
 * STRUCTURE_NOTIFY_MASK, RESIZE_REDIRECT_MASK, SUBSTRUCTURE_NOTIFY_MASK
 * SUBSTRUCTURE_REDIRECT_MASK, FOCUS_CHANGE_MASK, PROPERTY_CHANGE_MASK
 * COLORMAP_CHANGE_MASK, OWNER_GRAB_BUTTON_MASK.
 *
 * <event_data_bindings_list> is a list of symbols that get
 * bound to data specific to the action that caused the event
 * handler to fire. These symbols get lexically bound during
 * the execution of the event-handler <code>.
 * Valid symbols are:
 * EVHANDLER_WIDGET -- bound to the widgetobj of the event
 * EVHANDLER_XEVENT -- bound to the XEvent that fired handler.
 * EVHANDLER_BUTTON -- fixnum -- the button or keycode
 * EVHANDLER_TIME   -- fixnum -- the timestamp.
 *
 * <code> is a list of lisp-forms to be evaluated when the eventhandler
 * fires.
 *
 * The eventhandler may be removed by giving the <EVHANDLEROBJ> to
 * procedure XT_REMOVE_EVENT_HANDLER.
 *
 * Note that the :SET_EVENT_HANDLER variant does exactly the same
 * thing as :ADD_EVENT_HANDLER except that it ensure that only one
 * event handler with the given <event_mask>, :RAW and :NONMASKABLE
 * specifications exist on <widget>. It will remove all other matching
 * event handlers in order to set the current event handler. THis
 * function is useful for making interactive changes to an event
 * handler without having to remember to remove the previous
 * handler. Note that :SET_EVENT_HANDLER is slower, so it should 
 * not be used in cases where speed is important.
 *
 * NOTE: the EVHANDLER_TIME and EVHANDLER_BUTTON
 * binding values may return gibberish if the eventhandler returned
 * a event->type that doesn't define those fields. THis is
 * currently just a kludge for motif functions that require
 * the event->button info (popup menus) or event->time info
 * (XmClipboard*). Hopefully I'll come up with something better.
 *
 * void XtAddEventHandler(widget, eventMask, other, proc, closure)
 *     Widget	       widget;
 *     EventMask       eventMask;
 *     Boolean         other;
 *     XtEventHandler  proc;
 *     XtPointer       closure;
 * 
 * void XtAddRawEventHandler(widget, eventMask, other, proc, closure)
 *     Widget	       widget;
 *     EventMask       eventMask;
 *     Boolean         other;
 *     XtEventHandler  proc;
 *     XtPointer       closure;
 ******************************************************************************/
static LVAL Widget_Class_Meta_Method_Add_Event_Handler(one_evhandler_per_mask_p)
     Boolean one_evhandler_per_mask_p;
{
  extern LVAL s_lambda, xlenv, xlfenv;
  Boolean raw_p, nonmaskable_p;
  EventMask event_mask;
  Widget widget_id;
  LVAL o_self, l_fargs, l_code, evhandler_obj;

  /* get <widget_instance> */
  widget_id = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&o_self);
  Wxt_Error_If_Gadget(widget_id, o_self); /* prevent coredump on gadgets */

  /* get optional :RAW or :NONMASKABLE keyword args */
  raw_p = nonmaskable_p = FALSE;
  if (moreargs() && ((*xlargv == k_RAW))) {
    nextarg();
    raw_p = TRUE;
  }
  if (moreargs() && ((*xlargv == k_NONMASKABLE))) {
    nextarg();
    nonmaskable_p = TRUE;
  }
  
  /* get required <event_mask> arg, a fixnum */
  event_mask = (EventMask) getfixnum(xlgafixnum());

  /* get <event_data_bindings_list> -- args to be bound at call time.
     NOTE: may want to check that these args are valid... No biggie
     though invalid names will get get caught at runtime,
     when the eventhandler fires. */
  l_fargs = xlgalist();		
  
  /* get <code> */
  l_code = xlgalist();
  xllastarg();

  /* 
   * if this procedure is being called from the :set_event_handler method 
   * (indicated by one_evhandler_per_mask_p == TRUE), 
   * then remove all eventhandlers on <widget> matching <event_mask>
   * <raw_p> and <nonmaskable_p>.
   */
  if (one_evhandler_per_mask_p) {
    int  i = Wso_Hash(o_self);
    LVAL l_hbucket = getelement(v_savedobjs, i); /* a list of objects, including all evhandler-objs on this widget */
    LVAL obj;
    LVAL l_prev = NIL;
    while (l_hbucket != NIL) {	/* while there are elements in the hashbucket */
      obj = car(l_hbucket);	/* obj points to cur elt */
      if (evhandlerobj_p(obj)
	  && (get_evhandler_widget(obj) == o_self)
	  && (get_evhandler_mask(obj) == event_mask)
	  && (((get_evhandler_options(obj) & RAW_OPTION) ? TRUE : FALSE) == raw_p)
	  && (((get_evhandler_options(obj) & NONMASKABLE_OPTION) ? TRUE : FALSE) == nonmaskable_p)
	  ) {
	if (get_evhandler_options(obj) & RAW_OPTION)
	  XtRemoveRawEventHandler(widget_id, event_mask,
				  ((get_evhandler_options(obj) & NONMASKABLE_OPTION) ? TRUE : FALSE),
				  Winterp_Event_Handler_Proc, (XtPointer) obj);
	else
	  XtRemoveEventHandler(widget_id, event_mask,
			       ((get_evhandler_options(obj) & NONMASKABLE_OPTION) ? TRUE : FALSE),
			       Winterp_Event_Handler_Proc, (XtPointer) obj);

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
   * create the client_data to be sent to (*XtEventHandler)()
   * That procedure takes the client_data and extracts the widget-object,
   * and the closure, and uses these to execute the event handler's code.
   */
  xlsave1(evhandler_obj);	/* protect some pointers */
  evhandler_obj = new_evhandlerobj();
  set_evhandler_widget(evhandler_obj, o_self);
  set_evhandler_mask(evhandler_obj, event_mask);
  {
    long options = 0L;
    if (raw_p)
      options |= RAW_OPTION;	/* this bit tells XT_REMOVE_EVENT_HANDLER which removeproc to call */
    if (nonmaskable_p)
      options |= NONMASKABLE_OPTION; /* we need this bit for XT_REMOVE_EVENT_HANDLER */
    set_evhandler_options(evhandler_obj, options);
  }
  set_evhandler_closure(evhandler_obj, 
			xlclose(s_EVHANDLER, s_lambda, l_fargs, l_code, xlenv, xlfenv));
  
  if (raw_p)
    XtAddRawEventHandler(widget_id, event_mask, nonmaskable_p, 
			 Winterp_Event_Handler_Proc, (XtPointer) evhandler_obj);
  else
    XtAddEventHandler(widget_id, event_mask, nonmaskable_p, 
		      Winterp_Event_Handler_Proc, (XtPointer) evhandler_obj);

  /*
   * Enter the evhandler_obj in v_savedobjs, so that it gets marked.
   * This way, it won't be garbage collected while the eventhandler is
   * active. XT_REMOVE_EVENT_HANDLER, and :destroy on the widget will
   * end up removing the evhandlerobj from v_savedobjs,  which will
   * permit it to be garbage collected.
   */
  { 
    int  i = Wso_Hash(o_self);
    LVAL l_hbucket;
    
    xlsave1(l_hbucket);
    l_hbucket = cons(evhandler_obj, getelement(v_savedobjs, i));
    setelement(v_savedobjs, i, l_hbucket);
    xlpop(/*l_hbucket*/);
  }

  xlpop(/*evhandler_obj*/);
  return (evhandler_obj);
}

LVAL Widget_Class_Method_ADD_EVENT_HANDLER()
{
  return (Widget_Class_Meta_Method_Add_Event_Handler(FALSE));
}

LVAL Widget_Class_Method_SET_EVENT_HANDLER()
{
  return (Widget_Class_Meta_Method_Add_Event_Handler(TRUE));
}


/******************************************************************************
 * (XT_REMOVE_EVENT_HANDLER <EVHANDEROBJ>)
 * returns true.
 *
 * This procedure removes the eventhander corresponding to the 
 * <EVHANDLEROBJ> returned by method :ADD_EVENT_HANDLER
 *
 * void XtRemoveEventHandler(widget, eventMask, other, proc, closure)
 *     Widget	      widget;
 *     EventMask      eventMask;
 *     Boolean	      other;
 *     XtEventHandler proc;
 *     XtPointer      closure;
 *
 * void XtRemoveRawEventHandler(widget, eventMask, other, proc, closure)
 *     Widget	        widget;
 *     EventMask        eventMask;
 *     Boolean	        other;
 *     XtEventHandler   proc;
 *     XtPointer	closure;
 ******************************************************************************/
LVAL Weh_Prim_XT_REMOVE_EVENT_HANDLER()
{
  LVAL evhandler_obj;
  LVAL o_widget;
  Widget widget_id;
  Boolean raw_p, nonmaskable_p;
  extern LVAL true;

  evhandler_obj = xlga_evhandlerobj();
  xllastarg();

  /* check if this evhandler hasn't already been removed */
  if ((o_widget = get_evhandler_widget(evhandler_obj)) == NIL)
    xlerror("EventHandler associated with <evhandlerobj> has already been removed.", evhandler_obj);
  
  /* mark the evhandler_obj as being removed */
  set_evhandler_widget(evhandler_obj, NIL);
  
  {
    long options;
    options = get_evhandler_options(evhandler_obj);
    raw_p = (options & RAW_OPTION) ? TRUE : FALSE;
    nonmaskable_p = (options & NONMASKABLE_OPTION) ? TRUE : FALSE;
  }

  if (!(widget_id = get_widgetobj_widgetID(o_widget)))
    xlerror("widget object not properly initialized by :isnew.", o_widget);
  
  if (raw_p)
    XtRemoveRawEventHandler(widget_id, get_evhandler_mask(evhandler_obj),
			    nonmaskable_p, Winterp_Event_Handler_Proc,
			    (XtPointer) evhandler_obj);
  else
    XtRemoveEventHandler(widget_id, get_evhandler_mask(evhandler_obj),
			 nonmaskable_p, Winterp_Event_Handler_Proc,
			 (XtPointer) evhandler_obj);
			    
  /* remove <evhandler_obj> from v_savedobjs allowing it to be garbage collected */
  {
    int i = Wso_Hash(o_widget);	/* note that we hash all evhandlers on the same widget to the same hashbucket */
    LVAL l_hbucket = getelement(v_savedobjs, i);
    LVAL l_prev = NIL;

    while ((l_hbucket != NIL) && (car(l_hbucket) != evhandler_obj)) {
      l_prev = l_hbucket;
      l_hbucket = cdr(l_hbucket);
    }
    if (l_hbucket == NIL)
      xlerror("Internal error in XT_REMOVE_EVENT_HANDLER -- couldn't remove <evhandlerobj> from v_savedobjs. Hash error?",
	      evhandler_obj);
    if (l_prev == NIL)		/* first elt matched */
      setelement(v_savedobjs, i, cdr(l_hbucket));
    else
      rplacd(l_prev, cdr(l_hbucket));
  }
  
  return (true);
}


/******************************************************************************
 * (send <widget> :BUILD_EVENT_MASK)
 * ==> this returns as a FIXNUM the event mask representing the logical OR of
 * all event masks for event handlers registered on <widget>. This includes
 * masks set by XtAddEventHandler(), all event translations & accelerators.
 * 
 * EventMask XtBuildEventMask(widget)
 *     Widget widget;
 ******************************************************************************/
LVAL Widget_Class_Method_BUILD_EVENT_MASK()
{
  LVAL self;
  Widget widget_id;

  widget_id = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&self);
  Wxt_Error_If_Gadget(widget_id, self);	/* prevent coredump on gadgets */
  xllastarg();
  
  return (cvfixnum((FIXTYPE) XtBuildEventMask(widget_id)));
}


/******************************************************************************
 *
 ******************************************************************************/
LVAL Weh_Init()
{
  LVAL s_NO_EVENT_MASK = xlenter("NO_EVENT_MASK");
  LVAL s_KEY_PRESS_MASK = xlenter("KEY_PRESS_MASK");
  LVAL s_KEY_RELEASE_MASK = xlenter("KEY_RELEASE_MASK");
  LVAL s_BUTTON_PRESS_MASK = xlenter("BUTTON_PRESS_MASK");
  LVAL s_BUTTON_RELEASE_MASK = xlenter("BUTTON_RELEASE_MASK");
  LVAL s_ENTER_WINDOW_MASK = xlenter("ENTER_WINDOW_MASK");
  LVAL s_LEAVE_WINDOW_MASK = xlenter("LEAVE_WINDOW_MASK");
  LVAL s_POINTER_MOTION_MASK = xlenter("POINTER_MOTION_MASK");
  LVAL s_POINTER_MOTIONHINT_MASK = xlenter("POINTER_MOTIONHINT_MASK");
  LVAL s_BUTTON1_MOTION_MASK = xlenter("BUTTON1_MOTION_MASK");
  LVAL s_BUTTON2_MOTION_MASK = xlenter("BUTTON2_MOTION_MASK");
  LVAL s_BUTTON3_MOTION_MASK = xlenter("BUTTON3_MOTION_MASK");
  LVAL s_BUTTON4_MOTION_MASK = xlenter("BUTTON4_MOTION_MASK");
  LVAL s_BUTTON5_MOTION_MASK = xlenter("BUTTON5_MOTION_MASK");
  LVAL s_BUTTON_MOTION_MASK = xlenter("BUTTON_MOTION_MASK");
  LVAL s_KEYMAP_STATE_MASK = xlenter("KEYMAP_STATE_MASK");
  LVAL s_EXPOSURE_MASK = xlenter("EXPOSURE_MASK");
  LVAL s_VISIBILITY_CHANGE_MASK = xlenter("VISIBILITY_CHANGE_MASK");
  LVAL s_STRUCTURE_NOTIFY_MASK = xlenter("STRUCTURE_NOTIFY_MASK");
  LVAL s_RESIZE_REDIRECT_MASK = xlenter("RESIZE_REDIRECT_MASK");
  LVAL s_SUBSTRUCTURE_NOTIFY_MASK = xlenter("SUBSTRUCTURE_NOTIFY_MASK");
  LVAL s_SUBSTRUCTURE_REDIRECT_MASK = xlenter("SUBSTRUCTURE_REDIRECT_MASK");
  LVAL s_FOCUS_CHANGE_MASK = xlenter("FOCUS_CHANGE_MASK");
  LVAL s_PROPERTY_CHANGE_MASK = xlenter("PROPERTY_CHANGE_MASK");
  LVAL s_COLORMAP_CHANGE_MASK = xlenter("COLORMAP_CHANGE_MASK");
  LVAL s_OWNER_GRAB_BUTTON_MASK = xlenter("OWNER_GRAB_BUTTON_MASK");

  defconstant(s_NO_EVENT_MASK, cvfixnum((FIXTYPE) NoEventMask));
  defconstant(s_KEY_PRESS_MASK, cvfixnum((FIXTYPE) KeyPressMask));
  defconstant(s_KEY_RELEASE_MASK, cvfixnum((FIXTYPE) KeyReleaseMask));
  defconstant(s_BUTTON_PRESS_MASK, cvfixnum((FIXTYPE) ButtonPressMask));
  defconstant(s_BUTTON_RELEASE_MASK, cvfixnum((FIXTYPE) ButtonReleaseMask));
  defconstant(s_ENTER_WINDOW_MASK, cvfixnum((FIXTYPE) EnterWindowMask));
  defconstant(s_LEAVE_WINDOW_MASK, cvfixnum((FIXTYPE) LeaveWindowMask));
  defconstant(s_POINTER_MOTION_MASK, cvfixnum((FIXTYPE) PointerMotionMask));
  defconstant(s_POINTER_MOTIONHINT_MASK, cvfixnum((FIXTYPE) PointerMotionHintMask));
  defconstant(s_BUTTON1_MOTION_MASK, cvfixnum((FIXTYPE) Button1MotionMask));
  defconstant(s_BUTTON2_MOTION_MASK, cvfixnum((FIXTYPE) Button2MotionMask));
  defconstant(s_BUTTON3_MOTION_MASK, cvfixnum((FIXTYPE) Button3MotionMask));
  defconstant(s_BUTTON4_MOTION_MASK, cvfixnum((FIXTYPE) Button4MotionMask));
  defconstant(s_BUTTON5_MOTION_MASK, cvfixnum((FIXTYPE) Button5MotionMask));
  defconstant(s_BUTTON_MOTION_MASK, cvfixnum((FIXTYPE) ButtonMotionMask));
  defconstant(s_KEYMAP_STATE_MASK, cvfixnum((FIXTYPE) KeymapStateMask));
  defconstant(s_EXPOSURE_MASK, cvfixnum((FIXTYPE) ExposureMask));
  defconstant(s_VISIBILITY_CHANGE_MASK, cvfixnum((FIXTYPE) VisibilityChangeMask));
  defconstant(s_STRUCTURE_NOTIFY_MASK, cvfixnum((FIXTYPE) StructureNotifyMask));
  defconstant(s_RESIZE_REDIRECT_MASK, cvfixnum((FIXTYPE) ResizeRedirectMask));
  defconstant(s_SUBSTRUCTURE_NOTIFY_MASK, cvfixnum((FIXTYPE) SubstructureNotifyMask));
  defconstant(s_SUBSTRUCTURE_REDIRECT_MASK, cvfixnum((FIXTYPE) SubstructureRedirectMask));
  defconstant(s_FOCUS_CHANGE_MASK, cvfixnum((FIXTYPE) FocusChangeMask));
  defconstant(s_PROPERTY_CHANGE_MASK, cvfixnum((FIXTYPE) PropertyChangeMask));
  defconstant(s_COLORMAP_CHANGE_MASK, cvfixnum((FIXTYPE) ColormapChangeMask));
  defconstant(s_OWNER_GRAB_BUTTON_MASK, cvfixnum((FIXTYPE) OwnerGrabButtonMask));

  k_RAW              = xlenter(":RAW");
  k_NONMASKABLE      = xlenter(":NONMASKABLE");
  s_EVHANDLER_WIDGET = xlenter("EVHANDLER_WIDGET");
  s_EVHANDLER_XEVENT = xlenter("EVHANDLER_XEVENT");
  s_EVHANDLER_BUTTON = xlenter("EVHANDLER_BUTTON");
  s_EVHANDLER_TIME   = xlenter("EVHANDLER_TIME");
  s_EVHANDLER        = xlenter("EVHANDLER");
}  

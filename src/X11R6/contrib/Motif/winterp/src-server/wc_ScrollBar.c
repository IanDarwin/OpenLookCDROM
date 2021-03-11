/* -*-C-*-
********************************************************************************
*
* File:         wc_ScrollBar.c
* RCS:          $Header: /users/npm/src/winterp/src-server/RCS/wc_ScrollBar.c,v 2.6 1994/06/06 15:40:42 npm Exp $
* Description:  XM_SCROLL_BAR_WIDGET_CLASS
* Author:       Niels Mayer
* Created:      Sat Oct 28 04:35:49 1989
* Modified:     Sun Jun  5 15:05:28 1994 (Niels Mayer) npm@indeed
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
static char rcs_identity[] = "@(#)$Header: /users/npm/src/winterp/src-server/RCS/wc_ScrollBar.c,v 2.6 1994/06/06 15:40:42 npm Exp $";

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
#include <Xm/ScrollBar.h>
#include "winterp.h"
#include "w_funtab.h"


/******************************************************************************
 * typedef struct
 * {
 *    int reason;
 *    XEvent * event;
 *    int value;                 -- new slider location value
 *    int pixel;                 -- this is a stupid name: contains the coord of the XmNto{Top,Bottom}Callback
 * } XmScrollBarCallbackStruct;
 ******************************************************************************/
static LVAL s_CALLBACK_PIXEL;	/* initialized below in Wc_ScrollBar_Init() */
static void Lexical_Bindings_For_XmScrollBarCallbackStruct(bindings_list,
							   lexical_env,
							   call_data,
							   client_data)
     LVAL bindings_list;	/* a list of symbols to which values from XmScrollBarCallbackStruct are bound */
     LVAL lexical_env;		
     XtPointer call_data;
     LVAL client_data;		/* XLTYPE_CALLBACKOBJ */
{
  register LVAL s_bindname;
  XmScrollBarCallbackStruct* cd;

  if (!call_data) {		/* for Core(:XMN_DESTROY_CALLBACK) */
    for ( ; consp(bindings_list); bindings_list = cdr(bindings_list)) {
      s_bindname = car(bindings_list);
      if (s_bindname == s_CALLBACK_WIDGET) { /* if missing call_data, then only value we can retrieve is the CALLBACK_WIDGET */
	xlpbind(s_bindname, get_callback_widget(client_data), lexical_env);
      }
      else {			/* attempting to retrieve any other value will cause "NULL call-data error" */
	Wcb_Callback_Missing_Call_Data_Error(client_data, s_bindname); 
      }
    }
  }
  else
    switch (((XmAnyCallbackStruct*) call_data)->reason) {
    case XmCR_DECREMENT:	/* XmScrollBar(:XMN_DECREMENT_CALLBACK) */
    case XmCR_DRAG:		/* XmScrollBar(:XMN_DRAG_CALLBACK) */
    case XmCR_INCREMENT:	/* XmScrollBar(:XMN_INCREMENT_CALLBACK) */
    case XmCR_PAGE_DECREMENT:	/* XmScrollBar(:XMN_PAGE_DECREMENT_CALLBACK) */
    case XmCR_PAGE_INCREMENT:	/* XmScrollBar(:XMN_PAGE_INCREMENT_CALLBACK) */
    case XmCR_TO_BOTTOM:	/* XmScrollBar(:XMN_TO_BOTTOM_CALLBACK) */
    case XmCR_TO_TOP:		/* XmScrollBar(:XMN_TO_TOP_CALLBACK) */
    case XmCR_VALUE_CHANGED:	/* XmScrollBar(:XMN_VALUE_CHANGED_CALLBACK) */
      cd = (XmScrollBarCallbackStruct*) call_data;
      for ( ; consp(bindings_list); bindings_list = cdr(bindings_list)) {
	s_bindname = car(bindings_list);
	if (s_bindname == s_CALLBACK_WIDGET) {
	  xlpbind(s_bindname, get_callback_widget(client_data), lexical_env);
	}
	else if (s_bindname == s_CALLBACK_REASON) {
	  xlpbind(s_bindname, Wcb_Get_Callback_Reason_Symbol(cd->reason), lexical_env);
	}
	else if (s_bindname == s_CALLBACK_XEVENT) {
	  xlpbind(s_bindname, (cd->event) ? cv_xevent(cd->event) : NIL, lexical_env);
	}
	else if (s_bindname == s_CALLBACK_VALUE) {
	  xlpbind(s_bindname, cvfixnum((FIXTYPE) cd->value), lexical_env);
	}
	else if (s_bindname == s_CALLBACK_PIXEL) {
	  xlpbind(s_bindname, cvfixnum((FIXTYPE) cd->pixel), lexical_env);
	}
	else {
	  errputstr("Warning: in widget callback, ignored unknown binding name - ");
	  errprint(s_bindname);
	  sprintf(temptext,
		  "\tValid symbols for XmScrollBarCallbackStruct are [%s %s %s %s %s] - ",
		  getstring(getpname(s_CALLBACK_WIDGET)),
		  getstring(getpname(s_CALLBACK_REASON)),
		  getstring(getpname(s_CALLBACK_XEVENT)),
		  getstring(getpname(s_CALLBACK_VALUE)),
		  getstring(getpname(s_CALLBACK_PIXEL)));
	  errputstr(temptext);
	  errprint(get_callback_widget(client_data));
	}
      }
      break;
    default:			/* for XmPrimitive(:XMN_HELP_CALLBACK) */
      Lexical_Bindings_For_XmAnyCallbackStruct(bindings_list, lexical_env, call_data, client_data);
      break;
    }
}


/******************************************************************************
 * This is called indirectly via XtAddCallback() for callbacks returning
 * an XmScrollBarCallbackStruct as call_data. 
 ******************************************************************************/
static void XmScrollBarCallbackStruct_Callbackproc(widget, client_data, call_data)
     Widget    widget;
     XtPointer client_data;
     XtPointer call_data;
{
  Wcb_Meta_Callbackproc(get_callback_closure((LVAL) client_data),
			(LVAL) client_data,
			call_data,
			Lexical_Bindings_For_XmScrollBarCallbackStruct,
			NULL,
			Wcb_Callback_Trace_Proc);
}


/******************************************************************************
 * Same as WIDGET_CLASS's :add_callback method except that this understands
 * how to get values from the XmScrollBarCallbackStruct.
 * Specifying one or more of the following symbols in the callback bindings 
 * list will bind that symbol's value in the lexical environment of the callback:
 * CALLBACK_WIDGET
 * CALLBACK_REASON
 * CALLBACK_XEVENT
 * CALLBACK_VALUE
 * CALLBACK_PIXEL
 ******************************************************************************/
LVAL Xm_Scroll_Bar_Widget_Class_Method_ADD_CALLBACK()
{
  return (Wcb_Meta_Method_Add_Callback(XmScrollBarCallbackStruct_Callbackproc, FALSE));
}


/******************************************************************************
 * Same as WIDGET_CLASS's :set_callback method except that this understands
 * how to get values from the XmScrollBarCallbackStruct.
 * Specifying one or more of the following symbols in the callback bindings 
 * list will bind that symbol's value in the lexical environment of the callback:
 * CALLBACK_WIDGET
 * CALLBACK_REASON
 * CALLBACK_XEVENT
 * CALLBACK_VALUE
 * CALLBACK_PIXEL
 ******************************************************************************/
LVAL Xm_Scroll_Bar_Widget_Class_Method_SET_CALLBACK()
{
  return (Wcb_Meta_Method_Add_Callback(XmScrollBarCallbackStruct_Callbackproc, TRUE));
}


/******************************************************************************
 * (send <scrollbar_widget> :GET_VALUE) 
 * ==> returns a list (<value> <slider_size> <increment> <page_increment>)
 * all of which are FIXNUMs representing  the values of resources
 * :XMN_VALUE, :XMN_SLIDER_SIZE, :XMN_INCREMENT, and :XMN_PAGE_INCREMENT
 * respectively.
 *
 * Note that I couldn't call this method :GET_VALUES because it would override
 * method :GET_VALUES on WIDGET_CLASS.
 *
 * void XmScrollBarGetValues (w, value, slider_size, increment, page_increment)
 *	Widget w;
 *	int *value;
 *	int *slider_size;
 *	int *increment;
 *	int *page_increment;
 ******************************************************************************/
LVAL Xm_Scroll_Bar_Widget_Class_Method_GET_VALUE()
{
  LVAL self, result;
  Widget widget_id;
  int value, slider_size, increment, page_increment;
  
  widget_id = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&self);
  xllastarg();

  XmScrollBarGetValues(widget_id, &value, &slider_size, &increment, &page_increment);

  xlsave1(result);		/* protect from gc */
  result = cons(cvfixnum((FIXTYPE) page_increment), NIL);
  result = cons(cvfixnum((FIXTYPE) increment), result);
  result = cons(cvfixnum((FIXTYPE) slider_size), result);
  result = cons(cvfixnum((FIXTYPE) value), result);
  xlpop();
  return (result);
}


/******************************************************************************
 * (send <scrollbar_widget> :SET_VALUE <value> <slider_size> 
 *                                     [[[<increment>] <page_increment>] <notify>])
 * ==> returns <scrollbar_widget>.
 * <value> is a fixnum specifying the slider position
 *	(same as setting the resource :XMN_VALUE)
 * <slider_size> is a fixnum specifying the size of the slider.
 *	(same as setting resource :XMN_SLIDER_SIZE)
 * <increment> is an optional fixnum specifying the amount of button increment
 *	and decrement. (same as setting resource :XMN_INCREMENT).
 * <page_increment> is an optional fixnum specifying the page increment size.
 *	(same as setting resource :XMN_INCREMENT).
 * <notify> is an optional Boolean, if NIL, the value changed callback will not
 * 	be activated, else it will.
 * 
 * Note that I couldn't call this method :SET_VALUES because it would override
 * method :SET_VALUES on WIDGET_CLASS.
 *
 * void XmScrollBarSetValues (w, value, slider_size, 
 *                            increment, page_increment, notify)
 *		Widget w;
 *		int value;
 *		int slider_size;
 *		int increment;
 *		int page_increment;
 *		Boolean notify;
 ******************************************************************************/
LVAL Xm_Scroll_Bar_Widget_Class_Method_SET_VALUE()
{
  LVAL self;
  Widget widget_id;
  int value, slider_size, increment, page_increment;
  Boolean notify;

  widget_id = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&self);
  value = (int) getfixnum(xlgafixnum());
  slider_size = (int) getfixnum(xlgafixnum());
  if (moreargs()) {
    increment = (int) getfixnum(xlgafixnum());
    if (moreargs()) {
      page_increment = (int) getfixnum(xlgafixnum());
      if (moreargs())
	notify = ((xlgetarg() != NIL) ? TRUE : FALSE);
      else
	notify = FALSE;
    }
    else 
      page_increment = notify = FALSE;
  }
  else 
    increment = page_increment = notify = FALSE;
  xllastarg();

  XmScrollBarSetValues(widget_id, value, slider_size,
		       increment, page_increment, notify);

  return (self);
}


/******************************************************************************
 *
 ******************************************************************************/
Wc_ScrollBar_Init()
{
  LVAL o_XM_SCROLL_BAR_WIDGET_CLASS;

  o_XM_SCROLL_BAR_WIDGET_CLASS =
    Wcls_Create_Subclass_Of_WIDGET_CLASS("XM_SCROLL_BAR_WIDGET_CLASS",
					 xmScrollBarWidgetClass);

  xladdmsg(o_XM_SCROLL_BAR_WIDGET_CLASS, ":ADD_CALLBACK",
           FTAB_Xm_Scroll_Bar_Widget_Class_Method_ADD_CALLBACK);

  xladdmsg(o_XM_SCROLL_BAR_WIDGET_CLASS, ":SET_CALLBACK",
           FTAB_Xm_Scroll_Bar_Widget_Class_Method_SET_CALLBACK);

  xladdmsg(o_XM_SCROLL_BAR_WIDGET_CLASS, ":SET_VALUE",
           FTAB_Xm_Scroll_Bar_Widget_Class_Method_SET_VALUE);

  xladdmsg(o_XM_SCROLL_BAR_WIDGET_CLASS, ":GET_VALUE",
           FTAB_Xm_Scroll_Bar_Widget_Class_Method_GET_VALUE);

  s_CALLBACK_PIXEL = xlenter("CALLBACK_PIXEL");
}

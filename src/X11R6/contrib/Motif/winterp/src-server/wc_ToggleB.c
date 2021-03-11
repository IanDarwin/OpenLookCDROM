/* -*-C-*-
********************************************************************************
*
* File:         wc_ToggleB.c
* RCS:          $Header: /users/npm/src/winterp/src-server/RCS/wc_ToggleB.c,v 2.6 1994/06/06 15:40:39 npm Exp $
* Description:  XM_TOGGLE_BUTTON_WIDGET_CLASS/XM_TOGGLE_BUTTON_GADGET_CLASS
* Author:       Niels Mayer
* Created:      Sat Oct 28 04:56:30 1989
* Modified:     Sun Jun  5 15:09:43 1994 (Niels Mayer) npm@indeed
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
static char rcs_identity[] = "@(#)$Header: /users/npm/src/winterp/src-server/RCS/wc_ToggleB.c,v 2.6 1994/06/06 15:40:39 npm Exp $";

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
#include <Xm/ToggleB.h>
#include <Xm/ToggleBG.h>
#include "winterp.h"
#include "w_funtab.h"


/******************************************************************************
 * typedef struct
 * {
 *    int reason;
 *    XEvent * event;
 *    int set;
 * } XmToggleButtonCallbackStruct;
 ******************************************************************************/
static LVAL s_CALLBACK_SET;
static void Lexical_Bindings_For_XmToggleButtonCallbackStruct(bindings_list,
							      lexical_env,
							      call_data,
							      client_data)
     LVAL bindings_list;	/* a list of symbols to which values from XmToggleButtonCallbackStruct are bound */
     LVAL lexical_env;		
     XtPointer call_data;
     LVAL client_data;		/* XLTYPE_CALLBACKOBJ */
{
  extern LVAL true;
  register LVAL s_bindname;
  XmToggleButtonCallbackStruct* cd;

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
    case XmCR_ARM:		/* XmToggleButton|XmToggleButtonGadget(:XMN_ARM_CALLBACK) */
    case XmCR_DISARM:		/* XmToggleButton|XmToggleButtonGadget(:XMN_DISARM_CALLBACK) */
    case XmCR_VALUE_CHANGED:	/* XmToggleButton|XmToggleButtonGadget(:XMN_VALUE_CHANGED_CALLBACK) */
      cd = (XmToggleButtonCallbackStruct*) call_data;
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
	else if (s_bindname == s_CALLBACK_SET) {
	  xlpbind(s_bindname, (cd->set) ? true : NIL, lexical_env);
	}
	else {
	  errputstr("Warning: in widget callback, ignored unknown binding name - ");
	  errprint(s_bindname);
	  sprintf(temptext,
		  "\tValid symbols for XmToggleButtonCallbackStruct are [%s %s %s %s] - ",
		  getstring(getpname(s_CALLBACK_WIDGET)),
		  getstring(getpname(s_CALLBACK_REASON)),
		  getstring(getpname(s_CALLBACK_XEVENT)),
		  getstring(getpname(s_CALLBACK_SET)));
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
 * an XmToggleButtonCallbackStruct as call_data.
 ******************************************************************************/
static void XmToggleButtonCallbackStruct_Callbackproc(widget, client_data, call_data)
     Widget    widget;
     XtPointer client_data;
     XtPointer call_data;
{
  Wcb_Meta_Callbackproc(get_callback_closure((LVAL) client_data),
			(LVAL) client_data,
			call_data,
			Lexical_Bindings_For_XmToggleButtonCallbackStruct,
			NULL,
			Wcb_Callback_Trace_Proc);
}


/******************************************************************************
 * Same as WIDGET_CLASS's :add_callback method except that this understands
 * how to get values from the XmToggleButtonCallbackStruct.
 * Specifying one or more of the following symbols in the callback bindings 
 * list will bind that symbol's value in the lexical environment of the callback:
 * CALLBACK_WIDGET
 * CALLBACK_REASON
 * CALLBACK_XEVENT
 * CALLBACK_SET
 ******************************************************************************/
LVAL Xm_Toggle_Button_Widget_Class_Method_ADD_CALLBACK()
{
  return (Wcb_Meta_Method_Add_Callback(XmToggleButtonCallbackStruct_Callbackproc, FALSE));
}


/******************************************************************************
 * Same as WIDGET_CLASS's :set_callback method except that this understands
 * how to get values from the XmToggleButtonCallbackStruct.
 * Specifying one or more of the following symbols in the callback bindings 
 * list will bind that symbol's value in the lexical environment of the callback:
 * CALLBACK_WIDGET
 * CALLBACK_REASON
 * CALLBACK_XEVENT
 * CALLBACK_SET
 ******************************************************************************/
LVAL Xm_Toggle_Button_Widget_Class_Method_SET_CALLBACK()
{
  return (Wcb_Meta_Method_Add_Callback(XmToggleButtonCallbackStruct_Callbackproc, TRUE));
}


/******************************************************************************
 * (send <togglebutton> :GET_STATE)
 * ==> returns T if button is set, NIL if not.
 * 
 * Boolean XmToggleButtonGetState (Widget w);
 * Boolean XmToggleButtonGadgetGetState (Widget w);
 ******************************************************************************/
LVAL Xm_Toggle_Button_Widget_Class_Method_GET_STATE()
{
  LVAL self;
  Widget widget_id;
  extern LVAL true;

  widget_id = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&self);
  xllastarg();

  return (XmToggleButtonGetState(widget_id) ? true : NIL);
}

LVAL Xm_Toggle_Button_Gadget_Class_Method_GET_STATE()
{
  LVAL self;
  Widget widget_id;
  extern LVAL true;
  
  widget_id = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&self);
  xllastarg();

  return (XmToggleButtonGadgetGetState(widget_id) ? true : NIL);
}


/******************************************************************************
 * (send <togglebutton> :SET_STATE <state_p> <notify_p>)
 * ==> returns <togglebutton>.
 * <state_p> -- NIL if button is off, else button is on.
 * <notify_p> -- if not NIL, :xmn_value_changed_callback gets called.
 *
 * void XmToggleButtonSetState (Widget w, Boolean newstate, Boolean notify);
 * void XmToggleButtonGadgetSetState (Widget w, Boolean newstate, Boolean notify);
 ******************************************************************************/
LVAL Xm_Toggle_Button_Widget_Class_Method_SET_STATE()
{
  LVAL self, state_p, notify_p;
  Widget widget_id;
  
  widget_id = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&self);
  state_p = xlgetarg();
  notify_p = xlgetarg();
  xllastarg();

  XmToggleButtonSetState(widget_id, 
			 (state_p  != NIL) ? TRUE : FALSE,
			 (notify_p != NIL) ? TRUE : FALSE);
  return (self);
}

LVAL Xm_Toggle_Button_Gadget_Class_Method_SET_STATE()
{
  LVAL self, state_p, notify_p;
  Widget widget_id;
  
  widget_id = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&self);
  state_p = xlgetarg();
  notify_p = xlgetarg();
  xllastarg();

  XmToggleButtonGadgetSetState(widget_id, 
			       (state_p  != NIL) ? TRUE : FALSE,
			       (notify_p != NIL) ? TRUE : FALSE);
  return (self);
}


/******************************************************************************
 *
 ******************************************************************************/
Wc_ToggleB_Init()
{
  LVAL o_XM_TOGGLE_BUTTON_WIDGET_CLASS;
  LVAL o_XM_TOGGLE_BUTTON_GADGET_CLASS;
  
  o_XM_TOGGLE_BUTTON_WIDGET_CLASS =
    Wcls_Create_Subclass_Of_WIDGET_CLASS("XM_TOGGLE_BUTTON_WIDGET_CLASS",
					 xmToggleButtonWidgetClass);

  xladdmsg(o_XM_TOGGLE_BUTTON_WIDGET_CLASS, ":ADD_CALLBACK",
           FTAB_Xm_Toggle_Button_Widget_Class_Method_ADD_CALLBACK);

  xladdmsg(o_XM_TOGGLE_BUTTON_WIDGET_CLASS, ":SET_CALLBACK",
           FTAB_Xm_Toggle_Button_Widget_Class_Method_SET_CALLBACK);

  xladdmsg(o_XM_TOGGLE_BUTTON_WIDGET_CLASS, ":GET_STATE",
           FTAB_Xm_Toggle_Button_Widget_Class_Method_GET_STATE);

  xladdmsg(o_XM_TOGGLE_BUTTON_WIDGET_CLASS, ":SET_STATE",
           FTAB_Xm_Toggle_Button_Widget_Class_Method_SET_STATE);


  o_XM_TOGGLE_BUTTON_GADGET_CLASS =
    Wcls_Create_Subclass_Of_WIDGET_CLASS("XM_TOGGLE_BUTTON_GADGET_CLASS",
					 xmToggleButtonGadgetClass);

  xladdmsg(o_XM_TOGGLE_BUTTON_GADGET_CLASS, ":ADD_CALLBACK",
           FTAB_Xm_Toggle_Button_Widget_Class_Method_ADD_CALLBACK);

  xladdmsg(o_XM_TOGGLE_BUTTON_GADGET_CLASS, ":SET_CALLBACK",
           FTAB_Xm_Toggle_Button_Widget_Class_Method_SET_CALLBACK);

  xladdmsg(o_XM_TOGGLE_BUTTON_GADGET_CLASS, ":GET_STATE",
           FTAB_Xm_Toggle_Button_Gadget_Class_Method_GET_STATE);

  xladdmsg(o_XM_TOGGLE_BUTTON_GADGET_CLASS, ":SET_STATE",
           FTAB_Xm_Toggle_Button_Gadget_Class_Method_SET_STATE);

  s_CALLBACK_SET = xlenter("CALLBACK_SET");
}

/* -*-C-*-
********************************************************************************
*
* File:         wc_PushB.c
* RCS:          $Header: /users/npm/src/winterp/src-server/RCS/wc_PushB.c,v 2.6 1994/06/06 15:40:44 npm Exp $
* Description:  XM_PUSH_BUTTON_WIDGET_CLASS/XM_PUSH_BUTTON_GADGET_CLASS
* Author:       Niels Mayer
* Created:      Sat Oct 28 04:22:22 1989
* Modified:     Sun Jun  5 15:03:44 1994 (Niels Mayer) npm@indeed
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
static char rcs_identity[] = "@(#)$Header: /users/npm/src/winterp/src-server/RCS/wc_PushB.c,v 2.6 1994/06/06 15:40:44 npm Exp $";

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
#include <Xm/PushB.h>
#include <Xm/PushBG.h>
#include "winterp.h"
#include "w_funtab.h"

#ifdef WINTERP_MOTIF_11
/******************************************************************************
 * typedef struct
 * {
 *     int     reason;
 *     XEvent  *event;
 *     int	    click_count;
 * } XmPushButtonCallbackStruct;
 ******************************************************************************/
static void Lexical_Bindings_For_XmPushButtonCallbackStruct(bindings_list,
							    lexical_env,
							    call_data,
							    client_data)
     LVAL bindings_list;	/* a list of symbols to which values from XmPushButtonCallbackStruct are bound */
     LVAL lexical_env;		
     XtPointer call_data;
     LVAL client_data;		/* XLTYPE_CALLBACKOBJ */
{
  register LVAL s_bindname;
  XmPushButtonCallbackStruct* cd;

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
    case XmCR_ARM:		/* XmPushButton|XmPushButtonGadget(:XMN_ARM_CALLBACK) */
    case XmCR_DISARM:		/* XmPushButton|XmPushButtonGadget(:XMN_DISARM_CALLBACK) */
    case XmCR_ACTIVATE:		/* XmPushButton|XmPushButtonGadget(:XMN_ACTIVATE_CALLBACK) */
      cd = (XmPushButtonCallbackStruct*) call_data;
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
	else if (s_bindname == s_CALLBACK_CLICK_COUNT) {
	  xlpbind(s_bindname, cvfixnum((FIXTYPE) (cd->click_count)), lexical_env);
	}
	else {
	  errputstr("Warning: in widget callback, ignored unknown binding name - ");
	  errprint(s_bindname);
	  sprintf(temptext,
		  "\tValid symbols for XmPushButtonCallbackStruct are [%s %s %s %s] - ",
		  getstring(getpname(s_CALLBACK_WIDGET)),
		  getstring(getpname(s_CALLBACK_REASON)),
		  getstring(getpname(s_CALLBACK_XEVENT)),
		  getstring(getpname(s_CALLBACK_CLICK_COUNT)));
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
 * an XmPushButtonCallbackStruct as call_data.
 ******************************************************************************/
static void XmPushButtonCallbackStruct_Callbackproc(widget, client_data, call_data)
     Widget    widget;
     XtPointer client_data;
     XtPointer call_data;
{
  Wcb_Meta_Callbackproc(get_callback_closure((LVAL) client_data),
			(LVAL) client_data,
			call_data,
			Lexical_Bindings_For_XmPushButtonCallbackStruct,
			NULL,
			Wcb_Callback_Trace_Proc);
}

/******************************************************************************
 * Same as WIDGET_CLASS's :add_callback method except that this understands
 * how to get values from the XmPushButtonCallbackStruct.
 * Specifying one or more of the following symbols in the callback bindings 
 * list will bind that symbol's value in the lexical environment of the callback:
 * CALLBACK_WIDGET
 * CALLBACK_REASON
 * CALLBACK_XEVENT
 * CALLBACK_CLICK_COUNT -- new for motif 1.1
 ******************************************************************************/
LVAL Xm_Push_Button_Widget_Class_Method_ADD_CALLBACK()
{
  return (Wcb_Meta_Method_Add_Callback(XmPushButtonCallbackStruct_Callbackproc, FALSE));
}

/******************************************************************************
 * Same as WIDGET_CLASS's :set_callback method except that this understands
 * how to get values from the XmPushButtonCallbackStruct.
 * Specifying one or more of the following symbols in the callback bindings 
 * list will bind that symbol's value in the lexical environment of the callback:
 * CALLBACK_WIDGET
 * CALLBACK_REASON
 * CALLBACK_XEVENT
 * CALLBACK_CLICK_COUNT -- new for motif 1.1
 ******************************************************************************/
LVAL Xm_Push_Button_Widget_Class_Method_SET_CALLBACK()
{
  return (Wcb_Meta_Method_Add_Callback(XmPushButtonCallbackStruct_Callbackproc, TRUE));
}
#endif				/* WINTERP_MOTIF_11 */


/******************************************************************************
 *
 ******************************************************************************/
Wc_PushB_Init()
{
  LVAL o_XM_PUSH_BUTTON_WIDGET_CLASS;
  LVAL o_XM_PUSH_BUTTON_GADGET_CLASS;

  o_XM_PUSH_BUTTON_WIDGET_CLASS =
    Wcls_Create_Subclass_Of_WIDGET_CLASS("XM_PUSH_BUTTON_WIDGET_CLASS",
					 xmPushButtonWidgetClass);

#ifdef WINTERP_MOTIF_11
  xladdmsg(o_XM_PUSH_BUTTON_WIDGET_CLASS, ":ADD_CALLBACK",
           FTAB_Xm_Push_Button_Widget_Class_Method_ADD_CALLBACK);

  xladdmsg(o_XM_PUSH_BUTTON_WIDGET_CLASS, ":SET_CALLBACK",
           FTAB_Xm_Push_Button_Widget_Class_Method_SET_CALLBACK);
#endif				/* WINTERP_MOTIF_11 */

  o_XM_PUSH_BUTTON_GADGET_CLASS =
    Wcls_Create_Subclass_Of_WIDGET_CLASS("XM_PUSH_BUTTON_GADGET_CLASS",
					 xmPushButtonGadgetClass);

#ifdef WINTERP_MOTIF_11
  xladdmsg(o_XM_PUSH_BUTTON_GADGET_CLASS, ":ADD_CALLBACK",
           FTAB_Xm_Push_Button_Widget_Class_Method_ADD_CALLBACK);
  
  xladdmsg(o_XM_PUSH_BUTTON_GADGET_CLASS, ":SET_CALLBACK",
           FTAB_Xm_Push_Button_Widget_Class_Method_SET_CALLBACK);
#endif				/* WINTERP_MOTIF_11 */

}

/* -*-C-*-
********************************************************************************
*
* File:         wc_SelectioB.c
* RCS:          $Header: /users/npm/src/winterp/src-server/RCS/wc_SelectioB.c,v 2.6 1994/06/06 15:40:41 npm Exp $
* Description:  XM_SELECTION_BOX_WIDGET_CLASS
* Author:       Niels Mayer
* Created:      Sat Oct 28 04:41:20 1989
* Modified:     Sun Jun  5 15:06:52 1994 (Niels Mayer) npm@indeed
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
static char rcs_identity[] = "@(#)$Header: /users/npm/src/winterp/src-server/RCS/wc_SelectioB.c,v 2.6 1994/06/06 15:40:41 npm Exp $";

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
#include <Xm/SelectioB.h>
#include "winterp.h"
#include "w_funtab.h"
#include "w_XmString.h"


static LVAL k_prompt_dialog;

/*****************************************************************************
 * (send XM_SELECTION_BOX_WIDGET_CLASS :new 
 *                           [:managed/:unmanaged]
 *                           [:dialog/:prompt_dialog]
 *                           [<name>]
 *                           <parent> 
 *                           [:XMN_<arg1> <val1>]
 *                           [. . .             ]
 *                           [:XMN_<argN> <valN>])
 *
 * The optional keyword submessage :managed will cause a subsequent call
 * to XtManageChild(). If the submessage :unmanaged is present, or no
 * submessage, then XtManageChild() won't be called, and the resulting
 * widget will be returned unmanaged.
 *
 *   (send XM_SELECTION_BOX_WIDGET_CLASS :new ...)
 *   --> XmCreateSelectionBox();
 *   (send XM_SELECTION_BOX_WIDGET_CLASS :new :dialog ...)
 *   --> XmCreateSelectionDialog();
 *   (send XM_SELECTION_BOX_WIDGET_CLASS :new :prompt_dialog ...)
 *   --> XmCreatePromptDialog();
 ****************************************************************************/
LVAL Xm_Selection_Box_Widget_Class_Method_ISNEW()
{
  LVAL self, o_parent;
  char* name;
  Boolean managed_p;
  LVAL sb_kind;
  Widget widget_id, parent_widget_id;

  self = Wcls_Get_OBJECT_Arg_Returning_WIDGETOBJ(); /* NOTE: xlobj.c:clnew() returns an OBJECT; 
						       get the arg and mark it's type slot as a WIDGETOBJ */
  
  /* get optional managed/unmanaged arg */
  if (moreargs() && ((*xlargv == k_managed) || (*xlargv == k_unmanaged)))
    managed_p = (nextarg() == k_managed);
  else
    managed_p = FALSE;		/* by default don't call XtManageChild() */

  /* get optional :dialog/:prompt_dialog arg */
  if (moreargs() && ((*xlargv == k_dialog) || (*xlargv ==  k_prompt_dialog)))
    sb_kind = nextarg();
  else
    sb_kind = NIL;		/* default is XmCreateSelectionBox() */
  
  /* get optional <name> arg */
  if (moreargs() && (stringp(*xlargv)))
    name = getstring(nextarg());
  else
    name = "";			/* default name */

  /* get required <parent> widget-object arg */
  parent_widget_id = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&o_parent);

  /*
   * Store the widget object <self> in the XmNuserData resource on the
   * widget. This will allow us to retrieve the widget object from Xtoolkit
   * functions returning widget ID's without having to keep around a table
   * of widgetID-->widget-objects.
   */
   ARGLIST_RESET(); ARGLIST_ADD(XmNuserData, (XtArgVal) self); 

  if (moreargs()) {		/* if there are more arguments, */
    Cardinal xt_numargs;	/* then we have some extra widget resources to set */
    ArgList xt_arglist = Wres_Get_LispArglist(self, parent_widget_id, ARGLIST(), &xt_numargs);
    if (sb_kind == NIL)
      widget_id = XmCreateSelectionBox(parent_widget_id, name, xt_arglist, xt_numargs);
    else if (sb_kind == k_dialog)
      widget_id = XmCreateSelectionDialog(parent_widget_id, name, xt_arglist, xt_numargs);
    else if (sb_kind == k_prompt_dialog)
      widget_id = XmCreatePromptDialog(parent_widget_id, name, xt_arglist, xt_numargs);
    else
      xlfatal("Bug in Xm_Selection_Box_Widget_Class_Method_ISNEW()");
    Wres_Free_C_Arglist_Data();
  }
  else
    if (sb_kind == NIL)
      widget_id = XmCreateSelectionBox(parent_widget_id, name, ARGLIST());
    else if (sb_kind == k_dialog)
      widget_id = XmCreateSelectionDialog(parent_widget_id, name, ARGLIST());
    else if (sb_kind == k_prompt_dialog)
      widget_id = XmCreatePromptDialog(parent_widget_id, name, ARGLIST());
    else
      xlfatal("Bug in Xm_Selection_Box_Widget_Class_Method_ISNEW()");

  /*
   * put the hidden XM_DIALOG_POPUP_SHELL_WIDGET_CLASS parent onto
   * wc_SHELL.c:shell_widget_alist -- this is needed so that dialog shells 
   * display busy cursor when Wshl_Prim_WINTERP_SHOW_BUSY() is set...
   */
  if (sb_kind != NIL)
    (void) Wshl_WidgetID_To_WIDGETOBJ(XtParent(widget_id));

  Wcls_Initialize_WIDGETOBJ(self, widget_id);

  if (managed_p)
    XtManageChild(widget_id);
  
#ifdef WINTERP_DEBUG_1
  Wcls_Print_WidgetObj_Info(self);
#endif /* WINTERP_DEBUG_1 */
  return (self);
}


/******************************************************************************
 * typedef struct
 * {
 *     int reason;
 *     XEvent	*event;
 *     XmString	value;
 *     int	length;
 * } XmSelectionBoxCallbackStruct;
 ******************************************************************************/
void Lexical_Bindings_For_XmSelectionBoxCallbackStruct(bindings_list,
						       lexical_env,
						       call_data,
						       client_data)
     LVAL bindings_list;	/* a list of symbols to which values from XmSelectionBoxCallbackStruct are bound */
     LVAL lexical_env;		
     XtPointer call_data;
     LVAL client_data;		/* XLTYPE_CALLBACKOBJ */
{
  register LVAL s_bindname;
  XmSelectionBoxCallbackStruct* cd;

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
    case XmCR_APPLY:		/* XmSelectionBox(:XMN_APPLY_CALLBACK) */
    case XmCR_CANCEL:		/* XmSelectionBox(:XMN_CANCEL_CALLBACK) */
    case XmCR_NO_MATCH:		/* XmSelectionBox(:XMN_NO_MATCH_CALLBACK) */
    case XmCR_OK:		/* XmSelectionBox(:XMN_OK_CALLBACK) */
      cd = (XmSelectionBoxCallbackStruct*) call_data;
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
	  xlpbind(s_bindname, (cd->value) ? cv_xmstring(XmStringCopy(cd->value)) : NIL, lexical_env); /* copy the XmString even though cd->value never looks like it gets freed -- it's inconsistant w/ other callback XmStrings.. maybe it'll get fixed eventually */
	}
	else if (s_bindname == s_CALLBACK_LENGTH) {
	  xlpbind(s_bindname, (cd->value) ? cvfixnum((FIXTYPE) cd->length) : NIL, lexical_env);
	}
	else {
	  errputstr("Warning: in widget callback, ignored unknown binding name - ");
	  errprint(s_bindname);
	  sprintf(temptext,
		  "\tValid symbols for XmSelectionBoxCallbackStruct are [%s %s %s %s %s] - ",
		  getstring(getpname(s_CALLBACK_WIDGET)),
		  getstring(getpname(s_CALLBACK_REASON)),
		  getstring(getpname(s_CALLBACK_XEVENT)),
		  getstring(getpname(s_CALLBACK_VALUE)),
		  getstring(getpname(s_CALLBACK_LENGTH)));
	  errputstr(temptext);
	  errprint(get_callback_widget(client_data));
	}
      }
      break;
    default:			/* XmBulletinBoard(:XMN_FOCUS_CALLBACK,:XMN_MAP_CALLBACK,:XMN_UNMAP_CALLBACK), XmManager(:XMN_HELP_CALLBACK) */
      Lexical_Bindings_For_XmAnyCallbackStruct(bindings_list, lexical_env, call_data, client_data);
      break;
    }
}


/******************************************************************************
 * This is called indirectly via XtAddCallback() for callbacks returning
 * an XmSelectionBoxCallbackStruct as call_data.
 ******************************************************************************/
static void XmSelectionBoxCallbackStruct_Callbackproc(widget, client_data, call_data)
     Widget    widget;
     XtPointer client_data;
     XtPointer call_data;
{
  Wcb_Meta_Callbackproc(get_callback_closure((LVAL) client_data),
			(LVAL) client_data,
			call_data,
			Lexical_Bindings_For_XmSelectionBoxCallbackStruct,
			NULL,
			Wcb_Callback_Trace_Proc);
}


/******************************************************************************
 * Same as WIDGET_CLASS's :add_callback method except that this understands
 * how to get values from the XmSelectionBoxCallbackStruct.
 * Specifying one or more of the following symbols in the callback bindings 
 * list will bind that symbol's value in the lexical environment of the callback:
 * CALLBACK_WIDGET
 * CALLBACK_REASON
 * CALLBACK_XEVENT
 * CALLBACK_VALUE
 * CALLBACK_LENGTH
 ******************************************************************************/
LVAL Xm_Selection_Box_Widget_Class_Method_ADD_CALLBACK()
{
  return (Wcb_Meta_Method_Add_Callback(XmSelectionBoxCallbackStruct_Callbackproc, FALSE));
}


/******************************************************************************
 * Same as WIDGET_CLASS's :set_callback method except that this understands
 * how to get values from the XmSelectionBoxCallbackStruct.
 * Specifying one or more of the following symbols in the callback bindings 
 * list will bind that symbol's value in the lexical environment of the callback:
 * CALLBACK_WIDGET
 * CALLBACK_REASON
 * CALLBACK_XEVENT
 * CALLBACK_VALUE
 * CALLBACK_LENGTH
 ******************************************************************************/
LVAL Xm_Selection_Box_Widget_Class_Method_SET_CALLBACK()
{
  return (Wcb_Meta_Method_Add_Callback(XmSelectionBoxCallbackStruct_Callbackproc, TRUE));
}


/******************************************************************************
 * (send <selectionbox_widget> :get_child <childname>)
 * ==> returns the WIDGETOBJ correspomding to <childname> which can be one
 * of the following:
 * :DIALOG_LIST, :DIALOG_LIST_LABEL, :DIALOG_SELECTION_LABEL, :DIALOG_WORK_AREA
 * :DIALOG_TEXT, :DIALOG_SEPARATOR, :DIALOG_OK_BUTTON, :DIALOG_APPLY_BUTTON,
 * :DIALOG_CANCEL_BUTTON, :DIALOG_HELP_BUTTON, :DIALOG_DEFAULT_BUTTON
 *
 * Widget XmSelectionBoxGetChild (sb, which)
 *        Widget sb;
 *        int which;
 ******************************************************************************/
LVAL Xm_Selection_Box_Widget_Class_Method_GET_CHILD()
{
  LVAL self, lval_child;
  Widget widget_id;
  int child;
  extern LVAL s_XmDIALOG_LIST, s_XmDIALOG_LIST_LABEL, s_XmDIALOG_SELECTION_LABEL,
  s_XmDIALOG_WORK_AREA, s_XmDIALOG_TEXT, s_XmDIALOG_SEPARATOR,
  s_XmDIALOG_OK_BUTTON, s_XmDIALOG_APPLY_BUTTON, s_XmDIALOG_CANCEL_BUTTON,
  s_XmDIALOG_HELP_BUTTON, s_XmDIALOG_DEFAULT_BUTTON; /* from w_resources.c */
  
  widget_id = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&self);
  lval_child = xlgasymbol();
  xllastarg();

  if (lval_child == s_XmDIALOG_LIST)
    child = XmDIALOG_LIST;
  else if (lval_child == s_XmDIALOG_LIST_LABEL)
    child = XmDIALOG_LIST_LABEL;
  else if (lval_child == s_XmDIALOG_SELECTION_LABEL)
    child = XmDIALOG_SELECTION_LABEL;
  else if (lval_child == s_XmDIALOG_WORK_AREA)
    child = XmDIALOG_WORK_AREA;
  else if (lval_child == s_XmDIALOG_TEXT)
    child = XmDIALOG_TEXT;
  else if (lval_child == s_XmDIALOG_SEPARATOR)
    child = XmDIALOG_SEPARATOR;
  else if (lval_child == s_XmDIALOG_OK_BUTTON)
    child = XmDIALOG_OK_BUTTON;
  else if (lval_child == s_XmDIALOG_APPLY_BUTTON)
    child = XmDIALOG_APPLY_BUTTON;
  else if (lval_child == s_XmDIALOG_CANCEL_BUTTON)
    child = XmDIALOG_CANCEL_BUTTON; 
  else if (lval_child == s_XmDIALOG_HELP_BUTTON)
    child = XmDIALOG_HELP_BUTTON;
  else if (lval_child == s_XmDIALOG_DEFAULT_BUTTON)
    child =XmDIALOG_DEFAULT_BUTTON;
  else
    xlerror("Invalid selectionbox child symbol.", lval_child);
    
  return (Wcls_WidgetID_To_WIDGETOBJ(XmSelectionBoxGetChild(widget_id, child)));
}


/******************************************************************************
 * (send <selectionbox_widget> :GET_LIST_ITEMS)
 * 	==> returns an array of XmStrings.
 *
 * This retrieves the XmSelectionBox widget resources XmNlistItems and
 * XmNlistItemCount from <selectionbox_widget> and returns an array of
 * XmStrings representing the items in the selectionbox's list.
 *******************************************************************************/
LVAL Xm_Selection_Box_Widget_Class_Method_GET_LIST_ITEMS()
{
  LVAL self;
  Widget widget_id;
  XmStringTable xmstrtab;
  int		xmstrtab_size;

  widget_id = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&self);
  xllastarg();

  ARGLIST_RESET();
  ARGLIST_ADD(XmNlistItems, &xmstrtab);
  ARGLIST_ADD(XmNlistItemCount, &xmstrtab_size);
  XtGetValues(widget_id, ARGLIST());

  return (Wxms_XmStringTable_To_Lisp_Vector(xmstrtab, xmstrtab_size));
}


/******************************************************************************
 *
 ******************************************************************************/
Wc_SelectioB_Init()
{
  LVAL o_XM_SELECTION_BOX_WIDGET_CLASS;

  k_prompt_dialog = xlenter(":PROMPT_DIALOG");

  o_XM_SELECTION_BOX_WIDGET_CLASS =
    Wcls_Create_Subclass_Of_WIDGET_CLASS("XM_SELECTION_BOX_WIDGET_CLASS",
					 xmSelectionBoxWidgetClass);

  /* a special :isnew method on this class allows for the creation of this
     widget inside a popup shell if one of the following submessage keywords
     are given: 
     :dialog, :prompt_dialog */
  xladdmsg(o_XM_SELECTION_BOX_WIDGET_CLASS, ":ISNEW", 
	   FTAB_Xm_Selection_Box_Widget_Class_Method_ISNEW);

  xladdmsg(o_XM_SELECTION_BOX_WIDGET_CLASS, ":ADD_CALLBACK",
           FTAB_Xm_Selection_Box_Widget_Class_Method_ADD_CALLBACK);

  xladdmsg(o_XM_SELECTION_BOX_WIDGET_CLASS, ":SET_CALLBACK",
           FTAB_Xm_Selection_Box_Widget_Class_Method_SET_CALLBACK);

  xladdmsg(o_XM_SELECTION_BOX_WIDGET_CLASS, ":GET_CHILD",
           FTAB_Xm_Selection_Box_Widget_Class_Method_GET_CHILD);

  xladdmsg(o_XM_SELECTION_BOX_WIDGET_CLASS, ":GET_LIST_ITEMS",
           FTAB_Xm_Selection_Box_Widget_Class_Method_GET_LIST_ITEMS);
}

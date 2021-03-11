/* -*-C-*-
********************************************************************************
*
* File:         wc_Command.c
* RCS:          $Header: /users/npm/src/winterp/src-server/RCS/wc_Command.c,v 2.6 1994/06/06 15:40:50 npm Exp $
* Description:  XM_COMMAND_WIDGET_CLASS
* Author:       Niels Mayer
* Created:      Fri Oct 27 21:57:58 1989
* Modified:     Sun Jun  5 14:56:34 1994 (Niels Mayer) npm@indeed
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
static char rcs_identity[] = "@(#)$Header: /users/npm/src/winterp/src-server/RCS/wc_Command.c,v 2.6 1994/06/06 15:40:50 npm Exp $";

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
#include <Xm/Command.h>
#include "winterp.h"
#include "w_funtab.h"
#include "w_XmString.h"



/*****************************************************************************
 * (send XM_COMMAND_WIDGET_CLASS :new
 *			     [:managed/:unmanaged]
 *			     [:dialog] 
 *                           [<name>]
 *                           <parent> 
 *                           [:XMN_<arg1> <val1>]
 *                           [. . .             ]
 *                           [:XMN_<argN> <valN>])
 *
 * Create a new widget via XmCreateCommand().
 *
 * The optional keyword-argument :managed will cause a subsequent call to
 * XtManageChild(). If the submessage :unmanaged is present, or no submessage,
 * then XtManageChild() won't be called, and the resulting widget will be
 * returned unmanaged.
 ****************************************************************************/
LVAL Xm_Command_Widget_Class_Method_ISNEW()
{
  LVAL self, o_parent;
  char* name;
  Boolean managed_p;
#if (defined(WINTERP_MOTIF_12) && defined(XmCreateCommandDialog))
  Boolean dialog_p;
#endif /* (defined(WINTERP_MOTIF_12) && defined(XmCreateCommandDialog)) */
  Widget parent_widget_id, widget_id;

  self = Wcls_Get_OBJECT_Arg_Returning_WIDGETOBJ(); /* NOTE: xlobj.c:clnew() returns an OBJECT; 
						       get the arg and mark it's type slot as a WIDGETOBJ */

  /* get optional managed/unmanaged arg */
  if (moreargs() && ((*xlargv == k_managed) || (*xlargv == k_unmanaged)))
    managed_p = (nextarg() == k_managed);
  else
    managed_p = FALSE;		/* by default don't call XtManageChild() */

#if (defined(WINTERP_MOTIF_12) && defined(XmCreateCommandDialog))
  /* get optional :dialog arg */
  if (moreargs() && (*xlargv == k_dialog)) {
    nextarg();
    dialog_p = TRUE;
  }
  else
    dialog_p = FALSE;		/* by default, we don't want a dialog widget */
#endif /* (defined(WINTERP_MOTIF_12) && defined(XmCreateCommandDialog)) */

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
#if (defined(WINTERP_MOTIF_12) && defined(XmCreateCommandDialog))
    if (dialog_p)
      widget_id = XmCreateCommandDialog(parent_widget_id, name, xt_arglist, xt_numargs);
    else
      widget_id = XmCreateCommand(parent_widget_id, name, xt_arglist, xt_numargs);
#else /* Motif < 1.2.2 */
    widget_id = XmCreateCommand(parent_widget_id, name, xt_arglist, xt_numargs);
#endif /* (defined(WINTERP_MOTIF_12) && defined(XmCreateCommandDialog)) */
    Wres_Free_C_Arglist_Data();
  }
  else 
#if (defined(WINTERP_MOTIF_12) && defined(XmCreateCommandDialog))
    if (dialog_p)
      widget_id = XmCreateCommandDialog(parent_widget_id, name, ARGLIST());
    else
      widget_id = XmCreateCommand(parent_widget_id, name, ARGLIST());
#else /* Motif < 1.2.2 */
    widget_id = XmCreateCommand(parent_widget_id, name, ARGLIST());
#endif /* (defined(WINTERP_MOTIF_12) && defined(XmCreateCommandDialog)) */

#if (defined(WINTERP_MOTIF_12) && defined(XmCreateCommandDialog))
  /*
   * put the hidden XM_DIALOG_POPUP_SHELL_WIDGET_CLASS parent onto
   * wc_SHELL.c:shell_widget_alist -- this is needed so that dialog shells 
   * display busy cursor when Wshl_Prim_WINTERP_SHOW_BUSY() is set...
   */
  if (dialog_p)
    (void) Wshl_WidgetID_To_WIDGETOBJ(XtParent(widget_id));
#endif /* (defined(WINTERP_MOTIF_12) && defined(XmCreateCommandDialog)) */

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
 * } XmCommandCallbackStruct;
 ******************************************************************************/
static void Lexical_Bindings_For_XmCommandCallbackStruct(bindings_list,
							 lexical_env,
							 call_data,
							 client_data)
     LVAL bindings_list;	/* a list of symbols to which values from XmCommandCallbackStruct are bound */
     LVAL lexical_env;		
     XtPointer call_data;
     LVAL client_data;		/* XLTYPE_CALLBACKOBJ */
{
  register LVAL s_bindname;
  XmCommandCallbackStruct* cd;

  if (!call_data) {		/* Core(:XMN_DESTROY_CALLBACK) */
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
    case XmCR_COMMAND_ENTERED:	/* XmCommand(:XMN_COMMAND_ENTERED_CALLBACK) */
    case XmCR_COMMAND_CHANGED:	/* XmCommand(:XMN_COMMAND_CHANGED_CALLBACK) */
      cd = (XmCommandCallbackStruct*) call_data;
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
	  xlpbind(s_bindname, (cd->value) ? cv_xmstring(XmStringCopy(cd->value)) : NIL, lexical_env);
	}
	else if (s_bindname == s_CALLBACK_LENGTH) {
	  xlpbind(s_bindname, (cd->value) ? cvfixnum((FIXTYPE) cd->length) : NIL, lexical_env);
	}
	else {
	  errputstr("Warning: in widget callback, ignored unknown binding name - ");
	  errprint(s_bindname);
	  sprintf(temptext,
		  "\tValid symbols for XmCommandCallbackStruct are [%s %s %s %s %s] - ",
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
    case XmCR_APPLY:		/* XmSelectionBox(:XMN_APPLY_CALLBACK) */
    case XmCR_CANCEL:		/* XmSelectionBox(:XMN_CANCEL_CALLBACK) */
    case XmCR_NO_MATCH:		/* XmSelectionBox(:XMN_NO_MATCH_CALLBACK) */
    case XmCR_OK:		/* XmSelectionBox(:XMN_OK_CALLBACK) */
      Lexical_Bindings_For_XmSelectionBoxCallbackStruct(bindings_list, lexical_env, call_data, client_data);
      break;
    default:			/* XmBulletinBoard(:XMN_FOCUS_CALLBACK,:XMN_MAP_CALLBACK,:XMN_UNMAP_CALLBACK), XmManager(:XMN_HELP_CALLBACK) */
      Lexical_Bindings_For_XmAnyCallbackStruct(bindings_list, lexical_env, call_data, client_data);
      break;
    }
}


/******************************************************************************
 * This is called indirectly via XtAddCallback() for callbacks returning
 * an XmCommandCallbackStruct as call_data.
 ******************************************************************************/
static void XmCommandCallbackStruct_Callbackproc(widget, client_data, call_data)
     Widget    widget;
     XtPointer client_data;
     XtPointer call_data;
{
  Wcb_Meta_Callbackproc(get_callback_closure((LVAL) client_data),
			(LVAL) client_data,
			call_data,
			Lexical_Bindings_For_XmCommandCallbackStruct,
			NULL,
			Wcb_Callback_Trace_Proc);
}


/******************************************************************************
 * Same as WIDGET_CLASS's :add_callback method except that this understands
 * how to get values from the XmCommandCallbackStruct.
 * Specifying one or more of the following symbols in the callback bindings 
 * list will bind that symbol's value in the lexical environment of the callback:
 * CALLBACK_WIDGET
 * CALLBACK_REASON
 * CALLBACK_XEVENT
 * CALLBACK_VALUE
 * CALLBACK_LENGTH
 ******************************************************************************/
LVAL Xm_Command_Widget_Class_Method_ADD_CALLBACK()
{
  return (Wcb_Meta_Method_Add_Callback(XmCommandCallbackStruct_Callbackproc, FALSE));
}


/******************************************************************************
 * Same as WIDGET_CLASS's :set_callback method except that this understands
 * how to get values from the XmCommandCallbackStruct.
 * Specifying one or more of the following symbols in the callback bindings 
 * list will bind that symbol's value in the lexical environment of the callback:
 * CALLBACK_WIDGET
 * CALLBACK_REASON
 * CALLBACK_XEVENT
 * CALLBACK_VALUE
 * CALLBACK_LENGTH
 ******************************************************************************/
LVAL Xm_Command_Widget_Class_Method_SET_CALLBACK()
{
  return (Wcb_Meta_Method_Add_Callback(XmCommandCallbackStruct_Callbackproc, TRUE));
}


/******************************************************************************
 * (send <commandwidget> :get_child <symbol>)
 * This method returns a WIDGETOBJ corresonding to <symbol>
 * <symbol> can be :DIALOG_COMMAND_TEXT,
 *                 :DIALOG_HISTORY_LIST, or
 *                 :DIALOG_PROMPT_LABEL
 *
 * Widget XmCommandGetChild (widget, child)
 * Widget   widget;
 * unsigned char   child;
 ******************************************************************************/
LVAL Xm_Command_Widget_Class_Method_GET_CHILD()
{
  extern LVAL s_XmDIALOG_COMMAND_TEXT, s_XmDIALOG_HISTORY_LIST, s_XmDIALOG_PROMPT_LABEL; /* w_resources.c */
#ifdef WINTERP_MOTIF_12
  extern LVAL s_XmDIALOG_WORK_AREA; /* w_resources.c */
#endif /* WINTERP_MOTIF_12 */
  LVAL self, lval_child;
  Widget widget_id;
  unsigned char child;

  widget_id = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&self);
  lval_child = xlgasymbol();
  xllastarg();
  
  if (lval_child == s_XmDIALOG_COMMAND_TEXT)
    child = XmDIALOG_COMMAND_TEXT;
  else if (lval_child == s_XmDIALOG_HISTORY_LIST)
    child = XmDIALOG_HISTORY_LIST;
  else if (lval_child == s_XmDIALOG_PROMPT_LABEL)
    child = XmDIALOG_PROMPT_LABEL;
#ifdef WINTERP_MOTIF_12
  else if (lval_child == s_XmDIALOG_WORK_AREA)
    child = XmDIALOG_WORK_AREA;
#endif /* WINTERP_MOTIF_12 */
  else 
    xlerror("COMMAND_WIDGET_CLASS method :GET_CHILD -- unknown child type.", lval_child);

  return (Wcls_WidgetID_To_WIDGETOBJ(XmCommandGetChild(widget_id, child)));
}


/******************************************************************************
 * (send <commandwidget> :set_value <value>)
 * This sets the text in the widget's command area to the string or XmString
 * <value>. If a normal string is given, it will be converted to an XmString
 * and returned as the  method's result.
 *
 * void XmCommandSetValue (widget, value)
 * Widget   widget;
 * XmString value;
 ******************************************************************************/
LVAL Xm_Command_Widget_Class_Method_SET_VALUE()
{
  LVAL self, lval_value;
  Widget widget_id;
  XmString value;

  widget_id = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&self);
  value = Get_String_or_XmString_Arg_Returning_XmString(&lval_value);
  xllastarg();
  
  /* the lame part about all this is that the XmString value that we may
     have converted above will just be cvted back to a string below. */
  XmCommandSetValue(widget_id, value);
  
  return (lval_value);
}


/******************************************************************************
 * (send <commandwidget> :append_value <value>)
 * This appends the string or XmString <value> to the string in the command
 * area widget. If a normal string is given, it will be converted to an
 * XmString and returned as the method's result.
 * 
 * void XmCommandAppendValue (widget, value)
 * Widget widget;
 * XmString value;
 ******************************************************************************/
LVAL Xm_Command_Widget_Class_Method_APPEND_VALUE()
{
  LVAL self, lval_value;
  Widget widget_id;
  XmString value;

  widget_id = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&self);
  value = Get_String_or_XmString_Arg_Returning_XmString(&lval_value);
  xllastarg();
  
  /* the lame part about all this is that the XmString value that we may
     have converted above will just be cvted back to a string below */
  XmCommandAppendValue(widget_id, value);
  
  return (lval_value);
}


/******************************************************************************
 * (send <commandwidget> :error <error>)
 * This temporarily displays the string or XmString <error> in the history
 * area of the command widget, the display is cleared upon entry of the
 * next command. If a normal string is given, it will be converted to an
 * XmString and returned as the method's result.
 *
 * void XmCommandError (widget, error)
 * Widget widget;
 * XmString error;
 ******************************************************************************/
LVAL Xm_Command_Widget_Class_Method_ERROR()
{
  LVAL self, lval_value;
  Widget widget_id;
  XmString value;

  widget_id = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&self);
  value = Get_String_or_XmString_Arg_Returning_XmString(&lval_value);
  xllastarg();
  
  /* the lame part about all this is that the XmString value that we may
     have converted above will just be cvted back to a string below */
  XmCommandError(widget_id, value);
  
  return (lval_value);
}


/******************************************************************************
 * (send <commandwidget> :GET_HISTORY_ITEMS)
 * 	==> returns an array of XmStrings.
 *
 * This retrieves the XmCommand widget resources XmNhistoryItems and
 * XmNhistoryItemCount from <commandwidget> and returns an array of XmStrings
 * representing the history items.
 *******************************************************************************/
LVAL Xm_Command_Widget_Class_Method_GET_HISTORY_ITEMS()
{
  LVAL self;
  Widget widget_id;
  XmStringTable xmstrtab;
  int		xmstrtab_size;

  widget_id = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&self);
  xllastarg();

  ARGLIST_RESET();
  ARGLIST_ADD(XmNhistoryItems, &xmstrtab);
  ARGLIST_ADD(XmNhistoryItemCount, &xmstrtab_size);
  XtGetValues(widget_id, ARGLIST());

  return (Wxms_XmStringTable_To_Lisp_Vector(xmstrtab, xmstrtab_size));
}


/******************************************************************************
 *
 ******************************************************************************/
Wc_Command_Init()
{
  LVAL o_XM_COMMAND_WIDGET_CLASS;

  o_XM_COMMAND_WIDGET_CLASS =
    Wcls_Create_Subclass_Of_WIDGET_CLASS("XM_COMMAND_WIDGET_CLASS",
					 xmCommandWidgetClass);

  /* a special :isnew method on this class allows for the creation of this
   * widget with Command.c:XmCreateCommand(), rather than using default
   * :ISNEW (which does XtCreateWidget()). The only special thing done by
   * XmCreateCommand() is that it prepends the following arg to the arglist:
   * XmNdialogType == XmDIALOG_COMMAND.
   */
  xladdmsg(o_XM_COMMAND_WIDGET_CLASS, ":ISNEW", 
	   FTAB_Xm_Command_Widget_Class_Method_ISNEW);

  xladdmsg(o_XM_COMMAND_WIDGET_CLASS, ":ADD_CALLBACK",
           FTAB_Xm_Command_Widget_Class_Method_ADD_CALLBACK);

  xladdmsg(o_XM_COMMAND_WIDGET_CLASS, ":SET_CALLBACK",
           FTAB_Xm_Command_Widget_Class_Method_SET_CALLBACK);

  xladdmsg(o_XM_COMMAND_WIDGET_CLASS, ":GET_CHILD",
           FTAB_Xm_Command_Widget_Class_Method_GET_CHILD);

  xladdmsg(o_XM_COMMAND_WIDGET_CLASS, ":ERROR",
	   FTAB_Xm_Command_Widget_Class_Method_ERROR);

  xladdmsg(o_XM_COMMAND_WIDGET_CLASS, ":SET_VALUE",
	   FTAB_Xm_Command_Widget_Class_Method_SET_VALUE);

  xladdmsg(o_XM_COMMAND_WIDGET_CLASS, ":APPEND_VALUE",
	   FTAB_Xm_Command_Widget_Class_Method_APPEND_VALUE);

  xladdmsg(o_XM_COMMAND_WIDGET_CLASS, ":GET_HISTORY_ITEMS",
	   FTAB_Xm_Command_Widget_Class_Method_GET_HISTORY_ITEMS);
}

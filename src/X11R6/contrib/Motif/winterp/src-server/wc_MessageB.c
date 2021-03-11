/* -*-C-*-
********************************************************************************
*
* File:         wc_MessageB.c
* RCS:          $Header: /users/npm/src/winterp/src-server/RCS/wc_MessageB.c,v 2.6 1994/06/06 15:40:45 npm Exp $
* Description:  XM_MESSAGE_BOX_WIDGET_CLASS
* Author:       Niels Mayer
* Created:      Sat Oct 28 03:40:20 1989
* Modified:     Sun Jun  5 15:02:29 1994 (Niels Mayer) npm@indeed
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
static char rcs_identity[] = "@(#)$Header: /users/npm/src/winterp/src-server/RCS/wc_MessageB.c,v 2.6 1994/06/06 15:40:45 npm Exp $";

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
#include <Xm/MessageB.h>
#include "winterp.h"
#include "w_funtab.h"

static LVAL k_message_dialog;
static LVAL k_error_dialog;
static LVAL k_information_dialog;
static LVAL k_question_dialog;
static LVAL k_warning_dialog;
static LVAL k_working_dialog;
#ifdef WINTERP_MOTIF_12
static LVAL k_template_dialog;
#endif /* WINTERP_MOTIF_12 */

/*****************************************************************************
 * (send XM_MESSAGE_BOX_WIDGET_CLASS :new 
 *                           [:managed/:unmanaged]
 *                           [:message_dialog/:error_dialog/
 *                            :information_dialog,:question_dialog,
 *                            :warning_dialog,:working_dialog,:template_dialog]
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
 *    (send XM_MESSAGE_BOX_WIDGET_CLASS :new ...)
 *    --> XmCreateMessageBox();
 *    (send XM_MESSAGE_BOX_WIDGET_CLASS :new :message_dialog ...)
 *    --> XmCreateMessageDialog();
 *    (send XM_MESSAGE_BOX_WIDGET_CLASS :new :error_dialog ...)
 *    --> XmCreateErrorDialog();
 *     (send XM_MESSAGE_BOX_WIDGET_CLASS :new :information_dialog ...)
 *     --> XmCreateInformationDialog();
 *     (send XM_MESSAGE_BOX_WIDGET_CLASS :new :question_dialog ...)
 *     --> XmCreateQuestionDialog();
 *     (send XM_MESSAGE_BOX_WIDGET_CLASS :new :warning_dialog ...)
 *     --> XmCreateWarningDialog();
 *     (send XM_MESSAGE_BOX_WIDGET_CLASS :new :working_dialog ...)
 *     --> XmCreateWorkingDialog();
 *     (send XM_MESSAGE_BOX_WIDGET_CLASS :new :template_dialog ...) [Motif >= 1.2]
 *     --> XmCreateTemplateDialog();
 ****************************************************************************/
LVAL Xm_Message_Box_Widget_Class_Method_ISNEW()
{
  LVAL self, o_parent;
  char* name;
  Boolean managed_p;
  LVAL dialog_kind;
  Widget widget_id, parent_widget_id;

  self = Wcls_Get_OBJECT_Arg_Returning_WIDGETOBJ(); /* NOTE: xlobj.c:clnew() returns an OBJECT; 
						       get the arg and mark it's type slot as a WIDGETOBJ */
  
  /* get optional managed/unmanaged arg */
  if (moreargs() && ((*xlargv == k_managed) || (*xlargv == k_unmanaged)))
    managed_p = (nextarg() == k_managed);
  else
    managed_p = FALSE;		/* by default don't call XtManageChild() */

  /* get optional :message_dialog/:error_dialog/... arg */
  if (moreargs() && ((*xlargv == k_message_dialog)     ||
		     (*xlargv == k_error_dialog)       ||
		     (*xlargv == k_information_dialog) ||
		     (*xlargv == k_question_dialog)    ||
		     (*xlargv == k_warning_dialog)     ||
		     (*xlargv == k_working_dialog)
#ifdef WINTERP_MOTIF_12
		     || (*xlargv == k_template_dialog)
#endif /* WINTERP_MOTIF_12 */		     
		     ))
    dialog_kind = nextarg();
  else
    dialog_kind = NIL;		/* default is XmCreateMessageBox() */

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
    if (dialog_kind == NIL)
      widget_id = XmCreateMessageBox(parent_widget_id, name, xt_arglist, xt_numargs);
    else if (dialog_kind == k_message_dialog)
      widget_id = XmCreateMessageDialog(parent_widget_id, name, xt_arglist, xt_numargs);
    else if (dialog_kind == k_error_dialog)
      widget_id = XmCreateErrorDialog(parent_widget_id, name, xt_arglist, xt_numargs);
    else if (dialog_kind == k_information_dialog)
      widget_id = XmCreateInformationDialog(parent_widget_id, name, xt_arglist, xt_numargs);
    else if (dialog_kind == k_question_dialog)
      widget_id = XmCreateQuestionDialog(parent_widget_id, name, xt_arglist, xt_numargs);
    else if (dialog_kind == k_warning_dialog)
      widget_id = XmCreateWarningDialog(parent_widget_id, name,	xt_arglist, xt_numargs);
    else if (dialog_kind == k_working_dialog)
      widget_id = XmCreateWorkingDialog(parent_widget_id, name, xt_arglist, xt_numargs);
#ifdef WINTERP_MOTIF_12
    else if (dialog_kind == k_template_dialog)
      widget_id = XmCreateTemplateDialog(parent_widget_id, name, xt_arglist, xt_numargs);
#endif /* WINTERP_MOTIF_12 */
    else
      xlfatal("Bug in Xm_Message_Box_Widget_Class_Method_ISNEW()");
    Wres_Free_C_Arglist_Data();
  }
  else 
    if (dialog_kind == NIL)
      widget_id = XmCreateMessageBox(parent_widget_id, name, ARGLIST());
    else if (dialog_kind == k_message_dialog)
      widget_id = XmCreateMessageDialog(parent_widget_id, name, ARGLIST());
    else if (dialog_kind == k_error_dialog)
      widget_id = XmCreateErrorDialog(parent_widget_id, name, ARGLIST());
    else if (dialog_kind == k_information_dialog)
      widget_id = XmCreateInformationDialog(parent_widget_id, name, ARGLIST());
    else if (dialog_kind == k_question_dialog)
      widget_id = XmCreateQuestionDialog(parent_widget_id, name, ARGLIST());
    else if (dialog_kind == k_warning_dialog)
      widget_id = XmCreateWarningDialog(parent_widget_id, name, ARGLIST());	   
    else if (dialog_kind == k_working_dialog)
      widget_id = XmCreateWorkingDialog(parent_widget_id, name, ARGLIST());
#ifdef WINTERP_MOTIF_12
    else if (dialog_kind == k_template_dialog)
      widget_id = XmCreateTemplateDialog(parent_widget_id, name, ARGLIST());
#endif /* WINTERP_MOTIF_12 */
    else
      xlfatal("Bug in Xm_Message_Box_Widget_Class_Method_ISNEW()");

  /*
   * put the hidden XM_DIALOG_POPUP_SHELL_WIDGET_CLASS parent onto
   * wc_SHELL.c:shell_widget_alist -- this is needed so that dialog shells 
   * display busy cursor when Wshl_Prim_WINTERP_SHOW_BUSY() is set...
   */
  if (dialog_kind != NIL)
    (void) Wshl_WidgetID_To_WIDGETOBJ(XtParent(widget_id));

  Wcls_Initialize_WIDGETOBJ(self, widget_id);

  if (managed_p)
    XtManageChild(widget_id);
  
#ifdef WINTERP_DEBUG_1
  Wcls_Print_WidgetObj_Info(self);
#endif /* WINTERP_DEBUG_1 */
  return (self);
}


/*****************************************************************************
 * (send <messageboxwidget> :GET_CHILD <symbol>)
 * This method returns  a WIDGETOBJ, the child of <messageboxwidget> 
 * corresponding to <symbol>:
 * :DIALOG_DEFAULT_BUTTON
 * :DIALOG_SYMBOL_LABEL
 * :DIALOG_MESSAGE_LABEL
 * :DIALOG_OK_BUTTON
 * :DIALOG_CANCEL_BUTTON
 * :DIALOG_HELP_BUTTON
 * :DIALOG_SEPARATOR
 *
 * Widget XmMessageBoxGetChild (widget, child)
 * Widget          widget;
 * unsigned char   child;
 ****************************************************************************/
LVAL Xm_Message_Box_Widget_Class_Method_GET_CHILD()
{
  extern LVAL s_XmDIALOG_DEFAULT_BUTTON, s_XmDIALOG_SYMBOL_LABEL,
  s_XmDIALOG_MESSAGE_LABEL, s_XmDIALOG_OK_BUTTON, s_XmDIALOG_CANCEL_BUTTON,
  s_XmDIALOG_HELP_BUTTON, s_XmDIALOG_SEPARATOR;
  LVAL self, lval_child;
  Widget widget_id;
  unsigned char child;
  
  widget_id = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&self);
  lval_child = xlgasymbol();
  xllastarg();
  
  if (lval_child == s_XmDIALOG_DEFAULT_BUTTON)
    child = XmDIALOG_DEFAULT_BUTTON;
  else if (lval_child == s_XmDIALOG_SYMBOL_LABEL)
    child = XmDIALOG_SYMBOL_LABEL;
  else if (lval_child == s_XmDIALOG_MESSAGE_LABEL)
    child = XmDIALOG_MESSAGE_LABEL;
  else if (lval_child == s_XmDIALOG_OK_BUTTON)
    child = XmDIALOG_OK_BUTTON;
  else if (lval_child == s_XmDIALOG_CANCEL_BUTTON)
    child = XmDIALOG_CANCEL_BUTTON;
  else if (lval_child == s_XmDIALOG_HELP_BUTTON)
    child = XmDIALOG_HELP_BUTTON;
  else if (lval_child == s_XmDIALOG_SEPARATOR)
    child = XmDIALOG_SEPARATOR;
  else 
    xlerror("MESSAGE_BOX_WIDGET_CLASS method :GET_CHILD -- unknown child type.", lval_child);

  return (Wcls_WidgetID_To_WIDGETOBJ(XmMessageBoxGetChild(widget_id, child)));  
}

/******************************************************************************
 *
 ******************************************************************************/
Wc_MessageB_Init()
{
  LVAL o_XM_MESSAGE_BOX_WIDGET_CLASS;

  k_message_dialog     = xlenter(":MESSAGE_DIALOG");
  k_error_dialog       = xlenter(":ERROR_DIALOG");
  k_information_dialog = xlenter(":INFORMATION_DIALOG");
  k_question_dialog    = xlenter(":QUESTION_DIALOG");
  k_warning_dialog     = xlenter(":WARNING_DIALOG");
  k_working_dialog     = xlenter(":WORKING_DIALOG");
#ifdef WINTERP_MOTIF_12
  k_template_dialog    = xlenter(":TEMPLATE_DIALOG");
#endif /* WINTERP_MOTIF_12 */

  o_XM_MESSAGE_BOX_WIDGET_CLASS =
    Wcls_Create_Subclass_Of_WIDGET_CLASS("XM_MESSAGE_BOX_WIDGET_CLASS",
					 xmMessageBoxWidgetClass);
  
  /* a special :isnew method on this class allows for the creation of this
     widget inside a popup shell if one of the following submessage keywords
     are given: 
     :message_dialog, :error_dialog, :information_dialog, 
     :question_dialog, :warning_dialog, :working_dialog, :template_dialog */
  xladdmsg(o_XM_MESSAGE_BOX_WIDGET_CLASS, ":ISNEW", 
	   FTAB_Xm_Message_Box_Widget_Class_Method_ISNEW);

  xladdmsg(o_XM_MESSAGE_BOX_WIDGET_CLASS, ":GET_CHILD", 
	   FTAB_Xm_Message_Box_Widget_Class_Method_GET_CHILD);
}

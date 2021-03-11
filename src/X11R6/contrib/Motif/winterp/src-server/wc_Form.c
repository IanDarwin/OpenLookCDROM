/* -*-C-*-
********************************************************************************
*
* File:         wc_Form.c
* RCS:          $Header: /users/npm/src/winterp/src-server/RCS/wc_Form.c,v 2.6 1994/06/06 15:40:48 npm Exp $
* Description:  XM_FORM_WIDGET_CLASS
* Author:       Niels Mayer
* Created:      Fri Oct 27 22:35:52 1989
* Modified:     Sun Jun  5 14:59:20 1994 (Niels Mayer) npm@indeed
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
static char rcs_identity[] = "@(#)$Header: /users/npm/src/winterp/src-server/RCS/wc_Form.c,v 2.6 1994/06/06 15:40:48 npm Exp $";

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
#include <Xm/Form.h>
#include "winterp.h"
#include "w_funtab.h"


/*****************************************************************************
 * (send XM_FORM_WIDGET_CLASS :new 
 *                           [:managed/:unmanaged]
 *                           [:dialog]
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
 *     (send XM_FORM_WIDGET_CLASS :new ...)
 *     --> XmCreateForm();
 *     (send XM_FORM_WIDGET_CLASS :new :dialog ...)
 *     --> XmCreateFormDialog();
 ****************************************************************************/
LVAL Xm_Form_Widget_Class_Method_ISNEW()
{
  LVAL self, o_parent;
  char* name;
  Boolean managed_p, dialog_p;
  Widget parent_widget_id, widget_id;

  self = Wcls_Get_OBJECT_Arg_Returning_WIDGETOBJ(); /* NOTE: xlobj.c:clnew() returns an OBJECT; 
						       get the arg and mark it's type slot as a WIDGETOBJ */

  /* get optional managed/unmanaged arg */
  if (moreargs() && ((*xlargv == k_managed) || (*xlargv == k_unmanaged)))
    managed_p = (nextarg() == k_managed);
  else
    managed_p = FALSE;		/* by default don't call XtManageChild() */

  /* get optional :dialog arg */
  if (moreargs() && (*xlargv == k_dialog)) {
    nextarg();
    dialog_p = TRUE;
  }
  else
    dialog_p = FALSE;		/* by default, we don't want a dialog widget */

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
    if (dialog_p)
      widget_id = XmCreateFormDialog(parent_widget_id, name, xt_arglist, xt_numargs);
    else
      widget_id = XmCreateForm(parent_widget_id, name, xt_arglist, xt_numargs);
    Wres_Free_C_Arglist_Data();
  }
  else 
    if (dialog_p)
      widget_id = XmCreateFormDialog(parent_widget_id, name, ARGLIST());
    else
      widget_id = XmCreateForm(parent_widget_id, name, ARGLIST());

  /*
   * put the hidden XM_DIALOG_POPUP_SHELL_WIDGET_CLASS parent onto
   * wc_SHELL.c:shell_widget_alist -- this is needed so that dialog shells 
   * display busy cursor when Wshl_Prim_WINTERP_SHOW_BUSY() is set...
   */
  if (dialog_p)
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
 *
 ******************************************************************************/
Wc_Form_Init()
{
  LVAL o_XM_FORM_WIDGET_CLASS;

  o_XM_FORM_WIDGET_CLASS =
    Wcls_Create_Subclass_Of_WIDGET_CLASS("XM_FORM_WIDGET_CLASS",
					 xmFormWidgetClass);

  /* a special :isnew method on this class allows for the creation of this
     widget in a popup dialog if the submessage keyword :dialog is given ... */
  xladdmsg(o_XM_FORM_WIDGET_CLASS, ":ISNEW", 
	   FTAB_Xm_Form_Widget_Class_Method_ISNEW);
}

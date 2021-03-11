/* -*-C-*-
********************************************************************************
*
* File:         wc_FileSB.c
* RCS:          $Header: /users/npm/src/winterp/src-server/RCS/wc_FileSB.c,v 2.6 1994/06/06 15:40:48 npm Exp $
* Description:  XM_FILE_SELECTION_BOX_WIDGET_CLASS
* Author:       Niels Mayer
* Created:      Fri Oct 27 22:29:33 1989
* Modified:     Sun Jun  5 14:58:48 1994 (Niels Mayer) npm@indeed
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
static char rcs_identity[] = "@(#)$Header: /users/npm/src/winterp/src-server/RCS/wc_FileSB.c,v 2.6 1994/06/06 15:40:48 npm Exp $";

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
#include <Xm/FileSB.h>
#include "winterp.h"
#include "w_funtab.h"
#include "w_XmString.h"


/*****************************************************************************
 * (send XM_FILE_SELECTION_BOX_WIDGET_CLASS :new 
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
 * (send XM_FILE_SELECTION_BOX_WIDGET_CLASS :new ...)
 *   --> XmCreateFileSelectionBox();
 * (send XM_FILE_SELECTION_BOX_WIDGET_CLASS :new :dialog ...)
 *   --> XmCreateFileSelectionDialog();
 ****************************************************************************/
LVAL Xm_File_Selection_Box_Widget_Class_Method_ISNEW()
{
  LVAL self, o_parent;
  char* name;
  Boolean managed_p, dialog_p;
  Widget widget_id, parent_widget_id;

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
      widget_id = XmCreateFileSelectionDialog(parent_widget_id, name, xt_arglist, xt_numargs);
    else
      widget_id = XmCreateFileSelectionBox(parent_widget_id, name, xt_arglist, xt_numargs);
    Wres_Free_C_Arglist_Data();
  }
  else 
    if (dialog_p)
      widget_id = XmCreateFileSelectionDialog(parent_widget_id, name, ARGLIST());
    else
      widget_id = XmCreateFileSelectionBox(parent_widget_id, name, ARGLIST());

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
 * typedef struct
 * {
 *     int reason;
 *     XEvent	*event;
 *     XmString	value;
 *     int	length;
 *     XmString	mask;
 *     int	mask_length;
 *     XmString	dir ;	         -- THIS FIELD EXISTS ONLY IN MOTIF 1.1
 *     int	dir_length ;     -- THIS FIELD EXISTS ONLY IN MOTIF 1.1
 *     XmString pattern ;        -- THIS FIELD EXISTS ONLY IN MOTIF 1.1
 *     int	pattern_length ; -- THIS FIELD EXISTS ONLY IN MOTIF 1.1
 * } XmFileSelectionBoxCallbackStruct;
 ******************************************************************************/
static LVAL s_CALLBACK_MASK, s_CALLBACK_MASK_LENGTH;
#ifdef WINTERP_MOTIF_11
static LVAL s_CALLBACK_DIR, s_CALLBACK_DIR_LENGTH, s_CALLBACK_PATTERN, s_CALLBACK_PATTERN_LENGTH;
#endif				/* WINTERP_MOTIF_11 */
static void Lexical_Bindings_For_XmFileSelectionBoxCallbackStruct(bindings_list,
								  lexical_env,
								  call_data,
								  client_data)
     LVAL bindings_list;	/* a list of symbols to which values from XmFileSelectionBoxCallbackStruct are bound */
     LVAL lexical_env;		
     XtPointer call_data;
     LVAL client_data;		/* XLTYPE_CALLBACKOBJ */
{
  register LVAL s_bindname;
  XmFileSelectionBoxCallbackStruct* cd;

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
    case XmCR_APPLY:		/* XmFileSelectionBox(:XMN_APPLY_CALLBACK)    (actually XmSelectionBox w/ XmFileSelectionBoxCallbackStruct) */
    case XmCR_CANCEL:		/* XmFileSelectionBox(:XMN_CANCEL_CALLBACK)   (actually XmSelectionBox w/ XmFileSelectionBoxCallbackStruct) */
    case XmCR_NO_MATCH:		/* XmFileSelectionBox(:XMN_NO_MATCH_CALLBACK) (actually XmSelectionBox w/ XmFileSelectionBoxCallbackStruct) */
    case XmCR_OK:		/* XmFileSelectionBox(:XMN_OK_CALLBACK)       (actually XmSelectionBox w/ XmFileSelectionBoxCallbackStruct) */
      cd = (XmFileSelectionBoxCallbackStruct*) call_data;
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
	else if (s_bindname == s_CALLBACK_MASK) {
	  xlpbind(s_bindname, (cd->mask) ? cv_xmstring(XmStringCopy(cd->mask)) : NIL, lexical_env);
	}
	else if (s_bindname == s_CALLBACK_MASK_LENGTH) {
	  xlpbind(s_bindname, (cd->mask) ? cvfixnum((FIXTYPE) cd->mask_length) : NIL, lexical_env);
	}
#ifdef WINTERP_MOTIF_11
	else if (s_bindname == s_CALLBACK_DIR) {
	  xlpbind(s_bindname, (cd->dir) ? cv_xmstring(XmStringCopy(cd->dir)) : NIL, lexical_env);
	}
	else if (s_bindname == s_CALLBACK_DIR_LENGTH) {
	  xlpbind(s_bindname, (cd->dir) ? cvfixnum((FIXTYPE) cd->dir_length) : NIL, lexical_env);
	}
	else if (s_bindname == s_CALLBACK_PATTERN) {
	  xlpbind(s_bindname, (cd->pattern) ? cv_xmstring(XmStringCopy(cd->pattern)) : NIL, lexical_env);
	}
	else if (s_bindname == s_CALLBACK_PATTERN_LENGTH) {
	  xlpbind(s_bindname, (cd->pattern) ? cvfixnum((FIXTYPE) cd->pattern_length) : NIL, lexical_env);
	}
#endif /* WINTERP_MOTIF_11 */
	else {
	  errputstr("Warning: in widget callback, ignored unknown binding name - ");
	  errprint(s_bindname);
	  sprintf(temptext,
#ifdef WINTERP_MOTIF_11
		  "\tValid symbols for XmFileSelectionBoxCallbackStruct are [%s %s %s %s %s %s %s %s %s %s %s] - ",
#else
		  "\tValid symbols for XmFileSelectionBoxCallbackStruct are [%s %s %s %s %s %s %s] - ",
#endif /* WINTERP_MOTIF_11 */
		  getstring(getpname(s_CALLBACK_WIDGET)),
		  getstring(getpname(s_CALLBACK_REASON)),
		  getstring(getpname(s_CALLBACK_XEVENT)),
		  getstring(getpname(s_CALLBACK_VALUE)),
		  getstring(getpname(s_CALLBACK_LENGTH)),
#ifdef WINTERP_MOTIF_11
		  getstring(getpname(s_CALLBACK_DIR)),
		  getstring(getpname(s_CALLBACK_DIR_LENGTH)),
		  getstring(getpname(s_CALLBACK_PATTERN)),
		  getstring(getpname(s_CALLBACK_PATTERN_LENGTH)),
#endif /* WINTERP_MOTIF_11 */
		  getstring(getpname(s_CALLBACK_MASK)),
		  getstring(getpname(s_CALLBACK_MASK_LENGTH)));
	  errputstr(temptext);
	  errprint(get_callback_widget(client_data));
	}
      }
      break;
    default:			/* for XmBulletinBoard(:XMN_FOCUS_CALLBACK,:XMN_MAP_CALLBACK,:XMN_UNMAP_CALLBACK), XmManager(:XMN_HELP_CALLBACK) */
      Lexical_Bindings_For_XmAnyCallbackStruct(bindings_list, lexical_env, call_data, client_data);
      break;
    }
}


/******************************************************************************
 * This is called indirectly via XtAddCallback() for callbacks returning
 * an XmFileSelectionBoxCallbackStruct as call_data.
 ******************************************************************************/
static void XmFileSelectionBoxCallbackStruct_Callbackproc(widget, client_data, call_data)
     Widget    widget;
     XtPointer client_data;
     XtPointer call_data;
{
  Wcb_Meta_Callbackproc(get_callback_closure((LVAL) client_data),
			(LVAL) client_data,
			call_data,
			Lexical_Bindings_For_XmFileSelectionBoxCallbackStruct,
			NULL,
			Wcb_Callback_Trace_Proc);
}


/******************************************************************************
 * Same as WIDGET_CLASS's :add_callback method except that this understands
 * how to get values from the XmFileSelectionBoxCallbackStruct.
 * Specifying one or more of the following symbols in the callback bindings 
 * list will bind that symbol's value in the lexical environment of the callback:
 * CALLBACK_WIDGET
 * CALLBACK_REASON
 * CALLBACK_XEVENT
 * CALLBACK_VALUE
 * CALLBACK_LENGTH
 * CALLBACK_MASK
 * CALLBACK_MASK_LENGTH
 * CALLBACK_DIR 		-- ONLY IN MOTIF 1.1
 * CALLBACK_DIR_LENGTH		-- ONLY IN MOTIF 1.1
 * CALLBACK_PATTERN		-- ONLY IN MOTIF 1.1
 * CALLBACK_PATTERN_LENGTH	-- ONLY IN MOTIF 1.1
 ******************************************************************************/
LVAL Xm_File_Selection_Box_Widget_Class_Method_ADD_CALLBACK()
{
  return (Wcb_Meta_Method_Add_Callback(XmFileSelectionBoxCallbackStruct_Callbackproc, FALSE));
}


/******************************************************************************
 * Same as WIDGET_CLASS's :set_callback method except that this understands
 * how to get values from the XmFileSelectionBoxCallbackStruct.
 * Specifying one or more of the following symbols in the callback bindings 
 * list will bind that symbol's value in the lexical environment of the callback:
 * CALLBACK_WIDGET
 * CALLBACK_REASON
 * CALLBACK_XEVENT
 * CALLBACK_VALUE
 * CALLBACK_LENGTH
 * CALLBACK_MASK
 * CALLBACK_MASK_LENGTH
 * CALLBACK_DIR 		-- ONLY IN MOTIF 1.1
 * CALLBACK_DIR_LENGTH		-- ONLY IN MOTIF 1.1
 * CALLBACK_PATTERN		-- ONLY IN MOTIF 1.1
 * CALLBACK_PATTERN_LENGTH	-- ONLY IN MOTIF 1.1
 ******************************************************************************/
LVAL Xm_File_Selection_Box_Widget_Class_Method_SET_CALLBACK()
{
  return (Wcb_Meta_Method_Add_Callback(XmFileSelectionBoxCallbackStruct_Callbackproc, TRUE));
}


/******************************************************************************
 * (send <fileselboxwidget> :GET_CHILD <child_sym>)
 * This method returns a WIDGETOBJ corresponding to one of
 * <fileselboxwidget> children. <child_sym> can be one of:
 *	:DIALOG_WORK_AREA
 *	:DIALOG_SEPARATOR
 *	:DIALOG_APPLY_BUTTON
 *	:DIALOG_CANCEL_BUTTON
 *	:DIALOG_DEFAULT_BUTTON
 *	:DIALOG_FILTER_LABEL
 *	:DIALOG_FILTER_TEXT
 *	:DIALOG_HELP_BUTTON
 *	:DIALOG_LIST
 *	:DIALOG_LIST_LABEL
 *	:DIALOG_OK_BUTTON
 *	:DIALOG_SELECTION_LABEL
 *	:DIALOG_TEXT
 *	:DIALOG_DIR_LIST	-- EXISTS ONLY IN MOTIF 1.1
 *	:DIALOG_DIR_LIST_LABEL	-- EXISTS ONLY IN MOTIF 1.1
 *
 *  Widget XmFileSelectionBoxGetChild (fs, which)
 *  Widget fs;	         -- SelectionBox widget 
 *  unsigned char which; --  which child
 ******************************************************************************/
LVAL Xm_File_Selection_Box_Widget_Class_Method_GET_CHILD()
{
  extern LVAL s_XmDIALOG_FILTER_LABEL, s_XmDIALOG_FILTER_TEXT,
#ifdef WINTERP_MOTIF_11
  s_XmDIALOG_DIR_LIST, s_XmDIALOG_DIR_LIST_LABEL,
#endif				/* WINTERP_MOTIF_11 */
  s_XmDIALOG_LIST, s_XmDIALOG_LIST_LABEL, s_XmDIALOG_SELECTION_LABEL, 
  s_XmDIALOG_WORK_AREA, s_XmDIALOG_TEXT, s_XmDIALOG_SEPARATOR,
  s_XmDIALOG_OK_BUTTON, s_XmDIALOG_APPLY_BUTTON, s_XmDIALOG_CANCEL_BUTTON,
  s_XmDIALOG_HELP_BUTTON, s_XmDIALOG_DEFAULT_BUTTON;
  LVAL self, lval_child;
  Widget widget_id;
  unsigned char child;

  widget_id = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&self);
  lval_child = xlgasymbol();
  xllastarg();
  
  if (lval_child == s_XmDIALOG_FILTER_LABEL)
    child = XmDIALOG_FILTER_LABEL;
  else if (lval_child == s_XmDIALOG_FILTER_TEXT)
    child = XmDIALOG_FILTER_TEXT;
#ifdef WINTERP_MOTIF_11
  else if (lval_child == s_XmDIALOG_DIR_LIST)
    child = XmDIALOG_DIR_LIST;
  else if (lval_child == s_XmDIALOG_DIR_LIST_LABEL)
    child = XmDIALOG_DIR_LIST_LABEL;
#endif				/* WINTERP_MOTIF_11 */
  /* the following are from the selection box widget */
  else if (lval_child == s_XmDIALOG_LIST)
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
    xlerror("FILE_SELECTION_BOX_WIDGET_CLASS method :GET_CHILD -- unknown child type.", lval_child);

  return (Wcls_WidgetID_To_WIDGETOBJ(XmFileSelectionBoxGetChild(widget_id, child))); 
}


/******************************************************************************
 * (send <fileselboxwidget> :DO_SEARCH [<dirmask>])
 * this method initiates a directory search. The search will be according to 
 * the string or XmString <dirmask>. If <dirmask> is NIL or not supplied, then
 * the search will use the current directory mask. This method returns <dirmask>
 * as an XmString.
 *
 * void XmFileSelectionDoSearch(fs, dirmask)
 * Widget fs;
 * XmString   dirmask;
 ******************************************************************************/
LVAL Xm_File_Selection_Box_Widget_Class_Method_DO_SEARCH()
{
  LVAL self, lval_dirmask;
  Widget widget_id;
  XmString dirmask;

  widget_id = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&self);
  if (moreargs()) {
    if (*xlargv != NIL)		/* get a non-NIL <dirmask> argument */
      dirmask = Get_String_or_XmString_Arg_Returning_XmString(&lval_dirmask);
    else {
      nextarg();		/* else skip a NIL <dirmask> argument */
      dirmask = NULL;
      lval_dirmask = NIL;
    }
  }
  else {			/* else no <dirmask> argument */
    dirmask = NULL;
    lval_dirmask = NIL;
  }
  xllastarg();
  
  XmFileSelectionDoSearch(widget_id, dirmask);
  
  return (lval_dirmask);
}


#ifdef WINTERP_MOTIF_11
/******************************************************************************
 * (send <fileselboxwidget> :GET_DIR_LIST_ITEMS)
 * 	==> returns an array of XmStrings.
 *
 * This retrieves the XmFileSelectionBox widget resources XmNdirListItems and
 * XmNdirListItemCount from <fileselboxwidget> and returns an array of XmStrings
 * representing the items in the directory list.
 *******************************************************************************/
LVAL Xm_File_Selection_Box_Widget_Class_Method_GET_DIR_LIST_ITEMS()
{
  LVAL self;
  Widget widget_id;
  XmStringTable xmstrtab;
  int		xmstrtab_size;

  widget_id = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&self);
  xllastarg();

  ARGLIST_RESET();
  ARGLIST_ADD(XmNdirListItems, &xmstrtab);
  ARGLIST_ADD(XmNdirListItemCount, &xmstrtab_size);
  XtGetValues(widget_id, ARGLIST());

  return (Wxms_XmStringTable_To_Lisp_Vector(xmstrtab, xmstrtab_size));
}


/******************************************************************************
 * (send <fileselboxwidget>  :GET_FILE_LIST_ITEMS)
 * 	==> returns an array of XmStrings
 *
 * This retrieves the XmFileSelectionBox widget resources XmNfileListItems and
 * XmNfileListItemCount from <fileselboxwidget> and returns an array of XmStrings
 * representing the items in the file list.
 *******************************************************************************/
LVAL Xm_File_Selection_Box_Widget_Class_Method_GET_FILE_LIST_ITEMS()
{
  LVAL self;
  Widget widget_id;
  XmStringTable xmstrtab;
  int		xmstrtab_size;

  widget_id = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&self);
  xllastarg();

  ARGLIST_RESET();
  ARGLIST_ADD(XmNfileListItems, &xmstrtab);
  ARGLIST_ADD(XmNfileListItemCount, &xmstrtab_size);
  XtGetValues(widget_id, ARGLIST());

  return (Wxms_XmStringTable_To_Lisp_Vector(xmstrtab, xmstrtab_size));
}
#endif				/* WINTERP_MOTIF_11 */


/******************************************************************************
 *
 ******************************************************************************/
Wc_FileSB_Init()
{
  LVAL o_XM_FILE_SELECTION_BOX_WIDGET_CLASS;

  o_XM_FILE_SELECTION_BOX_WIDGET_CLASS =
    Wcls_Create_Subclass_Of_WIDGET_CLASS("XM_FILE_SELECTION_BOX_WIDGET_CLASS",
					 xmFileSelectionBoxWidgetClass);

  /* a special :isnew method on this class allows for the creation of this
     widget in a popup dialog if the submessage keyword :dialog is given ... */
  xladdmsg(o_XM_FILE_SELECTION_BOX_WIDGET_CLASS, ":ISNEW", 
	   FTAB_Xm_File_Selection_Box_Widget_Class_Method_ISNEW);

  xladdmsg(o_XM_FILE_SELECTION_BOX_WIDGET_CLASS, ":ADD_CALLBACK",
           FTAB_Xm_File_Selection_Box_Widget_Class_Method_ADD_CALLBACK);

  xladdmsg(o_XM_FILE_SELECTION_BOX_WIDGET_CLASS, ":SET_CALLBACK",
           FTAB_Xm_File_Selection_Box_Widget_Class_Method_SET_CALLBACK);

  xladdmsg(o_XM_FILE_SELECTION_BOX_WIDGET_CLASS, ":GET_CHILD",
           FTAB_Xm_File_Selection_Box_Widget_Class_Method_GET_CHILD);

  xladdmsg(o_XM_FILE_SELECTION_BOX_WIDGET_CLASS, ":DO_SEARCH",
	   FTAB_Xm_File_Selection_Box_Widget_Class_Method_DO_SEARCH);

#ifdef WINTERP_MOTIF_11
  xladdmsg(o_XM_FILE_SELECTION_BOX_WIDGET_CLASS, ":GET_DIR_LIST_ITEMS",
	   FTAB_Xm_File_Selection_Box_Widget_Class_Method_GET_DIR_LIST_ITEMS);

  xladdmsg(o_XM_FILE_SELECTION_BOX_WIDGET_CLASS, ":GET_FILE_LIST_ITEMS",
	   FTAB_Xm_File_Selection_Box_Widget_Class_Method_GET_FILE_LIST_ITEMS);
#endif				/* WINTERP_MOTIF_11 */

  s_CALLBACK_MASK           = xlenter("CALLBACK_MASK");
  s_CALLBACK_MASK_LENGTH    = xlenter("CALLBACK_MASK_LENGTH");
#ifdef WINTERP_MOTIF_11
  s_CALLBACK_DIR            = xlenter("CALLBACK_DIR");
  s_CALLBACK_DIR_LENGTH     = xlenter("CALLBACK_DIR_LENGTH");
  s_CALLBACK_PATTERN        = xlenter("CALLBACK_PATTERN");
  s_CALLBACK_PATTERN_LENGTH = xlenter("CALLBACK_PATTERN_LENGTH");
#endif				/* WINTERP_MOTIF_11 */
}

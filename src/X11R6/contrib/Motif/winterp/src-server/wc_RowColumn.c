/* -*-C-*-
********************************************************************************
*
* File:         wc_RowColumn.c
* RCS:          $Header: /users/npm/src/winterp/src-server/RCS/wc_RowColumn.c,v 2.9 1994/06/06 15:40:44 npm Exp $
* Description:  XM_ROW_COLUMN_WIDGET_CLASS
* Author:       Niels Mayer
* Created:      Sat Oct 28 04:28:02 1989
* Modified:     Sun Jun  5 15:04:16 1994 (Niels Mayer) npm@indeed
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
static char rcs_identity[] = "@(#)$Header: /users/npm/src/winterp/src-server/RCS/wc_RowColumn.c,v 2.9 1994/06/06 15:40:44 npm Exp $";

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
#include <Xm/RowColumn.h>
#include "winterp.h"
#include "w_funtab.h"


/*****************************************************************************
 * (send XM_ROW_COLUMN_WIDGET_CLASS :new 
 *                           [:managed/:unmanaged]
 *                           [:radio_box/:option_menu/:menu_bar/
 *                            :popup_menu,:pulldown_menu]
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
 *     (send XM_ROW_COLUMN_WIDGET_CLASS :new ...)
 *     -->  XmCreateRowColumn();
 *
 *     (send XM_ROW_COLUMN_WIDGET_CLASS :new :work_area...)		-- Motif 1.1 only
 *     -->  XmCreateWorkArea();						-- Motif 1.1 only
 *
 *     (send XM_ROW_COLUMN_WIDGET_CLASS :new :radio_box ...)
 *     -->  XmCreateRadioBox();
 *
 *     (send XM_ROW_COLUMN_WIDGET_CLASS :new :simple_radio_box ...)	-- Motif 1.1 only
 *     -->  XmCreateSimpleRadioBox();					-- Motif 1.1 only
 *
 *     (send XM_ROW_COLUMN_WIDGET_CLASS :new :option_menu ...)
 *     --> XmCreateOptionMenu();
 *
 *     (send XM_ROW_COLUMN_WIDGET_CLASS :new :simple_option_menu ...)	-- Motif 1.1 only
 *     --> XmCreateSimpleOptionMenu();	                                -- Motif 1.1 only
 *
 *     (send XM_ROW_COLUMN_WIDGET_CLASS :new :menu_bar ...)
 *     --> XmCreateMenuBar();
 *
 *     (send XM_ROW_COLUMN_WIDGET_CLASS :new :simple_menu_bar ...)	-- Motif 1.1 only
 *     --> XmCreateSimpleMenuBar();					-- Motif 1.1 only
 *
 *     (send XM_ROW_COLUMN_WIDGET_CLASS :new :popup_menu ...)
 *     --> XmCreatePopupMenu();
 *
 *     (send XM_ROW_COLUMN_WIDGET_CLASS :new :simple_popup_menu ...)	-- Motif 1.1 only
 *     --> XmCreateSimplePopupMenu();					-- Motif 1.1 only
 *
 *     (send XM_ROW_COLUMN_WIDGET_CLASS :new :pulldown_menu ...)
 *     --> XmCreatePulldownMenu();
 *
 *     (send XM_ROW_COLUMN_WIDGET_CLASS :new :simple_pulldown_menu ...) -- Motif 1.1 only
 *     --> XmCreateSimplePulldownMenu();					-- Motif 1.1 only
 *
 *     (send XM_ROW_COLUMN_WIDGET_CLASS :new :simple_check_box ...)	-- Motif 1.1 only
 *     --> XmCreateSimpleCheckBox();					-- Motif 1.1 only
 ****************************************************************************/
static LVAL k_RADIO_BOX, k_OPTION_MENU, k_MENU_BAR, k_POPUP_MENU, k_PULLDOWN_MENU;
#ifdef WINTERP_MOTIF_11
static LVAL k_WORK_AREA, k_SIMPLE_RADIO_BOX, k_SIMPLE_OPTION_MENU, k_SIMPLE_MENU_BAR, k_SIMPLE_POPUP_MENU, k_SIMPLE_PULLDOWN_MENU, k_SIMPLE_CHECK_BOX;
#endif				/* WINTERP_MOTIF_11 */
LVAL Xm_Row_Column_Widget_Class_Method_ISNEW()
{
  LVAL self, o_parent;
  char* name;
  Boolean managed_p;
  LVAL rc_kind;
  Widget widget_id, parent_widget_id;

  self = Wcls_Get_OBJECT_Arg_Returning_WIDGETOBJ(); /* NOTE: xlobj.c:clnew() returns an OBJECT; 
						       get the arg and mark it's type slot as a WIDGETOBJ */
  
  /* get optional managed/unmanaged arg */
  if (moreargs() && ((*xlargv == k_managed) || (*xlargv == k_unmanaged)))
    managed_p = (nextarg() == k_managed);
  else
    managed_p = FALSE;		/* by default don't call XtManageChild() */

  /* get optional :radio_box/:option_menu... arg */
  if (moreargs() && (   (*xlargv == k_RADIO_BOX)
		     || (*xlargv == k_OPTION_MENU)
		     || (*xlargv == k_MENU_BAR)
		     || (*xlargv == k_POPUP_MENU)
		     || (*xlargv == k_PULLDOWN_MENU)
#ifdef WINTERP_MOTIF_11
		     || (*xlargv == k_WORK_AREA)
		     || (*xlargv == k_SIMPLE_RADIO_BOX)
		     || (*xlargv == k_SIMPLE_OPTION_MENU)
		     || (*xlargv == k_SIMPLE_MENU_BAR)
		     || (*xlargv == k_SIMPLE_POPUP_MENU)
		     || (*xlargv == k_SIMPLE_PULLDOWN_MENU)
		     || (*xlargv == k_SIMPLE_CHECK_BOX)
#endif				/* WINTERP_MOTIF_11 */
		     ))
    rc_kind = nextarg();
  else
    rc_kind = NIL;		/* default is XmCreateRowColumn() */

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
    if (rc_kind == NIL)
      widget_id = XmCreateRowColumn(parent_widget_id, name, xt_arglist, xt_numargs);
    else if (rc_kind == k_RADIO_BOX)
      widget_id = XmCreateRadioBox(parent_widget_id, name, xt_arglist, xt_numargs);
    else if (rc_kind == k_OPTION_MENU)
      widget_id = XmCreateOptionMenu(parent_widget_id, name, xt_arglist, xt_numargs);
    else if (rc_kind == k_MENU_BAR)
      widget_id = XmCreateMenuBar(parent_widget_id, name, xt_arglist, xt_numargs);
    else if (rc_kind == k_POPUP_MENU)
      widget_id = XmCreatePopupMenu(parent_widget_id, name, xt_arglist, xt_numargs);
    else if (rc_kind == k_PULLDOWN_MENU)
      widget_id = XmCreatePulldownMenu(parent_widget_id, name, xt_arglist, xt_numargs);
#ifdef WINTERP_MOTIF_11
    else if (rc_kind == k_WORK_AREA)
      widget_id = XmCreateWorkArea(parent_widget_id, name, xt_arglist, xt_numargs);
    else if (rc_kind == k_SIMPLE_RADIO_BOX)
      widget_id = XmCreateSimpleRadioBox(parent_widget_id, name, xt_arglist, xt_numargs);
    else if (rc_kind == k_SIMPLE_OPTION_MENU)
      widget_id = XmCreateSimpleOptionMenu(parent_widget_id, name, xt_arglist, xt_numargs);
    else if (rc_kind == k_SIMPLE_MENU_BAR)
      widget_id = XmCreateSimpleMenuBar(parent_widget_id, name, xt_arglist, xt_numargs);
    else if (rc_kind == k_SIMPLE_POPUP_MENU)
      widget_id = XmCreateSimplePopupMenu(parent_widget_id, name, xt_arglist, xt_numargs);
    else if (rc_kind == k_SIMPLE_PULLDOWN_MENU)
      widget_id = XmCreateSimplePulldownMenu(parent_widget_id, name, xt_arglist, xt_numargs);
    else if (rc_kind == k_SIMPLE_CHECK_BOX)
      widget_id = XmCreateSimpleCheckBox(parent_widget_id, name, xt_arglist, xt_numargs);
#endif				/* WINTERP_MOTIF_11 */
    else
      xlfatal("Bug in Xm_Row_Column_Widget_Class_Method_ISNEW()");
    Wres_Free_C_Arglist_Data();
  }
  else 
    if (rc_kind == NIL)
      widget_id = XmCreateRowColumn(parent_widget_id, name, ARGLIST());
    else if (rc_kind == k_RADIO_BOX)
      widget_id = XmCreateRadioBox(parent_widget_id, name, ARGLIST());
    else if (rc_kind == k_OPTION_MENU)
      widget_id = XmCreateOptionMenu(parent_widget_id, name, ARGLIST());
    else if (rc_kind == k_MENU_BAR)
      widget_id = XmCreateMenuBar(parent_widget_id, name, ARGLIST());
    else if (rc_kind == k_POPUP_MENU)
      widget_id = XmCreatePopupMenu(parent_widget_id, name, ARGLIST());
    else if (rc_kind == k_PULLDOWN_MENU)
      widget_id = XmCreatePulldownMenu(parent_widget_id, name, ARGLIST());
#ifdef WINTERP_MOTIF_11
    else if (rc_kind == k_WORK_AREA)
      widget_id = XmCreateWorkArea(parent_widget_id, name, ARGLIST());
    else if (rc_kind == k_SIMPLE_RADIO_BOX)
      widget_id = XmCreateSimpleRadioBox(parent_widget_id, name, ARGLIST());
    else if (rc_kind == k_SIMPLE_OPTION_MENU)
      widget_id = XmCreateSimpleOptionMenu(parent_widget_id, name, ARGLIST());
    else if (rc_kind == k_SIMPLE_MENU_BAR)
      widget_id = XmCreateSimpleMenuBar(parent_widget_id, name, ARGLIST());
    else if (rc_kind == k_SIMPLE_POPUP_MENU)
      widget_id = XmCreateSimplePopupMenu(parent_widget_id, name, ARGLIST());
    else if (rc_kind == k_SIMPLE_PULLDOWN_MENU)
      widget_id = XmCreateSimplePulldownMenu(parent_widget_id, name, ARGLIST());
    else if (rc_kind == k_SIMPLE_CHECK_BOX)
      widget_id = XmCreateSimpleCheckBox(parent_widget_id, name, ARGLIST());
#endif				/* WINTERP_MOTIF_11 */
    else
      xlfatal("Bug in Xm_Row_Column_Widget_Class_Method_ISNEW()");

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
 *     int     reason;
 *     XEvent  *event;
 *     Widget  widget;
 *     char    *data;
 *     char    *callbackstruct;
 * } XmRowColumnCallbackStruct;
 *
 * Note: if rowcol widget has an XmNentryCallback set, then any 
 * activate action on any of it's entries will produce a callback
 * on the rowcol widget with
 *    cb.reason==XmCR_ACTIVATE,
 *    cb.event==cb.callbackstruct->event
 *    cb.widget==<entrywidget>
 *    cb.data==<entrywidget's client-data if a activate callback was spec'd for entry and it had client-data> 
 *    cb.callbackstruct==<entrywidget's call-data> (XmAnyCallbackStruct/XmToggleButtonCallbackStruct)
 *
 * The weird part here is the cb.callbackstruct slot -- we don't know the class
 * of the entrywidget, so we can't know what kind of callbackstruct it got.
 * We can only be guaranteed that the activated-entry-widget's .reason 
 * fields are valid. If we have a toggle button, then we may want to 
 * access the .set slot of the XmToggleButtonCallbackStruct. (??)
 ******************************************************************************/
static LVAL s_CALLBACK_ENTRY_WIDGET, s_CALLBACK_ENTRY_REASON, s_CALLBACK_ENTRY_SET, s_CALLBACK_ENTRY_DATA;
#ifdef WINTERP_MOTIF_11
static LVAL s_CALLBACK_ENTRY_CLICK_COUNT;
#endif				/* WINTERP_MOTIF_11 */

static void Lexical_Bindings_For_XmRowColumnCallbackStruct(bindings_list,
							   lexical_env,
							   call_data,
							   client_data)
     LVAL bindings_list;	/* a list of symbols to which values from XmRowColumnCallbackStruct are bound */
     LVAL lexical_env;		
     XtPointer call_data;
     LVAL client_data;		/* XLTYPE_CALLBACKOBJ */
{
  extern LVAL true;
  register LVAL s_bindname;
  XmRowColumnCallbackStruct* cd;

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

    case XmCR_ACTIVATE:		/* XmRowColumn(:XMN_ENTRY_CALLBACK) */
      cd = (XmRowColumnCallbackStruct*) call_data;
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
	else if (s_bindname == s_CALLBACK_ENTRY_WIDGET) {
	  xlpbind(s_bindname, Wcls_WidgetID_To_WIDGETOBJ(cd->widget), lexical_env);
	}
	else if (s_bindname == s_CALLBACK_ENTRY_REASON) {
	  xlpbind(s_bindname, (cd->callbackstruct) ? Wcb_Get_Callback_Reason_Symbol(((XmToggleButtonCallbackStruct*) cd->callbackstruct)->reason) : NIL, lexical_env);
	}
	else if (s_bindname == s_CALLBACK_ENTRY_SET) { /* this'll give meaningless results if entry isn't a togglebutton  */
	  xlpbind(s_bindname, (cd->callbackstruct) ? ((((XmToggleButtonCallbackStruct*) cd->callbackstruct)->set) ? true : NIL) : NIL, lexical_env);
	}
#ifdef WINTERP_MOTIF_11
	else if (s_bindname == s_CALLBACK_ENTRY_CLICK_COUNT) { /* this'll give meaningless results if entry isn't a pushbutton, drawnbutton, arrowbutton  */
	  xlpbind(s_bindname, (cd->callbackstruct) ? cvfixnum((FIXTYPE) ((XmPushButtonCallbackStruct*) cd->callbackstruct)->click_count) : NIL, lexical_env);
	}
#endif /* WINTERP_MOTIF_11 */
	/* ----- WARNING: Using this could cause coredump!! -- this should dereference to the CALLBACKOBJ set for the entry, else NIL ---- */
	else if (s_bindname == s_CALLBACK_ENTRY_DATA) { 
	  xlpbind(s_bindname, (cd->data) ? (LVAL) cd->data : NIL, lexical_env);
	}
	/* ------------------------------------------------------------------------------------------------------------------------------- */
	else {
	  errputstr("Warning: in widget callback, ignored unknown binding name - ");
	  errprint(s_bindname);
	  sprintf(temptext,
#ifdef WINTERP_MOTIF_11
		  "\tFor :XMN_ENTRY_CALLBACK, valid symbols for XmRowColumnCallbackStruct are [%s %s %s %s %s %s %s %s] - ",
#else  /* Motif 1.0 */
		  "\tFor :XMN_ENTRY_CALLBACK, valid symbols for XmRowColumnCallbackStruct are [%s %s %s %s %s %s %s] - ",
#endif /* WINTERP_MOTIF_11 */
		  getstring(getpname(s_CALLBACK_WIDGET)),
		  getstring(getpname(s_CALLBACK_REASON)),
		  getstring(getpname(s_CALLBACK_XEVENT)),
		  getstring(getpname(s_CALLBACK_ENTRY_WIDGET)),
		  getstring(getpname(s_CALLBACK_ENTRY_REASON)),
		  getstring(getpname(s_CALLBACK_ENTRY_SET)),
#ifdef WINTERP_MOTIF_11
		  getstring(getpname(s_CALLBACK_ENTRY_CLICK_COUNT)),
#endif /* WINTERP_MOTIF_11 */
		  getstring(getpname(s_CALLBACK_ENTRY_DATA)));
	  errputstr(temptext);
	  errprint(get_callback_widget(client_data));
	}
      }
      break;

    case XmCR_MAP:		/* XmRowColumn(:XMN_MAP_CALLBACK) */
    case XmCR_UNMAP:		/* XmRowColumn(:XMN_UNMAP_CALLBACK) */
#ifdef WINTERP_MOTIF_12
    case XmCR_TEAR_OFF_ACTIVATE: /* XmRowColumn(:XMN_TEAR_OFF_MENU_ACTIVATE_CALLBACK) */
    case XmCR_TEAR_OFF_DEACTIVATE: /* XmRowColumn(:XMN_TEAR_OFF_MENU_DEACTIVATE_CALLBACK) */
#endif /* WINTERP_MOTIF_12 */
      cd = (XmRowColumnCallbackStruct*) call_data;
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
	else {
	  errputstr("Warning: in widget callback, ignored unknown binding name - ");
	  errprint(s_bindname);
	  sprintf(temptext,
		  "\tValid symbols for XmRowColumnCallbackStruct are [%s %s %s] - ",
		  getstring(getpname(s_CALLBACK_WIDGET)),
		  getstring(getpname(s_CALLBACK_REASON)),
		  getstring(getpname(s_CALLBACK_XEVENT)));
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
 * an XmRowColumnCallbackStruct as call_data.
 ******************************************************************************/
static void XmRowColumnCallbackStruct_Callbackproc(widget, client_data, call_data)
     Widget    widget;
     XtPointer client_data;
     XtPointer call_data;
{
  Wcb_Meta_Callbackproc(get_callback_closure((LVAL) client_data),
			(LVAL) client_data,
			call_data,
			Lexical_Bindings_For_XmRowColumnCallbackStruct,
			NULL,
			Wcb_Callback_Trace_Proc);
}


/******************************************************************************
 * Same as WIDGET_CLASS's :add_callback method except that this understands
 * how to get values from the XmRowColumnCallbackStruct.
 * Specifying one or more of the following symbols in the callback bindings 
 * list will bind that symbol's value in the lexical environment of the callback:
 * CALLBACK_WIDGET
 * CALLBACK_REASON
 * CALLBACK_XEVENT
 * CALLBACK_ENTRY_WIDGET
 * CALLBACK_ENTRY_REASON
 * CALLBACK_ENTRY_SET
 * CALLBACK_ENTRY_DATA
 * CALLBACK_ENTRY_CLICK_COUNT -- new resource for Motif 1.1
 ******************************************************************************/
LVAL Xm_Row_Column_Widget_Class_Method_ADD_CALLBACK()
{
  return (Wcb_Meta_Method_Add_Callback(XmRowColumnCallbackStruct_Callbackproc, FALSE));
}


/******************************************************************************
 * Same as WIDGET_CLASS's :set_callback method except that this understands
 * how to get values from the XmRowColumnCallbackStruct.
 * Specifying one or more of the following symbols in the callback bindings 
 * list will bind that symbol's value in the lexical environment of the callback:
 * CALLBACK_WIDGET
 * CALLBACK_REASON
 * CALLBACK_XEVENT
 * CALLBACK_ENTRY_WIDGET
 * CALLBACK_ENTRY_REASON
 * CALLBACK_ENTRY_SET
 * CALLBACK_ENTRY_DATA
 * CALLBACK_ENTRY_CLICK_COUNT -- new resource for Motif 1.1
 ******************************************************************************/
LVAL Xm_Row_Column_Widget_Class_Method_SET_CALLBACK()
{
  return (Wcb_Meta_Method_Add_Callback(XmRowColumnCallbackStruct_Callbackproc, TRUE));
}


/******************************************************************************
 * (send <popup_menu> :MENU_POSITION <event>)
 *      ==> returns <popup_menu>
 * This method will position a popup menupane at the position specified by
 * event->x_root, event->y_root, where <event> is an XEvent. This
 * method should only be called on a row_column widget that was created via
 * (send XM_ROW_COLUMN_WIDGET_CLASS :new :popup_menu ...).
 *
 * Possible design change: should I make a POPUP_MENU_WIDGET_CLASS that
 * has this as one of it's methods, and don't provide this method on
 * other rowcolumn widgets?
 *
 * void XmMenuPosition (p, event)
 *     Widget  p;
 *     XButtonPressedEvent *event;
 ******************************************************************************/
LVAL Xm_Row_Column_Widget_Class_Method_MENU_POSITION()
{
  LVAL self;
  Widget widget_id;
  XEvent *event;

  widget_id = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&self);
  event = get_xevent(xlga_xevent());
  xllastarg();
  
  XmMenuPosition(widget_id, (XButtonPressedEvent*) event);
  
  return (self);
}


/******************************************************************************
 * (send <option_menu> :OPTION_LABEL_GADGET)
 *      ==> this method returns the WIDGETOBJ corresponding to the label
 *          gadget created in an <option_menu> instance. An <option_menu>
 *          rowcolumn instance can be created by doing
 *          (send XM_ROW_COLUMN_WIDGET_CLASS :new :option_menu ...)
 *
 * Widget XmOptionLabelGadget (m)
 *     Widget   m;
 ******************************************************************************/
LVAL Xm_Row_Column_Widget_Class_Method_OPTION_LABEL_GADGET()
{
  LVAL self;
  Widget widget_id;

  widget_id = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&self);
  xllastarg();
  
  return (Wcls_WidgetID_To_WIDGETOBJ(XmOptionLabelGadget(widget_id)));
}


/******************************************************************************
 * (send <option_menu> :OPTION_BUTTON_GADGET)
 *      ==> this method returns the WIDGETOBJ corresponding to the cascade
 *          button gadget created in an <option_menu> instance. An
 *          <option_menu> rowcolumn instance can be created by doing
 *          (send XM_ROW_COLUMN_WIDGET_CLASS :new :option_menu ...)
 *
 * Widget XmOptionButtonGadget (m)
 *     Widget   m;
 ******************************************************************************/
LVAL Xm_Row_Column_Widget_Class_Method_OPTION_BUTTON_GADGET()
{
  LVAL self;
  Widget widget_id;

  widget_id = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&self);
  xllastarg();

  return (Wcls_WidgetID_To_WIDGETOBJ(XmOptionButtonGadget(widget_id)));
}


#ifdef WINTERP_MOTIF_11
/******************************************************************************
 * (send <menu> :GET_POSTED_FROM_WIDGET)
 *      ==> this method returns the WIDGETOBJ corresponding to the the widget
 * from which the menu was posted. in a popup, it is the widget which initiated
 * the post. If it is in a pulldown from a menubar or option menu, then the
 * returned widget is the menubar or option menu.
 *
 * ----------------------------------------------------------------------------
 *  Widget XmGetPostedFromWidget (Widget menu);
 ******************************************************************************/
LVAL Xm_Row_Column_Widget_Class_Method_GET_POSTED_FROM_WIDGET()
{
  LVAL self;
  Widget widget_id;

  widget_id = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&self);
  xllastarg();
  
  return (Wcls_WidgetID_To_WIDGETOBJ(XmGetPostedFromWidget(widget_id)));
}
#endif				/* WINTERP_MOTIF_11 */


#ifdef WINTERP_MOTIF_11
/******************************************************************************
 * (send <rowcolwidget> :GET_SUB_MENU_WIDGET)
 *      ==> this method returns the WIDGETOBJ corresponding to the the widget
 * returned by resource XmNsubMenuId.
 *
 * Due to some weird 1.1 bug, (perhaps in motif) doing
 * (send <rowcolwidget> :get_values :xmn_sub_menu_id nil) returns
 * <rowcolwidget>. This seems to be failing in the same way that :PARENT
 * is failing on scrolled text and list widgets. See comments in 
 * wc_Text.c:Xm_Text_Widget_Class_Method_PARENT() and
 * wc_List.c:Xm_List_Widget_Class_Method_PARENT()
 * for details.
 * ----------------------------------------------------------------------------
 ******************************************************************************/
LVAL Xm_Row_Column_Widget_Class_Method_GET_SUB_MENU_WIDGET()
{
  LVAL self, lval_submenuid;
  Widget widget_id, submenu_id;
  XtPointer widget_data;

  widget_id = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&self);
  xllastarg();

  widget_data = Wres_GetValues_Token_Value();
  ARGLIST_RESET(); ARGLIST_ADD(XmNsubMenuId, &widget_data);
  XtGetValues(widget_id, ARGLIST());
  if (Wres_GetValues_Failed(widget_data)) { /* make sure that XmNsubMenuId resource actually exists on the widget in question. */
    widget_data = Wres_GetValues_Alternate_Token_Value();
    XtGetValues(widget_id, ARGLIST());
    if (Wres_GetValues_Alternate_Failed(widget_data))
      xlfail("Internal error in Xm_Row_Column_Widget_Class_Method_GET_SUB_MENU_WIDET() -- couldn't retrieve XmNsubMenuId resource from widget. Motif bug?");
  }
  if (widget_data == NULL)
    return (NIL);

  submenu_id = (Widget) widget_data;

  lval_submenuid = Wcls_WidgetID_To_WIDGETOBJ(submenu_id);
  
  if (lval_submenuid != self)
    return (lval_submenuid);
  else {			/* handle motif 1.1 fuckup */
    lval_submenuid = Wcls_WidgetID_To_Generic_WIDGETOBJ(submenu_id);

    /* Store the pointer to the new widgetobj in the widget's XmNuserData resource. */
    ARGLIST_RESET(); ARGLIST_ADD(XmNuserData, (XtArgVal) lval_submenuid);
    XtSetValues(submenu_id, ARGLIST());  
    return (lval_submenuid);
  }
}
#endif				/* WINTERP_MOTIF_11 */


/******************************************************************************
 *
 ******************************************************************************/
Wc_RowColumn_Init()
{
  LVAL o_XM_ROW_COLUMN_WIDGET_CLASS;

#ifdef WINTERP_MOTIF_11
  k_WORK_AREA		= xlenter(":WORK_AREA");
  k_SIMPLE_RADIO_BOX	= xlenter(":SIMPLE_RADIO_BOX");
  k_SIMPLE_OPTION_MENU	= xlenter(":SIMPLE_OPTION_MENU");
  k_SIMPLE_MENU_BAR	= xlenter(":SIMPLE_MENU_BAR");
  k_SIMPLE_POPUP_MENU	= xlenter(":SIMPLE_POPUP_MENU");
  k_SIMPLE_PULLDOWN_MENU = xlenter(":SIMPLE_PULLDOWN_MENU");
  k_SIMPLE_CHECK_BOX	= xlenter(":SIMPLE_CHECK_BOX");
#endif				/* WINTERP_MOTIF_11 */
  k_RADIO_BOX          = xlenter(":RADIO_BOX");
  k_OPTION_MENU        = xlenter(":OPTION_MENU");
  k_MENU_BAR           = xlenter(":MENU_BAR");
  k_POPUP_MENU         = xlenter(":POPUP_MENU");
  k_PULLDOWN_MENU      = xlenter(":PULLDOWN_MENU");

  s_CALLBACK_ENTRY_WIDGET = xlenter("CALLBACK_ENTRY_WIDGET");
  s_CALLBACK_ENTRY_REASON = xlenter("CALLBACK_ENTRY_REASON");
  s_CALLBACK_ENTRY_SET    = xlenter("CALLBACK_ENTRY_SET");
  s_CALLBACK_ENTRY_DATA   = xlenter("CALLBACK_ENTRY_DATA");
#ifdef WINTERP_MOTIF_11
  s_CALLBACK_ENTRY_CLICK_COUNT = xlenter("CALLBACK_ENTRY_CLICK_COUNT");
#endif				/* WINTERP_MOTIF_11 */

  o_XM_ROW_COLUMN_WIDGET_CLASS =
    Wcls_Create_Subclass_Of_WIDGET_CLASS("XM_ROW_COLUMN_WIDGET_CLASS",
					 xmRowColumnWidgetClass);

  /*
   * a special :isnew method on this class allows for the creation of this
   * widget inside a popup shell if one of the following submessage keywords
   * are given: 
   * :RADIO_BOX, :OPTION_MENU, :MENU_BAR, :POPUP_MENU, :PULLDOWN_MENU, :WORK_AREA
   * :SIMPLE_RADIO_BOX, :SIMPLE_OPTION_MENU, :SIMPLE_MENU_BAR, :SIMPLE_POPUP_MENU,
   * :SIMPLE_PULLDOWN_MENU, :SIMPLE_CHECK_BOX
   */
  xladdmsg(o_XM_ROW_COLUMN_WIDGET_CLASS, ":ISNEW", 
	   FTAB_Xm_Row_Column_Widget_Class_Method_ISNEW);

  xladdmsg(o_XM_ROW_COLUMN_WIDGET_CLASS, ":ADD_CALLBACK",
           FTAB_Xm_Row_Column_Widget_Class_Method_ADD_CALLBACK);

  xladdmsg(o_XM_ROW_COLUMN_WIDGET_CLASS, ":SET_CALLBACK",
           FTAB_Xm_Row_Column_Widget_Class_Method_SET_CALLBACK);

  xladdmsg(o_XM_ROW_COLUMN_WIDGET_CLASS, ":MENU_POSITION",
           FTAB_Xm_Row_Column_Widget_Class_Method_MENU_POSITION);

  xladdmsg(o_XM_ROW_COLUMN_WIDGET_CLASS, ":OPTION_LABEL_GADGET",
           FTAB_Xm_Row_Column_Widget_Class_Method_OPTION_LABEL_GADGET);

  xladdmsg(o_XM_ROW_COLUMN_WIDGET_CLASS, ":OPTION_BUTTON_GADGET",
           FTAB_Xm_Row_Column_Widget_Class_Method_OPTION_BUTTON_GADGET);

#ifdef WINTERP_MOTIF_11
  xladdmsg(o_XM_ROW_COLUMN_WIDGET_CLASS, ":GET_POSTED_FROM_WIDGET",
           FTAB_Xm_Row_Column_Widget_Class_Method_GET_POSTED_FROM_WIDGET);
#endif				/* WINTERP_MOTIF_11 */

#ifdef WINTERP_MOTIF_11
  xladdmsg(o_XM_ROW_COLUMN_WIDGET_CLASS, ":GET_SUB_MENU_WIDGET",
           FTAB_Xm_Row_Column_Widget_Class_Method_GET_SUB_MENU_WIDGET);
#endif				/* WINTERP_MOTIF_11 */
}

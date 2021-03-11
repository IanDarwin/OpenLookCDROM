/* -*-C-*-
********************************************************************************
*
* File:         wc_DropPockt.c
* RCS:          $Header: /users/npm/src/winterp/src-server/RCS/wc_DropPockt.c,v 2.1 1994/06/06 15:40:34 npm Exp $
* Description:  SG_DROP_POCKET_WIDGET_CLASS and SG_FINDER_WIDGET_CLASS
*		(These widgets are only available on SGI Irix 5.X).
*		The SgDropPocket(3x) provides a receptacle for doing drag/drop
*		of desktop items from IndigoMagic desktop into WINTERP. The
*		SgFinder(3x) widget provides a drop-site, a text-field,
*		and a dynamic-menu for selecting previously entered items.
*		To get these widgets into WINTERP, you must compile with
*		SGI_DROP_POCKET_WIDGET defined.	See Makefile.irix5
*		 "WANT_DROP_POCKET_WIDGET" for details.
* Author:       Niels Mayer
* Created:      January 1994
* Modified:     Sun Jun  5 15:54:33 1994 (Niels Mayer) npm@indeed
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
static char rcs_identity[] = "@(#)$Header: /users/npm/src/winterp/src-server/RCS/wc_DropPockt.c,v 2.1 1994/06/06 15:40:34 npm Exp $";

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
#include <Sgm/DropPocket.h>
#include <Sgm/Finder.h>
#include "winterp.h"
#include "w_funtab.h"


/******************************************************************************
 * typedef struct
 * {
 *     int     reason;
 *     XEvent  *event;
 *     Window  window;
 *     XmString iconName;
 *     char *  iconData;
 * } SgDropPocketCallbackStruct;
 ******************************************************************************/
static LVAL s_CALLBACK_ICON_NAME, s_CALLBACK_ICON_DATA;
static void Lexical_Bindings_For_SgDropPocketCallbackStruct(bindings_list,
							    lexical_env,
							    call_data,
							    client_data)
     LVAL bindings_list;	/* a list of symbols to which values from SgDropPocketCallbackStruct are bound */
     LVAL lexical_env;		
     XtPointer call_data;
     LVAL client_data;		/* XLTYPE_CALLBACKOBJ */
{
  register LVAL s_bindname;
  SgDropPocketCallbackStruct* cd;

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
    case SgCR_ICON_CHANGE:	/* SgDropPocket(:SGN_ICON_UPDATE_CALLBACK) */
      cd = (SgDropPocketCallbackStruct*) call_data;
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
	else if (s_bindname == s_CALLBACK_WINDOW) {
	  xlpbind(s_bindname, (cd->window) ? cv_window(cd->window) : NIL, lexical_env);
	}
	else if (s_bindname == s_CALLBACK_ICON_NAME) {
	  xlpbind(s_bindname, (cd->iconName) ? cv_xmstring(XmStringCopy(cd->iconName)) : NIL, lexical_env);
	}
	else if (s_bindname == s_CALLBACK_ICON_DATA) {
	  xlpbind(s_bindname, (cd->iconData) ? cvstring(cd->iconData) : NIL, lexical_env);
	}
	else {
	  errputstr("Warning: in widget callback, ignored unknown binding name - ");
	  errprint(s_bindname);
	  sprintf(temptext,
		  "\tValid symbols for SgDropPocketCallbackStruct are [%s %s %s %s %s %s] - ",
		  getstring(getpname(s_CALLBACK_WIDGET)),
		  getstring(getpname(s_CALLBACK_REASON)),
		  getstring(getpname(s_CALLBACK_XEVENT)),
		  getstring(getpname(s_CALLBACK_WINDOW)),
		  getstring(getpname(s_CALLBACK_ICON_NAME)),
		  getstring(getpname(s_CALLBACK_ICON_DATA)));
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
 * an SgDropPocketCallbackStruct as call_data.
 ******************************************************************************/
static void SgDropPocketCallbackStruct_Callbackproc(widget, client_data, call_data)
     Widget    widget;
     XtPointer client_data;
     XtPointer call_data;
{
  Wcb_Meta_Callbackproc(get_callback_closure((LVAL) client_data),
			(LVAL) client_data,
			call_data,
			Lexical_Bindings_For_SgDropPocketCallbackStruct,
			NULL,
			Wcb_Callback_Trace_Proc);
}

/******************************************************************************
 * Same as WIDGET_CLASS's :add_callback method except that this understands
 * how to get values from the SgDropPocketCallbackStruct.
 * Specifying one or more of the following symbols in the callback bindings 
 * list will bind that symbol's value in the lexical environment of the callback:
 * CALLBACK_WIDGET
 * CALLBACK_REASON
 * CALLBACK_XEVENT
 * CALLBACK_WINDOW
 * CALLBACK_ICON_NAME
 * CALLBACK_ICON_DATA
 ******************************************************************************/
LVAL Sg_Drop_Pocket_Widget_Class_Method_ADD_CALLBACK()
{
  return (Wcb_Meta_Method_Add_Callback(SgDropPocketCallbackStruct_Callbackproc, FALSE));
}

/******************************************************************************
 * Same as WIDGET_CLASS's :set_callback method except that this understands
 * how to get values from the SgDropPocketCallbackStruct.
 * Specifying one or more of the following symbols in the callback bindings 
 * list will bind that symbol's value in the lexical environment of the callback:
 * CALLBACK_WIDGET
 * CALLBACK_REASON
 * CALLBACK_XEVENT
 * CALLBACK_WINDOW
 * CALLBACK_ICON_NAME
 * CALLBACK_ICON_DATA
 ******************************************************************************/
LVAL Sg_Drop_Pocket_Widget_Class_Method_SET_CALLBACK()
{
  return (Wcb_Meta_Method_Add_Callback(SgDropPocketCallbackStruct_Callbackproc, TRUE));
}

/*****************************************************************************
 * (send <finder_widget> :ADD_HISTORY_ITEM <string>)
 *	==> returns <finder_widget>
 *
 * Method :ADD_HISTORY_ITEM adds STRING item <string> to the Finder history
 * list at the top position.  When the item is inserted into the list,
 * it is compared with the current items. If the new item matches an item on
 * the selected Finder, the item is removed from the old position, so it is
 * not duplicated in the history list.
 * --------------------------------------------------------------------------
 * void SgFinderAddHistoryItem(Widget w, char* str);
 ****************************************************************************/
LVAL Sg_Finder_Widget_Class_Method_ADD_HISTORY_ITEM()
{
  LVAL self;
  char* string;
  Widget widget_id;

  widget_id = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&self);
  string = getstring(xlgastring());
  xllastarg();

  SgFinderAddHistoryItem(widget_id, string);
  return (self);
}


/*****************************************************************************
 * (send <finder_widget> :CLEAR_HISTORY)
 * 
 * Method :CLEAR_HISTORY deletes all items from the Finder's history list.
 * --------------------------------------------------------------------------
 * void SgFinderClearHistory( Widget w );
 ****************************************************************************/
LVAL Sg_Finder_Widget_Class_Method_CLEAR_HISTORY()
{
  LVAL self;
  Widget widget_id;

  widget_id = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&self);
  xllastarg();

  SgFinderClearHistory(widget_id);
  return (self);
}

/*****************************************************************************
 * (send <finder_widget> :GET_STRING)
 *	==> returns a STRING
 * 
 * Method :GET_STRING accesses the string value of the Finder widget.
 * --------------------------------------------------------------------------
 * char* SgFinderGetTextString( Widget w );
 ****************************************************************************/
LVAL Sg_Finder_Widget_Class_Method_GET_STRING()
{
  LVAL self, s_result;
  char* string;
  Widget widget_id;

  widget_id = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&self);
  xllastarg();

  if (string = SgFinderGetTextString(widget_id))
    s_result = cv_string(string); /* SgFinderGetTextString() result is not copied, will be freed by XLISP-GC */
  else 
    s_result = cvstring("");	/* don't call cv_string on this because GC would end up doing XtFree("")... */

  return (s_result);
}


/*****************************************************************************
 * (send <finder_widget> :SET_STRING <string>)
 *	==> returns <finder_widget>
 *
 * Method :SET_STRING sets the string value of the Finder widget.
 * It also calls the widget's :XMN_VALUE_CHANGED_CALLBACK and
 * :XMN_ACTIVATE_CALLBACK.
 * --------------------------------------------------------------------------
 * void SgFinderSetTextString(Widget w, char* value);
 ****************************************************************************/
LVAL Sg_Finder_Widget_Class_Method_SET_STRING()
{
  LVAL self;
  char* string;
  Widget widget_id;

  widget_id = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&self);
  string = getstring(xlgastring());
  xllastarg();

  SgFinderSetTextString(widget_id, string);
  return (self);
}


/******************************************************************************
 * (send <finder_widget> :GET_CHILD <child_sym>)
 *	==> returns a WIDGETOBJ corresponding to a SgFinder child.
 *
 * <child_sym> can be one of the following keyword symbols:
 *           :DROP_POCKET, :TEXT, :ZOOM_BAR, or :HISTORY_MENUBAR.
 *
 * Method :GET_CHILD is used to access a widget component within a Finder.
 * The parameters given to the function are the Finder widget and a value
 * indicating which component to access.
 *
 * The Finder widget generally provides the functionality and access to
 * child behavior through other means.  Accessing the children of the
 * Finder and modifying their callbacks or resources significantly may
 * cause unpredictable results.  It is suggested that applications not
 * modify the state of the Finder children.
 * ----------------------------------------------------------------------------
 * Widget SgFinderGetChild(Widget w, int child);
 ******************************************************************************/
static LVAL s_SgFINDER_TEXT, s_SgFINDER_ZOOM_BAR, s_SgFINDER_DROP_POCKET, s_SgFINDER_HISTORY_MENUBAR;
LVAL Sg_Finder_Widget_Class_Method_GET_CHILD()
{
  LVAL self, lval_child;
  Widget widget_id;
  int child;

  widget_id = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&self);
  lval_child = xlgasymbol();
  xllastarg();
  
  if (lval_child == s_SgFINDER_TEXT)
    child = SgFINDER_TEXT;
  else if (lval_child == s_SgFINDER_ZOOM_BAR)
    child = SgFINDER_ZOOM_BAR;
  else if (lval_child == s_SgFINDER_DROP_POCKET)
    child = SgFINDER_DROP_POCKET;
  else if (lval_child == s_SgFINDER_HISTORY_MENUBAR)
    child = SgFINDER_HISTORY_MENUBAR;
  else 
    xlerror("SG_FINDER_WIDGET_CLASS method :GET_CHILD -- unknown child type.", lval_child);

  return (Wcls_WidgetID_To_WIDGETOBJ(SgFinderGetChild(widget_id, child)));
}


/******************************************************************************
 *
 ******************************************************************************/
Wc_DropPocket_Init()
{
  LVAL o_SG_DROP_POCKET_WIDGET_CLASS;
  LVAL o_SG_FINDER_WIDGET_CLASS;

  /* SgDropPocket widget */
  o_SG_DROP_POCKET_WIDGET_CLASS =
    Wcls_Create_Subclass_Of_WIDGET_CLASS("SG_DROP_POCKET_WIDGET_CLASS",
					 sgDropPocketWidgetClass);

  xladdmsg(o_SG_DROP_POCKET_WIDGET_CLASS, ":ADD_CALLBACK",
           FTAB_Sg_Drop_Pocket_Widget_Class_Method_ADD_CALLBACK);

  xladdmsg(o_SG_DROP_POCKET_WIDGET_CLASS, ":SET_CALLBACK",
           FTAB_Sg_Drop_Pocket_Widget_Class_Method_SET_CALLBACK);

  s_CALLBACK_ICON_NAME	= xlenter("CALLBACK_ICON_NAME");
  s_CALLBACK_ICON_DATA  = xlenter("CALLBACK_ICON_DATA");

  /* SgFinder widget */
  o_SG_FINDER_WIDGET_CLASS =
    Wcls_Create_Subclass_Of_WIDGET_CLASS("SG_FINDER_WIDGET_CLASS",
					 sgFinderWidgetClass);

  xladdmsg(o_SG_FINDER_WIDGET_CLASS, ":ADD_HISTORY_ITEM",
           FTAB_Sg_Finder_Widget_Class_Method_ADD_HISTORY_ITEM);

  xladdmsg(o_SG_FINDER_WIDGET_CLASS, ":CLEAR_HISTORY",
           FTAB_Sg_Finder_Widget_Class_Method_CLEAR_HISTORY);

  xladdmsg(o_SG_FINDER_WIDGET_CLASS, ":SET_STRING",
           FTAB_Sg_Finder_Widget_Class_Method_SET_STRING);

  xladdmsg(o_SG_FINDER_WIDGET_CLASS, ":GET_STRING",
           FTAB_Sg_Finder_Widget_Class_Method_GET_STRING);

  xladdmsg(o_SG_FINDER_WIDGET_CLASS, ":GET_CHILD",
           FTAB_Sg_Finder_Widget_Class_Method_GET_CHILD);

  s_SgFINDER_TEXT	     = xlenter(":TEXT");
  s_SgFINDER_ZOOM_BAR	     = xlenter(":ZOOM_BAR");
  s_SgFINDER_DROP_POCKET     = xlenter(":DROP_POCKET");
  s_SgFINDER_HISTORY_MENUBAR = xlenter(":HISTORY_MENUBAR");
}

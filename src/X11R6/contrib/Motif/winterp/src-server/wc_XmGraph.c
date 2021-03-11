/* -*-C-*-
********************************************************************************
*
* File:         wc_XmGraph.c
* RCS:          $Header: /users/npm/src/winterp/src-server/RCS/wc_XmGraph.c,v 2.7 1994/06/06 15:40:33 npm Exp $
* Description:  Interface to HP's "Graph Widget" which is a contributed Motif
*               widget, publicly available on the Interworks (HP Users Group) 
*		CD-ROM distribution of HP software. See widgets/XmGraph.README.
* Author:       Niels Mayer & Audrey Ishizaki (Hewlett-Packard Laboratories).
* Created:      Sun Feb 18 19:32:38 1990
* Modified:     Sun Jun  5 15:57:57 1994 (Niels Mayer) npm@indeed
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
static char rcs_identity[] = "@(#)$Header: /users/npm/src/winterp/src-server/RCS/wc_XmGraph.c,v 2.7 1994/06/06 15:40:33 npm Exp $";

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
#include "widgets/Graph.h"
#include "widgets/Arc.h"
#include "winterp.h"
#include "w_funtab.h"

extern LVAL xnotimp();		/* stub out unimplemented functions in this file -- note "FIX:" */
extern LVAL true;

/******************************************************************************
 ******************************************************************************/
static LVAL Wcxmg_WidgetList_To_Lisp_Vector(wlist, size)
     WidgetList wlist;
     int        size;
{
  LVAL result;
  int i;
  
  if (!wlist || !size)
    return (NIL);

  xlsave1(result);
  result = newvector((unsigned) size);
  for (i = 0; i < size; i++)
    setelement(result, i, Wcls_WidgetID_To_WIDGETOBJ((wlist[i])));
  xlpop();
  return (result);
}


/*****************************************************************************
 * (send XM_GRAPH_WIDGET_CLASS :new
 *                           [:managed/:unmanaged]
 *                           [:scrolled]
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
 *     (send XM_GRAPH_WIDGET_CLASS :new ...)
 *     --> XmCreateGraph();
 *     (send XM_GRAPH_WIDGET_CLASS :new :managed ...)
 *     --> XmCreateManagedGraph();
 *     (send XM_GRAPH_WIDGET_CLASS :new :scrolled ...)
 *     --> XmCreateScrolledGraph();
 *        Note: extra convenience fn XmCreateScrolledGraph() puts the
 *	  graph widget inside a scrolled window but returns the graph widget.
 ****************************************************************************/
LVAL Xm_Graph_Widget_Class_Method_ISNEW()
{
  LVAL self, o_parent;
  char* name;
  Boolean managed_p, scrolled_p;
  Widget parent_widget_id, widget_id;

  self = Wcls_Get_OBJECT_Arg_Returning_WIDGETOBJ(); /* NOTE: xlobj.c:clnew() returns an OBJECT; 
						       get the arg and mark it's type slot as a WIDGETOBJ */

  /* get optional managed/unmanaged arg */
  if (moreargs() && ((*xlargv == k_managed) || (*xlargv == k_unmanaged)))
    managed_p = (nextarg() == k_managed);
  else
    managed_p = FALSE;		/* by default don't call XtManageChild() */

  /* get optional :scrolled arg */
  if (moreargs() && (*xlargv == k_scrolled)) {
    nextarg();
    scrolled_p = TRUE;
  }
  else
    scrolled_p = FALSE;		/* by default, we don't want a scroled graphwidget */

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
    if (scrolled_p)
      widget_id = XmCreateScrolledGraph(parent_widget_id, name, xt_arglist, xt_numargs);
    else
      widget_id = XmCreateGraph(parent_widget_id, name, xt_arglist, xt_numargs);
    Wres_Free_C_Arglist_Data();
  }
  else 
    if (scrolled_p)
      widget_id = XmCreateScrolledGraph(parent_widget_id, name, ARGLIST());
    else
      widget_id = XmCreateGraph(parent_widget_id, name, ARGLIST());

  Wcls_Initialize_WIDGETOBJ(self, widget_id);

  if (managed_p)
    XtManageChild(widget_id);

#ifdef WINTERP_DEBUG_1
  Wcls_Print_WidgetObj_Info(self);
#endif /* WINTERP_DEBUG_1 */
  return (self);
}


/*****************************************************************************
 * (send XM_ARC_WIDGET_CLASS :new
 *                           [:managed/:unmanaged]
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
 *     (send XM_ARC_WIDGET_CLASS :new ...)
 *     --> XmCreateArc();
 *     (send XM_GRAPH_WIDGET_CLASS :new :managed ...)
 *     --> XmCreateArc();
 *
 * Note: the arc widget is kind of weird... it should only be created by the
 * "convenience function" XmCreateArc().
 ****************************************************************************/
LVAL Xm_Arc_Widget_Class_Method_ISNEW()
{
  LVAL self, o_parent;
  char* name;
  Boolean managed_p;
  Widget parent_widget_id, widget_id;

  self = Wcls_Get_OBJECT_Arg_Returning_WIDGETOBJ(); /* NOTE: xlobj.c:clnew() returns an OBJECT; 
						       get the arg and mark it's type slot as a WIDGETOBJ */

  /* get optional managed/unmanaged arg */
  if (moreargs() && ((*xlargv == k_managed) || (*xlargv == k_unmanaged)))
    managed_p = (nextarg() == k_managed);
  else
    managed_p = FALSE;		/* by default don't call XtManageChild() */

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
    widget_id = XmCreateArc(parent_widget_id, name, xt_arglist, xt_numargs);
    Wres_Free_C_Arglist_Data();
  }
  else 
    widget_id = XmCreateArc(parent_widget_id, name, ARGLIST());

  Wcls_Initialize_WIDGETOBJ(self, widget_id);

  if (managed_p)
    XtManageChild(widget_id);

#ifdef WINTERP_DEBUG_1
  Wcls_Print_WidgetObj_Info(self);
#endif /* WINTERP_DEBUG_1 */
  return (self);
}


/******************************************************************************
 * typedef struct {
 *     int            reason;
 *     XEvent        *event;
 *     Boolean        interactive;
 *     WidgetList     selected_widgets;
 *     int            num_selected_widgets;
 *     WidgetList     selected_arcs;
 *     int            num_selected_arcs;
 *     Widget         widget;
 *     Widget         old_to;
 *     Widget         old_from;
 *     Widget         new_to;
 *     Widget         new_from;
 *     Boolean        doit;
 * } XmGraphCallbackStruct;
 *
 * NPM: note, I ignore the "interactive" field since it is useless...
 * NPM: note also that this structure is used for XmArc callbacks...
 *******************************************************************************/
static LVAL s_CALLBACK_GRAPH_ELT,
  s_CALLBACK_SELECTED_WIDGETS, s_CALLBACK_NUM_SELECTED_WIDGETS,
  s_CALLBACK_SELECTED_ARCS, s_CALLBACK_NUM_SELECTED_ARCS,
  s_CALLBACK_OLD_TO, s_CALLBACK_OLD_FROM,
  s_CALLBACK_NEW_TO, s_CALLBACK_NEW_FROM;
static void Lexical_Bindings_For_XmGraphCallbackStruct(bindings_list,
						       lexical_env,
						       call_data,
						       client_data)
     LVAL bindings_list;	/* a list of symbols to which values from XmGraphCallbackStruct are bound */
     LVAL lexical_env;		
     XtPointer call_data;
     LVAL client_data;		/* XLTYPE_CALLBACKOBJ */
{
  register LVAL s_bindname;
  XmGraphCallbackStruct* cd;

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
      /* case XmCR_SUBGRAPH_MOVED: -- not used */
      /* case XmCR_DESELECT_ARC:   -- not used */
      /* case XmCR_DESELECT_NODE:  -- not used */
      /* case XmCR_DELETE_NODE:    -- not used */
      /* case XmCR_DELETE_ARC:     -- not used */
      /* case XmCR_SELECT:         -- not used */

    case XmCR_NEW_ARC:		/* XmGraph(:XMN_NEW_ARC_CALLBACK) */
      /* VALID FIELDS: cb.event cb.reason cb.widget 
	 cb.new_from cb.new_to
	 cb.selected_widgets cb.num_selected_widgets
	 cb.selected_arcs cb.num_selected_arcs
	 cb.doit -- (code looks at cd.doit after the callback...) */
      cd = (XmGraphCallbackStruct*) call_data;
      for ( ; consp(bindings_list); bindings_list = cdr(bindings_list)) {
	s_bindname = car(bindings_list);
	if (s_bindname == s_CALLBACK_WIDGET) {
	  xlpbind(s_bindname, get_callback_widget(client_data), lexical_env); /* add binding to lexical_env */
	}
	else if (s_bindname == s_CALLBACK_REASON) {
	  xlpbind(s_bindname, Wcb_Get_Callback_Reason_Symbol(cd->reason), lexical_env);
	}
	else if (s_bindname == s_CALLBACK_XEVENT) {
	  xlpbind(s_bindname, (cd->event) ? cv_xevent(cd->event) : NIL, lexical_env);
	}
	else if (s_bindname == s_CALLBACK_GRAPH_ELT) {
	  xlpbind(s_bindname, Wcls_WidgetID_To_WIDGETOBJ(cd->widget), lexical_env);
	}
	else if (s_bindname == s_CALLBACK_SELECTED_WIDGETS) {
	  xlpbind(s_bindname, Wcxmg_WidgetList_To_Lisp_Vector(cd->selected_widgets, cd->num_selected_widgets), lexical_env);
	}
	else if (s_bindname == s_CALLBACK_NUM_SELECTED_WIDGETS) {
	  xlpbind(s_bindname, (cd->selected_widgets) ? cvfixnum((FIXTYPE) cd->num_selected_widgets) : NIL, lexical_env);
	}
	else if (s_bindname == s_CALLBACK_SELECTED_ARCS) {
	  xlpbind(s_bindname, Wcxmg_WidgetList_To_Lisp_Vector(cd->selected_arcs, cd->num_selected_arcs), lexical_env);
	}
	else if (s_bindname == s_CALLBACK_NUM_SELECTED_ARCS) {
	  xlpbind(s_bindname, (cd->selected_arcs) ? cvfixnum((FIXTYPE) cd->num_selected_arcs) : NIL, lexical_env);
	}
	else if (s_bindname == s_CALLBACK_NEW_TO) {
	  xlpbind(s_bindname, Wcls_WidgetID_To_WIDGETOBJ(cd->new_to), lexical_env);
	}
	else if (s_bindname == s_CALLBACK_NEW_FROM) {
	  xlpbind(s_bindname, Wcls_WidgetID_To_WIDGETOBJ(cd->new_from), lexical_env);
	}
	else if (s_bindname == s_CALLBACK_DOIT) {
	  xlpbind(s_bindname, true, lexical_env); /* this value is to be set within the callback code, we just make space for it, bind to T */
	}
	else {
	  errputstr("Warning: in widget callback, ignored unknown binding name - ");
	  errprint(s_bindname);
	  sprintf(temptext,
		  "\tFor :XMN_NEW_ARC_CALLBACK, valid symbols for XmGraphCallbackStruct are [%s %s %s %s %s %s %s %s %s %s %s] - ",
		  getstring(getpname(s_CALLBACK_WIDGET)),
		  getstring(getpname(s_CALLBACK_REASON)),
		  getstring(getpname(s_CALLBACK_XEVENT)),
		  getstring(getpname(s_CALLBACK_GRAPH_ELT)),
		  getstring(getpname(s_CALLBACK_SELECTED_WIDGETS)),
		  getstring(getpname(s_CALLBACK_NUM_SELECTED_WIDGETS)),
		  getstring(getpname(s_CALLBACK_SELECTED_ARCS)),
		  getstring(getpname(s_CALLBACK_NUM_SELECTED_ARCS)),
		  getstring(getpname(s_CALLBACK_NEW_TO)),
		  getstring(getpname(s_CALLBACK_NEW_FROM)),
		  getstring(getpname(s_CALLBACK_DOIT)));
	  errputstr(temptext);
	  errprint(get_callback_widget(client_data));
	}
      }
      break;

      /*==============================================================================*/

    case XmCR_NEW_NODE:		/* XmGraph(:XMN_NEW_NODE_CALLBACK) */
      /* VALID FIELDS: cb.event cb.reason cb.widget
	 cb.selected_widgets cb.num_selected_widgets
	 cb.selected_arcs cb.num_selected_arcs
	 cb.doit -- (code looks at cd.doit after the callback...) */
      cd = (XmGraphCallbackStruct*) call_data;
      for ( ; consp(bindings_list); bindings_list = cdr(bindings_list)) {
	s_bindname = car(bindings_list);
	if (s_bindname == s_CALLBACK_WIDGET) {
	  xlpbind(s_bindname, get_callback_widget(client_data), lexical_env); /* add binding to lexical_env */
	}
	else if (s_bindname == s_CALLBACK_REASON) {
	  xlpbind(s_bindname, Wcb_Get_Callback_Reason_Symbol(cd->reason), lexical_env);
	}
	else if (s_bindname == s_CALLBACK_XEVENT) {
	  xlpbind(s_bindname, (cd->event) ? cv_xevent(cd->event) : NIL, lexical_env);
	}
	else if (s_bindname == s_CALLBACK_GRAPH_ELT) {
	  xlpbind(s_bindname, Wcls_WidgetID_To_WIDGETOBJ(cd->widget), lexical_env);
	}
	else if (s_bindname == s_CALLBACK_SELECTED_WIDGETS) {
	  xlpbind(s_bindname, Wcxmg_WidgetList_To_Lisp_Vector(cd->selected_widgets, cd->num_selected_widgets), lexical_env);
	}
	else if (s_bindname == s_CALLBACK_NUM_SELECTED_WIDGETS) {
	  xlpbind(s_bindname, (cd->selected_widgets) ? cvfixnum((FIXTYPE) cd->num_selected_widgets) : NIL, lexical_env);
	}
	else if (s_bindname == s_CALLBACK_SELECTED_ARCS) {
	  xlpbind(s_bindname, Wcxmg_WidgetList_To_Lisp_Vector(cd->selected_arcs, cd->num_selected_arcs), lexical_env);
	}
	else if (s_bindname == s_CALLBACK_NUM_SELECTED_ARCS) {
	  xlpbind(s_bindname, (cd->selected_arcs) ? cvfixnum((FIXTYPE) cd->num_selected_arcs) : NIL, lexical_env);
	}
	else if (s_bindname == s_CALLBACK_DOIT) {
	  xlpbind(s_bindname, true, lexical_env); /* this value is to be set within the callback code, we just make space for it, bind to T */
	}
	else {
	  errputstr("Warning: in widget callback, ignored unknown binding name - ");
	  errprint(s_bindname);
	  sprintf(temptext,
		  "\tFor :XMN_NEW_NODE_CALLBACK, valid symbols for XmGraphCallbackStruct are [%s %s %s %s %s %s %s %s %s] - ",
		  getstring(getpname(s_CALLBACK_WIDGET)),
		  getstring(getpname(s_CALLBACK_REASON)),
		  getstring(getpname(s_CALLBACK_XEVENT)),
		  getstring(getpname(s_CALLBACK_GRAPH_ELT)),
		  getstring(getpname(s_CALLBACK_SELECTED_WIDGETS)),
		  getstring(getpname(s_CALLBACK_NUM_SELECTED_WIDGETS)),
		  getstring(getpname(s_CALLBACK_SELECTED_ARCS)),
		  getstring(getpname(s_CALLBACK_NUM_SELECTED_ARCS)),
		  getstring(getpname(s_CALLBACK_DOIT)));
	  errputstr(temptext);
	  errprint(get_callback_widget(client_data));
	}
      }
      break;

      /*==============================================================================*/

    case XmCR_ARC_MOVED:	/* XmGraph(:XMN_ARC_MOVED_CALLBACK) */
    case XmCR_ARC_EDITED:	/* XmArc(:XMN_ARC_EDITED_CALLBACK) */
      /* VALID FIELDS: cb.event cb.reason cb.widget 
	 cb.old_from cb.old_to cb.new_from cb.new_to
	 cb.selected_widgets cb.num_selected_widgets cb.selected_arcs cb.num_selected_arcs
	 cb.doit  -- (code looks at cd.doit after the callback...) */
      cd = (XmGraphCallbackStruct*) call_data;
      for ( ; consp(bindings_list); bindings_list = cdr(bindings_list)) {
	s_bindname = car(bindings_list);
	if (s_bindname == s_CALLBACK_WIDGET) {
	  xlpbind(s_bindname, get_callback_widget(client_data), lexical_env); /* add binding to lexical_env */
	}
	else if (s_bindname == s_CALLBACK_REASON) {
	  xlpbind(s_bindname, Wcb_Get_Callback_Reason_Symbol(cd->reason), lexical_env);
	}
	else if (s_bindname == s_CALLBACK_XEVENT) {
	  xlpbind(s_bindname, (cd->event) ? cv_xevent(cd->event) : NIL, lexical_env);
	}
	else if (s_bindname == s_CALLBACK_GRAPH_ELT) {
	  xlpbind(s_bindname, Wcls_WidgetID_To_WIDGETOBJ(cd->widget), lexical_env);
	}
	else if (s_bindname == s_CALLBACK_SELECTED_WIDGETS) {
	  xlpbind(s_bindname, Wcxmg_WidgetList_To_Lisp_Vector(cd->selected_widgets, cd->num_selected_widgets), lexical_env);
	}
	else if (s_bindname == s_CALLBACK_NUM_SELECTED_WIDGETS) {
	  xlpbind(s_bindname, (cd->selected_widgets) ? cvfixnum((FIXTYPE) cd->num_selected_widgets) : NIL, lexical_env);
	}
	else if (s_bindname == s_CALLBACK_SELECTED_ARCS) {
	  xlpbind(s_bindname, Wcxmg_WidgetList_To_Lisp_Vector(cd->selected_arcs, cd->num_selected_arcs), lexical_env);
	}
	else if (s_bindname == s_CALLBACK_NUM_SELECTED_ARCS) {
	  xlpbind(s_bindname, (cd->selected_arcs) ? cvfixnum((FIXTYPE) cd->num_selected_arcs) : NIL, lexical_env);
	}
	else if (s_bindname == s_CALLBACK_OLD_TO) {
	  xlpbind(s_bindname, Wcls_WidgetID_To_WIDGETOBJ(cd->old_to), lexical_env);
	}
	else if (s_bindname == s_CALLBACK_OLD_FROM) {
	  xlpbind(s_bindname, Wcls_WidgetID_To_WIDGETOBJ(cd->old_from), lexical_env);
	}
	else if (s_bindname == s_CALLBACK_NEW_TO) {
	  xlpbind(s_bindname, Wcls_WidgetID_To_WIDGETOBJ(cd->new_to), lexical_env);
	}
	else if (s_bindname == s_CALLBACK_NEW_FROM) {
	  xlpbind(s_bindname, Wcls_WidgetID_To_WIDGETOBJ(cd->new_from), lexical_env);
	}
	else if (s_bindname == s_CALLBACK_DOIT) {
	  xlpbind(s_bindname, true, lexical_env); /* this value is to be set within the callback code, we just make space for it, bind to T */
	}
	else {
	  errputstr("Warning: in widget callback, ignored unknown binding name - ");
	  errprint(s_bindname);
	  sprintf(temptext,
		  "\tFor :XMN_ARC_MOVED_CALLBACK/:XMN_ARC_EDITED_CALLBACK, valid symbols for XmGraphCallbackStruct are [%s %s %s %s %s %s %s %s %s %s %s %s %s] - ",
		  getstring(getpname(s_CALLBACK_WIDGET)),
		  getstring(getpname(s_CALLBACK_REASON)),
		  getstring(getpname(s_CALLBACK_XEVENT)),
		  getstring(getpname(s_CALLBACK_GRAPH_ELT)),
		  getstring(getpname(s_CALLBACK_SELECTED_WIDGETS)),
		  getstring(getpname(s_CALLBACK_NUM_SELECTED_WIDGETS)),
		  getstring(getpname(s_CALLBACK_SELECTED_ARCS)),
		  getstring(getpname(s_CALLBACK_NUM_SELECTED_ARCS)),
		  getstring(getpname(s_CALLBACK_OLD_TO)),
		  getstring(getpname(s_CALLBACK_OLD_FROM)),
		  getstring(getpname(s_CALLBACK_NEW_TO)),
		  getstring(getpname(s_CALLBACK_NEW_FROM)),
		  getstring(getpname(s_CALLBACK_DOIT)));
	  errputstr(temptext);
	  errprint(get_callback_widget(client_data));
	}
      }
      break;

      /*==============================================================================*/

    case XmCR_NODE_MOVED:	/* XmGraph(:XMN_NODE_MOVED_CALLBACK) */ 
    case XmCR_SELECT_NODE:	/* XmGraph(:XMN_SELECT_NODE_CALLBACK) */
    case XmCR_SELECT_ARC:	/* XmGraph(:XMN_SELECT_ARC_CALLBACK) */
    case XmCR_SELECT_SUBGRAPH:	/* XmGraph(:XMN_SELECT_SUBGRAPH_CALLBACK) */
    case XmCR_RELEASE:		/* XmGraph(:XMN_DEFAULT_ACTION_CALLBACK) */
    case XmCR_NODE_DOUBLE_CLICK: /* XmGraph(:XMN_DEFAULT_ACTION_CALLBACK) */
    case XmCR_ARC_DOUBLE_CLICK:	/* XmGraph(:XMN_DEFAULT_ACTION_CALLBACK) */
    case XmCR_DOUBLE_CLICK:	/* XmGraph(:XMN_DEFAULT_ACTION_CALLBACK) */
    case XmCR_DESELECT:		/* XmGraph(:XMN_DESELECT_CALLBACK) */
    case XmCR_NODES_MOVED:	/* XmGraph(:XMN_NODE_MOVED_CALLBACK) */
    case XmCR_SELECT_NODES:	/* XmGraph(:XMN_SELECT_NODE_CALLBACK) */
    case XmCR_SELECT_ARCS:	/* XmGraph(:XMN_SELECT_ARC_CALLBACK) */
    case XmCR_SELECT_ARCS_AND_NODES: /* XmGraph(:XMN_SELECT_NODE_CALLBACK, XmGraph(:XMN_SELECT_ARC_CALLBACK) */
      /* VALID FIELDS: cb.event cb.reason cb.widget
	 cb.selected_widgets cb.num_selected_widgets
	 cb.selected_arcs cb.num_selected_arcs */

      cd = (XmGraphCallbackStruct*) call_data;
      for ( ; consp(bindings_list); bindings_list = cdr(bindings_list)) {
	s_bindname = car(bindings_list);
	if (s_bindname == s_CALLBACK_WIDGET) {
	  xlpbind(s_bindname, get_callback_widget(client_data), lexical_env); /* add binding to lexical_env */
	}
	else if (s_bindname == s_CALLBACK_REASON) {
	  xlpbind(s_bindname, Wcb_Get_Callback_Reason_Symbol(cd->reason), lexical_env);
	}
	else if (s_bindname == s_CALLBACK_XEVENT) {
	  xlpbind(s_bindname, (cd->event) ? cv_xevent(cd->event) : NIL, lexical_env);
	}
	else if (s_bindname == s_CALLBACK_GRAPH_ELT) {
	  xlpbind(s_bindname, Wcls_WidgetID_To_WIDGETOBJ(cd->widget), lexical_env);
	}
	else if (s_bindname == s_CALLBACK_SELECTED_WIDGETS) {
	  xlpbind(s_bindname, Wcxmg_WidgetList_To_Lisp_Vector(cd->selected_widgets, cd->num_selected_widgets), lexical_env);
	}
	else if (s_bindname == s_CALLBACK_NUM_SELECTED_WIDGETS) {
	  xlpbind(s_bindname, (cd->selected_widgets) ? cvfixnum((FIXTYPE) cd->num_selected_widgets) : NIL, lexical_env);
	}
	else if (s_bindname == s_CALLBACK_SELECTED_ARCS) {
	  xlpbind(s_bindname, Wcxmg_WidgetList_To_Lisp_Vector(cd->selected_arcs, cd->num_selected_arcs), lexical_env);
	}
	else if (s_bindname == s_CALLBACK_NUM_SELECTED_ARCS) {
	  xlpbind(s_bindname, (cd->selected_arcs) ? cvfixnum((FIXTYPE) cd->num_selected_arcs) : NIL, lexical_env);
	}
	else {
	  errputstr("Warning: in widget callback, ignored unknown binding name - ");
	  errprint(s_bindname);
	  sprintf(temptext,
		  "\tValid symbols for XmGraphCallbackStruct are [%s %s %s %s %s %s %s %s] - ",
		  getstring(getpname(s_CALLBACK_WIDGET)),
		  getstring(getpname(s_CALLBACK_REASON)),
		  getstring(getpname(s_CALLBACK_XEVENT)),
		  getstring(getpname(s_CALLBACK_GRAPH_ELT)),
		  getstring(getpname(s_CALLBACK_SELECTED_WIDGETS)),
		  getstring(getpname(s_CALLBACK_NUM_SELECTED_WIDGETS)),
		  getstring(getpname(s_CALLBACK_SELECTED_ARCS)),
		  getstring(getpname(s_CALLBACK_NUM_SELECTED_ARCS)));
	  errputstr(temptext);
	  errprint(get_callback_widget(client_data));
	}
      }
      break;

      /*==============================================================================*/

    case XmCR_ARM:		/* XmArc(:XMN_ARM_CALLBACK) */ 
    case XmCR_ACTIVATE:		/* XmArc(:XMN_ACTIVATE_CALLBACK) */
    case XmCR_DISARM:		/* XmArc(:XMN_DISARM_CALLBACK) */
      /* VALID FIELDS: cb.event cb.reason cb.widget */

      cd = (XmGraphCallbackStruct*) call_data;
      for ( ; consp(bindings_list); bindings_list = cdr(bindings_list)) {
	s_bindname = car(bindings_list);
	if (s_bindname == s_CALLBACK_WIDGET) {
	  xlpbind(s_bindname, get_callback_widget(client_data), lexical_env); /* add binding to lexical_env */
	}
	else if (s_bindname == s_CALLBACK_REASON) {
	  xlpbind(s_bindname, Wcb_Get_Callback_Reason_Symbol(cd->reason), lexical_env);
	}
	else if (s_bindname == s_CALLBACK_XEVENT) {
	  xlpbind(s_bindname, (cd->event) ? cv_xevent(cd->event) : NIL, lexical_env);
	}
	else if (s_bindname == s_CALLBACK_GRAPH_ELT) {
	  xlpbind(s_bindname, Wcls_WidgetID_To_WIDGETOBJ(cd->widget), lexical_env);
	}
	else {
	  errputstr("Warning: in widget callback, ignored unknown binding name - ");
	  errprint(s_bindname);
	  sprintf(temptext,
		  "\tFor XmArc callbacks, valid symbols for XmGraphCallbackStruct are [%s %s %s %s] - ",
		  getstring(getpname(s_CALLBACK_WIDGET)),
		  getstring(getpname(s_CALLBACK_REASON)),
		  getstring(getpname(s_CALLBACK_XEVENT)),
		  getstring(getpname(s_CALLBACK_GRAPH_ELT)));
	  errputstr(temptext);
	  errprint(get_callback_widget(client_data));
	}
      }
      break;

      /*==============================================================================*/

    default:			/* for XmManager(:XMN_HELP_CALLBACK) */
      Lexical_Bindings_For_XmAnyCallbackStruct(bindings_list, lexical_env, call_data, client_data);
      break;
    }
}


/******************************************************************************
 * Read the value of the 'doit' field after certain XmGraph callbacks fire.
 ******************************************************************************/
static void Set_Call_Data_For_XmGraphCallbackStruct(lexical_env, call_data)
     LVAL lexical_env;		
     XtPointer call_data;
{
  register LVAL ep;

  /* Note: Wcb_Meta_Callbackproc() won't call this proc if call_data==NULL */

  switch (((XmAnyCallbackStruct*) call_data)->reason) {
  case XmCR_NEW_ARC:		/* XmGraph(:XMN_NEW_ARC_CALLBACK) */
  case XmCR_NEW_NODE:		/* XmGraph(:XMN_NEW_NODE_CALLBACK) */
  case XmCR_ARC_MOVED:		/* XmGraph(:XMN_ARC_MOVED_CALLBACK) */
  case XmCR_ARC_EDITED:		/* XmArc(:XMN_ARC_EDITED_CALLBACK) */
    for (ep = car(lexical_env); /* get current environment stack frame which was created in Wcb_Meta_Callbackproc() by Lexical_Bindings_For_XmGraphCallbackStruct(). */
	 (ep != NIL); ep = cdr(ep)) /* while there are more bindings in current environment */
      if (s_CALLBACK_DOIT == car(car(ep))) { /* check to see if this symbol was bound in the envt */
	((XmGraphCallbackStruct*) call_data)->doit = ((cdr(car(ep)) != NIL) ? TRUE : FALSE); /* set doit field if value is non-NIL */
	return;
      }
    break;
  default:			/* do nothing for most cases... */
    break;
  }
}


/******************************************************************************
 * This is called indirectly via XtAddCallback() for callbacks returning an
 * XmGraphCallbackStruct as call_data.
 ******************************************************************************/
static void XmGraphCallbackStruct_Callbackproc(widget, client_data, call_data)
     Widget    widget;
     XtPointer client_data;
     XtPointer call_data;
{
  Wcb_Meta_Callbackproc(get_callback_closure((LVAL) client_data),
			(LVAL) client_data,
			call_data,
			Lexical_Bindings_For_XmGraphCallbackStruct,
			Set_Call_Data_For_XmGraphCallbackStruct,
			Wcb_Callback_Trace_Proc);
}


/*
 * The Graph widget receives the values from XmGraphCallbackStruct:
 * CALLBACK_REASON
 * CALLBACK_EVENT
 * CALLBACK_SELECTED_WIDGETS (list)
 * CALLBACK_NUM_SELECTED_WIDGETS
 * CALLBACK_SELECTED_ARCS (list)
 * CALLBACK_NUM_SELECTED_ARCS
 * CALLBACK_WIDGET
 * CALLBACK_OLD_TO
 * CALLBACK_OLD_FROM
 * CALLBACK_NEW_TO
 * CALLBACK_NEW_FROM
 * CALLBACK_DOIT
 */
LVAL Xm_Graph_Widget_Class_Method_ADD_CALLBACK()
{
  return (Wcb_Meta_Method_Add_Callback(XmGraphCallbackStruct_Callbackproc, FALSE));
}

LVAL Xm_Graph_Widget_Class_Method_SET_CALLBACK()
{
  return (Wcb_Meta_Method_Add_Callback(XmGraphCallbackStruct_Callbackproc, TRUE));
}


/*******************************************************************************
 * (send graph_widget :center_around_widget widget)
 *******************************************************************************/
LVAL Xm_Graph_Widget_Class_Method_CENTER_AROUND_WIDGET ()
{
  LVAL self, w;
  Widget graph, node;

  graph = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&self);
  node  = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&w);
  xllastarg();

  XmGraphCenterAroundWidget(graph, node);

  return (self);			/* NPM */
}
/*
 *
 */
LVAL Xm_Graph_Widget_Class_Method_DESTROY_ALL_ARCS()
{
  LVAL self;
  Widget graph;

  graph = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&self);
  xllastarg();

  XmGraphDestroyAllArcs(graph);

  return (self);			/* NPM */
}

/*
 *
 */
LVAL Xm_Graph_Widget_Class_Method_DESTROY_ALL_NODES()
{
  LVAL self;
  Widget graph;

  graph = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&self);
  xllastarg();

  XmGraphDestroyAllNodes(graph);

  return (self);			/* NPM */
}
/*
 *
 */
LVAL Xm_Graph_Widget_Class_Method_DESTROY_SELECTED_ARCS_OR_NODES()
{
  LVAL self;
  Widget graph;

  graph = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&self);
  xllastarg();

  XmGraphDestroySelectedArcsOrNodes(graph);

  return (self);			/* NPM */
}
/*
 *
 */
LVAL Xm_Graph_Widget_Class_Method_GET_ARCS()
{
  LVAL self;
  Widget graph;
  WidgetList alist;
  int num_arcs;
  
  graph = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&self);
  xllastarg();

  alist = XmGraphGetArcs(graph, &num_arcs);
  return (Wcxmg_WidgetList_To_Lisp_Vector(alist, num_arcs));
}
/*
 *
 */
LVAL Xm_Graph_Widget_Class_Method_GET_NODES()
{
  LVAL self;
  Widget graph;
  WidgetList nlist;
  int n_nodes;
  
  graph = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&self);
  xllastarg();

  nlist = XmGraphGetNodes (graph, &n_nodes);
  return (Wcxmg_WidgetList_To_Lisp_Vector(nlist, n_nodes));
}
/*
 *
 */
LVAL Xm_Graph_Widget_Class_Method_GET_ARCS_BETWEEN_NODES()
{
  LVAL self, f, t;
  Widget graph, from, to;
  WidgetList alist;
  int num_arcs;
  
  graph = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&self);
  from  = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&f);
  to    = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&t);
  xllastarg();

  alist = XmGraphGetArcsBetweenNodes (graph, from, to, &num_arcs);
  return (Wcxmg_WidgetList_To_Lisp_Vector(alist, num_arcs));
}
/*
 * returns a vector of values
 */
LVAL Xm_Graph_Widget_Class_Method_GET_NODE_ARCS()
{
  LVAL self, w, result;
  Widget graph, widget;
  int n_from, n_to;
  WidgetList fromlist, tolist;
  
  graph = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&self);
  widget = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&w);
  xllastarg();

  XmGraphGetNodeArcs (graph, widget, &fromlist, &tolist, &n_from, &n_to);

  xlsave1(result);
  result = newvector((unsigned) 2);
  setelement(result, 0, Wcxmg_WidgetList_To_Lisp_Vector(fromlist, n_from));
  setelement(result, 1, Wcxmg_WidgetList_To_Lisp_Vector(tolist, n_to));
  xlpop();			/* NPM */
  return (result);
}


/*
 * returns a vector of values -- NPM left out this one.
 */
LVAL Xm_Graph_Widget_Class_Method_GET_ARC_NODES()
{
  LVAL self, w, result;
  Widget graph, arc, from, to;

  graph = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&self);
  arc   = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&w);
  xllastarg();

  XmGraphGetArcNodes (graph, arc, &from, &to);

  xlsave1(result);
  result = newvector(2);
  setelement(result, 0, Wcls_WidgetID_To_WIDGETOBJ(from));
  setelement(result, 1, Wcls_WidgetID_To_WIDGETOBJ(to));
  xlpop();

  return(result);
}

/*
 *
 */
LVAL Xm_Graph_Widget_Class_Method_GET_ROOTS()
{
  LVAL self;
  Widget graph;
  WidgetList rlist;
  int num_nodes;
  
  graph = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&self);
  xllastarg();

  rlist = XmGraphGetRoots (graph, &num_nodes);
  return (Wcxmg_WidgetList_To_Lisp_Vector(rlist, num_nodes));
}
/*
 *
 */
LVAL Xm_Graph_Widget_Class_Method_GET_SELECTED_ARCS()
{
  LVAL self;
  Widget graph;
  WidgetList alist;
  int num_arcs;
  
  graph = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&self);
  xllastarg();

  alist = XmGraphGetSelectedArcs (graph, &num_arcs);
  return (Wcxmg_WidgetList_To_Lisp_Vector(alist, num_arcs));
}
/*
 *
 */
LVAL Xm_Graph_Widget_Class_Method_GET_SELECTED_NODES()
{
  LVAL self;
  Widget graph;
  WidgetList nlist;
  int num;
  
  graph = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&self);
  xllastarg();

  nlist = XmGraphGetSelectedNodes (graph, &num);
  return (Wcxmg_WidgetList_To_Lisp_Vector(nlist, num));
}
/*
 *
 */
LVAL Xm_Graph_Widget_Class_Method_INPUT_OVER_ARC()
{
  LVAL self;
  Widget graph;
  int x_pos, y_pos;
  
  graph = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&self);
  x_pos = (int) getfixnum(xlgafixnum());
  y_pos = (int) getfixnum(xlgafixnum());
  xllastarg();

  return (Wcls_WidgetID_To_WIDGETOBJ(XmGraphInputOverArc(graph, x_pos, y_pos)));
}

/*
 *  FIX:
 */
LVAL Xm_Graph_Widget_Class_Method_INSERT_ROOTS()
{
/*
   XmGraphInsertRoots(graphW, nodes, n_nodes)
*/
  xnotimp();
}

/*
 * (send <arc-w> :IS_POINT_IN_ARC <x-pos-fixnum> <y-pos-fixnum>)
 * Note that this is actually a method on the XmArc widget class.
 * Should rename....
 */
LVAL Xm_Graph_Widget_Class_Method_IS_POINT_IN_ARC()
{
  LVAL self;
  Widget arc;
  int x, y;

  arc = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&self);
  x = (int) getfixnum(xlgafixnum());
  y = (int) getfixnum(xlgafixnum());
  xllastarg();

  return (XmGraphIsPointInArc(arc, x, y) ? true : NIL);
}
/*
 *
 */
LVAL Xm_Graph_Widget_Class_Method_IS_SELECTED_ARC()
{
  LVAL self, a;
  Widget graph;
  Widget arc;
  
  graph = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&self);
  arc = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&a);
  xllastarg();

  return (XmGraphIsSelectedArc(graph, arc)? true : NIL);
}
/*
 *
 */
LVAL Xm_Graph_Widget_Class_Method_IS_SELECTED_NODE()
{
  LVAL self, w;
  Widget graph;
  Widget node;
  
  graph = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&self);
  node = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&w);
  xllastarg();

  return (XmGraphIsSelectedNode(graph, node) ? true : NIL);
}
/*
 *
 */
LVAL Xm_Graph_Widget_Class_Method_MOVE_ARC()
{
  LVAL self, a, f, t;
  Widget graph;
  Widget arc;
  Widget from, to;
  
  graph = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&self);
  arc = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&a);
  from = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&f);
  to = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&t);
  xllastarg();

  return (XmGraphMoveArc(graph, arc, from, to) ? true : NIL);
}
/*
 *
 */
LVAL Xm_Graph_Widget_Class_Method_MOVE_NODE()
{
  LVAL self, n;
  Widget graph, node;
  Position x, y;

  graph = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&self);
  node  = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&n);
  x     = Get_Position_Argument();
  y     = Get_Position_Argument();
  xllastarg();

  return (XmGraphMoveNode(graph, node, x, y) ? true : NIL);
}
/*
 *
 */
LVAL Xm_Graph_Widget_Class_Method_NUM_ARCS()
{
  LVAL self;
  Widget graph;
  
  graph = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&self);
  xllastarg();

  return (cvfixnum((FIXTYPE) XmGraphNumArcs(graph)));
}
/*
 *
 */
LVAL Xm_Graph_Widget_Class_Method_NUM_NODES()
{
  LVAL self;
  Widget graph;
  
  graph = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&self);
  xllastarg();

  return (cvfixnum((FIXTYPE) XmGraphNumNodes(graph)));
}
/*
 *  returns a vector of two values
 */
LVAL Xm_Graph_Widget_Class_Method_NUM_NODE_ARCS()
{
  LVAL self, w, result;
  Widget graph, node;
  int n_from, n_to;

  graph = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&self);
  node = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&w);
  xllastarg();

  XmGraphNumNodeArcs (graph, node, &n_from, &n_to);

  xlsave1(result);
  result = newvector((unsigned) 2);
  setelement(result, 0, cvfixnum((FIXTYPE) n_from));
  setelement(result, 1, cvfixnum((FIXTYPE) n_to));
  xlpop();			/* NPM */

  return (result);
}
/*
 *
 */
LVAL Xm_Graph_Widget_Class_Method_NUM_ROOTS()
{
  LVAL self;
  Widget graph;
  
  graph = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&self);
  xllastarg();

  return (cvfixnum((FIXTYPE) XmGraphNumRoots(graph)));
}
/*
 *
 */
LVAL Xm_Graph_Widget_Class_Method_NUM_SELECTED_ARCS()
{
  LVAL self;
  Widget graph;
  
  graph = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&self);
  xllastarg();

  return (cvfixnum((FIXTYPE) XmGraphNumSelectedArcs(graph)));
}
/*
 *
 */
LVAL Xm_Graph_Widget_Class_Method_NUM_SELECTED_NODES()
{
  LVAL self;
  Widget graph;
  
  graph = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&self);
  xllastarg();

  return (cvfixnum((FIXTYPE) XmGraphNumSelectedNodes(graph)));
}
/*
 *
 */
LVAL Xm_Graph_Widget_Class_Method_MOVE_ALL()
{
  LVAL self;
  Widget graph;
  int delta_x, delta_y;
  
  graph = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&self);
  delta_x = (int) getfixnum(xlgafixnum());
  delta_y = (int) getfixnum(xlgafixnum());
  xllastarg();

  XmGraphMoveAll(graph, delta_x, delta_y);

  return (self);			/* NPM */
}
/*
 *
 */
LVAL Xm_Graph_Widget_Class_Method_LAYOUT()
{
  LVAL self;
  Widget graph;
  
  graph = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&self);
  xllastarg();

  XmGraphLayout(graph);

  return (self);			/* NPM */
}
/*
 *
 */
LVAL Xm_Graph_Widget_Class_Method_RELAY_SUBGRAPH()
{
  LVAL self, w;
  Widget graph;
  Widget node;

  graph = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&self);
  node = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&w);
  xllastarg();

  XmGraphRelaySubgraph(graph, node);

  return (self);			/* NPM */
}
/*
 *
 */
LVAL Xm_Graph_Widget_Class_Method_REMOVE_ARC_BETWEEN_NODES()
{
  LVAL self, w1, w2;
  Widget graph;
  Widget widget1, widget2;
  
  graph = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&self);
  widget1 = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&w1);
  widget2 = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&w2);
  xllastarg();

  XmGraphRemoveArcBetweenNodes(graph, widget1, widget2);

  return (self);			/* NPM */
}
/*
 * FIX: 
 */
LVAL Xm_Graph_Widget_Class_Method_REMOVE_ROOTS()
{
 /*
  XmGraphRemoveRoots (graph, nodes, n_nodes);
 */
  xnotimp();
}
/*
 *
 */
LVAL Xm_Graph_Widget_Class_Method_SELECT_ARC()
{
  LVAL self, a;
  Widget graph;
  Widget arc;

  graph = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&self);
  arc = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&a);
  xllastarg();

  XmGraphSelectArc (graph, arc);

  return (self);			/* NPM */
}

/*
 *  FIX: 
 */
LVAL Xm_Graph_Widget_Class_Method_SELECT_ARCS()
{
/*
  XmGraphSelectArcs (graph, arcs, n_arcs);
 */
  xnotimp();
}

/*
 *  FIX:
 */
LVAL Xm_Graph_Widget_Class_Method_SELECT_NODES()
{
/*
   XmGraphSelectNodes (graph, nodes, n_nodes)
*/
  xnotimp();
}

/*
 *
 */
LVAL Xm_Graph_Widget_Class_Method_UNSELECT_ARC()
{
  LVAL self, a;
  Widget graph;
  Widget arc;

  graph = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&self);
  arc = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&a);
  xllastarg();

  XmGraphUnselectArc (graph, arc); 

  return (self);			/* NPM */
}

/*
 *  FIX:
 */
LVAL Xm_Graph_Widget_Class_Method_UNSELECT_ARCS()
{
/*
  XmGraphUnselectArcs (graph, arcs, n_arcs);
*/
  xnotimp();
}

/*
 *  FIX:
 */
LVAL Xm_Graph_Widget_Class_Method_UNSELECT_NODES()
{
/*
  XmGraphUnselectNodes (graph, nodes, n_nodes);
*/
  xnotimp();
}

/*
 * (send <graph_widget> :select_node <node_widget>)
 */
LVAL Xm_Graph_Widget_Class_Method_SELECT_NODE ()
{
  LVAL self, node;
  Widget graph_widget;
  Widget node_widget;

  graph_widget = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&self);
  node_widget = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&node);
  xllastarg();

  XmGraphSelectNode(graph_widget, node_widget);

  return (self);			/* NPM */
}

/*
 * (send <graph_widget> :unselect_node <node_widget>)
 */
LVAL Xm_Graph_Widget_Class_Method_UNSELECT_NODE ()
{
  LVAL self, node;
  Widget graph_widget;
  Widget node_widget;
  
  graph_widget = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&self);
  node_widget = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&node);
  xllastarg();

  XmGraphUnselectNode(graph_widget, node_widget);

  return (self);			/* NPM */
}

/******************************************************************************
 *
 ******************************************************************************/
Wc_XmGraph_Init()
{
  LVAL o_XM_GRAPH_WIDGET_CLASS;
  LVAL o_XM_ARC_WIDGET_CLASS;
  
  /******************************* GRAPH WIDGET *********************************/
  o_XM_GRAPH_WIDGET_CLASS =
    Wcls_Create_Subclass_Of_WIDGET_CLASS("XM_GRAPH_WIDGET_CLASS",
					 xmGraphWidgetClass);

  /* a special :isnew method on this class allows for the creation of this
     widget inside a scrolled window if the submessage keyword :scrolled 
     is given ... */
  xladdmsg(o_XM_GRAPH_WIDGET_CLASS, ":ISNEW", 
	   FTAB_Xm_Graph_Widget_Class_Method_ISNEW);
  xladdmsg(o_XM_GRAPH_WIDGET_CLASS, ":ADD_CALLBACK", 
	   FTAB_Xm_Graph_Widget_Class_Method_ADD_CALLBACK);
  xladdmsg(o_XM_GRAPH_WIDGET_CLASS, ":SET_CALLBACK", 
	   FTAB_Xm_Graph_Widget_Class_Method_SET_CALLBACK);
  xladdmsg(o_XM_GRAPH_WIDGET_CLASS, ":CENTER_AROUND_WIDGET",
	   FTAB_Xm_Graph_Widget_Class_Method_CENTER_AROUND_WIDGET);
  xladdmsg(o_XM_GRAPH_WIDGET_CLASS, ":DESTROY_ALL_ARCS",
	   FTAB_Xm_Graph_Widget_Class_Method_DESTROY_ALL_ARCS);
  xladdmsg(o_XM_GRAPH_WIDGET_CLASS, ":DESTROY_ALL_NODES",
	   FTAB_Xm_Graph_Widget_Class_Method_DESTROY_ALL_NODES);
  xladdmsg(o_XM_GRAPH_WIDGET_CLASS, ":DESTROY_SELECTED_ARCS_OR_NODES",
	   FTAB_Xm_Graph_Widget_Class_Method_DESTROY_SELECTED_ARCS_OR_NODES);
  xladdmsg(o_XM_GRAPH_WIDGET_CLASS, ":GET_ARCS",
	   FTAB_Xm_Graph_Widget_Class_Method_GET_ARCS);
  xladdmsg(o_XM_GRAPH_WIDGET_CLASS, ":GET_NODES",
	   FTAB_Xm_Graph_Widget_Class_Method_GET_NODES);
  xladdmsg(o_XM_GRAPH_WIDGET_CLASS, ":GET_ARCS_BETWEEN_NODES",
	   FTAB_Xm_Graph_Widget_Class_Method_GET_ARCS_BETWEEN_NODES);
  xladdmsg(o_XM_GRAPH_WIDGET_CLASS, ":GET_NODE_ARCS",
	   FTAB_Xm_Graph_Widget_Class_Method_GET_NODE_ARCS);
  xladdmsg(o_XM_GRAPH_WIDGET_CLASS, ":GET_ROOTS",
	   FTAB_Xm_Graph_Widget_Class_Method_GET_ROOTS);
  xladdmsg(o_XM_GRAPH_WIDGET_CLASS, ":GET_SELECTED_ARCS",
	   FTAB_Xm_Graph_Widget_Class_Method_GET_SELECTED_ARCS);
  xladdmsg(o_XM_GRAPH_WIDGET_CLASS, ":GET_SELECTED_NODES",
	   FTAB_Xm_Graph_Widget_Class_Method_GET_SELECTED_NODES);
  xladdmsg(o_XM_GRAPH_WIDGET_CLASS, ":INPUT_OVER_ARC",
	   FTAB_Xm_Graph_Widget_Class_Method_INPUT_OVER_ARC);
  xladdmsg(o_XM_GRAPH_WIDGET_CLASS, ":INSERT_ROOTS",
	   FTAB_Xm_Graph_Widget_Class_Method_INSERT_ROOTS);
  xladdmsg(o_XM_GRAPH_WIDGET_CLASS, ":IS_SELECTED_ARC",
	   FTAB_Xm_Graph_Widget_Class_Method_IS_SELECTED_ARC);
  xladdmsg(o_XM_GRAPH_WIDGET_CLASS, ":IS_SELECTED_NODE",
	   FTAB_Xm_Graph_Widget_Class_Method_IS_SELECTED_NODE);
  xladdmsg(o_XM_GRAPH_WIDGET_CLASS, ":MOVE_ARC",
	   FTAB_Xm_Graph_Widget_Class_Method_MOVE_ARC);
  xladdmsg(o_XM_GRAPH_WIDGET_CLASS, ":MOVE_NODE",
	   FTAB_Xm_Graph_Widget_Class_Method_MOVE_NODE);
  xladdmsg(o_XM_GRAPH_WIDGET_CLASS, "NUM_ARCS:",
	   FTAB_Xm_Graph_Widget_Class_Method_NUM_ARCS);
  xladdmsg(o_XM_GRAPH_WIDGET_CLASS, ":NUM_NODES",
	   FTAB_Xm_Graph_Widget_Class_Method_NUM_NODES);
  xladdmsg(o_XM_GRAPH_WIDGET_CLASS, ":NUM_NODE_ARCS",
	   FTAB_Xm_Graph_Widget_Class_Method_NUM_NODE_ARCS);
  xladdmsg(o_XM_GRAPH_WIDGET_CLASS, ":NUM_ROOTS",
	   FTAB_Xm_Graph_Widget_Class_Method_NUM_ROOTS);
  xladdmsg(o_XM_GRAPH_WIDGET_CLASS, ":NUM_SELECTED_ARCS",
	   FTAB_Xm_Graph_Widget_Class_Method_NUM_SELECTED_ARCS);
  xladdmsg(o_XM_GRAPH_WIDGET_CLASS, ":NUM_SELECTED_NODES",
	   FTAB_Xm_Graph_Widget_Class_Method_NUM_SELECTED_NODES);
  xladdmsg(o_XM_GRAPH_WIDGET_CLASS, ":MOVE_ALL",
	   FTAB_Xm_Graph_Widget_Class_Method_MOVE_ALL);
  xladdmsg(o_XM_GRAPH_WIDGET_CLASS, ":LAYOUT",
	   FTAB_Xm_Graph_Widget_Class_Method_LAYOUT);
  xladdmsg(o_XM_GRAPH_WIDGET_CLASS, ":RELAY_SUBGRAPH",
	   FTAB_Xm_Graph_Widget_Class_Method_RELAY_SUBGRAPH);
  xladdmsg(o_XM_GRAPH_WIDGET_CLASS, ":REMOVE_ARC_BETWEEN_NODES",
	   FTAB_Xm_Graph_Widget_Class_Method_REMOVE_ARC_BETWEEN_NODES);
  xladdmsg(o_XM_GRAPH_WIDGET_CLASS, ":REMOVE_ROOTS",
	   FTAB_Xm_Graph_Widget_Class_Method_REMOVE_ROOTS);
  xladdmsg(o_XM_GRAPH_WIDGET_CLASS, ":SELECT_ARC",
	   FTAB_Xm_Graph_Widget_Class_Method_SELECT_ARC);
  xladdmsg(o_XM_GRAPH_WIDGET_CLASS, ":SELECT_ARCS",
	   FTAB_Xm_Graph_Widget_Class_Method_SELECT_ARCS);
  xladdmsg(o_XM_GRAPH_WIDGET_CLASS, ":SELECT_NODE", 
	   FTAB_Xm_Graph_Widget_Class_Method_SELECT_NODE);
  xladdmsg(o_XM_GRAPH_WIDGET_CLASS, ":SELECT_NODES", 
	   FTAB_Xm_Graph_Widget_Class_Method_SELECT_NODES);
  xladdmsg(o_XM_GRAPH_WIDGET_CLASS, ":UNSELECT_ARC",
	   FTAB_Xm_Graph_Widget_Class_Method_UNSELECT_ARC);
  xladdmsg(o_XM_GRAPH_WIDGET_CLASS, ":UNSELECT_ARCS",
	   FTAB_Xm_Graph_Widget_Class_Method_UNSELECT_ARCS);
  xladdmsg(o_XM_GRAPH_WIDGET_CLASS, ":UNSELECT_NODE", 
	   FTAB_Xm_Graph_Widget_Class_Method_UNSELECT_NODE);
  xladdmsg(o_XM_GRAPH_WIDGET_CLASS, ":UNSELECT_NODES", 
	   FTAB_Xm_Graph_Widget_Class_Method_UNSELECT_NODES);
  xladdmsg(o_XM_GRAPH_WIDGET_CLASS, ":GET_ARC_NODES",
	   FTAB_Xm_Graph_Widget_Class_Method_GET_ARC_NODES);

  /******************************** ARC WIDGET **********************************/
  o_XM_ARC_WIDGET_CLASS =
    Wcls_Create_Subclass_Of_WIDGET_CLASS("XM_ARC_WIDGET_CLASS",
					 xmArcWidgetClass);

  /* A special :isnew method on this class overrides default Xt widget creation  
     routine on meta-class WIDGET_CLASS. We need to call special creator XmCreateArc(). */
  xladdmsg(o_XM_ARC_WIDGET_CLASS, ":ISNEW", 
	   FTAB_Xm_Arc_Widget_Class_Method_ISNEW);

  /* The XmArc widget uses the same callback structure as XmGraph, so just add
     the methods Xm_Graph_Widget_Class_Method_ADD_CALLBACK() and
     Xm_Graph_Widget_Class_Method_SET_CALLBACK() to the XmArc widget class ... */
  xladdmsg(o_XM_ARC_WIDGET_CLASS, ":ADD_CALLBACK", 
	   FTAB_Xm_Graph_Widget_Class_Method_ADD_CALLBACK);
  xladdmsg(o_XM_ARC_WIDGET_CLASS, ":SET_CALLBACK", 
	   FTAB_Xm_Graph_Widget_Class_Method_SET_CALLBACK);

  xladdmsg(o_XM_ARC_WIDGET_CLASS, ":IS_POINT_IN_ARC",
	   FTAB_Xm_Graph_Widget_Class_Method_IS_POINT_IN_ARC);
    
  s_CALLBACK_GRAPH_ELT			= xlenter("CALLBACK_GRAPH_ELT");
  s_CALLBACK_SELECTED_WIDGETS		= xlenter("CALLBACK_SELECTED_WIDGETS");
  s_CALLBACK_NUM_SELECTED_WIDGETS	= xlenter("CALLBACK_NUM_SELECTED_WIDGETS");
  s_CALLBACK_SELECTED_ARCS		= xlenter("CALLBACK_SELECTED_ARCS");
  s_CALLBACK_NUM_SELECTED_ARCS		= xlenter("CALLBACK_NUM_SELECTED_ARCS");
  s_CALLBACK_OLD_TO			= xlenter("CALLBACK_OLD_TO");
  s_CALLBACK_OLD_FROM			= xlenter("CALLBACK_OLD_FROM");
  s_CALLBACK_NEW_TO			= xlenter("CALLBACK_NEW_TO");
  s_CALLBACK_NEW_FROM			= xlenter("CALLBACK_NEW_FROM");
}

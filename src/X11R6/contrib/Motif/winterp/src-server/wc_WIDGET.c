/* -*-C-*-
********************************************************************************
*
* File:         wc_WIDGET.c
* RCS:          $Header: /users/npm/src/winterp/src-server/RCS/wc_WIDGET.c,v 2.6 1994/06/06 15:40:38 npm Exp $
* Description:  WIDGET_CLASS (the widget superclass)
* Author:       Niels Mayer
* Created:      Tue Nov 21 00:55:07 1989
* Modified:     Sun Jun  5 15:11:14 1994 (Niels Mayer) npm@indeed
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
static char rcs_identity[] = "@(#)$Header: /users/npm/src/winterp/src-server/RCS/wc_WIDGET.c,v 2.6 1994/06/06 15:40:38 npm Exp $";

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
#include "winterp.h"
#include "w_funtab.h"


LVAL o_WIDGET_CLASS;		/* note -- this is used in w_classes.c */
LVAL s_WIDGET_CLASS_ID;		/* note -- this is used in w_classes.c */
LVAL k_managed;
LVAL k_unmanaged;
LVAL k_dialog;
LVAL k_scrolled;

/*
 * Note -- various methods on WIDGET_CLASS are implemented in:
 * w_libXt.c -- interfaces to Xtoolkit intrinsics.
 * w_libXm.c -- interfaces to Motif toolkit.
 * w_evnthndlr.c -- interfaces to event handling routines
 */


/******************************************************************************
 * typedef struct
 * {
 *     int     reason;
 *     XEvent  *event;
 * } XmAnyCallbackStruct;
 ******************************************************************************/
void Lexical_Bindings_For_XmAnyCallbackStruct(bindings_list,
					      lexical_env,
					      call_data,
					      client_data)
     LVAL bindings_list;	/* a list of symbols to which values from XmAnyCallbackStruct are bound */
     LVAL lexical_env;		
     XtPointer call_data;
     LVAL client_data;		/* XLTYPE_CALLBACKOBJ */
{
  register LVAL s_bindname;
  XmAnyCallbackStruct* cd;

  if (!call_data) 		/* for :XMN_DESTROY_CALLBACK */
    for ( ; consp(bindings_list); bindings_list = cdr(bindings_list)) {
      s_bindname = car(bindings_list);
      if (s_bindname == s_CALLBACK_WIDGET) { /* if missing call_data, then only value we can retrieve is the CALLBACK_WIDGET */
	xlpbind(s_bindname, get_callback_widget(client_data), lexical_env);
      }
      else {			/* attempting to retrieve any other value will cause "NULL call-data error" */
	Wcb_Callback_Missing_Call_Data_Error(client_data, s_bindname); 
      }
    }
  else {			/* other callbacks bind call_data... */
    cd = (XmAnyCallbackStruct*) call_data;
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
		"\tValid symbols for XmAnyCallbackStruct are [%s %s %s] - ",
		getstring(getpname(s_CALLBACK_WIDGET)),
		getstring(getpname(s_CALLBACK_REASON)),
		getstring(getpname(s_CALLBACK_XEVENT)));
	errputstr(temptext);
	errprint(get_callback_widget(client_data));
      }
    }
  }
}


/******************************************************************************
 * This is called indirectly via XtAddCallback() for callbacks returning
 * an XmAnyCallbackStruct as call_data.
 ******************************************************************************/
static void XmAnyCallbackStruct_Callbackproc(widget, client_data, call_data)
     Widget    widget;
     XtPointer client_data;
     XtPointer call_data;
{
  Wcb_Meta_Callbackproc(get_callback_closure((LVAL) client_data),
			(LVAL) client_data,
			call_data,
			Lexical_Bindings_For_XmAnyCallbackStruct,
			NULL,
			Wcb_Callback_Trace_Proc);
}


/*****************************************************************************
 * (send <widget_instance> :add_callback <name>
 *       <call_data_binding_names_list>
 *       <code>)
 * returns: <callback_id_object>
 *
 * <name> is a resource keyword of type XmRCallback, eg,
 * :XMN_ACTIVATE_CALLBACK, :XMN_ARM_CALLBACK, :XMN_DISARM_CALLBACK.
 *
 * <call_data_binding_name_list> is a list of symbols that get
 * bound to data specific to the action on the callback widget. For 
 * the XmAnyCallbackStruct passed as call data here, the only valid symbol
 * names are CALLBACK_WIDGET CALLBACK_REASON CALLBACK_XEVENT.
 * 
 * <code> is a list of lisp expressions that are evaluated when the callback
 * occurs. During the callback, the lexical environment that existed for
 * the call to :add_callback will be used for value and functional bindings.
 * 
 * The returned <callback_id_object> may be passed into the 
 * function (remove_callback <callback_id_object>).
 ****************************************************************************/
LVAL Widget_Class_Method_ADD_CALLBACK()
{
  return (Wcb_Meta_Method_Add_Callback(XmAnyCallbackStruct_Callbackproc, FALSE));
}


/*****************************************************************************
 * (send <widget_instance> :set_callback <name>
 *       <call_data_binding_names_list>
 *       <code>)
 * returns: <callback_id_object>
 *
 * This does exactly what method :add_callback does, except that it ensures
 * that the callback currently being set is the only callback on <widget_instance>
 * for <name>.
 *
 * This method is best used for interactively changing callbacks on a widget.
 * It is slightly less efficient than method :add_callback because it must
 * search-for and remove other callbacks on <widget> with the same <name>.
 ****************************************************************************/
LVAL Widget_Class_Method_SET_CALLBACK()
{
  return (Wcb_Meta_Method_Add_Callback(XmAnyCallbackStruct_Callbackproc, TRUE));
}


/*****************************************************************************
 * (send <Widget_Class> :new [:managed/:unmanaged]
 *                           [<name>]
 *                           <parent> 
 *                           [:XMN_<arg1> <val1>]
 *                           [. . .             ]
 *                           [:XMN_<argN> <valN>])
 *
 * For the <widget_class> meta-class (the parent class of all widget-objects), 
 * create a new widget via XtCreateWidget().
 *
 * The optional keyword-argument :managed will cause a subsequent call to
 * XtManageChild(). If the submessage :unmanaged is present, or no submessage,
 * then XtManageChild() won't be called, and the resulting widget will be
 * returned unmanaged.
 ****************************************************************************/
LVAL Widget_Class_Method_ISNEW()
{
  LVAL self, o_parent;
  char* name;
  Boolean managed_p;
  WidgetClass widgetclass_id;
  Widget widget_id, parent_widget_id;

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

  if (!(widgetclass_id = Wcls_WIDGETCLASSOBJ_To_WidgetClassID(getclass(self))))
    xlerror("Expected a 'Class' object that is a subclass of 'WIDGET_CLASS'.", self);

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
    widget_id = XtCreateWidget(name, widgetclass_id, parent_widget_id, xt_arglist, xt_numargs);
    Wres_Free_C_Arglist_Data();
  }
  else
    widget_id = XtCreateWidget(name, widgetclass_id, parent_widget_id, ARGLIST());

  Wcls_Initialize_WIDGETOBJ(self, widget_id);

  if (managed_p)
    XtManageChild(widget_id);

#ifdef WINTERP_DEBUG_1
  Wcls_Print_WidgetObj_Info(self);
#endif /* WINTERP_DEBUG_1 */
  return (self);
}


/*****************************************************************************
 * (send <Widget_Class> :SHOW)
 *		--> return <Widget_Class>
 *
 * overrides OBJECT method :SHOW (xlobj.c:obshow()). Since WIDGETOBJs contain
 * a special non-LVAL " _bogus_ivar_ " we don't want to print that, since it
 * isn't a LVAL and could result in segmentation violations.
 ****************************************************************************/
LVAL Widget_Class_Method_SHOW()
{
  return (Wcls_Generic_Hybrid_Array_Method_SHOW(o_WIDGET_CLASS));
}


#ifdef WINTERP_MOTIF_11
/******************************************************************************
 * (send <compositewidget> :GET_CHILDREN)
 * 	==> returns an array of WIDGETOBJs
 *
 * This retrieves the composite resources XmNchildren/ XmNnumChildren
 * from <compositewidget> and returns an array of WIDGETOBJ
 * representing the children of the composite widget.
 *******************************************************************************/
LVAL Widget_Class_Method_GET_CHILDREN()
{
  LVAL self, result;
  Widget widget_id;
  Widget* widgettab = NULL;
  Cardinal widgettab_size = 0;
  int i;

  widget_id = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&self);
  xllastarg();

  if (!XtIsComposite(widget_id))
    xlerror("Method :GET_CHILDREN only applies to widgets that are subclasses of \"Composite\".", self);
  
  ARGLIST_RESET();
  ARGLIST_ADD(XmNchildren, &widgettab);
  ARGLIST_ADD(XmNnumChildren, &widgettab_size);
  XtGetValues(widget_id, ARGLIST());

  if (!widgettab || !widgettab_size)
    return (NIL);

  xlsave1(result);
  result = newvector((unsigned) widgettab_size);
  for (i = 0; i < widgettab_size; i++)
    setelement(result, i, Wcls_WidgetID_To_WIDGETOBJ(widgettab[i]));
  xlpop();
  return (result);
}
#endif /* WINTERP_MOTIF_11 */


/******************************************************************************
 * (send <widget> :EXISTS_P)
 * 	==> returns T if the widget exists (hasn't been destroyed)
 *	==> returns NIL if the widget has been destroyed
 *	    or has never been initialized.
 *******************************************************************************/
LVAL Widget_Class_Method_EXISTS_P()
{
  extern LVAL true;
  LVAL self = xlga_widgetobj();
  xllastarg();

  if (get_widgetobj_widgetID(self))
    return (true);
  else
    return (NIL);
}


/******************************************************************************
 * (send <widget> :PRIN1)
 *
 * the default :PRIN1 method for WIDGETOBJs -- redefine this in WINTERP to
 * alter the format used to print WIDGETOBJs.
 *******************************************************************************/
LVAL Widget_Class_Method_PRIN1()
{
  extern LVAL s_stdout;
  LVAL self,fptr;

  /* get self and the file pointer */
  /* NPM: 4/23/94 -- this was "self = xlga_widgetobj();", however, if you get
     an error in :isnew, then ntype(self)==OBJECT, not WIDGETOBJ.
     xlga_widgetobj() specifically checks for a WIDGETOBJ, which means
     that it will get a xlbadtype() in the special case where :isnew failed.
     If any kind of traceback was set, this error will cause an infinite loop
     of errors (because it tries to print the uninitialized WIDGETOBJ in the
     traceback routines) until you get a stack overflow. */
  self = xlgaobject(); 

#ifdef BETTERIO
  fptr = (moreargs() ? xlgetfile(TRUE) : getvalue(s_stdout));
#else
  fptr = (moreargs() ? xlgetfile() : getvalue(s_stdout));
#endif
  xllastarg();

  /* print it */
  Wcls_Print_WIDGETOBJ(fptr,self);

  /* return the object */
  return (self);
}


#ifdef WINTERP_MOTIF_11
/******************************************************************************
 * (send <widget> :FORCED_EXPOSE_UPDATE)
 * 	==> returns <widget>
 *
 * This is useful in making popup dialogues visible before embarking on a long
 * computation. Although the Event loop in this method will not process timeouts
 * or events from input sources, it can and will process Lisp callbacks.
 * if an error or catch/throw causes a nonlocal exit from the callback, 
 * one may expect trouble. Be careful. 
 *
 * TODO: enhance this to defer processing any Lisp callbacks (??)
 *******************************************************************************/
LVAL Widget_Class_Method_FORCED_EXPOSE_UPDATE()
{
  LVAL self;
  Widget widget_id;

  widget_id = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&self);
  Wxt_Error_If_Gadget(widget_id, self); /* prevent coredump on gadgets */
  xllastarg();
  Wut_Wait_Till_Expose(widget_id);
  return (self);
}
#endif /* WINTERP_MOTIF_11 */


/*****************************************************************************
 * Initialize the Xtoolkit interface. Note that this procedure does
 * equivelent kinds of setup as done by xloinit(). The following call 
 * sequence happens on startup:
 * [xlsymbols(), xloinit()] <-- initwks() <-- xlinit() <-- main()...
 * However, note that main() calls xlinit() only if xlirestore("xlisp.wks") 
 * fails. In that case, we get the calling sequence:
 * obsymbols() <-- xlsymbols() <-- xlirestore() <-- main(). We could patch
 * in the symbols initialized in this module by putting initialization 
 * routines in ossymbols(), but it's not clear that this would make sense:
 * restore will only be able to restore widget objects' data, but will not
 * be able to recreate widgets themselves.
 *
 * Thus, we call this procedure after xlinit() in main() and this reinitializes
 * the widget objects independent of restoring. Of course, any widget objects
 * that were saved/restored may now not have a valid class, since the 
 * pointers may have changed ...
 ****************************************************************************/
Wc_WIDGET_Init()
{
  s_WIDGET_CLASS_ID    = xlenter("WIDGET_CLASS_ID");
  k_managed            = xlenter(":MANAGED");
  k_unmanaged          = xlenter(":UNMANAGED");
  k_dialog             = xlenter(":DIALOG"); /* used by various :isnew methods */
  k_scrolled           = xlenter(":SCROLLED"); /* used by various :isnew methods */

  /* 
   * create 'WIDGET_CLASS' by doing the (almost) equivalent of 
   * (setq Widget_Class (send Class :new 
   *                           '(widget_id)
   *                             object))
   * Creating a new 'Class' instance would end up calling clnew() to create
   * a new instance of 'Class', and then call clisnew() on that 'Class'
   * instance to set the appropriate slots for 'WIDGET_CLASS'. Thus we need to
   * create object 'WIDGET_CLASS', and with that object, set
   *    self.MESSAGES = (:SET_CALLBACK :GET_VALUES :SET_VALUES ...)
   *    self.IVARS = (WIDGET_ID SETVALUES_ALIST)
   *    self.CVARS = NIL
   *    self.CVALS = NIL
   *    self.SUPERCLASS = 'Object'
   *    self.IVARCNT = WIDGETOBJ_SIZE     ;; == length(self.IVARS)
   *    self.IVARTOTAL = WIDGETOBJ_SIZE   ;; == self.IVARCNT + length(superclass.IVARS)
   *
   * 'WIDGET_CLASS' contains methods corresponding to common operations on
   * 'Widget' in the Xtoolkit. The :isnew method in 'Widget_Class' is the
   * default means to create a new widget in this system -- this method will
   * look up the WidgetClass pointer from class(self).CVALS[0]. Note that 
   * 'WIDGET_CLASS' is a core class. Widget instances should be created
   * from classes inheriting from 'Widget_Class' The message :isnew will 
   * complain if you try to instantiate 'WIDGET_CLASS' since 
   * class(self).CVALS == NIL for that class only.
   *
   * Specific widget classes, corresponding to a instantiatable widget objects
   * in Motif, are created by doing, for example
   * (setq PUSHBUTTON_CLASS (send Class :new
   *                              '()                 ;; no new IVARS
   *                              '(WIDGET_CLASS_ID   ;; value==xmPushButtonWidgetClass
   *                                WIDGET_CLASS_SYM  ;; value==PUSHBUTTON_CLASS
   *                              Widget_Class)       ;; superclass
   * where WIDGET_CLASS_ID is associated with the WidgetClass pointer
   * designating the class of the widget for the Xtoolkit creation 
   * functions.
   */

  /*--------------- create 'Class' instance 'WIDGET_CLASS' -----------------*/
  /* Calling xlclass() creates the 'Class' instance called 'WIDGET_CLASS' and 
     sets IVARCNT, IVARTOTAL, and SUPERCLASS slots
     slots CVARS, CVALS are set to NIL */
  o_WIDGET_CLASS = xlclass("WIDGET_CLASS", WIDGETOBJ_SIZE); 

  /* set up the list of ivarnames in IVARS */
  xladdivar(o_WIDGET_CLASS, " _bogus_ivar_ "); /* this "variable name" corresponds to the WIDGETID slot on a WIDGETOBJ 
				               -- users shouldn't access this var, since it isn't an LVAL (hack). */

  /* set up the list of methods in MESSAGES, and bind the associated FSUBR */

#ifdef THE_FOLLOWING_CODE_IS_COMMENTED_OUT /* template for adding new methods */
  xladdmsg(o_WIDGET_CLASS, ":",
	   FTAB_Widget_Class_Method_); /*  */
#endif

  xladdmsg(o_WIDGET_CLASS, ":SHOW",
	   FTAB_Widget_Class_Method_SHOW); /* wc_WIDGET.c */

#ifdef WINTERP_MOTIF_12
  xladdmsg(o_WIDGET_CLASS, ":CHANGE_COLOR",
	   FTAB_Widget_Class_Method_CHANGE_COLOR); /* w_libXm.c */
#endif /* WINTERP_MOTIF_12 */

#ifdef WINTERP_MOTIF_11
  xladdmsg(o_WIDGET_CLASS, ":FORCED_EXPOSE_UPDATE",
	   FTAB_Widget_Class_Method_FORCED_EXPOSE_UPDATE); /* w_libXt.c */
#endif /* WINTERP_MOTIF_11 */

#ifdef WINTERP_MOTIF_11
  xladdmsg(o_WIDGET_CLASS, ":PROCESS_TRAVERSAL",
	   FTAB_Widget_Class_Method_PROCESS_TRAVERSAL); /* w_libXm.c */
#endif /* WINTERP_MOTIF_11 */

  xladdmsg(o_WIDGET_CLASS, ":EXISTS_P",
	   FTAB_Widget_Class_Method_EXISTS_P); /* wc_WIDGET.c */

  xladdmsg(o_WIDGET_CLASS, ":PRIN1",
	   FTAB_Widget_Class_Method_PRIN1); /* wc_WIDGET.c */

#ifdef WINTERP_MOTIF_11
  xladdmsg(o_WIDGET_CLASS, ":GET_CHILDREN",
	   FTAB_Widget_Class_Method_GET_CHILDREN); /* wc_WIDGET.c */
#endif /* WINTERP_MOTIF_11 */

  xladdmsg(o_WIDGET_CLASS, ":BUILD_EVENT_MASK",
	   FTAB_Widget_Class_Method_BUILD_EVENT_MASK); /* w_evnthndlr.c */

  xladdmsg(o_WIDGET_CLASS, ":OVERRIDE_TRANSLATIONS",
	   FTAB_Widget_Class_Method_OVERRIDE_TRANSLATIONS); /* w_txlations.c */

  xladdmsg(o_WIDGET_CLASS, ":AUGMENT_TRANSLATIONS",
	   FTAB_Widget_Class_Method_AUGMENT_TRANSLATIONS); /* w_txlations.c */

  xladdmsg(o_WIDGET_CLASS, ":UNINSTALL_TRANSLATIONS",
	   FTAB_Widget_Class_Method_UNINSTALL_TRANSLATIONS); /* w_txlations.c */

  xladdmsg(o_WIDGET_CLASS, ":INSTALL_ACCELERATORS",
	   FTAB_Widget_Class_Method_INSTALL_ACCELERATORS); /* w_txlations.c */

  xladdmsg(o_WIDGET_CLASS, ":INSTALL_ALL_ACCELERATORS",
	   FTAB_Widget_Class_Method_INSTALL_ALL_ACCELERATORS); /* w_txlations.c */

#ifdef WINTERP_MOTIF_11
  xladdmsg(o_WIDGET_CLASS, ":CALL_ACTION_PROC",
	   FTAB_Widget_Class_Method_CALL_ACTION_PROC); /* w_txlations.c */
#endif /* WINTERP_MOTIF_11 */

  xladdmsg(o_WIDGET_CLASS, ":REMOVE_ALL_CALLBACKS",
	   FTAB_Widget_Class_Method_REMOVE_ALL_CALLBACKS); /* w_callbacks.c */

  xladdmsg(o_WIDGET_CLASS, ":HAS_CALLBACKS",
	   FTAB_Widget_Class_Method_HAS_CALLBACKS); /* w_callbacks.c */

  xladdmsg(o_WIDGET_CLASS, ":ADD_TAB_GROUP",
	   FTAB_Widget_Class_Method_ADD_TAB_GROUP); /* w_libXm.c */

  xladdmsg(o_WIDGET_CLASS, ":REMOVE_TAB_GROUP",
	   FTAB_Widget_Class_Method_REMOVE_TAB_GROUP); /* w_libXm.c */

  xladdmsg(o_WIDGET_CLASS, ":UPDATE_DISPLAY", /* w_libXm.c */
	   FTAB_Widget_Class_Method_UPDATE_DISPLAY);

  xladdmsg(o_WIDGET_CLASS, ":ADD_GRAB",
	   FTAB_Widget_Class_Method_ADD_GRAB); /* w_libXt.c */

  xladdmsg(o_WIDGET_CLASS, ":REMOVE_GRAB",
	   FTAB_Widget_Class_Method_REMOVE_GRAB); /* w_libXt.c */

  xladdmsg(o_WIDGET_CLASS, ":IS_COMPOSITE",
	   FTAB_Widget_Class_Method_IS_COMPOSITE); /* w_libXt.c */

  xladdmsg(o_WIDGET_CLASS, ":IS_CONSTRAINT",
	   FTAB_Widget_Class_Method_IS_CONSTRAINT); /* w_libXt.c */

  xladdmsg(o_WIDGET_CLASS, ":IS_SHELL",
	   FTAB_Widget_Class_Method_IS_SHELL); /* w_libXt.c */

  xladdmsg(o_WIDGET_CLASS, ":IS_PRIMITIVE",
	   FTAB_Widget_Class_Method_IS_PRIMITIVE); /* w_libXm.c */

  xladdmsg(o_WIDGET_CLASS, ":IS_GADGET",
	   FTAB_Widget_Class_Method_IS_GADGET); /* w_libXm.c */

  xladdmsg(o_WIDGET_CLASS, ":IS_MANAGER",
	   FTAB_Widget_Class_Method_IS_MANAGER); /* w_libXm.c */
 
  xladdmsg(o_WIDGET_CLASS, ":SET_SENSITIVE",
	   FTAB_Widget_Class_Method_SET_SENSITIVE); /* w_libXt.c */

  xladdmsg(o_WIDGET_CLASS, ":SET_MAPPED_WHEN_MANAGED",
	   FTAB_Widget_Class_Method_SET_MAPPED_WHEN_MANAGED); /* w_libXt.c */

  xladdmsg(o_WIDGET_CLASS, ":IS_MANAGED",
	   FTAB_Widget_Class_Method_IS_MANAGED); /* w_libXt.c */

  xladdmsg(o_WIDGET_CLASS, ":IS_REALIZED",
	   FTAB_Widget_Class_Method_IS_REALIZED); /* w_libXt.c */

  xladdmsg(o_WIDGET_CLASS, ":IS_SENSITIVE",
	   FTAB_Widget_Class_Method_IS_SENSITIVE); /* w_libXt.c */

#ifdef WINTERP_MOTIF_11		/* actually, just X11r4, but Motif1.1-->X11r4 */
  xladdmsg(o_WIDGET_CLASS, ":NAME",
	   FTAB_Widget_Class_Method_NAME); /* w_libXt.c */
#endif /* WINTERP_MOTIF_11 */

  xladdmsg(o_WIDGET_CLASS, ":PARENT",
	   FTAB_Widget_Class_Method_PARENT); /* w_libXt.c */

  xladdmsg(o_WIDGET_CLASS, ":WINDOW",
	   FTAB_Widget_Class_Method_WINDOW); /* w_libXt.c */

  xladdmsg(o_WIDGET_CLASS, ":MAP",
	   FTAB_Widget_Class_Method_MAP); /* w_libXt.c */

  xladdmsg(o_WIDGET_CLASS, ":UNMAP",
	   FTAB_Widget_Class_Method_UNMAP); /* w_libXt.c */

  xladdmsg(o_WIDGET_CLASS, ":SET_EVENT_HANDLER",
	   FTAB_Widget_Class_Method_SET_EVENT_HANDLER);	/* w_evnthndlr.c */

  xladdmsg(o_WIDGET_CLASS, ":ADD_EVENT_HANDLER",
	   FTAB_Widget_Class_Method_ADD_EVENT_HANDLER);	/* w_evnthndlr.c */

  xladdmsg(o_WIDGET_CLASS, ":SET_CALLBACK",
	   FTAB_Widget_Class_Method_SET_CALLBACK);

  xladdmsg(o_WIDGET_CLASS, ":ADD_CALLBACK",
	   FTAB_Widget_Class_Method_ADD_CALLBACK);

  xladdmsg(o_WIDGET_CLASS, ":DESTROY", /* w_libXt.c */
	   FTAB_Widget_Class_Method_DESTROY);

  xladdmsg(o_WIDGET_CLASS, ":MANAGE", /* w_libXt.c */
	   FTAB_Widget_Class_Method_MANAGE);

  xladdmsg(o_WIDGET_CLASS, ":UNMANAGE",	/* w_libXt.c */
	   FTAB_Widget_Class_Method_UNMANAGE);

  xladdmsg(o_WIDGET_CLASS, ":GET_VALUES", /* w_libXt.c */
	   FTAB_Widget_Class_Method_GET_VALUES);	

  xladdmsg(o_WIDGET_CLASS, ":SET_VALUES", /* w_libXt.c */
	   FTAB_Widget_Class_Method_SET_VALUES);

  xladdmsg(o_WIDGET_CLASS, ":ISNEW",
	   FTAB_Widget_Class_Method_ISNEW);
}

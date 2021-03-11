/* -*-C-*-
********************************************************************************
*
* File:         w_classes.c
* RCS:          $Header: /users/npm/src/winterp/src-server/RCS/w_classes.c,v 2.8 1994/06/06 15:41:01 npm Exp $
* Description:  XLISP <--> Motif object and class interface.
* Author:       Niels Mayer
* Created:      Wed Jun 14 16:28:45 1989
* Modified:     Sun Jun  5 14:44:59 1994 (Niels Mayer) npm@indeed
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
static char rcs_identity[] = "@(#)$Header: /users/npm/src/winterp/src-server/RCS/w_classes.c,v 2.8 1994/06/06 15:41:01 npm Exp $";

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

#ifdef WINTERP_MOTIF_11
/*
 * For Motif 1.1, <X11/Intrinsic.h> included by <Xm/Xm.h> uses "fast subclassing"
 * to implement XtIsShell(), therefore we don't need to declare shellWidgetClass.
 */
#else				/* MOTIF 1.0 */
 extern WidgetClass shellWidgetClass; /* For Motif 1.0 this is needed by XtIsShell() macro from Intrinsics.h */
#endif				/* WINTERP_MOTIF_11 */


/******************************************************************************
 * Wcls_Widget_Destroy_Callback() -- 
 * When a widget gets destroyed, we set the WIDGETOBJ's widgetID field to
 * NIL to mark that the WIDGETOBJ's associated widget got destroyed. This
 * ensures that operations on any WIDGETOBJ's that are sitting around
 * (i.e. referenced in a user's variable) give an appropriate error.
 * Widgets can be destroyed via XtDestroyWidget (== method :DESTROY on 
 * WIDGET_CLASS). This callback will also get called on any child
 * widget of a widget that got XtDestroyWidget()'d.
 * 
 * Additionally, destroying a widget will remove the associated WIDGETOBJ
 * from v_savedobjs so that it may be garbage collected (if not referenced
 * elsewhere). This in turn will allow the garbage collection of any
 * PIXMAP_REFOBJ and CALLBACKOBJs that are referenced implicitly inside the
 * Motif toolkit implementation of widgets.
 ******************************************************************************/
void Wcls_Widget_Destroy_CallbackProc(widgetID, client_data, call_data)
     Widget    widgetID;
     XtPointer client_data;	/* really an LVAL of type XLTYPE_WIDGETOBJ */
     XtPointer call_data;
{
  LVAL o_widget;
  o_widget = (LVAL) client_data;
  set_widgetobj_widgetID(o_widget, NULL); /* mark widgetobject as invalid */

  /*
   * remove from v_savedobjs all WIDGETOBJ, CALLBACKOBJ, or PIXMAP_REFOBJ
   * corresponding to the destroyed widget ... this will allow them
   * to be garbage collected next time gc() gets called.
   */
  {
    int  i = Wso_Hash(o_widget);
    LVAL l_hbucket = getelement(v_savedobjs, i); /* a list of saved objects belonging to this hashbucket */
    LVAL obj;
    LVAL l_prev = NIL;
    while (l_hbucket != NIL)	/* while there are elements in the hashbucket */
      if (((obj = car(l_hbucket)) != NIL) /* <obj> points to cur elt which is non-NIL */
	  && ((o_widget == obj)	/* is <obj> the WIDGETOBJ being destroyed? */
	      || ((ntype(obj) == XLTYPE_CALLBACKOBJ) /* or is <obj> a CALLBACKOBJ on this widget? */
		  && (get_callback_widget(obj) == o_widget))
	      || ((ntype(obj) == XLTYPE_PIXMAP_REFOBJ) /* or is <obj> a PIXMAPREFOBJ on this widget? */
		  && (get_pixref_widget(obj) == o_widget))
	      || ((ntype(obj) == XLTYPE_EVHANDLEROBJ) /* or is <obj> a EVHANDLEROBJ on this widget? */
		  && (get_evhandler_widget(obj) == o_widget))
	      )) {
	l_hbucket = cdr(l_hbucket); /* l_hbucket now points to next elt or NIL */
	if (l_prev == NIL)
	  setelement(v_savedobjs, i, l_hbucket); /* remove first, head is now next elt */
	else
	  rplacd(l_prev, l_hbucket); /* remove cur, point previous to next */
      }
      else {			/* go to next elt in hashbucket */
	l_prev = l_hbucket;
	l_hbucket = cdr(l_hbucket);
      }
  }
}


/******************************************************************************
 * This procedure is meant to be called in an :ISNEW instance initializer
 * method for any widget instances created as subclasses of WIDGET_OBJECT.
 * xlobj.c:clnew() returns an OBJECT, which is passed to the :ISNEW method
 * as the first argument. Here, we retrieve that argument (actually any 
 * OBJECT arg) and set it to XLTYPE_WIDGETOBJ so that we can tell that this
 * is a special kind of object.
 ******************************************************************************/
LVAL Wcls_Get_OBJECT_Arg_Returning_WIDGETOBJ()
{
  LVAL self = xlgaobject();	/* get an OBJECT from arg stack */
  self->n_type = XLTYPE_WIDGETOBJ; /* OBJECT o_widget is now really a WIDGETOBJ */
  return (self);
}


/******************************************************************************
 * This procedure is meant to be called in an :ISNEW instance initializer
 * method for any widget instances created as subclasses of WIDGET_OBJECT.
 *
 * This procedure initializes a WIDGETOBJ by initializing the WidgetID slot.
 * Then it saves the  WIDGETOBJ in v_savedobjs[] so that the WIDGETOBJ
 * doesn't get garbage collected. WIDGETOBJ's may be garbage collected after
 * they are explicitly destroyed by the Xtoolkit, therefore we set up the
 * XmNdestroyCallback to destroy WIDGETOBJ and any other lisp values
 * (CALLBACKOBJs, PIXMAP_REFOBJs) implicitly referenced inside the
 * Motif toolkit implementation of widgets.
 ******************************************************************************/
void Wcls_Initialize_WIDGETOBJ(o_widget, widget_id)
     LVAL o_widget;		/* XLTYPE_WIDGETOBJ */
     Widget widget_id;
{
  set_widgetobj_widgetID(o_widget, widget_id);
  XtAddCallback(widget_id, XmNdestroyCallback, Wcls_Widget_Destroy_CallbackProc, (XtPointer) o_widget);

  /*
   * Enter the WIDGETOBJ in v_savedobjs, so that it gets mark()'d.
   * This way, it won't be garbage collected while the widget is 
   * around. Wcls_Widget_Destroy_CallbackProc() above will remove
   * the WIDGETOBJ when it's widget no  longer exists, thereby
   * allowing it and any lisp objects it references to be garbage
   * collected.
   */
  { 
    int  i = Wso_Hash(o_widget);
    LVAL l_hbucket;
    
    xlsave1(l_hbucket);
    l_hbucket = cons(o_widget, getelement(v_savedobjs, i));
    setelement(v_savedobjs, i, l_hbucket);
    xlpop();
  }
}


/******************************************************************************
 * Given a widget_id, this subroutine will return an XLISP widget-object
 * instance. 
 * This routine does the following:
 *  0) Check to see if widget_id is a shell widget. If so, then we let wc_SHELL.c
 *     lookup the shell widgetobj in it's hashtable. That's because XmNuserData
 *     is not a valid resource for shell widgets. 
 *  1) if the XmNuserData resource on the widget is non-NULL, we take the
 *     value as the pointer to the widgetOBJ. (note that this resource
 *     can be NULL (by default) for a widgetID that wasn't created via
 *     winterp.
 *  2) if XmNuserData resource is NULL, we must find the WINTERP widget-class
 *     object associated with the widget and create a new instance of that
 *     class around the widgetID, and return this widgetOBJ.
 *     To do this, we call XtClass(widgetID) to get a pointer to the 
 *     widget's class structure. During initialization of the WINTERP
 *     widget-class objects, we have set up a table of associated widgetclassID's
 *     WIDGETCLASSOBJ-ID's for use by Wcls_WidgetClassID_To_WIDGETCLASSOBJ().
 *     From that fn's result, we get the WINTERP widget-class and create a
 *     "wrapper" widget instance.
 *  3) if the widget-class object is NIL or invalid, then we create a fake
 *     generic widgetobj which is of class o_WIDGET_CLASS.
 ******************************************************************************/
LVAL Wcls_WidgetID_To_WIDGETOBJ(widget_id)
     Widget widget_id;		/* assume that widget_id is a valid Widget, not NULL */
{
  LVAL o_widget;
  XtPointer user_data;

  if (!widget_id)
    return (NIL);

  /* (0) check to see if the widget is a shell, if so, then look up widgetobj in table */
  if (XtIsShell(widget_id)) {
    return (Wshl_WidgetID_To_WIDGETOBJ(widget_id));
  }

  /* (1) try to get valid widgetobj from XmNuserData backpointer. */
  user_data = Wres_GetValues_Token_Value();
  ARGLIST_RESET(); ARGLIST_ADD(XmNuserData, &user_data);
  XtGetValues(widget_id, ARGLIST());
  if (Wres_GetValues_Failed(user_data)) { /* make sure that XmNuserData resource actually exists on the widget in question. */
    user_data = Wres_GetValues_Alternate_Token_Value();
    XtGetValues(widget_id, ARGLIST());
    if (Wres_GetValues_Alternate_Failed(user_data))
      xlfail("Internal error in Wcls_WidgetID_To_WidgetObj() -- couldn't retrieve XmNuserData resource from widget. Motif bug?");
  }

  if ((o_widget = (LVAL) user_data) != (LVAL) NULL) { /* note NULL, not NIL (which isn't == 0 if defined(NILSYMBOL)) */
    if (ntype(o_widget) == XLTYPE_WIDGETOBJ) /* quick sanity check */
      return (o_widget);
    else
      xlfail("Internal error in WidgetID_To_WidgetObj() -- someone's been messing with this widget's XmNuserData resource!");
  }
  else {
    /* (2)&(3): Its not a shell, and XmNuserData wasn't set, so it must be a child of a Motif composite widget */
    o_widget = Wcls_WidgetID_To_Generic_WIDGETOBJ(widget_id);
  
    /* Store the pointer to the new widgetobj in the widget's XmNuserData resource. */
    ARGLIST_RESET(); ARGLIST_ADD(XmNuserData, (XtArgVal) o_widget);
    XtSetValues(widget_id, ARGLIST());  
    return (o_widget);  
  }
}


/******************************************************************************
 * Given a widget_id, this subroutine will return an XLISP widget-object
 * instance. This should only be called in cases where the XtGetValues(widgetID,
 * XmNuserData) returned NULL.
 *  1) we must find the WINTERP widget-class object associated with the
 *     widget_id and create a new instance of that class around the widgetID,
 *     and return this widgetOBJ. To do this, we call XtClass(widgetID) to get
 *     a pointer to the widget's class structure. During initialization of the
 *     WINTERP widget-class objects, we have set up a table of associated
 *     widgetclassID's WIDGETCLASSOBJ-ID's for use by
 *     Wcls_WidgetClassID_To_WIDGETCLASSOBJ(). From that fn's result, we get
 *     the WINTERP widget-class and create a "wrapper" widget instance.
 *  2) if the widget-class object is NIL or invalid, then we create a fake
 *     generic widgetobj which is of class o_WIDGET_CLASS.
 ******************************************************************************/
LVAL Wcls_WidgetID_To_Generic_WIDGETOBJ(widget_id)
     Widget widget_id;		/* assume that widget_id is a valid Widget, not NULL */
{
  extern LVAL o_WIDGET_CLASS;	/* from wc_WIDGET.c */
  LVAL o_widget, o_widgetclass;

  xlsave1(o_widget);		/* protect the WIDGETOBJ we will create below against premature garbage collection */

  if ((o_widgetclass = Wcls_WidgetClassID_To_WIDGETCLASSOBJ(XtClass(widget_id))) != NIL) {
    /* Create a new WIDGETOBJ of the appropriate class. */
    o_widget = newobject(o_widgetclass, WIDGETOBJ_SIZE);
    o_widget->n_type = XLTYPE_WIDGETOBJ; /* OBJECT is now really a WIDGETOBJ */
  }
  else {
    /* fail gracefully (i think). */
    errputstr("Warning -- In Wcls_WidgetID_To_Generic_WIDGETOBJ() couldn't find a\n");
    errputstr("           valid widgetclass object inside widget classrecord....\n");
    errputstr("           Creating a \"generic\" WIDGETOBJ of class WIDGET_CLASS.\n");
    o_widget = newobject(o_WIDGET_CLASS, WIDGETOBJ_SIZE);
    o_widget->n_type = XLTYPE_WIDGETOBJ; /* OBJECT is now really a WIDGETOBJ */
  }

  Wcls_Initialize_WIDGETOBJ(o_widget, widget_id); /* note: this protexts o_widget against premature garbage colection by storing it on v_savedobjs */

  xlpop();			/* un-xlsave1() o_widget, it is already protected above in v_savedobjs */

  return (o_widget);
}


/******************************************************************************
 * Fetches and removes a WIDGETOBJ from the argument stack, returning
 * the widgetID. If the WIDGETOBJ has been destroyed or is not initialized,
 * then this will signal an error. <wobj_return> is a pointer to an LVAL,
 * it returns the WIDGETOBJ retrieved from the argument stack.
 ******************************************************************************/
Widget Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(wobj_return)
     LVAL *wobj_return;
{
  Widget widget_id;

  if (widget_id = get_widgetobj_widgetID(*wobj_return = xlga_widgetobj()))
    return (widget_id);
  else
    xlerror("Widget object has been :destroy'd or hasn't been initialized by :isnew.", *wobj_return);
}


/******************************************************************************/
/******************************************************************************/
/******************************************************************************/
/******************************************************************************/

/* instance variable numbers for the class 'Class' -- from xlobj.c */
#include "xlisp/xlobj.h"

/* class variable numbers for classes derived from 'WIDGET_CLASS' */
#define WIDGET_CLASS_ID		0 
#define DERIVED_WIDGET_CLASS_CVALS_SIZE 1

/* 
 * For looking up WIDGETCLASSOBJ<-->WidgetClass, we setup a table of
 * these pairs, which is initialized by
 * Wcls_Create_Subclass_Of_WIDGET_CLASS() and used by other proc's in this
 * module.
 */
typedef struct _WidgetClass_Pair {
  LVAL        widgetclass_OBJ;
  WidgetClass widgetclass_ID;
} WidgetClass_Pair;

#define WIDGETCLASS_TABLE_SIZE 50 /* need to increase this if we add more widgetclasses */
static WidgetClass_Pair widgetclass_table[WIDGETCLASS_TABLE_SIZE];
static int widgetclass_table_end_idx = 0; /* holds index of last element in widgetclass_table */

/*****************************************************************************
 * This procedure creates a new 'Class' instance which inherits from the
 * base class 'Widget_Class'. This derived class contains no new instance
 * variables. This procedure is equivalent to doing:
 * (set (implode <class_name>) 
 *      (send Class :new
 *                  '()                ;; no IVARS
 *                  '(WIDGET_CLASS_ID) ;; class variable, inited to
 *                                     ;; fixnum value <widgetclass_id>
 *                  Widget_Class       ;; SUPERCLASS
 ****************************************************************************/
LVAL Wcls_Create_Subclass_Of_WIDGET_CLASS(class_name, widgetclass_id)
     char* class_name;
     WidgetClass widgetclass_id;
{
  extern LVAL o_WIDGET_CLASS;	/* from wc_WIDGET.c */
  extern LVAL s_WIDGET_CLASS_ID; /* from wc_WIDGET.c */
  LVAL self;
  LVAL v_cvals;

  /* create 'Class' instance;
     set slot IVARTOTAL = WIDGETOBJ_SIZE (superclass has all slots)
     set slot IVARS = NIL              ;; no IVAR slots in this class
     set slot MESSAGES = NIL           ;; no messages (yet) for this class */
  self = xlclass(class_name, WIDGETOBJ_SIZE); /* note: self won't get gc'd because it is a symbol's value */
  
  /* set slot IVARCNT = 0, since there's no I-Vars in this WIDGET_CLASS
     subclass. Note that xlclass() above sets IVARTOTAL==WIDGETOBJ_SIZE==1
     since WIDGET_CLASS contains a single slot that holds the WidgetID.
     This is not accessible from lisp (since it's just a pointer), but
     is accessible from C via the accessors get_widgetobj_widgetID()
     and set_widgetobj_widgetID */
  setivar(self, IVARCNT, cvfixnum((FIXTYPE)0));

  /* set SUPERCLASS slot to o_WIDGET_CLASS ... */
  setivar(self, SUPERCLASS, o_WIDGET_CLASS);

  /* set CVARS */
  setivar(self, CVARS, cons(s_WIDGET_CLASS_ID, NIL));

  /* set CVALS -- set value for WIDGET_CLASS_ID */
  v_cvals = newvector((unsigned) DERIVED_WIDGET_CLASS_CVALS_SIZE);
  setivar(self, CVALS, v_cvals); /* note: also prevents gc'ing of v_cvals since <self> is protected */
  setelement(v_cvals, WIDGET_CLASS_ID, cvfixnum((FIXTYPE) widgetclass_id));

  /* add <WidgetClass, WIDGETCLASSOBJ> pair to table for future lookups */
  if (widgetclass_table_end_idx >= WIDGETCLASS_TABLE_SIZE)
    xlfatal("Fatal Error in Initialization -- please recompile w_classes.c with a larger WIDGETCLASS_TABLE_SIZE.");
  widgetclass_table[widgetclass_table_end_idx].widgetclass_OBJ = self;
  widgetclass_table[widgetclass_table_end_idx].widgetclass_ID = widgetclass_id;
  widgetclass_table_end_idx++;

#ifdef WINTERP_MOTIF_11
  /*
   * For Motif 1.1, we initialize the widget class.  If this weren't done,
   * then certain automatic resource conversion operations may not work right
   * for the first widget instance for a particular class (because first-time
   * widget creation also inits the widget class if not already done).
   *
   * Since there is no XtInitializeWidgetClass() in Motif 1.0/X11r3, the
   * aforementioned  problems may occur in 1.0 (anybody that really cares
   * should be on 1.1 by now anyways!)
   */
  XtInitializeWidgetClass(widgetclass_id);
#endif /* WINTERP_MOTIF_11 */

  return (self);
}

/*****************************************************************************
 * meta method overrides OBJECT method :SHOW (xlobj.c:obshow()). Since
 * TANGOIMAGEOBJs contain a special non-LVAL " _bogus_ivar_ " we don't want
 * to print that, since it isn't a LVAL and could result in segmentation
 * violations.
 ****************************************************************************/
extern LVAL s_stdout;
LVAL Wcls_Generic_Hybrid_Array_Method_SHOW(base_class)
     LVAL base_class;		/* a class OBJECT */
{
  LVAL self,fptr,cls,names;
  int ivtotal,n;

  /* get self and the file pointer */
  self = xlgaobject();		/* NPM: this method is used to retrieve WIDGETOBJs or TANGOIMAGEOBJs */
#ifdef BETTERIO
  fptr = (moreargs() ? xlgetfile(TRUE) : getvalue(s_stdout));
#else
  fptr = (moreargs() ? xlgetfile() : getvalue(s_stdout));
#endif
  xllastarg();

  /* get the object's class */
  cls = getclass(self);

  /* print the object and class */
  xlputstr(fptr,"Object is ");
  xlprint(fptr,self,TRUE);
  xlputstr(fptr,", Class is ");
  xlprint(fptr,cls,TRUE);
  xlterpri(fptr);

  /* print the object's instance variables -- NPM's modification over
     xlobj.c:obshow() is that we don't attempt to print the ivars for the
     "base_class" since these contain unprintable ivars */
  for (; ((cls != NIL) && (cls != base_class)); cls = getivar(cls,SUPERCLASS)) {
    names = getivar(cls,IVARS);
    ivtotal = getivcnt(cls,IVARTOTAL);
    for (n = ivtotal - getivcnt(cls,IVARCNT); n < ivtotal; ++n) {
      xlputstr(fptr,"  ");
      xlprint(fptr,car(names),TRUE);
      xlputstr(fptr," = ");
      xlprint(fptr,getivar(self,n),TRUE);
      xlterpri(fptr);
      names = cdr(names);
    }
  }

  /* return the object */
  return (self);
}


/*****************************************************************************
 * routine to print an object for PRINx -- stolen from xlisp/xlobj.c:xputobj().
 * this is called by wc_WIDGET.c:Widget_Class_Method_PRIN1()
 ****************************************************************************/
#ifdef ANSI
void Wcls_Print_WIDGETOBJ(LVAL fptr, LVAL o_widget)
#else /* !defined(ANSI) */
void Wcls_Print_WIDGETOBJ(fptr, o_widget)
     LVAL fptr;			/* STREAM || USTREAM */
     LVAL o_widget;		/* XLTYPE_WIDGETOBJ */
#endif /* ANSI */
{
  /* extern char buf[]; -- from xlglob.c, extern'd in xlisp.h */
  LVAL o_class = getclass(o_widget);
  LVAL lval_pname;

  if (((lval_pname = getivar(o_class, PNAME)) != NIL)
      && (ntype(lval_pname) == STRING)) {
    sprintf(buf,"#<widget %s: #", getstring(lval_pname));
    xlputstr(fptr, buf);
  }
  else {			/* if class of o_widget isn't named w/ PNAME, use Motif widget class name -- for back-compat w/ classes not defd w/ defmethod */
    extern LVAL o_WIDGET_CLASS;	/* from wc_WIDGET.c */
    LVAL o_prevclass = NIL;

    /* loop through superclasses, stopping at WIDGET_CLASS or NIL(superclass of object) */
    while ((o_class != o_WIDGET_CLASS) && (o_class != NIL)) {
      o_prevclass = o_class;
      o_class = getivar(o_class, SUPERCLASS);
    }

    /*
     * if the loop terminated with o_CLASS==o_WIDGET_CLASS and o_prevclass!=NIL
     * then o_prevclass is the subclass of WIDGET_CLASS created by
     * Wcls_Create_Subclass_Of_WIDGET_CLASS(). The latter calls xlclass() w/ the
     * widget-class name set in class-ivar PNAME.
     */
    if ((o_class == o_WIDGET_CLASS)
	&& (o_prevclass != NIL)
	&& ((lval_pname = getivar(o_prevclass, PNAME)) != NIL)
	&& (ntype(lval_pname) == STRING)) {
      sprintf(buf,"#<widget %s: #", getstring(lval_pname));
      xlputstr(fptr, buf);
    }
    else
      xlputstr(fptr, "#<widget WIDGET_CLASS: #");
  }
  
  sprintf(buf, AFMT, (long) o_widget);
  xlputstr(fptr, buf);
  xlputc(fptr, '>');
}


/*****************************************************************************
 * This routine accesses the WIDGET_CLASS_ID class variable that was
 * initialized in the above Wcls_Create_Subclass_Of_WIDGET_CLASS(). This
 * routine expects parameter o_class to be a class-object, that is
 * xlobj.c:xlclass_p(o_class) must be true. If your code
 * accidentally gives this routine an object instance rather than a class
 * instance, then you're hosed as the while loop will go trapseing off
 * through memory in a random fashion....
 *
 * This routine may be called with a class object as created by
 * Wcls_Create_Subclass_Of_WIDGET_CLASS(), or it may be called with
 * a class object that is a subclass of a class generated by
 * Wcls_Create_Subclass_Of_WIDGET_CLASS(). In other words, it
 * will work for widget subclasses created in lisp too.
 *
 * NOTE: the current implementation of this routine doesn't lookup
 * info in widgetclass_table[] because it is faster to chain up a
 * few superclasses to get to the WidgetClassID than to look for a
 * matching widetclassID amongst the 40-odd widgetclasses in winterp.
 ****************************************************************************/
WidgetClass Wcls_WIDGETCLASSOBJ_To_WidgetClassID(o_class)
     LVAL o_class;		/* OBJECT satisfying xlclass_p() */
{
  extern LVAL o_WIDGET_CLASS;	/* from wc_WIDGET.c */
  LVAL v_cvals;
  LVAL o_prevclass = NIL;
  
  /* loop through superclasses, stopping at WIDGET_CLASS or NIL(superclass of object) */
  while ((o_class != o_WIDGET_CLASS) && (o_class != NIL)) {
    o_prevclass = o_class;
    o_class = getivar(o_class, SUPERCLASS);
  }

  /*
   * if the loop terminated with o_CLASS==o_WIDGET_CLASS and o_prevclass!=NIL
   * then o_prevclass is the subclass of WIDGET_CLASS created by
   * Wcls_Create_Subclass_Of_WIDGET_CLASS() in which the class variable
   * WIDGET_CLASS_ID was set. So return that value as WidgetClassID, else NULL
   */
  if ((o_class == o_WIDGET_CLASS)
      && (o_prevclass != NIL)
      && ((v_cvals = getivar(o_prevclass, CVALS)) != NIL))
    return ((WidgetClass) getfixnum(getelement(v_cvals, WIDGET_CLASS_ID)));
  else
    return ((WidgetClass) NULL);
}

/******************************************************************************
 * given a WidgetClass, this returns the WIDGETCLASSOBJ corresponding to that
 * toolkit ID. If it can't find it, it returns NIL.
 ******************************************************************************/
LVAL Wcls_WidgetClassID_To_WIDGETCLASSOBJ(widget_class)
     WidgetClass widget_class;	/* address of widget's class structure */
{
  register int idx;
  
  for (idx = 0 ; (idx < widgetclass_table_end_idx) ; idx++)
    if (widgetclass_table[idx].widgetclass_ID == widget_class)
      return (widgetclass_table[idx].widgetclass_OBJ);

  return (NIL);
}


/******************************************************************************
 * (WIDGETOBJP <expr>)
 * returns T if argument is a WIDGETOBJ, else NIL
 ******************************************************************************/
LVAL Wcls_Prim_WIDGETOBJP()
{
  extern LVAL true;
  LVAL arg = xlgetarg();
  xllastarg();
  return (widgetobj_p(arg) ? true : NIL);
}


#ifdef WINTERP_DEBUG_1
/******************************************************************************
 * For debugging
 ******************************************************************************/ 
Wcls_Print_WidgetObj_Info(o_widget)
     LVAL o_widget;		/* XLTYPE_WIDGETOBJ */
{
  fprintf(stderr, "Created <WIDGETOBJ:0x%lx>\n", (unsigned long) o_widget);
  fprintf(stderr, "	widgetID = 0x%lx\n", (unsigned long) get_widgetobj_widgetID(o_widget));
  fprintf(stderr, "	windowID = 0x%lx\n", (unsigned long) XtWindow(get_widgetobj_widgetID(o_widget)));
}
#endif /* WINTERP_DEBUG_1 */

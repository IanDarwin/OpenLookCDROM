/* -*-C-*-
********************************************************************************
*
* File:         wc_Xtango.c
* RCS:          $Header: /users/npm/src/winterp/src-server/RCS/wc_Xtango.c,v 2.10 1994/06/06 15:40:37 npm Exp $
* Description:  TANGO:WIDGET_CLASS -- a Drawing Area "subclass" which
*		interfaces to Stasko&Hayes' XTANGO 2-D Animation Package
*		(xtangovararg, VERSION 1.52) -- the files in xtangovararg/src
*		were (or should be) installed in ../src-server/xtango. Note,
*		however, that WINTERP requires a modified version of Xtango
*		1.52. If attempting to interface to a different version of
*		Xtango, you should merge in portions from
*		../src-server/xtango/ *.[ch] within "#ifdef WINTERP"...
* Author:       Niels Mayer
* Created:      Fri Oct 27 22:22:38 1989
* Modified:     Sun Jun  5 15:15:40 1994 (Niels Mayer) npm@indeed
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
static char rcs_identity[] = "@(#)$Header: /users/npm/src/winterp/src-server/RCS/wc_Xtango.c,v 2.10 1994/06/06 15:40:37 npm Exp $";

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
#include <Xm/DrawingA.h>
#include <Xm/DrawnB.h>
#include "winterp.h"
#include "w_funtab.h"
#include "tango.h"

static LVAL k_BUTTON;		/* init'd in Wc_Xtango_Init(),
				   used by Xtango_Widget_Class_Method_ISNEW() */
LVAL k_VISIBLE, k_INVISIBLE;	/* init'd in Wc_Xtango_Init(),
				   used by Xtango_Widget_Class_Method_GET_IMAGES()
				   used by all Tango_*_Image_Class_Method_ISNEW() */
LVAL k_SHOW;			/* init'd in Wc_Xtango_Init(),
				   used by all Tango_*_Image_Class_Method_ISNEW() */
LVAL s_SEND, k_NEW, s_TANGO_W, k_LOAD_COLOR; /* init'd by Wc_Xtango_Init(),
						used by all Tango_*_Image_Get_Values() routines */

#ifdef _NO_PROTO		/* <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
extern void XmDrawnButtonCallbackStruct_Callbackproc(); /* wc_DrawnB.c */
extern void XmDrawingAreaCallbackStruct_Callbackproc(); /* wc_DrawingA.c */
#else  /* !defined(_NO_PROTO) ==> ANSI... */
extern void XmDrawnButtonCallbackStruct_Callbackproc( /* wc_DrawnB.c */
     Widget    widget,
     XtPointer client_data,
     XtPointer call_data);
extern void XmDrawingAreaCallbackStruct_Callbackproc( /* wc_DrawingA.c */
     Widget    widget,
     XtPointer client_data,
     XtPointer call_data);
#endif /* _NO_PROTO */


/*****************************************************************************
 *
 ****************************************************************************/
static void Xtango_Widget_Resize_Callback(widgetID, client_data, call_data)
     Widget    widgetID;
     XtPointer client_data;	/* WINTERP_TANGO_CONTEXT */
     XtPointer call_data;
{
  WINTERP_TANGO_CONTEXT tango_context = (WINTERP_TANGO_CONTEXT) client_data;

  Xtango_Save_Set_Context(tango_context);

  /*
   * Taken from xtango/xtangoxt.c:resize_callback()
   * the call is resize_callback(widgetID, NULL, call_data);
   */
  if (TANGO__data && TANGO__data->pixmap) { /* Avoid resize when widget initially displayed -- *before* TANGO__data->pixmap has been created. */
    TANGO_anim_setup(widgetID);	/* inits and sizes offscrn pixmap TANGO__data->pixmap */
    TANGO_refresh();
  }

  Xtango_Restore_Context();
}


/*****************************************************************************
 *
 ****************************************************************************/
static void Xtango_Widget_Destroy_Callback(widgetID, client_data, call_data)
     Widget    widgetID;
     XtPointer client_data;	/* WINTERP_TANGO_CONTEXT */
     XtPointer call_data;	/* not used */
{
  WINTERP_TANGO_CONTEXT elt_to_destroy = (WINTERP_TANGO_CONTEXT) client_data;
  Xtango_Save_Set_Context(elt_to_destroy);
  /* NOT NEEDED: Xtango_Check_Initialized() --  "if (TANGO__data)" below wraps
     the Xtango calls dependent on an initialized tango. Destruction of
     TANGOIMAGEOBJs in that section is only needed if the widget&TANGO__data
     have been initialized by a previous call to :BEGIN_DRAWING... */
  
  if (TANGO__data) {
    /*
     * remove from v_savedobjs all the TANGOIMAGEOBJs assoc'd with the widget being
     * destroyed. This will allow them to be garbage collected next time gc() gets
     * called.
     */
    IMAGE_PTR	imageptr, next_imageptr;
    LVAL obj;
    int  i         = Wso_Hash(elt_to_destroy->widget_OBJ); /* note that we hash on the widgetobj with which the images are assoc'd */
    LVAL l_hbucket = getelement(v_savedobjs, i); /* a list of saved objects belonging to this hashbucket */
    LVAL l_prev    = NIL;

    while (l_hbucket != NIL)	/* while there are elements in the hashbucket */
      if (((obj = car(l_hbucket)) != NIL) /* <obj> points to cur elt which is non-NIL */
	  && (ntype(obj) == XLTYPE_TANGOIMAGEOBJ) /* is <obj> a TANGOIMAGEOBJ on this widget? */
	  && (get_tangoimageobj_context(obj) == elt_to_destroy)) /* (that is, is it associated with the widget being destroyed) */
	{			/* THEN (1) mark the TANGOIMAGEOBJ as destroyed and (2) remove TANGOIMAGEOBJ from savedobjs[]  */
	  /* (0) Normally, we'd want to free/destroy/delete each TANGO_IMAGE
	     assoc'd with the tango-widget being destroyed. However the Tango call
	     TANGOtrans_perform(TANGOtrans_create(TANGO_TRANS_TYPE_DELETE, get_tangoimageobj_timageID(obj), path))
	     doesn't actually free TANGO_IMAGE data in get_tangoimageobj_timageID(obj),
	     therefore, we don't bother doing that. Furthermore doing TANGO_TRANS_TYPE_DELETE
	     on each TANGO_IMAGE is a very slow operation because, for each TANGO_IMAGE,
	     it deletes the image then redraws all the remaining ones... */

	  /* (1) mark the TANGOIMAGEOBJ as destroyed, such that any destroyed
	     TANGOIMAGEOBJ referenced in a symbol-value or closure will generate
	     an xlerror() when passed on to a method or function expecting a TANGOIMAGEOBJ
	     parameter. */
	  set_tangoimageobj_timageID(obj, (TANGO_IMAGE) NULL); /* mark TANGOIMAGEOBJ as invalid */
	  set_tangoimageobj_context(obj, (WINTERP_TANGO_CONTEXT) NULL);

	  /* (2) remove the TANGOIMAGEOBJ from savedobjs[]. */
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

    /*
     * Free up data associated with the TANGO_WIDGET instance. This data
     * is treated by Xtango as "global" with a lifetime that equals the life
     * of an Xtango program, therefore Xtango provides no way of freeing the
     * data. In WINTERP, the global tango data is kludged
     * into being associated w/ a TANGO_WIDGET, and is free'd when the
     * TANGO_WIDGET is destroyed.
     *
     * Note that we don't free up Graphics-Context or assocated
     * data set up by TANGO_initGC(). This data is inited by the
     * first call to TANGO_initGC() and shared by all subsequent
     * instances of TANGO_WIDGET
     */

    /* free offscreen frame pixmaps created by xtangoanim.c:TANGO_anim_setup(). */
    if (TANGO__data->pixmap)
      XFreePixmap(TANGO__data->display, TANGO__data->pixmap); 

    /* free all IMAGE_PTRs, their TANGO_IMAGE and if composite, sub TANGO_IMAGEs... */
    imageptr = TANGO__data->confighead;
    while (imageptr) {
      Tcls_Free_TANGO_IMAGE(imageptr->image);
      next_imageptr = imageptr->nexti;
      free((char*) imageptr);
      imageptr = next_imageptr;
    }
      
    free((char*) TANGO__data);	/* free 'struct ANIMATION' created in Xtango_Widget_Class_Method_BEGIN_DRAWING()/TANGO_setup_windows() */      

    TANGO__data	= (ANIMATION_PTR) NULL;
    Xtango_Copy_Context(elt_to_destroy); /* copy the reset tango context back to the WINTERP_TANGO_CONTEXT, just incase something tries to reference the freed node before it gets clobbered by another malloc... */
  }

  Xtango_Restore_Context();

  /*
   * Remove a assoc-list elt for this tango-WIDGETOBJ. Mark as invalid any other
   * objects (e.g. LVAL(XLTYPE_TANGO_TRANS)) that might reference the now-destroyed
   * WINTERP_TANGO_CONTEXT, e.g. s.t. get_tangotrans(x)==NULL.
   */
  Xtango_Remove_WINTERP_TANGO_CONTEXT(elt_to_destroy);
}


/*****************************************************************************
 *
 ****************************************************************************/
static void Wc_Xtango_Initialize_WIDGETOBJ(o_widget, widget_id)
     LVAL   o_widget;		/* XLTYPE_WIDGETOBJ */
     Widget widget_id;
{   
  WINTERP_TANGO_CONTEXT tango_data =
    /* create a assoc-list elt for this tango-WIDGETOBJ */
    Xtango_Create_WINTERP_TANGO_CONTEXT(o_widget, widget_id);

  XtAddCallback(widget_id, XmNdestroyCallback,
		Xtango_Widget_Destroy_Callback, (XtPointer) tango_data); /* on destroy free tango_data assoc'd w/ this widget */
  XtAddCallback(widget_id, XmNresizeCallback,
		Xtango_Widget_Resize_Callback, (XtPointer) tango_data);	/* wrapper for xtangoxt.c:resize_callback() sets tango_data to xtango globals */
}


/*****************************************************************************
 * (send TANGO:WIDGET_CLASS :new 
 *                           [:managed/:unmanaged]
 *                           [:button]
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
 * (send TANGO:WIDGET_CLASS :new ...)
 *  --> XtCreateWidget(... xmDrawingAreaWidgetClass ...)
 * (send TANGO:WIDGET_CLASS :new :button ...)
 *  --> XtCreateWidget(... xmDrawnButtonWidgetClass ...)
 ****************************************************************************/
LVAL Xtango_Widget_Class_Method_ISNEW()
{
  LVAL self, o_parent;
  char* name;
  Boolean managed_p;
  Widget parent_widget_id, widget_id;
  WidgetClass widget_class_id;

  self = Wcls_Get_OBJECT_Arg_Returning_WIDGETOBJ(); /* NOTE: xlobj.c:clnew() returns an OBJECT; 
						       get the arg and mark it's type slot as a WIDGETOBJ */

  /* get optional managed/unmanaged arg */
  if (moreargs() && ((*xlargv == k_managed) || (*xlargv == k_unmanaged)))
    managed_p = (nextarg() == k_managed);
  else
    managed_p = FALSE;		/* by default don't call XtManageChild() */

  /* get optional :button arg */
  if (moreargs() && (*xlargv == k_BUTTON)) {
    nextarg();
    widget_class_id = xmDrawnButtonWidgetClass;	/* :BUTTON arg means create a subclass of XM_DRAWN_BUTTON_WIDGET_CLASS */
  }
  else
    widget_class_id = xmDrawingAreaWidgetClass;	/* by default, we want this to be a subclass of XM_DRAWING_AREA_WIDGET_CLASS, not XM_DRAWN_BUTTON_WIDGET_CLASS */

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
    widget_id = XtCreateWidget(name, widget_class_id, parent_widget_id, xt_arglist, xt_numargs);
    Wres_Free_C_Arglist_Data();
  }
  else 
    widget_id = XtCreateWidget(name, widget_class_id, parent_widget_id, ARGLIST());

  Wcls_Initialize_WIDGETOBJ(self, widget_id);
  Wc_Xtango_Initialize_WIDGETOBJ(self, widget_id); /* set up resize and destroy callbacks for xtango */

  if (managed_p)
    XtManageChild(widget_id);

#ifdef WINTERP_DEBUG_1
  Wcls_Print_WidgetObj_Info(self);
#endif /* WINTERP_DEBUG_1 */
  return (self);
}


/******************************************************************************
 * Same as WIDGET_CLASS's :add_callback method except that this understands
 * how to get values from the XmDrawnButtonCallbackStruct/XmDrawningAreaCallbackStruct.
 *
 * If the TANGO:WIDGET_CLASS instance was created without the :BUTTON argument, then
 * specifying one or more of the following symbols in the callback bindings 
 * list will bind that symbol's value in the lexical environment of the callback:
 * CALLBACK_WIDGET
 * CALLBACK_REASON
 * CALLBACK_XEVENT
 * CALLBACK_WINDOW
 *
 * If the TANGO:WIDGET_CLASS instance was created WITH the :BUTTON argument, then
 * the following symbols are valid:
 * CALLBACK_WIDGET
 * CALLBACK_REASON
 * CALLBACK_XEVENT
 * CALLBACK_WINDOW
 * CALLBACK_CLICK_COUNT
 ******************************************************************************/
LVAL Xtango_Widget_Class_Method_ADD_CALLBACK()
{
  Widget widget_id;

  /* HACK-O-RAMA: We need to peek at <self>, the first argument on the argument
     stack, in order to retrieve the widget instance and find out the actual
     underlying class of the Widget... We can't actually fetch the id using
     xlga_widgetobj() since Wcb_Meta_Method_Add_Callback() expects to find
     all arguments intact... */
  if (moreargs()) {
    if (widgetobj_p(*xlargv)) {
      if (widget_id = get_widgetobj_widgetID(*xlargv)) {
	if (XtClass(widget_id) == xmDrawingAreaWidgetClass)
	  return (Wcb_Meta_Method_Add_Callback(XmDrawingAreaCallbackStruct_Callbackproc, FALSE));
	else			/* xmDrawnButtonWidgetClass */
	  return (Wcb_Meta_Method_Add_Callback(XmDrawnButtonCallbackStruct_Callbackproc, FALSE));
      }
      else
	xlerror("Widget object has been :destroy'd or hasn't been initialized by :isnew.", *xlargv);
    }
    else
      xlbadtype(*xlargv);
  }
  else
    xltoofew();
}


/******************************************************************************
 * Same as WIDGET_CLASS's :set_callback method except that this understands
 * how to get values from the XmDrawnButtonCallbackStruct/XmDrawningAreaCallbackStruct.
 *
 * If the TANGO:WIDGET_CLASS instance was created without the :BUTTON argument, then
 * specifying one or more of the following symbols in the callback bindings 
 * list will bind that symbol's value in the lexical environment of the callback:
 * CALLBACK_WIDGET
 * CALLBACK_REASON
 * CALLBACK_XEVENT
 * CALLBACK_WINDOW
 *
 * If the TANGO:WIDGET_CLASS instance was created WITH the :BUTTON argument, then
 * the following symbols are valid:
 * CALLBACK_WIDGET
 * CALLBACK_REASON
 * CALLBACK_XEVENT
 * CALLBACK_WINDOW
 * CALLBACK_CLICK_COUNT
 ******************************************************************************/
LVAL Xtango_Widget_Class_Method_SET_CALLBACK()
{
  Widget widget_id;

  /* HACK-O-RAMA: We need to peek at <self>, the first argument on the argument
     stack, in order to retrieve the widget instance and find out the actual
     underlying class of the Widget... We can't actually fetch the id using
     xlga_widgetobj() since Wcb_Meta_Method_Add_Callback() expects to find
     all arguments intact... */
  if (moreargs()) {
    if (widgetobj_p(*xlargv)) {
      if (widget_id = get_widgetobj_widgetID(*xlargv)) {
	if (XtClass(widget_id) == xmDrawingAreaWidgetClass)
	  return (Wcb_Meta_Method_Add_Callback(XmDrawingAreaCallbackStruct_Callbackproc, TRUE));
	else			/* xmDrawnButtonWidgetClass */
	  return (Wcb_Meta_Method_Add_Callback(XmDrawnButtonCallbackStruct_Callbackproc, TRUE));
      }
      else
	xlerror("Widget object has been :destroy'd or hasn't been initialized by :isnew.", *xlargv);
    }
    else
      xlbadtype(*xlargv);
  }
  else
    xltoofew();
}


/******************************************************************************
 *  (send <tango-widget> :BEGIN_DRAWING)
 *	==> returns NIL.
 *
 * The following is derived from TANGOinit(), with changes to accomodate
 * the WINTERP/XTANGO interface...
 *
 * TODO: xtangovarargs vsn 1.43 introduced call 'TANGOset_bgcolor(char* color)'
 * and I've introduced (send <tangowidget> :set_bgcolor <color>), however
 * it would be useful to set the background color of the tangodrawingarea
 * to that of it's drawing-area widget parent. We should do that in this proc.
 * Need to get in below level of TANGOset_bgcolor() [see TANGOload_color()]
 * such that we cause the widget's bg Pixel to be loaded into xtango's color
 * database (perhaps under name "widget-default-bg"), then do a TANGOset_bgcolor()
 * to that color....
 ******************************************************************************/
LVAL Xtango_Widget_Class_Method_BEGIN_DRAWING()
{
  extern Display* display;	/* winterp.c */
  Visual *vis;
  LVAL self;
  Widget widget_id;
  WINTERP_TANGO_CONTEXT tango_context;

  widget_id = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&self);
  xllastarg();

  /* valid window assoc'd with widget is expected by the time animate_begin() is called ... */
  if (!XtIsRealized(widget_id))
    xlfail("TANGO_WIDGET_CLASS:BEGIN_DRAWING called on unrealized tango-widget instance...");

  tango_context = Xtango_Find_Context_From_WidgetID(widget_id);
  Xtango_Save_Set_Context(tango_context); /* initialize globals used by Xtango to values set in Wc_Xtango_Initialize_WIDGETOBJ() */
  if (TANGO__data) {
    Xtango_Restore_Context();	/* restore before returning -- assumes prior call to Xtango_Save_Set_Context_From_WidgetID() or Xtango_Save_Set_Context() */
    xlfail("TANGO_WIDGET_CLASS:BEGIN_DRAWING already called...");
  }
  Xtango_Begin_Drawing_Context_Kludge(); /* see commented out usage of Xtango_Restore_Context() below */

  /* from TANGOinit():TANGO_image_init() */
  TANGO_image_init();		/* a NO-OP in Xtango 1.52 */

  /* from TANGOinit():TANGO_setup_windows() */
  TANGO__data = (ANIMATION_PTR) XtMalloc(sizeof(struct ANIMATION));
  TANGO__data->display = display;		/* currently, use WINTERP's global display, multi-display version will have to do something here... */
  vis = DefaultVisual(TANGO__data->display, DefaultScreen(TANGO__data->display));
  TANGO__data->color_screen = (vis->class != GrayScale && vis->class != StaticGray);
  TANGO__data->pixmap = (Pixmap) NULL; /* needed to short-circuit initial attempt at refreshing window */
  TANGO__data->debug = 0;

  /* from TANGO_setup_windows():TANGO_initGC() */
  TANGO_initGC();

  /* from TANGO_setup_windows():TANGO_layout_window() */
  TANGO__data->easel = widget_id; /* when xtangomotif.c:TANGO_layout_window() finishes, 'TANGO__data->easel'
				   is realized (e.g. a window exists).
				   "if (!XtIsRealized(widget_id))" above ensures this is true... */
		      
  /* from TANGO_setup_windows():animate_begin() */
  animate_begin(); /* TANGO__data structures init'd here */

  /* ok, now we've done all the steps taken by TANGOinit() ... */

  /* update associated struct in WINTERP_TANGO_CONTEXT alist w/ now-initialized globals... */
  Xtango_Copy_Context(tango_context);

  /* restore previous context */
  /*
   * Xtango_Restore_Context();
   * --> actually, don't restore, this way makes all tango-ops after
   *     :BEGIN_DRAWING don't have to set&restore context....
   */

  return (NIL);
}


/******************************************************************************
 *   (send <tango-widget> :PAN <direction> [<pan-amount-flonum>])
 *	==> returns NIL.
 *
 *   <direction> can be one of
 *	:UP	-- pans the xtango view-window up.
 *	:DOWN	-- pans the xtango view-window down.
 *	:LEFT	-- pans the xtango view-window left.
 *	:RIGHT	-- pans the xtango view-window right.
 *
 *   <pan-amount-flonum> is an optional flonum indicating amount to
 *	pan the viewer. If ommitted, it defaults to 0.2
 ******************************************************************************/
static LVAL k_UP, k_DOWN, k_LEFT, k_RIGHT; /* init'd in Wc_Xtango_Init() */
LVAL Xtango_Widget_Class_Method_PAN()
{
  LVAL self, lval_direction;
  Widget widget_id;
  double panfactor, change;

  /* get <self> */
  widget_id = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&self);

  /* get <direction> */
  lval_direction = xlgetarg();

  /* get optional [<pan-amount-flonum>] */
  if (moreargs())
    panfactor = (double) getflonum(xlgaflonum());
  else
    panfactor = 0.2;
    
  xllastarg();

  Xtango_Save_Set_Context_From_WidgetID(widget_id);
  Xtango_Check_Initialized();

  /* code derived from xtango/xtangoxt.c:pan_callback() */
  if (lval_direction == k_LEFT) {
    change = (TANGO__data->rx - TANGO__data->lx) * panfactor;
    TANGO__data->lx -= change;
    TANGO__data->rx -= change;
  }
  else if (lval_direction == k_RIGHT) {
    change = (TANGO__data->rx - TANGO__data->lx) * panfactor;
    TANGO__data->lx += change;
    TANGO__data->rx += change;
  }
  else if (lval_direction == k_UP) {
    change = (TANGO__data->by - TANGO__data->ty) * panfactor;
    TANGO__data->ty -= change;
    TANGO__data->by -= change;
  }
  else if (lval_direction == k_DOWN) {
    change = (TANGO__data->by - TANGO__data->ty) * panfactor;
    TANGO__data->by += change;
    TANGO__data->ty += change;
  }
  else {
    Xtango_Restore_Context();
    xlerror("Invalid <direction> keyword, expected one of [:LEFT, :RIGHT, :UP, :DOWN].",
	    lval_direction);
  }

  TANGO__data->x_WIN_COORD_to_int = ((WIN_COORD) TANGO__data->width) /
    (TANGO__data->rx - TANGO__data->lx);
  TANGO__data->y_WIN_COORD_to_int = ((WIN_COORD) TANGO__data->height) /
    (TANGO__data->by - TANGO__data->ty);

  TANGO_refresh();

  Xtango_Restore_Context();

  return (NIL);
}


/******************************************************************************
 *   (send <tango-widget> :ZOOM <direction> [<zoom-amount-flonum>])
 *	==> returns NIL.
 *
 *   <direction> can be one of
 *	:IN	-- zoom in
 *	:OUT	-- zoom out
 *
 *   <zoom-amount-flonum> is an optional flonum indicating amount to
 *	zoom the viewer. If ommitted, it defaults to 0.8
 ******************************************************************************/
static LVAL k_IN, k_OUT;	/* init'd in Wc_Xtango_Init() */
LVAL Xtango_Widget_Class_Method_ZOOM()
{
  LVAL self, lval_direction;
  Widget widget_id;
  double zoomfactor, change;

  /* get <self> */
  widget_id = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&self);

  /* get <direction> */
  lval_direction = xlgetarg();

  /* get optional [<zoom-amount-flonum>] */
  if (moreargs())
    zoomfactor = (double) getflonum(xlgaflonum());
  else
    zoomfactor = 0.8;
    
  xllastarg();

  Xtango_Save_Set_Context_From_WidgetID(widget_id);
  Xtango_Check_Initialized();

  /* code derived from xtango/xtangoxt.c:zoom_callback() */
  if (lval_direction == k_IN) {
    change = (TANGO__data->rx - TANGO__data->lx) * (1.0-zoomfactor);
    TANGO__data->rx -= change/2.0;
    TANGO__data->lx += change/2.0;
    change = (TANGO__data->by - TANGO__data->ty) * (1.0-zoomfactor);
    TANGO__data->by -= change/2.0;
    TANGO__data->ty += change/2.0;
  }
  else if (lval_direction == k_OUT) {
    change = (TANGO__data->rx - TANGO__data->lx) * (1.0/zoomfactor - 1.0);
    TANGO__data->rx += change/2.0;
    TANGO__data->lx -= change/2.0;
    change = (TANGO__data->by - TANGO__data->ty) * (1.0/zoomfactor - 1.0);
    TANGO__data->by += change/2.0;
    TANGO__data->ty -= change/2.0;
  }
  else {
    Xtango_Restore_Context();
    xlerror("Invalid <direction> keyword, expected one of [:IN, :OUT].",
	    lval_direction);
  }

  TANGO__data->x_WIN_COORD_to_int = ((WIN_COORD) TANGO__data->width) /
    (TANGO__data->rx - TANGO__data->lx);
  TANGO__data->y_WIN_COORD_to_int = ((WIN_COORD) TANGO__data->height) /
    (TANGO__data->by - TANGO__data->ty);

  TANGO_refresh();

  Xtango_Restore_Context();

  return (NIL);
}


/******************************************************************************
 *   (send <tango-widget> :SET_COORD <lx> <by> <rx> <ty>)
 *	==> returns NIL.
 *
 *	where:	<lx> - left x coord (flonum)	(default, 0.0)
 *		<by> - bottom y coord (flonum)	(default, 1.0)
 *		<rx> - right x coord (flonum)   (default, 1.0)
 *		<ty> - top y coord (flonum)	(default, 0.0)
 ******************************************************************************/
LVAL Xtango_Widget_Class_Method_SET_COORD()
{
  LVAL self;
  Widget widget_id;
  double lx, by, rx, ty;

  /* get <self> */
  widget_id = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&self);

  /* get <lx> - left x coord (flonum) */
  lx = (double) getflonum(xlgaflonum());
  /* get <by> - bottom y coord (flonum) */
  by = (double) getflonum(xlgaflonum());
  /* get <rx> - right x coord (flonum) */
  rx = (double) getflonum(xlgaflonum());
  /* get <ty> - top y coord (flonum) */
  ty = (double) getflonum(xlgaflonum());
    
  xllastarg();

  Xtango_Save_Set_Context_From_WidgetID(widget_id);
  Xtango_Check_Initialized();

  TANGOset_coord(lx, by, rx, ty);

  Xtango_Restore_Context();

  return (NIL);
}


/******************************************************************************
 *   (send <tango-widget> :INQ_COORD)
 *	==> returns list of flonums (<lx> <by> <rx> <ty>) 
 *	(which can be applied to subsequent calls to :SET_COORD)
 ******************************************************************************/
LVAL Xtango_Widget_Class_Method_INQ_COORD()
{
  LVAL self;
  Widget widget_id;
  double lx, by, rx, ty;
  LVAL lval_lx, lval_by, lval_rx, lval_ty, lval_result;

  /* protect some pointers */
  xlstkcheck(5);
  xlsave(lval_lx);
  xlsave(lval_by);
  xlsave(lval_rx);
  xlsave(lval_ty);
  xlsave(lval_result);

  /* get <self> */
  widget_id = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&self);

  xllastarg();

  Xtango_Save_Set_Context_From_WidgetID(widget_id);
  Xtango_Check_Initialized();

  TANGOinq_coord(&lx, &by, &rx, &ty);

  lval_ty = cvflonum((FLOTYPE) ty);
  lval_rx = cvflonum((FLOTYPE) rx);
  lval_by = cvflonum((FLOTYPE) by);
  lval_lx = cvflonum((FLOTYPE) lx);
  lval_result = cons(lval_ty, NIL);
  lval_result = cons(lval_rx, lval_result);
  lval_result = cons(lval_by, lval_result);
  lval_result = cons(lval_lx, lval_result);

  Xtango_Restore_Context();

  /* restore the stack */
  xlpopn(5);

  return (lval_result);
}


/******************************************************************************
 * (send <tango-widget> :SET_ANIMATION_EVENT_PROCESSING <event_mask>)
 *	--> returns NIL.
 * <event_mask> is a FIXNUM, created by taking the LOGIOR of KEY_PRESS_MASK
 * BUTTON_PRESS_MASK, and EXPOSURE_MASK. Any other mask values (as used perhaps
 * by :ADD_EVENT_HANDLER/:SET_EVENT_HANDLER methods) are ignored.
 *
 * KEY_PRESS_MASK	-- interrupts running animation when ^C (control-C)
 *			   entered in the window running the animation.
 * BUTTON_PRESS_MASK	-- interrupts running animation when any mouse button
 *			   is clicked within the window running the animation.
 * EXPOSURE_MASK	-- if the window running the animation is obscured
 *			   then exposed, LOGIORing this value to the event mask
 *			   will cause the entire window to refresh it's graphics.
 *			   When not set, animation windows will not completely
 *			   refresh themselves until the current animation's
 *			   transition has finished.
 * By default, all TANGO:WIDGET_CLASS instances are created with the following
 * setting:
 *		(send <tango-widget> :SET_ANIMATION_EVENT_PROCESSING 
 *			             (logior KEY_PRESS_MASK EXPOSURE_MASK))
 ******************************************************************************/
LVAL Xtango_Widget_Class_Method_SET_ANIMATION_EVENT_PROCESSING()
{
  LVAL self;
  Widget widget_id;
  EventMask evmask;

  widget_id = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&self);
  evmask = (EventMask) getfixnum(xlgafixnum());
  xllastarg();

  Xtango_Save_Set_Context_From_WidgetID(widget_id);
  Xtango_Check_Initialized();

  /* ignore all other than KEY_PRESS_MASK BUTTON_PRESS_MASK EXPOSURE_MASK */
  evmask = (evmask & KeyPressMask)
         | (evmask & ButtonPressMask)
	 | (evmask & ExposureMask);

  TANGO__data->anim_event_mask = evmask;

  Xtango_Restore_Context();

  return (NIL);
}


/******************************************************************************
 *  (send <tango-widget> :SET_DELAY <sleep_usecs_fixnum>)
 *	==> returns NIL.
 *
 *	where <sleep_usecs_fixnum> is a fixnum representing the number of
 *	miscroseconds to sleep between frames of the animation. The
 *	default value is 0.
 ******************************************************************************/
LVAL Xtango_Widget_Class_Method_SET_DELAY()
{
  LVAL self;
  Widget widget_id;
  int sleep_usecs;

  widget_id = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&self);
  sleep_usecs = (int) getfixnum(xlgafixnum());
  xllastarg();

  if (sleep_usecs < 0)		/* prevent negative sleep values??? */
    sleep_usecs = 0;
  
  Xtango_Save_Set_Context_From_WidgetID(widget_id);
  Xtango_Check_Initialized();

  TANGO__data->delay = sleep_usecs;

  Xtango_Restore_Context();

  return (NIL);
}


/******************************************************************************
 *  (send <tango-widget> :SET_DEBUG <debug_p>)
 *	==> returns NIL.
 *
 *	When <debug_p> is non-NIL, this method turns on Xtango's internal
 *	debugging output -- prints out names of Xtango functions being called
 *	during the course of an animation. The output is sent to *trace-output*
 *	which defaults to *terminal-io* unless redirected.
 ******************************************************************************/
LVAL Xtango_Widget_Class_Method_SET_DEBUG()
{
  LVAL self, debug_p;
  Widget widget_id;
  WINTERP_TANGO_CONTEXT tango_context;

  widget_id = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&self);
  debug_p = xlgetarg();
  xllastarg();

  tango_context = Xtango_Find_Context_From_WidgetID(widget_id);
  Xtango_Save_Set_Context(tango_context);
  Xtango_Check_Initialized();

  TANGO__data->debug = (debug_p != NIL) ? TANGO_DEBUG_EXTERNAL : 0;

  /* Xtango_Copy_Context(tango_context) */
  Xtango_Restore_Context();

  return (NIL);
}


/******************************************************************************
 * (send <tango-widget> :MONO_PATTERN_REPRESENTATION <kind>)
 *	--> returns NIL if <tango-widget> is on a color screen, and
 *	    doesn't make any changes.
 *	--> returns T if <tango-widget> is on a monochrome screen,
 *
 * where <kind> can be
 *	:COLORS		-- patterns represent colors on mono displays.
 *	:FILLS		-- patterns represent fill-styles on mono displays
 ******************************************************************************/
static LVAL k_COLORS, k_FILLS;	/* init'd in Wc_Xtango_Init() */
LVAL Xtango_Widget_Class_Method_MONO_PATTERN_REPRESENTATION()
{
  extern LVAL true;
  LVAL self, lval_rep_kind, lval_result;
  Widget widget_id;

  /* get <self> */
  widget_id = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&self);

  /* get <kind> */
  lval_rep_kind = xlgetarg();

  xllastarg();

  Xtango_Save_Set_Context_From_WidgetID(widget_id);
  Xtango_Check_Initialized();
  
  if (!TANGO__data->color_screen) {
    if (lval_rep_kind == k_COLORS)
      TANGO__data->mono_fillstyle = 0;
    else if (lval_rep_kind == k_FILLS)
      TANGO__data->mono_fillstyle = 1;
    else {
      Xtango_Restore_Context();
      xlerror("Invalid <pattern-rep-kind> keyword, expected one of [:COLORS, :FILLS].",
	      lval_rep_kind);
    }
    lval_result = true;
  }
  else {
    lval_result = NIL;		/* do nothing for color screen, return NIL */
  }

  Xtango_Restore_Context();

  return (lval_result);
}


/******************************************************************************
 *  (send <tango-widget> :REFRESH)
 *	==> returns NIL.
 ******************************************************************************/
LVAL Xtango_Widget_Class_Method_REFRESH()
{
  LVAL self;
  Widget widget_id;

  widget_id = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&self);
  xllastarg();

  Xtango_Save_Set_Context_From_WidgetID(widget_id);
  Xtango_Check_Initialized();

  TANGO_refresh();

  Xtango_Restore_Context();

  return (NIL);
}


/******************************************************************************
 * (send <tango-widget> :INPUT_COORD)
 *	==> on success, returns a COMPLEX FLONUM number #C(<x> <y>).
 *	==> on failure, returns NIL.
 ******************************************************************************/
LVAL Xtango_Widget_Class_Method_INPUT_COORD()
{
  WIN_COORD x, y;
  LVAL self, lval_result;
  Widget widget_id;

  widget_id = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&self);
  xllastarg();

  Xtango_Save_Set_Context_From_WidgetID(widget_id);
  Xtango_Check_Initialized();

  if (TANGOinput_coord(&x, &y))
    lval_result = newdcomplex((double) x, (double) y);
  else 
    lval_result = NIL;

  Xtango_Restore_Context();

  return (lval_result);
}


/******************************************************************************
 * (send <tango-widget> :INPUT_IMAGE)
 *	==> returns <tango_image> on success, NIL on failure
 ******************************************************************************/
LVAL Xtango_Widget_Class_Method_INPUT_IMAGE()
{
  TANGO_IMAGE im;
  LVAL self, lval_result;
  Widget widget_id;

  widget_id = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&self);
  xllastarg();

  Xtango_Save_Set_Context_From_WidgetID(widget_id);
  Xtango_Check_Initialized();

  if (TANGOinput_image(&im)) 
    lval_result = Tcls_TangoImageID_To_TANGOIMAGEOBJ(im);
  else 
    lval_result = NIL;

  Xtango_Restore_Context();

  return (lval_result);
}


/******************************************************************************
 * (send <tango-widget> :GET_EVENT_IMAGE <xevent>)
 *	==> returns <tango_image> on success, NIL on failure
 *
 * <xevent> is typically taken from the CALLBACK_XEVENT or EVHANDLER_XEVENT
 * values set within widget-class methods :SET_CALLBACK/:ADD_CALLBACK or
 * :SET_EVHANDLER/:ADD_EVHANDLER. <xevent> is used to determine the x,y location
 * of a KeyPress, KeyRelease, ButtonPress, ButtonRelease, MotionNotify,
 *  EnterNotify, or LeaveNotify type event. 
 *
 * Note that if you use the TANGO:WIDGET_CLASS's :XMN_INPUT_CALLBACK
 * (set by :SET_CALLBACK/:ADD_CALLBACK method on XmDrawingArea or XmDrawnButton)
 * by default, the semi-useless result of calling the same input callback function
 * on an ButtonPress, ButtonRelease, KeyPress, and KeyRelease events. You must call
 * one of the following methods in conjunction with an :XMN_INPUT_CALLBACK:
 * ":SET_VALUES :XMN_TRANSLATIONS" or :OVERRIDE_TRANSLATIONS
 *
 * One may also use standard event handlers without having to alter the
 * translation table. Use :SET_EVHANDLER/:ADD_EVHANDLER while specifying
 * the desired event-mask. (e.g. BUTTON_PRESS_MASK, BUTTON_RELEASE_MASK,
 * KEY_PRESS_MASK, KEY_RELEASE_MASK).
 * 
 * For example, the following event-handler flashes the tango-image which was
 * clicked via mouse, as specified by the event-mask BUTTON_PRESS_MASK.
 *
 *  (send tango_w :set_event_handler BUTTON_PRESS_MASK
 *	'(EVHANDLER_WIDGET EVHANDLER_XEVENT)
 *	'(
 *	  (let ((timage (send EVHANDLER_WIDGET :get_event_image EVHANDLER_XEVENT)))
 *	    (if timage
 *		(send timage :tap_flash :perform 1)
 *	      ))
 *	  ))
 ******************************************************************************/
LVAL Xtango_Widget_Class_Method_GET_EVENT_IMAGE()
{
  TANGO_IMAGE im;
  LVAL self, lval_result;
  Widget widget_id;
  XEvent *event;

  widget_id = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&self);
  event = get_xevent(xlga_xevent());
  xllastarg();

  Xtango_Save_Set_Context_From_WidgetID(widget_id);
  Xtango_Check_Initialized();

  if (TANGOget_event_image(&im, event)) 
    lval_result = Tcls_TangoImageID_To_TANGOIMAGEOBJ(im);
  else 
    lval_result = NIL;

  Xtango_Restore_Context();

  return (lval_result);
}


/******************************************************************************
 * (send <tango-widget> :GET_EVENT_COORD <xevent>)
 *	==> returns #C(<x> <y>) on success, NIL on failure
 *
 * <xevent> is typically taken from the CALLBACK_XEVENT or EVHANDLER_XEVENT
 * values set within widget-class methods :SET_CALLBACK/:ADD_CALLBACK or
 * :SET_EVHANDLER/:ADD_EVHANDLER. <xevent> is used to determine the x,y
 * location of a KeyPress, KeyRelease, ButtonPress, ButtonRelease,
 * MotionNotify, EnterNotify, or LeaveNotify type event. 
 ******************************************************************************/
LVAL Xtango_Widget_Class_Method_GET_EVENT_COORD()
{
  LVAL self, lval_result;
  Widget widget_id;
  XEvent *event;
  WIN_COORD x, y;

  widget_id = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&self);
  event = get_xevent(xlga_xevent());
  xllastarg();

  Xtango_Save_Set_Context_From_WidgetID(widget_id);
  Xtango_Check_Initialized();

  if (TANGOget_event_coord(&x, &y, event)) 
    lval_result = newdcomplex((double) x, (double) y);
  else 
    lval_result = NIL;

  Xtango_Restore_Context();

  return (lval_result);
}


/******************************************************************************
 * (send <tango-widget> :LOAD_COLOR <color-str>)
 *	==> returns a FIXNUM representing a new TANGO_COLOR pixel value.
 ******************************************************************************/
LVAL Xtango_Widget_Class_Method_LOAD_COLOR()
{
  LVAL self;
  char* color_str;
  Widget widget_id;
  TANGO_COLOR tcolor;

  widget_id = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&self);
  color_str = getstring(xlgastring());
  xllastarg();

  Xtango_Save_Set_Context_From_WidgetID(widget_id);
  Xtango_Check_Initialized();

  /*
   * note that for Xtango_Load_Color_Else_Error() is a special version of
   * TANGOload_color() which will signal an error when Xtango runs out of
   * colors, or when XAllocNamedColor() fails. For monochrome systems,
   * this will only issue a warning to *error-output*.
   */
  tcolor = Xtango_Load_Color_Else_Error(color_str);

  Xtango_Restore_Context();
  
  return (cvfixnum((FIXTYPE) tcolor));
}


/******************************************************************************
 * (send <tango-widget> :SET_BGCOLOR <color-str>)
 *	==> returns NIL.
 ******************************************************************************/
LVAL Xtango_Widget_Class_Method_SET_BGCOLOR()
{
  LVAL self;
  char* color_str;
  Widget widget_id;

  widget_id = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&self);
  color_str = getstring(xlgastring());
  xllastarg();

  Xtango_Save_Set_Context_From_WidgetID(widget_id);
  Xtango_Check_Initialized();

  TANGOset_bgcolor(color_str);

  Xtango_Restore_Context();
  
  return (NIL);
}


/******************************************************************************
 * (send <tango-widget> :GET_IMAGES [<visibility_kwd>])
 *	==> returns LIST of IMAGES
 *
 * [<visibility_kwd>] -- if absent, all the images are returned, both visible
 *	and invisible. If :VISIBLE, then only visible images are returned;
 *	if :INVISIBLE, then only invisible images are returned.
 ******************************************************************************/
typedef enum{ WINTERP_TANGO_ALL, WINTERP_TANGO_VISIBLE, WINTERP_TANGO_INVISIBLE } WINTERP_TIMAGE_VISIBILITY;
LVAL Xtango_Widget_Class_Method_GET_IMAGES()
{
  WINTERP_TIMAGE_VISIBILITY visibility;
  WINTERP_TANGO_CONTEXT context;
  IMAGE_PTR imageptr;
  TANGO_IMAGE image;
  LVAL self, lval_image, lval_result;
  Widget widget_id;

  /* get <self> */
  widget_id = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&self);

  /* get optional [<visibility_kwd>] */
  if (moreargs() && ((*xlargv == k_VISIBLE) || (*xlargv == k_INVISIBLE))) {
    if (nextarg() == k_VISIBLE)
      visibility = WINTERP_TANGO_VISIBLE;
    else
      visibility = WINTERP_TANGO_INVISIBLE;
  }
  else if (moreargs())
    xlbadtype(nextarg());
  else
    visibility = WINTERP_TANGO_ALL;

  xllastarg();

  xlsave1(lval_result);		/* protect from gc */
  lval_result = NIL;

  context = Xtango_Find_Context_From_WidgetID(widget_id);
  Xtango_Save_Set_Context(context);
  Xtango_Check_Initialized();

  switch (visibility) {
  case WINTERP_TANGO_ALL:
    for (imageptr = context->adata->confighead; imageptr; imageptr = imageptr->nexti) {
      image = imageptr->image;
      if (image->alive) {
	lval_image = (LVAL) image->tangoimageobj;
	if (lval_image && tangoimageobj_p(lval_image)) /* sanity check incase we retrieve an "internal" TANGO_IMAGE that wasn't created by WINTERP */
	  lval_result = cons(lval_image, lval_result);
	else {
	  Xtango_Restore_Context();
	  xlabort("Internal error -- Bad TANGOIMAGEOBJ (case WINTERP_TANGO_ALL).");
	}
      }
    }
    break;
  case WINTERP_TANGO_VISIBLE:
    for (imageptr = context->adata->confighead; imageptr; imageptr = imageptr->nexti) {
      image = imageptr->image;
      if ((image->visible) && (image->alive)) {
	lval_image = (LVAL) image->tangoimageobj;
	if (lval_image && tangoimageobj_p(lval_image)) /* sanity check incase we retrieve an "internal" TANGO_IMAGE that wasn't created by WINTERP */
	  lval_result = cons(lval_image, lval_result);
	else {
	  Xtango_Restore_Context();
	  xlabort("Internal error -- Bad TANGOIMAGEOBJ (case WINTERP_TANGO_VISIBLE).");
	}
      }
    }
    break;
  case WINTERP_TANGO_INVISIBLE:
    for (imageptr = context->adata->confighead; imageptr; imageptr = imageptr->nexti) {
      image = imageptr->image;
      if (!(image->visible) && (image->alive)) {
	lval_image = (LVAL) image->tangoimageobj;
	if (lval_image && tangoimageobj_p(lval_image)) /* sanity check incase we retrieve an "internal" TANGO_IMAGE that wasn't created by WINTERP */
	  lval_result = cons(lval_image, lval_result);
	else {
	  Xtango_Restore_Context();
	  xlabort("Internal error -- Bad TANGOIMAGEOBJ (case WINTERP_TANGO_INVISIBLE).");
	}
      }
    }
    break;
  }

  Xtango_Restore_Context();

  xlpop();
  return (lval_result);
}


/******************************************************************************
 * (send <tango-widget> :COLORS_STOREON)
 *	 ==> returns LIST of calls to method :LOAD_COLOR, each element of
 *	     the list must be evaluated.
 *
 * Note: this should be used prior to 'eval'ing results of :STOREON on any
 * TANGO:IMAGE_CLASS subclasses. 
 *
 * LIST format:
 * (
 *  (send tango_w :load_color "red")
 *  (send tango_w :load_color "black")
 *  (send tango_w :load_color "blue")
 *  ...
 *  )
 ******************************************************************************/
LVAL Xtango_Widget_Class_Method_COLORS_STOREON()
{
  LVAL self, result, load_elt, load_form;
  Widget widget_id;
  char* color_str;
  int color;

  widget_id = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&self);
  xllastarg();

  xlstkcheck(3);
  xlsave(result);
  xlsave(load_elt);
  xlsave(load_form);

  Xtango_Save_Set_Context_From_WidgetID(widget_id);
  Xtango_Check_Initialized();

  for (color = 0; (color_str = Xtango_TANGO_COLOR_To_Color_String(color)); color++) {
    load_elt = cvstring(color_str);
    load_form = cons(load_elt,		NIL);
    load_form = cons(k_LOAD_COLOR,	load_form);
    load_form = cons(s_TANGO_W,		load_form);
    load_form = cons(s_SEND,		load_form);
    result = cons(load_form, result);
  }

  Xtango_Restore_Context();

  /* restore the stack */
  xlpopn(3);

  return (result);
}


/******************************************************************************
 * (send <tango-widget> :COPY_TO_2D_BITMAP_ARRAY <x> <y> <width> <height>)
 * 
 *	 ==> returns ARRAY of TANGO_COLOR values.
 *	 #(#( <0,0>		... <width-1,0> )
 *            .
 *            .
 *            .
 *	   #( <0,height-1>	... <width-1,height-1>)
 *	   )
 *
 * <x> <y> <width> and <height> are FLONUM's representing the area of the
 * <tango-widget> to copy into a 2D_BITMAP_ARRAY. The size of the returned
 * array is the integer size represented by those args.

 * USAGE: (send <tango-w> :COPY_TO_2D_BITMAP_ARRAY 0.0 0.0 1.0 1.0)
 * returns a bitmap representing all the images in <tango-w> (where
 * values 0.0,1.0 represent default initial :PAN/:ZOOM factors.
 * To reset <tango-widget> to defaults prior to calling this routine,
 * evaluate the followingt: (send <tango-widget> :SET_COORD 0.0 1.0 1.0 0.0)
 *
 * NOTE: you should 'eval' the results of :COLORS_STOREON prior to 
 * creating a new bitmap image with the results of :COPY_TO_2D_BITMAP_ARRAY.
 ******************************************************************************/
LVAL Xtango_Widget_Class_Method_COPY_TO_2D_BITMAP_ARRAY()
{
  LVAL self, result;
  Widget widget_id;
  int win_x, win_y, win_width, win_height;
  double x, y, width, height;

  widget_id = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&self);
  x = (double) getflonum(xlgaflonum());
  y = (double) getflonum(xlgaflonum());
  width = (double) getflonum(xlgaflonum());
  height = (double) getflonum(xlgaflonum());
  xllastarg();

  Xtango_Save_Set_Context_From_WidgetID(widget_id);
  Xtango_Check_Initialized();

  win_x		= XPIXEL(TANGO__data->x_WIN_COORD_to_int, x);
  win_y		= YPIXEL(TANGO__data->y_WIN_COORD_to_int, y);
  win_width	= XPIXEL(TANGO__data->x_WIN_COORD_to_int, width);
  win_height	= YPIXEL(TANGO__data->y_WIN_COORD_to_int, height);

  if (win_x < 0)
    win_x = 0;
  if (win_y < 0)
    win_y = 0;
  if ((win_x > TANGO__data->width) || (win_y > TANGO__data->height)) {
    win_width = 0;
    win_height = 0;
  }
  else if ((win_width <= 0) || (win_height <= 0)) {
    win_width = 0;
    win_height = 0;
  }
  else {
    if ((win_x + win_width) > TANGO__data->width)
      win_width = TANGO__data->width - win_x;
    if ((win_y + win_height) > TANGO__data->height)
      win_height = TANGO__data->height - win_y;
  }

  if ((win_width == 0) || (win_height == 0))
    result = NIL;
  else
    result = Xtango_Pixmap_To_Lisp_2D_Array(TANGO__data->pixmap,
					    win_x, win_y,
					    win_width, win_height);

  Xtango_Restore_Context();
  
  return (result);
}


/******************************************************************************
 * Fetches and removes a WIDGETOBJ from the argument stack, returning
 * the widgetID. If the WIDGETOBJ has been destroyed or is not initialized,
 * then this will signal an error. If the WIDGETOBJ isn't a TANGO:WIDGET_CLASS
 * instance, also signal an error.
 * 
 * <wobj_return> is a pointer to an LVAL,
 * it returns the WIDGETOBJ retrieved from the argument stack.
 ******************************************************************************/
static LVAL o_TANGO_WIDGET_CLASS;
#include "xlisp/xlobj.h" /* for SUPERCLASS */
Widget Xtango_Get_TANGO_WIDGETOBJ_Returning_Validated_WidgetID(wobj_return)
     LVAL *wobj_return;
{
  Widget widget_id;
  LVAL o_class;

  widget_id = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(wobj_return);

  /* loop through superclasses, stopping at TANGO_WIDGET_CLASS, or NIL */
  o_class = getclass(*wobj_return);
  while ((o_class != o_TANGO_WIDGET_CLASS) && (o_class != NIL)) {
    o_class = getivar(o_class, SUPERCLASS);
  }

  if (o_class == o_TANGO_WIDGET_CLASS)
    return (widget_id);		/* we have a TANGO:WIDGET_CLASS instance -- return widgetID */
  else				/* signal error */
    xlerror("Bad argument type - expected TANGO:WIDGET_CLASS instance", *wobj_return);
}


/******************************************************************************
 *
 ******************************************************************************/
Wc_Xtango_Init()
{
  o_TANGO_WIDGET_CLASS =
    Wcls_Create_Subclass_Of_WIDGET_CLASS("TANGO:WIDGET_CLASS",
					 xmDrawingAreaWidgetClass); /* this parameter isn't actually used anywhere in this particular subclass */

  xladdmsg(o_TANGO_WIDGET_CLASS, ":ADD_CALLBACK",
           FTAB_Xtango_Widget_Class_Method_ADD_CALLBACK);
  xladdmsg(o_TANGO_WIDGET_CLASS, ":SET_CALLBACK",
           FTAB_Xtango_Widget_Class_Method_SET_CALLBACK);
  xladdmsg(o_TANGO_WIDGET_CLASS, ":ISNEW", 
	   FTAB_Xtango_Widget_Class_Method_ISNEW);
  xladdmsg(o_TANGO_WIDGET_CLASS, ":BEGIN_DRAWING", 
	   FTAB_Xtango_Widget_Class_Method_BEGIN_DRAWING);
  xladdmsg(o_TANGO_WIDGET_CLASS, ":PAN", 
	   FTAB_Xtango_Widget_Class_Method_PAN);
  xladdmsg(o_TANGO_WIDGET_CLASS, ":ZOOM", 
	   FTAB_Xtango_Widget_Class_Method_ZOOM);
  xladdmsg(o_TANGO_WIDGET_CLASS, ":SET_COORD", 
	   FTAB_Xtango_Widget_Class_Method_SET_COORD);
  xladdmsg(o_TANGO_WIDGET_CLASS, ":INQ_COORD", 
	   FTAB_Xtango_Widget_Class_Method_INQ_COORD);
  xladdmsg(o_TANGO_WIDGET_CLASS, ":SET_ANIMATION_EVENT_PROCESSING", 
	   FTAB_Xtango_Widget_Class_Method_SET_ANIMATION_EVENT_PROCESSING);
  xladdmsg(o_TANGO_WIDGET_CLASS, ":SET_DELAY", 
	   FTAB_Xtango_Widget_Class_Method_SET_DELAY);
  xladdmsg(o_TANGO_WIDGET_CLASS, ":SET_DEBUG", 
	   FTAB_Xtango_Widget_Class_Method_SET_DEBUG);
  xladdmsg(o_TANGO_WIDGET_CLASS, ":MONO_PATTERN_REPRESENTATION", 
	   FTAB_Xtango_Widget_Class_Method_MONO_PATTERN_REPRESENTATION);
  xladdmsg(o_TANGO_WIDGET_CLASS, ":REFRESH", 
	   FTAB_Xtango_Widget_Class_Method_REFRESH);
  xladdmsg(o_TANGO_WIDGET_CLASS, ":INPUT_COORD", 
	   FTAB_Xtango_Widget_Class_Method_INPUT_COORD);
  xladdmsg(o_TANGO_WIDGET_CLASS, ":INPUT_IMAGE", 
	   FTAB_Xtango_Widget_Class_Method_INPUT_IMAGE);
  xladdmsg(o_TANGO_WIDGET_CLASS, ":GET_EVENT_IMAGE", 
	   FTAB_Xtango_Widget_Class_Method_GET_EVENT_IMAGE);
  xladdmsg(o_TANGO_WIDGET_CLASS, ":GET_EVENT_COORD", 
	   FTAB_Xtango_Widget_Class_Method_GET_EVENT_COORD);
  xladdmsg(o_TANGO_WIDGET_CLASS, ":LOAD_COLOR", 
	   FTAB_Xtango_Widget_Class_Method_LOAD_COLOR);
  xladdmsg(o_TANGO_WIDGET_CLASS, ":SET_BGCOLOR", 
	   FTAB_Xtango_Widget_Class_Method_SET_BGCOLOR);
  xladdmsg(o_TANGO_WIDGET_CLASS, ":GET_IMAGES", 
	   FTAB_Xtango_Widget_Class_Method_GET_IMAGES);
  xladdmsg(o_TANGO_WIDGET_CLASS, ":COLORS_STOREON", 
	   FTAB_Xtango_Widget_Class_Method_COLORS_STOREON);
  xladdmsg(o_TANGO_WIDGET_CLASS, ":COPY_TO_2D_BITMAP_ARRAY", 
	   FTAB_Xtango_Widget_Class_Method_COPY_TO_2D_BITMAP_ARRAY);

  /* for Xtango_Widget_Class_Method_ISNEW() */
  k_BUTTON = xlenter(":BUTTON");

  /* for Xtango_Widget_Class_Method_PAN() */
  k_UP		= xlenter(":UP");
  k_DOWN	= xlenter(":DOWN");
  k_LEFT	= xlenter(":LEFT");
  k_RIGHT	= xlenter(":RIGHT");

  /* for Xtango_Widget_Class_Method_ZOOM() */
  k_IN		= xlenter(":IN");
  k_OUT		= xlenter(":OUT");

  /* for Xtango_Widget_Class_Method_MONO_PATTERN_REPRESENTATION() */
  k_COLORS	= xlenter(":COLORS");
  k_FILLS	= xlenter(":FILLS");

  /* for Xtango_Widget_Class_Method_GET_IMAGES() and all Tango_*_Image_Class_Method_ISNEW() */
  k_VISIBLE	= xlenter(":VISIBLE");
  k_INVISIBLE	= xlenter(":INVISIBLE");

  /* for all Tango_*_Image_Class_Method_ISNEW() methods */
  k_SHOW	= xlenter(":SHOW");

  /* for all Tango_*_Image_Get_Values() routines */
  s_SEND	= xlenter("SEND");
  k_NEW		= xlenter(":NEW"); /* yeah, this is defined in xlobj.c, but... */
  s_TANGO_W	= xlenter("*TANGO_WIDGET*"); /* dummy name used by :STOREON... */
  setsvalue(s_TANGO_W, NIL);	/* declare this a special variable so we can set it via PROGV */
  k_LOAD_COLOR  = xlenter(":LOAD_COLOR");


  /* for Tango_Image_Class_Method_TAP_COLOR(), Xtango_Prim_TANGO_PATH_COLOR() */
  {
    LVAL s_TANGO_COLOR_WHITE	= xlenter("TANGO_COLOR_WHITE");
    LVAL s_TANGO_COLOR_YELLOW	= xlenter("TANGO_COLOR_YELLOW");
    LVAL s_TANGO_COLOR_GREEN	= xlenter("TANGO_COLOR_GREEN");
    LVAL s_TANGO_COLOR_BLUE	= xlenter("TANGO_COLOR_BLUE");
    LVAL s_TANGO_COLOR_ORANGE	= xlenter("TANGO_COLOR_ORANGE");
    LVAL s_TANGO_COLOR_RED	= xlenter("TANGO_COLOR_RED");
    LVAL s_TANGO_COLOR_MAROON	= xlenter("TANGO_COLOR_MAROON");
    LVAL s_TANGO_COLOR_BLACK	= xlenter("TANGO_COLOR_BLACK");

    defconstant(s_TANGO_COLOR_WHITE,	cvfixnum((FIXTYPE) TANGO_COLOR_WHITE));
    defconstant(s_TANGO_COLOR_YELLOW,	cvfixnum((FIXTYPE) TANGO_COLOR_YELLOW));
    defconstant(s_TANGO_COLOR_GREEN,	cvfixnum((FIXTYPE) TANGO_COLOR_GREEN));
    defconstant(s_TANGO_COLOR_BLUE,	cvfixnum((FIXTYPE) TANGO_COLOR_BLUE));
    defconstant(s_TANGO_COLOR_ORANGE,	cvfixnum((FIXTYPE) TANGO_COLOR_ORANGE));
    defconstant(s_TANGO_COLOR_RED,	cvfixnum((FIXTYPE) TANGO_COLOR_RED));
    defconstant(s_TANGO_COLOR_MAROON,	cvfixnum((FIXTYPE) TANGO_COLOR_MAROON));
    defconstant(s_TANGO_COLOR_BLACK,	cvfixnum((FIXTYPE) TANGO_COLOR_BLACK));
  }
}

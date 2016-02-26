/*
 * Copyright (c) 1992 The Regents of the University of Texas System.
 * Copyright (c) 1993 The Regents of the University of Texas System.
 * Copyright (c) 1994 The Regents of the University of Texas System.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted provided
 * that the above copyright notice and this paragraph are duplicated in all
 * such forms and that any documentation, advertising materials,  and other
 * materials  related to such  distribution  and use  acknowledge  that the
 * software  was  developed  by the  University of Texas.  The  name of the
 * University may not be  used to endorse or promote  products derived from
 * this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char copyright[] =
"@(#) Copyright (c) 1993 The Regents of the University of Texas System.\n\
 All rights reserved.\n";
static char rcsid[] =
"$Id: ListSW.c,v 1.1 1994/03/14 18:55:53 jones Exp $\n";
static char rcspath[] =
"$Source: /mintaka/u0/xx/ss/jones/jones.old/mftp.src/moxftp-2.0/RCS/ListSW.c,v $\n";
#endif

/* $Log: ListSW.c,v $
 * Revision 1.1  1994/03/14  18:55:53  jones
 * Initial revision
 *
 */

#include <stdio.h>
#include <ctype.h>

#include <X11/StringDefs.h>
#include <X11/IntrinsicP.h>

#if defined(MOTIF)
#include <Xm/Xm.h>
#include <Xm/XmP.h>
#include <Xm/Label.h>
#include <Xm/ScrollBar.h>
#else
#if defined(XAW3D)
#include <X11/Xaw3d/XawInit.h>
#include <X11/Xaw3d/Scrollbar.h>
#include <X11/Xaw3d/Label.h>
#else
#if defined(XAW)
#include <X11/Xaw/XawInit.h>
#include <X11/Xaw/Scrollbar.h>
#include <X11/Xaw/Label.h>
#else
#include <Xol/OpenLookP.h>
#include <Xol/Manager.h>
#include <Xol/ManagerP.h>
#include <Xol/Scrollbar.h>
#include <Xol/StaticText.h>
#endif
#endif
#endif

#include "ListSWP.h"
#include "List.h"

static Widget CreateVBar();
static Widget CreateHBar();
static Widget Createlabel();
static void CreateLabel();
static void CreateVScrollBar();
static void CreateHScrollBar();
static void PositionVScrollBar();
static void PositionHScrollBar();
static void CreateList();
static void Scroll_list_vb_cb();
#if defined(OPENWINDOW)
static Widget register_focus();
#endif



/****************************************************************
 *
 * Full class record constant
 *
 ****************************************************************/

/* Private Data */

#define offset(field) XtOffset(MyListSWWidget, field)

static XtResource resources[] = {
#if defined(MOTIF)
    {XmNshadowThickness, XmCShadowThickness, XmRHorizontalDimension,
        sizeof (Dimension), 
	XtOffset (MyListSWWidget, manager.shadow_thickness),
     	XmRImmediate,  (caddr_t) 2 },
#endif
#if defined(XAW)||defined(OPENWINDOW)
    {XtNfont,  XtCFont, XtRFontStruct, sizeof(XFontStruct *),
        offset(listsw.font),XtRString, XtDefaultFont},
#else
    {XmNfontList, XmCFontList, XmRFontList, sizeof(XmFontList),
        offset(listsw.font),  XmRImmediate, (caddr_t) NULL},
#endif
    {XtNlabelw, XtCBoolean, XtRBoolean,  sizeof(Boolean),
        offset(listsw.labelw), XtRImmediate, (XtPointer) False},
};

static void Initialize();
static void Resize();
static void Redisplay();
static void Exposex();
static void Destroy();
static void ChangeManaged();
static void ClassInitialize();
static Boolean SetValues();

static void Realize();

static XtGeometryResult 
ToBad(w, request, reply)
Widget w;
XtWidgetGeometry * request;
XtWidgetGeometry * reply;
{
    return XtGeometryNo;
}

MyListSWClassRec MylistSWClassRec = {
  {
/* core_class fields */	
#if defined(MOTIF)
#define superclassx		(&xmManagerClassRec)
#else
#if defined(XAW)
#define superclassx             (&compositeClassRec)
#else
#if defined(OPENWINDOW)
#define superclassx             (&managerClassRec)
#endif
#endif
#endif
    /* superclass	  	*/	(WidgetClass) superclassx,
    /* class_name	  	*/	"MyListSW",
    /* widget_size	  	*/	sizeof(MyListSWRec),
    /* class_initialize   	*/	ClassInitialize,
    /* class_part_initialize	*/	NULL,
    /* class_inited       	*/	FALSE,
    /* initialize	  	*/	Initialize,
    /* initialize_hook		*/	NULL,
    /* realize		  	*/	Realize,
    /* actions		  	*/	NULL,
    /* num_actions	  	*/	0,
    /* resources	  	*/	resources,
    /* num_resources	  	*/	XtNumber(resources),
    /* xrm_class	  	*/	NULLQUARK,
    /* compress_motion	  	*/	TRUE,
    /* compress_exposure  	*/	FALSE,
    /* compress_enterleave	*/	TRUE,
    /* visible_interest	  	*/	FALSE,
    /* destroy		  	*/	Destroy,
    /* resize		  	*/	Resize,
    /* expose		  	*/	Redisplay,
    /* set_values	  	*/	SetValues,
    /* set_values_hook		*/	NULL,
    /* set_values_almost	*/	XtInheritSetValuesAlmost,
    /* get_values_hook		*/	NULL,
    /* accept_focus	 	*/	XtInheritAcceptFocus,
    /* version			*/	XtVersion,
    /* callback_private   	*/	NULL,
    /* tm_table		   	*/	NULL,
    /* query_geometry		*/      NULL,
    /* disp acceletor	        */	NULL,
    /* extension                */      NULL,
  },
  { /* composite_class fields */
    /* geometry_manager   */    ToBad,
    /* change_managed     */    ChangeManaged,
    /* insert_child       */    XtInheritInsertChild,
    /* delete_child       */    XtInheritDeleteChild,
    /* extension          */    NULL
  },

#if defined(MOTIF)
/* Constraint class Init */
  {
      NULL, 
      0,      
      NULL,
      NULL,
      NULL,
      NULL,
      NULL
  },
/* Manager Class */
   {
      XtInheritTranslations,  		/* translations        */
      NULL,                            	/* get resources          */
      NULL,                  		/* num get_resources      */
      NULL,                             /* get_cont_resources     */
      0,                                /* num_get_cont_resources */
      XmInheritParentProcess,           /* parent_process         */
      NULL,                             /* extension           */
   },
#endif
#if defined(OPENWINDOW)
/* Constraint class Init */
  {
      NULL, 
      0,      
      NULL,
      NULL,
      NULL,
      NULL,
      NULL
  },
/* Manager Class */
  {
      XtInheritHighlightHandler,
      NULL,
      NULL,
      XtInheritTraversalHandler,
      XtInheritActivateFunc,
      NULL,
      0,
      register_focus,
      NULL,
      OlVersion,
      NULL,
      NULL,
   },
#endif
   {
    /* empty              */    0,
   }
};

WidgetClass MylistSWWidgetClass = (WidgetClass)&MylistSWClassRec;


static void
ChangeManaged(w)
Widget w;
{
      MyListSWWidget lw = (MyListSWWidget) w;    
 
      if (lw->listsw.wait) return;
      Resize(w);
}
/****************************************************************
 *
 * Private Procedures
 *
 ****************************************************************/

static void ClassInitialize(w)
    Widget w;
{
#if defined(XAW)
    XawInitializeWidgetSet();
#endif
}

static void
Realize( w, valueMask, attributes )
Widget w;
Mask *valueMask;
XSetWindowAttributes *attributes;
{
  MyListSWWidget lw = (MyListSWWidget) w;    
  int i;
  
 
  (*MylistSWClassRec.core_class.superclass->core_class.realize)
    (w, valueMask, attributes);
}

/*	Function Name: Initialize
 *	Description: Function that initilizes the widget instance.
 *	Arguments: junk - NOT USED.
 *                 new  - the new widget.
 *	Returns: none
 */

/* ARGSUSED */
static void 
Initialize(junk, new)
Widget junk, new;
{
    MyListSWWidget lw = (MyListSWWidget) new;
    Widget parent;

    lw->listsw.resizeing = 0;
    lw->listsw.allowresize = 0;
 
#if defined(MOTIF)
    if (lw->listsw.font == NULL) {
        lw->listsw.font = _XmGetDefaultFontList ((Widget)lw,XmTEXT_FONTLIST);
        lw->listsw.font = XmFontListCopy(lw->listsw.font);
    }
    _XmFontListGetDefaultFont(lw->listsw.font, &lw->listsw.fs);
    if (!lw->listsw.fs)  {
        XtAppWarning(XtWidgetToApplicationContext(new), "No font list");
        return;
    }
#else
    lw->listsw.fs = lw->listsw.font;
#endif

/* 
 * Initialize all private resources.
 */
    lw->listsw.hbar  = NULL;
    lw->listsw.vbar  = NULL;
    lw->listsw.List  = NULL;
    lw->listsw.label = NULL;
    lw->listsw.wait = 0;
    if (lw->listsw.labelw) CreateLabel((Widget)lw);
    CreateVScrollBar((Widget)lw);
    CreateHScrollBar((Widget)lw);
    CreateList(lw);
/*
    _OlUpdateTraversalWidget(lw, NULL, NULL, TRUE);
*/
} 

/*	Function Name: Redisplay
 *	Description: Repaints the widget window on expose events.
 *	Arguments: w - the list widget.
 *                 event - the expose event for this repaint.
 *                 junk - NOT USED.
 *	Returns: 
 */

/* ARGSUSED */
static void 
Redisplay(w, event, junk)
Widget w;
XEvent *event;
Region junk;
{
    MyListSWWidget lw = (MyListSWWidget) w;
 
#if defined(MOTIF)
    int ht;
    int x,y;

    Resize(w);
    if (!XtIsRealized(lw)) return;
    if (!lw->listsw.List) return;
    ht = lw->manager.shadow_thickness;
    x  = lw->listsw.list_x - ht;
    y  = lw->listsw.list_y - ht;
    _XmDrawShadow (XtDisplay (lw), XtWindow (lw),
                   lw->manager.bottom_shadow_GC,
                   lw->manager.top_shadow_GC,
                   lw->manager.shadow_thickness,
                   x,
                   y,
                   lw->listsw.List->core.width  + (ht * 2),
                   lw->listsw.List->core.height + (ht * 2));
#else
    Resize(w);
#endif
     

}

static void
Resize(w)
Widget w;
{
    MyListSWWidget lw = (MyListSWWidget) w;

    if (!XtIsRealized(w)) return;
    if(lw->listsw.resizeing && !lw->listsw.allowresize) return;
    lw->listsw.resizeing++;
    PositionVScrollBar(w);
    PositionHScrollBar(w);
    Positionlabel(lw);
    PositionList(lw);
/*
    if (XtIsRealized((Widget)lw)) {
   	XClearArea(XtDisplay(lw),
  		   XtWindow(lw->listsw.List),
                   0,
                   0,
                   0,
                   0,
	     	   TRUE);
    }
*/
    XSync(XtDisplay(lw), FALSE);
    lw->listsw.resizeing = 0;
}

/*
 * Set specified arguments into widget
 */

static Boolean 
SetValues(current, request, new)
Widget current, request, new;
{
    MyListSWWidget cl = (MyListSWWidget) current;
    MyListSWWidget rl = (MyListSWWidget) request;
    MyListSWWidget nl = (MyListSWWidget) new;
    Boolean redraw = FALSE;

    if ((cl->core.sensitive != rl->core.sensitive) ||
	(cl->core.ancestor_sensitive != rl->core.ancestor_sensitive)) {
	redraw = TRUE;
    }
    
    if (!XtIsRealized(current))
      return(FALSE);
      
    return(redraw);
}

static void Destroy(w)
    Widget w;
{
    MyListSWWidget lw = (MyListSWWidget) w;
    
}


static void
CreateList(lw)
MyListSWWidget lw;
{
  lw->listsw.List = 
	XtCreateManagedWidget("list", MylistWidgetClass, (Widget)lw, NULL, 0);
  XtAddCallback(lw->listsw.List, 
		XtNscrollcallback, Scroll_list_vb_cb, (XtPointer)lw );
}

Positionlabel(lw)
MyListSWWidget lw;
{
  Dimension bw;
  Widget    label;

  label =  lw->listsw.label;
  if (label == NULL) return;
  bw = label->core.border_width;
  XtResizeWidget( label, lw->listsw.label_w, lw->listsw.label_h, bw);
  XtMoveWidget( label, lw->listsw.label_x, lw->listsw.label_y );
}

PositionList(lw)
MyListSWWidget lw;
{
  Dimension bw;
  Widget    list;

  list =  lw->listsw.List;
  if (list == NULL) return;
  bw = list->core.border_width;
  XtResizeWidget( list, lw->listsw.list_w, lw->listsw.list_h, bw);
  XtMoveWidget( list, lw->listsw.list_x, lw->listsw.list_y );
}


static void
CreateHScrollBar(w)
Widget w;
{
  MyListSWWidget lw = ( MyListSWWidget ) w;

  if (lw->listsw.hbar != NULL) return;
  lw->listsw.hbar =  CreateHBar(lw);
}

static void
CreateVScrollBar(w)
Widget w;
{
  MyListSWWidget lw = ( MyListSWWidget ) w;

  if (lw->listsw.vbar != NULL) return;
  lw->listsw.vbar =  CreateVBar((Widget)lw);
}

static void
CreateLabel(w)
Widget w;
{
  MyListSWWidget lw = ( MyListSWWidget ) w;
  if (lw->listsw.label != NULL) return;
  lw->listsw.label =  Createlabel((Widget)lw);
}


/*      Function Name: PositionHScrollBar.
 *      Description: Positions the Horizontal scrollbar.
 *      Returns: none.
 */

static void
PositionHScrollBar(w)
Widget w;
{
    MyListSWWidget lw = ( MyListSWWidget ) w;
    Widget hbar;
    Widget vbar;
    Dimension bw;
    Dimension st = 0;
    int height = 0;

    height = lw->core.height;
    if (lw->listsw.label) {
	int label_h;
	label_h = compute_label_height(lw);
	height -=  label_h;
    }
    hbar = lw->listsw.hbar;
    vbar = lw->listsw.vbar;
    if (vbar) bw   = lw->listsw.vbar->core.border_width;

#if defined(MOTIF)
    st = lw->manager.shadow_thickness;
#endif
    if (hbar && XtIsRealized((Widget)hbar) && XtIsManaged((Widget)hbar)) {
#if defined(MOTIF)
	if (vbar && XtIsRealized((Widget)vbar) && XtIsManaged((Widget)vbar)) {
	    lw->listsw.hbar_w = -bw + lw->core.width - vbar->core.width; 
  	    lw->listsw.hbar_h = hbar->core.height;

  	    lw->listsw.hbar_x =  -bw;
  	    lw->listsw.hbar_y = -bw + height - hbar->core.height;

  	    lw->listsw.vbar_h -= hbar->core.height;

  	    lw->listsw.list_h -= hbar->core.height;

            if (lw->listsw.list_h == 0) lw->listsw.list_h = 1;
            XtResizeWidget( vbar, lw->listsw.vbar_w, lw->listsw.vbar_h, bw);
            XtMoveWidget( vbar, (Position)lw->listsw.vbar_x,
                                (Position)lw->listsw.vbar_y);
            XtResizeWidget( hbar, lw->listsw.hbar_w, lw->listsw.hbar_h, bw);
            XtMoveWidget( hbar, (Position)lw->listsw.hbar_x, 
	  		        (Position)lw->listsw.hbar_y);
	} else {
	    lw->listsw.hbar_w = -bw + lw->core.width; 
  	    lw->listsw.hbar_h = hbar->core.height;

  	    lw->listsw.hbar_x =  -bw;
  	    lw->listsw.hbar_y = -bw + height - hbar->core.height;

  	    lw->listsw.list_h -= hbar->core.height;
            if (lw->listsw.list_h == 0) lw->listsw.list_h = 1;
            XtResizeWidget( hbar, lw->listsw.hbar_w, lw->listsw.hbar_h, bw);
            XtMoveWidget( hbar, (Position)lw->listsw.hbar_x, 
	  		        (Position)lw->listsw.hbar_y);
	}
#else
	if (vbar && XtIsRealized((Widget)vbar) && XtIsManaged((Widget)vbar)) {
	    lw->listsw.hbar_w  = -bw + lw->core.width - vbar->core.width; 
  	    lw->listsw.hbar_h  = vbar->core.width;

  	    lw->listsw.list_h -= lw->listsw.hbar_h +
				 lw->listsw.List->core.border_width;
            if (lw->listsw.list_h == 0) lw->listsw.list_h = 1;
#if defined(OPENWINDOW)
  	    lw->listsw.hbar_x  = lw->listsw.List->core.border_width;
  	    lw->listsw.hbar_y  = lw->listsw.list_h + 
				 2*lw->listsw.List->core.border_width;
#else
  	    lw->listsw.hbar_x  = lw->listsw.list_x;
  	    lw->listsw.hbar_y  = lw->listsw.list_h;
#endif
  	    lw->listsw.vbar_h -= lw->listsw.hbar_h;

            XtResizeWidget( vbar, lw->listsw.vbar_w, lw->listsw.vbar_h, bw);
            XtMoveWidget( vbar, (Position)lw->listsw.vbar_x,
                                (Position)lw->listsw.vbar_y);
            XtResizeWidget( hbar, lw->listsw.hbar_w, lw->listsw.hbar_h, bw);
            XtMoveWidget( hbar, (Position)lw->listsw.hbar_x, 
	  		        (Position)lw->listsw.hbar_y);
	} else {
	    lw->listsw.hbar_w = -bw + lw->core.width; 
  	    lw->listsw.hbar_h = vbar->core.width;

  	    lw->listsw.hbar_x =  -bw;
  	    lw->listsw.hbar_y = -bw + height - lw->listsw.hbar_h;

  	    lw->listsw.list_h -= lw->listsw.hbar_h;
            if (lw->listsw.list_h == 0) lw->listsw.list_h = 1;
            XtResizeWidget( hbar, lw->listsw.hbar_w, lw->listsw.hbar_h, bw);
            XtMoveWidget( hbar, (Position)lw->listsw.hbar_x, 
	  		        (Position)lw->listsw.hbar_y);
	}
#endif
    } 
}

/*      Function Name: PositionVScrollBar.
 *      Description: Positions the Vertical scrollbar.
 *      Arguments: ctx - the text widget.
 *      Returns: none.
 */

static void
PositionVScrollBar(w)
Widget w;
{
    MyListSWWidget lw = ( MyListSWWidget ) w;
    Widget vbar;
    Widget hbar;
    Dimension bw = 0;
    Dimension st = 0;
    int height = 0;

    height = lw->core.height;
    vbar = lw->listsw.vbar;
    hbar = lw->listsw.hbar;
    if (vbar) bw  = lw->listsw.vbar->core.border_width;
    if (lw->listsw.label) {
	int label_h;
        label_h =  compute_label_height(lw);
        height -=  label_h;
        lw->listsw.label_w = lw->core.width;
  	lw->listsw.label_h = label_h;
  	lw->listsw.label_x = -bw;
  	lw->listsw.label_y = height;
    }

#if defined(MOTIF)
    st = lw->manager.shadow_thickness;
#endif
    if (vbar && XtIsRealized((Widget)vbar) && XtIsManaged((Widget)vbar)) {
#if defined(MOTIF)
	lw->listsw.vbar_w = vbar->core.width;
  	lw->listsw.vbar_h = -bw + height;
  	lw->listsw.vbar_x = -bw + lw->core.width - vbar->core.width;
  	lw->listsw.vbar_y = -bw;
  	lw->listsw.list_x = -bw + st; 
  	lw->listsw.list_w = -bw - st + lw->core.width  - lw->listsw.list_x 
			    - vbar->core.width;
  	lw->listsw.list_y = -bw + st;
  	lw->listsw.list_h = -bw - st + height - lw->listsw.list_y ; 
#else
	lw->listsw.vbar_w = vbar->core.width;
  	lw->listsw.vbar_h = height;
#if defined(OPENWINDOW)
  	lw->listsw.vbar_x = lw->core.width - vbar->core.width;
  	lw->listsw.list_x =  0; 
#else
  	lw->listsw.vbar_x = -bw;
  	lw->listsw.list_x =  vbar->core.width + 1;
#endif
#if defined(OPENWINDOW)
        lw->listsw.vbar_y  = lw->listsw.List->core.border_width;
#else
  	lw->listsw.vbar_y = -bw;
#endif
  	lw->listsw.list_w = lw->core.width
			    - 2*lw->listsw.List->core.border_width 
			    - vbar->core.width - vbar->core.border_width;
  	lw->listsw.list_y = 0;
  	lw->listsw.list_h = height;
#endif
	if (hbar && XtIsRealized((Widget)hbar) && XtIsManaged((Widget)hbar))
		return;
        XtResizeWidget( vbar, lw->listsw.vbar_w, lw->listsw.vbar_h, bw);
        XtMoveWidget( vbar, (Position)lw->listsw.vbar_x, 
			    (Position)lw->listsw.vbar_y);
    } else {
#if defined(MOTIF)
  	lw->listsw.list_x = -bw + st; 
  	lw->listsw.list_w = -bw - st + lw->core.width - lw->listsw.list_x; 
  	lw->listsw.list_y = -bw + st;
  	lw->listsw.list_h = -bw - st + height - lw->listsw.list_y ; 
#else
  	lw->listsw.list_x = 0; 
  	lw->listsw.list_w = lw->core.width; 
  	lw->listsw.list_y = 0;
  	lw->listsw.list_h = height;
#endif
    }
}


#if defined(MOTIF)

static void
ScrollUpDownProc(w, closure, call_data)
Widget w;
XtPointer closure;
XmScrollBarCallbackStruct *call_data;
{
    MyListSWWidget lw = ( MyListSWWidget )closure;
    Widget child = lw->listsw.List;
    
    if (child == NULL) return;  /* no child to scroll. */
    
    set_list_yoffset(child, call_data->value);
}

static void
ScrollRightLeft(w, closure, call_data)
Widget w;
XtPointer closure;
XmScrollBarCallbackStruct *call_data;
{
    MyListSWWidget lw = ( MyListSWWidget )closure;
    Widget child = lw->listsw.List;

    if (child == NULL) return;  /* no child to scroll. */

    set_list_xoffset(child, call_data->value);
}



static Widget
CreateHBar(w)
Widget w;
{
    Widget x;
    Arg arg[1];

 
    XtSetArg(arg[0],  XmNorientation, (XtArgVal)XmHORIZONTAL);
    x = XtCreateManagedWidget("vScrollbar", xmScrollBarWidgetClass, (Widget)w, 
			arg, 1);

#define RL (XtCallbackProc)ScrollRightLeft
    XtAddCallback( x, XmNvalueChangedCallback,  RL, (XtPointer)w);
    XtAddCallback( x, XmNincrementCallback,     RL, (XtPointer)w);
    XtAddCallback( x, XmNdecrementCallback,     RL, (XtPointer)w);
    XtAddCallback( x, XmNpageIncrementCallback, RL, (XtPointer)w);
    XtAddCallback( x, XmNpageDecrementCallback, RL, (XtPointer)w);
    XtAddCallback( x, XmNtoTopCallback,         RL, (XtPointer)w);
    XtAddCallback( x, XmNtoBottomCallback,      RL, (XtPointer)w);
    XtAddCallback( x, XmNdragCallback,          RL, (XtPointer)w);
#undef RL
    XtUnmanageChild(x);
    return x;
}

static Widget
CreateVBar(w)
Widget w;
{
    Widget x;
    Arg Args[100];

    x = XtCreateManagedWidget("vScrollbar", xmScrollBarWidgetClass, (Widget)w, 
			NULL, 0);
#define UD (XtCallbackProc)ScrollUpDownProc
    XtAddCallback( x, XmNvalueChangedCallback,  UD, (XtPointer)w);
    XtAddCallback( x, XmNincrementCallback,     UD, (XtPointer)w);
    XtAddCallback( x, XmNdecrementCallback,     UD, (XtPointer)w);
    XtAddCallback( x, XmNpageIncrementCallback, UD, (XtPointer)w);
    XtAddCallback( x, XmNpageDecrementCallback, UD, (XtPointer)w);
    XtAddCallback( x, XmNtoTopCallback,         UD, (XtPointer)w);
    XtAddCallback( x, XmNtoBottomCallback,      UD, (XtPointer)w);
    XtAddCallback( x, XmNdragCallback,          UD, (XtPointer)w);
#undef UD
    XtUnmanageChild(x);
    return x;
}

static Widget
Createlabel(w)
Widget w;
{
    Widget x;
    x = XtCreateManagedWidget("items",  xmLabelWidgetClass, (Widget)w, NULL, 0);
    return x;
}


setThumb(w, itop, ishown, length, inc, page)
Widget w;
int    itop;
int    ishown;
int    length;
int    inc;
int    page;
{
    Arg args[5];
    int n = 0;

    XtSetArg(args[n], XmNmaximum       , length); n++;
    XtSetArg(args[n], XmNvalue         , itop  ); n++;
    XtSetArg(args[n], XmNsliderSize    , ishown); n++;
    XtSetArg(args[n], XmNpageIncrement , page  ); n++;
    XtSetArg(args[n], XmNincrement     , inc   ); n++;
    XtSetValues(w, args, n);
}

setThumPos(w, pos, max)
Widget w;
int    pos;
MyListSWWidget max;
{
    Arg arg[1];
    
    XmScrollBarCallbackStruct t;
    t.reason = 0;
    t.event  = NULL;
    t.value  = pos;
    t.pixel  = 0;
    XtSetArg(arg[0], XmNvalue, pos);
    XtSetValues(w, arg, 1);
    XtCallCallbacks(w, XmNvalueChangedCallback, &t);
}
#endif

#if defined(XAW)
static  XtCallbackProc
ScrolljumpUpDown(w, closure, call_data)
Widget w;
XtPointer closure;
XtPointer call_data;
{
    MyListSWWidget lw = ( MyListSWWidget )closure;
    Widget child = lw->listsw.List;
    float  percent = *(float *) call_data;
    int pos;

    if (child == NULL) return;  /* no child to scroll. */

    if (w == lw->listsw.hbar) {
        percent = (percent * lw->listsw.hbar_max);
        pos = (percent+.5);
        if (pos > (lw->listsw.hbar_max - lw->listsw.hbar_page)) 
	   pos = lw->listsw.hbar_max - lw->listsw.hbar_page;
        set_list_xoffset(child, pos);
    } else if (w == lw->listsw.vbar) {
        percent = (percent * lw->listsw.vbar_max);
        pos = (percent+.5);
        if (pos > (lw->listsw.vbar_max - lw->listsw.vbar_page)) 
	     pos = lw->listsw.vbar_max - lw->listsw.vbar_page;
        set_list_yoffset(child, pos);
    } else {
	return;
    }
}

static XtCallbackProc
ScrollUpDown(w, closure, call_data)
Widget w;
XtPointer closure;
XtPointer call_data;
{
    MyListSWWidget lw = ( MyListSWWidget )closure;
    Widget child = lw->listsw.List;
    int pix = (int)call_data;
    int pos = 0;
    
    if (child == NULL) return;  /* no child to scroll. */

    if (w == lw->listsw.hbar) {
        pos = lw->listsw.hbar_x - pix;
        pos = (int)(pix*lw->listsw.hbar_max)/(int)lw->listsw.hbar->core.width;
        if (pos < 0) {
            pos += lw->listsw.hbar_max;
        }
        if (pos < 0) pos = 0;
        else if (pos >= (lw->listsw.hbar_max - lw->listsw.hbar_page)) 
		pos = lw->listsw.hbar_max - lw->listsw.hbar_page;
        setThumPos(w, pos, lw->listsw.hbar_max);
    } else if (w == lw->listsw.vbar) {
        pos = lw->listsw.vbar_y - pix;
        pos = (int)(pix*lw->listsw.vbar_max)/(int)lw->listsw.vbar->core.height;
        if (pos < 0) {
            pos += lw->listsw.vbar_max;
        }
        if (pos < 0) pos = 0;
        else if (pos > (lw->listsw.vbar_max - lw->listsw.vbar_page)) 
		pos = lw->listsw.vbar_max - lw->listsw.vbar_page;
        setThumPos(w, pos, lw->listsw.vbar_max);
    } else {
	return;
    }
}

static Widget
Createlabel(w)
Widget w;
{
    Widget x;
    x = XtCreateManagedWidget("items", labelWidgetClass, (Widget)w, NULL, 0);
    return x;
}


static Widget
CreateHBar(w)
Widget w;
{
    Widget x;
    Arg arg[1];

    XtSetArg(arg[0],  XtNorientation, (XtArgVal)XtorientHorizontal);
    x = XtCreateManagedWidget("hScrollbar", scrollbarWidgetClass, (Widget)w, 
	arg, 1);
    XtAddCallback(x, XtNscrollProc, (XtCallbackProc)ScrollUpDown,    
	          ( XtPointer)w );
    XtAddCallback(x, XtNjumpProc  , (XtCallbackProc)ScrolljumpUpDown, 
		  (XtPointer)w );
    XtUnmanageChild(x);
    return x;
}

static Widget
CreateVBar(w)
Widget w;
{
    Widget x;

    x = XtCreateManagedWidget("vScrollbar", scrollbarWidgetClass, 
	(Widget)w, NULL, 0);
    XtAddCallback(x, XtNscrollProc, (XtCallbackProc)ScrollUpDown,    
	          (XtPointer)w );
    XtAddCallback(x, XtNjumpProc  , (XtCallbackProc)ScrolljumpUpDown, 
		  (XtPointer)w );
    XtUnmanageChild(x);
    return x;
}

setThumb(w, itop, ishown, length, inc, page)
Widget w;
int    itop;
int    ishown;
int    length;
int    inc;
int    page;
{
    Arg args[5];
    int n = 0;
    float top;
    float shown;

    top = (float)itop/(float)length;
    shown = (float)ishown/(float)length;
    XawScrollbarSetThumb(w, top, shown);
}

setThumPos(w, pos, max)
Widget w;
int    pos;
int    max;
{
    float call_data;
    float shown = -1.0;

    call_data = (float)pos/(float)max;
    XawScrollbarSetThumb(w, call_data, shown);
    XtCallCallbacks(w, XtNjumpProc, (XtPointer)&call_data );
}

#endif

#if defined(OPENWINDOW)
static Widget
register_focus(w)
Widget w;
{
    return w;
}

void
slidermoved(w, closure, call_data)
Widget w;
XtPointer closure;
XtPointer call_data;
{
    MyListSWWidget lw = ( MyListSWWidget )closure;
    Widget child = lw->listsw.List;
    OlScrollbarVerify *cb = (OlScrollbarVerify *)call_data;

    if (w == lw->listsw.vbar) {
	set_list_yoffset(child, cb->new_location);
    } else {
	set_list_xoffset(child, cb->new_location);
    }
}

static Widget
Createlabel(w)
Widget w;
{
    Widget x;
    Arg args[3];
    int n = 0;

    XtSetArg(args[n],  XtNgravity,   CenterGravity); n++;
    XtSetArg(args[n],  XtNalignment, OL_CENTER)    ; n++;
    XtSetArg(args[n],  XtNborderWidth, 1)	   ; n++;

    x = XtCreateManagedWidget("items", staticTextWidgetClass,
           (Widget)w, args, n);
    return x;
	
}
    
static Widget
CreateVBar(w)
Widget w;
{
    Widget x;

    x = XtCreateManagedWidget("vScrollbar", scrollbarWidgetClass,
           (Widget)w, NULL, 0);
    XtAddCallback( x, XtNsliderMoved, slidermoved,   (XtPointer)w );
    XtUnmanageChild(x);
    return x;
}

static Widget
CreateHBar(w)
Widget w;
{
    Widget x;
    Arg arg[1];

    XtSetArg(arg[0],  XtNorientation, (XtArgVal)OL_HORIZONTAL);
    x = XtCreateManagedWidget("vScrollbar", scrollbarWidgetClass,
           (Widget)w, arg, 1);
    XtAddCallback( x, XtNsliderMoved, slidermoved,   (XtPointer)w );
    XtUnmanageChild(x);
    return x;
}

setThumPos(w, pos, max)
Widget w;
int    pos;
int    max;
{
    Arg arg[1];
    OlScrollbarVerify cb;

    XtSetArg(arg[0],  XtNsliderValue , pos);
    XtSetValues(w, arg, 1);
    cb.new_location = pos;
    XtCallCallbacks(w, XtNsliderMoved, &cb);
}
     
setThumb(w, itop, ishown, length, inc, page)
Widget w;
int    itop;
int    ishown;
int    length;
int    inc;
int    page;
{
    Arg args[10];
    int n = 0;

    XtSetArg(args[n], XtNsliderValue           , itop  ); n++;
    XtSetArg(args[n], XtNsliderMax             , length); n++;
    XtSetArg(args[n], XtNproportionLength      , page  ); n++;
    XtSetArg(args[n], XtNgranularity           , inc   ); n++;
    XtSetValues(w, args, n);
    return;
}
#endif

static void
Scroll_list_vb_cb(w, closure, unused)
Widget  w;
char    *closure;
caddr_t unused;
{
    MyXawListScrollReturnStruct *cb = (MyXawListScrollReturnStruct *)unused;
    MyListSWWidget lw = ( MyListSWWidget )closure;
    Widget vbar;
    Widget hbar;
    Widget u_children[2];
    Widget m_children[2];
    Cardinal u_num_children = 0;
    Cardinal m_num_children = 0;
    int value = 0;

    if (!XtIsRealized((Widget)lw)) return;
    vbar = lw->listsw.vbar;
    hbar = lw->listsw.hbar;
    if (!vbar || !hbar) return;

    switch(cb->reason) {
	case SCROLL_SET:
	 	switch (cb->type) {
    	    	     case SCROLL_TYPE_V:
		     case SCROLL_TYPE_H:
			cb->resize =  0;
	    	    	if (!cb->page_y) {
			    set_list_yoffset(lw->listsw.List, 0);
			    if (XtIsManaged(vbar)) {
			        		
			   	u_children[u_num_children] = vbar;
			    	u_num_children++;
			    }
			} else {
 	    	            if (!XtIsManaged(vbar)) {
			   	m_children[m_num_children] = vbar;
			    	m_num_children++;
			    }
			}
			if (!cb->page_x) {
			    set_list_xoffset(lw->listsw.List, 0);
			    if(XtIsManaged(hbar)) {
			    	u_children[u_num_children] = hbar;
			    	u_num_children++;
			    } 	
			} else {
			    if(!XtIsManaged(hbar)) {
			    	m_children[m_num_children] = hbar;
			    	m_num_children++;
			    } 	
			}
  			lw->listsw.wait = m_num_children;
			lw->listsw.allowresize++;
			if (u_num_children)
			    XtUnmanageChildren(u_children, u_num_children);
			lw->listsw.wait = 0;
			if (m_num_children)
			    XtManageChildren (m_children, m_num_children);
			lw->listsw.allowresize = 0;
  			if (m_num_children || u_num_children) {
				cb->resize++;
				return;
			}
	                lw->listsw.vbar_max  = cb->max_y;
	                lw->listsw.vbar_page = cb->page_y;
			if (cb->page_y)
	    	            setThumb(vbar, 0, lw->listsw.vbar_page, 
				     lw->listsw.vbar_max, 1, 
				     lw->listsw.vbar_page);
                        lw->listsw.hbar_max  = cb->max_x;
                        lw->listsw.hbar_page = cb->page_x;
			if (cb->page_x)
                             setThumb(hbar, 0, lw->listsw.hbar_page,
                                      lw->listsw.hbar_max, 1, 
				      lw->listsw.hbar_page);
		    default:
			return;
		}
    	case  SCROLL_MOVE:
	 	switch (cb->type) {
    	    	    case  SCROLL_TYPE_V:
	   		if (!XtIsManaged(vbar) || cb->value < 0) return;
	   		value =  cb->value;
	   		if (value > 
			  (lw->listsw.vbar_max - lw->listsw.vbar_page)) 
		      	    value = lw->listsw.vbar_max - lw->listsw.vbar_page;
	    		setThumPos(vbar, value, lw->listsw.vbar_max);
	    		return;
	       	    case  SCROLL_TYPE_H:
                        if (!XtIsManaged(hbar) || cb->value < 0)  return;
                        value =  cb->value;
                        if (value >
                          (lw->listsw.hbar_max - lw->listsw.hbar_page))
                            value = lw->listsw.hbar_max - lw->listsw.hbar_page;
                        setThumPos(hbar, value, lw->listsw.hbar_max);
                        return;

		    default:
			return;
		}
    }
}

compute_label_height(lw)
MyListSWWidget lw;
{
   double height;

   height = lw->listsw.fs->max_bounds.ascent + 
	    lw->listsw.fs->max_bounds.descent;
   height = height*1.6;
   return ((int)height);
}

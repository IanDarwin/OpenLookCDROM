/* -*-C-*-
********************************************************************************
*
* File:         Arc.c
* RCS:		$Header: /users/npm/src/winterp/src-server/widgets/RCS/Arc.c,v 2.1 1994/06/06 15:48:06 npm Exp $
* Description:  An Arc widget for use with the XmGraph widget.
* Author:       Doug Young dayoung@hplabs, Luis Miguel luis@postgres.berkeley.edu
* Created:
* Modified:     Sun May 29 22:51:21 1994 (Niels Mayer) npm@indeed
* Language:     C
* Package:      N/A
* Status:       Experimental (Do Not Distribute)
*
* Copyright (c) 1988 by Hewlett-Packard Company
* 
* Permission to use, copy, modify, and distribute this software 
* and its documentation for any purpose and without fee is hereby 
* granted, provided that the above copyright notice appear in all 
* copies and that both that copyright notice and this permission 
* notice appear in supporting documentation, and that the name of 
* Hewlett-Packard not be used in advertising or publicity pertaining 
* to distribution of the software without specific, written prior 
* permission.
*
********************************************************************************
*/

static char rcs_identity[] = "XmArc Version 2.1 @(#)$Header: /users/npm/src/winterp/src-server/widgets/RCS/Arc.c,v 2.1 1994/06/06 15:48:06 npm Exp $";

/*
 * Notes: arc->arc.visible is a flag used to minimize redrawing. It indicates whether
 * the arc has been drawn (TRUE) or is currently thought to be erased. If arc->arc.visible
 * is FALSE, it is not visible on the screen, if TRUE it *may* be visible, because of exposures, etc.
 *
 * arc->core.visible indicates whether or not the arc is currently in the visible
 * portion of the graph, when in a scrolled window.
 */

/* CYY modified 2/1/91 for  R3 and R4 compatible and ANSI_C fixes*/
/* CYY modified 11/19/92 for  R5 and SESD compatible*/
#define ARC_VERSION 11-19-92

#define SESD 1

#ifndef _HPUX_SOURCE
#define _HPUX_SOURCE
#endif

#include <math.h>
#include <stdio.h>

#include <Xm/XmP.h>
#if ((defined (XtSpecificationRelease)) && (XtSpecificationRelease >= 5))
#define _R5_
#else
#undef _R5_
#endif

#if ((defined (XtSpecificationRelease)) && (XtSpecificationRelease >= 4))
#define _R4_
#else
#undef _R4_
#endif

#include <X11/Xutil.h>


#if (XmREVISION==2)
#define RX XtX
#define RY XtY
#define RWidth XtWidth
#define RHeight XtHeight
#include <Xm/ManagerP.h>
#include <Xm/GadgetP.h>
#endif

#include "ArcP.h"
#include "GraphP.h"


/* CYY
#include "Graph.h"
#include "Arc.h"
*/

#define MAXDIM          2000
#define ABS(a)         (((a) < (0.0)) ? -(a) : (a))
#define LENGTH(dx,dy)  (sqrt ( (double) (dx) * (dx) + (dy) * (dy)))
#define ROUND(A)       ( (A) < 0 ? (int) ((A) - 0.5) : (int) ((A) + 0.5))

#ifndef _NO_PROTO
#ifdef _R5_
static void 	GetValuesHook	(Widget w,ArgList args, Cardinal *num_args);
#endif
static void    AddArcLineToList (XmGraphWidget, GC,int,int,int,int);
static void    Arm   		(XmArcWidget, XEvent *);
static void    Activate 	(XmArcWidget, XEvent *);
static void    Destroy  	(XmArcWidget);
static void    Disarm 		(XmArcWidget, XEvent *);
static void    DispatchInput 	(XmArcWidget, XEvent *, Mask);
static void    Enter 		(XmArcWidget, XEvent *);
static void    Initialize    	(Widget,Widget);
static void    Leave 		(XmArcWidget, XEvent *);
static void    Redisplay 	(XmArcWidget, XEvent *, Region);
static Boolean SetValues 	(Widget, Widget, Widget);
static void    bezier    	(int, XPoint *, float,float,float,
						float,float,float);
static void    clear_arrow 	(XmArcWidget, int,int,int, int);
static void    draw_arrow  	(XmArcWidget, int,int,int, int);

#ifndef _R4_
static void    ClassPartInitialize (WidgetClass);
static Boolean _XmStringIsXmString (XmString);
#endif /* _R4_ */


#else /* _NO_PROTO */
#ifdef _R5_
static void 	GetValuesHook();
#endif

static void    Initialize();
static void    DispatchInput();
static void    bezier();
static void    Redisplay();
static Boolean SetValues();
static void    Destroy();
static void    draw_arrow();
static void    clear_arrow ();
static void Arm ();
static void Disarm ();
static void Activate ();
static void Leave ();
static void Enter ();
static void AddArcLineToList ();
#ifndef _R4_
static void    ClassPartInitialize();
static Boolean _XmStringIsXmString ();
#endif /* _R4_ */

#endif /* _NO_PROTO */



/****************************************************************
 *
 * Full class record constant
 *
 ****************************************************************/

/* Arc Resources List */

static XtResource resources[] = {
{
    XmNto, XmCTo, XmRPointer, sizeof(caddr_t),
    XtOffset(XmArcWidget, arc.to), XmRPointer, NULL
    },

{
    XmNfrom, XmCFrom, XmRPointer, sizeof(caddr_t),
    XtOffset(XmArcWidget, arc.from), XmRPointer, NULL
    },

{
    XmNarcDirection,  XmCDirection, XmRArcDirection, sizeof(unsigned char),
    XtOffset(XmArcWidget, arc.direction), XmRString, "undirected"
    },

{
    XmNforeground,  XmCForeground, XmRPixel, sizeof(Pixel),
    XtOffset(XmArcWidget, arc.foreground), XmRString, "Black"
    },

{
    XmNhighlightColor,  XmCHighlightColor, XmRPixel, sizeof(Pixel),
    XtOffset(XmArcWidget, arc.highlightcolor), XmRString, "White"
    },

{
    XmNhighlight,  XmCHighlight, XmRBoolean, sizeof(Boolean),
    XtOffset(XmArcWidget, arc.highlight), XmRString, "False"
    },

{
    XmNdelta,  XmCDelta, XmRInt, sizeof(int),
    XtOffset(XmArcWidget, arc.delta), XmRString, "5"
    },

{
    XmNfontList,  XmCFontList, XmRFontList, sizeof(XmFontList),

#ifdef _R4_ 
    XtOffset(XmArcWidget, arc.font), XmRImmediate, NULL
#else
#ifdef _R5_
    XtOffset(XmArcWidget, arc.font), XmRImmediate, NULL
#else
/* R3 */
    XtOffset(XmArcWidget, arc.font), XmRString, "fixed"
#endif
#endif
    },

{
    XmNlabelString,  XmCXmString, XmRXmString, sizeof(_XmString),
    XtOffset(XmArcWidget, arc.label), XmRImmediate, NULL
    },

{
    XmNmapLabel,  XmCMapLabel, XmRBoolean, sizeof(Boolean),
    XtOffset(XmArcWidget, arc.map_name), XmRString, "False"
    },

{
    XmNarcWidth, XmCArcWidth, XmRInt, sizeof(int),
    XtOffset(XmArcWidget, arc.width), XmRString, "0"
    },

{
    XmNstyle, XmCStyle, XmRLineStyle, sizeof(unsigned char),
    XtOffset(XmArcWidget, arc.style), XmRString, "LineSolid"
    },

{
    XmNcapStyle, XmCCapStyle, XmRCapStyle, sizeof(unsigned char),
    XtOffset(XmArcWidget, arc.cap_style), XmRString, "CapNotLast"
    },

{
    XmNdashes, XmCDashes, XmRInt, sizeof(int),
    XtOffset(XmArcWidget, arc.dashes), XmRString, "4"
    },

{
    XmNdashOffset, XmCDashOffset, XmRInt, sizeof(int),
    XtOffset(XmArcWidget, arc.dash_offset), XmRString, "0"
    },

{
    XmNarmCallback, XmCCallback, XmRCallback, sizeof(caddr_t),
    XtOffset(XmArcWidget, arc.arm_callback), XmRPointer, (caddr_t) NULL
    },

{
    XmNdisarmCallback, XmCCallback, XmRCallback, sizeof(caddr_t),
    XtOffset(XmArcWidget, arc.disarm_callback), XmRPointer, (caddr_t) NULL
    },

{
    XmNactivateCallback, XmCCallback, XmRCallback, sizeof(caddr_t),
    XtOffset(XmArcWidget, arc.activate_callback), XmRPointer, (caddr_t) NULL
    },

{
    XmNarcEditedCallback, XmCCallback, XmRCallback, sizeof(caddr_t),
    XtOffset(XmArcWidget, arc.arc_edited), XmRPointer, (caddr_t) NULL
    },
{
    XmNuserData, XmCUserData, XmRPointer, sizeof(caddr_t),
    XtOffset(XmArcWidget, arc.user_data), XmRPointer, (caddr_t) NULL
    }
};


externaldef(xmarcclassrec) XmArcClassRec xmArcClassRec = {
  {
    /* core_class fields */
    /* superclass 		*/    (WidgetClass) &widgetClassRec,
    /* class_name		*/    "XmArc",
    /* widget_size	  	*/	sizeof(XmArcRec),
    /* class_initialize   	*/	NULL,
#ifdef _R4_
    /* class_part_init    	*/	NULL,
#else
    /* class_part_init    	*/	ClassPartInitialize,
#endif
    /* class_inited       	*/	FALSE,
    /* initialize	  	*/	(XtInitProc)Initialize,
    /* initialize_hook		*/	NULL,
    /* realize		  	*/	XtInheritRealize,
    /* actions		  	*/	NULL,
    /* num_actions	  	*/	0,
    /* resources	  	*/	resources,
    /* num_resources	  	*/	XtNumber(resources),
    /* xrm_class	  	*/	NULLQUARK,
    /* compress_motion	  	*/	TRUE,
    /* compress_exposure  	*/	TRUE,
    /* compress_enterleave	*/	TRUE,
    /* visible_interest	  	*/	FALSE,
    /* destroy		  	*/	(XtWidgetProc)Destroy,
    /* resize		  	*/	NULL,
    /* expose		  	*/	(XtExposeProc)Redisplay,
    /* set_values	  	*/	(XtSetValuesFunc)SetValues,
    /* set_values_hook		*/	NULL,
    /* set_values_almost	*/	XtInheritSetValuesAlmost,
#ifdef _R5_
    /* get_values_hook		*/	GetValuesHook,
#else
    /* get_values_hook		*/	NULL,
#endif
    /* accept_focus	 	*/	NULL,
    /* version			*/	XtVersion,
    /* callback_private   	*/	NULL,
    /* tm_table		   	*/	NULL,
    /* query_geometry		*/	NULL,
    /* disp accelerator         */	NULL,
    /* extension                */	NULL,
   },
   {
#ifdef _R4_
    /* input_dispatch */               (XmWidgetDispatchProc) DispatchInput,
#else
    /* input_dispatch */               (XtWidgetProc) DispatchInput,
#endif
   }

};

externaldef(xmarcwidgetclass)
WidgetClass xmArcWidgetClass = (WidgetClass) &xmArcClassRec;


int _sibling_rank
#ifndef _NO_PROTO
  (XmArcWidget arc)
#else
  (arc) XmArcWidget arc;
#endif
{
  int  i, n_arcs = 0;
  XmGraphWidget  graph = (XmGraphWidget) XtParent(arc);

  if (arc->arc.siblings == NULL ||
      (graph->graph.siblings_visible != TRUE) ||
      ((Widget) arc->arc.siblings->arcs[0]) == arc->core.self)
    return 0;  /* no siblings, or up in front */

  for (i = 0; ((Widget) arc->arc.siblings->arcs[i]) !=  arc->core.self; i++)
/*    if(!(arc->arc.siblings->arcs[i]->core.being_destroyed))  */
      n_arcs++;
  return n_arcs;
}


/************************************************************************
 *
 *  Destroy
 *	Clean up allocated resources when the widget is destroyed.
 *
 ************************************************************************/
static void Destroy
#ifndef _NO_PROTO
  (XmArcWidget arc)
#else
  (arc) XmArcWidget  arc;
#endif
{
  Widget  arcW =( Widget)  arc;

  arc->arc.to = NULL;
  arc->arc.from = NULL;

  XtReleaseGC (arcW, arc->arc.gc);
  XtReleaseGC (arcW, arc->arc.highlight_gc);
  XtReleaseGC (arcW, arc->arc.clear_gc);

  if (arc->arc.region)
      XDestroyRegion (arc->arc.region);

  if (arc->arc.font)
      XmFontListFree(arc->arc.font);

  if (arc->arc.label)
    _XmStringFree(arc->arc.label);

  arc->arc.label = NULL;

  XtFree((char *)(arc->arc.siblings));
  arc->arc.siblings = NULL;

  XtRemoveAllCallbacks (arcW, XmNarmCallback);
  XtRemoveAllCallbacks (arcW, XmNdisarmCallback);
  XtRemoveAllCallbacks (arcW, XmNactivateCallback);
  XtRemoveAllCallbacks (arcW, XmNarcEditedCallback);
}

#ifndef _R4_
/************************************************************************
 *
 *  ClassPartInitialize
 *     Set up the fast subclassing for the widget
 *
 ************************************************************************/
static void ClassPartInitialize
#ifndef _NO_PROTO
  (WidgetClass wc)
#else
  (wc) WidgetClass wc;
#endif
{
  _XmFastSubclassInit (wc, XmARC_BIT);
}


static Boolean _XmStringIsXmString
#ifndef _NO_PROTO
  (XmString p1)
#else
  (p1) XmString p1;
#endif
{ char *p;
  p = (char *) p1;
  return (*p == XmSTRING_COMPOUND_STRING);
}
#endif

/*************************************<->*************************************
 *
 *  Initialize
 *
 *************************************<->***********************************/
static void Initialize
#ifndef _NO_PROTO
  (Widget request_w, Widget new_w)
#else
  (request_w, new_w) Widget request_w, new_w;
#endif
{ XmArcWidget new =  (XmArcWidget) new_w;
  XGCValues       values;
  XtGCMask        valueMask;
  Widget          graph = XtParent(new);
  Dimension w, h;
  short           myindex;
  XFontStruct     *fs;

  new->core.width  = 1;
  new->core.height = 1;
  new->core.x = 0;
  new->core.y = 0;
  new->core.border_width = 0;

  new->arc.region     = NULL;
  new->arc.siblings   = NULL;
  new->arc.up_to_date = FALSE;
  new->arc.visible    = FALSE;
  new->arc.armed      = FALSE;
  new->core.visible   = TRUE;
  new->arc.from_x     = new->arc.from_y = new->arc.to_x = new->arc.to_y = 0;


    if (new->arc.font == NULL)
    {

#ifdef _R4_
/* _XmGetDefaultFontList() is not implemented in _R3_ Motif1.0 */
	XmFontList defaultFont;

	defaultFont = _XmGetDefaultFontList(new_w, XmLABEL_FONTLIST);

	new->arc.font = XmFontListCopy (defaultFont);
#endif

    }
    else      /* Make a local copy of the font list */
    {
	new->arc.font = XmFontListCopy( new->arc.font);
    }

  /*
   * Copy the name into our space
   */

  if(new->arc.label == NULL)
  {

      XmString string;
#if (XmVersion >= 1002)		/* Motif 1.2 -- NPM mod */
      XmStringCharSet cset = (XmStringCharSet) XmFONTLIST_DEFAULT_TAG;
#else /* Motif 1.1 or 1.0 -- NPM mod */
      XmStringCharSet cset = (XmStringCharSet) XmSTRING_DEFAULT_CHARSET;
#endif /* WINTERP_MOTIF_12 -- NPM mod */

      string   =  XmStringLtoRCreate(XrmQuarkToString(new->core.xrm_name), cset);
      new->arc.label =  _XmStringCreate(string);
      XtFree((void *)string);


    }
    else
    {
	if (_XmStringIsXmString((XmString)(new->arc.label)))
	    new->arc.label = _XmStringCreate((XmString)(new->arc.label));
	else
	{
	    XmString string;
#if (XmVersion >= 1002)		/* Motif 1.2 -- NPM mod */
	    XmStringCharSet cset = (XmStringCharSet) XmFONTLIST_DEFAULT_TAG;
#else /* Motif 1.1 or 1.0 -- NPM mod */
	    XmStringCharSet cset = (XmStringCharSet) XmSTRING_DEFAULT_CHARSET;
#endif /* WINTERP_MOTIF_12 -- NPM mod */

	    _XmWarning (new_w, "Arc Widget expects a compound string");
	    string   =  XmStringLtoRCreate(XrmQuarkToString(new->core.xrm_name), cset);
	    new->arc.label =  _XmStringCreate(string);
	    XtFree((void *)string);

	}
    }

  _XmStringExtent (new->arc.font, new->arc.label, &w, &h);
  new->arc.labelwidth = w;
  new->arc.labelheight = h;

  valueMask = GCForeground | GCBackground | GCLineWidth | GCLineStyle |
    GCCapStyle | GCDashOffset | GCDashList | GCFont;

  values.foreground = new->arc.foreground;
  values.background = graph->core.background_pixel;
  values.line_width = new->arc.width;

  _XmFontListSearch(new->arc.font,
#if (XmVersion >= 1002) /* Motif 1.2 -- NPM mod */
		    XmFONTLIST_DEFAULT_TAG,
#else /* Motif 1.1 or 1.0 -- NPM mod */
		    XmSTRING_DEFAULT_CHARSET,
#endif /* WINTERP_MOTIF_12 -- NPM mod */
		    &myindex ,
		    &fs);

  if (fs==NULL)
      valueMask &= ~GCFont;
  else
      values.font = fs->fid;

  if (new->arc.style == 0)
    values.line_style = LineSolid;
  else
    values.line_style = new->arc.style;

  if (new->arc.cap_style == 0)
    values.cap_style = CapButt;
  else
    values.cap_style = new->arc.cap_style;

  values.dash_offset = new->arc.dash_offset;
  values.dashes = new->arc.dashes;
  new->arc.gc = XtGetGC ((Widget) graph, valueMask, &values);

  /*
   * Set the HIGHLIGHT graphic Context
   */

  values.foreground = new->arc.highlightcolor;
  new->arc.highlight_gc = XtGetGC((Widget) graph, valueMask, &values);

  /*
   * Set the CLEAR graphic Context
   */

  valueMask = GCForeground | GCBackground | GCFunction | GCLineWidth | GCFont;
  values.function   = GXcopy;
  values.foreground = graph->core.background_pixel;
  values.background = graph->core.background_pixel;

  /* restore the background pickmap if any  WJS 920103 */
  if (graph->core.background_pixmap != XmUNSPECIFIED_PIXMAP)
    { valueMask |= GCFillStyle | GCTile;
      values.foreground = ((XmGraphWidget) graph)->graph.foreground;
      values.fill_style = FillTiled;
      values.tile = graph->core.background_pixmap;
    }

  if (fs==NULL)
      valueMask &= ~GCFont;

  new->arc.clear_gc = XtGetGC((Widget) graph, valueMask, &values);

  if (new->arc.highlight)
    new->arc.current_gc = new->arc.highlight_gc;
  else
    new->arc.current_gc = new->arc.gc;

  if(new->arc.to && new->arc.from)
  { _SetupArcInternal (new);
  }
  else if(new->arc.to || new->arc.to)
  { XtWarning("You must specify both a to and from widget when creating an XmArcWidget");

  }
  if (XtClass (graph) != xmGraphWidgetClass)
  {
/* CYY  commented out and replace by simple return;
    XtError("Arc widget can only be managed by an XmGraph widget");
*/
    return;
  }
}

void _EraseArc
#ifndef _NO_PROTO
  (XmArcWidget arc)
#else
  (arc) XmArcWidget          arc;
#endif
{
  Widget      from = arc->arc.from,
              to = arc->arc.to;
  float       x1 = arc->arc.from_x,
              x2 = arc->arc.to_x,
              y1 = arc->arc.from_y,
              y2 = arc->arc.to_y;

  XmGraphWidget  graph = (XmGraphWidget) XtParent(arc);
  int            s_rank = arc->arc.rank;

  if(!to || !from || !XtIsRealized(graph))
    return;

  if(!arc->arc.visible)
    return;

  arc->arc.visible = FALSE;

  if (to == from)
    {
      int  x      = RX(to) + RWidth(to) / 2,
           y      = RY(to) - RHeight(to) / 2,
           width  = RWidth(to),
           height = 2 * RHeight(to),
           a1     = 146 * 64,
           a2     = -292 * 64;
      int  name_x, name_y, arrow1a_x, arrow1a_y, arrow1b_x, arrow1b_y,
           arrow2a_x, arrow2a_y, arrow2b_x, arrow2b_y;

      /*
       *  compute the arc parameters
       */

      switch (s_rank % 4)
	{
	case 3:
	  x = (int) (RX(to) + RWidth(to) *(6/10.0));
	  y = RY(to) - RHeight(to)/2;
	  width = RWidth(to);
	  height = 2 * RHeight(to);
	  a1 = 150 * 64;
	  a2 = -2 * a1;

#if SESD
	  name_x = x + width;
	  name_y = y + height/2;
#else
	  name_x = x + width - (arc->arc.map_name ? (arc->arc.labelwidth / 2) :  0);
	  name_y = y + height/2  - arc->arc.labelheight;
#endif

	  arrow1a_x = x + width/3;
	  arrow1a_y = y + height;
	  arrow1b_x = x + width/18.0;
	  arrow1b_y = y + height - height/4;
	  arrow2a_x = x + width/3;
	  arrow2a_y = y;
	  arrow2b_x = x + width/18.0;
	  arrow2b_y = y + height/4;
	  break;
	case 1:
	  x = (int) (RX(to) - 6/10.0 * RWidth(to));
	  y = RY(to) - RHeight(to)/2;
	  width = RWidth(to);
	  height = 2 * RHeight(to);
	  a1 = 30 * 64;
	  a2 = 326 * 64;

#if SESD
	  name_x = x;
	  name_y = y + height/2;
#else
	  name_x = x - (arc->arc.map_name ? (arc->arc.labelwidth / 2) :  0);
	  name_y = y + height/2 - arc->arc.labelheight;
#endif
	  arrow1a_x = x + (2*width)/3;
	  arrow1a_y = y + height;
	  arrow1b_x = x + width - width/18.0;
	  arrow1b_y = y + height - height/4;
	  arrow2a_x = x + (2*width)/3;
	  arrow2a_y = y;
	  arrow2b_x = x + width - width/18.0;
	  arrow2b_y = y + height/4;
	  break;
	case 0:
	  width = RWidth(to)/2;
	  height = RHeight(to) * 3/2;
	  x = RX(to) + width/2;
	  y = RY(to) - 2 * height/3;
	  a1 = -30 * 64;
	  a2 = 180 * 64 - 2 * a1;
	  name_x = x + width/2;
#if SESD
	  name_y = y;
#else
	  name_y = y - arc->arc.labelheight;
#endif

	  arrow1a_x = x + width;
	  arrow1a_y = y;
	  arrow1b_x = x + width - width/36;
	  arrow1b_y = RY(to);
	  arrow2a_x = x;
	  arrow2a_y = y;
	  arrow2b_x = x + width/36;
	  arrow2b_y = RY(to);
	  break;
	case 2:
	  width  = RWidth(to)/2;
	  height = RHeight(to) * 3/2;
	  x      = RX(to) + width/2;
	  y      = RY(to) + height/3;
	  a1     = 30 * 64;
	  a2     = -180 * 64 - 2 * a1;
	  name_x = x + width/2;
#if SESD
	  name_y = y + height;
#else
	  name_y = y + height - arc->arc.labelheight;
#endif

	  arrow1a_x = x + width;
	  arrow1a_y = y + height;
	  arrow1b_x = x + width - width/36;
	  arrow1b_y = RY(to) + RHeight(to);
	  arrow2a_x = x;
	  arrow2a_y = y + height;
	  arrow2b_x = x + width/36;
	  arrow2b_y = RY(to) + RHeight(to);
	  break;
	}
      /*
       * erase the arc
       */

      XDrawArc (XtDisplay(graph), XtWindow(graph),
		arc->arc.clear_gc, (int) x, (int) y, (int)width, (int)height, (int)a1 , (int)a2);

      /*
       * erase the arrow heads
       */

      if (arc->arc.direction == XmDIRECTED)
	{
	  clear_arrow (arc, (int)arrow1a_x, (int)arrow1a_y, (int) arrow1b_x, (int) arrow1b_y);
	}
      else if (arc->arc.direction == XmBIDIRECTED)
	{
	  clear_arrow (arc, (int)arrow1a_x, (int)arrow1a_y, (int)arrow1b_x, (int)arrow1b_y);
	  clear_arrow (arc, (int)arrow2a_x, (int)arrow2a_y, (int)arrow2b_x, (int)arrow2b_y);
	}

      /*
       * erase the name
       */

      if (arc->arc.map_name)
	{
	    _XmStringDraw (XtDisplay(arc), XtWindow(graph),
			   arc->arc.font, arc->arc.label,
			   arc->arc.clear_gc,
#if SESD
			   (int) name_x - arc->arc.labelwidth/2,
			   (int) name_y - arc->arc.labelheight/2,
#else
			   (int) name_x, (int) name_y,
#endif
			   arc->arc.labelwidth,
			   XmALIGNMENT_CENTER, XmSTRING_DIRECTION_L_TO_R, NULL);
	}
    }
  else
    { /* to != from */

      if (s_rank == 0)
	{
	  /*
	   * a straight line
	   */

#ifdef __sun
          /* 910918 mohammad - This is a workaround for the current problem
                               with the residual dots that appear when a node
                               is moved on the sun X server */
	  XGCValues values;
	  values.line_width = 5;
	  XChangeGC(XtDisplay(graph), arc->arc.clear_gc, GCLineWidth, &values);

#endif

	  if(graph->graph.batch_drawing_mode)
	    {
	      AddArcLineToList (graph, arc->arc.clear_gc, (int) x1, (int) y1, (int) x2, (int) y2);
	    }
	  else
	    XDrawLine (XtDisplay (graph), XtWindow (graph),
		       arc->arc.clear_gc, (int)x1, (int)y1, (int)x2, (int)y2);

	  if (arc->arc.direction == XmDIRECTED)
	    clear_arrow (arc, (int)x1, (int)y1, (int)x2, (int)y2);
	  else if (arc->arc.direction == XmBIDIRECTED)
	    {
	      clear_arrow (arc, (int) x1, (int) y1, (int) x2, (int) y2);
	      clear_arrow (arc, (int) x2, (int) y2, (int) x1, (int) y1);
	    }

	  if ( arc->arc.map_name)
	    _XmStringDraw (XtDisplay(arc), XtWindow(graph),
			   arc->arc.font, arc->arc.label,
			   arc->arc.clear_gc,
#if SESD
			   (int) ((x1 + x2 - arc->arc.labelwidth) / 2),
			   (int) ((y2 + y1 - arc->arc.labelheight) / 2),
#else
			   (int) ((x1 + x2) / 2),
			   (int) ((y2 + y1) / 2) - arc->arc.labelheight,
#endif
			   arc->arc.labelwidth,
			   XmALIGNMENT_CENTER, XmSTRING_DIRECTION_L_TO_R, NULL);
	}
      else
	{ /* rank > 0, a curved sibling */

	  XPoint curve [12];
	  int    mid_x, mid_y;
	  float  Xm, Ym, slope, displacement;
	  float  par_disp, perp_disp;
	  int    factor = 30; /* picked 30 heuristically */

	  mid_x = x1 + (x2 - x1) / 2;
	  mid_y = y1 + (y2 - y1) / 2;

	  /*
           * Check for slope == 0 or slope == +- infinity
	   */

	  if ((int) y1 == (int) y2)
	    slope = 0.01;
	  else if ((int) x1 == (int) x2)
	    slope = 100.0;
	  else
	    slope = (y1-y2)/(x1-x2);

	  /*
           * factor here comes with a value that determines
	   * the distance between siblings
	   */

	  {
	    double sqrt_arg = (factor * factor) + ((factor * factor) /
						   (MAX(s_rank,1) / (slope * slope)));
	    factor = sqrt (ABS(sqrt_arg));
	  }
	  if ((s_rank % 2) == 1) /* odd sibling */
	    factor = -(factor + (s_rank * factor));
	  else
	    factor = factor + ((s_rank-1) * factor);

	  displacement = y1 - (slope * x1);
	  par_disp = displacement + factor;
	  perp_disp = mid_y + (mid_x / slope);
	  Xm = (perp_disp - par_disp) / (slope + (1.0 / slope));
	  Ym = slope * Xm + par_disp;

	  /*
	   *  erase the arc
	   */
	  bezier (8, curve, x1, y1, Xm, Ym, x2, y2);
	  /* modified 10/7/91 -- TQP: XtWindow(arc) ==> XtWindow(graph) */
	  XDrawLines (XtDisplay (arc), XtWindow (graph),
		      arc->arc.clear_gc, curve, 9,
		      CoordModeOrigin);

	  /*
	   *  draw the arrow heads
	   */
	  if (arc->arc.direction == XmDIRECTED)
	    {
	      clear_arrow (arc, (int) Xm, (int) Ym, (int) x2, (int) y2);
	    }
	  else if (arc->arc.direction == XmBIDIRECTED)
	    {
	      clear_arrow (arc, (int) Xm, (int) Ym, (int) x2, (int) y2);
	      clear_arrow (arc, (int) Xm, (int) Ym, (int) x1, (int) y1);
	    }

	  /*
	    draw the name
	    */
	  if (arc->arc.map_name)
	    {
		_XmStringDraw (XtDisplay(arc), XtWindow(graph),
			       arc->arc.font, arc->arc.label,
			       arc->arc.clear_gc,
#if SESD
			       (int) curve[4].x - arc->arc.labelwidth/2,
			       (int) curve[4].y - arc->arc.labelheight/2,
#else
			       (int) curve[4].x,
			       (int) curve[4].y - arc->arc.labelheight,
#endif
			       arc->arc.labelwidth,
			       XmALIGNMENT_CENTER, XmSTRING_DIRECTION_L_TO_R, NULL);
	    }
	}
    }
}

/*********
 *
 * _GetPoints: Calculates the end points for Arcs.
 *            When arcDrawMode != PositionFixed, he points are
 *            drawn on the edge of the nodes, as if they went from the
 *            center of one to the center of the other one.

 *   Note: This routine is really weird. Shouldn't possibly be this complex
 *         to compute the mid points of two rectangles. The floating point
 *         should go as well. Doug Young
 *
 **********/
void _GetPoints
#ifndef _NO_PROTO
  (XmGraphWidget widget,
		 int f_x, int f_y, int f_width, int f_height,
		 int t_x, int t_y, int t_width, int t_height,
		 float *X1, float *Y1, float *X2, float *Y2)
#else
  (widget, f_x, f_y, f_width, f_height,
	   t_x, t_y, t_width, t_height, X1, Y1, X2, Y2)
     XmGraphWidget  widget;
     int      f_x, f_y, f_width, f_height, t_x, t_y, t_width, t_height;
     float    *X1, *Y1, *X2, *Y2;
#endif
{
  XmGraphWidget  parent = (XmGraphWidget) widget;
  float equis1, equis2, ye1, ye2, slope;
  float from_slope, to_slope;

  if (parent->graph.arc_draw_mode ==  XmPOSITION_FIXED)
    {
      if (parent->graph.direction == XmHORIZONTAL)
	{
	  *X1 = f_x + f_width;
	  *Y1 = f_y + (f_height / 2);
	  *X2 = t_x;
	  *Y2 = t_y + (t_height / 2);
	}
      else
	{
	  *X1 = f_x + (f_width / 2);
	  *Y1 = f_y + f_height;
	  *X2 = t_x + (t_width / 2);
	  *Y2 = t_y;
	}
      return;
    }

  /*
   *  This seemingly stupid way of calculating the slope of the line
   *  is done so the automatic type coercions will not affect the
   *  result too much
   */

  ye1    = f_y + (f_height / 2.0);
  ye2    = t_y + (t_height / 2.0);
  equis1 = f_x + (f_width / 2.0);
  equis2 = t_x + (t_width / 2.0);

  /* Check for slope == 0 or slope == +- infinity */

  if (((int) ye1) == ((int) ye2) && ((int) equis1) == ((int) equis2))
    {

      /*
       * Just set coords to center of each widget.
       */

      *X1 = f_x + (f_width  / 2);
      *Y1 = f_y + (f_height / 2);
      *X2 = t_x + (t_width  / 2);
      *Y2 = t_y + (t_height / 2);
      return;
    }
  else  if (((int) ye1) == ((int) ye2))
    {

      /*
       * Y coords are the same,  compute X coords
       */

      *Y1 = f_y + (f_height / 2);
      *Y2 = t_y + (t_height / 2);

      if(equis1 < equis2)
	{
	  *X2 = t_x ;
	  *X1 = f_x + f_width;
	}
      else
	{
	  *X2 = t_x + t_width;
	  *X1 = f_x;
	}
      return;
    }

  else if (((int) equis1) == ((int) equis2))
    {
      /*
       * X coords are the same,  compute Y coords
       */

      *X1 = f_x + (f_width / 2);
      *X2 = t_x + (t_width / 2);

      if(ye1 < ye2)
	{
	  *Y2 = t_y ;
	  *Y1 = f_y + f_height;
	}
      else
	{
	  *Y2 = t_y + t_height;
	  *Y1 = f_y;
	}
      return;
    }
  else
    slope = (ye1 - ye2) / (equis1 - equis2);

  /*
   * Get end points on from node
   */

  from_slope = (f_width) ? ((float) f_height / f_width) : 100.0 ;

  if (slope > 0.0)
    {
      if (equis1 < equis2)
	{ /* Quadrant 0 (lower, right) */
	  if (slope > from_slope)
	    {
	      *Y1 = f_y + f_height;
	      *X1 = (f_x + (f_width/2)) +
		(f_height / (2 * slope));
	    }
	  else
	    {
	      *X1 = f_x + f_width;
	      *Y1 = (f_y + (f_height/2)) + (slope * (f_width / 2));
	    }
	}
      else
	{ /* Quadrant 2 (upper, left) */
	  if (slope > from_slope)
	    {
	      *Y1 = f_y;
	      *X1 = (f_x + (f_width/2)) -  (f_height / (2 * slope));
	    }
	  else
	    {
	      *X1 = f_x;
	      *Y1 = (f_y + (f_height/2)) -  (slope * (f_width / 2));
	    }
	}
    }
  else
    { /* slope is <= 0.0 */
      if (equis1 < equis2)
	{ /* Quadrant 1 (upper right) */
	  if (slope > (-from_slope))
	    {
	      *X1 = f_x + f_width;
	      *Y1 = (f_y + (f_height/2)) +   (slope * (f_width / 2));
	    }
	  else
	    {
	      *Y1 = f_y;
	      *X1 = (f_x + (f_width/2)) - (f_height / (2 * slope));
	    }
	}
      else
	{ /* Quadrant 3 (lower, left) */
	  if (slope > (-from_slope))
	    {
	      *X1 = f_x;
	      *Y1 = (f_y + (f_height/2)) - (slope * (f_width / 2));
	    }
	  else
	    {
	      *Y1 = f_y + f_height;
	      *X1 = (f_x + (f_width/2)) + (f_height / (2 * slope));
	    }
	}
    }

  /*
   * Get end points on to node
   */
  to_slope = (t_width) ? ((float) t_height / t_width) : 100.0;
  if (slope > 0.0)
    {
      if (equis1 < equis2)
      { /* Quadrant 0 (lower, right) */
	if (slope > to_slope)
	  {
	    *Y2 = t_y;
	    *X2 = (t_x + (t_width/2)) -
	      (t_height / (2 * slope));
	  } else {
	    *X2 = t_x;
	    *Y2 = (t_y + (t_height/2)) -
	      (slope * (t_width / 2));
	  }
      } else
	{ /* Quadrant 2 (upper, left) */
	  if (slope > to_slope)
	    {
	      *Y2 = t_y + t_height;
	      *X2 = (t_x + (t_width/2)) +
		(t_height / (2 * slope));
	    } else
	      {
		*X2 = t_x + t_width;
		*Y2 = (t_y + (t_height/2)) +
		  (slope * (t_width / 2));
	      }
	}
    }
  else
    { /* slope is <= 0.0 */
      if (equis1 < equis2)
	{ /* Quadrant 1 (upper right) */
	  if (slope > (-to_slope))
	    {
	      *X2 = t_x;
	      *Y2 = (t_y + (t_height/2)) -
		(slope * (t_width / 2));
	    }
	  else
	    {
	      *Y2 = t_y + t_height;
	      *X2 = (t_x + (t_width/2)) +
		(t_height / (2 * slope));
	    }
	}
      else
	{ /* Quadrant 3 (lower, left) */
	  if (slope > (-to_slope))
	    {
	      *X2 = t_x + t_width;
	      *Y2 = (t_y + (t_height/2)) +
		(slope * (t_width / 2));
	    }
	  else
	    {
	      *Y2 = t_y;
	      *X2 = (t_x + (t_width/2)) -
		(t_height / (2 * slope));
	    }
	}
    }
}

/************************************************************************
 *
 * Redisplay: Draw an arc on its parents window.
 *            Horrendously long, because we have many special cases:
 *            2 ArcDrawModes, arcs where to == from, and arcs with
 *            siblings.
 *
 **************************************************************************/
static void Redisplay
#ifndef _NO_PROTO
  (XmArcWidget w, XEvent *event, Region region)
#else
  (w, event, region) XmArcWidget   w;
     XEvent       *event;
     Region        region;
#endif
{
  float  x1 = (float) w->arc.from_x,
         y1 = (float) w->arc.from_y,
         x2 = (float) w->arc.to_x,
         y2 = (float) w->arc.to_y ;
  int    width, height, x, y, a1, a2, s_rank = _sibling_rank (w);
  int    arrow1a_x, arrow1a_y, arrow2a_x, arrow2a_y,
  arrow1b_x, arrow1b_y, arrow2b_x, arrow2b_y;
  int    name_x, name_y, delta = w->arc.delta; /* to != from */
  Widget to   = w->arc.to,
         from = w->arc.from;
  XPoint points[30], curve [12];
  XmGraphWidget parent = (XmGraphWidget) XtParent(w);

  w->arc.rank = s_rank;

  if(!to || !from || !XtIsRealized(parent))
    return;

  if (w->core.visible && w->core.managed && !w->core.being_destroyed)
    {
      w->arc.visible = TRUE;

      if (to && from)
	{
	  /*
	   * If we need to, update endpoints
	   */
	  if(!w->arc.up_to_date)
	    { Dimension bwTo = XtBorderWidth(to) + XtBorderWidth(to);
	      Dimension bwFrom = XtBorderWidth(from) + XtBorderWidth(from);
	      _GetPoints (parent, RX(from), RY(from),
			  RWidth(from)+bwFrom, RHeight(from)+bwFrom,
			  RX(to) , RY(to),
			  RWidth(to)+bwTo, RHeight(to)+bwTo,
			  &x1, &y1, &x2, &y2);
	      /*
	       * Update widget.
	       */
	      w->arc.from_x =  (int) x1;
	      w->arc.to_x   = (int) x2;
	      w->arc.from_y =  (int) y1;
	      w->arc.to_y   = (int) y2;

	      /* Round off time */
	      x1 =  w->arc.from_x;
	      x2 =  w->arc.to_x;
	      y1 =  w->arc.from_y;
	      y2 =  w->arc.to_y;

	      w->arc.up_to_date = TRUE;

	      FreeArcRegions(w);
	    }

	  /* Check if we even need to draw this arc. No reason to if the arc hasn't moved,
	   * and the arc is outside the region requested.
	   */

	  if(region)
	    {
	      XRectangle rect;
	      XClipBox(region, &rect);

	      if(!_ArcInRect(w, &rect))
		return;
	    }

	  if(w->arc.region)
	    {
	      /*
	       * Fast case - if widgets haven't moved and the arc is just a straight line, and we don't
	       * need to calc the associated region,
	       * just use cached coords.
	       */

	      if (to != from && s_rank == 0)
		{
		  /*
		   * draw the arc
		   */

		  if(parent->graph.batch_drawing_mode)
		    AddArcLineToList (parent, w->arc.current_gc,  (int) x1, (int) y1, (int) x2, (int) y2);
		  else
		    XDrawLine (XtDisplay (parent), XtWindow (parent),
			       w->arc.current_gc, (int) x1, (int) y1, (int) x2, (int) y2);

		  if (w->arc.direction == XmDIRECTED)
		    {
		      draw_arrow (w, (int) x1, (int) y1, (int) x2, (int) y2);
		    }
		  else if (w->arc.direction == XmBIDIRECTED)
		    {
		      draw_arrow (w, (int) x1, (int) y1, (int) x2, (int) y2);
		      draw_arrow (w, (int) x2, (int) y2, (int) x1, (int) y1);
		    }

		  /*
		   *  draw the name
		   */

		  if (w->arc.map_name)
		  {
		      _XmStringDraw (XtDisplay(w), XtWindow(XtParent(w)),
				     w->arc.font, w->arc.label,
				     w->arc.current_gc,
#if SESD
				     ((int) ((x1 +  x2 - w->arc.labelwidth) / 2)),
				     ((int) ((y2 + y1 - w->arc.labelheight) / 2)),
#else
				     ((int) ((x1 +  x2) / 2)),
				     ((int) ((y2 + y1) / 2)) - w->arc.labelheight,
#endif
				     w->arc.labelwidth,
				     XmALIGNMENT_CENTER, XmSTRING_DIRECTION_L_TO_R, NULL);
		  }
		  return;
		}
	    }

	  /*
	   * If we got here it is a more complex case, the end points have moved or we
	   * need a region computed.
	   */


	  if (w->arc.region)
	    XDestroyRegion (w->arc.region);

	  if (to != from)
	    {
	      /* 2 cases here:
	       *   1) rank == 0
	       *   2) rank > 0
	       */

	      if (s_rank == 0)
		{
		  /*
		   * Get the region for graphic op. purposes
		   */
		  points[0].x = x1 - delta;
		  points[0].y = y1 - delta;
		  points[1].x = x1 + delta;
		  points[1].y = y1 + delta;

		  points[2].x = x2 + delta;
		  points[2].y = y2 + delta;
		  points[3].x = x2 - delta;
		  points[3].y = y2 - delta;

		  w->arc.region = XPolygonRegion (points, 4, EvenOddRule);

		  /* compute the region for the name */
		  if ( w->arc.map_name)
		    {

		      XPoint text_points[5];
		      Region tmp;

#if SESD
		      text_points[0].x = (x1 + x2 - w->arc.labelwidth) / 2;
		      text_points[0].y = (y2 + y1 - w->arc.labelheight) / 2;
#else
		      text_points[0].x = (x1 + x2) / 2;
		      text_points[0].y = (y2 + y1) / 2 - w->arc.labelheight;
#endif
		      text_points[1].x =  text_points[0].x + w->arc.labelwidth;
		      text_points[1].y =  text_points[0].y;
		      text_points[2].x =  text_points[1].x;
		      text_points[2].y =  text_points[0].y  + w->arc.labelheight;
		      text_points[3].x =  text_points[0].x;
		      text_points[3].y =  text_points[2].y;

		      tmp =  XPolygonRegion(text_points, 4, EvenOddRule);
		      w->arc.region = _AddRegionToRegion (w->arc.region, tmp);
		      XDestroyRegion(tmp);

		    }

		  /*
		    draw the arc
		    */
		  if(parent->graph.batch_drawing_mode)
		    AddArcLineToList (parent,  w->arc.current_gc, (int) x1, (int) y1, (int) x2, (int) y2);
		  else
		    XDrawLine (XtDisplay (XtParent(w)), XtWindow (XtParent(w)),
			       w->arc.current_gc, (int) x1, (int) y1, (int) x2, (int) y2);

		  /*
		    draw the arrow heads
		    */
		  if (w->arc.direction == XmDIRECTED)
		    draw_arrow (w, (int) x1, (int) y1, (int) x2, (int) y2);
		  else if (w->arc.direction == XmBIDIRECTED)
		    {
		      draw_arrow (w, (int) x1, (int) y1, (int) x2, (int) y2);
		      draw_arrow (w, (int) x2, (int) y2, (int) x1, (int) y1);
		    }

		  /*
		   * draw the name
		   */

		  if (w->arc.map_name)
		      _XmStringDraw (XtDisplay(w), XtWindow(XtParent(w)),
				     w->arc.font, w->arc.label,
				     w->arc.current_gc,
#if SESD
				     ((int) ((x1 +  x2 - w->arc.labelwidth) / 2)),
				     ((int) ((y2 + y1 - w->arc.labelheight) / 2)),
#else
				     ((int) ((x1 +  x2) / 2)),
				     ((int) ((y2 + y1) / 2)) - w->arc.labelheight,
#endif
				     w->arc.labelwidth,
				     XmALIGNMENT_CENTER, XmSTRING_DIRECTION_L_TO_R, NULL);
		}
	      else
		{ /* to != from, rank > 0 */


		  int    mid_x = x1 + (x2 - x1) / 2,
		  mid_y = y1 + (y2 - y1) / 2;
		  float  Xm, Ym, slope, displacement;
		  float  par_disp, perp_disp;
		  int    factor = 30; /* picked 30 heuristically */

		  /*
		   * Check for slope == 0 or slope == +- infinity
		   */

		  if (((int) y1) == ((int) y2))
		    slope = 0.01;
		  else if (((int) x1) == ((int) x2))
		    slope = 100.0;
		  else
		    slope = (y1-y2)/(x1-x2);

		  /*
		   * factor here comes with a value that determines
		   * the distance between siblings
		   */
		  {
		    double sqrt_arg = (factor * factor) + ((factor * factor) /
							   (MAX(s_rank, 1) / (slope * slope)));

		    factor = sqrt (ABS(sqrt_arg));
		  }

		  if ((s_rank % 2) == 1) /* odd sibling */
		    factor = -(factor + (s_rank * factor));
		  else

		    factor = factor + ((s_rank-1) * factor);

		  displacement = y1 - (slope * x1);
		  par_disp     = displacement + factor;
		  perp_disp    = mid_y + (mid_x / slope);
		  Xm           = (perp_disp - par_disp) / (slope + (1.0/slope));
		  Ym           = slope * Xm + par_disp;

		  /*
		   * draw the arc
		   */
		  bezier (8, curve, x1, y1, Xm, Ym, x2, y2);
		  XDrawLines (XtDisplay (XtParent(w)), XtWindow (XtParent(w)),
			      w->arc.current_gc, curve, 9,
			      CoordModeOrigin);

		  /*
		   * draw the arrow heads
		   */

		  if (w->arc.direction == XmDIRECTED)
		    draw_arrow (w, (int) Xm, (int) Ym, (int) x2, (int) y2);
		  else if (w->arc.direction == XmBIDIRECTED)
		    {
		      draw_arrow (w, (int) Xm, (int) Ym, (int) x2, (int) y2);
		      draw_arrow (w, (int) Xm, (int) Ym, (int) x1, (int) y1);
		    }

		  /*
		   * draw the name
		   */

		  if ( w->arc.map_name)
		      _XmStringDraw (XtDisplay(w), XtWindow(XtParent(w)),
				     w->arc.font, w->arc.label,
				     w->arc.current_gc,
#if SESD
				     (int) curve[4].x - w->arc.labelwidth/2,
				     (int) curve[4].y - w->arc.labelheight/2,
#else
				     (int) curve[4].x, (int) curve[4].y - w->arc.labelheight,
#endif
				     w->arc.labelwidth,
				     XmALIGNMENT_CENTER, XmSTRING_DIRECTION_L_TO_R, NULL);

		  /*
		   * Get the region for graphic op. purposes
		   */

		  points[0].x = x1 - delta;
		  points[0].y = y1 - delta;
		  points[1].x = x1 + delta;
		  points[1].y = y1 + delta;

		  points[2].x = curve[2].x + delta;
		  points[2].y = curve[2].y + delta;
		  points[3].x = curve[4].x + delta;
		  points[3].y = curve[4].y + delta;
		  points[4].x = curve[6].x + delta;
		  points[4].y = curve[6].y + delta;
		  points[5].x = x2 + delta;
		  points[5].y = y2 + delta;

		  points[6].x = x2 - delta;
		  points[6].y = y2 - delta;
		  points[7].x = curve[6].x - delta;
		  points[7].y = curve[6].y - delta;
		  points[8].x = curve[4].x - delta;
		  points[8].y = curve[4].y - delta;
		  points[9].x = curve[2].x - delta;
		  points[9].y = curve[2].y - delta;

		  w->arc.region = XPolygonRegion (points, 10, EvenOddRule);

		  /*
		   * compute the region for the name
		   */

		  if ( w->arc.map_name)
		    {
		      XPoint       text_points[5];
		      Region       tmp;


#if SESD
		      text_points[0].x =  curve[4].x - w->arc.labelwidth/2;
		      text_points[0].y =  curve[4].y - w->arc.labelheight/2;
#else
		      text_points[0].x =  curve[4].x;
		      text_points[0].y =  curve[4].y - w->arc.labelheight;
#endif
		      text_points[1].x =  text_points[0].x + w->arc.labelwidth;
		      text_points[1].y =  text_points[0].y;
		      text_points[2].x =  text_points[1].x;
		      text_points[2].y =  text_points[0].y  + w->arc.labelheight;
		      text_points[3].x =  text_points[0].x;
		      text_points[3].y =  text_points[2].y;

		      tmp           = XPolygonRegion(text_points, 4, EvenOddRule);
		      w->arc.region = _AddRegionToRegion (w->arc.region, tmp);
		      XDestroyRegion(tmp);

		    }
		}
	    }
	  else
	    { /* to == from */
	      /* compute the arc parameters */
	      switch (s_rank % 4) {
	      case 3: /* draw to right of node */
		x =      (int) (RX(to) + RWidth(to) *(6/10.0));
		y =      RY(to) - RHeight(to)/2;
		width =  RWidth(to);
		height = 2 * RHeight(to);
		a1 =     150 * 64;
		a2 =     -2 * a1;
#if SESD
		name_x = x + width;
		name_y    = y + height/2;
#else
		name_x = x + width -
		  ((w->arc.map_name) ? w->arc.labelwidth / 2 :0);

		name_y    = y + height/2 - w->arc.labelheight;
#endif

		arrow1a_x = x + width/3;
		arrow1a_y = y + height;
		arrow1b_x = x + width/18.0;
		arrow1b_y = y + height - height/4;
		arrow2a_x = x + width/3;
		arrow2a_y = y;
		arrow2b_x = x + width/18.0;
		arrow2b_y = y + height/4;
		break;
	      case 1: /* draw to left of node */
		x =      (int) (RX(to) - 6/10.0 * RWidth(to));
		y =      RY(to) - RHeight(to)/2;
		width =  RWidth(to);
		height = 2 * RHeight(to);
		a1 =     30 * 64;
		a2 =     326 * 64;
#if SESD
		name_x = x;
		name_y    = y + height/2;
#else
		name_x = x - ((w->arc.map_name) ? w->arc.labelwidth / 2 :0);
		name_y    = y + height/2 - w->arc.labelheight;
#endif

		arrow1a_x = x + (2*width)/3;
		arrow1a_y = y + height;
		arrow1b_x = x + width - width/18.0;
		arrow1b_y = y + height - height/4;
		arrow2a_x = x + (2*width)/3;
		arrow2a_y = y;
		arrow2b_x = x + width - width/18.0;
		arrow2b_y = y + height/4;
		break;
	      case 0:  /* draw on top of node */
		width  = RWidth(to)/2;
		height = RHeight(to) * 3/2;
		x =      RX(to) + width/2;
		y =      RY(to) - 2 * height/3;
		a1 =     -30 * 64;
		a2 =     180 * 64 - 2 * a1;
		name_x = x + width/2;
#if SESD
		name_y = y;
#else
		name_y = y - w->arc.labelheight;
#endif

		arrow1a_x = x + width;
		arrow1a_y = y;
		arrow1b_x = x + width - width/36;
		arrow1b_y = RY(to);
		arrow2a_x = x;
		arrow2a_y = y;
		arrow2b_x = x + width/36;
		arrow2b_y = RY(to);
		break;
	      case 2: /* draw on bottom of node */
		width =  RWidth(to)/2;
		height = RHeight(to) * 3/2;
		x =      RX(to) + width/2;
		y =      RY(to) + height/3;
		a1 =     30 * 64;
		a2 =     -180 * 64 - 2 * a1;
		name_x = x + width/2;
#if SESD
		name_y = y + height;
#else
		name_y = y + height - w->arc.labelheight;
#endif

		arrow1a_x = x + width;
		arrow1a_y = y + height;
		arrow1b_x = x + width - width/36;
		arrow1b_y = RY(to) + RHeight(to);
		arrow2a_x = x;
		arrow2a_y = y + height;
		arrow2b_x = x + width/36;
		arrow2b_y = RY(to) + RHeight(to);
		break;
	      }

	      /*
	       * compute the region for the arc
	       */

	      points[0].x = x;
	      points[0].y = y;
	      points[1].x = x + width;
	      points[1].y = y;
	      points[2].x = x + width;
	      points[2].y = y + height;
	      points[3].x = x;
	      points[3].y = y + height;


	      w->arc.region = XPolygonRegion (points, 4, EvenOddRule);

	      /*
	       * compute the region for the name
	       */

	      if (w->arc.map_name)
	      {
		Region tmp;
		XPoint text_points[5];

#if SESD
		text_points[0].x =  name_x;
		text_points[0].y =  name_y;
		text_points[1].x =  text_points[0].x + w->arc.labelwidth;
		text_points[1].y =  text_points[0].y;
		text_points[2].x =  text_points[1].x;
		text_points[2].y =  text_points[0].y + w->arc.labelheight;
		text_points[3].x =  text_points[0].x;
		text_points[3].y =  text_points[2].y;
#else
		text_points[0].x =  name_x;
		text_points[0].y =  name_y - w->arc.labelheight;
		text_points[1].x =  name_x + w->arc.labelwidth;
		text_points[1].y =  name_y;
		text_points[2].x =  text_points[1].x;
		text_points[2].y =  name_y + w->arc.labelheight;
		text_points[3].x =  name_x;
		text_points[3].y =  text_points[2].y;
#endif

		tmp =  XPolygonRegion(text_points, 4, EvenOddRule);
		w->arc.region = _AddRegionToRegion (w->arc.region, tmp);
		XDestroyRegion(tmp);
	      }

	      /*
	       * Draw the arc
	       */

	      XDrawArc (XtDisplay(XtParent(w)), XtWindow(XtParent(w)),
			w->arc.current_gc, (int) x, (int)y, (int)width, (int) height, (int) a1 , (int) a2);

	      /*
	       * Draw the arrow heads
	       */

	      if (w->arc.direction == XmDIRECTED)
		draw_arrow (w,  arrow1a_x, arrow1a_y, arrow1b_x, arrow1b_y);
	      else if (w->arc.direction == XmBIDIRECTED)
		{
		  draw_arrow (w, arrow1a_x, arrow1a_y, arrow1b_x, arrow1b_y);
		  draw_arrow (w, arrow2a_x, arrow2a_y, arrow2b_x, arrow2b_y);
		}

	      /*
	       * Draw the name
	       */

	      if ( w->arc.map_name)
		      _XmStringDraw (XtDisplay(w), XtWindow(XtParent(w)),
				     w->arc.font, w->arc.label,
				     w->arc.current_gc,
#if SESD
				     (int)name_x - w->arc.labelwidth/2,
				     (int) name_y - w->arc.labelheight/2,
#else
				     (int)name_x, (int) name_y,
#endif
				     w->arc.labelwidth,
				     XmALIGNMENT_CENTER, XmSTRING_DIRECTION_L_TO_R, NULL);
	    }
	}
    }
#if SESD
    FreeArcRegions(w);
#endif
}



/************************************************************************
 *
 * ComputeRegionsForArc: Similar to Redisplay, but don't draw the arcs
 *
 **************************************************************************/
void ComputeRegionsForArc
#ifndef _NO_PROTO
  (XmArcWidget w)
#else
  (w) XmArcWidget	   w;
#endif
{
  float  x1 = (float) w->arc.from_x,
  y1 = (float) w->arc.from_y,
  x2 = (float) w->arc.to_x,
  y2 = (float) w->arc.to_y ;
  int    width, height, x, y,  s_rank = _sibling_rank (w);


  int    name_x, name_y, delta = w->arc.delta;
  Widget to   = w->arc.to,
  from = w->arc.from;
  XPoint points[30], curve [12];

  XmGraphWidget parent = (XmGraphWidget) XtParent(w);

  w->arc.rank = s_rank;

  if (to && from)
    {

      if(!w->arc.up_to_date)
	{ Dimension bwto = XtBorderWidth(to) + XtBorderWidth(to);
	   Dimension bwFrom = XtBorderWidth(from) + XtBorderWidth(from);
	  _GetPoints (parent, RX(from), RY(from),
		      RWidth(from)+bwFrom, RHeight(from)+bwFrom,
		      RX(to), RY(to),
		      RWidth(to)+bwto, RHeight(to)+bwto,
		      &x1, &y1, &x2, &y2);
	  /*
	   * Update widget.
	   */

	  w->arc.from_x =  (int) x1;
	  w->arc.to_x   = (int) x2;
	  w->arc.from_y =  (int) y1;
	  w->arc.to_y   = (int) y2;

	  w->arc.up_to_date = TRUE;

	  FreeArcRegions(w);

	}

      if(w->arc.region) /* If upto date, just go on. */
	return;

      if (to != from)
	{
	  /* 2 cases here: 1) rank == 0
	     2) rank > 0   */

	  if (s_rank == 0)
	    { /* Get the region for graphic op. purposes */
#if SESD
	      /* Two cases.  Positive slope and negative slope. WJS 5/13/92 */
	      if ((x1 - x2)*(y1 - y2) <= 0)
		{ /* Region for up and to the right */
		  points[0].x = x1 - delta;
		  points[0].y = y1 - delta;
		  points[1].x = x1 + delta;
		  points[1].y = y1 + delta;

		  points[2].x = x2 + delta;
		  points[2].y = y2 + delta;
		  points[3].x = x2 - delta;
		  points[3].y = y2 - delta;
		}
	      else
		{
		  /* Region for up and to the left */
		  points[0].x = x1 - delta;
		  points[0].y = y1 + delta;
		  points[1].x = x1 + delta;
		  points[1].y = y1 - delta;

		  points[2].x = x2 + delta;
		  points[2].y = y2 - delta;
		  points[3].x = x2 - delta;
		  points[3].y = y2 + delta;
		}
#else
	      points[0].x = x1 - delta;
	      points[0].y = y1 - delta;
	      points[1].x = x1 + delta;
	      points[1].y = y1 + delta;

	      points[2].x = x2 + delta;
	      points[2].y = y2 + delta;
	      points[3].x = x2 - delta;
	      points[3].y = y2 - delta;
#endif

	      w->arc.region = XPolygonRegion (points, 4, EvenOddRule);

	      /* compute the region for the name */
	      if ( w->arc.map_name)
		{ XPoint text_points[5];
		  Region tmp;

#if SESD
		  text_points[0].x = (x1 + x2 - w->arc.labelwidth) / 2;
		  text_points[0].y = (y2 + y1 - w->arc.labelheight) / 2;
#else
		  text_points[0].x = (x1 + x2) / 2;
		  text_points[0].y = (y2 + y1) / 2 - w->arc.labelheight;
#endif
		  text_points[1].x =  text_points[0].x + w->arc.labelwidth;
		  text_points[1].y =  text_points[0].y;
		  text_points[2].x =  text_points[1].x;
		  text_points[2].y =  text_points[0].y + w->arc.labelheight;
		  text_points[3].x =  text_points[0].x;
		  text_points[3].y =  text_points[2].y;

		  tmp =  XPolygonRegion(text_points, 4, EvenOddRule);
		  w->arc.region = _AddRegionToRegion (w->arc.region, tmp);
		  XDestroyRegion(tmp);
		}
	    }
	  else
	    { /* to != from, rank > 0 */


	      int    mid_x = x1 + (x2 - x1) / 2,
	      mid_y = y1 + (y2 - y1) / 2;
	      float  Xm, Ym, slope, displacement;
	      float  par_disp, perp_disp;
	      int    factor = 30; /* picked 30 heuristically */

	      /*
	       * Check for slope == 0 or slope == +- infinity
	       */

	      if (((int) y1) == ((int) y2))
		slope = 0.01;
	      else if (((int) x1) == ((int) x2))
		slope = 100.0;
	      else
		slope = (y1-y2)/(x1-x2);

	      /*
	       * factor here comes with a value that determines
	       * the distance between siblings
	       */
	      {
		double sqrt_arg = (factor * factor) + ((factor * factor) /
						       (MAX(s_rank, 1) / (slope * slope)));

		factor = sqrt (ABS(sqrt_arg));
	      }


	      if ((s_rank % 2) == 1) /* odd sibling */
		factor = -(factor + (s_rank * factor));
	      else
		factor = factor + ((s_rank-1) * factor);

	      displacement = y1 - (slope * x1);
	      par_disp     = displacement + factor;
	      perp_disp    = mid_y + (mid_x / slope);
	      Xm           = (perp_disp - par_disp) / (slope + (1.0/slope));
	      Ym           = slope * Xm + par_disp;

	      bezier (8, curve, x1, y1, Xm, Ym, x2, y2);

	      /*
	       * Get the region for graphic op. purposes
	       */
	      points[0].x = x1 - delta;
	      points[0].y = y1 - delta;
	      points[1].x = x1 + delta;
	      points[1].y = y1 + delta;

	      points[2].x = curve[2].x + delta;
	      points[2].y = curve[2].y + delta;
	      points[3].x = curve[4].x + delta;
	      points[3].y = curve[4].y + delta;
	      points[4].x = curve[6].x + delta;
	      points[4].y = curve[6].y + delta;
	      points[5].x = x2 + delta;
	      points[5].y = y2 + delta;

	      points[6].x = x2 - delta;
	      points[6].y = y2 - delta;
	      points[7].x = curve[6].x - delta;
	      points[7].y = curve[6].y - delta;
	      points[8].x = curve[4].x - delta;
	      points[8].y = curve[4].y - delta;
	      points[9].x = curve[2].x - delta;
	      points[9].y = curve[2].y - delta;

	      w->arc.region = XPolygonRegion (points, 10, EvenOddRule);

	      /*
	       * compute the region for the name
	       */
	      if ( w->arc.map_name)
		{
		  XPoint text_points[5];
		  Region tmp;

#if SESD
		  text_points[0].x = curve[4].x - w->arc.labelwidth / 2;
		  text_points[0].y = curve[4].y - w->arc.labelheight / 2;
#else
		  text_points[0].x = curve[4].x;
		  text_points[0].y = curve[4].y - w->arc.labelheight;
#endif
		  text_points[1].x =  text_points[0].x + w->arc.labelwidth;
		  text_points[1].y =  text_points[0].y;
		  text_points[2].x =  text_points[1].x;
		  text_points[2].y =  text_points[0].y + w->arc.labelheight;
		  text_points[3].x =  text_points[0].x;
		  text_points[3].y =  text_points[2].y;

		  tmp = XPolygonRegion(text_points, 4, EvenOddRule);
		  w->arc.region = _AddRegionToRegion (w->arc.region, tmp);
		  XDestroyRegion(tmp);

		}
	    }
	}
      else
	{ /* to == from */

	  /*
	   * compute the arc parameters
	   */

	  switch (s_rank % 4) {
	  case 3: /* draw to right of node */
	    x     = (int) (RX(to) + RWidth(to) *(6/10.0));
	    y     = RY(to) - RHeight(to)/2;
	    width = RWidth(to);
	    height = 2 * RHeight(to);
#if SESD
	    name_x = x + width;
	    name_y    = y + height/2;
#else
	    name_x = x + width -
	      ((w->arc.map_name) ?
			   w->arc.labelwidth / 2 :  0);

	    name_y    = y + height/2 - w->arc.labelheight;
#endif


	    break;
	  case 1: /* draw to left of node */
	    x     = (int) (RX(to) - 6/10.0 * RWidth(to));
	    y     = RY(to) - RHeight(to)/2;
	    width = RWidth(to);
	    height = 2 * RHeight(to);
#if SESD
	    name_x = x;
	    name_y = y + height/2;
#else
	    name_x = x -
	      ((w->arc.map_name) ?
			   w->arc.labelwidth / 2 :  0);

	    name_y = y + height/2 - w->arc.labelheight;
#endif

	    break;
	  case 0:  /* draw on top of node */
	    width  = RWidth(to)/2;
	    height = RHeight(to) * 3/2;
	    x      = RX(to) + width/2;
	    y      = RY(to) - 2 * height/3;
	    name_x = x + width/2;
	    name_y = y - w->arc.labelheight;

	    break;
	  case 2: /* draw on bottom of node */
	    width  = RWidth(to)/2;
	    height = RHeight(to) * 3/2;
	    x      = RX(to) + width/2;
	    y      = RY(to) + height/3;
	    name_x = x + width/2;
#if SESD
	    name_y = y + height;
#else
	    name_y = y + height - w->arc.labelheight;
#endif

	    break;
	  }
	  /*
	   * compute the region for the arc
	   */
	  points[0].x = x;
	  points[0].y = y;
	  points[1].x = x + width;
	  points[1].y = y;
	  points[2].x = x + width;
	  points[2].y = y + height;
	  points[3].x = x;
	  points[3].y = y + height;
	  w->arc.region = XPolygonRegion (points, 4, EvenOddRule);

	  /*
	   * compute the region for the name
	   */

	  if (w->arc.map_name)
	    {
	      Region tmp;
	      XPoint text_points[5];

#if SESD
	      text_points[0].x =  name_x;
	      text_points[0].y =  name_y;
	      text_points[1].x =  text_points[0].x + w->arc.labelwidth;
	      text_points[1].y =  text_points[0].y;
	      text_points[2].x =  text_points[1].x;
	      text_points[2].y =  text_points[0].y + w->arc.labelheight;
	      text_points[3].x =  text_points[0].x;
	      text_points[3].y =  text_points[2].y;
#else
	      text_points[0].x =  name_x;
	      text_points[0].y =  name_y - w->arc.labelheight;
	      text_points[1].x =  name_x + w->arc.labelwidth;
	      text_points[1].y =  name_y;
	      text_points[2].x =  text_points[1].x;
	      text_points[2].y =  name_y + w->arc.labelheight;
	      text_points[3].x =  name_x;
	      text_points[3].y =  text_points[2].y;
#endif
	      tmp =  XPolygonRegion(text_points, 4, EvenOddRule);
	      w->arc.region = _AddRegionToRegion (w->arc.region, tmp);
	      XDestroyRegion(tmp);
	    }
	}
   }
}

void FreeArcRegions
#ifndef _NO_PROTO
  (XmArcWidget w)
#else
  (w) XmArcWidget   w;
#endif
{
  if (w->arc.region)
    XDestroyRegion (w->arc.region);

  w->arc.region = NULL;

}

static Boolean SetValues
#ifndef _NO_PROTO
  (Widget current_w, Widget request_w, Widget new_w)
#else
  (current_w, request_w, new_w)
     Widget current_w, request_w, new_w;
#endif
{
  XmArcWidget current = (XmArcWidget) current_w;
  XmArcWidget request = (XmArcWidget) request_w;
  XmArcWidget new  = (XmArcWidget) new_w;
  XGCValues       values;
  XtGCMask        valueMask;
  Boolean         redraw = FALSE;
  Widget   graphW  =  XtParent(new);
  XmGraphWidget   graph  = (XmGraphWidget) graphW;
  Dimension w, h;

  new->core.width = 1;
  new->core.height = 1;
  new->core.x = 0;
  new->core.y = 0;
  new->core.border_width = 0;

  if (new->arc.foreground != current->arc.foreground ||
      (new->arc.font      != current->arc.font && current->arc.map_name) ||
       new->arc.width      != current->arc.width ||
       new->arc.style      != current->arc.style ||
       new->arc.cap_style  != current->arc.cap_style ||
       (new->arc.dash_offset!= current->arc.dash_offset ||
        new->arc.dashes != current->arc.dashes &&
        current->arc.style != LineSolid) ||
       (current->arc.label  != new->arc.label &&  current->arc.map_name) ||
       current->arc.map_name != new->arc.map_name ||
       new->arc.highlight != current->arc.highlight ||
       (new->arc.highlightcolor != current->arc.highlightcolor &&
        current->arc.highlight)
    )
    {
      if(XtIsRealized(graph))
	_EraseArc (current);
      redraw = TRUE;
    }


    if (new->arc.label != current->arc.label)
    {
	redraw = TRUE;
	if (new->arc.label == NULL)
	{
	    XmString string;
#if (XmVersion >= 1002) /* Motif 1.2 -- NPM mod */
	    XmStringCharSet cset = (XmStringCharSet) XmFONTLIST_DEFAULT_TAG;
#else /* Motif 1.1 or 1.0 -- NPM mod */
	    XmStringCharSet cset = (XmStringCharSet) XmSTRING_DEFAULT_CHARSET;
#endif /* WINTERP_MOTIF_12 -- NPM mod */

	    string   =  XmStringLtoRCreate(XrmQuarkToString(current->core.xrm_name), cset);
	    new->arc.label =  _XmStringCreate(string);
	    XtFree((void *)string);
	}
	else
	{
	  if (_XmStringIsXmString((XmString)new->arc.label))
	    new->arc.label = _XmStringCreate((XmString)new->arc.label);
	  else
	  {
	      XmString string;
#if (XmVersion >= 1002)		/* Motif 1.2 -- NPM mod */
	      XmStringCharSet cset = (XmStringCharSet) XmFONTLIST_DEFAULT_TAG;
#else /* Motif 1.1 or 1.0 -- NPM mod */
	      XmStringCharSet cset = (XmStringCharSet) XmSTRING_DEFAULT_CHARSET;
#endif /* WINTERP_MOTIF_12 -- NPM mod */

	      _XmWarning (new_w, "Arc Widget expects a compound string");
	      string = XmStringLtoRCreate(XrmQuarkToString(new->core.xrm_name), cset);
	      new->arc.label =  _XmStringCreate(string);
	      XtFree((void *)string);
	  }
      }

	_XmStringFree(current->arc.label);
	current->arc.label= NULL;
	request->arc.label= NULL;
  }


    if (new->arc.font != current->arc.font)
    {
	redraw = TRUE;
	if (new->arc.font == NULL)
	{
	    XFontStruct *fs;
#if (XmVersion >= 1002) /* Motif 1.2 -- NPM mod */
	    XmStringCharSet cset = (XmStringCharSet) XmFONTLIST_DEFAULT_TAG;
#else /* Motif 1.1 or 1.0 -- NPM mod */
	    XmStringCharSet cset = (XmStringCharSet) XmSTRING_DEFAULT_CHARSET;
#endif /* WINTERP_MOTIF_12 -- NPM mod */

	    fs = XLoadQueryFont(XtDisplay(new), "fixed");
	    new->arc.font = XmFontListCreate (fs, cset);
	}
	else
	    new->arc.font = XmFontListCopy (new->arc.font);

        _XmStringUpdate (new->arc.font, new->arc.label);
    }

  if(redraw)
  {
      _XmStringExtent (new->arc.font, new->arc.label, &w, &h);
      new->arc.labelwidth = w;
      new->arc.labelheight = h;
  }

  if (new->arc.highlightcolor != current->arc.highlightcolor ||
      new->arc.foreground != current->arc.foreground ||
      new->arc.font       != current->arc.font ||
      new->arc.width      != current->arc.width ||
      new->arc.style      != current->arc.style ||
      new->arc.cap_style  != current->arc.cap_style ||
      new->arc.dash_offset!= current->arc.dash_offset ||
      new->arc.dashes     != current->arc.dashes)
    {
	short           myindex;
	XFontStruct     *fs;

      valueMask = GCForeground | GCBackground | GCLineWidth | GCLineStyle |
	GCCapStyle | GCDashOffset | GCDashList | GCFont;

      values.foreground = new->arc.foreground;
      values.background = graph->core.background_pixel;
      values.line_width = new->arc.width;
      values.line_style = new->arc.style;
      values.cap_style  = new->arc.cap_style;
      values.dash_offset= new->arc.dash_offset;
      values.dashes     = new->arc.dashes;

      _XmFontListSearch(new->arc.font,
#if (XmVersion >= 1002) /* Motif 1.2 -- NPM mod */
			XmFONTLIST_DEFAULT_TAG,
#else /* Motif 1.1 or 1.0 -- NPM mod */
			XmSTRING_DEFAULT_CHARSET,
#endif /* WINTERP_MOTIF_12 -- NPM mod */
			&myindex ,
			&fs);

      if (fs==NULL)
	  valueMask &= ~GCFont;
      else
	  values.font = fs->fid;

      XtReleaseGC(graphW, current->arc.gc);
      new->arc.gc = XtGetGC((Widget) graph, valueMask, &values);

      /*
       * Set the HIGHLIGHT graphic Context
       */
      values.foreground = new->arc.highlightcolor;
      XtReleaseGC(graphW, current->arc.highlight_gc);
      new->arc.highlight_gc = XtGetGC((Widget) graph, valueMask, &values);

      /*
       * Set the CLEAR graphic Context
       */

      valueMask = GCForeground | GCBackground | GCFunction |  GCLineWidth | GCFont;

      values.function = GXcopy;
      values.foreground = graph->core.background_pixel;
      values.background = graph->core.background_pixel;
      XtReleaseGC(graphW, current->arc.clear_gc);
      new->arc.clear_gc = XtGetGC((Widget) graph, valueMask, &values);

      redraw = TRUE;
    }

  if (new->arc.highlight)
    {
      new->arc.current_gc = new->arc.highlight_gc;
    }
  else
    {
      new->arc.current_gc = new->arc.gc;
    }

  if(!new->arc.to || !new->arc.from)
  {
      XtWarning("Arc widget should have non-null to and from widgets");
  }


  if(new->arc.to != current->arc.to ||
     new->arc.from  != current->arc.from)
    {
      Widget new_to = new->arc.to;
      Widget new_from = new->arc.from;
      /*
       * Set it back for now so the move works right.
       */
      new->arc.to = current->arc.to;
      new->arc.from = current->arc.from;

      XmGraphMoveArc(graphW, new_w, new_from, new_to);
    }
  else if (redraw)
    Redisplay (new, (XEvent *) NULL, (Region) NULL);


  return FALSE;
}


static void draw_arrow
#ifndef _NO_PROTO
  (XmArcWidget w, int x1, int y1, int x2, int y2)
#else
  (w, x1, y1, x2, y2) XmArcWidget w; int x1, y1, x2, y2;
#endif
{
  double c, s, len;
  double headWidth = (w->arc.width ? w->arc.width : 1) + 2;
  /* width 0 lines are really width 1  WJS 920103 */
  XPoint poly[4];

 if (x1 == x2 && y1 == y2)
   return;

  len = LENGTH(x2 - x1, y2 - y1);

  c = (len == 0) ? 0 :  (x2 - x1) / len;
  s = (len == 0) ? 0 : (y2 - y1) / len;

  poly[0].x = x2;
  poly[0].y = y2;
  poly[1].x = (int) x2 - ROUND(10 * c + headWidth * s);
  poly[1].y = (int) y2 - ROUND(10 * s - headWidth * c);
  poly[2].x = (int) x2 - ROUND(10 * c - headWidth * s);
  poly[2].y = (int) y2 - ROUND(10 * s + headWidth * c);
  poly[3].x = x2;
  poly[3].y = y2;

  XFillPolygon (XtDisplay(XtParent(w)), XtWindow(XtParent(w)),
		w->arc.current_gc, poly, 4, Convex, CoordModeOrigin);

}

static void clear_arrow
#ifndef _NO_PROTO
  (XmArcWidget w, int x1, int y1, int x2, int y2)
#else
  (w, x1, y1, x2, y2) XmArcWidget w; int x1, y1, x2, y2;
#endif
{
  double c, s, len;
  double headWidth = (w->arc.width ? w->arc.width : 1) + 2;
  /* width 0 lines are really width 1  WJS 920103 */
  XPoint poly[4];

 if (x1 == x2 && y1 == y2)
   return;

  len = LENGTH(x2 - x1, y2 - y1);

  c = (len == 0) ? 0 :  (x2 - x1) / len;
  s = (len == 0) ? 0 :  (y2 - y1) / len;

  poly[0].x = x2;
  poly[0].y = y2;
  poly[1].x = (int) x2 - ROUND(10 * c + headWidth * s);
  poly[1].y = (int) y2 - ROUND(10 * s - headWidth * c);
  poly[2].x = (int) x2 - ROUND(10 * c - headWidth * s);
  poly[2].y = (int) y2 - ROUND(10 * s + headWidth * c);
  poly[3].x = x2;
  poly[3].y = y2;

  XFillPolygon (XtDisplay(XtParent(w)), XtWindow(XtParent(w)),
		w->arc.clear_gc, poly, 4, Convex, CoordModeOrigin);
}

/*
 * bezier: given the end points of an arc and a third point
 *         it will calculate the points needed to draw n lines
 *         between the end points so that it will look like a curve.
 *
 *        limit must be at least 1 less than size of points
 */
static void bezier
#ifndef _NO_PROTO
  (int limit, XPoint *points,
	    float x0, float y0, float x1, float y1, float x2, float y2)
#else
  (limit, points, x0, y0, x1, y1, x2, y2)
     int     limit;
     XPoint *points;
     float   x0, y0, x1, y1, x2, y2;
#endif
{
  float du = 1.0 /limit, u = 0.0, c0, c1, c2;
  int i;

  for (i = 0; i <= limit; i++) {
    c0 = (1 - u) * (1 - u);
    c1 = (2 * u) * (1 - u);
    c2 = u * u;
    points[i].x = (int) (x0 * c0 + x1 * c1 + x2 * c2);
    points[i].y = (int) (y0 * c0 + y1 * c1 + y2 * c2);
    u = u + du;
  }
}

#define MAX_ARCS 200
static struct  {
  int       n;
  XSegment  lines[MAX_ARCS];
} arc_line_list = { 0, 0};


void _InitArcList
#ifndef _NO_PROTO
  (XmGraphWidget graph)
#else
  (graph)
     XmGraphWidget graph;
#endif
{
  AddArcLineToList (graph, (GC) NULL, 0, 0, 0, 0);
  arc_line_list.n = 0;
}

static void AddArcLineToList
#ifndef _NO_PROTO
  (XmGraphWidget graph, GC gc, int x1, int y1, int x2, int y2)
#else
  (graph, gc, x1, y1, x2, y2)
     XmGraphWidget graph;
     GC            gc;
     int x1, y1, x2, y2;
#endif
{
  int pos = arc_line_list.n;
  static GC last_gc = (GC) NULL; /* This seems like a potential bug.
                                    Could we ever end up using a GC that has already been
                                    freed? Seems possible, but probably not
                                    in this widget because we always do this
                                    stuff in a tight loop, with no possiblity of
                                    a setvalues in between. Still... */

#if SESD
  /* We need to keep track of the server type since a Sun doesn't
     do clipping with an XDrawSegments() call. */
  static int serverType = 0; /* unknown */
#endif

  if (arc_line_list.n > 0 && (MAX_ARCS <= (1 + pos) || gc != last_gc))
  { if (serverType == 0)
    { if (strcmp(XServerVendor(XtDisplay(graph)),
		    "X11/NeWS - Sun Microsystems Inc.") == 0)
	       serverType = 1; /* Sun */
	  else serverType = 2; /* not Sun */
    }

    if (serverType == 1)  /* workaround for Sun Server */
    { int i;
      for(i=0;i< arc_line_list.n; i++)
      { XDrawLine(XtDisplay (graph), XtWindow (graph), last_gc,
		  arc_line_list.lines[i].x1,
		  arc_line_list.lines[i].y1,
		  arc_line_list.lines[i].x2,
		  arc_line_list.lines[i].y2);
	XFlush(XtDisplay (graph));
      }
    } else
    {
      XDrawSegments (XtDisplay (graph), XtWindow (graph), last_gc,
		   &(arc_line_list.lines[0]), arc_line_list.n);
     }
      pos = arc_line_list.n = 0;
  }
  last_gc = gc;
  arc_line_list.lines[pos].x1 = x1;
  arc_line_list.lines[pos].y1 = y1;
  arc_line_list.lines[pos].x2 = x2;
  arc_line_list.lines[pos].y2 = y2;
  arc_line_list.n++;
}

static void DispatchInput
#ifndef _NO_PROTO
  (XmArcWidget arc, XEvent *event, Mask event_mask)
#else
  (arc, event, event_mask)
     XmArcWidget arc;
     XEvent     *event;
     Mask        event_mask;
#endif
{  if (event_mask & XmARM_EVENT)
     {
       Arm (arc, event);
     }
   else if (event_mask & XmACTIVATE_EVENT)
     {
       Activate (arc, event);
       Disarm (arc, event);
     }
   else if (event_mask & XmENTER_EVENT) Enter (arc, event);
   else if (event_mask & XmLEAVE_EVENT) Leave (arc, event);

}


static void Leave
#ifndef _NO_PROTO
  (XmArcWidget arc, XEvent *event)
#else
  (arc, event) XmArcWidget arc; XEvent     *event;
#endif
{
   arc->arc.current_gc = arc->arc.gc;

   Redisplay(arc, event, (Region) NULL);
}

static void Enter
#ifndef _NO_PROTO
  (XmArcWidget arc, XEvent *event)
#else
  (arc, event)
     XmArcWidget arc;
     XEvent     *event;
#endif
{

   arc->arc.current_gc = arc->arc.highlight_gc;

   Redisplay(arc, event, (Region) NULL);
}


static void Arm
#ifndef _NO_PROTO
  (XmArcWidget arc, XEvent *event)
#else
  (arc, event) XmArcWidget arc; XEvent     *event;
#endif
{
   Widget arcW =  (Widget) arc;
   XmGraphCallbackStruct cb;

   arc -> arc.armed = TRUE;
   arc->arc.current_gc = arc->arc.highlight_gc;

   Redisplay(arc, event, (Region) NULL);

   if (XtHasCallbacks(arcW, XmNarmCallback) == XtCallbackHasSome)
   {

      XFlush(XtDisplay (arc));
      cb.event  = event;
      cb.reason = XmCR_ARM;
      cb.widget = (Widget) arc;
      cb.interactive = TRUE;

      XtCallCallbacks (arcW, XmNarmCallback, &cb);
   }
}



static void Activate
#ifndef _NO_PROTO
  (XmArcWidget arc, XEvent *event)
#else
  (arc, event) XmArcWidget arc; XEvent     *event;
#endif
{
   Widget arcW =(Widget) arc;
   XButtonEvent *buttonEvent = (XButtonEvent *) event;
   XmGraphCallbackStruct cb;

   arc -> arc.armed = FALSE;
   arc->arc.current_gc = arc->arc.gc;

   Redisplay(arc, event, (Region) NULL);

   if(XPointInRegion(((XmArcWidget) arc)->arc.region,
		     buttonEvent->x,buttonEvent->y))
     {

       cb.reason = XmCR_ACTIVATE;
       cb.event = event;
       cb.widget = (Widget) arc;
       cb.interactive = TRUE;

       if (XtHasCallbacks(arcW, XmNactivateCallback) == XtCallbackHasSome)
       {
	  XFlush (XtDisplay (arc));
	  XtCallCallbacks (arcW, XmNactivateCallback, &cb);
       }
    }
}

/************************************************************************
 *
 *    Disarm
 *
 *     Mark the pushbutton as unarmed (i.e. active).
 *     The callbacks for XmNdisarmCallback are called..
 *
 ************************************************************************/

static void Disarm
#ifndef _NO_PROTO
  (XmArcWidget arc, XEvent *event)
#else
  (arc, event) XmArcWidget  arc; XEvent      *event;
#endif
{ Widget  arcW = (Widget)  arc;
  XmGraphCallbackStruct cb;

   arc -> arc.armed = FALSE;
   arc->arc.current_gc = arc->arc.gc;

   if (XtHasCallbacks(arcW,XmNdisarmCallback) == XtCallbackHasSome)
   {
      cb.reason = XmCR_DISARM;
      cb.event = event;
      cb.widget = (Widget) arc;
      cb.interactive = TRUE;
      XtCallCallbacks (arcW, XmNdisarmCallback, &cb);
   }
}



Region _AddRegionToRegion
#ifndef _NO_PROTO
  (Region region1, Region region2)
#else
  (region1, region2) Region region1, region2;
#endif
{
  Region compositeregion = XCreateRegion();

  if(!region1 && !region2)
    return NULL;

  if(!region1)        /* Make a copy of region2 */
    XUnionRegion(region2, region2, compositeregion);

  else if(!region2)   /* Make a copy of region1 */
    XUnionRegion(region1, region1, compositeregion);
  else                /* Really create a union of the two */
    XUnionRegion(region1, region2, compositeregion);

  /*
   * Free former region1
   */

  if(region1)
    XDestroyRegion(region1);

  return compositeregion;
}


void _XmUnhighlightArc
#ifndef _NO_PROTO
  (XmArcWidget w)
#else
  (w) XmArcWidget    w;
#endif
{
  w->arc.highlight = FALSE;
  w->arc.current_gc = w->arc.gc;
  Redisplay (w, (XEvent *) NULL, (Region) NULL);
}

void _XmHighlightArc
#ifndef _NO_PROTO
  (XmArcWidget w)
#else
  (w) XmArcWidget    w;
#endif
{
  w->arc.highlight = TRUE;
  w->arc.current_gc = w->arc.highlight_gc;
  Redisplay (w, NULL, NULL);
}

void XmArcGetPos
#ifndef _NO_PROTO
  (XmArcWidget w, Position *x1_ret, Position *y1_ret,
				Position *x2_ret, Position *y2_ret)
#else
  (w, x1_ret, y1_ret, x2_ret, y2_ret)
     XmArcWidget w;
     Position *x1_ret, *y1_ret, *x2_ret, *y2_ret;
#endif
{
  float  x1, y1, x2, y2;
  XmGraphWidget parent = (XmGraphWidget)XtParent(w);
  Widget to   = w->arc.to,
         from = w->arc.from;

  if(!w->arc.up_to_date)
    { Dimension bwto = XtBorderWidth(to) + XtBorderWidth(to);
      Dimension bwFrom = XtBorderWidth(from) + XtBorderWidth(from);
      _GetPoints (parent, RX(from), RY(from),
		  RWidth(from)+bwFrom, RHeight(from)+bwFrom,
		  RX(to), RY(to),
		  RWidth(to)+bwto, RHeight(to)+bwto,
		  &x1, &y1, &x2, &y2);
      /*
       * Update widget.
       */
      w->arc.from_x =  (int) x1;
      w->arc.to_x   = (int) x2;
      w->arc.from_y =  (int) y1;
      w->arc.to_y   = (int) y2;

      w->arc.up_to_date = TRUE;
    }

  *x1_ret = w->arc.from_x;
  *y1_ret = w->arc.from_y;
  *x2_ret = w->arc.to_x;
  *y2_ret = w->arc.to_y;
}


/* CYY addition, June 10, 1993
   In R5, XmString has been modified to have "internal" and "external"
   representations. We have to convert the internal representation to
   external one for GetValues.  Normally, this is doen by the synthetic
   resources in Primitive. However, since Arc inherits from Widget rather
   than Primitive, we have to do this by hand.
*/
#ifdef _R5_
static void GetValuesHook
#ifdef _NO_PROTO
  ( w, args, num_args ) Widget w ; ArgList args ; Cardinal *num_args ;
#else
  (Widget w, ArgList args, Cardinal *num_args )
#endif /* _NO_PROTO */
{ int i, no;
  XmArcWidget arc = (XmArcWidget) w;

  no = *num_args;
  for (i=0;i<no;i++)
  { if (strcmp(args[i].name, XmNlabelString) == NULL)
    { XmString string;
      string = _XmStringCreateExternal (arc->arc.font, arc->arc.label);

      *((XtArgVal *)(args[i].value)) = (XtArgVal) string;
    }
  }

}
#endif

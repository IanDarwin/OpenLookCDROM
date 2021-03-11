/* -*-C-*-
********************************************************************************
*
* File:         Arc.h
* RCS:          $Header: /users/npm/src/winterp/src-server/widgets/RCS/Arc.h,v 2.1 1994/06/06 15:48:05 npm Exp $
* Description:  Public Header file for a Motif Arc Widget
* Author:       Doug Young dyoung@wpd.sgi.com, Luis Miguel, (luis@postgres.berkeley.edu)
* Created:      
* Modified:     Sun May 29 22:59:07 1994 (Niels Mayer) npm@indeed
* Language:     C
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

/* CYY modified 2/1/91 for  R3 and R4 compatible and ANSI_C fixes*/
#ifndef _XmArc_h
#define _XmArc_h

externalref WidgetClass xmArcWidgetClass;
typedef struct _XmArcClassRec *XmArcWidgetClass;
typedef struct _XmArcRec      *XmArcWidget;
typedef XmArcWidget 	      *XmArcWidgetList;

#define  XmARC_BIT    (1<<29)

#define XmIsArc(w)  XtIsSubclass(w, xmArcWidgetClass)


#ifndef XmNhighlightColor
#define XmNhighlightColor	"highlightColor"
#endif

#ifndef XmCHighlightColor
#define XmCHighlightColor	"HighlightColor"
#endif

#define XmNlabel		"label"
#define XmNarcEditedCallback	"arcEditedCallback"
#define XmNaddArcCallback       "addArcCallback"
#define XmNto		        "to"
#define XmCTo	                "To"
#define XmNfrom		        "from"
#define XmCFrom	             	"From"
#define XmNmapLabel             "mapLabel"
#define XmCMapLabel             "MapLabel"
#define XmNdelta		"delta"
#define XmCDelta		"Delta"
#define XmCHighlight		"Highlight"
#define XmNarcDirection		"arcDirection" /* XmCDirection class */
#define XmNstyle                "style"
#define XmCStyle                "Style"
#define XmRLineStyle             "LineStyle"
#define XmNcapStyle             "capStyle"
#define XmCCapStyle             "CapStyle"
#define XmRCapStyle             "CapStyle"
#define XmNdashes               "dashes"
#define XmCDashes               "Dashes"
#define XmNdashOffset           "dashOffset"
#define XmCDashOffset           "DashOffset"
#define XmCDirection            "Direction"
#define XmRArcDirection         "ArcDirection"
#define XmNarcWidth             "arcWidth"
#define XmCArcWidth         	"ArcWidth"


#ifdef __cplusplus                    /* do not leave open across includes */
extern "C" {                                  /* for C++ V2.0 */
#endif

#ifndef _NO_PROTO

extern Region  _AddRegionToRegion (Region,Region);   /* Arc.c */
extern Boolean _ArcInRect	(XmArcWidget, XRectangle *); /* Graph.c */
extern void    _EraseArc 	(XmArcWidget arc);  /* Arc.c */
extern void    _SetupArcInternal (XmArcWidget arc);  /* Arc.c */
extern void    _XmUnhighlightArc (XmArcWidget);      /* Arc.c */
extern void    _XmHighlightArc 	(XmArcWidget);      /* Arc.c */
extern int     _sibling_rank 	(XmArcWidget);      /* Arc.c */
extern void    ComputeRegionsForArc (XmArcWidget);   /* Arc.c */
extern void    FreeArcRegions 	(XmArcWidget);      /* Arc.c */
extern void    XmArcGetPos	(XmArcWidget, Position *, Position *, 
					      Position *, Position *);/*Arc.c*/
extern Widget  XmCreateArc	(Widget,String,ArgList,Cardinal); /* Graph.c */
extern Widget  XmCreateAttachedArc (Widget,String,Widget, Widget, 
					  ArgList,Cardinal); /* Graph.c */



#else /* _NO_PROTO */

extern Widget XmCreateArc();	 	/* Graph.c */
extern Widget XmCreateAttachedArc();	 /* Graph.c */
extern Region _AddRegionToRegion ();     /* Arc.c */
extern Boolean _ArcInRect	();      /* Graph.c */
extern void   _EraseArc 	();      /* Arc.c */
extern void   _XmUnhighlightArc ();      /* Arc.c */
extern void   _XmHighlightArc 	();      /* Arc.c */
extern int    _sibling_rank 	();      /* Arc.c */
extern void   ComputeRegionsForArc ();   /* Arc.c */
extern void   FreeArcRegions 	();      /* Arc.c */
#endif  /* _NO_PROTO */


#ifdef __cplusplus
}                                             /* for C++ V2.0 */
#endif


#define XmBIDIRECTED  0
#define XmDIRECTED    1
#define XmUNDIRECTED  2

#define XmLineSolid               0
#define XmLineOnOffDash           1
#define XmLineDoubleDash          2


#define XmCapNotLast              0
#define XmCapButt                 1
#define XmCapRound                2
#define XmCapProjecting           3

#endif /* _XmArc_h */
/* DON'T ADD ANYTHING AFTER THIS #endif */


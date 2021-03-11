/* -*-C-*-
********************************************************************************
*
* File:         ArcP.h
* RCS:          $Header: /users/npm/src/winterp/src-server/widgets/RCS/ArcP.h,v 2.1 1994/06/06 15:48:04 npm Exp $
* Description:  Private Header file for a Motif Arc Widget
* Author:       Doug Young dyoung@wpd.sgi.com, Luis Miguel, (luis@postgres.berkeley.edu)
* Created:      
* Modified:     Sun May 29 22:59:16 1994 (Niels Mayer) npm@indeed
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

#ifndef ARCP_H
#define ARCP_H
    
#include <Xm/Xm.h>

/* CYY added */
#include "Arc.h"

    
typedef struct arclist {
	XmArcWidgetList  arcs;
	int         n_arcs;
	int         n_slots;
    } ArcList;

typedef struct _XmArcClassPart{
#ifdef _R4_
    XmWidgetDispatchProc         input_dispatch;
#else
    XtWidgetProc         input_dispatch;
#endif
} XmArcClassPart;


typedef struct _XmArcClassRec {
    CoreClassPart	core_class;
    XmArcClassPart	arc_class;
} XmArcClassRec;

externalref XmArcClassRec xmArcClassRec;

typedef struct _XmArcPart {
    int		from_x,             /* cache the last x,y coords of the */
    		from_y,             /* head (to) and tail (from) of this */
    		to_x,               /* arc widget. Used to efficiently rubber */
    		to_y,               /* band arcs to moving nodes */
    		width,
    		dashes,
    		dash_offset;
    Widget	to;
    Widget	from;
    unsigned char direction,
    		  style,
    		  cap_style;
    Pixel      foreground;
    Pixel      highlightcolor;
    Boolean      highlight;
    Boolean      visible;
    XmFontList	font;  
    _XmString     label;
    Dimension    labelwidth, labelheight;
    Boolean      map_name;
    ArcList      *siblings;
    GC           gc;
    GC           highlight_gc;
    GC           current_gc;
    GC           clear_gc;
    int          delta;
    Region       region;
    XtCallbackList arm_callback;
    XtCallbackList activate_callback;
    XtCallbackList disarm_callback;
    XtCallbackList arc_edited;
    caddr_t        user_data;
    Boolean        armed;
    Boolean        up_to_date;
    int            rank;
} XmArcPart;


typedef struct _XmArcRec {
    CorePart	core;
    XmArcPart	arc;
} XmArcRec;

typedef struct {
    Widget child;
    int isarc;
} Rubber;



#endif /* ARCP_H */

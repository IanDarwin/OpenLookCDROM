
/***********************************************************

  -- Copyright (c) 1994 Regents of the University of California.
  -- All rights reserved.
  --
  -- This software was developed by the Answer Garden project
  -- at the University of California, Irvine.
  --
  -- Redistribution and use in source and binary forms are permitted
  -- provided that the above copyright notice and this paragraph are
  -- duplicated in all such forms and that any documentation,
  -- advertising materials, and other materials related to such
  -- distribution and use acknowledge that the software was developed
  -- by the University of California, Irvine.  The name of the
  -- University may not be used to endorse or promote products derived
  -- from this software without specific prior written permission.
  -- THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
  -- IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
  -- WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
  
  -- Answer Garden is a trademark of the Regents of the University of
  -- California.  All rights reserved.

  LAYOUT.C:  The Layout widget

  This is just like a bulletin board widget *except*
  - It does not resize itself to fit its children
  
  
  Mark Ackerman
  Information and Computer Science
  University of California, Irvine
  
  formerly -
  MIT/Center for Coordination Science
  
  ackerman@ics.uci.edu
  
  This was mostly done by Russ Sasnett, MIT/Project Athena and GTE
  Laboratories.  It is a stripped-down box widget.
  
  send bugs to:
    Mark Ackerman
    ackerman=ag@ics.uci.edu
  


******************************************************************/


/* 
 * Layout.c - Layout composite widget
 * 
 */

#include	<X11/IntrinsicP.h>
#include	<X11/StringDefs.h>
#include	"LayoutP.h"

/****************************************************************
 *
 * Layout Resources
 *
 ****************************************************************/

#define Offset(field) XtOffset(LayoutWidget, field)
static XtResource resources[] = {
    {XtNheight,XtCHeight,XtRDimension,sizeof(Dimension),
	 Offset(core.height), XtRImmediate, (XtPointer) 10},
    {XtNwidth,XtCWidth,XtRDimension,sizeof(Dimension),
	 Offset(core.width), XtRImmediate, (XtPointer) 10},
};

    
/****************************************************************
 *
 * Full class record constant
 *
 ****************************************************************/

static void Initialize();
static void Realize();
static void Resize();
static XtGeometryResult GeometryManager();
static void ChangeManaged();

LayoutClassRec layoutClassRec = {
  {
/* core_class fields      */
    /* superclass         */    (WidgetClass) &compositeClassRec,
    /* class_name         */    "Layout",
    /* widget_size        */    sizeof(LayoutRec),
    /* class_initialize   */    NULL,
    /* class_part_init    */	NULL,
    /* class_inited       */	FALSE,
    /* initialize         */    Initialize,
    /* initialize_hook    */	NULL,
    /* realize            */    Realize,
    /* actions            */    NULL,
    /* num_actions	  */	0,
    /* resources        */      resources,
    /* num_resources    */      XtNumber(resources),
    /* xrm_class          */    NULLQUARK,
    /* compress_motion	  */	TRUE,
    /* compress_exposure  */	TRUE,
    /* compress_enterleave*/	TRUE,
    /* visible_interest   */    FALSE,
    /* destroy            */    NULL,
    /* resize             */    XtInheritResize,
    /* expose             */    NULL,
    /* set_values         */    NULL,
    /* set_values_hook    */	NULL,
    /* set_values_almost  */    XtInheritSetValuesAlmost,
    /* get_values_hook    */	NULL,
    /* accept_focus       */    NULL,
    /* version            */	XtVersion,
    /* callback_private   */    NULL,
    /* tm_table           */    NULL,
    /* query_geometry     */	NULL,
    /* display_accelerator*/	XtInheritDisplayAccelerator,
    /* extension          */	NULL
  },{
/* composite_class fields */
    /* geometry_manager   */    (XtGeometryHandler)GeometryManager,
    /* change_managed     */    ChangeManaged,
    /* insert_child	  */	XtInheritInsertChild,
    /* delete_child	  */	XtInheritDeleteChild,
    /* extension          */	NULL
  },{
/* Layout class fields */
    /* empty		  */	0,
  }
};

WidgetClass layoutWidgetClass = (WidgetClass)&layoutClassRec;


/****************************************************************
 *
 * Private Routines
 *
 ****************************************************************/

/*
 *
 * Geometry Manager
 *
 * lets the child do whatever it wants to
 *
 */

/*ARGSUSED*/
static XtGeometryResult GeometryManager(w, request, reply)
    Widget		w;
    XtWidgetGeometry	*request;
    XtWidgetGeometry	*reply;	/* RETURN */

{
    if (request->request_mode & (CWX | CWY | CWWidth | CWHeight | CWBorderWidth)) 
	{
	if ((request->request_mode & CWWidth) == 0)
	    request->width = w->core.width;
	if ((request->request_mode & CWHeight) == 0)
	    request->height = w->core.height;
        if ((request->request_mode & CWBorderWidth) == 0)
	    request->border_width = w->core.border_width;
	if ((request->request_mode & CWX) == 0)
	    request->x = w->core.x;
	if ((request->request_mode & CWY) == 0)
	    request->y = w->core.y;

	w->core.x = request->x;
	w->core.y = request->y;
	w->core.width = request->width;
	w->core.height = request->height;
	w->core.border_width = request->border_width;
	}

    return (XtGeometryYes);
}


static void ChangeManaged(w)
    Widget w;
{
	/* don't care if anything goes away or not */
}

/* ARGSUSED */
static void Initialize(request, new)
    Widget request, new;
{
    LayoutWidget newlw = (LayoutWidget)new;

    newlw->layout.last_query_mode = CWWidth | CWHeight;
    newlw->layout.last_query_width = newlw->core.width;
    newlw->layout.last_query_height = newlw->core.height;

} /* Initialize */

static void Realize(w, valueMask, attributes)
    register Widget w;
    Mask *valueMask;
    XSetWindowAttributes *attributes;
{
    attributes->bit_gravity = NorthWestGravity;
    *valueMask |= CWBitGravity;

    XtCreateWindow( w, (unsigned)InputOutput, (Visual *)CopyFromParent,
		    *valueMask, attributes);
} /* Realize */

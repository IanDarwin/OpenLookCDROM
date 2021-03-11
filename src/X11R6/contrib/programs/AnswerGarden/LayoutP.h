
/* LayoutP.h */

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

******************************************************************/

/* 
 * LayoutP.h - Private definitions for Layout widget
 * 
 */

#ifndef _LayoutP_h
#define _LayoutP_h

/***********************************************************************
 *
 * Layout Widget Private Data
 *
 ***********************************************************************/

#include "Layout.h"
#include <X11/CompositeP.h>

/* New fields for the Layout widget class record */
typedef struct {int empty;} LayoutClassPart;

/* Full class record declaration */
typedef struct _LayoutClassRec {
    CoreClassPart	core_class;
    CompositeClassPart  composite_class;
    LayoutClassPart	layout_class;
} LayoutClassRec;

extern LayoutClassRec layoutClassRec;

/* New fields for the Layout widget record */
typedef struct {
    Dimension	last_query_width, last_query_height;
    XtGeometryMask last_query_mode;
} LayoutPart;


/****************************************************************
 *
 * Full instance record declaration
 *
 ****************************************************************/

typedef struct _LayoutRec {
    CorePart	    core;
    CompositePart   composite;
    LayoutPart 	    layout;
} LayoutRec;

#endif /* _LayoutP_h */


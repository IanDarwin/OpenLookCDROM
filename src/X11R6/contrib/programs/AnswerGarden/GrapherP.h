
/* GrapherP.h */

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
 * GrapherP.h - Private definitions for Grapher widget
 * 
 */

#ifndef _GrapherP_h
#define _GrapherP_h

/***********************************************************************
 *
 * Grapher Widget Private Data
 *
 ***********************************************************************/

#include "Grapher.h"
#include "LayoutP.h"

typedef struct _Node 
{
    struct _Node *child;
    struct _Node *sibling;
    struct _Node *parent;
    Position x;
    Position y;
    Dimension width;
    Dimension height;
    String label;
    String node_name;
    Widget w;
} Node;

#define NEWLINE '\n'


/* New fields for the Grapher widget class record */
typedef struct {int makes_compiler_happy;} GrapherClassPart;

/* Full class record declaration */
typedef struct _GrapherClassRec {
    CoreClassPart	core_class;
    CompositeClassPart  composite_class;
    LayoutClassPart	layout_class;
    GrapherClassPart    grapher_class;
} GrapherClassRec;

extern GrapherClassRec grapherClassRec;

/* New fields for the Grapher widget record */
typedef struct {
    Node *root;
    Position next_y;
    Dimension x_increment;
    Dimension y_increment;
    String text;
    XtCallbackList callback;
    XtCallbackList edit_callback;
    Cursor cursor;
    Dimension max_width;
    Dimension min_width;
    Dimension max_height;
    Dimension min_height;
    Boolean edit_mode;
    Pixel foreground;
    XtCallbackList dynamic_callback;
#ifndef NO_FUNCOBJ
    Widget func_obj;
#endif

      /* Private */
    int nNodes;
    GC gc;
    Position max_x;
} GrapherPart;


/****************************************************************
 *
 * Full instance record declaration
 *
 ****************************************************************/

typedef struct _GrapherRec {
    CorePart	    core;
    CompositePart   composite;
    LayoutPart 	    layout;
    GrapherPart     grapher;
} GrapherRec;

#endif /* _GrapherP_h */


















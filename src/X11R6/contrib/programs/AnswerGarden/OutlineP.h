
/* OutlineP.h */

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
 * OutlineP.h - Private definitions for Outline widget
 * 
 */

#ifndef _OutlineP_h
#define _OutlineP_h

/***********************************************************************
 *
 * Outline Widget Private Data
 *
 ***********************************************************************/

#include "Outline.h"
#include <X11/Xaw/ListP.h>

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


/* New fields for the Outline widget class record */
typedef struct {int makes_compiler_happy;} OutlineClassPart;

/* Full class record declaration */
typedef struct _OutlineClassRec {
    CoreClassPart	core_class;
    SimpleClassPart  simple_class;
    ListClassPart	list_class;
    OutlineClassPart    outline_class;
} OutlineClassRec;

extern OutlineClassRec outlineClassRec;

/* New fields for the Outline widget record */
typedef struct {
    Node *root;
    Position next_y;
    Dimension x_increment;
    Dimension y_increment;
    String text;
    Cursor cursor;
    Dimension max_width;
    Dimension min_width;
    Dimension max_height;
    Dimension min_height;
    
      /* Private */
    int nNodes;
    Position max_x;
    String *list_strings;
    XtPointer *list_client_data;
} OutlinePart;


/****************************************************************
 *
 * Full instance record declaration
 *
 ****************************************************************/

typedef struct _OutlineRec {
    CorePart	    core;
    SimplePart   simple;
    ListPart 	    list;
    OutlinePart     outline;
} OutlineRec;

#endif /* _OutlineP_h */


















/* NodeOutP.h */

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
 * NodeOutP.h - Private definitions for NodeOutline widget
 * 
 */

#ifndef _NodeOutP_h
#define _NodeOutP_h

/***********************************************************************
 *
 * NodeOutline Widget Private Data
 *
 ***********************************************************************/

#include "NodeOut.h"
#include "OutlineP.h"


/* New fields for the NodeOutline widget class record */
typedef struct {
  int makes_compiler_happy;
} NodeOutlineClassPart;

/* Full class record declaration */
typedef struct _NodeOutlineClassRec {
    CoreClassPart	core_class;
    SimpleClassPart  simple_class;
    ListClassPart	list_class;
    OutlineClassPart    outline_class;
    NodeOutlineClassPart node_outline_class;
} NodeOutlineClassRec; 

extern NodeOutlineClassRec nodeOutlineClassRec;

/* New fields for the NodeOutline widget record */
typedef struct _NodeOutlinePart {
    XtCallbackList edit_callback;
    Boolean edit_mode;
    XtCallbackList dynamic_callback;
    
      /* Private */
    String selection_string;
} NodeOutlinePart;


/****************************************************************
 *
 * Full instance record declaration
 *
 ****************************************************************/

typedef struct _NodeOutlineRec {
    CorePart	    core;
    SimplePart   simple;
    ListPart 	    list;
    OutlinePart     outline;
    NodeOutlinePart node_outline;
} NodeOutlineRec;

#endif /* _NodeOutP_h */


















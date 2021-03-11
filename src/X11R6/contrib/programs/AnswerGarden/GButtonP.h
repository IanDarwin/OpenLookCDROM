/**********************************************************

  The ANSWER GARDEN PROJECT


 * GrapherButtonP.h - Private definitions for GrapherButton widget

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

  Mark Ackerman
  Information and Computer Science
  University of California, Irvine

  formerly -
  MIT/Center for Coordination Science

  ackerman@ics.uci.edu

**********************************************************************/

#ifndef _XtGrapherButtonP_h
#define _XtGrapherButtonP_h

/***********************************************************************
 *
 * Grapher Button Widget Private Data
 *
 ***********************************************************************/

#include "GButton.h"
#include <X11/CoreP.h>
#include <X11/Xaw/CommandP.h>



/* New fields for the GrapherButton widget class record */

typedef struct {int makes_compiler_happy;} GrapherButtonClassPart;

/* Full class record declaration */
typedef struct _GrapherButtonClassRec {
    CoreClassPart	core_class;
    SimpleClassPart	simple_class;
    LabelClassPart      label_class;
    CommandClassPart    command_class;
    GrapherButtonClassPart	grapherButton_class;
} GrapherButtonClassRec;

extern GrapherButtonClassRec grapherButtonClassRec;

/* New fields for the GrapherButton widget record */
typedef struct {
    Boolean edit_mode;
    String  selection_string;

    /* private state */
    Boolean owns_selection;
} GrapherButtonPart;


/****************************************************************
 *
 * Full instance record declaration
 *
 ****************************************************************/

typedef struct _GrapherButtonRec {
    CorePart	core;
    SimplePart	simple;
    LabelPart   label;
    CommandPart  command;
    GrapherButtonPart	grapherButton;
} GrapherButtonRec;



#endif /* _XtGrapherButtonP_h */

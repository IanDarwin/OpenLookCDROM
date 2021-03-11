/**********************************************************

  The ANSWER GARDEN PROJECT

  GBUTTON.H      Grapher Button, "Towards A Better Button"

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

  GButton.h


  Mark Ackerman
  Information and Computer Science
  University of California, Irvine
  
  formerly -
  MIT/Center for Coordination Science
  
  ackerman@ics.uci.edu
**********************************************************************/

#ifndef _XtGrapherButton_h
#define _XtGrapherButton_h

/***********************************************************************
 *
 * Grapher Button Widget
 *
 *
 ***********************************************************************/

#include <X11/Xaw/Command.h>

#define XtNselectionString "selectionString"
#define XtNeditMode "editMode"

#define XtCEditMode "EditMode"

#define AGGrapherButtonEditNotify 0
#define AGGrapherButtonNodeNotify 1
#define AGGrapherButtonDynamicNotify 2

typedef struct {
  XEvent *event;
  int notify_type;
  String selection_string;
  String *params;
  int num_params;
} AGGrapherButtonCallbackStruct;

 
/* Class record constants */

extern WidgetClass grapherButtonWidgetClass;

typedef struct _GrapherButtonClassRec *GrapherButtonWidgetClass;
typedef struct _GrapherButtonRec      *GrapherButtonWidget;

#endif /* _XtGrapherButton_h */
/* DON'T ADD STUFF AFTER THIS #endif */


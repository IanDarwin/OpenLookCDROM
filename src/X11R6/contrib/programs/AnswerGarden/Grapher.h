
/* Grapher.h */

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

#ifndef _Grapher_h
#define _Grapher_h

/***********************************************************************
 *
 * Grapher Widget (subclass of CompositeClass)
 *
 ***********************************************************************/

/* Parameters:

 Name		     Class		RepType		Default Value
 ----		     -----		-------		-------------
 background	     Background		Pixel		XtDefaultBackground
 border		     BorderColor	Pixel		XtDefaultForeground
 borderWidth	     BorderWidth	Dimension	1
 destroyCallback     Callback		Pointer		NULL
 height		     Height		Dimension	0
 mappedWhenManaged   MappedWhenManaged	Boolean		True
 width		     Width		Dimension	0
 x		     Position		Position	0
 y		     Position		Position	0

*/

#include "GButton.h"

#define XtNxIncrement "xIncrement"
#define XtNyIncrement "yIncrement"
#define XtNnextY "nextY"
#define XtNnodeCallback "nodeCallback"
#define XtNtext "text"
#define XtNrootNode "rootNode"
#ifndef _XtShell_h
#define XtNminHeight "minHeight"
#define XtNmaxHeight "maxHeight"
#define XtNminWidth "minWidth"
#define XtNmaxWidth "maxWidth"
#endif
#define XtNeditMode "editMode"
#define XtNeditCallback "editCallback"
#define XtNdynamicCallback "dynamicCallback"
#ifndef NO_FUNCOBJ
#define XtNfuncObj "funcObj"
#endif


#define XtCIncrement "Increment"
#define XtCNextY "NextY"
#ifndef _AnswerG_h
#define XtCText "Text"
#endif
#define XtCRootNode "RootNode"
#define XtCEditMode "EditMode"
#define XtCEditCallback "EditCallback"
#define XtCDynamicCallback "DynamicCallback"
#ifndef NO_FUNCOBJ
#define XtCWidget "Widget"
#endif

typedef struct {
  AGGrapherButtonCallbackStruct *grapherButton_callback_struct;
  char *node_name;
}  AGGrapherCallbackStruct;



extern WidgetClass grapherWidgetClass;

typedef struct _GrapherClassRec *GrapherWidgetClass;
typedef struct _GrapherRec      *GrapherWidget;

#endif /* _Grapher_h */


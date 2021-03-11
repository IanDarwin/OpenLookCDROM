
/* Outline.h */

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

#ifndef _Outline_h
#define _Outline_h

/***********************************************************************
 *
 * Outline Widget (subclass of CompositeClass)
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
#include <X11/Xaw/List.h>

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
#define XtNnumberNodes "numberNodes"

#define XtCIncrement "Increment"
#define XtCNextY "NextY"
#ifndef _AnswerG_h
#define XtCText "Text"
#endif
#define XtCRootNode "RootNode"
#define XtCNumberNodes "NumberNodes"

#ifdef NEXT
typedef struct {
  AGGrapherButtonCallbackStruct *outlineButton_callback_struct;
  char *node_name;
}  AGOutlineCallbackStruct;

#endif

extern WidgetClass outlineWidgetClass;

typedef struct _OutlineClassRec *OutlineWidgetClass;
typedef struct _OutlineRec      *OutlineWidget;

#endif /* _Outline_h */


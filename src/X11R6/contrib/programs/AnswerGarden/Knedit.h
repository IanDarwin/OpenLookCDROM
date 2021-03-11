
/* Knedit.h */
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

#ifndef _Knedit_h
#define _Knedit_h

/***********************************************************************
 *
 * Knedit Widget (subclass of CompositeClass)
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

/* Class record constants */

#define XtNlineHeight "lineHeight"
#define XtNfilename "filename"
#define XtNleftMargin "leftMargin"
#define XtNrightMargin "rightMargin"
#define XtNtopMargin "topMargin"
#define XtNboldFont "boldFont"
#define XtNitalicFont "italicFont"
#define XtNinternalButtonCallback "internalButtonCallback"
#define XtNeditCallback "editCallback"
#define XtNtotalHeight "totalHeight"
#define XtNforceSize "forceSize"
#define XtNeditMode "editMode"
#define XtNnodeName "nodeName"
#define XtNseparatorHeight "separatorHeight"
#define XtNdynamicCallback "dynamicCallback"
#define XtNfuncObj "funcObj"

#define XtCLineHeight "LineHeight"
#define XtCFilename "Filename"
#define XtCForceSize "ForceSize"
#define XtCEditMode "EditMode"
#define XtCNodeName "NodeName"
#define XtCWidget "Widget"

typedef struct _AGKneditCallbackStruct {
  AGGrapherButtonCallbackStruct *grapherButton_callback_struct;
  char *node_name;
}  AGKneditCallbackStruct;


extern WidgetClass kneditWidgetClass;

typedef struct _KneditClassRec *KneditWidgetClass;
typedef struct _KneditRec      *KneditWidget;

#endif /* _Knedit_h */


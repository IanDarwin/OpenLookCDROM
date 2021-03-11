/**********************************************************

  The ANSWER GARDEN PROJECT

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

***********************************************************/
#ifndef _StackObj_h
#define _StackObj_h

#include <X11/Object.h>

#define XtNvertDistance "vertDistance"
#define XtNhorizDistance "horizDistance"
#define XtNmaxStackSize "maxStackSize"
#define XtNlockingEnabled "lockingEnabled"
#define XtNmaxMostRecentList "maxMostRecentList"
#ifndef _AnswerG_h
#define XtNbackCallback "backCallback"
#endif
#define XtNmaxOnScreen "maxOnScreen"
#define XtNautoRemovalCallback "autoRemovalCallback"
#define XtNautoRemovalDelete "autoRemovalDelete"

#define XtCMaxListSize "MaxListSize"
#define XtCLockingEnabled "LockingEnabled"
#define XtCAutoRemovalDelete "AutoRemovalDelete"

typedef struct _StackObjRec *StackObj;
typedef struct _StackObjClassRec *StackObjClass;
extern WidgetClass stackObjClass;

#endif /* _StackObj_h */
/* DON'T ADD STUFF AFTER THIS #endif */

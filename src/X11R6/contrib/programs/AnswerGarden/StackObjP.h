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
#ifndef _StackObjP_h_
#define _StackObjP_h_

#include <X11/ObjectP.h>
#include "StackObj.h"


#define MAXCHILDREN    64
#define MAXHISTORY     64

/**********************************************************
 * StackObj Instance Data Structures
 *
 **********************************************************/


typedef struct _StackObjPart {
  /* resources */
  Dimension             dx;
  Dimension             dy;
  Position              x;
  Position              y;
  XtCallbackList        back_callback;
  int                   max_most_recent_list;
  int                   max_stack_size;
  int                   max_on_screen;
  Boolean               locking_enabled;
  XtCallbackList        auto_removal_callback;
  Boolean               auto_removal_delete;
  
  /* private */
  Position              initial_x;
  Position              initial_y;
  Position              current_x;
  Position              current_y;
  Widget                *children;
  int                   nchildren;
  Boolean               *locked;
  XtPointer             *id_data;
  XtPointer             *most_recent;
  int                   num_most_recent;


} StackObjPart;

typedef struct _StackObjRec {
    ObjectPart   object;
    StackObjPart  stackObj;
} StackObjRec;

/********************************************************
 * StackObj Class Data Structures
 *
 ********************************************************/


typedef struct _StackObjClassPart {
  char *makes_compiler_happy;
}StackObjClassPart;



typedef struct _StackObjClassRec {
  ObjectClassPart  object_class;
  StackObjClassPart stackObj_class;
} StackObjClassRec;

extern StackObjClassRec stackObjClassRec;

#endif /*_StackObjP_h_*/



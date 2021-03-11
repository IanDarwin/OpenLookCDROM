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
#ifndef _FuncObjP_h_
#define _FuncObjP_h_

#include <X11/ObjectP.h>
#include "FuncObj.h"




typedef struct _BlackboardEntry
{
  XrmQuark  entry_quark;
  union 
    {
      String *entry;
      XtPointer *proc;
    } innards;
} BlackboardEntry;


/**********************************************************
 * FuncObj Instance Data Structures
 *
 **********************************************************/


typedef struct _FuncObjPart {
  /* resources */
  Boolean               use_instance_blackboard;
  XtCallbackList        crush_callback;
  XtCallbackList        dynamic_callback;
  BlackboardEntry       *blackboard;
  XtPointer             substitution_table; /* MSA 5/13/94 */
  int                   substitution_table_number; /* MSA 5/13/94 */

  /* private */
  int                   num_entries;
} FuncObjPart;

typedef struct _FuncObjRec {
    ObjectPart   object;
    FuncObjPart  funcObj;
} FuncObjRec;

/********************************************************
 * FuncObj Class Data Structures
 *
 ********************************************************/


typedef struct _FuncObjClassPart {
  void                  (*external_handle)();
  int                   (*parse_command)();
  Boolean               (*trigger_command)();
  void                  (*destroy_parsing_tree)();
  Boolean               first_flag;
  BlackboardEntry       *blackboard;
  int                   num_entries;
}FuncObjClassPart;

#define XtInheritExternalHandle ((void (*)())_XtInherit)
#define XtInheritParseCommand ((int (*)())_XtInherit)
#define XtInheritTriggerCommand ((Boolean (*)())_XtInherit)
#define XtInheritDestroyParsingTree ((void (*)())_XtInherit)


typedef struct _FuncObjClassRec {
  ObjectClassPart  object_class;
  FuncObjClassPart funcObj_class;
} FuncObjClassRec;

extern FuncObjClassRec funcObjClassRec;

#endif /*_FuncObjP_h_*/

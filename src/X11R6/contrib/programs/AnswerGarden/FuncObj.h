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
#ifndef _FuncObj_h
#define _FuncObj_h

#include <X11/Object.h>

typedef struct _FuncObjRec *FuncObj;
typedef struct _FuncObjClassRec *FuncObjClass;
extern WidgetClass funcObjClass;


#define XtNcrushCallback "crushCallback"
#define XtNdynamicCallback "dynamicCallback"
#define XtNuseInstanceBlackboard "useInstanceBlackboard"
#define XtNblackboard "blackboard"

#define XtNsubstitutionTableNumber "substitutionTableNumber"
#define XtNsubstitutionTable "substitutionTable"

#define XtCUseInstanceBlackboard "UseInstanceBlackboard"
#define XtCBlackboard "Blackboard"
#define XtCSubstitutionTable "SubstitutionTable"
#define XtCSubstitutionTableNumber "SubstitutionTableNumber"

#define XtRBlackboard "Blackboard"

typedef struct  {
    int nparams;
    String *params;
  } FuncObjReturnStruct;

#define FUNC_MAXCHILDREN  16


typedef struct _FuncParsingTree
{
  Boolean is_terminal;
  union
    {
      String terminal_token;
      struct _FuncParsingTreeSub
	{
	  int command;
	  struct _FuncParsingTree *children[FUNC_MAXCHILDREN];
	  int nchildren;
	} non_terminal;
    } innards;
} FuncParsingTree;


/* MSA 5/13/94 */
typedef struct _FuncSubstitutionRec
{
  char *old_string;
  char *new_string;
} FuncSubstitutionRec, *FuncSubstitutionTable;



void AGFuncObjHandle();

#endif /* _FuncObj_h */
/* DON'T ADD STUFF AFTER THIS #endif */

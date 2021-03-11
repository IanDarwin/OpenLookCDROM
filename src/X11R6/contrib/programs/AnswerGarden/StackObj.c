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

  092092     MSA        Massive changes for locking.

***********************************************************/


/* 
 * StackObj.c - StackObj object
 * 
 */

#include	<X11/IntrinsicP.h>
#include	<X11/StringDefs.h>
#include	"StackObjP.h"
#include        "AGmacros.h"


/****************************************************************
 *
 * StackObj Resources
 *
 ****************************************************************/

#define offset(field) XtOffset(StackObj, field)
static XtResource resources[] = {
  {XtNx, XtCX, XtRPosition, sizeof(Position),
     offset(stackObj.x),XtRImmediate,(XtPointer)0},
  {XtNy, XtCY, XtRPosition, sizeof(Position),
     offset(stackObj.y),XtRImmediate,(XtPointer)0},  
  {XtNhorizDistance, XtCThickness, XtRDimension, sizeof(Dimension),
     offset(stackObj.dx), XtRImmediate, (XtPointer) 10},
  {XtNvertDistance, XtCThickness, XtRDimension, sizeof(Dimension),
     offset(stackObj.dy), XtRImmediate, (XtPointer) 20},
  {XtNmaxStackSize, XtCMaxListSize, XtRInt, sizeof(int),
     offset(stackObj.max_stack_size), XtRImmediate, (XtPointer) 128},
  {XtNlockingEnabled, XtCLockingEnabled, XtRBoolean, sizeof(Boolean),
     offset(stackObj.locking_enabled), XtRImmediate, (XtPointer) False},
  {XtNmaxMostRecentList, XtCMaxListSize, XtRInt, sizeof(int),
     offset(stackObj.max_most_recent_list), XtRImmediate, (XtPointer) 32},
  {XtNmaxOnScreen, XtCMaxListSize, XtRInt, sizeof(int),
     offset(stackObj.max_on_screen), XtRImmediate, (XtPointer) 3},
  {XtNbackCallback,XtCCallback,XtRCallback,sizeof(XtPointer),
     offset(stackObj.back_callback),XtRCallback,NULL},
  {XtNautoRemovalCallback,XtCCallback,XtRCallback,sizeof(XtPointer),
     offset(stackObj.auto_removal_callback),XtRCallback,NULL},
  {XtNautoRemovalDelete, XtCAutoRemovalDelete, XtRBoolean, sizeof(Boolean),
     offset(stackObj.auto_removal_delete),XtRImmediate,(XtPointer) False},
};


    
/****************************************************************
 *
 * Full class record constant
 *
 ****************************************************************/

static void Initialize();


#define SuperClass ((WidgetClass)&objectClassRec)

StackObjClassRec stackObjClassRec = {
{
  
  /* superclass	        */	SuperClass,
  /* class_name	        */	"StackObj",
  /* widget_size	*/	sizeof(StackObjRec),
  /* class_initialize   */    NULL,
  /* class_part_initialize*/	NULL,
  /* class_inited       */	FALSE,
  /* initialize	        */	Initialize,
  /* initialize_hook    */	NULL,		
  /* pad                */    NULL,
  /* pad		  */	NULL,
  /* pad       	        */	0,
  /* resources	       */	resources,
  /* num_resources	*/      XtNumber(resources),
  /* xrm_class	        */	NULLQUARK,
  /* pad                */      FALSE,
  /* pad                */      FALSE,
  /* pad                */      FALSE,
  /* pad                */      FALSE,
  /* destroy		*/	NULL,
  /* pad		*/	NULL,
  /* pad		*/	NULL,
  /* set_values	        */      NULL,
  /* set_values_hook    */	NULL,			
  /* pad                */      NULL,
  /* get_values_hook    */	NULL,			
  /* pad                */      NULL,
  /* version		*/	XtVersion,
  /* callback_offsets   */      NULL,
  /* pad                */      NULL,
  /* pad                */      NULL,
  /* pad                */      NULL,
  /* extension	    */  NULL
},
{
  NULL, /* makes_compiler_happy */
}
};

WidgetClass stackObjClass = (WidgetClass)&stackObjClassRec;



/****************************************************************
 *
 * Private Routines
 *
 ****************************************************************/


static void Initialize(request, new)
    Widget request, new;
{
  int i;
  StackObj so = (StackObj)new;
  
  so->stackObj.initial_x = so->stackObj.x;
  so->stackObj.initial_y = so->stackObj.y;
  so->stackObj.current_x = so->stackObj.x;
  so->stackObj.current_y = so->stackObj.y;

  so->stackObj.nchildren = -1;
  so->stackObj.num_most_recent = -1;
  if (( so->stackObj.children = (Widget *)
       XtMalloc(sizeof(Widget)*so->stackObj.max_stack_size)) == NULL)
    XtWarning("unable to allocate memory in window stack.  Continuing.");
  if (so->stackObj.locking_enabled)
    {
      if (( so->stackObj.locked = (Boolean *)
	   XtMalloc(sizeof(Boolean)*so->stackObj.max_stack_size)) == NULL)
	XtWarning("unable to allocate memory in window stack.  Continuing.");
      if (( so->stackObj.id_data = (XtPointer *)
	   XtMalloc(sizeof(XtPointer)*so->stackObj.max_stack_size)) == NULL)
	XtWarning("unable to allocate memory in window stack.  Continuing.");
      if (( so->stackObj.most_recent = (XtPointer *)
	   XtMalloc(sizeof(XtPointer)*so->stackObj.max_most_recent_list)) 
	  == NULL)
	XtWarning("unable to allocate memory in window stack.  Continuing.");
    }
  else
    so->stackObj.max_on_screen = so->stackObj.max_stack_size;

}

AG_Stack_Add_Shell(widget,shell,x,y,id_data,locked)
     Widget widget;
     Widget shell;
     Position *x;
     Position *y;
     XtPointer id_data;
     Boolean locked;
{  
  char msgbuf[512];
  int i;

  StackObj so = (StackObj)widget;

  
  if (shell == NULL || so == NULL)
    {
      Util_Debug("bad stack obj or bad shell in AGStackAddShell");
      return;
    }


  if (so->stackObj.nchildren < so->stackObj.max_on_screen-1) 
    {
	(so->stackObj.nchildren)++;
    }
  else
    {
      if (so->stackObj.locking_enabled)
	{
	  for (i=0;i <= so->stackObj.nchildren;i++)
	    if (!so->stackObj.locked[i]) /* find the first one not locked */
	      {
		XtCallCallbackList((Widget)so,
				   so->stackObj.auto_removal_callback,
				   (XtPointer)so->stackObj.children[i]);
		if (so->stackObj.auto_removal_delete)
		  AG_Stack_Delete_Shell(so,so->stackObj.children[i]);
		break;
	      }
	   if (so->stackObj.nchildren < so->stackObj.max_stack_size)
	     (so->stackObj.nchildren)++;
	}
      else
	{ /**062592**/
	  sprintf(msgbuf,"window overflow in stack.  ");
	  XtWarning(msgbuf);
	  strcpy(msgbuf,"\n\tGeometry may get \"interesting\"\n");
	  XtWarning(msgbuf);
	}
    }

  
  so->stackObj.children[so->stackObj.nchildren] = shell;
  if (so->stackObj.locking_enabled)
    {
       /* id_data is NodeInfo when used from AG.c */
      so->stackObj.id_data[so->stackObj.nchildren] = id_data;
      so->stackObj.locked[so->stackObj.nchildren] = locked;
    }
  
  *x =  so->stackObj.current_x = so->stackObj.current_x + so->stackObj.dx;
  *y =  so->stackObj.current_y = so->stackObj.current_y + so->stackObj.dy;
  
}

/**092192**/
Boolean AG_Stack_Set_Locked(widget,shell,locked)
     Widget widget;
     Widget shell;
     Boolean locked;
{
  int i;
  StackObj so = (StackObj)widget;

  if (so == NULL || shell == NULL)
    return(False);
  if (!so->stackObj.locking_enabled)
    return(False);

  for (i=0;i<=so->stackObj.nchildren;i++)
    if (shell == so->stackObj.children[i])
      {
	so->stackObj.locked[i] = locked;
	return(True);
      }
  return(False);
}

Boolean AG_Stack_Get_Locked(widget,shell)
     Widget widget;
     Widget shell;
{
  int i;
  StackObj so = (StackObj)widget;
  
  if (so == NULL || shell == NULL)
    return(False);

  if (!so->stackObj.locking_enabled)
    return(False);

  for (i=0;i<=so->stackObj.nchildren;i++)
    if (shell == so->stackObj.children[i])
      {
	return(	so->stackObj.locked[i] );
      }
  return(False);
}


/** 062592 **/
XtPointer AG_Stack_Go_Back(widget)
     Widget widget;
{
  int current_loc;
  XtPointer value;
  StackObj so = (StackObj)widget;

  if (so == NULL)
    {
      Util_Debug("bad stack obj or bad shell in AGStackGoBack");
      return(NULL);
    }
  if (!so->stackObj.locking_enabled)
    {
      Util_Debug("no locking enabled AGStackGoBack");
      return(NULL);
    }

  if (so->stackObj.num_most_recent < 0)
    return(NULL); /* nothing on stack */ /* 10/19/92 */

  /* return the shell to the callback so it can do what it wants */
  value = so->stackObj.most_recent[so->stackObj.num_most_recent];
  /* make it clean so the callback can call AG_Stack_Add */
  so->stackObj.num_most_recent--;

  /* probably won't be needed, but if it is ... */
  XtCallCallbackList((Widget)so,so->stackObj.back_callback,(XtPointer)value);

  return(value);
}
  


AG_Stack_Delete_Shell(widget,shell)
     Widget widget;
     Widget shell;
{
  int i;
  int j;
  StackObj so = (StackObj)widget;

  if (so == NULL || shell == NULL)
    {
      Util_Debug("bad stack obj or bad shell in AGStackDelete");
      return;
    }
  for (i=0;i<=so->stackObj.nchildren;i++)
    if (so->stackObj.children[i] == shell)
      {
	if (so->stackObj.locking_enabled)
	  {
	    /* most recent contains the id_data which in the AG.c
	       call of the Stack routines is NodeInfo 10/17/92 */
	    if (so->stackObj.num_most_recent == 
		so->stackObj.max_most_recent_list-1)
	      for (j=0;j<so->stackObj.num_most_recent;j++)
		so->stackObj.most_recent[j] = so->stackObj.most_recent[j+1];
	    else
	      so->stackObj.num_most_recent++;
	    so->stackObj.most_recent[so->stackObj.num_most_recent] = 
	      so->stackObj.id_data[i];
	  }

	if (i == so->stackObj.nchildren) /* it's the last one */
	  {
	    so->stackObj.current_x = so->stackObj.current_x - so->stackObj.dx;
	    so->stackObj.current_y = so->stackObj.current_y - so->stackObj.dy;
	  }
	else
	  {
	    for (j=i;j<so->stackObj.nchildren;j++)
	      so->stackObj.children[j] = so->stackObj.children[j+1];
	    if (so->stackObj.locking_enabled)
	      {
		for (j=i;j<so->stackObj.nchildren;j++)
		  so->stackObj.locked[j] = so->stackObj.locked[j+1];
		for (j=i;j<so->stackObj.nchildren;j++)
		  so->stackObj.id_data[j] = so->stackObj.id_data[j+1];
	      }
	  }
	if (so->stackObj.nchildren >= 0)  /* 5/8/92 */
	  so->stackObj.nchildren--;
	break;
      }

  return;
}

AG_Stack_Tidy(widget)
     Widget widget;
{
  int i;
  Position curx;
  Position cury;
  Window window;
  Window parent;
  Window root;
  Window *children;
  unsigned int nchildren;
  Widget shell;
  StackObj so = (StackObj)widget;

  if (so == NULL)
    {
      Util_Debug("bad stack obj in AGStackTidy");
      return;
    }
  
  curx = so->stackObj.initial_x;
  cury = so->stackObj.initial_y;

  for (i=0;i<=so->stackObj.nchildren;i++)
    {
      shell = so->stackObj.children[i];
      if (!XQueryTree(XtDisplay(shell), XtWindow(shell),&root,
		      &parent,&children,&nchildren))
	return;  /* something is wrong */
      while (parent != root)
	{
	  window = parent;
	  XFree((char *)children);
	  if (!XQueryTree(XtDisplay(shell), window,&root,
			  &parent,&children, &nchildren))
	    {
	      Util_Debug("unclear state in AGRaiseShell");
	      return; /* something is wrong */
	    }
	}
      XFree((char *)children);
        /* This is a brute force way of doing it.  I cannot for hours
	   of trying move my popups by doing a SetValues */
      XMoveWindow(XtDisplay(shell),window,curx,cury);
      XRaiseWindow(XtDisplay(shell),window);
      so->stackObj.current_x = curx;
      so->stackObj.current_y = cury;
      curx = curx + so->stackObj.dx;
      cury = cury + so->stackObj.dy;
    }

}




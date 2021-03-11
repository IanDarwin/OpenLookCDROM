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

  Mark Ackerman
  Information and Computer Science
  University of California, Irvine
  
  formerly -
  MIT/Center for Coordination Science
  
  ackerman@ics.uci.edu

******************************************************************/


/*
 * GrapherButton.c - A super duper special version of the command button
 *
 */

#include <stdio.h>
#include <X11/IntrinsicP.h>
#include <X11/StringDefs.h>
#include <X11/Xatom.h>
#include <X11/Xmu/Xmu.h>
#include "GButtonP.h"
#include "AGmacros.h"


#define MaxSelectionToken 256

/****************************************************************
 *
 * Full class record constant
 *
 ****************************************************************/

/* Private Data */

static char defaultTranslations[] =
    "<EnterWindow>:	highlight()		\n\
     <LeaveWindow>:	reset()			\n";


#ifdef OLD
     "<Btn1Down>:	set()			\n\
     <Btn1Up>:		grapher_button_node_notify() unset()	\n\
     <Btn2Down>:        set()                   \n\
     <Btn2Up>:          grapher_button_edit_notify() unset()     \n\
     <Btn3Down>:        set()                   \n\
     <Btn3Up>:          grapher_button_cut_selection_string() unset()    ";
#endif

  /* Nice long names for the action tree. */
static void GrapherButtonCutSelectionString();
static void GrapherButtonNodeNotify();
static void GrapherButtonEditNotify();
static void GrapherButtonDynamicNotify();

   /* For edit mode */
static XtActionsRec actionsList[] = 
{   
  {"grapher_button_cut_selection_string", GrapherButtonCutSelectionString},
  {"grapher_button_node_notify",                 GrapherButtonNodeNotify},
  {"grapher_button_edit_notify",                GrapherButtonEditNotify},
  {"grapher_button_dynamic_notify",                GrapherButtonDynamicNotify},
};


#define offset(field) XtOffset(GrapherButtonWidget, field)
static XtResource resources[] = { 

   {XtNeditMode, XtCEditMode, XtRBoolean, sizeof(Boolean), 
      offset(grapherButton.edit_mode), XtRString, (XtPointer)"False"}, 
   {XtNselectionString, XtCSelection, XtRString, sizeof(String), 
      offset(grapherButton.selection_string), XtRString, (XtPointer)NULL},
};
#undef offset

static void Initialize(), Redisplay();
static void Destroy();

#define SuperClass ((CommandWidgetClass)&commandClassRec)

GrapherButtonClassRec grapherButtonClassRec = {
  {
    (WidgetClass) SuperClass,		/* superclass		  */	
    "GrapherButton",			/* class_name		  */
    sizeof(GrapherButtonRec),		/* size			  */
    NULL,        			/* class_initialize	  */
    NULL,				/* class_part_initialize  */
    FALSE,				/* class_inited		  */
    Initialize,				/* initialize		  */
    NULL,				/* initialize_hook	  */
    XtInheritRealize,			/* realize		  */
    actionsList,			/* actions		  */
    XtNumber(actionsList),		/* num_actions		  */
    resources,				/* resources		  */
    XtNumber(resources),		/* resource_count	  */
    NULLQUARK,				/* xrm_class		  */
    FALSE,				/* compress_motion	  */
    TRUE,				/* compress_exposure	  */
    TRUE,				/* compress_enterleave    */
    FALSE,				/* visible_interest	  */
    Destroy,				/* destroy		  */
    XtInheritResize,			/* resize		  */
    Redisplay,		        	/* expose		  */
    NULL,				/* set_values		  */
    NULL,				/* set_values_hook	  */
    XtInheritSetValuesAlmost,		/* set_values_almost	  */
    NULL,				/* get_values_hook	  */
    NULL,				/* accept_focus		  */
    XtVersion,				/* version		  */
    NULL,				/* callback_private	  */
    defaultTranslations,		/* tm_table		  */
    XtInheritQueryGeometry,	        /* query_geometry	  */
    XtInheritDisplayAccelerator,	/* display_accelerator	  */
    NULL				/* extension		  */
  },  /* CoreClass fields initialization */
  {
    XtInheritChangeSensitive		/* change_sensitive	*/
  },  /* SimpleClass fields initialization */
  {
    0,                                     /* field not used    */
  },  /* LabelClass fields initialization */
  {
    0,                                     /* field not used    */
  },  /* CommandClass fields initialization */
  {
    0,                                     /* field not used    */
  },  /* GrapherbuttonClass fields initialization */
};

  /* for public consumption */
WidgetClass grapherButtonWidgetClass = (WidgetClass) &grapherButtonClassRec;

static void Initialize(request, new, args, num_args)
     Widget request, new;
     ArgList args;			/* unused */
     Cardinal *num_args;		/* unused */
{

  GrapherButtonWidget gbw = (GrapherButtonWidget)new;
  gbw->grapherButton.owns_selection = False;

  if (gbw->grapherButton.selection_string)
	gbw->grapherButton.selection_string = 
	  XtNewString(gbw->grapherButton.selection_string);

}

static void Destroy(w)
     Widget w;
{
  GrapherButtonWidget gbw = (GrapherButtonWidget)w;
  XtFree(gbw->grapherButton.selection_string);
}

static void GrapherButton_Lose_Selection(w, selection)
     Widget w;
     Atom *selection;
{
  GrapherButtonWidget gbw = (GrapherButtonWidget)w;
  
  if (*selection == XA_PRIMARY && gbw->grapherButton.owns_selection)
    {
      gbw->grapherButton.owns_selection = False;
    }
}

Boolean GrapherButton_Convert_Selection (w, selection, target, type, value, 
			       length,format)
     Widget w;
     Atom *selection;
     Atom *target;
     Atom *type;
     XtPointer *value;
     unsigned long *length;
     int *format;
{
  GrapherButtonWidget gbw = (GrapherButtonWidget)w;
  Atom *target_ptr;
  char *buffer;
  if (*selection == XA_PRIMARY) 
      {
	  
	  if (*target == XA_TARGETS(XtDisplay(w))) 
	      {
		  
		  *value = XtMalloc(sizeof(Atom) * 2);
		  target_ptr = *(Atom**)value;
		  
		  *target_ptr++ = XA_STRING;
		  *target_ptr++ = XA_TEXT(XtDisplay(w));
		  *type = XA_ATOM;
		  *length = 2;
		  *format = 32;
		  return (True);
	      }
	  
	  if (*target == XA_STRING || *target == XA_TEXT(XtDisplay(w))) 
	      {
		  *type = XA_STRING;
		  buffer = XtMalloc(sizeof(char) * MaxSelectionToken);
		  sprintf(buffer,"%s",gbw->grapherButton.selection_string);
		  *value = (XtPointer)buffer;
		  *length = AGstrlen(buffer);
		  *format = 8;
		  return (True);
	      }
	  return (False);

#ifdef NEVER  
	  if (*target == XA_STRING || *target == XA_TEXT(XtDisplay(w))) 
	      {
		  *type = XA_STRING;
		  buffer = XtMalloc(sizeof(char) * MaxSelectionToken);
		  *value = (XtPointer)buffer;
		  
		  *length = AGstrlen(buffer);  
		  *format = 8;
		  return (True);
	      }
	  return (False);
#endif
      }
}

static void GrapherButtonCutSelectionString(w,xevent,params,num_params)
     Widget w;
     XEvent *xevent;
     String *params;		/* unused */
     Cardinal *num_params;
{
    GrapherButtonWidget gbw = (GrapherButtonWidget)w;

    if (gbw->grapherButton.edit_mode && !gbw->grapherButton.owns_selection)
	gbw->grapherButton.owns_selection =  XtOwnSelection(w,XA_PRIMARY,
					      xevent->xbutton.time,
					      GrapherButton_Convert_Selection,
					      GrapherButton_Lose_Selection, 
					      NULL);


}



#define SetCallbackStruct(reason) \
  callback_struct.event = event; \
  callback_struct.notify_type = reason; \
  callback_struct.params = params; \
  callback_struct.num_params = *num_params; \
  callback_struct.selection_string = gbw->grapherButton.selection_string; 


#define NotifyMacro \
  if (gbw->command.set) \
	  XtCallCallbackList(w, gbw->command.callbacks, \
			     (XtPointer)&callback_struct);


static void GrapherButtonNodeNotify(w,event,params,num_params)
     Widget w;
     XEvent *event;
     String *params;	       
     Cardinal *num_params;	
{
  AGGrapherButtonCallbackStruct callback_struct;
  GrapherButtonWidget gbw = (GrapherButtonWidget)w; 

  /* Check left in so command user interface syntax stays the
     same, only adding call_data.  -- MSA */
  /* check to be sure state is still Set so that user can cancel
     the action (e.g. by moving outside the window, in the default
     bindings.
  */
  SetCallbackStruct(AGGrapherButtonNodeNotify);
  NotifyMacro
}

  /* Why two?  (Why not just pass along the event pointer?)
     So, the user can rebind the keyboard and mouse
     bindings.*/
static void GrapherButtonEditNotify(w,event,params,num_params)
     Widget w;
     XEvent *event;
     String *params;		
     Cardinal *num_params;	
{
  AGGrapherButtonCallbackStruct callback_struct;
  GrapherButtonWidget gbw = (GrapherButtonWidget)w; 

  /* if editMode not true, then don't trigger this */
  if (!gbw->grapherButton.edit_mode)
    return;

  SetCallbackStruct(AGGrapherButtonEditNotify);
  NotifyMacro
}


static void GrapherButtonDynamicNotify(w,event,params,num_params)
     Widget w;
     XEvent *event;
     String *params;		
     Cardinal *num_params;	
{
  AGGrapherButtonCallbackStruct callback_struct;
  GrapherButtonWidget gbw = (GrapherButtonWidget)w; 

  SetCallbackStruct(AGGrapherButtonDynamicNotify);
  NotifyMacro
}


static void Redisplay(w, event, region)
     Widget w;
     XEvent *event;
     Region region;
{
    (SuperClass->core_class.expose)(w,event,region);
}


/*********************************************************

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

  AG-OUTLINE

  A simple outline for trees, specialized for Answer Garden

  Mark Ackerman
  MIT/Center for Coordination Technology
  July, 1992
                    
*********************************************************/


#include        <stdio.h>
#include	<X11/IntrinsicP.h>
#include	<X11/StringDefs.h>
#include        <X11/Xatom.h>
#include        <X11/Xmu/Xmu.h>
#include	"NodeOutP.h"
#include        "GButton.h"
#include        "Grapher.h"
#include        "AGmacros.h"


/****************************************************************
 *
 * Outline Resources
 *
 ****************************************************************/


#define offset(field) XtOffset(NodeOutlineWidget, field)
static XtResource resources[] = { 
    {XtNeditCallback,XtCCallback,XtRCallback,sizeof(XtPointer),
	 offset(node_outline.edit_callback),XtRCallback,NULL},
    {XtNdynamicCallback,XtCCallback,XtRCallback,sizeof(XtPointer),
	 offset(node_outline.dynamic_callback),XtRCallback,NULL},
    {XtNeditMode,XtCEditMode,XtRBoolean,sizeof(Boolean),
	 offset(node_outline.edit_mode),XtRImmediate,False},
};
#undef offset

static void Node_Outline_Node();
static void Node_Outline_Edit_Notify();
static void Node_Outline_Dynamic_Notify();
static void Node_Outline_Cut_Selection_String();
static void Node_Outline_Node_Notify();

static char defaultTranslations[] =  
  "Ctrl<Btn1Down>:   Set() NodeOutlineNodeNotify() \n\
   Ctrl<Btn1Up>:     Unset() \n\
   Ctrl<Btn2Down>:   Set() NodeOutlineEditNotify() \
              		XawPositionSimpleMenu(editMenuShell) \
			XtMenuPopup(editMenuShell) \n\
   Ctrl<Btn2Up>:     Unset() \n\
   Ctrl<Btn3Down>:   Set() NodeOutlineCutSelectionString() \n\
   Ctrl<Btn3Up>:     Unset()\n\
   <BtnDown>:   Set() NodeOutlineNodeNotify() \n\
   <BtnUp>:     Unset() ";



static XtActionsRec actionList[] =
{
  { "NodeOutlineNodeNotify", (XtActionProc) Node_Outline_Node_Notify },
  { "NodeOutlineEditNotify", (XtActionProc) Node_Outline_Edit_Notify },
  { "NodeOutlineDynamicNotify", (XtActionProc) Node_Outline_Dynamic_Notify },
  { "NodeOutlineCutSelectionString", 
                 (XtActionProc) Node_Outline_Cut_Selection_String },
 };



/****************************************************************
 *
 * Full class record constant
 *
 ****************************************************************/


#define SuperClass ((OutlineWidgetClass)&outlineClassRec)
NodeOutlineClassRec nodeOutlineClassRec = {
  {
/* core_class fields      */
    /* superclass         */    (WidgetClass) SuperClass,
    /* class_name         */    "NodeOutline",
    /* widget_size        */    sizeof(NodeOutlineRec),
    /* class_initialize   */    NULL,
    /* class_part_init    */	NULL,
    /* class_inited       */	FALSE,
    /* initialize         */    NULL,
    /* initialize_hook    */	NULL,
    /* realize            */    XtInheritRealize,
    /* actions            */    actionList,
    /* num_actions	  */	XtNumber(actionList),
    /* resources          */    resources,
    /* num_resources      */    XtNumber(resources),
    /* xrm_class          */    NULLQUARK,
    /* compress_motion	  */	TRUE,
    /* compress_exposure  */	TRUE,
    /* compress_enterleave*/	TRUE,
    /* visible_interest   */    FALSE,
    /* destroy            */    NULL,
    /* resize             */    XtInheritResize,
    /* expose             */    (XtExposeProc)_XtInherit,
    /* set_values         */    NULL,
    /* set_values_hook    */	NULL,
    /* set_values_almost  */    XtInheritSetValuesAlmost,
    /* get_values_hook    */	NULL,
    /* accept_focus       */    NULL,
    /* version            */	XtVersion,
    /* callback_private   */    NULL,
    /* tm_table           */    defaultTranslations,
    /* query_geometry     */	NULL,
    /* display_accelerator*/	XtInheritDisplayAccelerator,
    /* extension          */	NULL
  },
  {
     /* Simple class fields initialization */
    /* change_sensitive		*/	XtInheritChangeSensitive
  },
 {
    /* Outline class fields */
    /* empty            */    0,
  },
 {
    /* NodeOutline class fields */
    /* empty            */    0,
  },
};

WidgetClass nodeOutlineWidgetClass = (WidgetClass)&nodeOutlineClassRec;



/****************************************************************
 *
 * Private Routines
 *
 ****************************************************************/

/*******************************

  Routines specific to NodeOutline

*******************************/

#ifdef NEVER  

/*  This is for reference */
typedef struct _XawListReturnStruct {
	String string;	/* string shown in the list. */
	int list_index;	/* index of the item selected. */
} XawListReturnStruct;

typedef struct {
  AGGrapherButtonCallbackStruct *grapherButton_callback_struct;
  char *node_name;
}  AGGrapherCallbackStruct;

typedef struct {
  XEvent *event;
  int notify_type;
  String selection_string;
  String *params;
  int num_params;
} AGGrapherButtonCallbackStruct;

#endif

static String Get_Node_Name(gw,list_index)
     NodeOutlineWidget gw;
     int list_index;
{
  /* get the node name by looking through the client_data list (parallel
     to the list of strings supplied to the List widget's code.  The
     list_client_data is a pointer to the Outline Node 
     (defined in OutlineP.h and the same as GrapherP.h). Got it? -:) */
  return(((Node *)(gw->outline.list_client_data[list_index]))->node_name);
}

#define BogusListIndex  \
      (who->list_index == XAW_LIST_NONE || who->list_index < 0 || \
	  who->list_index > gw->outline.nNodes)


static Boolean
Node_Outline_Common_Action(w,event,params,num_params,callback_name,notify_type)
     Widget w;
     XEvent * event;
     String * params;
     Cardinal *num_params;
     String callback_name;
     int notify_type;
{
  NodeOutlineWidget gw = (NodeOutlineWidget)w;
  AGGrapherButtonCallbackStruct grapher_button_callback_struct;
  AGGrapherCallbackStruct grapher_callback_struct;
  XawListReturnStruct *who;

  who = XawListShowCurrent(w);
  if (BogusListIndex)
    {
      /* who knows how we got here... */
      return(False);
    }
  
  grapher_button_callback_struct.event = event;
  grapher_button_callback_struct.selection_string = who->string;
  grapher_button_callback_struct.params = params;
  grapher_button_callback_struct.num_params = *num_params; /* FIX MSA 5/14/93*/
  grapher_callback_struct.grapherButton_callback_struct =
    &grapher_button_callback_struct;
  grapher_callback_struct.node_name = Get_Node_Name(gw,who->list_index);

  grapher_button_callback_struct.notify_type = notify_type;
  XtCallCallbacks(w,callback_name,(XtPointer)&grapher_callback_struct);

  return(True);
}

static void
Node_Outline_Node_Notify(w, event, params, num_params)
     Widget w;
     XEvent * event;
     String * params;
     Cardinal *num_params;
{
  Node_Outline_Common_Action(w,event,params,num_params,
			     XtNcallback,AGGrapherButtonNodeNotify);
}

static void
Node_Outline_Edit_Notify(w, event, params, num_params)
     Widget w;
     XEvent * event;
     String * params;
     Cardinal *num_params;
{
  NodeOutlineWidget gw = (NodeOutlineWidget)w;
  if (gw->node_outline.edit_mode)
    Node_Outline_Common_Action(w,event,params,num_params,
			       XtNeditCallback,AGGrapherButtonEditNotify);
}

static void
Node_Outline_Dynamic_Notify(w, event, params, num_params)
     Widget w;
     XEvent * event;
     String * params;
     Cardinal *num_params;
{
  Node_Outline_Common_Action(w,event,params,num_params,
			     XtNdynamicCallback,AGGrapherButtonDynamicNotify);
}



Boolean Node_Outline_Convert_Selection (w, selection, target, type, value, 
					length,format)
     Widget w;
     Atom *selection;
     Atom *target;
     Atom *type;
     XtPointer *value;
     unsigned long *length;
     int *format;
{
  NodeOutlineWidget gw = (NodeOutlineWidget)w;
  Atom *target_ptr;
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
		  *value = 
		    (XtPointer)XtNewString(gw->node_outline.selection_string);
		  *length = AGstrlen(*value) + 1;
		  *format = 8;
		  return (True);
	      }
	  return (False);
      }
}

static void 
Node_Outline_Cut_Selection_String(w,xevent,params,num_params)
     Widget w;
     XEvent *xevent;
     String *params;		/* unused */
     Cardinal *num_params;
{
  NodeOutlineWidget gw = (NodeOutlineWidget)w;
  AGGrapherButtonCallbackStruct grapher_button_callback_struct;
  AGGrapherCallbackStruct grapher_callback_struct;
  XawListReturnStruct *who;

  /* If we don't get it, it doesn't matter here */
  XtOwnSelection(w,XA_PRIMARY, xevent->xbutton.time,
		 Node_Outline_Convert_Selection,
		 NULL,
		 NULL);

  who = XawListShowCurrent(w);
  if (BogusListIndex)
    {
      /* who knows how we got here... */
      gw->node_outline.selection_string = NULL;
      return;
    }
  gw->node_outline.selection_string = Get_Node_Name(gw,who->list_index);
}


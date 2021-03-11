
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

  OUTLINE

  A simple outline for trees.

  Mark Ackerman
  MIT/Center for Coordination Technology
  MIT/Project Athena
  July, 1992
                    
*********************************************************/


#include        <stdio.h>
#include        <ctype.h>
#include	<X11/IntrinsicP.h>
#include	<X11/StringDefs.h>
#include	"OutlineP.h"
#include        "GButton.h"
#include        <X11/Xaw/Label.h>
#include        "GButtonP.h" /* for callback, to get parent */
#include        "AGmacros.h"


/****************************************************************
 *
 * Outline Resources
 *
 ****************************************************************/


#define offset(field) XtOffset(OutlineWidget, field)
static XtResource resources[] = { 
    {XtNxIncrement,XtCIncrement,XtRDimension, sizeof(Dimension),
	 offset(outline.x_increment),XtRImmediate, (XtPointer)1},
    {XtNtext,XtCText,XtRString, sizeof(String),
	 offset(outline.text),XtRString,NULL},
    /* List widget has the XtNcallback list */
    {XtNcursor,XtCCursor,XtRCursor,sizeof(Cursor),
	 offset(outline.cursor),XtRString,"arrow"},
    {XtNrootNode,XtCRootNode,XtRPointer,sizeof(Node *),
	 offset(outline.root),XtRImmediate,NULL}, 
    {XtNnumberNodes,XtCNumberNodes,XtRInt,sizeof(int),
	offset(outline.nNodes),XtRImmediate,0},
    {XtNminHeight,XtCHeight,XtRDimension,sizeof(Dimension),
	 offset(outline.min_height),XtRImmediate,0},  
    {XtNminWidth,XtCWidth,XtRDimension,sizeof(Dimension),
	 offset(outline.min_width),XtRImmediate,0},  
    {XtNheight,XtCHeight,XtRDimension,sizeof(Dimension),
	 offset(core.height),XtRImmediate,(XtPointer)10},  
    {XtNwidth,XtCWidth,XtRDimension,sizeof(Dimension),
	 offset(core.width),XtRImmediate,(XtPointer)10},  
};
#undef offset



/****************************************************************
 *
 * Full class record constant
 *
 ****************************************************************/

static void Initialize();
static void Destroy();
static int Make_Nodes();
static int Create_Outline_List();

#define SuperClass ((ListWidgetClass)&listClassRec)
OutlineClassRec outlineClassRec = {
  {
/* core_class fields      */
    /* superclass         */    (WidgetClass) SuperClass,
    /* class_name         */    "Outline",
    /* widget_size        */    sizeof(OutlineRec),
    /* class_initialize   */    NULL,
    /* class_part_init    */	NULL,
    /* class_inited       */	FALSE,
    /* initialize         */    Initialize,
    /* initialize_hook    */	NULL,
    /* realize            */    XtInheritRealize,
    /* actions            */    NULL,
    /* num_actions	  */	0,
    /* resources          */    resources,
    /* num_resources      */    XtNumber(resources),
    /* xrm_class          */    NULLQUARK,
    /* compress_motion	  */	TRUE,
    /* compress_exposure  */	TRUE,
    /* compress_enterleave*/	TRUE,
    /* visible_interest   */    FALSE,
    /* destroy            */    Destroy,
    /* resize             */    XtInheritResize,
    /* expose             */    (XtExposeProc)_XtInherit,
    /* set_values         */    NULL,
    /* set_values_hook    */	NULL,
    /* set_values_almost  */    XtInheritSetValuesAlmost,
    /* get_values_hook    */	NULL,
    /* accept_focus       */    NULL,
    /* version            */	XtVersion,
    /* callback_private   */    NULL,
    /* tm_table           */    NULL,
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
};

WidgetClass outlineWidgetClass = (WidgetClass)&outlineClassRec;


/****************************************************************
 *
 * Private Routines
 *
 ****************************************************************/

/*******************************************

  Routines common to Grapher and Outline

*******************************************/

#define MAXBUF 256
#define EOS '\0'

static void Make_String(buffer,ptr,start_textloc,size)
     char *buffer;
     char *ptr;
     char *start_textloc;
     int size;
{
  int action_size;

  if ((action_size = ptr-start_textloc) >= size)
    action_size = size-1;
  strncpy(buffer,start_textloc,action_size);
  buffer[(int)(action_size)] = EOS;
}

static char *Get_Next_Line(buffer,size,start_textloc)
     char *buffer;
     unsigned int size;
     char *start_textloc;
{
  char *ptr; 
  char *temp;
  int action_size;
  if ((ptr = AGindex(start_textloc,NEWLINE)) == NULL)
    {
      /* 11/4/91 MSA watch for no \n on last line; stop now only on EOS */
      if ((ptr = AGindex(start_textloc,EOS)) == NULL || ptr == start_textloc)
	return(NULL);
      else
	{
	  Make_String(buffer,ptr,start_textloc,size);
	  /* return ptr so next round will catch EOS and terminate */
	  return(ptr);
	}
    }
  Make_String(buffer,ptr,start_textloc,size);
  /* return 1 past \n for next line */
  return(++ptr);
}


static Boolean Get_Outline_File(gw)
     OutlineWidget gw;
{
  char buffer[MAXBUF];
  int pos;
  
  char *remember_text_startloc;
  if (gw->outline.text == NULL)
    {   
      /* No file?  Get upset. */
      sprintf(buffer,"no outline file.  Continuing....");
      XtWarning(buffer);
      return(True);
    }
  
  if ((remember_text_startloc =
       Get_Next_Line(buffer,MAXBUF,gw->outline.text))
      == NULL)
    {  
      /* A broken file?  Get upset. */
      XtWarning("empty outline file after header.  Continuing...");
      return(True);
    }
  
  pos = 0;
  gw->outline.nNodes = 0;
  Make_Nodes(&remember_text_startloc,pos,
	     &(gw->outline.root),NULL,buffer,&(gw->outline.nNodes));
  

  
  return(False);
}

static Node *Get_Node(node_count)
     int *node_count;
{
    Node *curptr;
    
    if ((curptr = (Node *)XtCalloc(1,sizeof(Node))) == NULL)
	{
	    XtError("No memory\n");
	    return(NULL);
	}
    (*node_count)++;
    return(curptr);
}


static int Make_Nodes(remember_text_loc,pos,prevptr,parent,buffer,node_count)
     char **remember_text_loc;
     int pos;
     Node **prevptr;
     Node *parent;
     char buffer[];
     int *node_count;
{
    Node *curptr;
    Node **remember;
    int i;
    int j;
    int level;
    char button_name[MAXBUF];
    char node_name[MAXBUF];
    char *ptr;
    char *startptr;
    char templabel[MAXBUF];


    remember = prevptr;  
    level = pos;
    
    while(1) {
	curptr = Get_Node(node_count);
	*remember = curptr; /* store curptr ***WHAT? *** */
	remember = (Node **)&curptr->sibling; 
	curptr->parent = parent;

	startptr = &buffer[level];
	while (True)
	    {
		ptr = AGindex(startptr,'/');
		if (ptr == NULL)
		    break;
		else
		    if (*(++ptr) == '/')
			;
		    else
			break;
	    }

	if (ptr != NULL)
	    {
		strncpy(templabel,startptr,(int)(ptr-startptr-1));
		templabel[ptr-startptr-1] = '\0';
		curptr->label = XtNewString(templabel);
		curptr->node_name = (XtPointer) XtNewString(ptr);
	    }
	else
	    {
		/* avoid bug with XtNewString */
		curptr->label = XtNewString(startptr);
		curptr->node_name = (XtPointer) XtNewString("NoAction");
	    }

	if ((*remember_text_loc = 
	     Get_Next_Line(buffer,MAXBUF,*remember_text_loc)) == NULL)
	  return(-2);

	for (i=0;i<AGstrlen(buffer);i++)
	    if (buffer[i] != ' ')
		break;

	if (i == level) /* sibling */
	    ;/* we'll get it on the next loop */
	else
	    if (i > level) /* child */
		{
		    if ((i = Make_Nodes(remember_text_loc,level+1,
					(struct _Node **)&(curptr->child),
					curptr,buffer,node_count)) < 0)
			return(i);
		    else if (level != i)
			return(i);
		}
	    else /* parent */
		return(i);
    }
}

/*******************************

  Routines specific to Outline

*******************************/

static void Outline_Setup(gw,error_flag)
     OutlineWidget gw;
     Boolean *error_flag;
{
   /* Is there a file from which to read the nodes or a 
      tree structure given by the application?   If not,
      get upset. */
   if (gw->outline.text == NULL && gw->outline.root == NULL)
      {
	 XtWarning("No outline file or list.  Continuing....");
	 *error_flag = True;
      }
   /* Is there a file?  If so, it overrides any passed
      network.  Why?  It's arbitrary. */
   else if (gw->outline.text) 
      *error_flag = Get_Outline_File(gw); /* returns true if error */
}

static void List_Setup(gw)
     OutlineWidget gw;
{
   int text_loc;
   Dimension span;  /* not used - remnant of grapher algorithm */
   /* Create the list buttons */
   span = 0;
   text_loc = 0;
   Create_Outline_List(gw,(Node *)(gw->outline.root),&span,
	     1, /* starting text strings for list at level 1 of outline*/
	     False,
	     gw->outline.list_strings,&text_loc,gw->outline.list_client_data);
   
   /* Null terminate the list of strings */
   gw->outline.list_strings[text_loc] = EOS;
   if (gw->outline.nNodes != text_loc)
      {
	 XtWarning("outline widget:  bad item count.  Attempting to continue");
      }
}

static void 
Initialize(junk, new, args, num_args)
     Widget junk;
     Widget new;
     ArgList args;
     Cardinal *num_args;
{
    OutlineWidget gw = (OutlineWidget)new;
    char buffer[MAXBUF];
    int pos;
    XGCValues values;
    Dimension temp_height;
    Dimension temp_width;
    Boolean error_flag;
    Widget label;

    gw->outline.max_x = 0;
    error_flag = False;

    Outline_Setup(gw,&error_flag);


    if ((gw->outline.list_strings = 
	 (String *)XtMalloc((sizeof(String) * (gw->outline.nNodes + 1)))) 
	== NULL)
      error_flag = True;

    /* This is one less than list_strings because we don't need the
       null terminator.  This is private to the outline widget.  */
    if ((gw->outline.list_client_data = 
	 (XtPointer *)XtMalloc((sizeof(XtPointer) * (gw->outline.nNodes)))) 
	== NULL)
      error_flag = True;


    if (error_flag)
	{
	    sprintf(buffer,"%s is in an error state",gw->core.name);
	    label = XtVaCreateManagedWidget("errorLabel",labelWidgetClass,
					    (Widget)gw,
					    XtNlabel, (XtArgVal)buffer,
					    XtNsensitive, (XtArgVal)False,
					    XtNborderWidth, (XtArgVal)0,
					    NULL);
	    XtVaGetValues(label, XtNwidth, &temp_width, 
			  XtNheight, &temp_height,
			  NULL);
	    gw->core.width = temp_width;
	    gw->core.height = temp_height;
	    return;
	}
    List_Setup(gw);

    /* Can I get away with this?  This is slightly faster than
       triggering off SetValues */
    /* gw->list.list = gw->outline.list_strings; 
    (SuperClass->core_class.initialize)(junk, new, args, num_args); */

    XawListChange(new, gw->outline.list_strings, 0,0,True);

} /* Initialize */



static int Create_Outline_List(gw,tp,span,level,siblingFlag,
			       list_strings,text_loc,list_client_data)
     OutlineWidget gw;
     Node *tp;
     Dimension *span; /* not used - remnant of Grapher algorithm */
     int level;
     int siblingFlag;
     char **list_strings;
     int *text_loc;
     XtPointer *list_client_data;
{
    Widget w;
    Dimension newspan = 0;
    char buffer[MAXBUF];
    Dimension height;
    int i;


    if (tp->child == NULL)
	{
	    /* leaf node */
	    tp->x = (level-1) * gw->outline.x_increment;

	    for (i=0;i<tp->x;i++)
	       buffer[i]= ' ';
	    buffer[i] = EOS;
	    AGstrcat(buffer,tp->label);
	    list_client_data[*text_loc] = (XtPointer)tp;
	    list_strings[(*text_loc)++] = XtNewString(buffer);
	    
	}
    else
	{
	    tp->x = (level-1) * gw->outline.x_increment;
	    for (i=0;i<tp->x;i++)
	       buffer[i]= ' ';
	    buffer[i] = EOS;
	    AGstrcat(buffer,tp->label);
	    list_client_data[*text_loc] = (XtPointer)tp;
	    list_strings[(*text_loc)++] = XtNewString(buffer);
	    Create_Outline_List(gw,(Node *)(tp->child),&newspan,
		    level+1,False,
		    list_strings,text_loc,list_client_data);
	}

    if (tp->sibling)
       Create_Outline_List(gw,(Node *)(tp->sibling),span,level,True,
			   list_strings,text_loc,list_client_data);

}

	    

static void Destroy_Outline(node)
     Node *node;
{
    int i;
    Node *temp;

    temp= node->child;
    if (temp)
	Destroy_Outline(temp);

    temp= node->sibling;
    if (temp)
	{
	    Destroy_Outline(temp);
	}
    XtFree((char *)node->label);
    XtFree((char *)node->node_name);
    XtFree((char *)node);
}


static void Destroy_Sub(list_strings,list_client_data,root)
     String *list_strings;
     String *list_client_data;
     Node *root;
{
   /* This routine exists so it can be called on the old stuff from
      SetValues and equivalent routines */
   /* Should this be a subclass method? */

    if (list_strings != NULL)
      XtFree((char *)list_strings);
    if (list_client_data != NULL)
      XtFree((char *)list_client_data);

    if (root) /* if not, then outline had an error condition
			     on setup */
       Destroy_Outline(root); 
}

static void Destroy(w)
     Widget w;
{
   OutlineWidget gw = (OutlineWidget)w;
   Destroy_Sub(gw->outline.list_strings,gw->outline.list_client_data,
	       gw->outline.root);
}

XawOutlineUpdate(w,text,root,nNodes)
     Widget w;
     String text;
     XtPointer root;
{
   OutlineWidget gw = (OutlineWidget) w;
   String *old_list_strings;
   XtPointer *old_client_data;
   Node *old_root;
   Boolean error_flag;

   /* check to make sure you have some valid data */
   if (w == NULL || (root == NULL && text == NULL))
      return;
   /* this could have been done in the above check, but I'm
      worried about problematic compilers.*/
   if (root == NULL && text!= NULL && text[0] == EOS)
      return;

   /* store the old values so they can be destroyed if everything
      is successful */
   old_list_strings = gw->outline.list_strings;
   old_client_data = gw->outline.list_client_data;

   old_root = gw->outline.root;

   /* if there is a text file, it wins.  THe rule is arbitrary */
   if (text != NULL && text[0] != EOS)
      {
	 error_flag = False;
	 /* okay to do the next line because Outline doesn't need
	    the old text buffer after it creates itself - in fact
	    the calling program may free it */
	 gw->outline.text = text; 
	 /* parse the text file and setup the new nodes */
	 Outline_Setup(gw,&error_flag);
      }


   /* get some space */
   if ((gw->outline.list_strings = 
	(String *)XtMalloc((sizeof(String) * (gw->outline.nNodes + 1)))) 
       == NULL)
      error_flag = True;
   
   /* This is one less than list_strings because we don't need the
      null terminator.  This is private to the outline widget.  */
   if ((gw->outline.list_client_data = 
	(XtPointer *)XtMalloc((sizeof(XtPointer) * (gw->outline.nNodes)))) 
       == NULL)
      error_flag = True;

   if (error_flag)
      {
	 XtWarning("unable to update Outline.  Continuing with old...");
      }
   else
      {
	 /* Get the list of strings */
	 List_Setup(gw);
	 /* Update the list via the list widget code */
	 XawListChange((Widget)gw, gw->outline.list_strings, 0,0,True);
	 /* safer to do this after the list has been updated - no
	    screen garbage, I think */
	 Destroy_Sub(old_list_strings,old_client_data,old_root);
      }
}





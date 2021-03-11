
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

  GRAPHER

  A simple grapher for trees.

  Mark Ackerman
  MIT/Center for Coordination Technology
  MIT/Project Athena
  February, 1989

  9/12/89  MSA   Added ability to have rootNode resource.
  10/20/89 MSA   Made sure Dimensions and Positions were such and
		 not ints.
		 Grapher now figures out preferred size.  Gave widget
		 user ability to have max and min height to override
		 preferred size.  Also watches height and width resources.
		 Added max and min height and width resources and hooked into
                 Initialize.  
	   MSA   Added inEditMode
   7/9/90  MSA   Much stuff.  Changed over to R4, installed grapher buttons
                 for editing, installed necessary changes for grapher file
		 headers, installed destroy proc, changed grapher file
		 layout (now returns node name), initialize now handles
		 error condition by creating label.

   Todo:  Shouldn't need to know Layout's default size (10/20/89).
          Need a more robust handling of blank lines in files (7/9/90).
	  More testing of destroy proc (7/9/90).
                    
*********************************************************/


#include        <stdio.h>
#include        <ctype.h>
#include	<X11/IntrinsicP.h>
#include	<X11/StringDefs.h>
#include	"GrapherP.h"
#include        "GButton.h"
#include        <X11/Xaw/Label.h>
#include        "GButtonP.h" /* for callback, to get parent */
#include        "AGmacros.h"
#ifndef NO_FUNCOBJ
#include        "FuncObj.h"
#endif

/****************************************************************
 *
 * Grapher Resources
 *
 ****************************************************************/


#define offset(field) XtOffset(GrapherWidget, field)
static XtResource resources[] = { 
    {XtNnextY, XtCNextY, XtRPosition, sizeof(Position),
	 offset(grapher.next_y), XtRImmediate, (XtPointer)0},
    {XtNxIncrement,XtCIncrement,XtRDimension, sizeof(Dimension),
	 offset(grapher.x_increment),XtRImmediate, (XtPointer)50},
    {XtNyIncrement,XtCIncrement,XtRDimension, sizeof(Dimension),
	 offset(grapher.y_increment),XtRImmediate, (XtPointer)8},
    {XtNtext,XtCText,XtRString, sizeof(String),
	 offset(grapher.text),XtRString,NULL},
    {XtNcallback,XtCCallback,XtRCallback,sizeof(XtPointer),
	 offset(grapher.callback),XtRCallback,NULL},
    {XtNeditCallback,XtCCallback,XtRCallback,sizeof(XtPointer),
	 offset(grapher.edit_callback),XtRCallback,NULL},
    {XtNdynamicCallback,XtCCallback,XtRCallback,sizeof(XtPointer),
	 offset(grapher.dynamic_callback),XtRCallback,NULL},
    {XtNcursor,XtCCursor,XtRCursor,sizeof(Cursor),
	 offset(grapher.cursor),XtRString,"arrow"},
    {XtNrootNode,XtCRootNode,XtRPointer,sizeof(Node *),
	 offset(grapher.root),XtRImmediate,NULL}, 
    {XtNminHeight,XtCHeight,XtRDimension,sizeof(Dimension),
	 offset(grapher.min_height),XtRImmediate,0},  
    {XtNminWidth,XtCWidth,XtRDimension,sizeof(Dimension),
	 offset(grapher.min_width),XtRImmediate,0},  
    {XtNeditMode,XtCEditMode,XtRBoolean,sizeof(Boolean),
	 offset(grapher.edit_mode),XtRImmediate,False},
    {XtNforeground,XtCForeground,XtRPixel,sizeof(Pixel),
	 offset(grapher.foreground),XtRString,XtDefaultForeground},
    {XtNheight,XtCHeight,XtRDimension,sizeof(Dimension),
	 offset(core.height),XtRImmediate,(XtPointer)10},  
    {XtNwidth,XtCWidth,XtRDimension,sizeof(Dimension),
	 offset(core.width),XtRImmediate,(XtPointer)10},  
#ifndef NO_FUNCOBJ
    /* funcObj handles general AG functions such as @close and @system.
       If it's not present, this instance can't handle them. */
    {XtNfuncObj, XtCWidget, XtRWidget, sizeof(Widget),
	 offset(grapher.func_obj), XtRPointer, (XtPointer)NULL},
#endif

};
#undef offset



/****************************************************************
 *
 * Full class record constant
 *
 ****************************************************************/

static void Initialize();
static void Redisplay();
static void Destroy();
static void draw_lines();
static int Make_Nodes();
static int Do_This();

#define SuperClass ((LayoutWidgetClass)&layoutClassRec)
GrapherClassRec grapherClassRec = {
  {
/* core_class fields      */
    /* superclass         */    (WidgetClass) SuperClass,
    /* class_name         */    "Grapher",
    /* widget_size        */    sizeof(GrapherRec),
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
    /* expose             */    Redisplay,
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
  },{
/* composite_class fields */
    /* geometry_manager   */    (XtGeometryHandler)XtInheritGeometryManager,
    /* change_managed     */    XtInheritChangeManaged,
    /* insert_child	  */	XtInheritInsertChild,
    /* delete_child	  */	XtInheritDeleteChild,
    /* extension          */	NULL
    },
	{
/* Layout class fields */
    /* empty		  */	0,
			    },
  {
/* Grapher class fields */
      /* empty            */    0,
			    },
};

WidgetClass grapherWidgetClass = (WidgetClass)&grapherClassRec;


/****************************************************************
 *
 * Private Routines
 *
 ****************************************************************/

#define MAXBUF 256
#define EOS '\0'

   /* I'm allowing these to be global since they're not used to
      keep state.  */
static Dimension height; 
static Dimension width;
static Arg getNodeArgs[] = 
{
    {XtNheight, (XtArgVal)&height},
    {XtNwidth, (XtArgVal)&width},
};


#ifdef GRAPHERv1
static char* Strip_WhiteSpace(str)
     char *str;
{
    static char temp[MAXBUF];
    register char *ptr = str;
    register char *ptr2 = temp;

     /* Get rid of any space characters */    
    while (*ptr != EOS )
	if (!isspace(*ptr))
	    *(ptr2++) = *(ptr++);
	else
	    ptr++;
    *ptr2 = EOS;
    return(temp);
}
#else

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
#endif

static Boolean Get_Grapher_File(gw)
     GrapherWidget gw;
{
    char buffer[MAXBUF];
    int pos;

#ifdef GRAPHERv1
    FILE *fp;
    if ((fp = fopen(gw->grapher.filename,"r")) == NULL)
	{   
	    /* No file?  Get upset. */
	    sprintf(buffer,"cannot open grapher file%s.  Continuing....",
		    gw->grapher.filename);
	    XtWarning(buffer);
	    return(True);
	}
    if (fgets(buffer,MAXBUF,fp) == NULL)
	{  
	    /* A broken file?  Get upset. */
	    XtWarning("premature end to grapher file.  Continuing...");
	    return(True);
	}

	    /* look for the header */
    if (strcmplo("@begin(header)",Strip_WhiteSpace(buffer)))
	{
		    /* No header?  Get upset. */
		  XtWarning("missing header on grapher file.  Continuing...");
		  return(True);
		}
    /* Prowl through the file looking for the end of the
       header */
    
    while (strcmplo("@end(header)",Strip_WhiteSpace(buffer)))
	if (fgets(buffer,MAXBUF,fp) == NULL)
	    {
		XtWarning("mangled header on grapher file.");
		return(True);
	    }

    /* Get the next record to start */
    if (fgets(buffer,MAXBUF,fp) == NULL)
	{  
	    /* A broken file?  Get upset. */
	    XtWarning("empty grapher file after header.  Continuing...");
	    return(True);
	}
    /* If everything is okee-dokee, cut off the final
       \n on the buffer, and begin the recursion. */
    
    buffer[AGstrlen(buffer)-1] = '\0';
#else
    char *remember_text_startloc;
    if (gw->grapher.text == NULL)
	{   
	    /* No file?  Get upset. */
	    sprintf(buffer,"no grapher file.  Continuing....");
	    XtWarning(buffer);
	    return(True);
	 }

    if ((remember_text_startloc =
	 Get_Next_Line(buffer,MAXBUF,gw->grapher.text) )
	== NULL)
	{  
	    /* A broken file?  Get upset. */
	    XtWarning("empty grapher file after header.  Continuing...");
	    return(True);
	}
#endif
    pos = 0;
#ifdef GRAPHERv1
    Make_Nodes(fp,pos,&(gw->grapher.root),NULL,buffer);
    fclose(fp);
#else
    Make_Nodes(&remember_text_startloc,pos,
	       &(gw->grapher.root),NULL,buffer);

#endif    
    return(False);
}

static void Initialize(request, new)
    Widget request, new;
{
    GrapherWidget gw = (GrapherWidget)new;
    Dimension span;
    char buffer[MAXBUF];
    int pos;
    XGCValues values;
    Dimension temp_height;
    Dimension temp_width;
    Boolean error_flag;
    Widget label;

    gw->grapher.nNodes = 0;
    gw->grapher.max_x = 0;
    error_flag = False;
    gw->grapher.gc = NULL;

       /* Is there a file from which to read the nodes or a 
	  tree structure given by the application?   If not,
	  get upset. */
#ifdef GRAPHERv1
    if (gw->grapher.filename == NULL && gw->grapher.root == NULL)
	{
	    XtWarning("No grapher file or list.  Continuing....");
	    error_flag = True;
	}
       /* Is there a file?  If so, it overrides any passed
	  network.  Why?  It's arbitrary. */
    else if (gw->grapher.filename) 
	error_flag = Get_Grapher_File(gw); /* returns true if error */
#else
    if (gw->grapher.text == NULL && gw->grapher.root == NULL)
	{
	    XtWarning("No grapher file or list.  Continuing....");
	    error_flag = True;
	}
       /* Is there a file?  If so, it overrides any passed
	  network.  Why?  It's arbitrary. */
    else if (gw->grapher.text) 
	error_flag = Get_Grapher_File(gw); /* returns true if error */
#endif    
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

    /* If everything is still okee-dokee, get the gcs */
    
    values.function = GXcopy;
    values.line_width = 0;
    
    values.foreground = gw->grapher.foreground;
    gw->grapher.gc = XtGetGC((Widget)gw,GCFunction|GCForeground|GCLineWidth,
			     &values);
    
    /* Create the grapher buttons */
    span = 0;
    Do_This(gw,(Node *)(gw->grapher.root),&span,(Position)0,False);
    
    
    /* figure out size of widget */
    temp_height = gw->grapher.next_y + gw->grapher.y_increment;
    temp_width = gw->grapher.max_x + gw->grapher.x_increment;

#ifdef OLD    
    if (gw->core.height > 10) /* Layout sets it to 10 */
	; /* somebody set it */
    else 
	{
	    if (gw->grapher.max_height < gw->grapher.min_height)
		;  /* messed up; forget it */
	    else
		{
		    if (gw->grapher.max_height > 0 && 
			temp_height > gw->grapher.max_height)
			temp_height = gw->grapher.max_height;
		    if (gw->grapher.min_height > 0 && 
			temp_height < gw->grapher.min_height)
			temp_height = gw->grapher.min_height;
		}
	    gw->core.height = temp_height;
	}
    
    if (gw->core.width > 10) /* Layout sets it to 10 */
	; /* somebody set it */
    else 
	{
	    if (gw->grapher.max_width < gw->grapher.min_width)
		; /* messed up; forget it */
	    else
		{
		    if (gw->grapher.max_width > 0 && 
			temp_width > gw->grapher.max_width)
			temp_width = gw->grapher.max_width;
		    if (gw->grapher.min_width > 0 && 
			temp_width < gw->grapher.min_width)
			temp_width = gw->grapher.min_width;
		}
	    gw->core.width = temp_width;
	}
#endif

    if (gw->core.height != 10) /* set to 10 as the default */
	; /* somebody set it */
    else 
	{
	  if (gw->grapher.min_height > 0 && 
	      temp_height < gw->grapher.min_height)
	    temp_height = gw->grapher.min_height;
	  gw->core.height = temp_height;
	}
    
    if (gw->core.width != 10) /* set to 10 as the default */
	; /* somebody set it */
    else 
	{
	  if (gw->grapher.min_width > 0 && 
	      temp_width < gw->grapher.min_width)
	    temp_width = gw->grapher.min_width;
	  gw->core.width = temp_width;
	}

} /* Initialize */

static Node *Get_Node()
{
    Node *curptr;

    if ((curptr = (Node *)XtCalloc(1,sizeof(Node))) == NULL)
	{
	    XtError("No memory\n");
	    return(NULL);
	}
    return(curptr);
}

  /* For historical continuity. */
static void bye_bye()
{
    exit(0);
}


static void InternalCallback(w,client_data,call_data)
     Widget w;
     XtPointer client_data;
     XtPointer call_data;
{
    AGGrapherButtonCallbackStruct *callback_struct;
    AGGrapherCallbackStruct edit_struct;
    GrapherButtonWidget gbw = (GrapherButtonWidget)w;
    GrapherWidget gw = (GrapherWidget)gbw->core.parent;
    char *buffer;
    int i;
    Boolean error;
    callback_struct = (AGGrapherButtonCallbackStruct *)call_data;
#ifndef NO_FUNCOBJ
#define ATSIGN '@'
    buffer = callback_struct->selection_string;    

    if (buffer[0] == ATSIGN)
      {
	/* Must be a command.  Check whether we are in a normal button
	   hit.  If so, get the command and check whether it is a valid
	   command for being here.  If so, find the rest of the token
	   (ie, any arguments to the command), and send everything
	   to a routine to execute the command.  If there are any
	   error conditions, fail silently (except maybe for an error
	   message or two) but don't set the knedit.error_flag.  That
	   is, on failure, don't change the appearance of the Knedit. */
	if (callback_struct->notify_type == AGGrapherButtonNodeNotify)
	  {
	    /* funcObj handles general AG functions such as @close 
	       and @system.  If it's not present, this instance 
	       can't handle them. */
	    if (gw->grapher.func_obj)
	      {
		i = 0;
		AGFuncObjHandle(gw->grapher.func_obj,
				buffer,
				&i,
				&error,
				False,
				False, NULL);
		/* ignore error conditions in func_obj */
	      }
	  }
	/* else ignore */
      }
    else
#undef ATSIGN
#endif
    if (callback_struct->notify_type == AGGrapherButtonNodeNotify)
	XtCallCallbacks((Widget)gw,XtNcallback,
			call_data);
    else if (callback_struct->notify_type == AGGrapherButtonEditNotify)
      {
	if (gw->grapher.edit_mode)
	  XtCallCallbacks((Widget)gw, XtNeditCallback,
			call_data);
      }
    else 
	XtCallCallbacks((Widget)gw, XtNdynamicCallback,call_data);

}

#ifdef GRAPHERv1
static int Make_Nodes(fp,pos,prevptr,parent,buffer)
     FILE *fp;
#else
static int Make_Nodes(remember_text_loc,pos,prevptr,parent,buffer)
     char **remember_text_loc;
#endif
     int pos;
     Node **prevptr;
     Node *parent;
     char buffer[];
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
	curptr = Get_Node();
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

#ifdef GRAPHERv1
	if (fgets(buffer,MAXBUF,fp) == NULL)
	    return(-2);
	buffer[AGstrlen(buffer)-1] = '\0';
#else
	if ((*remember_text_loc = 
	     Get_Next_Line(buffer,MAXBUF,*remember_text_loc)) == NULL)
	  return(-2);
#endif
	for (i=0;i<AGstrlen(buffer);i++)
	    if (buffer[i] != ' ')
		break;

	if (i == level) /* sibling */
	    ;/* we'll get it on the next loop */
	else
	    if (i > level) /* child */
		{
#ifdef GRAPHERv1
		    if ((i = Make_Nodes(fp,level+1,
					(struct _Node **)&(curptr->child),
					curptr,buffer)) < 0)
#else
		    if ((i = Make_Nodes(remember_text_loc,level+1,
					(struct _Node **)&(curptr->child),
					curptr,buffer)) < 0)
#endif
			return(i);
		    else if (level != i)
			return(i);
		}
	    else /* parent */
		return(i);
    }
}





static int Do_This(gw,tp,span,parent_x2,siblingFlag)
     GrapherWidget gw;
     Node *tp;
     Dimension *span;
     Position parent_x2;
     int siblingFlag;
{
    Widget w;
    Dimension newspan = 0;
    Position old_y;
    char buffer[MAXBUF];

    if (tp->child == NULL)
	{
	    /* leaf node */
	    tp->y = gw->grapher.next_y + gw->grapher.y_increment;
	    tp->x = parent_x2 + gw->grapher.x_increment;
/*	    if (gw->grapher.max_x < tp->x)
		gw->grapher.max_x = tp->x; */
	    sprintf(buffer,"node%d",++(gw->grapher.nNodes));
	    w = tp->w = XtVaCreateManagedWidget(buffer,
			   grapherButtonWidgetClass,(Widget)gw,
			   XtNy, (XtArgVal) tp->y,
			   XtNx, (XtArgVal) tp->x,
			   XtNselectionString, (XtArgVal) tp->node_name, 
			   XtNlabel, (tp->label) ? tp->label : NULL,
			   XtNeditMode, (XtArgVal) gw->grapher.edit_mode, 
			   NULL);

	    XtAddCallback(w,XtNcallback,InternalCallback,(XtPointer)tp);

	    XtGetValues(w,getNodeArgs,XtNumber(getNodeArgs));
	    if (siblingFlag)
		*span += (height + gw->grapher.y_increment);
	    gw->grapher.next_y += (height+ gw->grapher.y_increment);
	    tp->height = height;
	    tp->width = width;
	    if (gw->grapher.max_x < (tp->x + width))
		gw->grapher.max_x = tp->x+width;
	}
    else
	{
	    tp->x = parent_x2 + gw->grapher.x_increment;
	    if (gw->grapher.max_x < tp->x)
		gw->grapher.max_x = tp->x;
	    sprintf(buffer,"node%d",++(gw->grapher.nNodes));
	    w = tp->w = XtVaCreateManagedWidget(buffer,
			   grapherButtonWidgetClass,(Widget)gw,
			   XtNselectionString, (XtArgVal) tp->node_name, 
			   XtNlabel, (tp->label) ? tp->label : NULL,
			   XtNeditMode, (XtArgVal) gw->grapher.edit_mode, 
			   NULL);

	    XtAddCallback(w,XtNcallback,InternalCallback,(XtPointer)tp);
	    XtGetValues(w,getNodeArgs,XtNumber(getNodeArgs));
	    tp->height = height;
	    tp->width = width;
	      /* be able to recover y after recursion */
	    old_y = gw->grapher.next_y + gw->grapher.y_increment; 
	    Do_This(gw,(Node *)(tp->child),&newspan,
		    (Position)(tp->x+tp->width),False);
	    if (newspan != 0)
		tp->y = old_y + (newspan + height)/2 ;
	    else
		tp->y = old_y;
	    XtVaSetValues(w,XtNy, (XtArgVal)tp->y,
			    XtNx, (XtArgVal)tp->x,
			    NULL);
	    *span += newspan;
	}

    if (tp->sibling)
	Do_This(gw,(Node *)(tp->sibling),span,parent_x2,True);
}

static void Redisplay(w, event, region)
    Widget w;
    XEvent *event;
    Region region;
{
    GrapherWidget gw = (GrapherWidget)w;

    /* if root is null, then grapher had an error condition on setup. */
    /* if child is null, then there is only the root node in this 
       grapher, and don't draw  7/6/91 */
    if (gw->grapher.root && (gw->grapher.root)->child)
       draw_lines((Node *)((gw->grapher.root)->child),0,w,gw->grapher.gc);
}
	    

#define yPoint(node) (node->y)+(node->height)/2
#define x1Point(node) (node->x)+(node->width)

static void draw_lines(node,level,w,the_GC)
     Node *node;
     int level;
     Widget w;
     GC the_GC;
{
    int i;
    Node *temp;
    Node *parent;

    parent = node->parent;
    XDrawLine(XtDisplay(w),XtWindow(w),the_GC,x1Point(parent),
		      yPoint(parent),node->x,yPoint(node));
    temp= node->child;
    if (temp)
	draw_lines(temp,level+1,w,the_GC);

    temp= node->sibling;
    if (temp)
	{
	    XDrawLine(XtDisplay(w),XtWindow(w),the_GC,
		      (parent->x)+parent->width,yPoint(parent),
		      temp->x,yPoint(temp));
	    draw_lines(temp,level,w,the_GC);
	}
}

#undef yPoint
#undef x1Point

static void destroy_grapher(node)
     Node *node;
{
    int i;
    Node *temp;

    temp= node->child;
    if (temp)
	destroy_grapher(temp);

    temp= node->sibling;
    if (temp)
	{
	    destroy_grapher(temp);
	}
    XtFree((char *)node->label);
    XtFree((char *)node->node_name);
    XtFree((char *)node);
}

static void Destroy(w)
     Widget w;
{
    GrapherWidget gw = (GrapherWidget)w;

    if (gw->grapher.gc != NULL)
       XtReleaseGC(w,gw->grapher.gc);
    if (gw->grapher.root) /* if not, then grapher had an error condition
			     on setup */
	destroy_grapher(gw->grapher.root); 


}


AG_ReshowGrapher(w)
     Widget w;
{
    GrapherWidget gw = (GrapherWidget)w;

    if (gw->grapher.gc != NULL)
       XtReleaseGC(w,gw->grapher.gc);

    /* do it again */

    Initialize(w,w);
    Redisplay(w,NULL,NULL);
}

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

/****************************************************************

  KNEDIT.C

  The display widget for the Answer Garden (formerly the
  Knowledge Garden (hence the name KNowledge garden EDITor)).

  This is a compound/convenience widget based on Russ Sasnett's
  Layout widget.


  Mark Ackerman
  Information and Computer Science
  University of California, Irvine
  
  formerly -
  MIT/Center for Coordination Science
  
  ackerman@ics.uci.edu

  notes:
    6/16/89   Allow bold and italic.  Required changing mnemomic for 
                button.
              Corrected action on \n to end line and on a blankline
	        to actually give a blankline in the output.
    6/18/89   Moved commands out of central Redisplay loop into FindCommand.
              Changed mnemomics again.  See array commandStrings.
              Allowing <, (, [, { as delimiters.  No nesting allowed yet.
	        Routine FindDelimiter installed.
	      Now allows setting of foreground and background pixels.
	      Will display the error message for file that doesn't
	        exist or is empty within the browser rather than coming
		up blank.
    9/15/89   Changed height and width from ints to dimensions.
              Changed strcasecmp (DEC C) to strncmplo (internal)
    4/11/91   Sent in nodeName to grapherButton on create for selections
	      resize knedit on larger text size


*****************************************************************/



#include        <stdio.h>
#include        <ctype.h>
#include	<X11/IntrinsicP.h>
#include	<X11/StringDefs.h>
#include	"KneditP.h"
#include        <X11/Xaw/Command.h>
#include        "GButton.h"
#include        "AGmacros.h"
#ifndef NO_FUNCOBJ
#include        "FuncObj.h"
#endif

/*****************

  For the KneditI.h file

*********************/

#define EOS  '\0'

typedef struct _passback {
    KneditWidget knedit;
    GrapherButtonWidget grapher_button;
    char *node_name;
    struct _passback *next_passback;
} *PassbackPtr, PassbackStruct;

#define DEFAULT_WIDTH 10
#define DEFAULT_HEIGHT 10
#define DEFAULT_LINE_HEIGHT 20

  /* for knedit.when_flag */
#define KW_INITIALIZE 0
#define KW_REALIZE    1
#define KW_DONE       2

#define ATSIGN  '@'

/****************************************************************
 *
 * Knedit Resources
 *
 ****************************************************************/


#define offset(field) XtOffset(KneditWidget, field)
static XtResource resources[] = { 
    {XtNleftMargin, XtCMargin, XtRPosition, sizeof(Position),
	 offset(knedit.left_margin), XtRImmediate, (XtPointer)20},
    {XtNrightMargin, XtCMargin, XtRPosition, sizeof(Position),
	 offset(knedit.right_margin), XtRImmediate, (XtPointer)20},
    {XtNtopMargin, XtCMargin, XtRPosition, sizeof(Position),
	 offset(knedit.top_margin), XtRImmediate, (XtPointer)50},
    {XtNlineHeight, XtCLineHeight, XtRDimension, sizeof(Dimension),
       offset(knedit.line_height), XtRImmediate, 
       (XtPointer)DEFAULT_LINE_HEIGHT},
    {XtNvSpace, XtCVSpace, XtRDimension, sizeof(Dimension),
	 offset(knedit.v_space), XtRImmediate, (XtPointer)4},

    {XtNfilename,XtCFilename,XtRString, sizeof(String),
	 offset(knedit.filename),XtRString,NULL},
    {XtNfont,  XtCFont, XtRFontStruct, sizeof(XFontStruct *),
	offset(knedit.font), XtRString, "XtDefaultFont"},
    {XtNboldFont, XtCFont, XtRFontStruct, sizeof(XFontStruct *),
	offset(knedit.bold_font), XtRString, "XtDefaultFont"},
    {XtNitalicFont, XtCFont, XtRFontStruct, sizeof(XFontStruct *),
	offset(knedit.italic_font), XtRString, "XtDefaultFont"},
    {XtNforeground,XtCForeground,XtRPixel,sizeof(Pixel),
	 offset(knedit.foreground),XtRString,XtDefaultForeground},
    {XtNinternalButtonCallback, XtCCallback, XtRCallback, sizeof(XtPointer),
	 offset(knedit.internal_button_callback), XtRCallback, (XtPointer)NULL},
    {XtNeditCallback, XtCCallback, XtRCallback, sizeof(XtPointer),
	 offset(knedit.edit_callback), XtRCallback, (XtPointer)NULL},
    {XtNcursor,XtCCursor,XtRCursor,sizeof(Cursor),
	 offset(knedit.cursor),XtRString,"arrow"},
    {XtNheight, XtCHeight, XtRDimension, sizeof(Dimension),
	 offset(core.height), XtRImmediate, (XtPointer)DEFAULT_HEIGHT},
    {XtNwidth, XtCWidth, XtRDimension, sizeof(Dimension),
	 offset(core.width), XtRImmediate, (XtPointer)DEFAULT_WIDTH},
      /* total height is for getvalues only.  It will be reset in Initialize */
    {XtNtotalHeight, XtCHeight, XtRDimension, sizeof(Dimension),
	 offset(knedit.total_height), XtRImmediate, (XtPointer)10}, 
    {XtNforceSize, XtCForceSize, XtRBoolean, sizeof(Boolean),
	 offset(knedit.force_size), XtRImmediate, (XtPointer)False},
    {XtNeditMode, XtCEditMode, XtRBoolean, sizeof(Boolean),
	 offset(knedit.edit_mode), XtRImmediate, (XtPointer)False},
    {XtNnodeName,XtCNodeName,XtRString, sizeof(String),
	 offset(knedit.knedit_node_name),XtRString,NULL},
    {XtNseparatorHeight, XtCLineHeight, XtRDimension, sizeof(Dimension),
       offset(knedit.separator_height), XtRImmediate, 
       (XtPointer)DEFAULT_LINE_HEIGHT},
    {XtNdynamicCallback, XtCCallback, XtRCallback, sizeof(XtPointer),
	 offset(knedit.dynamic_callback), XtRCallback, (XtPointer)NULL},
    /* funcObj handles general AG functions such as @close and @system.
       If it's not present, this instance can't handle them. */
#ifndef NO_FUNCOBJ
    {XtNfuncObj, XtCWidget, XtRWidget, sizeof(Widget),
	 offset(knedit.func_obj), XtRPointer, (XtPointer)NULL},
#endif
};
#undef offset
 
/****BUG:   line height ought to be some multiple (as in float) of 
   the maximum vertical size of theprevious line */

/*****************************************************************
 *
 *  Actions and translations
 *
 ***************************************************************/




static void KneditInternalEditAction();

static void Internal_Button_Callback();

static XtActionsRec actionsList[] =
{
  {"knedit_internal_edit_action",		KneditInternalEditAction},
};





/****************************************************************
 *
 * Full class record constant
 *
 ****************************************************************/

static void Initialize();
static void Redisplay();
static void Destroy();
static void Realize();



#define SuperClass ((LayoutWidgetClass)&layoutClassRec)
KneditClassRec kneditClassRec = {
  {
/* core_class fields      */
    /* superclass         */    (WidgetClass) SuperClass,
    /* class_name         */    "Knedit",
    /* widget_size        */    sizeof(KneditRec),
    /* class_initialize   */    NULL,
    /* class_part_init    */	NULL,
    /* class_inited       */	FALSE,
    /* initialize         */    Initialize,
    /* initialize_hook    */	NULL,
    /* realize            */    Realize,
    /* actions            */    actionsList,
    /* num_actions	  */	XtNumber(actionsList),
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
  },{
/* Layout class fields */
    /* empty		  */	0,
  },{
/* Knedit class fields */
      /* empty            */    0,
			    },
};

WidgetClass kneditWidgetClass = (WidgetClass)&kneditClassRec;


/****************************************************************
 *
 * Private Routines
 *
 ****************************************************************/


  /* Put up a message saying that the widget is in an error state */
  /* Moved from inside Initialize 6/19/91 to handle errors when
     creating buttons and system calls (done inside Redisplay) */
static void Put_Error_Knedit(kw)
     KneditWidget kw;
{
   Widget label;
   Dimension temp_width;
   Dimension temp_height;
   Dimension width_return;
   Dimension height_return;
   char buffer[MAXBUF];
   XtGeometryResult rc;


     /* create a label indicating that the widget is
	in an error state */
   sprintf(buffer,"%s is in an error state",kw->core.name);
   label = XtVaCreateManagedWidget("errorLabel",labelWidgetClass,
				   (Widget)kw,
				   XtNlabel, (XtArgVal)buffer,
				   XtNsensitive, (XtArgVal)False, 
				   XtNborderWidth, (XtArgVal)0,
				   NULL);
     /* figure out how high and wide the label is */
   XtVaGetValues(label, XtNwidth, &temp_width, 
		 XtNheight, &temp_height,
		 NULL);
     /* Try to make the widget just slightly larger than the
	size of the label */
     /* This code is called inside Initialize only right now
	so I could just set the core.height and core.width
	directly.  But, I'm not sure it won't be called from
	elsewhere later, so I'm going through the geometry
	request.  It should be equivalent for Initialize */
   rc = XtMakeResizeRequest((Widget)kw,temp_width+10,temp_height+10,
			    &width_return,&height_return);
     /* accept whatever the parent offers */
   if (rc == XtGeometryAlmost)
      XtMakeResizeRequest((Widget)kw,width_return,height_return,
			  NULL,NULL);
     /* in any case, get the current height */
   XtVaGetValues(label, XtNheight, &temp_height,
		 NULL);  
   kw->knedit.total_height = kw->core.height; /* 4/11/91 */
   kw->knedit.when_flag = KW_REALIZE;
   return;
}



/* ARGSUSED */
static void Initialize(request, new)
    Widget request, new;
{
    KneditWidget kw = (KneditWidget)new;
    char *text;
    XGCValues values;
    char buffer[MAXBUF];
    Widget label;
    char Buffer[MAXBUF];
    Dimension temp_width;
    Dimension temp_height;

    kw->knedit.error_flag = False;

    if (kw->knedit.filename == NULL)
	{
	    sprintf(buffer,"Knedit file cannot be opened.\n");
	    kw->knedit.textbuffer = buffer;
	    kw->knedit.error_flag = True;
	}
    else
	{
	  kw->knedit.textbuffer = kw->knedit.filename;  /* FOR NOW */
	}

    /*  Eventually will have to go to Redisplay here to layout
	size for the first time.  This really only prints an
	error message for now if the file is missing.  */

    if (kw->knedit.error_flag)
	{
	   Put_Error_Knedit(kw);
	   return;
	}

    if (kw->knedit.knedit_node_name)
       kw->knedit.knedit_node_name = XtNewString(kw->knedit.knedit_node_name);

    values.foreground = kw->knedit.foreground;
    values.background = kw->core.background_pixel;
    values.font = kw->knedit.font->fid;
    kw->knedit.normal_gc = XtGetGC(new,
				   GCForeground|GCBackground|GCFont,
				   &values);
    values.font = kw->knedit.bold_font->fid;
    kw->knedit.bold_gc = XtGetGC(new,
				 GCForeground|GCBackground|GCFont,
				 &values);
    values.font = kw->knedit.italic_font->fid;
    kw->knedit.italic_gc = XtGetGC(new,
				   GCForeground|GCBackground|GCFont,
				   &values);
    
    kw->knedit.when_flag = KW_INITIALIZE;
    kw->knedit.first_passback = NULL; /* added 3/18/91 */
    kw->knedit.total_height = 0;

    Redisplay(new,NULL,NULL);
      /* check to see if there was an error in the Redisplay
	 procedures - usually on a create button or a system
	 call 6/19/91 */
    if (kw->knedit.error_flag)
       Put_Error_Knedit(kw);
       
    temp_height = kw->core.height;

    /* total_height is the height required by the knedit to show
       it's stuff.  It gets set in Redisplay.  If it's larger than
       the height for the widget, then set core.height to be that size. ?? */
    if (kw->knedit.total_height > kw->core.height || kw->knedit.force_size)
      kw->core.height = kw->knedit.total_height;

    /* If the knedit isn't to be forced to the total size needed, then
       reset total_height to show the size of each page.  If total_height
       is larger than the core.height, then total_height now is the
       size of each page.  If total_height was less, they are now
       the same value. */
    if (!kw->knedit.force_size)
      kw->knedit.total_height = temp_height;

    kw->knedit.when_flag = KW_REALIZE;

} /* Initialize */


static void Realize(w,value_mask,attributes)
     Widget w;
     XtValueMask *value_mask;
     XSetWindowAttributes *attributes;
{
  KneditWidget kw = (KneditWidget)w;
  
  kw->knedit.when_flag = KW_REALIZE;
  Redisplay(w,NULL,NULL);
  kw->knedit.when_flag = KW_DONE;

  (SuperClass->core_class.realize)(w,value_mask,attributes);
  return;
} /* Realize */



#ifdef OLD
static void normal_char(flag,tokenptr,textbuffer,curpos)
     int *flag;
     char **tokenptr;
     char textbuffer[];
     int curpos;
{
    *flag = True;
    *(*tokenptr++) = textbuffer[curpos];
}
#endif


#ifdef NEXT
typedef struct _environment {
    int environmentType;
    XFontStruct *currentFont;
    GC currentGC;
    char delimiter;
    Position left_margin;
    Position right_margin;
    Dimension spacing;
    Position indent;
} environment;
#endif


#define DoButton  0
#define DoBold    1
#define DoItalic  2
#define DoAtSign  3

#define NCommandStrings 7

  
static char *commandStrings[NCommandStrings] = 
      {"button","command","bold","b","italic","i","@"};

static int commandNumber[NCommandStrings] = 
          {DoButton,DoButton,DoBold,DoBold,DoItalic,DoItalic,
	       DoAtSign};
static int nCommandStrings[NCommandStrings] = {6,7,4,1,6,1,1};



#define NDelimiters  4
static char startDelimiters[NDelimiters] = {'{','[','(','<'};
static char endDelimiters[NDelimiters] = {'}',']',')','>'};


/***********************************************************************
  Find_Delimiter
     routine determines what delimiter matches the char specified

  incoming vars
     buffer    text buffer
     loc       index into text buffer
  returns
     delimiter (ie, index into startDelimiters and endDelimiters arrays)

*/
static int Find_Delimiter(buffer,loc)
     char *buffer;
     int loc;
{
    int j;

    for (j=0;j<NDelimiters;j++)
	if (buffer[loc] == startDelimiters[j])
	    return(j);
    return(-2);
}


/*************************************************************************
    Find_Command
       routine determines what command string is next in the buffer

    incoming vars
        buffer     text buffer
	i          present location in the buffer
    return vars
        i          location at the end of the command string
	delimiter  what delimiter is being used for the command (ie,
	             next delimiter after the end of the command)
    returns
        command id (ie, index into various command arrays)
    error-returns
        value < 0

*/
static int Find_Command(buffer,i,delimiter)
     char *buffer;
     int *i;
     int *delimiter;
{
    int j;
    int command;
    int string;
    

    for (j=0;j<NCommandStrings;j++)
	if (strncmplo(&(buffer[*i]),commandStrings[j],
			nCommandStrings[j]) == 0)
	    break;

    if (j >= NCommandStrings)
	return(-1);

    string = j;
    command = commandNumber[j];

    if (command == DoAtSign)
	return(command);

    if ((*delimiter = Find_Delimiter(buffer,*i+nCommandStrings[string])) < 0)
	return(*delimiter);
    
    *i = *i+nCommandStrings[string];
    return(command);
}
	       
								    


#define UseNormalFont 0
#define UseBoldFont 1
#define UseItalicFont 2

static void SetupFont(kw,fontType,temp_GC,temp_fontStruct)
     KneditWidget kw;
     int fontType;
     GC *temp_GC;
     XFontStruct **temp_fontStruct;
{
    switch (fontType)
	{
	  case UseNormalFont:
	    *temp_GC = kw->knedit.normal_gc;
	    *temp_fontStruct = kw->knedit.font;
	    break;
	  case UseBoldFont:
       	    *temp_GC = kw->knedit.bold_gc;
	    *temp_fontStruct = kw->knedit.bold_font;
	    break;
	  case UseItalicFont:
	    *temp_GC = kw->knedit.italic_gc;
	    *temp_fontStruct = kw->knedit.italic_font;
	    break;
	}
    return;
}

static void Make_Button(kw,i,token,flag,nextY_orig,delimiter,height,width)
     KneditWidget kw;
     int *i;
     char *token;
     Boolean *flag;
     Position *nextY_orig;
     int delimiter;
     Dimension *height;
     Dimension *width;
{
    char *pos1, *pos2;
    Position nextY;
    Widget button;
    int temp;
    char *remember;
    char *node_name;
    char argToken[MAXBUF];
    PassbackPtr passback;
    PassbackPtr temp_pass_ptr;
    PassbackPtr prev;

    nextY = *nextY_orig;  /* hack to deal with nextY */
    if ((pos2 = AGindex(&(kw->knedit.textbuffer[*i]),endDelimiters[delimiter]))
	== NULL)
	{
	    XtWarning("no end delimiter on button\n");
	    kw->knedit.error_flag = True;
	    return;
	}
    else
	{
	    pos1 = &(kw->knedit.textbuffer[(*i)+1]);
	    remember = pos1;
	    temp = (int)(pos2-pos1);
	    AGstrncpy(token,pos1,temp);
	    token[temp] = EOS;
	    *i += ++temp; /* also eats the end delimiter */
	    *flag = False;
	}

    if (kw->knedit.when_flag == KW_INITIALIZE)
	{

	    if ((pos1 = AGindex(remember,',')) != NULL)
 		{
		    temp = (int)(pos1-remember);
		    token[temp] = EOS;
		    while (isspace(*(++pos1))); /* 3/18/91 fix for
						spaces after comma */
		    temp = (int)(pos2-pos1);
		    if (temp <= 0) /* ie, the next comma is after the end
				      delimiter */
		       {
			  XtWarning("error in button statement in knedit");
			  kw->knedit.error_flag = True;
			  return;
		       }
		    AGstrncpy(argToken,pos1,temp);
		    argToken[temp] = EOS;
		    if (AGstrlen(argToken) > 0)
		       node_name = XtNewString(argToken);
		    else
		       {
			  XtWarning("error in button statement in knedit");
			  kw->knedit.error_flag = True;
			  return;
		       }
		}
	    else
	       {
		  XtWarning("link error in button statement in knedit");
		  kw->knedit.error_flag = True;
		  return;
		  /* later this should get the label from the node service */
#ifdef LATER
		  argToken[0] = EOS;
#endif
	       }
			 
	    /* Do this to create off screen and then move on-screen when size
	       of the button is known (after it is created) */
	    button = XtVaCreateManagedWidget(token,
					     grapherButtonWidgetClass,
					     (Widget)kw,
					     XtNx, (XtArgVal) 1000,
					     XtNy, (XtArgVal) nextY,
					     XtNselectionString, 
					       (XtArgVal)node_name, 
					     XtNeditMode,
					       (XtArgVal)kw->knedit.edit_mode,
					     NULL);

	    XtVaGetValues(button,XtNwidth, (XtArgVal)width,
			         XtNheight, (XtArgVal)height,
			         NULL);
	    /* 10/5/92 */ /* this moves a button half on a page to the next
	       page */

	    if (((int)(nextY + *height))%((int)kw->core.height) < 
		((int)nextY)%((int)kw->core.height))
	      nextY = nextY + *height;

	    XtVaSetValues(button, XtNx, (XtArgVal)
                   			  ((kw->core.width - *width)/2),
			          XtNy, (XtArgVal)nextY,
			          NULL);
	    passback = (PassbackPtr)XtMalloc (sizeof(PassbackStruct));
	    passback->knedit = kw;
	    passback->node_name = node_name;
	    passback->grapher_button = (GrapherButtonWidget) button;

	    if (kw->knedit.first_passback)
		{
		    temp_pass_ptr = (PassbackPtr)kw->knedit.first_passback;
		    while (temp_pass_ptr)
			{
			    prev = temp_pass_ptr;
			    temp_pass_ptr = temp_pass_ptr->next_passback;
			}
		    prev->next_passback = passback;
		}
	    else
		kw->knedit.first_passback = (char *) passback;

	    passback->next_passback = NULL;
		    
	    XtAddCallback(button,XtNcallback,
			  Internal_Button_Callback,(XtPointer)passback);
	}
    else
      {
	  /* kludge:  they're all the same */
	XtVaGetValues((Widget)((PassbackPtr)(kw->knedit.first_passback))
		                              ->grapher_button,
		      XtNwidth, (XtArgVal)width, 
		      XtNheight,(XtArgVal)height, 
		      NULL);
      }
    *nextY_orig = nextY;
}    

static void Set_Font(kw,UseFont,i,setFontToNormalFlag,fontType,delimiter)
     KneditWidget kw;
     int UseFont;
     int *i;
     Boolean *setFontToNormalFlag;
     int *fontType;
     int *delimiter;
{
    if ((*delimiter = Find_Delimiter(kw->knedit.textbuffer,*i)) < 0)
      {
	XtWarning("no start delimiter in knedit\n");
	kw->knedit.error_flag = True;
      }
    else
	{
	    *fontType = UseFont;
	    *setFontToNormalFlag = False;
	}
}

/**************************************************************************

  Get_Command_Token
    routine returns everything between matching delimiters

  incoming
    buffer     text buffer
    delimiter  index into startDelimiters array previously determined
    i          index into buffer (assumes set to location of starting
                 delimiter)
    len        length of text buffer
  return vars
    token      string containing everything between the matching delimiters
                 (calling routine is responsible for allocation)
    i          index into buffer at the position of the ending delimiter
  returns
    True if successful, False if not

*/
static Boolean Get_Command_Token(buffer,token,delimiter,i,len)
     char *buffer;
     char *token;
     int *delimiter;
     int *i;
     int len;
{
   char *remember;
   int remember_i;
   if ((*delimiter = Find_Delimiter(buffer,*i)) < 0)
      {
	 XtWarning("no start delimiter in knedit command");
	 return(False);
      }
   else
      {
	 remember_i = ++(*i);
	 for ( ;*i<len  ;(*i)++)
	    if (buffer[*i] == endDelimiters[*delimiter])
	       break;
	 if (*i < len )
	    {
	       AGstrncpy(token,&buffer[remember_i],
			 *i - remember_i);
	       token[*i-remember_i] = EOS;
	    }
	 else
	    {
	       token[0] = EOS;
	       XtWarning("no end delimiter in knedit command");
	       return(False);
	    }
      }
   return(True);
}



#define IsLineStrattle(page_height) \
				  (((int)(nextY+kw->knedit.line_height)) \
				       % ((int)page_height) < \
				      ((int)nextY)%((int)page_height))


static void Redisplay(w, event, region)
    Widget w;
    XEvent *event;
    Region region;
{
    KneditWidget kw = (KneditWidget)w;

    int i;
    Boolean flag;
    Position nextX;
    Position nextY;
    char token[MAXBUF];
    char *tokenptr;
    int leftOnWindow;
    int len;
    int temp;
    GC temp_GC;
    XFontStruct *temp_fontStruct;
    int fontType;
    Boolean setFontToNormalFlag;
    int delimiter;
    int command;

    Dimension height;  /* 3/18/91 */
    Dimension width;
    Boolean just_did_button; /* 7/2/91 */

    if (kw->knedit.error_flag)
       {
	  return;
       }

    /* 11/4/91 add 1 to length to allow check of EOS (see comment below) */
    len = AGstrlen(kw->knedit.textbuffer) +1;
    
    nextX = kw->knedit.left_margin;
    nextY = kw->knedit.top_margin;
    tokenptr = token;
    flag = False;
    fontType = UseNormalFont;
    setFontToNormalFlag = False;
    delimiter = -1;
    just_did_button = False; /* 7/2/91 */

    for (i=0;i<len;i++)
      {
	  switch (kw->knedit.textbuffer[i])
	      {
		case '\0':
		  /* 11/4/91:  on EOS, if something left in the
		     buffer, flush it */
		  if (!flag)
		    break;
		case ' ':
		case '\n':
		  

		    if (flag) /* if there was a normal char */
			{
			    SetupFont(kw,fontType,&temp_GC,&temp_fontStruct);
			    flag = False;
			    leftOnWindow = w->core.width-nextX
				- (kw->knedit.left_margin + 
				   kw->knedit.right_margin);
			    *(tokenptr++) = ' ';
			    *tokenptr = EOS; /* for the strlen */
			    if ((temp = XTextWidth(temp_fontStruct,token,
					   AGstrlen(token))) > leftOnWindow)
				{
				  nextY += kw->knedit.line_height;
				  nextX = kw->knedit.left_margin;

				  /* if the line is partway between two pages
				     kick it over to the next page 10/20/92*/
				  
				  /* This is ugly because the value of
				     core.height shifts between initialize
				     and realize */ /* 10/20/92 */
				  if (kw->knedit.when_flag == KW_INITIALIZE )
				    {
				      if (IsLineStrattle(kw->core.height))
					nextY += kw->knedit.line_height;
				    }
				  else if
				    (IsLineStrattle(kw->knedit.total_height))
					nextY += kw->knedit.line_height;

			      }
			    if (XtIsRealized(w))
				XDrawString(XtDisplay(w),XtWindow(w),
					    temp_GC,nextX,nextY,token,
					    AGstrlen(token));
			    just_did_button = False; /* 7/2/91 */
			    tokenptr = token;
			    token[0] = EOS;
			    nextX += temp;
			    if (setFontToNormalFlag)
				{
				    fontType = UseNormalFont;
				    setFontToNormalFlag = False;
				}
			    /*XFlush(XtDisplay(w));*/
			}
		    else /* check for an empty line */
			{
			    if (kw->knedit.textbuffer[i-1] == '\n')
				{
				    nextY += kw->knedit.line_height +
				      kw->knedit.separator_height;
				    nextX = kw->knedit.left_margin;
				}
			}
		    break;
		  case ATSIGN:
		    i++; 
		    if ((command = Find_Command(kw->knedit.textbuffer,
						&i,&delimiter)) < 0)
		      {
			/* Don't know how to handle the command, so
			   check to see if General AG Command */
			/* funcObj handles general AG functions 
			   such as @close and @system.  If
			   it's not present, this instance can't handle them. */
			i--; /* get the @ back */
#ifndef NO_FUNCOBJ
			if (kw->knedit.func_obj)
			  {

			    AGFuncObjHandle(kw->knedit.func_obj,
					    kw->knedit.textbuffer,
					    &i,
					    &kw->knedit.error_flag,
					    (kw->knedit.when_flag == KW_REALIZE)
					         ? False : True,
					    False, NULL);
			    if (kw->knedit.error_flag == True)
			      return;
			  }
#endif
		      }
		    else switch (command)
		      {
		      case DoAtSign:
			/* should be same as normal character */
			flag = True;
			*(tokenptr++) = kw->knedit.textbuffer[i];
			break;

		      case DoButton:
			if (!just_did_button) /* 7/2/91 */
			  {
			    nextY += kw->knedit.line_height;
			  }
			Make_Button(kw,&i,token,&flag,&nextY,delimiter,
				    &height,&width);
			/* test whether error inside Make_Button */
			if (kw->knedit.error_flag) /* 6/19/91 */
			  return;
			/****BUG:  need to have different heights */
			    /* nextY += height + kw->knedit.v_space;*/
			nextY += height + kw->knedit.v_space 
			  + kw->knedit.separator_height; /* 7/2/91 */
			nextX = kw->knedit.left_margin; /*7/2/91 */
			just_did_button = True;
			token[0] = EOS;
			break;

		      case DoBold:
			Set_Font(kw,UseBoldFont,&i,&setFontToNormalFlag,
				 &fontType,&delimiter);
			break;
		      case DoItalic:
			Set_Font(kw,UseItalicFont,&i,&setFontToNormalFlag,
				 &fontType,&delimiter);
			break;

			
		      default:
			break;
		      }
		    break;
		  case '>':
		  case ']':
		  case ')':
		  case '}':
		    if (delimiter > 0 && 
			kw->knedit.textbuffer[i] == endDelimiters[delimiter])
			if (fontType != UseNormalFont)
			    {
				setFontToNormalFlag = True;
				break;
			    }
		    /* else fall thru */
		    /* if curEnvironment->delimiter == this then
		          pop up an environment stack
			  swallow character
		       else
		          treat it like a normal character
		    */
		  default:
		    flag = True;
		    *(tokenptr++) = kw->knedit.textbuffer[i];
		    break;
	      }
      }
    if (kw->knedit.when_flag == KW_INITIALIZE)
      kw->knedit.total_height = nextY;

    return;
}
	    


static void Destroy(w)
     Widget w;
{
    KneditWidget kw = (KneditWidget)w;
    PassbackPtr temp;
    PassbackPtr prev;


    if (kw->knedit.error_flag)  /* 4/11/91 */
      return;

    XtReleaseGC(w,kw->knedit.normal_gc);
    XtReleaseGC(w,kw->knedit.bold_gc);
    XtReleaseGC(w,kw->knedit.italic_gc);

    /* Got to find all the passback structs and their strings */
    temp = (PassbackPtr)(kw->knedit.first_passback);
    while (temp)
	{
	    prev = temp;
	    temp = temp->next_passback;
	    XtFree(prev->node_name);
	    XtFree((char *)prev);
	}
    XtFree(kw->knedit.knedit_node_name);
}




static void Internal_Button_Callback(w,client_data,call_data)
     Widget w;
     XtPointer client_data;
     XtPointer call_data;
{
    PassbackPtr passback = (PassbackPtr)client_data;
    KneditWidget kw = (KneditWidget)(passback->knedit);
    AGGrapherButtonCallbackStruct *callback_struct;

    int i;
    int delimiter;
    int command;
    char token[MAXBUF];
    char *buffer;
    Boolean error;

    /* check to see if the node name coming back is a valid
       node name or whether it is a command */

    callback_struct = (AGGrapherButtonCallbackStruct *)call_data;
    buffer = callback_struct->selection_string;    
#ifndef NO_FUNCOBJ
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
	    if (kw->knedit.func_obj)
	      {
		i = 0;
		AGFuncObjHandle(kw->knedit.func_obj,
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
#endif
      /* Not a command, so return the callback to appropriate party
	 depending on the callback type */
      if (callback_struct->notify_type == AGGrapherButtonNodeNotify)
	XtCallCallbacks((Widget)(passback->knedit),
			XtNinternalButtonCallback,
			call_data);
      else if (callback_struct->notify_type == AGGrapherButtonEditNotify)
	{
	  if (kw->knedit.edit_mode)
	    XtCallCallbacks((Widget)(passback->knedit),XtNeditCallback,
			    call_data);
	}
      else
	XtCallCallbacks((Widget)(passback->knedit),XtNdynamicCallback,
			call_data);
    
}


static void KneditInternalEditAction(w,xevent,params,num_params)
     Widget w;
     XEvent *xevent;
     String *params;		/* unused */
     Cardinal *num_params;
{
   KneditWidget kw = (KneditWidget) w;
   AGGrapherButtonCallbackStruct callback_struct;
   
   callback_struct.event = xevent;
   callback_struct.notify_type = AGGrapherButtonEditNotify;
   callback_struct.selection_string = kw->knedit.knedit_node_name;
   callback_struct.params = NULL;
   callback_struct.num_params = 0;

   XtCallCallbacks((Widget)kw,XtNeditCallback,(XtPointer)&callback_struct);
}


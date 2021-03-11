/**********************************************************

  The ANSWER GARDEN PROJECT

  AG.C  Answer Garden main module

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

  Answer Garden

  AG.c


  Mark Ackerman
  Information and Computer Science
  University of California, Irvine

  formerly -
  MIT/Center for Coordination Science

  ackerman@ics.uci.edu


  RELEASE 1.10  6/4/94
  

  system dependencies:
    1.  Program uses the "system" call to send mail.  See mailer types.
        The system handles MH and /bin/mail.
    2.  Program uses gettimefday to get system time.

  To compile Answer Garden to disable editing, give cc the flag -DNO_EDIT.

  
  NOTE of personal disclaimer (READ THIS BEFORE READING THE CODE):
     Like most things that tend to smell after too long a time,
     code ought to have an expiration date.  This does.
     If you are reading this code more than 3 months
     its release date, don't bother.  I figured better ways
     of doing things most likely, and you should get the newer code.
     Contact me at the email addresses below.  

     (Thanks to Stephen Gildea for the idea.)

  Note of personal interest:
     Notwithstanding the above, I am extremely interested in bugs,
     suggestions, and general comments you might have about this code.
     I am particularly interested in the uses you (yes, you) may make
     of this code, so let me know...

                              Mark Ackerman
			      ("Ack")

  TO GET NEW RELEASES AND BUG FIXES
     Send your email address to ackerman=ag@ics.uci.edu


  THANKS to the following people who have kindly reported back
  updates and patches:
        Eric Mandel (Smithsonian Astrophysical Observatory)
        John Roll (Smithsonian Astrophysical Observatory)
        Klaus Elsbernd (DFKI)
        Alyce Brady (RPI)

***********************************************************/

#include <X11/StringDefs.h>
#include <X11/Intrinsic.h>
#include <X11/Shell.h>
#include <X11/Xaw/Command.h>
#include <X11/Xaw/Box.h>
#include <X11/Xaw/AsciiText.h>
#include <X11/Xaw/Viewport.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/List.h>
#include <X11/Xaw/SimpleMenu.h>
#include <X11/Xaw/SmeBSB.h>
#include <X11/Xaw/SmeLine.h>
#include "AG.h"
#include "Knedit.h"
#include "Layout.h"
#include "Grapher.h"
#include "FuncObj.h"
#include "StackObj.h"
#include "Outline.h"

#define AGRootGrapher "RootGrapher"
#define AGRoot        "Root"
#define AGMissingNode "MissingNode"
#define MissingFileMessage \
"Unable to open this node's physical file.  \n\
Please contact your site administrator. \n"

void AG_Create_Node_Callback_For_GrapherButton();
static void Create_Null_Node();
static void Internal_Close_Callback();
Widget AG_Create_Node();
void AG_Close_Node();
void Edit_Node_Callback();
static void Show_Root_Grapher();
static void Show_Root();
void AG_Dynamic_Callback_For_GrapherButton();
static void Dynamic_Create_Node_Callback();
void General_Null_Callback();

Boolean Util_Get_And_Check_Timestamp();

extern GlobalInfo global_info;
extern NodeType *global_node_types[];
extern UserSettings user_settings;

extern void Util_Open_ButtonBox_With_Bindings();
extern void User_History_List_Add_Member();
extern void Edit_Initialize();
extern Boolean CommService_Initialize();
extern int NodeService_Initialize();
extern XtPointer AG_Stack_Go_Back();

/****************************

  SBROWSER ROUTINES

****************************/


void General_Crush_Callback(w,client_data,call_data)
     Widget w;
     XtPointer client_data;
     XtPointer call_data;
{
    char *label;
    AGNodeTypes type;
    char *location;
    char buffer[MaxString];
    NodeInfoPtr node_info;
    Widget shell;

    if (call_data == NULL)
      return;

    if ((NodeService_Request_By_Name((char * )call_data,&node_info,
				     &label,&type,&location) >= 0))
	{
	  if ((shell = NodeService_Get_Shell(node_info)) != NULL)
	    /* MSA 6/2/94 patch from SAO */
	    Internal_Close_Callback(shell,(XtPointer)node_info,NULL,True);
	}
}	  


static void SBrowser_Destroy_Callback(w,client_data,call_data)
     Widget w;
     XtPointer client_data;
     XtPointer call_data;
{
  /* not needed for now */
}


static void SBrowser_Back_Callback(w,client_data,call_data)
     Widget w;
     XtPointer client_data;
     XtPointer call_data;
{
    Widget knedit = (Widget)client_data;
    Dimension shown_height, height;
    Widget inner_form;
    Widget viewport;
    Position y;
    Position goto_y;

    Stat_Button_Hit(w);

    inner_form = XtParent(knedit);

    XtVaGetValues(knedit,XtNtotalHeight,(XtArgVal)&shown_height,
		  XtNheight,(XtArgVal)&height,NULL);
    XtVaGetValues(inner_form,XtNy,(XtArgVal)&y,NULL);

    goto_y = y+shown_height;
    if ( goto_y > 0)
      goto_y = 0;
    XtVaSetValues(inner_form,XtNy,(XtArgVal)goto_y, NULL);
    
}

static void SBrowser_Forward_Callback(w,client_data,call_data)
     Widget w;
     XtPointer client_data;
     XtPointer call_data;
{
    Widget knedit = (Widget)client_data;
    Dimension shown_height, height;
    Widget inner_form;
    Widget viewport;
    Position y;
    int temp;

    Stat_Button_Hit(w);

    inner_form = XtParent(knedit);

    XtVaGetValues(knedit,XtNtotalHeight,(XtArgVal)&shown_height,
		  XtNheight,(XtArgVal)&height,NULL);
    XtVaGetValues(inner_form,XtNy,(XtArgVal)&y,NULL);

    temp = y-shown_height;
    if ( (int)(temp + height) > 0 )
	XtVaSetValues(inner_form,XtNy,(XtArgVal)temp, NULL);
    

}

/**062592** routine separated out */
/**092192** moved checks of stacks out, modifed to call ConvertAndStore */
/**060294** made generally callable routine returning Boolean */
Boolean AG_Get_Resource(w,node_info,resource_name,
				  resource_class,to_resource_type,to_value)
     Widget w;
     NodeInfo *node_info;
     String resource_name;
     String resource_class;
     String to_resource_type;
     XrmValue *to_value;
{
  Widget shell;
  char name_buffer[MaxString];
  char class_buffer[MaxString];
  char *resource_type;
  XrmValue value;
  Boolean rc;
  int j;
  
  /* MSA 6/2/94 moved in SAO changes to allow more flexibility in
     the application name and class */
  AGstrcpy(name_buffer,global_info.program_name);
  AGstrcat(name_buffer, ".");
  AGstrcat(name_buffer,NodeService_Get_Node_Name(node_info));
  AGstrcat(name_buffer,".");
  AGstrcat(name_buffer,resource_name);
  AGstrcpy(class_buffer,global_info.program_name);
  AGstrcat(class_buffer, ".");
  AGstrcat(class_buffer,NodeService_Get_String_From_Type(
		       NodeService_Get_Type(node_info)));
  AGstrcat(class_buffer,".");
  AGstrcat(class_buffer,resource_class);

  if ((rc = XrmGetResource(XtDatabase(XtDisplay(w)),name_buffer,class_buffer,
		      &resource_type,&value)) == False)
    {
      AGstrcpy(name_buffer,global_info.class_name);
      AGstrcat(name_buffer, ".");
      AGstrcat(name_buffer,NodeService_Get_Node_Name(node_info));
      AGstrcat(name_buffer,".");
      AGstrcat(name_buffer,resource_name);
      AGstrcpy(class_buffer,global_info.class_name);
      AGstrcat(class_buffer, ".");
      AGstrcat(class_buffer,NodeService_Get_String_From_Type(
				     NodeService_Get_Type(node_info)));
      AGstrcat(class_buffer,".");
      AGstrcat(class_buffer,resource_class);
#ifdef OLD
      AGstrcpy(name_buffer,"AnswerGarden.");
      AGstrcat(name_buffer,NodeService_Get_Node_Name(node_info));
      AGstrcat(name_buffer,".");
      AGstrcat(name_buffer,resource_name);
      AGstrcpy(class_buffer,"AnswerGarden.");
      AGstrcat(class_buffer,NodeService_Get_String_From_Type(
		     NodeService_Get_Type(node_info)));
      AGstrcat(class_buffer,".");
      AGstrcat(class_buffer,resource_class);
#endif 
      /* MSA 6/2/94 put in second call to XrmGetResource if first fails.
	 Not in SAO patches */
      rc = XrmGetResource(XtDatabase(XtDisplay(w)),name_buffer,class_buffer,
			  &resource_type,&value);
    }


#ifdef NEXT
  if (to_value == NULL) /* don't want a conversion *//* 10/19/92 */
    return;
#endif

  if (!rc)
    {
      /* MSA 6/2/94 changed to avoid gcc errors.  Calling routines
	 should use return code. */
      to_value->size = (unsigned int)NULL;
    }
  else
    XtConvertAndStore(global_info.main_shell,resource_type,&value,
		      to_resource_type, to_value);
  return (rc);
}  

/**062592**  new routine */
/**092192** modified to return Boolean */
static Boolean Geometry_Get_Lock(w,node_info)
     Widget w;
     NodeInfo *node_info;
{
  XrmValue to_value;
  Boolean locked;

  to_value.addr = (char *)&locked;
  to_value.size = sizeof(Boolean);
  if (!AG_Get_Resource(w,node_info,XtNlocked,XtCLocked,XtRBoolean,
		      &to_value) == True)
    return(False);
  else
    return(locked);
}  

/**062592** heavily modified */
/**092192** moved checks back into this routine */
/**060294** incorporated SAO changes MSA */
int AG_Geometry_Get_Stack(w,node_info)
     Widget w;
     NodeInfo *node_info;
{
  int stack;
  XrmValue to_value;

  /* the number of stacks ought to be positive, or we're in trouble */
  if (global_info.n_stacks <= 0)
    return(0);  /* no stacks anyways */

  to_value.addr = (char *)&stack;
  to_value.size = sizeof(int);

  /* Get the stack number.  If it can't be determined from the resource
     file, return stack = 0 which semantically means "make the user 
     do the placing" */
  if (!AG_Get_Resource(w,node_info,XtNstack,XtCStack,XtRInt,
		       &to_value))
      return(0);

  if (stack > 0)
    return(stack);
  else
    return(0);
}


#define DELTA 25
#define VIEWPORT_HEIGHT_FUDGE 50


void AG_Geometry(shell,node_info)
     Widget shell;
     NodeInfo *node_info;
{
  int stack_num;
  Position x;
  Position y;
  static  char geometry_string[MaxString];
  Boolean locked;

  if (global_info.n_stacks <= 0)
    return;  /* no stacks anyways */

  stack_num = AG_Geometry_Get_Stack(shell,node_info);

  if (stack_num == 0)
    return; /* not set geometry */

#ifdef NEXT
  if (global_info.locking_enabled) /**062592**/
    {
      old_node_info = (NodeInfo *) 
	AG_Stack_Go_Back(global_info.stacks[stack_num-1],False);
      old_shell = NodeService_Get_Shell(old_node_info);
      AG_Stack_Delete_Shell(global_info.stacks[stack_num-1],shell)
    }
#endif
  locked = Geometry_Get_Lock(shell,node_info);

  AG_Stack_Add_Shell(global_info.stacks[stack_num-1],shell,&x,&y,
		     (XtPointer)node_info, locked);
  sprintf(geometry_string,"+%d+%d",x,y);
  XtVaSetValues(shell,XtNgeometry,(XtArgVal)geometry_string,NULL);
}



Widget Open_SBrowser(junk_w,node_info,geometry,misc_info,num_info)
     Widget junk_w;
     NodeInfoPtr node_info;
     String geometry;
     String *misc_info;
     int num_info;
{
    Widget shell;
    Widget knedit;
    Widget box;
    Widget *command;
    Widget form;
    Arg arglist[10];
    char filestring[MaxString];

    Dimension width;
    Dimension total;
    Dimension hspace;
    WidgetClass create_class;
    int i;
    Widget viewport;
    Widget inner_form;
    Dimension total_height;
    Dimension height;
    Boolean force_size;
    char name_string[MaxString];
    char *filename;
    char msg[MaxString];
    int n;
    char *node_name;
    int num_buttons;

    if ((filename = NodeService_Get_Location(node_info)) == NULL)
      {
	 XtWarning("null name passed to discussion node.  Continuing...");
	 return(NULL);
      }

    Form_Filename(filestring,filename);
    if (!NodeService_Open_File(node_info,filestring))
      {
	sprintf(msg,"Error in opening node at location %s.",filename);
	XtWarning(msg);
	XtWarning("Continuing...");
	NodeService_Close_File(node_info);
	return(NULL);
      }



    n=0;
    XtSetArg(arglist[n],XtNinput,(XtArgVal)True);n++;
    XtSetArg(arglist[n],XtNallowShellResize,(XtArgVal)True);n++;
    XtSetArg(arglist[n],XtNtitle,(XtArgVal)NodeService_Get_Label(node_info));
    n++;

    if (geometry != NULL)
      {
	XtSetArg(arglist[n],XtNgeometry,(XtArgVal)geometry);n++;
      }


    shell = XtCreatePopupShell(NodeService_Get_Node_Name(node_info),
			       topLevelShellWidgetClass,
			       global_info.main_shell,
			       arglist,n);

    form =  XtCreateManagedWidget("sbrowserForm",formWidgetClass,
			  shell, NULL,0);

    Util_Open_ButtonBox_With_Bindings(form,&box,True,&command,
				      &num_buttons,(XtPointer)node_info);

    /* Gotta a bit of a problem, here.  I need to create all of the 
       buttons so I can figure out how wide the Knedit should be.
       Unfortunately, the callbacks for the next 2 buttons want
       a client_data of the knedit id.  So, the callbacks will
       be set later. 10/19/92 */

    Util_Add_Button_With_Bindings(box,command,&command,&num_buttons,
				  "forwButton",
				  NULL, NULL);
    Util_Add_Button_With_Bindings(box,command,&command,&num_buttons,
				  "backButton",
				  NULL, NULL);


    total = 0;
    for (i=0;i<num_buttons;i++)
      {
	XtVaGetValues(command[i],XtNwidth,(XtArgVal)&width,NULL);
	total = total+width+4;
      }
    width = total;  /* for use below */

    viewport = XtVaCreateManagedWidget("sbrowserViewport",layoutWidgetClass,
				       form,
				       XtNheight, (XtArgVal)50,
				       XtNborderWidth, (XtArgVal)0,
				       NULL);


      /* Todo: Get rid of this by making knedit subclass of form */
    inner_form =  XtVaCreateManagedWidget("sbrowserInnerForm",formWidgetClass,
					  viewport,
 					  XtNborderWidth, (XtArgVal)0,
					  NULL);

    node_name = NodeService_Get_Node_Name(node_info);
    knedit = XtVaCreateManagedWidget("sbrowserKnedit",
				     kneditWidgetClass,inner_form,
				     XtNnodeName, (XtArgVal)node_name,
				     XtNfilename, 
				       (XtArgVal)NodeService_Get_Text(
							 node_info),
				     XtNwidth, (XtArgVal)width,
				     XtNborderWidth, (XtArgVal)0,
				     XtNeditMode, 
				       (XtArgVal)global_info.edit_mode,
				     XtNfuncObj,
				       (XtArgVal)global_info.global_func_obj,
				     NULL);

    XtAddCallback(knedit,
		  XtNinternalButtonCallback,
		  AG_Create_Node_Callback_For_GrapherButton,
		  (XtPointer)knedit); 
    XtAddCallback(knedit,XtNeditCallback,Edit_Node_Callback,
		  (XtPointer)knedit);
    XtAddCallback(knedit,XtNdynamicCallback,
		  AG_Dynamic_Callback_For_GrapherButton,
		  (XtPointer)knedit);
    XtAddCallback(knedit,XtNdestroyCallback, SBrowser_Destroy_Callback, 
		  (XtPointer)node_info);


    XtVaGetValues(knedit,XtNtotalHeight, (XtArgVal)&total_height,
		         XtNheight, (XtArgVal) &height,
		         XtNforceSize, (XtArgVal) &force_size,
		         NULL);
    
    if (height > total_height)
	{
	    XtAddCallback(command[num_buttons-2],XtNcallback,
			  SBrowser_Forward_Callback,
			  (XtPointer)knedit);
	    XtSetSensitive(command[num_buttons-2],True);
	    XtAddCallback(command[num_buttons-1],XtNcallback,
			  SBrowser_Back_Callback,
			  (XtPointer)knedit);
	    XtSetSensitive(command[num_buttons-1],True);
	}

    if (force_size)
      height = total_height + VIEWPORT_HEIGHT_FUDGE;

      /* Get viewport to be right size.  Assumption is that knedit
	 reports its total height != height if paging is desired */
    XtVaSetValues(viewport,XtNheight, (XtArgVal)total_height, 
		  XtNwidth, (XtArgVal)width, NULL);

    
    XtVaSetValues(box,XtNfromVert,(XtArgVal)viewport,NULL);
    XtFree((char *)command); /* 10/19/92 */
    
    AG_Geometry(shell,node_info);
    XtRealizeWidget(shell);
    XtPopup(shell,XtGrabNone);

    return(shell);
}

  /* For historical continuity:  the singular remains of
     piggie.c. */
static void bye_bye(w,client_data,call_data)
     Widget w;
     XtPointer client_data;
     XtPointer call_data;
{
   Stat_Button_Hit(w);
   exit(0);
}

Widget Open_Control(parent,node_name,call_data)
     Widget parent;
     char *node_name;
     XtPointer call_data;
{
    Widget knedit;
    Widget box;
    Widget command;
    char filename[MaxString];
    char msg[MaxString];
    Widget layout;
    Widget form;
    Arg arglist[10];
    int n;
    NodeInfoPtr node_info;
    char *label;
    char *location;
    AGNodeTypes type;
    Widget shell;
    Widget buttonbox;
    Widget buttons[5];

    if (NodeService_Request_By_Name(node_name,&node_info,
				    &label,&type,&location) < 0)
      {
	sprintf(msg,"unable to locate %s in node service",node_name);
	XtWarning(msg);
	return(NULL);
      }

    n=0;
    XtSetArg(arglist[n],XtNinput,(XtArgVal)True);n++;
    XtSetArg(arglist[n],XtNallowShellResize, (XtArgVal)True);n++;
    /* title will get set in the resource file to "Answer Garden" */

    if ((char *)call_data != NULL)
      {
	XtSetArg(arglist[n],XtNgeometry,(XtArgVal)call_data);n++;
      }
    
    shell = XtCreatePopupShell((char *)node_name,applicationShellWidgetClass,
			       global_info.main_shell,
			       arglist,n);


    box =  XtCreateManagedWidget("control",formWidgetClass,
			  shell, NULL,0);

    Form_Filename(filename,location);
    if (NodeService_Open_File(node_info,filename))
      XtSetArg(arglist[0],XtNfilename,
	       (XtArgVal)NodeService_Get_Text(node_info));
    else
      XtSetArg(arglist[0],XtNfilename,(XtArgVal)MissingFileMessage);

    knedit = XtCreateManagedWidget("controlKnedit",
				   kneditWidgetClass,box,
			 	   arglist,1);
    buttonbox = XtVaCreateManagedWidget("controlButtonBox",
					formWidgetClass,box,
					XtNfromVert,(XtArgVal)knedit,
					NULL);

    command = XtVaCreateManagedWidget("button1",commandWidgetClass,
			  buttonbox, NULL);
    XtAddCallback(command,XtNcallback,bye_bye,NULL);

    command = XtVaCreateManagedWidget("button2",commandWidgetClass,
				      buttonbox,
				      XtNfromHoriz, (XtArgVal)command,
				      NULL);

    /* MSA 6/2/94 SAO patch */
    XtAddCallback(command,XtNcallback,Dynamic_Help_Callback,
		  (XtPointer)node_info);

    command = XtVaCreateManagedWidget("button3",grapherButtonWidgetClass,
			      buttonbox, 
			      XtNselectionString, 
				      (XtArgVal)global_info.root_grapher,
			      XtNeditMode, (XtArgVal) global_info.edit_mode,
				      XtNfromHoriz, (XtArgVal)command,
				      NULL);

    XtAddCallback(command,XtNcallback,Show_Root_Grapher,
		  (XtPointer)global_info.root_grapher);

    command = XtVaCreateManagedWidget("button4",grapherButtonWidgetClass,
			      buttonbox, 
			      XtNselectionString, 
				      (XtArgVal) global_info.root,
			      XtNeditMode, (XtArgVal) global_info.edit_mode,
				      XtNfromHoriz, (XtArgVal)command,
				      NULL);
    XtAddCallback(command,XtNcallback,Show_Root,(XtPointer)global_info.root);
    command = XtVaCreateManagedWidget("button5",commandWidgetClass,
				      buttonbox, 
				      XtNfromHoriz, (XtArgVal)command,
				      NULL);


#ifdef SAO_MOD
    AG_Geometry(shell,node_info);
#endif
    XtRealizeWidget(shell);
    XtPopup(shell,XtGrabNone);
    NodeService_Register_Open_Node(node_info,shell,NULL,NULL);
    return(shell);
}



	
	 



/****************************

  DYNAMIC CALLBACKS

*****************************/

/* This is the basic structure for the Dynamic Tables.  It maps
   a name to a function pointer. */
typedef struct _DynamicTable
{
  XrmQuark name_quark;
  XtCallbackProc callback; /*void (*callback)(); */
} DynamicTable;

static int dynamic_table_counter = -1; /* since routine inc's to start */
static int max_dynamic_table = 32;
static DynamicTable *dynamic_table;

/* The initialize routine just creates the initial DynamicTable of (now)
   32 entries. */
Boolean Dynamic_Initialize()
{
  if ((dynamic_table = 
       (DynamicTable *)XtMalloc(sizeof(DynamicTable) * max_dynamic_table)) 
      == NULL)
    return(False);
  return(True);
}

/* This routine allows node types (and AG's own initialization routines)
   to add dynamic callbacks.  It consists of a simple check for bad
   names, a check to see if the table is full (if it is, new space is
   allocated), hashing the name, and then entry of the tokenized
   name (quark) and callback pointer into the table. */
Boolean Dynamic_Add_Callback(string,callback)
     String string;
     XtCallbackProc callback;
{
  if (!dynamic_table)
    return(False);
  if (string == NULL || string[0] == EOS || callback == NULL || 
      AGstrlen(string) > MaxString)
    {
      Util_Debug("illegal string or callback ptr passed to DynamicAddCallback");
      return(False);
    }

  if (++dynamic_table_counter >= max_dynamic_table)
    {
      max_dynamic_table = max_dynamic_table + (int)(max_dynamic_table/2);
      if ((dynamic_table = 
	   (DynamicTable *)XtRealloc((char *)dynamic_table,
				     max_dynamic_table * sizeof(DynamicTable)))
	  == NULL)
	{
	  Util_Debug("unable to alloc in DynamicAddCallback");
	  return(False);
	}
    }
  
  dynamic_table[dynamic_table_counter].name_quark
    = XrmStringToQuark(string);
  dynamic_table[dynamic_table_counter].callback = callback;
  return(True);
}


/* This routine triggers a callback in the dynamic table based on the
   string provided in the parameter list. */
static Boolean Dynamic_Internal_Trigger(w,client_data,call_data,string)
     Widget w;
     XtPointer client_data;
     XtPointer call_data;
     char *string;
{
  int i;
  XrmQuark temp_quark;
  if (!dynamic_table)
    return(False);

  if (string == NULL || string[0] == EOS || AGstrlen(string) > MaxString)
    {
      Util_Debug("illegal string or callback ptr passed to DynamicCallback");
      return(False);
    }

  temp_quark = XrmStringToQuark(string);
  for (i=0;i<=dynamic_table_counter;i++)
    if (dynamic_table[i].name_quark == temp_quark)
      {
	(dynamic_table[i].callback)(w,client_data,call_data);
	return(True);
      }
  return(False);
}

/* AG_Dynamic_Callback_For_GrapherButton 
   is called as the result of an action proc 
   getting triggered somewhere deep inside AG.

   The critical thing to note is that the call_data is actually
   a struct.  Inside that struct is the param field from the action
   description in the translation table; this param field contains
   the dynamic callback name.

   Example:

          btn1Up:  Dynamic_Notify(My_Locking_Routine)

   would trigger the Dynamic_Notify action proc in a widget.  This
   action proc should call AG_Dynamic_Callback_For_GrapherButton, passing on
   the params parameter of the action proc.  params[0] will
   contain the string "My_Locking_Routine", which will get looked
   up in the DynamicTable.

   Example 2:
   
   One can specify a dynamic callback in a grapher or sbrowser widget.
   For the SBrowswer, it's:

          @button(some label here, @dynamic(callback_name,params))

   Here the grapherButton (through its parent's callback) triggers
   off the AG_Dynamic_Callback_For_GrapherButton.  The params fields here are
   moved down and the callback_name is moved into params[0] of
   the AGGrapherButtonCallbackStruct.

   Got it?  

**062592** only comments added */

void AG_Dynamic_Callback_For_GrapherButton(w,client_data,call_data)
     Widget w;
     XtPointer client_data;
     XtPointer call_data;
{
  int i;
  String string;
  XrmQuark temp_quark;

  AGGrapherButtonCallbackStruct *callback_struct = 
    (AGGrapherButtonCallbackStruct *)call_data;

  if (!dynamic_table)
    return;
  string = callback_struct->params[0];

  Dynamic_Internal_Trigger(w,client_data,call_data,string);
}

void General_Dynamic_Callback(w,client_data,call_data)
     Widget w;
     XtPointer client_data;
     XtPointer call_data;
{
  int i;
  String string;
  XrmQuark temp_quark;
  FuncObjReturnStruct new_return_struct;
  FuncObjReturnStruct *real_return_struct;
  int new_nparams;
  String *new_params;

  if (!dynamic_table)
    return;


  if (XtIsSubclass(w,funcObjClass))
    {
      real_return_struct = (FuncObjReturnStruct *)call_data;
      if (real_return_struct->nparams <= 0)
	return;
      string = real_return_struct->params[0];
      new_nparams = real_return_struct->nparams - 1;
      if ((new_params = (String *)XtMalloc(sizeof(String)*new_nparams))
	  == NULL)
	return;
      new_return_struct.nparams = new_nparams;
      for (i=1;i<real_return_struct->nparams;i++)
	new_params[i-1] = real_return_struct->params[i];
      new_return_struct.params = new_params;
      Dynamic_Internal_Trigger(global_info.main_shell,client_data,
			       (XtPointer)&new_return_struct,string);
      XtFree((char *)real_return_struct);

    }
  else
    {
      /* Don't know where it's from - why would anyone call this routine
	 as a callback instead of just calling the specified callback? 
	 We'll just hope for the best. */
    string = (char *)call_data;
    Dynamic_Internal_Trigger(w,client_data,call_data,string);
  }

}


static void Dynamic_Create_Node_Callback(w,client_data,call_data)
     Widget w;
     XtPointer client_data;
     XtPointer call_data;
{
  int i;
  String second_param;


  AGGrapherButtonCallbackStruct *callback_struct = 
    (AGGrapherButtonCallbackStruct *)call_data;

  if (!dynamic_table)
    return;

  second_param = callback_struct->params[1];
  
  if (second_param != NULL  && second_param[0] != EOS)
    AG_Create_Node(w,callback_struct->selection_string,
		   &callback_struct->params[2],
		   callback_struct->num_params-2);
  else
    Util_Debug("illegal string passed to Dynamic_Create_Node_Callback");
}



void Dynamic_Button_Callback(w,client_data,call_data,callback_name,
			     default_proc)
     Widget w;
     XtPointer client_data;
     XtPointer call_data;
     String callback_name;
     XtCallbackProc default_proc;
{
  Widget shell;
  char name_buffer[MaxString];
  char class_buffer[MaxString];
  char *resource_type;
  XrmValue value;
  XrmValue to_value;
  Boolean rc;

  NodeInfoPtr node_info = (NodeInfoPtr) client_data;
  if (node_info == NULL) /* 2/12/92 */
    {
      Util_Debug("missing node info in DynamicButtonCallback");
    }
  else
    {
      /* MSA 6/2/94 SAO patch */
      AGstrcpy(name_buffer,global_info.program_name);
      AGstrcat(name_buffer,".");
      AGstrcat(name_buffer,NodeService_Get_Node_Name(node_info));
      AGstrcat(name_buffer,".");
      AGstrcat(name_buffer,callback_name);
      AGstrcpy(class_buffer,global_info.program_name);
      AGstrcat(class_buffer,"."); /* MSA 6/2/94 added period to string */
      AGstrcat(class_buffer,NodeService_Get_String_From_Type(
					  NodeService_Get_Type(node_info)));
      AGstrcat(class_buffer,".");
      AGstrcat(class_buffer,callback_name);
      if ((rc = XrmGetResource(XtDatabase(XtDisplay(w)),name_buffer,
			       class_buffer,
			       &resource_type,&value)) == False)
	{ /* second round of lookup added 6/2/94 MSA as SAO patch */
	  AGstrcpy(name_buffer,global_info.class_name);
	  AGstrcat(name_buffer, ".");
	  AGstrcat(name_buffer,NodeService_Get_Node_Name(node_info));
	  AGstrcat(name_buffer,".");
	  AGstrcat(name_buffer,callback_name);
	  AGstrcpy(class_buffer,global_info.class_name);
	  AGstrcat(class_buffer, ".");
	  AGstrcat(class_buffer,NodeService_Get_String_From_Type(
				 NodeService_Get_Type(node_info)));
	  AGstrcat(class_buffer,".");
	  AGstrcat(class_buffer,callback_name);
	  /* MSA 6/2/94 put in second call to XrmGetResource if first fails.
	     Not in SAO patches */
	  rc = XrmGetResource(XtDatabase(XtDisplay(w)),name_buffer,
			      class_buffer,
			      &resource_type,&value);
	}
    }
  if (rc && dynamic_table)
    {
      if (!Dynamic_Internal_Trigger(w,client_data,call_data,value.addr))
	(default_proc)(w,client_data,call_data);
    }
  else
    (default_proc)(w,client_data,call_data);
}  

  /* 10/19/92 This is triggered only from Util_Open_ButtonBox_With_Bindings,
     and is used when a node type adds extra buttons to all nodes */
void Dynamic_New_Button_Callback(w,client_data,call_data)
     Widget w;
     XtPointer client_data;
     XtPointer call_data;
{
  char msg[MaxString];
  AGGeneralButtonStruct *button_struct = (AGGeneralButtonStruct *)client_data;
  XtPointer internal_client_data = (XtPointer)button_struct->client_data;

  if (button_struct->id > 0)
    sprintf(msg,"newButtonCallback%d",button_struct->id);
  else
    return;

  /* This goes out, looking for newButtonCallback1, newButtonCallback2, etc */
  Dynamic_Button_Callback(w,internal_client_data,call_data,msg,
			  General_Null_Callback);
}


void Dynamic_Close_Callback(w,client_data,call_data)
     Widget w;
     XtPointer client_data;
     XtPointer call_data;
{
  Dynamic_Button_Callback(w,client_data,call_data,XtNcloseCallback,
			  General_Close_Callback);
}
			  
void Dynamic_Help_Callback(w,client_data,call_data)
     Widget w;
     XtPointer client_data;
     XtPointer call_data;
{
  Dynamic_Button_Callback(w,client_data,call_data,XtNhelpCallback,
			  General_Help_Callback);
}
			  
void Dynamic_Question_Callback(w,client_data,call_data)
     Widget w;
     XtPointer client_data;
     XtPointer call_data;
{
  Dynamic_Button_Callback(w,client_data,call_data,XtNquestionCallback,
			  General_Question_Callback);
}
			  
void Dynamic_Back_Callback(w,client_data,call_data)
     Widget w;
     XtPointer client_data;
     XtPointer call_data;
{
  Dynamic_Button_Callback(w,client_data,call_data,XtNbackCallback,
			  General_Back_Callback);
}
			  
void Dynamic_Locked_Callback(w,client_data,call_data)
     Widget w;
     XtPointer client_data;
     XtPointer call_data;
{
  Dynamic_Button_Callback(w,client_data,call_data,XtNlockedCallback,
			  General_Locked_Callback);
}
			  




  /* General callback for creating a new node (ie, following a link).
     Get the node name (part of call_data) and pass it on to the 
     proper routine. */
void AG_Create_Node_Callback_For_GrapherButton(w,client_data,call_data)
     Widget w;
     XtPointer client_data;
     XtPointer call_data;
{
  /*** Right now this struct is the same for Knedits and Graphers.
    This routine ought to check the type of w and cast appropriately ****/
  AGGrapherButtonCallbackStruct *callback_struct = 
    (AGGrapherButtonCallbackStruct *)call_data;

  /* selection string is the node name */
  AG_Create_Node(w,callback_struct->selection_string,callback_struct->params,
		 callback_struct->num_params);
}


static void Create_Null_Node(w)
     Widget w;
{
    char *label;
    AGNodeTypes type;
    char *location;
    NodeInfoPtr node_info;
    char buffer[MaxString];
    char *node_name = AGMissingNode;

    /* request node service here to avoid a loop */
    if ((NodeService_Request_By_Name(node_name,&node_info,
				     &label,&type,&location) < 0))
	{
	  sprintf(buffer, "missing name %s in node service.  Continuing...",
		  node_name);
	  XtWarning(buffer);
	  User_History_List_Add_Member(node_name,"NotInNodeService");
	  Stat_Node_Create(w,NULL);
	  return;
	}
    else
      AG_Create_Node(w,node_name,NULL,0);
}

Widget AG_Create_Node(w,node_name,misc_info,num_info)
     Widget w;
     char *node_name;
     String *misc_info;
     int num_info;
{
    char *label;
    AGNodeTypes type;
    char *location;
    char buffer[MaxString];
    Widget shell;
    NodeInfoPtr node_info;
    char *geometry;
    char geometry_string[MaxString];
    Dimension height;
    Dimension width;
    Position x;
    Position y;
    Boolean set_geometry;
    Widget old_shell;

    if ((NodeService_Request_By_Name(node_name,&node_info,
				     &label,&type,&location) < 0))
	{
	  sprintf(buffer, "missing name %s in node service.  Continuing...",
		  node_name);
	  XtWarning(buffer);
	  if (strcmplo(node_name,AGMissingNode))
	    Create_Null_Node(w);
	  else
	    XtWarning("Missing NodeService info for \"MissingNode\"");
	  User_History_List_Add_Member(node_name,"NotInNodeService");
	  Stat_Node_Create(w,NULL);
	  return(NULL);
	}

    if (!location[0])
	{
	    User_History_List_Add_Member(node_name,"MissingLocation");
	    Stat_Node_Create(w,node_info);
	    if (strcmplo(node_name,AGMissingNode))
	      Create_Null_Node(w);
	    else
	      XtWarning("Missing location for \"MissingNode\"");
	    return(NULL);
	}

    if (!strcmplo(node_name,"noaction")) /* may not be sbrowser type */
	{
	  User_History_List_Add_Member(node_name,"NoAction");
	  Stat_Node_Create(w,node_info);
/*	  XtWarning("node service returns no action on this node"); */
	  return(NULL);
	}

    
    if (type < 0)
      {
	sprintf(buffer,"node type for \"%s\" is unknown.   Continuing...", 
		node_name);
	XtWarning(buffer);
	XtWarning("\tPlease contact your site administrator.");
	User_History_List_Add_Member(node_name,"Unknown");
	Stat_Node_Create(w,node_info);
	if (strcmplo(node_name,AGMissingNode))
	  Create_Null_Node(w);
	else
	  XtWarning("Bad node type for \"MissingNode\"");
	return(NULL);
      }

    /* This should change to a workproc going through the list
       of open files checking whether any are dirty, and automatically
       setvaluing the nodes correctly. */


    /* Interesting problem:  the shell widgets do not necessarily
       return a valid geometry resource (including returning NULL
       in the default case).  Therefore I am creating a geometry
       string myself. */

    set_geometry = False;
    if ((old_shell = NodeService_Get_Shell(node_info)) != NULL)
      {
	if (!NodeService_Has_Node_Changed(node_info)) 
	  {
	    AG_Raise_Shell(old_shell);
	    User_History_List_Add_Member(node_name,
				      NodeService_Get_String_From_Type(type));
	    Stat_Node_Raise(w,node_info);
	    return(NULL);
	  }
	else
	  {
	    XtVaGetValues(old_shell,XtNheight,&height,
			  XtNwidth,&width, XtNx,&x, XtNy,&y, NULL);
	    sprintf(geometry_string,"%dx%d+%d+%d",width,height,x,y);
	    set_geometry = True;
	  }
      }
	
    
    if (!global_node_types[type]->virtual_node)
      if (!Check_Node_Physical_Existance(location))
	{
	  /* 5/14/93 msa additional fix to print warning message on
	     missing node, put up null node on any missing condition
	     except the MissingNode */
	  sprintf(buffer,"Missing file for \"%s\".  Continuing...",
		  node_name);  /* 5/14/93 msa put file name in msg */
	  XtWarning(buffer);
	  XtWarning("\tPlease contact your site administrator.");
	  if (strcmplo(node_name,AGMissingNode)) /* not Missing Node */
	    {
	      /* Avoid an infinite loop on the MissingNode if it's
		 missing. */
	      Create_Null_Node(w);  
	    }
	  User_History_List_Add_Member(node_name,"NoPhysicalFile");
	  Stat_Node_Create(w,node_info);
	  return(NULL);
	}

    User_History_List_Add_Member(node_name,
				 NodeService_Get_String_From_Type(type));
    if (!global_node_types[type]->class_created 
	&& global_node_types[type]->class_create_function != NULL)
      {
	(global_node_types[type]->class_create_function)(w,node_info,NULL);
	global_node_types[type]->class_created = True;
      }
    if (global_node_types[type]->create_function == NULL)
      {
	sprintf(buffer,"no create function for node type %d\n",
		type);
	Util_Debug(buffer);
	return(NULL);
      }
    if (!set_geometry)
      {
	shell = (global_node_types[type]->create_function)(w,
					     node_info,
					     NULL,
       					     misc_info,
					     num_info);
      }
    else     
      {
	General_Close_Callback((Widget)old_shell,(XtPointer)node_info,NULL);
	shell = (global_node_types[type]->create_function)(w,
					     node_info,
					     geometry_string,
       					     misc_info,
					     num_info);

      }

    NodeService_Register_Open_Node(node_info,shell,NULL,NULL);
    Stat_Node_Create(w,node_info);

    return(shell);
}

static void Grapher_Destroy_Callback(w,client_data,call_data)
     Widget w;
     XtPointer client_data;
     XtPointer call_data;
{
  /* not needed for now */
}


static void Show_Root_Grapher(w,client_data,call_data)
     Widget w;
     XtPointer client_data;
     XtPointer call_data;
{
  AGGrapherButtonCallbackStruct *callback_struct =
    (  AGGrapherButtonCallbackStruct *) call_data;
    
  char *node_name = global_info.root_grapher;

  /*** CHANGE THIS ***/
  if (callback_struct->notify_type == AGGrapherButtonEditNotify) 
    /* must be edit */
    {
      Edit_Node_Callback(w,client_data,call_data);
    }
  else
    {
      AG_Create_Node(w,node_name,callback_struct->params,
		     callback_struct->num_params);
    }
  
}



Widget Show_Grapher(w,node_info,geometry,misc_info,num_info)
     Widget w;
     NodeInfoPtr node_info;
     String geometry;
     String *misc_info;
     int num_info;
{
    Widget grapher;
    Widget shell;
    Widget viewport;
    Widget form;
    char complete_filename[MaxString];
    char msg[MaxString];
    Arg arglist[10];
    int n;
    Dimension height;
    Dimension width;
    Boolean flag;
    char *filename;
    Widget button_box;
    Widget command[5];
    int num_buttons;

    filename = NodeService_Get_Location(node_info);

    if (filename == NULL)
      {
	 XtWarning("null name passed to discussion node.  Continuing...");
	 return(NULL);
      }

    Form_Filename(complete_filename,filename);
    if (!NodeService_Open_File(node_info,complete_filename))
      {
	sprintf(msg,"Error in opening node at location %s.",filename);
	XtWarning(msg);
	XtWarning("Continuing...");
	NodeService_Close_File(node_info);
	return(NULL);
      }
    n = 0;
    XtSetArg(arglist[n],XtNinput,(XtArgVal)True);n++;
    XtSetArg(arglist[n],XtNallowShellResize,(XtArgVal)True);n++;
    XtSetArg(arglist[n],XtNtitle,(XtArgVal)NodeService_Get_Label(node_info));
      n++;

    if (geometry != NULL)
      {
	XtSetArg(arglist[n],XtNgeometry,(XtArgVal)geometry);n++;
      }
    shell = XtCreatePopupShell(NodeService_Get_Node_Name(node_info),
			       topLevelShellWidgetClass,
			       global_info.main_shell,
			       arglist,n);

    viewport = XtVaCreateManagedWidget("grapherViewport",viewportWidgetClass,
				       shell,
				       XtNallowHoriz,(XtArgVal)True,
				       XtNallowVert,(XtArgVal)True,
				       NULL);

    form = XtVaCreateManagedWidget("grapherForm",formWidgetClass,
				       viewport,
				       NULL);

    /* Create the standard node buttons */
    Util_Open_ButtonBox_With_Bindings(form,&button_box,False,NULL,&num_buttons,
				      (XtPointer)node_info);
    

    grapher = XtVaCreateManagedWidget("grapher",grapherWidgetClass,
			      form,
			      XtNtext,
                                     (XtArgVal)NodeService_Get_Text(node_info),
			      XtNborderWidth, (XtArgVal)0,
			      XtNfromVert, (XtArgVal)button_box,
			      XtNvertDistance, (XtArgVal)0,
			      XtNeditMode, (XtArgVal)global_info.edit_mode,
#ifndef NO_FUNCOBJ
				     XtNfuncObj,
				       (XtArgVal)global_info.global_func_obj,
#endif
			      NULL);

    NodeService_Free_Buffer(node_info);

    XtAddCallback(grapher,
		  XtNcallback,
		  AG_Create_Node_Callback_For_GrapherButton,
		  (XtPointer)grapher);
    XtAddCallback(grapher,XtNeditCallback,Edit_Node_Callback,
		  (XtPointer)grapher);
    XtAddCallback(grapher,XtNdynamicCallback,
		  AG_Dynamic_Callback_For_GrapherButton,
		  (XtPointer)grapher);
    XtAddCallback(shell,XtNdestroyCallback,Grapher_Destroy_Callback,
		  (XtPointer)node_info);

    /* Unfortunately, there is no way to do this by subclassing Viewport
       to do this nicely.  Believe me, I tried. -- MSA */

    XtVaGetValues(grapher,XtNheight,&height,XtNwidth,&width,NULL);
    flag = False;
    if (height > global_info.grapher_max_height)
      {
	flag = True;
	height = global_info.grapher_max_height;
      }
    if (width > global_info.grapher_max_width)
      {
	width = global_info.grapher_max_width;
	flag = True;
      }
    if (flag)
      XtVaSetValues(viewport,XtNheight,(XtArgVal)height,
		    XtNwidth,(XtArgVal)width,
		    NULL);

    AG_Geometry(shell,node_info);

    XtRealizeWidget(shell);
    XtPopup(shell,XtGrabNone);
    return(shell);
}

static void Show_Root(w,client_data,call_data)
     Widget w;
     XtPointer client_data;
     XtPointer call_data;
{
  NodeInfoPtr node_info;
  char *label;
  char *location;
  AGNodeTypes type;
  char buffer[MaxString];
  Widget shell;
  AGGrapherButtonCallbackStruct *callback_struct =
    (  AGGrapherButtonCallbackStruct *) call_data;

  char *node_name = global_info.root;

  /*** CHANGE THIS ***/
  if (callback_struct->notify_type == AGGrapherButtonEditNotify)
    /* must be edit */
    {
      Edit_Node_Callback(w,client_data,call_data);
    }
  else
    {
#ifdef OLDAPI
      AG_Create_Node(w,node_name);
#else
      AG_Create_Node(w,node_name,callback_struct->params,
		     callback_struct->num_params);
#endif
      
    }
}

static void Internal_Close_Callback(w,client_data,call_data,delete_flag)
     Widget w;
     XtPointer client_data;
     XtPointer call_data;
     Boolean delete_flag;
{
  Widget shell;
  int stack_num;
  NodeInfoPtr node_info = (NodeInfoPtr) client_data;

  if ((shell = NodeService_Get_Shell(node_info)) != NULL)
    {
      /* why wouldn't you want to delete it?  I don't remember
	 any more.  MSA 10/23/92 */
      if (delete_flag)
	XtDestroyWidget(shell);
    }
  else
    {
      /* 11/3/91 bug fix - missing return so routine
	 kept executing */
      Util_Debug("General Close Callback:  missing shell");
      return;
    }
      
  if (global_info.n_stacks > 0)
    {
      stack_num = AG_Geometry_Get_Stack(shell,node_info);
      if (stack_num > 0)
	AG_Stack_Delete_Shell(global_info.stacks[stack_num-1],shell);
    }
  NodeService_Register_Closed_Node(node_info,shell,NULL,NULL);
}

/* AG_Close_Node removed 092092 */

/**092092**/
void General_Close_Callback_With_Shell(w,client_data,call_data)
     Widget w;
     XtPointer client_data;
     XtPointer call_data;
{
  Widget shell = (Widget)call_data;
  NodeInfoPtr node_info;

  if ((node_info = NodeService_Find_Open_Node(shell)) == NULL)
    return;

  /* This is a little redundant, but it avoids a second routine
     that knows the internals */
  Stat_Button_Hit(shell);
  Internal_Close_Callback(shell,(XtPointer)node_info,call_data,True);
}


void General_Close_Callback(w,client_data,call_data)
     Widget w;
     XtPointer client_data;
     XtPointer call_data;
{
  Widget shell;
  NodeInfoPtr node_info = (NodeInfoPtr) client_data;

  Stat_Button_Hit(w);
  Internal_Close_Callback(w,client_data,call_data,True);
}

void General_Null_Callback(w,client_data,call_data)
     Widget w;
     XtPointer client_data;
     XtPointer call_data;
{
  Util_Debug("in General_Null_Callback");
}

void General_Locked_Callback(w,client_data,call_data)
     Widget w;
     XtPointer client_data;
     XtPointer call_data;
{
  Widget shell;
  int stack;
  Boolean locked;

  NodeInfoPtr node_info = (NodeInfoPtr) client_data;
  
  if ((shell = NodeService_Get_Shell(node_info)) == NULL)
    return;
  
  Stat_Button_Hit(w);
  stack = AG_Geometry_Get_Stack(w,node_info);
  if (stack > 0) /* changed from >= 10/20/92 */
    {
      locked = AG_Stack_Get_Locked(global_info.stacks[stack-1],shell);
      locked = !locked;
      AG_Stack_Set_Locked(global_info.stacks[stack-1],shell,locked);
      /* 6/2/94 change labels on button when it changes state */
      XtVaSetValues(w, 
		    XtNlabel,(XtArgVal) (locked ? global_info.locked_text :
					 global_info.unlocked_text), 
		    NULL);
    }
}



void General_Back_Callback(w,client_data,call_data)
     Widget w;
     XtPointer client_data;
     XtPointer call_data;
{
  Widget shell;
  int stack;
  char *node_name;
  NodeInfoPtr prev_node_info;
  NodeInfoPtr node_info = (NodeInfoPtr) client_data;

  /* NodeInfo when it comes in is the current node */
  if ((shell = NodeService_Get_Shell(node_info)) == NULL)
    return;
  
  Stat_Button_Hit(w);
  stack = AG_Geometry_Get_Stack(w,node_info);
  if (stack >= 0)
    {
      prev_node_info = (NodeInfoPtr)
	AG_Stack_Go_Back(global_info.stacks[stack-1]);
      if ((node_name = NodeService_Get_Node_Name(prev_node_info)) == NULL)
	  return; /* error condition or no other node on stack */
      AG_Create_Node(w,node_name,NULL,0);
    }
}




void General_Help_Callback(w,client_data,call_data)
     Widget w;
     XtPointer client_data;
     XtPointer call_data;
{

  char *label;
  int type;
  char *location;
  char buffer[MaxString];
  char msg[MaxString];
  NodeInfoPtr help_node_info;
  NodeInfoPtr node_info = (NodeInfoPtr)client_data;

  AGstrcpy(buffer,NodeService_Get_Node_Name(node_info));
  AGstrcat(buffer,".hp");

  Stat_Button_Hit(w);

  if ((NodeService_Request_By_Name(buffer,&help_node_info,
				   &label,&type,&location) < 0))
    {
      Create_Null_Node(w);
      sprintf(msg,"missing help node for '%s'",
		NodeService_Get_Label(node_info));
      XtWarning(msg);
      Util_Debug("Missing help node in GeneralHelpCallback");
      return;
    }
  
#ifdef OLDAPI
    AG_Create_Node(w,buffer);
#else
  AG_Create_Node(w,buffer,NULL,0);
#endif

}

void AG_WMProtocols_Action(w, event, params, num_params)
     Widget w;
     XEvent *event;
     String *params;
     Cardinal *num_params;
{
  /* Following XmhWMProtocols action proc */
  Boolean flag;
  NodeInfo *node_info;

  flag = False;
  if (event && event->type == ClientMessage)
    {
      if (event->xclient.message_type == global_info.wm_protocols)
	{
	  if (event->xclient.data.l[0] == global_info.wm_delete_window &&
	      ((*num_params == 0 || 
		Util_Search_Array(params,"WM_DELETE_WINDOW",
				  *num_params) > 0)))
	    flag = True;
	}
    }
  else
    if (Util_Search_Array(params,"WM_DELETE_WINDOW", *num_params) > 0)
      flag = True;
  
  if (!flag)
    return;

  while (w && !XtIsShell(w))
    w = XtParent(w);

  if ((node_info = NodeService_Find_Open_Node(w)) == NULL)
    return;

  if (!(AGstrcmp(NodeService_Get_Node_Name(node_info),
		 global_info.startup_node)))
    bye_bye(w,NULL,NULL);
  else
    General_Close_Callback(w,(XtPointer)node_info,(XtPointer)NULL);
}

static void Iconification_Watcher(w,client_data,event,continue_on)
     Widget w;
     XtPointer client_data;
     XEvent *event;
     Boolean *continue_on;
{
  Window window;
  caddr_t data;
  char *name;
  NodeInfo *node_info;
  char *label;
  char *location;
  AGNodeTypes type;

  switch (event->type)
    {
    case MapNotify:
      data = NULL;
      if (XFindContext(XtDisplay(w),XtWindow(w),global_info.icon_context,
		       &data) > 0  || (int)data != MapNotify)
	XSaveContext(XtDisplay(w),XtWindow(w),global_info.icon_context,
		     (caddr_t)(MapNotify));
      break;
    case UnmapNotify:
      data = NULL;
      if (XFindContext(XtDisplay(w),XtWindow(w),global_info.icon_context,
		       &data) > 0  || (int)data != UnmapNotify)
	XSaveContext(XtDisplay(w),XtWindow(w),global_info.icon_context,
		     (caddr_t)(UnmapNotify));
      break;
    case DestroyNotify:
      XDeleteContext(XtDisplay(w),XtWindow(w),global_info.icon_context);
      break;
    }
  *continue_on = True;
}
      
static void Iconification_Destroy_Callback(w,client_data,call_data)
     Widget w;
     XtPointer client_data;
     XtPointer call_data;
{
  /* w is the shell */
  XDeleteContext(XtDisplay(w),XtWindow(w),global_info.icon_context);
}  

AG_Register_Open_Node(shell)
     Widget shell;
{
  XtAddEventHandler(shell,StructureNotifyMask,False,Iconification_Watcher,
		    NULL);
  XtAddCallback(shell,XtNdestroyCallback,Iconification_Destroy_Callback,
		(XtPointer)shell);
  XtAugmentTranslations(shell, global_info.wm_protocols_translations);
  

}

void AG_Raise_Shell(widget)
     Widget widget;
{
  Window window;
  Window w2;
  Screen *the_screen;
  Display *the_display;
  Window parent;
  Window root;
  Window *children;
  unsigned int nchildren;
  Widget shell;
  char *data;

  if (widget == NULL)  /* 2/12/92 */
    {
      Util_Debug("missing widget id in AGRaiseShell");
      return;
    }

  the_screen = XtScreen(widget);
  window = XtWindow(widget);
  the_display = XtDisplay(widget);
  /* I should get a shell in as the widget here, so there's not
     much point in going through the toolkit to get the parents.
     See if the window manager has put anything between the shell
     and the root window */

  if (!XQueryTree(the_display,window,&root,&parent,&children,&nchildren))
    return;  /* something is wrong */
  while ((parent != RootWindowOfScreen(the_screen)))
    {
      window = parent;
      XFree((char *)children);
      if (!XQueryTree(the_display,window,&root,&parent,&children,&nchildren))
	{
	  Util_Debug("unclear state in AGRaiseShell");
	  return; /* something is wrong */
	}
    }
  XFree((char *)children);

  /* just to make it more interesting, we have gotten mapnotify and
     unmap notify on the shell's window instead of the toplevel window */
  if (XFindContext(the_display,(XID)XtWindow(widget),
		   global_info.icon_context,&data) == 0)
    {
      if ((int)data == UnmapNotify) /* it's an icon */ 
	{
	  XtVaSetValues(widget,XtNiconic,(XtArgVal)False,NULL);
	  XtMapWidget(widget);
	}
      else
	{
	  XRaiseWindow(the_display,XtWindow(widget)); 
	  /* for olwm which doesn't like
	     the parent to be raised; it wants the original window id
	     and then it translates it into its parent.  But since
	     I can't tell it's olwm, we do this.  Talk to Sun.  MSA 120392 */
	  XRaiseWindow(the_display,window);
        }
    }
  else /* we're lost, so we might as well try */
    {
      XRaiseWindow(the_display,XtWindow(widget)); 
      XRaiseWindow(the_display,window);
      Util_Debug("missing xcontext in AGRaiseShell");
    }

}

void History_Close_Callback(w,client_data,call_data)
     Widget w;
     XtPointer client_data;
     XtPointer call_data;
{
  Widget shell = (Widget) client_data;
  global_info.history_list = NULL;
  XtDestroyWidget(shell);
}


void Show_History_Callback(w,client_data,call_data)
     Widget w;
     XtPointer client_data;
     XtPointer call_data;
{
  char **name_array = (char **)client_data;
  XawListReturnStruct *lr_struct = (XawListReturnStruct *)call_data;
  char *node_name;

  node_name = name_array[lr_struct->list_index];

  if (node_name != NULL && node_name[0] != EOS 
      && AGstrlen(node_name) <= MaxString)
#ifdef OLD_API
    AG_Create_Node(w,node_name);
#else
  AG_Create_Node(w,node_name,NULL,0);
#endif
}
  

#define MaxHistoryList 100
#define FUDGE_LIST 10
#define FUDGE_SCROLL 20

Show_More_History_List()
{
  int num;
  static char *name_array[MaxHistoryList+1];
  static char *label_array[MaxHistoryList+1];

  User_History_Make_List(label_array,name_array,MaxHistoryList,&num);

  if (num == 0)
    {
      XtWarning("No history list yet.");
      return;
    }

  label_array[num++] = NULL;
  name_array[num++] = NULL;


  XtRemoveAllCallbacks(global_info.history_list,XtNcallback);
  XawListChange(global_info.history_list,label_array,0,0,True);
  XtAddCallback(global_info.history_list,XtNcallback,Show_History_Callback,
		(XtPointer)name_array);
}

Show_History_List()
{
  Widget shell;
  Widget list;
  static char *name_array[MaxHistoryList+1];
  static char *label_array[MaxHistoryList+1];
  int num;
  Widget form;
  Widget buttons[5];
  Widget buttonbox;
  Widget parent;
  Widget sibling;
  Widget view;
  Widget inner_form;
  Dimension width, width_list;

  User_History_Make_List(label_array,name_array,MaxHistoryList,&num);

  if (num == 0)
    {
      XtWarning("No history list yet.");
      return;
    }

  label_array[num++] = NULL;
  name_array[num++] = NULL;

    
  shell = XtVaCreatePopupShell("HistoryList",
			       topLevelShellWidgetClass,
			       global_info.main_shell,
			       XtNallowShellResize, (XtArgVal)True,
			       XtNinput, (XtArgVal)True, 
			       XtNtitle, (XtArgVal)"History List",
			       NULL);
  
  form = XtVaCreateManagedWidget("historyListForm",formWidgetClass,
				 shell,
				 NULL);
  view = XtVaCreateManagedWidget("historyListViewport",viewportWidgetClass,
				 form,
				 XtNforceBars, (XtArgVal)True,
				 XtNallowVert, (XtArgVal) True,
				 NULL);
  inner_form = XtVaCreateManagedWidget("historyListInnerForm",
				       formWidgetClass,view,NULL);
  global_info.history_list = 
    list = XtVaCreateManagedWidget("historyListList",
				   listWidgetClass,
				   inner_form,
				   XtNdefaultColumns, (XtArgVal)1,
				   XtNlist, (XtArgVal)label_array,
				   NULL);
  
  Util_Open_Minimal_ButtonBox(form,&buttonbox,buttons);
  XtVaSetValues(buttonbox,XtNfromVert,(XtArgVal)view,NULL);

  XtVaGetValues(buttonbox,XtNwidth,(XtArgVal)&width,NULL);
  XtVaGetValues(list,XtNwidth,(XtArgVal)&width_list,NULL);
  
  width = (width_list < width + FUDGE_LIST) ? width+FUDGE_LIST : width_list;

  width = width_list + FUDGE_SCROLL;

  XtVaSetValues(view,XtNwidth,(XtArgVal)width,NULL);
  XtAddCallback(list,XtNcallback,Show_History_Callback,
		(XtPointer)name_array);
  XtAddCallback(buttons[0],XtNcallback,History_Close_Callback,(XtPointer)shell);

  XtRealizeWidget(shell);
  XtPopup(shell,XtGrabNone);
  
}

#undef FUDGE_LIST
#undef FUDGE_SCROLL;

void Other_Menu_Callback(w,client_data,call_data)
     Widget w;
     XtPointer client_data;
     XtPointer call_data;
{
  int menu_button = (int)client_data;
  Dimension width, hieght;
  Position x,y;
  char geometry_string[MaxString];
  int i;
  NodeInfoPtr node_info;
  AGNodeTypes junk_type;
  char *junk1;
  char *junk2;
  Widget shell;

  switch (menu_button)
    {
      /* Cannot pass on w since w is actually an object.  For creating
	 additional shells, we need a widget (4/3/91). */
    case 0:
      if (global_info.history_list == NULL)
	Show_History_List();
      else
	Show_More_History_List();
      break;
    case 1:
      break;
    case 2:
      /* give it the control node or whatever the startup node is */
      /**092092**/
      NodeService_Request_By_Name(global_info.startup_node,
				  &node_info,&junk1,&junk_type,&junk2);
      if ((shell = NodeService_Get_Shell(node_info)) == NULL)
	shell = global_info.main_shell;

      Dynamic_Question_Callback(shell,(XtPointer)node_info,NULL);
      break;


    case 3:   /** 062592 **/
      for (i=0;i<global_info.n_stacks;i++)
	AG_Stack_Tidy(global_info.stacks[i]);
      break;
      
    default:
      Util_Debug("Unknown value in other callback.  Continuing....");
    }
}

#define NumOtherFunctions 4

void Other_Initialize()
{
  char buffer[MaxString];
  int i;
  Widget menu_button[NumOtherFunctions];
  Widget menu_shell;

    /* Xaw requires this */
  XawSimpleMenuAddGlobalActions(XtWidgetToApplicationContext(
					    global_info.main_shell));

  menu_shell =
    XtVaCreatePopupShell("otherMenuShell",simpleMenuWidgetClass,
			 global_info.main_shell,
			 XtNlabel, (XtArgVal) "Other Functions",
			 NULL);
  XtVaCreateManagedWidget("otherMenuLine",smeLineObjectClass,menu_shell,
			  NULL);
			  
  for (i=0;i<NumOtherFunctions;i++)
    {
      sprintf(buffer,"otherMenuButton%1d",i+1);
      menu_button[i] = XtVaCreateManagedWidget(buffer,smeBSBObjectClass,
					menu_shell,NULL);
      XtAddCallback(menu_button[i],XtNcallback,Other_Menu_Callback,
		    (XtPointer)i);
    }

  XtVaSetValues(menu_shell,
		XtNpopupOnEntry, (XtArgVal) menu_button[0],
		NULL);


}

static XtActionsRec actionsList[] = 
{   
  {"AGWMProtocols", AG_WMProtocols_Action},
};



AG_Initialize()
{
  int i;
  char name_buffer[MaxString];
  char class_buffer[MaxString];

  /* Initialize all the flags indicating whether the class initialize
     functions have already been triggered off to False */
  Other_Initialize();

  for (i=0;i<global_info.n_global_node_types;i++)
    global_node_types[i]->class_created = False;

  /* One time node type initializations; used primarily to add 
     dynamic callbacks */

  for (i=0;i<global_info.n_global_node_types;i++)
    if (global_node_types[i]->initialize_function != NULL)
      (global_node_types[i]->initialize_function)();

      
  Dynamic_Add_Callback("CreateNode",Dynamic_Create_Node_Callback);
  Dynamic_Add_Callback("GeneralCloseCallback",General_Close_Callback);
  Dynamic_Add_Callback("GeneralQuitCallback",bye_bye);
  Dynamic_Add_Callback("GeneralHelpCallback",General_Help_Callback);
  /* added 10/19/92 */
  Dynamic_Add_Callback("GeneralLockedCallback",General_Locked_Callback);
  Dynamic_Add_Callback("GeneralBackCallback",General_Back_Callback);

  XtAddCallback(global_info.global_func_obj,XtNcrushCallback,
		General_Crush_Callback,NULL);

  XtAddCallback(global_info.global_func_obj,XtNdynamicCallback,
		General_Dynamic_Callback,NULL);

  XtAppAddActions(XtWidgetToApplicationContext(global_info.main_shell),
		  actionsList,XtNumber(actionsList));

}




main(argc,argv)
     unsigned int argc;
     char *argv[];
{
    Widget shell;
    Widget layout;
    Widget first_shell;
    char *progname, *rindex(); /* MSA 6/2/94 SAO patch */


    /* Set up the shell widget.  */
    /* MSA 6/2/94 SAO patch */
    if( (progname=rindex(argv[0], '/')) != NULL )
      progname++;
    else
      progname = argv[0];
    shell = 
      XtInitialize(argv[0],AGSClassName,NULL,0,&argc,argv);

    /* Need to do this early since it's got all the global info,
       including the number of NodeTypes */
    /* MSA 6/2/94 SAO patch */
    Globals_Initialize(shell,progname);

    Dynamic_Initialize();

      /* Stat must be after Globals since it needs the name of
	 the statistics file from the appdefaults resource. */
    Stat_Start_User(shell);

    if (CommService_Initialize(shell) < 0)
      {
	XtWarning("Unable to initialize Answer Garden's communications");
	XtWarning("\tservice.  You will not be able to ask a question.");
	XtWarning("\tContinuing....");
      }

#ifdef NO_EDIT
      /* If no editing is allowed, hard-wire edit_mode to be False,
	 disabling it everywhere.  AnswerGarden.c explicitly passes
	 XtNeditMode in the arglist to set the resource
	 when creating SBrowsers and Graphers.  SBrowsers and Graphers
	 explicitly pass the resource to GrapherButtons. */
    global_info.edit_mode = False;
#endif

    if (NodeService_Initialize() < 0)
      {
	XtWarning("Empty or missing NodeService file (AGNodeFile)");
	XtWarning("You are unlikely to get good results.  Continuing...");
      }

    AG_Initialize();

/*    
    FileService_Initialize(shell);
*/

    Edit_Initialize(shell); /* must before open_control for correct
			       setup of grapher button actions for
			       edit menu */

    /* Globals.c sets this to "Control" if the startup_node isn't
       set in some resource file */ /**092092**/
    if (!AGstrcmp(global_info.startup_node,"Control"))
	first_shell = Open_Control(shell,"Control",NULL);
    else
	first_shell = AG_Create_Node(shell,global_info.startup_node,NULL,0);
    if (first_shell == NULL)
      return;

    XtMainLoop();
}




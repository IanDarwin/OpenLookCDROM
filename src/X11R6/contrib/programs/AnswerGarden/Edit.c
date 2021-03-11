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

  The ANSWER GARDEN:  EDIT.C

     The major editing portions of Answer Garden.


  Mark Ackerman
  Information and Computer Science
  University of California, Irvine
  
  formerly -
  MIT/Center for Coordination Science
  
  ackerman@ics.uci.edu

  To do:
    make sure all widgets freed up (4/2/91)

***********************************************************/
#include <stdio.h>
#include <X11/StringDefs.h>
#include <X11/Intrinsic.h>
#include "AG.h" 
#include "Knedit.h"
#include "Grapher.h"
#include <X11/Shell.h>
#include <X11/Xaw/Command.h>
#include <X11/Xaw/AsciiText.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/Box.h>
#include <X11/Xaw/Toggle.h>
#include <X11/Xaw/Label.h>
#include <X11/Xaw/SimpleMenu.h>
#include <X11/Xaw/SmeBSB.h>
#include <X11/Xaw/Dialog.h>

#include "Edit.h"

XtPointer global_node_name; /* can't figure out any other way to do this
			       than to make it a global */
Widget global_node_parent;

Widget menu_shell;
Widget menu_toggle_auto_save;
Boolean edit_auto_save;



void Edit_File();
static void Add_Item();
static void Delete_Node();
void Edit_Node_Info();
void Add_Node();
void Edit_Menu_Callback();
static void Toggle_Auto_Save();
extern void Edit_NIP_Create();

/******************************

  MAJOR EDIT CALLBACKS AND
  PUBLIC ROUTINES

******************************/
#ifdef OLD
void Edit_Node_Callback(w,client_data,call_data)
     Widget w;
     caddr_t client_data;
     caddr_t call_data;
{
    Edit_Node(w,(char *)call_data);
}
#endif


#define FUDGE 12
#define NumEditStates 7
#define MenuEditButton 0
#define MenuToggleButton 6

  /* Initialize the Editor:  Setup the edit menu. */
void Edit_Initialize(w)
     Widget w;
{
  char buffer[MaxString];
  int i;
  Widget menu_button[NumEditStates];

    /* Xaw requires this */
#ifndef NO_EDIT
  XawSimpleMenuAddGlobalActions(XtWidgetToApplicationContext(w));

  menu_shell =
    XtVaCreatePopupShell("editMenuShell",simpleMenuWidgetClass,w,
			 XtNlabel, (XtArgVal) "Edit Menu",
			 NULL);
  for (i=0;i<NumEditStates;i++)
    {
      sprintf(buffer,"editMenuButton%1d",i+1);
      menu_button[i] = XtVaCreateManagedWidget(buffer,smeBSBObjectClass,
					menu_shell,NULL);
      XtAddCallback(menu_button[i],XtNcallback,Edit_Menu_Callback,
		    (XtPointer)i);
    }

  menu_toggle_auto_save = menu_button[MenuToggleButton];
  
  XtVaSetValues(menu_shell,
		XtNpopupOnEntry, (XtArgVal) menu_button[MenuEditButton],
		NULL);

  edit_auto_save = False;
#endif

}

 /* Callback triggered whenever user has pressed the EditButton on
    any GrapherButton in any grapher or sbrowser.  Pop ups the edit
    menu. */
 /* call_data is coming back with AGGrapherButtonCallbackStruct */
void Edit_Node_Callback(w,client_data,call_data)
     Widget w;
     caddr_t client_data;
     caddr_t call_data;
{
  /*** Right now this struct is the same for Knedits and Graphers.
       This routine ought to check the type of w and cast appropriately ****
       The whole reason for this callback struct is to get the event to
       pass on to Chris' routine that demands an event (3/31/91).
       Change o' mind.  After trying to get the right menu popup
       semantics, I realized that I could just chain the action
       procs appropriately in the translation table for the
       GrapherButtons.  This routine gets called now just to set
       the node_name.  Since the menu is spring loaded, no other
       node can sneak in and change the node_name (ie, the global
       is safe).  Eventually, the grapher and the sbrowser ought
       to set up the correct translations for the grapher buttons,
       but now this is done in the appdefaults file. (4/2/91) */
  AGGrapherButtonCallbackStruct *callback_struct = 
    (AGGrapherButtonCallbackStruct *)call_data;
#ifdef NOPE
    /* set up the params for the action proc call below */
  static String params[] = {"editMenuShell","test"};
#endif
    /* set this up so we can remember it in the menu callback */
    /* I could move this into the client_data by re-registering all
       of the callbacks, but it doesn't seem worth it.  The user
       has to select something from the menu or pop it down. */
  /* selection string is the node name */
  global_node_name = callback_struct->selection_string;
    /* KLUDGE ALERT:  w is the parent of the grapher button */
  global_node_parent = w;
#ifdef NOPE
    /* Want to trigger off the positioning manually */
  XtCallActionProc(w,"XawPositionSimpleMenu",
		   callback_struct->event,
		   params,(Cardinal)1);

    /* Now popup it up manually */
  XtPopupSpringLoaded(menu_shell);

    /* Another try */

  XtCallActionProc(menu_shell,"XtMenuPopup",
		   callback_struct->event,
		   params,(Cardinal)1);
#endif
  return;
}


  /* Callback for the edit menu.  Dispatches to the proper code. */
  /* client_data is coming back with node_name */
void Edit_Menu_Callback(w,client_data,call_data)
     Widget w;
     XtPointer client_data;
     XtPointer call_data;
{
  int menu_button = (int)client_data;
  Widget parent_w;
  Boolean flag;
  Boolean incoming;

  XtPopdown(menu_shell);
  
  incoming = False; 
  flag = False;
  if (global_node_name == NULL || !strcmplo(global_node_name,"noaction"))
     {
	flag = True;
     }
  
  switch (menu_button)
    {
      /* Cannot pass on w since w is actually an object.  For creating
	 additional shells, we need a widget (4/3/91). */
    case 0:
       if (flag)
	  {
	     XtWarning("cannot edit NoAction - it is equivalent to a no-op");
	     return;
	  }
       Edit_File(menu_shell,(char *)global_node_name,incoming);
      break;
    case 1:
       if (flag)
	  XtWarning("cannot edit NoAction - it is equivalent to a no-op");
       Edit_Node_Info(menu_shell,(char *)global_node_name,
		      incoming);
       break;
    case 2:
      /* This is an unabashed KLUDGE to correct a difference in UI semantics
	 between grapherbuttons on graphers and grapherbuttons in 
	 knedits/sbrowsers */
      if (XtIsSubclass(global_node_parent,kneditWidgetClass))
	 XtVaGetValues(global_node_parent,XtNnodeName,&global_node_name,NULL);
       Add_Node(menu_shell,(char *)call_data);
       break;
    case 3:
       Delete_Node(menu_shell,(char *)global_node_name, incoming);
       break;
     case 4:
       Add_Item(menu_shell,(char *)call_data,incoming);
       break;
     case MenuToggleButton:
       Toggle_Auto_Save(menu_shell,(char *)call_data);
       break;
     default:
       XtWarning("Missing value in edit callback.  Continuing....");
    }
}



/*************************************
  
  UTILITIES

**************************************/

static EditInfo *Edit_EditInfo_New()
{
    EditInfo *edit_info;
    int i;

    if ((edit_info = (EditInfo *)XtMalloc(sizeof(EditInfo))) == NULL)
	{
	    XtWarning("unable to allocate node information");
	}
    edit_info->menu_shell = menu_shell;  /*** kludge, should just be global **/
    edit_info->state = E_PRELIMINARY;
    edit_info->label = NULL;
    edit_info->location = NULL;
    edit_info->node_name = NULL;
    edit_info->type = -1;
    edit_info->header_values = NULL;
    edit_info->node_info = NULL;
    edit_info->editNIP_info.shell = NULL;
    for (i=0;i< NFileServHeaderValues;i++)
	edit_info->editNIP_info.header_values[i] = NULL;
    edit_info->editFile_info.shell = NULL;
    return(edit_info);
}

void Edit_EditInfo_Destroy(edit_info) 
     EditInfo *edit_info;
{
  int i;

  if (edit_info->label)
    XtFree(edit_info->label);
  if (edit_info->location)
    XtFree(edit_info->location);
  if (edit_info->node_name)
    XtFree(edit_info->node_name);
  
  NodeService_Register_Closed_Edit(edit_info->node_info,edit_info);
  
  for (i=0;i< NFileServHeaderValues;i++)
    if (edit_info->editNIP_info.header_values[i] != NULL)
      XtFree(edit_info->editNIP_info.header_values[i]);


  XtFree((char *)edit_info);
}



/************************************

  STUBS

************************************/


static void Add_Item(w,node_name)
     Widget w;
     char *node_name;
{
}
static void Delete_Node(w,node_name)
     Widget w;
     char *node_name;
{
}



static void Edit_InfoDialog_Put(w,string)
     Widget w;
     char *string;
{
  XtWarning(string);
}


/*****************************

  INPUT DIALOG ROUTINES

*****************************/

static void Edit_InputDialog_Okay_Callback(w,client_data,call_data)
     Widget w;
     XtPointer client_data;
     XtPointer call_data;
{
  EditInfo *edit_info = (EditInfo *)client_data;

  edit_info->inputDialog_info.cancel = False;
  edit_info->inputDialog_info.done = True;

  edit_info->inputDialog_info.string
    = (char *)XawDialogGetValueString(edit_info->inputDialog_info.dialog);
  XtDestroyWidget(edit_info->inputDialog_info.shell);
}


static void Edit_InputDialog_Cancel_Callback(w,client_data,call_data)
     Widget w;
     XtPointer client_data;
     XtPointer call_data;
{
  EditInfo *edit_info = (EditInfo *)client_data;

  edit_info->inputDialog_info.cancel = True;
  edit_info->inputDialog_info.done = True;

  XtDestroyWidget(edit_info->inputDialog_info.shell);
}


static void Edit_InputDialog_Put(w,string,edit_info)
     Widget w;
     char *string;
     EditInfo *edit_info;
{
  Widget shell;
  Widget dialog;

  edit_info->inputDialog_info.shell = shell
    = XtVaCreatePopupShell("inputDialogShell",transientShellWidgetClass,w,
			   XtNinput,(XtArgVal)True,
			   NULL);

  edit_info->inputDialog_info.dialog = dialog 
    = XtVaCreateManagedWidget("inputDialog",dialogWidgetClass,shell,
			    XtNlabel,string,
			    NULL);
  XawDialogAddButton(dialog,"inputDialogButton1",
		     Edit_InputDialog_Okay_Callback,(XtPointer)shell);
  XawDialogAddButton(dialog,"inputDialogButton2",
		     Edit_InputDialog_Cancel_Callback,(XtPointer)shell);
  XtPopup(shell,XtGrabExclusive);
}

Boolean Edit_Check_Write_Permission(file_string)
     char *file_string;
{
   Boolean rc;
   if ((rc = AG_File_Check_Write_Permission(file_string)) == False)
      XtWarning("you do not have write permission for editing this node");
   return(rc);
}

static Boolean Edit_Check_File(location,type)
    char *location;
    AGNodeTypes type;
{
   char buffer[MaxString];

   if (Check_Node_Physical_Existance(location))
      {
	 if (Edit_Check_Write_Permission(location))
	    return(True);
      }
   else
      {
	 sprintf(buffer,"unable to locate physical file \"%s\".  Please check",
		 location);
	 XtWarning(buffer);
	 XtWarning("\tfile system.  Continuing...");
      }
   return(False);

}



/***********************************************************************

  EDIT NODE:  Node must exist.  


************************************************************************/


void Edit_File(w,node_name,incoming)
     Widget w;
     char *node_name;
     Boolean incoming;
{
    char *label;
    AGNodeTypes type;
    char *location;
    EditInfo *edit_info;
    int status; 
    char buffer[MaxString];
    NodeInfoPtr node_info;
    char filestring[MaxString];

    if ((NodeService_Request_By_Name(node_name,&node_info,
				     &label,&type,&location) < 0))
       {
	  AGstrcpy(buffer,"Node does not exists.  Create it with ");
	  AGstrcat(buffer,"\"Add node\"");
	  Edit_InfoDialog_Put(w,buffer);
	  return;
       }
    if (type < 0)
       {
	  Edit_InfoDialog_Put(w,"unknown node type.  Aborting edit...");
	  return;
       }

    if ((edit_info = (EditInfo *) NodeService_Get_EditFileInfo(node_info)) 
	!= NULL)
      {
	if (edit_info->editFile_info.shell != NULL)
	    {
	      AG_Raise_Shell(edit_info->editFile_info.shell);
	      return;
	    }
	/* otherwise something is wrong, might as well create a new one */
      }

    if ((edit_info = Edit_EditInfo_New()) == NULL)
	return;


    edit_info->state = E_EDIT_FILE;
    edit_info->location = XtNewString(location);
    edit_info->label = XtNewString(label);
    edit_info->node_name = XtNewString(node_name);
    edit_info->type = type;
    edit_info->node_info = node_info;

    Form_Filename(filestring,edit_info->location);
    if (!NodeService_Open_File(node_info,filestring))
       {
	  XtWarning("error with file.  Not continuing with edit....");
	  Edit_EditInfo_Destroy(edit_info);
	  return;
       }

    /* register node in Edit_File_Create */

    if (Edit_Check_File(location,type))
	Edit_File_Create(w,edit_info);
    else
      {
	Util_Debug("unable to edit file in EditFile");
	Edit_EditInfo_Destroy(edit_info);
      }
	

#ifdef LATER 
    /*Should have something in history list*/
    Add_To_History_List(edit_info->node_name); 
#endif 
}



/************************

  EDIT NODE INFO

  Node must exist

************************/




void Edit_Node_Info(w,node_name,incoming)
     Widget w;
     char *node_name;
     Boolean incoming;
{
   char *label;
   AGNodeTypes type;
   char *location;
   EditInfo *edit_info;
   char *string_ptr;
   char buffer[MaxString];
   NodeInfoPtr node_info;
   char filestring[MaxString];

   if ((NodeService_Request_By_Name(node_name,&node_info,
				    &label,&type,&location) < 0))
     {
       if (!incoming)  /* from menu */
	 {
	   AGstrcpy(buffer,"Node does not exist.\n");
	   AGstrcat(buffer,"  Either cancel and select \"add node\" from");
	   AGstrcat(buffer," menu, or enter new node name");
	   Edit_InfoDialog_Put(w,buffer);
	   return;
	 }
       else  /* from an editfile */
	 {
	   AGstrcpy(buffer,"Node does not exists.  Create it with");
	   AGstrcat(buffer," \"Add node\"");
	   Edit_InfoDialog_Put(w,buffer);
	   return;
	 }
     }
   
   /* type < 0 -> problem with type or no action is to be taken */ 
   if (type < 0)
     {
       Edit_InfoDialog_Put(w,"unknown node type.  Aborting edit...");
       return;
     }

   if ((edit_info = (EditInfo *)NodeService_Get_EditNIPInfo(node_info)) 
       != NULL)
      {
	if (edit_info->editNIP_info.shell != NULL)
	    {
	      AG_Raise_Shell(edit_info->editNIP_info.shell);
	      return;
	    }
	/* otherwise something is wrong, might as well create a new one */   
      }



    if ((edit_info = Edit_EditInfo_New()) == NULL)
	return;

   edit_info->state = E_EDIT_NODEINFO;
   edit_info->location = XtNewString(location);
   edit_info->label = XtNewString(label);
   edit_info->node_name = XtNewString(node_name);
   edit_info->type = type;
   edit_info->node_info = node_info;

   /* open the file */
   Form_Filename(filestring,edit_info->location);
   if (!NodeService_Open_File(node_info,filestring))
     {
       XtWarning("error with file.  Not continuing with edit....");
       Edit_EditInfo_Destroy(edit_info);
       return;
     }
   
   /* register the editing session in Edit_NIP_Create */

   if (Edit_Check_File(location,type))
     Edit_NIP_Create(w,edit_info);
   else
     {
       Util_Debug("unable to edit file in EditFile");
       Edit_EditInfo_Destroy(edit_info);
     }

#ifdef LATER 
   /*Should have something in history list*/
   Add_To_History_List(edit_info->node_name); 
#endif 
}



/***********************************************************************

  ADD NODE:  Node can *not* exist.  

************************************************************************/

  /* This gets called when the editor selects a line in a existing
     grapher or knedit to add */
void Add_Node_Incoming(w,node_name,incoming_label)
     Widget w;
     char *node_name;
     char *incoming_label;
{
   char *junk_label;
   AGNodeTypes junk_type;
   char *junk_location;
   EditInfo *edit_info;
   NodeInfoPtr node_info;


   if ((NodeService_Request_By_Name(node_name,&node_info,&junk_label,
			    &junk_type,&junk_location) >= 0))
      {
	 Edit_InfoDialog_Put(w,"Node already exists.");
	 return;
      }


   if ((edit_info = Edit_EditInfo_New()) == NULL)
     return;

#ifdef NOPE
   if (!NodeService_Open_New_File(node_info))
     {
       XtWarning("error with file.  Not continuing with edit....");
       Edit_EditInfo_Destroy(edit_info);
       return;
     }
#endif

   edit_info->state = E_ADD_NODEINFO_FROM_EDITFILE;

   edit_info->node_name = XtNewString(node_name);
   if (incoming_label != NULL)
      edit_info->label = XtNewString(incoming_label);

   User_History_List_Add_Member(node_name,"Add Node Incoming");
   
   Edit_NIP_Create(w,edit_info);
}


  /* This gets called from the menu */
void Add_Node(w,node_name)
     Widget w;
     char *node_name;
{
  EditInfo *edit_info;
  char *junk_label;
  AGNodeTypes junk_type;
  char *junk_location;
  NodeInfoPtr node_info;
  
#ifdef OLD
  if ((NodeService_Request_By_Name(node_name,&node_info,&junk_label,
				   &junk_type,&junk_location) >= 0))
    {
      Edit_InfoDialog_Put(w,"Node already exists.");
      return;
    }
#endif
  
  if ((edit_info = Edit_EditInfo_New()) == NULL)
    return;

#ifdef NOPE
  if (!NodeService_Open_New_File(node_info))
    {
      XtWarning("error with file.  Not continuing with edit....");
      Edit_EditInfo_Destroy(edit_info);
      return;
    }
#endif

  edit_info->state = E_ADD_NODE_IN_NODEINFO;
  
  User_History_List_Add_Member(node_name,"Add Node");
  
  Edit_NIP_Create(w,edit_info);
}

/***********************************************************************

  TOGGLE AUTO SAVE

************************************************************************/

static void Toggle_Auto_Save(w,junk)
     Widget w;
     char *junk;
{

   if (edit_auto_save)
      {
	 edit_auto_save = False;
	 XtVaSetValues(menu_toggle_auto_save,
		       XtNlabel,"set add link to auto",NULL);
      }
   else
      {
	 edit_auto_save = True;
	 XtVaSetValues(menu_toggle_auto_save,
		       XtNlabel,"set add link to manual",NULL);
      }
}



/* For NodeService's Use */
Widget Edit_Get_EditFile_Shell(edit_info)
     EditInfo *edit_info;
{
  return((edit_info->editNIP_info.shell));
}

Widget Edit_Get_EditNIP_Shell(edit_info)
     EditInfo *edit_info;
{
  return((edit_info->editNIP_info.shell));
}




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

  The ANSWER GARDEN:  EDITINFO.C

     The major editing portions of Answer Garden.


  Mark Ackerman
  Information and Computer Science
  University of California, Irvine
  
  formerly -
  MIT/Center for Coordination Science
  
  ackerman@ics.uci.edu

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
#include <X11/Xaw/Dialog.h>

#include "Edit.h"

extern GlobalInfo global_info;

static void Edit_NIP_Cancel_Callback();
void Util_Get_UserName_With_Machine();



/***********************************

  NODEINFO PANEL

***********************************/


#define CheckNullString(string)  (string[0] == EOS)

#define IfManualToggleOn(field) \
  XtVaGetValues(edit_info->editNIP_info.field.toggle2, \
		XtNstate,&state, \
		NULL); \
  if (state)

#define GetValue(field) \
      XtVaGetValues(edit_info->editNIP_info.field.text, \
		    XtNstring,(XtArgVal)&new_string, \
		    NULL); 



static void Edit_NIP_Save_Ascii(edit_info)
     EditInfo *edit_info;
{
  extern void Edit_Add_Link();
  extern Boolean edit_auto_save;  /* from Edit.c */

  /* create the node info */
  edit_info->node_info = 
    NodeService_Add_Node(edit_info->node_name,
			 edit_info->label,edit_info->type,
			 edit_info->location);
  
  if (edit_auto_save)
    {
      Edit_Add_Link(edit_info);
    }

  edit_info->state = E_ADD_NODE_IN_FILE_AFTER_SAVE;

  return;
}




static void Edit_NIP_Okay_New_Callback(w,client_data,call_data)
     Widget w;
     caddr_t client_data;
     caddr_t call_data;
{
  EditInfo *edit_info = (EditInfo *)client_data;
  char buffer[MaxString];
  String new_string;
  Boolean state;
  int i;
  String temp1;
  AGNodeTypes temp2;
  String temp3;
  int month;
  int day;
  int year;
  NodeInfoPtr node_info;

  char **header_values = edit_info->header_values;

    /* Check whether this is in the right state.  If so,
       get the buffer from the text widget for the node's
       label and check whether it is different from
       the pre-existing one.  If it is, then it must be
       handed to the NodeService for insertion and 
       saving */
  if (edit_info->state != E_ADD_NODE_IN_NODEINFO &&
      edit_info->state !=  E_ADD_NODEINFO_FROM_EDITFILE)
    {
      sprintf(buffer,"Edit_NIP_Okay_New_Callback state (%d).  Continuing...",
	      edit_info->state);
      XtWarning(buffer);
      return;
    }


  /* If toggles for node name are set to "manual", then get the
     string that the user has entered.  There is no choice on
     name - one must be entered. */
  IfManualToggleOn(name)
    {
      GetValue(name);
      if (CheckNullString(new_string))
	{
	  XtWarning("You must specify a node name.    Cancelling request...");
	  return;
	}
      Util_Replace_String(&edit_info->node_name,new_string);
    }
  
  /* If toggles for label are set to "manual", then get the
     string that the user has entered.  If the user didn't enter
     a string, refuse to continue until he does.  If the toggles
     were set to "auto", then use the same string as for node_name. 
     (Auto mode is the default.) */
  IfManualToggleOn(label)
    {
      GetValue(label);
      if (CheckNullString(new_string))
	{
	  XtWarning("You must specify a label in manual mode.");
	  XtWarning("\tCancelling request.");
	  return;
	}
      Util_Replace_String(&edit_info->label,new_string);
    }
  else
    Util_Replace_String(&edit_info->label,edit_info->node_name);


  /* See comments for label - this is exactly the same. */
  IfManualToggleOn(storage)
    {
      GetValue(storage);
      if (CheckNullString(new_string))
	{
	  XtWarning("You must specify a storage location in manual mode.");
	  XtWarning("\tCancelling request.");
	  return;
	}
      Util_Replace_String(&edit_info->location,new_string);
    }
  else
    Util_Replace_String(&edit_info->location,edit_info->node_name);


  /* If the toggles for the expiration date are set to "manual",
     then get the string that the user has entered.  If the user
     has entered one, then refuse to continue until he has.  If
     the user entered one, then replace the proper header_values for
     the file with the entered string.

     If the toggles were set to "auto", then use a value for the date
     that is 2 years from the current date. */
  IfManualToggleOn(date)
    {
      GetValue(date);
      if (CheckNullString(new_string))
	{
	  XtWarning("You must specify an expiration date in manual mode.");
	  XtWarning("\tCancelling request.");
	  return;
	}
      Util_Replace_String(&header_values[FileServHeaderExpirationDate],
			  new_string);
    }
  else
     {
	Util_Get_LocalDate(&month,&day,&year);
	sprintf(buffer,"%02d/%02d/%02d",month,day,year+2);
	Util_Replace_String(&header_values[FileServHeaderExpirationDate],
			    buffer);
     }

  
  /* If the node was new, the text for node expert was set up
     with the default node expert (from the appdefaults file) if
     there was one.  If there was not a default node expert,
     the empty string put into the text widget.  

     If the toggles for expert were set to "manual", then get the
     string that the user has entered (or was already there).  
     If the string is null string (ie, he didn't enter one or
     blanked out the one that was there), then use NULL (which defaults 
     at run-time to the global expert for AG).  If the user did 
     enter a string, then use it.

     If the toggles for expert were set to "auto", and there is some
     string present, then use it.  If there is no
     string present (eg, if there was no default node expert) then get the user
     who has edited (or created) the node.  If you cannot, use NULL
     (which defaults to the global expert at run-time).  If you can
     get the current user, use his name as the node expert.  
     the user has entered.  

     To summarize behavior:

     If there is a default node expert in the appdefaults file, you
     will use it automatically if in auto mode and also in manual
     mode unless you change it.

     Manual      string in text widget           used as is
                 no string in text widget        NULL (global expert used)
     Auto        string in text widget           used as is
                 no string in text widget        set to current user if
		                                 possible, otherwise to
						 NULL (global expert used)

  */

  IfManualToggleOn(expert)
    { 
      GetValue(expert);
      if (CheckNullString(new_string))
	Util_Replace_String(&header_values[FileServHeaderNodeExpert],NULL);
      else
	Util_Replace_String(&header_values[FileServHeaderNodeExpert],
			    new_string);
   }
  else
     { 
       GetValue(expert);
       if (CheckNullString(new_string))
	 {
	   Util_Get_UserName_With_Machine(buffer);
	   if (buffer[0] == EOS)
	     Util_Replace_String(&header_values[FileServHeaderNodeExpert],NULL);
	   else
	     Util_Replace_String(&header_values[FileServHeaderNodeExpert],
				 buffer);
	 }
       else
	 Util_Replace_String(&header_values[FileServHeaderNodeExpert],
			     new_string);
     }
  
  for (i=0;i< global_info.n_global_node_types; i++)
    {
      XtVaGetValues(edit_info->editNIP_info.type_toggles[i],
		    XtNstate,&state, NULL);
      if (state)
	{
	  edit_info->type = NodeService_Node_Type_From_Int(i);
	  break;
	}
    }
  if (i>=global_info.n_global_node_types)
    {
      XtWarning("Unable to determine new node type.  Cancelling...");
      return;
    }

  if (NodeService_Request_By_Name(edit_info->node_name,&node_info,
				  &temp1,&temp2,&temp3) >= 0)
    {
      XtWarning("A node with this name already exists.  Try again.");
      return;
    }
  
    /* if you've gotten this far, set the rest of the header fields */
  Util_Get_UserName_With_Machine(buffer);
  Util_Replace_String(&header_values[FileServHeaderAuthor],buffer);
  Util_Replace_String(&header_values[FileServHeaderOrganization],
		      global_info.organization);

    /* hard-wire this for now  - this ought to go on the edit_info
       panel ***/
  Util_Replace_String(&header_values[FileServHeaderShowAuthor],"ShowNone");
  
  
  XtDestroyWidget(edit_info->editNIP_info.shell);
   /* Since we're not going to EditInfo_Destroy, and since
      this edit_info will be registered, must set shell to NULL */
  edit_info->editNIP_info.shell = NULL;

  edit_info->state = E_ADD_NODE_IN_FILE_BEFORE_SAVE;
  
  if (! NodeService_Is_Node_Type_Without_Headers(edit_info->type) )
    Edit_File_Create(edit_info->menu_shell,edit_info);
  else
    Edit_NIP_Save_Ascii(edit_info);
}


static void Edit_NIP_Okay_Exists_Callback(w,client_data,call_data)
     Widget w;
     caddr_t client_data;
     caddr_t call_data;
{
  EditInfo *edit_info = (EditInfo *)client_data;
  char buffer[MaxString];
  Boolean flag;
  String new_string;
  Boolean state;
  char **header_values = edit_info->header_values;

  flag = False;

    /* Check whether this is in the right state.  If so,
       get the buffer from the text widget for the node's
       label and check whether it is different from
       the pre-existing one.  If it is, then it must be
       handed to the NodeService for insertion and 
       saving */

  if (edit_info->state != E_EDIT_NODEINFO)
    {
      sprintf(buffer,"Edit_NIP_Okay_Exists_Callback state (%d)",
	      edit_info->state);
      Util_Debug(buffer);
      return;
    }

  GetValue(label);
  if (AGstrcmp(new_string,edit_info->label)) /* must be case sensitive */
    {
      /* there is a new label */
      flag = True;
      Util_Replace_String(&edit_info->label,new_string);
      if (NodeService_Replace_Node(edit_info->node_info,
				   edit_info->node_name,
				   edit_info->label,edit_info->type,
				   edit_info->location) < 0)
	XtWarning("Unable to save new node information.  Continuing...");
    }


  IfManualToggleOn(date)
    {
       flag = True;
       GetValue(date);
       if (CheckNullString(new_string))
	  {
	     XtWarning("You must specify an expiration date in manual mode.");
	     XtWarning("\tCancelling request....");
	     return;
	  }
       Util_Replace_String(&header_values[FileServHeaderExpirationDate],
			   new_string);
    }

  /* If the node existed, the text for the node expert was 
     whatever was already in the file for the node.

     If the user set it to manual, then use whatever he put there 
  */
  IfManualToggleOn(expert)
     {
	flag = True;
	GetValue(expert);
	if (CheckNullString(new_string))
	  Util_Replace_String(&header_values[FileServHeaderNodeExpert],NULL);
	else
	  Util_Replace_String(&header_values[FileServHeaderNodeExpert],
			    new_string);
	/* A null string is okay since that just means to use the global
	   experts */
     }

  if (!flag) /* nothing changed */
    {
      XtWarning("You didn't change anything.  Use \"cancel\" to exit.");
      return;
    }
  
  /* Did change something */
  if (!NodeService_Set_Defined_Headers(edit_info->node_info, header_values))
    {
      /* This should work unless data structures are corrupted. */
      XtWarning("Unable to save information in file.  Continuing...");
      return;
    }

  if (!NodeService_Save_File(edit_info->node_info,
			     NodeService_Get_Text(edit_info->node_info)))
    {
      XtWarning("Unable to save information in file.  Continuing...");
      return;
    }
      
    /* Use the cancel callback to destroy the widgets and 
       free edit_info */
  Edit_NIP_Cancel_Callback(w,client_data,call_data);
}


#undef CheckNullString
#undef IfManaulToggleOn
#undef GetValue



static void Edit_NIP_Cancel_Callback(w,client_data,call_data)
     Widget w;
     caddr_t client_data;
     caddr_t call_data;
{
  EditInfo *edit_info = (EditInfo *)client_data;
  XtDestroyWidget(edit_info->editNIP_info.shell);
  Edit_EditInfo_Destroy(edit_info);  /* end this all */
}

static void Edit_NIP_Text_On_Callback(w,client_data,call_data)
     Widget w;
     XtPointer client_data;
     XtPointer call_data;
{
   Widget text = (Widget)client_data;
   XtSetSensitive(text,True);
}

static void Edit_NIP_Text_Off_Callback(w,client_data,call_data)
     Widget w;
     XtPointer client_data;
     XtPointer call_data;
{
   Widget text = (Widget)client_data;
   XtSetSensitive(text,False);
}

static void Edit_NIP_Toggle_Text_Setup(toggle1,toggle2,text)
     Widget toggle1;
     Widget toggle2;
     Widget text;
{
  XtAddCallback(toggle1,XtNcallback,
		Edit_NIP_Text_Off_Callback,
		(XtPointer)text);
  XtAddCallback(toggle2,XtNcallback,
		Edit_NIP_Text_On_Callback,
		(XtPointer)text);
}

static void Edit_NIP_Setup_Node_Exists(edit_info)
     EditInfo *edit_info;
{
  int i;
  char **headers;

  headers = NodeService_Get_Defined_Headers(edit_info->node_info);
  /* This should get changed to be more efficient.  Doing it this
     way to adhere to older code */
  for (i=0;i<NFileServHeaderValues;i++)
    edit_info->editNIP_info.header_values[i] = XtNewString(headers[i]);
  edit_info->header_values = edit_info->editNIP_info.header_values;

  XtVaSetValues(edit_info->editNIP_info.name.text,
		XtNstring,(XtArgVal)edit_info->node_name,
		XtNsensitive,(XtArgVal)False,NULL);
  XtSetSensitive(edit_info->editNIP_info.name.toggle1,False);
  XtSetSensitive(edit_info->editNIP_info.name.toggle2,False);
  XtVaSetValues(edit_info->editNIP_info.label.text,
		XtNstring,(XtArgVal)edit_info->label,
		NULL);
  XtSetSensitive(edit_info->editNIP_info.label.toggle1,False);
  XtVaSetValues(edit_info->editNIP_info.storage.text,
		XtNstring,(XtArgVal)edit_info->location,
		XtNsensitive,(XtArgVal)False,
		NULL);
  XtSetSensitive(edit_info->editNIP_info.storage.toggle1,False);
  XtSetSensitive(edit_info->editNIP_info.storage.toggle2,False);
  XtVaSetValues(edit_info->editNIP_info.date.toggle1,
		XtNstate, (XtArgVal)True,
		NULL);
  XtVaSetValues(edit_info->editNIP_info.date.text,
		XtNsensitive,(XtArgVal)False,
		XtNstring,
		  (XtArgVal)(headers[FileServHeaderExpirationDate] != NULL) ?
		        headers[FileServHeaderExpirationDate] : "",
		NULL);
  Edit_NIP_Toggle_Text_Setup(edit_info->editNIP_info.date.toggle1,
			     edit_info->editNIP_info.date.toggle2,
			     edit_info->editNIP_info.date.text);
  
  XtVaSetValues(edit_info->editNIP_info.expert.text,
		XtNsensitive, (XtArgVal)False,
		XtNstring,
		  (XtArgVal)(headers[FileServHeaderNodeExpert] != NULL) ?
		        headers[FileServHeaderNodeExpert] : "",
		NULL);
  Edit_NIP_Toggle_Text_Setup(edit_info->editNIP_info.expert.toggle1,
			     edit_info->editNIP_info.expert.toggle2,
			     edit_info->editNIP_info.expert.text);
  
  XtVaSetValues(edit_info->editNIP_info.expert.toggle1,
		XtNstate, (XtArgVal)True,
		NULL);
  for (i=0;i<global_info.n_global_node_types;i++)
    XtSetSensitive(edit_info->editNIP_info.type_toggles[i],False); 
  if ((i = NodeService_Node_Int_From_Type(edit_info->type)) >= 0)
    XtVaSetValues(edit_info->editNIP_info.type_toggles[i],
		  XtNstate,(XtArgVal)True,
		  NULL);
  else
    XtWarning("unable to determine node type.  Continuing....\n");
  XtAddCallback(edit_info->editNIP_info.okay_button,XtNcallback,
		Edit_NIP_Okay_Exists_Callback,
		(XtPointer)edit_info);

} 

static void Edit_NIP_Setup_Node_New(edit_info)
     EditInfo *edit_info;

{
  int i;
  char **headers;

  headers =  edit_info->header_values  =
    edit_info->editNIP_info.header_values;

  for (i=0;i<NFileServHeaderValues;i++)
    headers[i] = NULL;


  XtVaSetValues(edit_info->editNIP_info.storage.text,
		XtNsensitive,(XtArgVal)False,
		NULL);
  XtVaSetValues(edit_info->editNIP_info.date.text,
		XtNsensitive,(XtArgVal)False,
		NULL);
  XtVaSetValues(edit_info->editNIP_info.expert.text,
		XtNstring, 
		  (XtArgVal) (global_info.node_expert) ?
  		    global_info.node_expert : "",
		XtNsensitive,(XtArgVal)False,
		NULL);
  Edit_NIP_Toggle_Text_Setup(edit_info->editNIP_info.storage.toggle1,
			     edit_info->editNIP_info.storage.toggle2,
			     edit_info->editNIP_info.storage.text);
  Edit_NIP_Toggle_Text_Setup(edit_info->editNIP_info.date.toggle1,
			     edit_info->editNIP_info.date.toggle2,
			     edit_info->editNIP_info.date.text);
  Edit_NIP_Toggle_Text_Setup(edit_info->editNIP_info.expert.toggle1,
			     edit_info->editNIP_info.expert.toggle2,
			     edit_info->editNIP_info.expert.text);
  XtAddCallback(edit_info->editNIP_info.okay_button,XtNcallback,
		Edit_NIP_Okay_New_Callback,
		(XtPointer)edit_info);

} 


  /* Routine to set up the NIP if it is coming in from a
     EditFile panel.  In this case, the node name and
     label may be preset in the edit_info even though
     the node is new. */
static void Edit_NIP_Setup_Node_Incoming_New(edit_info)
     EditInfo *edit_info;

{
  if (edit_info->node_name != NULL)
     XtVaSetValues(edit_info->editNIP_info.name.text,
		   XtNstring,edit_info->node_name,NULL);
  if (edit_info->label != NULL)
     XtVaSetValues(edit_info->editNIP_info.label.text,
		   XtNstring,edit_info->label,NULL);

  Edit_NIP_Setup_Node_New(edit_info);

} 



#define SetInstanceName(token)  AGstrcpy(string2,string1); \
                                strcat(string2,token)

static Widget Edit_NIP_Create_Radio(name,toggles,form,last_inner_form,edit_info)
     char *name;
     Widget toggles[];
     Widget form;
     Widget last_inner_form;
     EditInfo *edit_info;
{
  int i;
  Widget label;
  Widget box;
  char string1[MaxString];
  char string2[MaxString];
  Widget inner_form;

  AGstrcpy(string1,"editNIP");
  strcat(string1,name);

  SetInstanceName("InnerForm"); /* 6/19/91 get around resource converter problem */
  inner_form = XtVaCreateManagedWidget(string2,formWidgetClass,
				       form,
				       XtNfromVert,(XtArgVal)last_inner_form,
				       NULL);

  sprintf(string2,"%sLabel",string1);
  label = XtVaCreateManagedWidget(string2,labelWidgetClass,
				  inner_form,NULL);
  SetInstanceName("Box");
  box = XtVaCreateManagedWidget(string2,boxWidgetClass,
				inner_form, 
				XtNfromVert, (XtArgVal) label,
				NULL);
  for (i=0;i<global_info.n_global_node_types;i++)
    {
      sprintf(string2,"%sToggle%1d",string1,i+1);
      if (i>0)
	toggles[i] = XtVaCreateManagedWidget(string2,toggleWidgetClass,
					     box,
					     XtNradioGroup,
					       (XtArgVal)toggles[0],
					     NULL);
      else
	toggles[i] = XtVaCreateManagedWidget(string2,toggleWidgetClass,
					     box,
					     NULL);
      sprintf(string2,"%sToggle%1dLabel",string1,i+1);
      label = XtVaCreateManagedWidget(string2,labelWidgetClass,
				      box,
				      XtNlabel,
			      (XtArgVal)NodeService_Get_String_From_Type(i),
				      NULL);
  
    }
  return(inner_form);
}
      

static Widget Edit_NIP_Create_Sub(name,toggle1,toggle2,text,form,last_inner_form)
     char *name;
     Widget *toggle1;
     Widget *toggle2;
     Widget *text;
     Widget form;
     Widget last_inner_form;
{
  Widget label;
  Widget box;
  char string1[MaxString];
  char string2[MaxString];
  Widget inner_form;

  AGstrcpy(string1,"editNIP");
  strcat(string1,name);

  SetInstanceName("InnerForm"); /* 6/19/91 get around resource converter problem */
  inner_form = XtVaCreateManagedWidget(string2,formWidgetClass,
				       form,
				       XtNfromVert,(XtArgVal)last_inner_form,
				       NULL);

  SetInstanceName("Label");
  label = XtVaCreateManagedWidget(string2,labelWidgetClass,
				  inner_form,NULL);

  /*  Hard-wiring editType now, should be able to be set from
      resource file, but having problems 4/2/91 ****/
  SetInstanceName("Text");
  *text = XtVaCreateManagedWidget(string2,asciiTextWidgetClass,
				  inner_form,
				  XtNfromVert, (XtArgVal)label,
				  XtNeditType,(XtArgVal)XawtextEdit,
				  NULL);

  SetInstanceName("Box");
  box = XtVaCreateManagedWidget(string2,boxWidgetClass,
				inner_form, 
				XtNfromVert, (XtArgVal)label,
				XtNfromHoriz, (XtArgVal) *text,
				NULL);

  SetInstanceName("Toggle1");
  *toggle1 = XtVaCreateManagedWidget(string2,toggleWidgetClass,
				    box,NULL);

  SetInstanceName("Toggle1Label");
  label = XtVaCreateManagedWidget(string2,labelWidgetClass,
				  box,NULL);
  
  SetInstanceName("Toggle2");
  *toggle2 = XtVaCreateManagedWidget(string2,toggleWidgetClass,
				     box,
				     XtNradioGroup,(XtArgVal)*toggle1,
				     NULL);
  SetInstanceName("Toggle2Label");
  label = XtVaCreateManagedWidget(string2,labelWidgetClass,
				  box,NULL);

  return(inner_form);
}

void Edit_NIP_Create_Common(w,edit_info,shell_name,edit_text)
     Widget w;
     EditInfo *edit_info;
     char *shell_name;
     char *edit_text;
{
  int temp;
  char buffer[MaxString];
  Widget shell;
  Widget form;
  Widget command1;
  Widget command2;
  Widget text;
  Boolean exists;
  char *node_name;
  char filestring[MaxString];
  Widget inner_form;
  Boolean incoming;

  
  /* Create popup shell.  Give it a useful title. */
  edit_info->editNIP_info.shell = 
    shell = XtVaCreatePopupShell(shell_name,
				 topLevelShellWidgetClass,
				 w,
				 XtNtitle, (XtArgVal) shell_name,
				 XtNinput, (XtArgVal) True,
				 XtNallowShellResize, (XtArgVal) True,
				 NULL);
  
  /* create form */
  form = XtVaCreateManagedWidget("editNIPForm",formWidgetClass,
				 shell, NULL);
  
  text = XtVaCreateManagedWidget("editNIPInstructions",asciiTextWidgetClass,
				 form,
				 XtNstring,
				   (XtArgVal)edit_text,
				 XtNeditType, (XtArgVal)XawtextRead,
				 NULL);
  
  inner_form = Edit_NIP_Create_Radio("Type",
				     edit_info->editNIP_info.type_toggles,
				     form,text,edit_info);
  
  /* Node name */
  inner_form = Edit_NIP_Create_Sub("Name",
				   &edit_info->editNIP_info.name.toggle1,
				   &edit_info->editNIP_info.name.toggle2,
				   &edit_info->editNIP_info.name.text,
				   form,inner_form);
  
  /* Node label */
  inner_form = Edit_NIP_Create_Sub("Label",
				   &edit_info->editNIP_info.label.toggle1,
				   &edit_info->editNIP_info.label.toggle2,
				   &edit_info->editNIP_info.label.text,
				   form,inner_form);
  
  /* Physical storage location */
  inner_form = Edit_NIP_Create_Sub("Storage",
				   &edit_info->editNIP_info.storage.toggle1,
				   &edit_info->editNIP_info.storage.toggle2,
				   &edit_info->editNIP_info.storage.text,
				   form,inner_form);
  
    /* Expiration date for node */
    inner_form = Edit_NIP_Create_Sub("Date",
				     &edit_info->editNIP_info.date.toggle1,
				     &edit_info->editNIP_info.date.toggle2,
				     &edit_info->editNIP_info.date.text,
				     form,inner_form);
  
  inner_form = Edit_NIP_Create_Sub("Expert",
				   &edit_info->editNIP_info.expert.toggle1,
				   &edit_info->editNIP_info.expert.toggle2,
				   &edit_info->editNIP_info.expert.text,
				   form,inner_form);
  
    /* Okay and cancel buttons */
  edit_info->editNIP_info.okay_button = command1
    = XtVaCreateManagedWidget("editNIPButton1",commandWidgetClass,
				form,
				XtNfromVert,(XtArgVal)inner_form,
				NULL);
  
  command2 = XtVaCreateManagedWidget("editNIPButton2",commandWidgetClass,
				     form,
				     XtNfromHoriz, (XtArgVal)command1,
				     XtNfromVert,  (XtArgVal)inner_form,
				     NULL);
  XtAddCallback(command2,XtNcallback,Edit_NIP_Cancel_Callback,
		(XtPointer)edit_info);
}



void Edit_NIP_Create(w,edit_info)
     Widget w;
     EditInfo *edit_info;
{
    int temp;
    char buffer[MaxString];
    Widget shell;
    Widget form;
    Widget command1;
    Widget command2;
    Widget text;
    Boolean exists;
    char *node_name;
    char filestring[MaxString];
    Widget inner_form;
    Boolean incoming;
    char *edit_text;

    static String editNIP_info_text_new = 
"This is the editor for node information.\n\n\
You need to name the node (a later feature will be to\n\
create the name automatically) and provide the\n\
default label.  The node name must be suitable for a\n\
legal file name.  The default label will be the label\n\
given in Grapher nodes, but you will be able to\n\
override this label in the SBrowswer nodes.\n\n\
Unless you desire, the storage location will\n\
automatically be the same name as the node name and\n\
the expiration date for the information will be two\n\
years from now.  The default expert will be you.";

    static String editNIP_info_text_exists = 
"This is the editor for node information.\n\n\
You can edit only the label, expiration date\n\
and experts currently.";


    switch (edit_info->state)
       {
	case E_EDIT_NODEINFO:
	  exists = True;
	  incoming = False;
	  break;
	case E_ADD_NODEINFO_FROM_EDITFILE:
	  exists = False;
	  incoming = True;
	  break;
	case E_ADD_NODE_IN_NODEINFO:
	  exists = False;
	  incoming = False;
	  break;
	default:
	  XtWarning("unable to determine edit state.  aborting edit...");
	  return;
       }

    AGstrcpy(buffer,"NodeInfo: ");
    if (exists || incoming)
      strcat(buffer,edit_info->node_name);
    else
      strcat(buffer,"New Node");

    if (exists)
      edit_text = editNIP_info_text_exists;
    else
      edit_text = editNIP_info_text_new;

    Edit_NIP_Create_Common(w,edit_info,buffer,edit_text);
    shell = edit_info->editNIP_info.shell;

    if (incoming)
       Edit_NIP_Setup_Node_Incoming_New(edit_info);
    else if (exists)
       Edit_NIP_Setup_Node_Exists(edit_info);
    else
       Edit_NIP_Setup_Node_New(edit_info);
    
    XtRealizeWidget(shell);
    XtPopup(shell,XtGrabNone);


  if (edit_info->state == E_EDIT_NODEINFO)
    {
      NodeService_Register_Open_Node(edit_info->node_info,NULL,NULL,edit_info);
    }
}


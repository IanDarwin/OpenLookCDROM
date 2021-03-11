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

  The ANSWER GARDEN:  EDITFILE.C

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

/* These are declared in Edit.c */
extern XtPointer global_node_name; 
extern Boolean edit_auto_save;
extern Widget menu_shell;


char *Util_Remove_WhiteSpace();

/**********************************************

  FILE PANEL CALLBACKS

**********************************************/


void Edit_Add_Link(edit_info)
     EditInfo *edit_info;
{
  char *text;
  char *ptr;
  char buffer[MaxString];
  char file_string[MaxString];
  int len;
  NodeInfoPtr node_info;
  char *parent_label;
  AGNodeTypes parent_type;
  char *parent_location;

  if ((NodeService_Request_By_Name(global_node_name,&node_info,
				   &parent_label,
				   &parent_type,&parent_location)) > 0)
    {
      Form_Filename(file_string,parent_location);
      if (NodeService_Open_File(node_info,file_string))
	{
	  text = NodeService_Get_Text(node_info);
	  sprintf(buffer,"@button(%s,%s)\n",edit_info->label,
		  edit_info->node_name);
	  len = AGstrlen(text) + AGstrlen(buffer) + 1;
	  if ((ptr = XtMalloc(len)) != NULL)
	    {
	      AGstrcpy(ptr,text);
	      AGstrcat(ptr,buffer);
	      NodeService_Save_File(node_info,ptr);
	      NodeService_Close_File(node_info);
	    }
	  else
	    {
	      XtWarning("unable to save link in original file");
	      NodeService_Close_File(node_info);
	    }
	}
      else
	XtWarning("unable to save link in original file");
    }
}


static void Edit_File_New_Text_Callback(w,client_data,call_data)
     Widget w;
     XtPointer client_data;
     XtPointer call_data;
{
  Widget button = (Widget)client_data;
  XtSetSensitive(button,True);
   /* This comes in with the widget as the src ? */
  XtRemoveAllCallbacks(w,XtNcallback);
}

  
static void Edit_File_Save_Callback(w,client_data,call_data)
     Widget w;
     XtPointer client_data;
     XtPointer call_data;
{
   EditInfo *edit_info = (EditInfo *)client_data;
   
   char filename[MaxString];
   char buffer[MaxString];
   char *textbuffer;
   NodeInfoPtr junk_node_info;
   char *junk_label;
   char *junk_location;
   AGNodeTypes junk_type;

   XtVaGetValues(edit_info->editFile_info.text,XtNstring,&textbuffer,NULL);

   switch (edit_info->state)
     {
     case E_EDIT_FILE:
     case E_ADD_NODE_IN_FILE_AFTER_SAVE:
       NodeService_Save_File(edit_info->node_info,textbuffer);
       break;
     case E_ADD_NODE_IN_FILE_BEFORE_SAVE:
       Form_Filename(filename,edit_info->location);
       if (!Edit_Check_Write_Permission(edit_info->location))
	 return;

         /* The following piece of code is required in the nasty
	    case of a writer trying to add a node twice before saving
	    either.  Once one is saved, the other is doomed. */
       if ((NodeService_Request_By_Name(edit_info->node_name,&junk_node_info,
				     &junk_label,&junk_type,&junk_location) 
	    >= 0))
       {
	 XtWarning("Node now exists.  You must abort this transaction....");
	 Util_Debug("Node now exists in EditFileSaveCallback");
	 /* time to get out of here */
	 return;
       }


       /* create the node info */
       edit_info->node_info = 
	 NodeService_Add_Node(edit_info->node_name,
			      edit_info->label,edit_info->type,
			      edit_info->location);

       /* save the file (creates FileInfo as well) */
       if (!NodeService_Save_New_File(edit_info->node_info,filename,
				 edit_info->header_values,textbuffer))
	 XtWarning("Unable to save file.  Continuing...");
       
       NodeService_Register_Open_Node(edit_info->node_info,NULL,NULL,
				      edit_info);

       if (edit_auto_save)
	 {
	   Edit_Add_Link(edit_info);
	 }
       edit_info->state = E_ADD_NODE_IN_FILE_AFTER_SAVE;
       break;
     default:
       sprintf(buffer,"%s (%d) on node\n\t%s.  No write.   Continuing...",
	       "Edit_File_Save_File state",
	       edit_info->state,edit_info->node_name);
       Util_Debug(buffer);
       return;
     }
       
   XtSetSensitive(w,False);
   XtAddCallback(XawTextGetSource(edit_info->editFile_info.text),
		 XtNcallback,Edit_File_New_Text_Callback,
		 (XtPointer)w);
   
   /* too late to abort once the user has saved */
   XtSetSensitive(edit_info->editFile_info.abort_button,False);
   XtSetSensitive(edit_info->editFile_info.close_button,True);

}

static void Edit_File_Abort_Callback(w,client_data,call_data)
     Widget w;
     XtPointer client_data;
     XtPointer call_data;
{
    EditInfo *edit_info = (EditInfo *)client_data;
    XtDestroyWidget(edit_info->editFile_info.shell);
    Edit_EditInfo_Destroy(edit_info);
}

static void Edit_File_Close_Callback(w,client_data,call_data)
     Widget w;
     XtPointer client_data;
     XtPointer call_data;
{
    EditInfo *edit_info = (EditInfo *)client_data;
    XtDestroyWidget(edit_info->editFile_info.shell);
    Edit_EditInfo_Destroy(edit_info);
}


#define NDelimiters  4
static char endDelimiters[NDelimiters] = {'}',']',')','>'};
static char startDelimiters[NDelimiters] = {'{','[','(','<'};

static int Find_Delimiter(c)
     char c;
{
    int j;

    for (j=0;j<NDelimiters;j++)
	if (c == startDelimiters[j])
	    break;

    if (j>=NDelimiters)
	return(-2);

    return(j);
}

#define button_string "@button"
#define NButtonStringChars 7

static Boolean Edit_File_Parse_Selection(w,edit_info,node_name,label)
     Widget w;
     EditInfo *edit_info;  /* the old edit_info the edited file */
     char *node_name;
     char *label;
{
   Widget text;
   XawTextPosition begin;
   XawTextPosition end;
   char selection[MaxString];
   char token_for_label[MaxString];
   char token_for_name[MaxString];
   char junk_label[MaxString];
   char junk_location[MaxString];
   AGNodeTypes junk_type;
   int delimiter;
   char *ptr;
   char *end_ptr;
   char delimiter_char;

   text = edit_info->editFile_info.text;
   XawTextGetSelectionPos(text,&begin,&end);
   if (end <= begin)
      {
	 XtWarning("please select a line beforehand");
	 return(False);
      }
   XtVaGetValues(text,XtNstring,&ptr,NULL);
   AGMakeString(selection,&ptr[begin],&ptr[end]);
   /* wipe out any whitespace at the beginning of the line */
   Util_Remove_Leading_WhiteSpace(selection,token_for_label);
   Util_Remove_WhiteSpace(selection,token_for_name);
   
   if (!strncmplo(token_for_label,"@button",NButtonStringChars))
      {
	 ptr = &token_for_label[NButtonStringChars];
	 while (isspace(*ptr))
	    ptr++;
	 if ((delimiter = Find_Delimiter(*ptr)) < 0)
	    {
	       XtWarning("missing start delimiter in @button");
		 return(False);
	    }
	 ptr++;
	 while (isspace(*ptr))
	    ptr++;
	 if ((end_ptr = AGindex(token_for_label,',')) == NULL)
	    {
	       XtWarning("missing separator in @button");
	       return(False);
	    }
	 AGMakeString(label,ptr,end_ptr-1);
	 ptr = AGindex(token_for_name,',');
	 ptr++;
	 if ((end_ptr = AGindex(token_for_name,endDelimiters[delimiter])) == NULL)
	    {
	       XtWarning("missing end delimiter in @button");
	       return(False);
	      }
	 end_ptr--;
	 if (ptr > end_ptr)
	    {
	       XtWarning("missing node name in @button");
	       return(False);
	    }
	 AGMakeString(node_name,ptr,end_ptr);
      }
   else
      {  /* grapher */
	 if ((ptr = AGindex(token_for_label,'/')) == NULL)
	    {
	       XtWarning("missing separator in grapher item");
	       return(False);
	    }
	 AGMakeString(label,token_for_label,ptr-1);
	 ptr = AGindex(token_for_name,'/');
	 ptr++; /* a new instruction, for saber */
	 while (isspace(*ptr))
	    ptr++;
	 if ((end_ptr = AGindex(token_for_name,'\n')) == NULL)
	    {
	       if ((end_ptr = AGindex(token_for_name,EOS)) == NULL)
		   {
		      XtWarning("missing end of line in grapher item");
		      return(False);
		   }
	    }
	 end_ptr--;
	 AGMakeString(node_name,ptr,end_ptr);
      }
   return(True);
}


    
static void Edit_File_Add_Node_Callback(w,client_data,call_data)
     Widget w;
     XtPointer client_data;
     XtPointer call_data;
{
   EditInfo *edit_info = (EditInfo *)client_data;
   char node_name[MaxString];
   char label[MaxString];

   if (Edit_File_Parse_Selection(w,edit_info,node_name,label))
      Add_Node_Incoming(menu_shell,node_name,label);
 }
	  
/*  Routine to show a pre-existing node from the
    editFile panel */
static void Edit_File_Show_Node_Callback(w,client_data,call_data)
     Widget w;
     XtPointer client_data;
     XtPointer call_data;
{
   EditInfo *edit_info = (EditInfo *)client_data;
   char node_name[MaxString];
   char label[MaxString];
   char *junk_label;
   char *location;
   AGNodeTypes type;
   NodeInfoPtr node_info;

   /* If there is a selection and it can be parsed, and if the
      node name is not null or empty, then check to see whether the
      node exists already.  It can possibily not exist if the editor
      has added the node to the parent but not yet created the actual
      node in the NodeService.   If it does exist, then open it up
      in the usual way */

   if (Edit_File_Parse_Selection(w,edit_info,node_name,label))
     if (node_name != NULL && node_name[0] != EOS)
       if (NodeService_Request_By_Name(node_name,&node_info,&junk_label,&type,
				       &location) > 0)
	 {
#ifdef OLDAPI
	   AG_Create_Node(w,node_name);
#else
	   AG_Create_Node(w,node_name,NULL,0);
#endif
	   return;
	 }
       else
	 XtWarning("Node does not exist yet.");
}
	  
    
static void Edit_File_Edit_File_Callback(w,client_data,call_data)
     Widget w;
     XtPointer client_data;
     XtPointer call_data;
{
   EditInfo *edit_info = (EditInfo *)client_data;
   char node_name[MaxString];
   char label[MaxString];
   
   if (Edit_File_Parse_Selection(w,edit_info,node_name,label))
      Edit_File(menu_shell,node_name,True);
 }
	  
static void Edit_File_Edit_NodeInfo_Callback(w,client_data,call_data)
     Widget w;
     XtPointer client_data;
     XtPointer call_data;
{
   EditInfo *edit_info = (EditInfo *)client_data;
   char node_name[MaxString];
   char label[MaxString];
   
   if (Edit_File_Parse_Selection(w,edit_info,node_name,label))
      Edit_Node_Info(menu_shell,node_name,True);
 }
	  
	  






/*********************************

  FILE EDIT PANEL

*********************************/

  /**** Once this exists, shouldn't I just reset the contents of the
    text widget?  Re-creating each time is somewhat easier. (4/3/91) ***/
int Edit_File_Create(w,edit_info)
     Widget w;
     EditInfo *edit_info;
{
    char filename[MaxString];
    Arg arglist[10];
    Widget shell, form,titlebar,command,text;
    char *text_name;
    char *buffer;
    char msg_buffer[MaxString];
    char *name_ptr;

    AGstrcpy(msg_buffer,"NodeFile: ");
    if (edit_info->node_name != NULL)
       AGstrcat(msg_buffer,edit_info->node_name);
    else
       AGstrcat(msg_buffer,"no name");

    edit_info->editFile_info.shell = shell 
      = XtVaCreatePopupShell(msg_buffer,
			     topLevelShellWidgetClass, w,
			     XtNtitle, (XtArgVal)msg_buffer,
			     XtNinput, (XtArgVal)True,
			     XtNallowShellResize, (XtArgVal)True,
			     NULL);

    form = XtCreateManagedWidget("editorForm",formWidgetClass,shell,
				 NULL,0);
    titlebar = XtVaCreateManagedWidget("editorLabel",labelWidgetClass,form,
				       XtNborderWidth, (XtArgVal)0,
				       XtNlabel, (XtArgVal)edit_info->node_name,
				       XtNfromHoriz, (XtArgVal) NULL,
				       NULL);

    /* This ought to change to a pre-edit function, but for now it's
       hard wired */
    if (strcmplo(NodeService_Get_String_From_Type(edit_info->type),
		 "Discussion") == 0)
      text_name = "editorDiscussion";
    else
      text_name = "editorText";
    switch (edit_info->state)
	{
	case  E_EDIT_FILE:
	  buffer= NodeService_Get_Text(edit_info->node_info);
	  edit_info->editFile_info.text = text = 
	    XtVaCreateManagedWidget(text_name,asciiTextWidgetClass,
				    form,
				    XtNtype, (XtArgVal)XawAsciiString,
				    XtNstring, (XtArgVal)buffer,
				    XtNlength, (XtArgVal)AGstrlen(buffer),
				    XtNeditType, (XtArgVal)XawtextEdit,
				    XtNscrollHorizontal, 
				    (XtArgVal)XawtextScrollWhenNeeded,
				    XtNscrollVertical, 
				    (XtArgVal)XawtextScrollWhenNeeded,
				    XtNfromVert, (XtArgVal)titlebar,
				    NULL);
#ifdef ADD_BACK_IN_LATER
	  /* Unfortunately, I have two copies of the text, so I might
	     as well get rid of the old one. */
	  AG_File_Free_Buffer(edit_info->fileInfo);
#endif
	  break;

	case E_ADD_NODE_IN_FILE_BEFORE_SAVE:
	  edit_info->editFile_info.text = text = 
	    XtVaCreateManagedWidget(text_name,asciiTextWidgetClass,
				    form,
				    XtNtype, (XtArgVal)XawAsciiString,
				    XtNscrollHorizontal, 
				    (XtArgVal)XawtextScrollWhenNeeded,
				    XtNscrollVertical, 
				    (XtArgVal)XawtextScrollWhenNeeded,
				    XtNeditType, (XtArgVal)XawtextEdit,
				    XtNfromVert, (XtArgVal)titlebar,
				    NULL);
	    break;
	  default:
	    sprintf(msg_buffer,"Edit_File state (%d). Continuing...",
		    edit_info->state);
	    XtWarning(msg_buffer);
	}
    
    edit_info->editFile_info.abort_button = command 
      = XtVaCreateManagedWidget("editorButton1",commandWidgetClass,form,
				      XtNfromVert, (XtArgVal)text,
				      NULL);
    XtAddCallback(command,XtNcallback,Edit_File_Abort_Callback,
		  (XtPointer) edit_info);


      /* Initially, the close button is not available */
    edit_info->editFile_info.close_button = command
      = XtVaCreateManagedWidget("editorButton2",commandWidgetClass,form,
				      XtNfromVert, (XtArgVal)text,
				      XtNfromHoriz, (XtArgVal)command,
				      XtNsensitive, (XtArgVal)False,
				      NULL);
    XtAddCallback(command,XtNcallback,Edit_File_Close_Callback,
		  (XtPointer) edit_info);
    
    if (edit_info->state == E_ADD_NODE_IN_FILE_BEFORE_SAVE)
       edit_info->editFile_info.save_button = command 
	  = XtVaCreateManagedWidget("editorButton3",commandWidgetClass,form,
				    XtNfromVert, (XtArgVal)text,
				    XtNfromHoriz, (XtArgVal)command,
				    NULL);
    else
       {
	  edit_info->editFile_info.save_button = command 
	     = XtVaCreateManagedWidget("editorButton3",commandWidgetClass,form,
				       XtNsensitive, (XtArgVal)False,
				       XtNfromHoriz, (XtArgVal)command,
				       XtNfromVert, (XtArgVal)text,
				       NULL);
	  XtAddCallback(XawTextGetSource(edit_info->editFile_info.text),
			XtNcallback,Edit_File_New_Text_Callback,
			(XtPointer)command);
       }


    XtAddCallback(command,XtNcallback,Edit_File_Save_Callback,
				      (XtPointer) edit_info);
    command = XtVaCreateManagedWidget("editorButton4",commandWidgetClass,form,
				      XtNfromVert, (XtArgVal)text,
				      XtNfromHoriz, (XtArgVal)command,
				      NULL);
    XtAddCallback(command,XtNcallback,Edit_File_Edit_File_Callback,
		  (XtPointer) edit_info);		  
    command = XtVaCreateManagedWidget("editorButton5",commandWidgetClass,form,
				      XtNfromVert, (XtArgVal)text,
				      XtNfromHoriz, (XtArgVal)command,
				      NULL);
    XtAddCallback(command,XtNcallback,Edit_File_Edit_NodeInfo_Callback,
		  (XtPointer) edit_info);		  

    command = XtVaCreateManagedWidget("editorButton6",commandWidgetClass,form,
				      XtNfromVert, (XtArgVal)text,
				      XtNfromHoriz, (XtArgVal)command,
				      NULL);
    XtAddCallback(command,XtNcallback,Edit_File_Add_Node_Callback,
		  (XtPointer) edit_info);		  

    command = XtVaCreateManagedWidget("editorButton7",commandWidgetClass,form,
				      XtNfromVert, (XtArgVal)text,
				      XtNfromHoriz, (XtArgVal)command,
				      NULL);
    XtAddCallback(command,XtNcallback,Edit_File_Show_Node_Callback,
		  (XtPointer) edit_info);		  

    XtRealizeWidget(edit_info->editFile_info.shell);
    XtPopup(edit_info->editFile_info.shell,XtGrabNone);
    
    if (edit_info->state == E_EDIT_FILE)
      {
	/* no nodeinfo for registration unless pre-existing */
	NodeService_Register_Open_Node(edit_info->node_info,
				       NULL,edit_info,NULL);
      }

    return(True);
}





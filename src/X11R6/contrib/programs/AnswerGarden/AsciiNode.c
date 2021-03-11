/**********************************************************


  The ANSWER GARDEN PROJECT:  AsciiNode.c

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


***********************************************************/
#include <X11/StringDefs.h>
#include <X11/Intrinsic.h>
#include <X11/Shell.h>
#include <X11/Xaw/Command.h>
#include <X11/Xaw/AsciiText.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/Label.h>
#include "AG.h"

extern GlobalInfo global_info;


static void AsciiType_Put_Error(w,msg)
     Widget w;
     char *msg;
{
   XtWarning(msg);
}

static void AsciiType_Destructor_Callback(w,client_data,call_data)
     Widget w;
     XtPointer client_data;
     XtPointer call_data;
{
  /* currently unnecessary */
}


static Widget AsciiType_Create(w,node_info,geometry)
     Widget w;
     NodeInfoPtr node_info;
     XtPointer geometry;
{
    Widget shell;
    Widget text;
    Widget form;
    Widget command[5];
    Widget button_form;
    Widget label;

    Arg arglist[10];
    char filestring[MaxString];
    char *file_body_buffer;
    char msg[MaxString];
    char resource_name[MaxString];

    char *filename;
    int num;


    filename = NodeService_Get_Location (node_info);

    if (filename == NULL)
      {
	 AsciiType_Put_Error(w,
			   "null name passed to Ascii node.  Continuing...");
	 return(NULL);
      }

    Form_Filename(filestring,filename);
    if (! NodeService_Open_File(node_info,filestring))
      {
	sprintf(msg,"error accessing ascii-node file %s.  Continuing...",
		 filename);
	AsciiType_Put_Error(w,msg);
	NodeService_Close_File(node_info);
	return(NULL);
      }

    if (geometry == NULL)
      shell = XtVaCreatePopupShell(NodeService_Get_Node_Name(node_info),
				   topLevelShellWidgetClass,
				   global_info.main_shell,
				   XtNinput,(XtArgVal)True,
				   XtNallowShellResize, (XtArgVal)True,
				   XtNtitle, (XtArgVal)
				     NodeService_Get_Label(node_info),
				   NULL);
    else
      shell = XtVaCreatePopupShell(NodeService_Get_Node_Name(node_info),
				   topLevelShellWidgetClass,
				   global_info.main_shell,
				   XtNinput,(XtArgVal)True,
				   XtNallowShellResize, (XtArgVal)True,
				   XtNgeometry,(XtArgVal)geometry,
				   XtNtitle, (XtArgVal)
				     NodeService_Get_Label(node_info),
				   NULL);


    form = XtCreateManagedWidget("asciiNodeForm",formWidgetClass,shell,
				 NULL,0);

    label = XtVaCreateManagedWidget("asciiNodeLabel",labelWidgetClass,form,
				    XtNlabel,filename,
				    NULL);

    file_body_buffer = NodeService_Get_Text(node_info);

    text = XtVaCreateManagedWidget("asciiNodeText",asciiTextWidgetClass,form,
		     XtNtype, (XtArgVal)XawAsciiString,
		     XtNstring, (XtArgVal) file_body_buffer,
		     XtNeditType, (XtArgVal)XawtextRead,
		     XtNscrollHorizontal, 
					  (XtArgVal)XawtextScrollWhenNeeded,
		     XtNscrollVertical, 
					  (XtArgVal)XawtextScrollWhenNeeded,
		     XtNuseStringInPlace, (XtArgVal) True,
		     XtNlength, (XtArgVal) AGstrlen(file_body_buffer),
		     XtNfromVert, (XtArgVal)label,
		     XtNfromHoriz, (XtArgVal)NULL,
		     NULL);

    Util_Open_ButtonBox_With_Bindings(form,&button_form,False,NULL,&num,
				      (XtPointer)node_info);
    XtVaSetValues(button_form,XtNfromVert,(XtArgVal)text,NULL);
#ifdef OLD
    Util_Open_ButtonBox(form,&button_form,command,NULL,NULL,0);
    XtVaSetValues(button_form,XtNfromVert,(XtArgVal)text,NULL);

    XtAddCallback(command[2],XtNcallback,Dynamic_Question_Callback,
		  (XtPointer)node_info);
    XtAddCallback(command[1],XtNcallback,Dynamic_Help_Callback,
		  (XtPointer)node_info);
    XtAddCallback(command[0],XtNcallback,Dynamic_Close_Callback,
		  (XtPointer)node_info);
#endif
    XtAddCallback(shell,XtNdestroyCallback,AsciiType_Destructor_Callback,
		  (XtPointer)node_info);

    AG_Geometry(shell,node_info);
    XtRealizeWidget(shell);
    XtPopup(shell,XtGrabNone);
    return(shell);
}

Widget Ascii_Create(w,node_info,geometry,misc_info,num_info)
     Widget w;
     NodeInfoPtr node_info;
     String geometry;
     String *misc_info;
     int num_info;
{
  return(AsciiType_Create(w,node_info,geometry));
}









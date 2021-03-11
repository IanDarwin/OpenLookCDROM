/***************************************************************************

  The ANSWER GARDEN PROJECT

  COMMSERV.C  The Communications Service

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

  system dependencies:
    1.  Program uses a "system" call to send mail.  See mailer types.
        The system handles MH and /bin/mail, but can be extended easily.
    2.  Program uses gettimefday to get system time.

  To compile Answer Garden to disable editing, give cc the flag -DNO_EDIT.

*****************************************************************************/
#include <stdio.h>
#include <X11/StringDefs.h>
#include <X11/Intrinsic.h>
#include <X11/Shell.h>
#include <X11/Xaw/Command.h>
#include <X11/Xaw/Box.h>
#include <X11/Xaw/AsciiText.h>
#include <X11/Xaw/Viewport.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/Dialog.h>
#include "Knedit.h"
#include "AG.h"

extern GlobalInfo global_info;
extern UserSettings user_settings;



  /* used for mailer callbacks */
typedef struct _MailerPassback
{
    Widget shell;
    Widget text;
    char *internal_header;
    NodeInfoPtr node_info;
} MailerPassback;


#define NotAbleToSendMail  127

/************************

  MAILER ROUTINES

*************************/

void Comm_Destructor(mailer_passback)
     MailerPassback *mailer_passback;
{
   if (mailer_passback)
      {
	 XtDestroyWidget(mailer_passback->shell);
	 XtFree((char *)mailer_passback);
      }
}

void Mailer_Cancel_Callback(w,client_data,call_data)
     Widget w;
     XtPointer client_data;
     XtPointer call_data;
{
    MailerPassback *mailer_passback;
    mailer_passback = (MailerPassback *)client_data;

    Stat_Button_Hit(w);
    Comm_Destructor(mailer_passback);
}

void Comm_Initialize_Mailer(mailer_passback,generic_pointer,mail_file)
     MailerPassback *mailer_passback;
     XtPointer *generic_pointer;
     char **mail_file;
{
   FILE *fp;
   char *home;
   char directory[MaxString];
   static char filename[MaxString];
   int temp;

   *generic_pointer = NULL;
   if ((home = getenv("HOME")) != NULL)
      {
	 temp = AGstrlen(home);
	 if (temp > 255)
	    {
		    XtWarning("Directory name for mail file is too long");
		    return;
		 }
	 AGstrcpy(directory,home);
	 strcat(directory,"/");
      }
   
   if ( (temp + AGstrlen(MailUserFile)) > 255)
      {
	 XtWarning("File name for mail file is too long");
	 return;
      }

   AGstrcpy(filename,directory);
   strcat(filename,MailUserFile);
   
   if ((fp = fopen(filename,"w")) == NULL)
      {
	    XtWarning("Cannot open mail file \n");
	    return;
	 }
   *generic_pointer = (XtPointer)fp;
   *mail_file = filename;
}

/****************************************************************************

  Find the expert (that's what this is all about, isn't it?)
  
  The algorithm goes like this:

  useLocalExpert    local node?    node expert      use

  true              yes            set              node expert
  true              yes            not set          local expert list
  true              no             set              local expert list; forward
  true              no             not set          local expert list
  false             yes            set              node expert
  false             yes            not set          local expert list; forward
  false             no             set              node expert
  false             no             not set          global expert list

  How is a local node determined?  
    If the organization in the node header matches the current organization,
    then this must be a local node.  (An alternative would be to check
    the node header's organization against a global organization.  However,
    there may be a node contributed by a different organization from
    the global organization (MIT in the case of hte X database, or 
    SAO in the case of the PROS database).  If this organization is not
    the same as the organization for the contributed node, then that
    contributing organization has contributed essentially a global node.

    If this node is global but contributed by this organization, this
    algorithm still works since if a node expert is specified for the node,
    then the question/comment will still go to that node expert.


*****************************************************************************/

Comm_Get_Expert(mailer_passback,node_expert,forward_expert,local_node)
     MailerPassback *mailer_passback;
     char **node_expert;
     char **forward_expert;
     Boolean *local_node;
{
  char **header_values;
  
  *node_expert = NULL;
  *forward_expert = NULL;
  *local_node = False;

  header_values = NodeService_Get_Defined_Headers(mailer_passback->node_info);


  /* What to do if there is no header?  Probably a node type ought to
     register these headers for a virtual or temporary node. */
  if (header_values == NULL)
    {
      *local_node = True;
      if (global_info.use_local_expert)
	{
	  *node_expert = global_info.local_expert_list;
	  if (*node_expert == NULL)
	    *node_expert = global_info.global_expert_list;
	  *forward_expert = NULL;
	}
      else
	*node_expert = global_info.global_expert_list;
      return;
    }

  if (header_values == NULL ||
      !strcmplo(header_values[FileServHeaderOrganization],
	       global_info.organization))
    *local_node = True;
  else
    *local_node = False;

  if (header_values != NULL)
    *node_expert = header_values[FileServHeaderNodeExpert];
  else
    *node_expert = NULL;

  if (global_info.use_local_expert)
    {
      if (*local_node)
	{
	  if (*node_expert != NULL)
	    ; /* okay */
	  else
	    *node_expert = global_info.local_expert_list;
	}
      else
	{
	  if (*node_expert != NULL)
	    {
	      *forward_expert = *node_expert;
	      *node_expert = global_info.local_expert_list;
	    }
	  else
	    *node_expert = global_info.local_expert_list;
	}
    }
  else
    {
      if (*local_node)
	{
	  if (*node_expert != NULL)
	    ;
	  else
	    {
	      *forward_expert = *node_expert;
	      *node_expert = global_info.local_expert_list;
	    }
	}
      else
	{
	  if (*node_expert != NULL)
	    ;
	  else
	    *node_expert = global_info.global_expert_list;
	}
    }

}


/*****************************************************************************

  Write a header that doesn't include the history list and all
  the extras.  This just does the To: and the Subject: lines.

******************************************************************************/

void Comm_Write_Copy_Header(mailer_passback,generic_pointer)
     MailerPassback *mailer_passback;
     XtPointer generic_pointer;
{
   char *node_expert;
   FILE *fp = (FILE *)generic_pointer;
   char buffer[MaxString];

   Util_Get_UserName_With_Machine(buffer);
   fprintf(fp,"To:\t%s\n",buffer);
   /* MSA 6/2/94 SAO patch */
   fprintf(fp,"Subject:\t%s/ Question/%s/%s/Q\n",
	   global_info.program_name,
	   NodeService_Get_Node_Name(mailer_passback->node_info),
	   NodeService_Get_String_From_Type
	    (NodeService_Get_Type(mailer_passback->node_info)));
   fprintf(fp,"------------\n");
 }

/*****************************************************************************

  Write out a complete header.  This includes:

    To:            the appropriate expert (see Comm_Get_Expert for more info)
    Cc:            a notification list (usually me (Ack))
    Subject:       a fully qualified subject line (see below)
    Reply-To:       useful for some experts' mailers
    Organization:  the user's organization (from appdefaults file)
    Version:       AG version number (from AG.h, hardwired in)
    MessageType:   hardwired here to XQuestion (for Info Lens type systems)
    HistoryList:   the history list of accessed nodes for the last 15 minutes
                   or the beginning of the session (whichever appropriate)
    NodeLocation:  the name of the node
    NodeLabel:     the label of the node
    NodeFile:      the physical location of the node

    From:          user name (provided by mailer)

  This is a semi-structured header, in the sense that additional info
  is provided in the header, so that Information Lens or Object Lens type
  systems can operate on this additional information.  

  The Subject: line is used by the Garden Question/Answer Tracking Server 
  (ATS for short).  It provides the topic (for humans), the node name
  (for appending answers to QA nodes and the like), the node type (to
  determine whether automatic replacement is feasible), the tracking
  stage (Q for the user asking a question, here).  In later uses, the
  Subject: line will also be the location for the assigned user-id 
  (equivalent to the question-id).

******************************************************************************/

void Comm_Write_Outgoing_Header(mailer_passback,generic_pointer)
     MailerPassback *mailer_passback;
     XtPointer generic_pointer;
{
   char *node_expert;
   char *forward_expert;
   Boolean local_node;
   char user_info[MaxString];
   FILE *fp = (FILE *)generic_pointer;

   Comm_Get_Expert(mailer_passback,&node_expert,&forward_expert,&local_node);

   if (node_expert)
      fprintf(fp,"To:\t%s\n",node_expert);

   fprintf(fp,"Cc:\t%s\nSubject:\tAnswerGarden Question/%s/%s/Q\n",
	   MailQuestionNotificationList,
	   NodeService_Get_Node_Name(mailer_passback->node_info),
	   NodeService_Get_String_From_Type
	    (NodeService_Get_Type(mailer_passback->node_info)));
   
   if (forward_expert != NULL)
     fprintf(fp,"Forward-To:\t%s\n",forward_expert);
   
   if (global_info.tracking_server_address != NULL)
     fprintf(fp,"Reply-To:\t%s\n",global_info.tracking_server_address);
   fprintf(fp,"MessageType:\tXQuestion\n");
   
   Util_Get_UserInfo(user_info);
   if (user_info != NULL)
     fprintf(fp,"UserInfo:\t%s\n",user_info);
   fprintf(fp,"Organization:\t%s\n",global_info.organization);
   fprintf(fp,"Version:\t%s\n",AGVersionNumber);   
   User_History_Print(fp);


   fprintf(fp,"NodeLocation:\t%s\n",
	   NodeService_Get_Node_Name(mailer_passback->node_info));


   fprintf(fp,"NodeLabel:\t%s\n",
	   NodeService_Get_Label(mailer_passback->node_info));
   fprintf(fp,"NodeFile:\t%s\n",
	   NodeService_Get_Location(mailer_passback->node_info));
   fprintf(fp,"------------\n");
}


/*****************************************************************************

  Write out the body of the question.  This is just retrieving the text
  from the appropriate text widget.

******************************************************************************/
void Comm_Write_Outgoing_Body(mailer_passback,generic_pointer)
     MailerPassback *mailer_passback;
     XtPointer generic_pointer;
{
   int offset_into_msg;
   char *ptr;
   FILE *fp = (FILE *)generic_pointer;

   /* Extra carriage return on end to avoid mailer problem.
      (If no last \n, then spost rejects mail message.) */

   XtVaGetValues(mailer_passback->text,XtNstring,&ptr,NULL);
   fprintf(fp,"%s\n",ptr);
}

void Comm_End_Write(mailer_passback,generic_pointer)
     MailerPassback *mailer_passback;
     XtPointer generic_pointer;
{
   FILE *fp = (FILE *)generic_pointer;

   fflush(fp);
   fclose(fp);
}



/*****************************************************************************

  The message is out on the temporary file and the communications mechanism
  has been initialized.  It's time to invoke the specified communications
  mechanism (specified in the appdefaults file, natch).

  Right now, AG knows about only the Rand Mail Handler (mh) and /bin/mail.
  Usually /bin/mail is faster on Unix platforms, and mh often invokes
  /bin/mail anyways.  

*****************************************************************************/
  
/* what to do with warning message ***/

int Comm_Send_Mail(mailer_passback,generic_pointer,mail_file)
     MailerPassback *mailer_passback;
     XtPointer generic_pointer;
     char *mail_file;
{
  char buffer[MaxString];
  int rc;
  char *node_expert;
  char *forward_expert;
  Boolean local_node;
  
  
  if (!strcmplo(global_info.mailer_type,"MH"))
    {
      sprintf(buffer,"send %s",mail_file);
      if ((rc = system(buffer)) == NotAbleToSendMail)
	XtWarning("Could not send mail: 1");
      else
	XtWarning("mail sent");
    }
  else if (!strcmplo(global_info.mailer_type,"BinMail"))
    {
      Comm_Get_Expert(mailer_passback,&node_expert,&forward_expert,
		      &local_node);
      
      sprintf(buffer,"/bin/mail %s %s < %s",
	      node_expert,
	      MailQuestionNotificationList,
	      mail_file);
      if ((rc = system(buffer)) == NotAbleToSendMail)
	XtWarning("Could not send mail: 1");
      else
	XtWarning("mail sent");
    }
  else
    {
      rc = NotAbleToSendMail;
       XtWarning("Could not send mail: don't know your mailer");
    }
  return(rc);
}

int Comm_Send_Copy(mailer_passback,generic_pointer,mail_file)
     MailerPassback *mailer_passback;
     XtPointer generic_pointer;
     char *mail_file;
{
   char buffer[MaxString];
   int rc;
   char user[MaxString];

   if (!strcmplo(global_info.mailer_type,"MH"))
      {
	 sprintf(buffer,"send %s",mail_file);
	 if ((rc = system(buffer)) == NotAbleToSendMail)
	    XtWarning("Could not send mail: 1");
	 else
	   XtWarning("copy of mail sent to your account");

      }
   else if (!strcmplo(global_info.mailer_type,"BinMail"))
      {
	Util_Get_UserName_With_Machine(user);
	sprintf(buffer,"/bin/mail %s < %s",
		user,
		mail_file);
	if ((rc = system(buffer)) == NotAbleToSendMail)
	  XtWarning("Could not send mail: 1");
	else
	  XtWarning("copy of mail sent to your account");
      }
   else
      {
	 rc = NotAbleToSendMail;
	 XtWarning("Could not send mail: don't know your mailer");
      }
   return(rc);
}



/****************************************************************************

  This is a callback to send a text question specified in a popup 
  (from GeneralLostCallback) and send it via e-mail.

  There are two reasons for the modularity here, ie putting the functionality
  into lots of tiny routines.  First, it is anticipated that AG hackers
  will write their own routines to include additional information,
  change the location of the temporary file, or to use additional
  mailers.  An example of this would be to allow the use of mailers
  that do not depend on /bin/mail or to use other transport mechanisms
  such as MIT's Zephyr synchronous communications system.

  Second, completely different transport mechanisms such as video or
  voice-mail will require different routines. However, in these cases,
  one would still want to send book-keeping information (such as the
  user's history list) in a parallel e-mail message.  These routines
  may be reused for that.  (We'll see.)

  This callback is specified by the externally-specifiable (from an
  appdefaults file, eg) GeneralQuestionCallback.

***************************************************************************/

void Mailer_Send_Callback(w,client_data,call_data)
     Widget w;
     XtPointer client_data;
     XtPointer call_data;
{
   XtPointer generic_pointer;
   MailerPassback *mailer_passback;
   char *mail_file;
   int rc;

   mailer_passback = (MailerPassback *)client_data;

   Stat_Button_Hit(w);
   Comm_Initialize_Mailer(mailer_passback,&generic_pointer,&mail_file);
   if (generic_pointer) /* ie, if fp != NULL */
      {
	 Comm_Write_Outgoing_Header(mailer_passback,generic_pointer);
	 Comm_Write_Outgoing_Body(mailer_passback,generic_pointer);
	 Comm_End_Write(mailer_passback,generic_pointer);
	 rc = Comm_Send_Mail(mailer_passback,generic_pointer,mail_file);
      }
   if (rc != NotAbleToSendMail)
      Stat_Sent_Mail(w);  /* for successful mail launch */
   Comm_Destructor(mailer_passback);
 }


/****************************************************************************

  This is a callback to send a text question specified in a popup 
  (from GeneralLostCallback) and send it via e-mail.  It is the
  same as Mailer_Send_Callback except
  that it also sends out a copy of the question (with a reduced
  header) to the user.  It could call Mailer_Send_Callback, except
  for some statistics keeping.

  This callback is specified by the externally-specifiable (from an
  appdefaults file, eg) GeneralQuestionWithCopyCallback.

***************************************************************************/


void Mailer_Send_With_Copy_Callback(w,client_data,call_data)
     Widget w;
     XtPointer client_data;
     XtPointer call_data;
{
   XtPointer generic_pointer;
   MailerPassback *mailer_passback;
   char *mail_file;
   int rc;

   mailer_passback = (MailerPassback *)client_data;

   Stat_Button_Hit(w);
   Comm_Initialize_Mailer(mailer_passback,&generic_pointer,&mail_file);
   if (generic_pointer) /* ie, if fp != NULL */
      {
	 Comm_Write_Copy_Header(mailer_passback,generic_pointer);
	 Comm_Write_Outgoing_Body(mailer_passback,generic_pointer);
	 Comm_End_Write(mailer_passback,generic_pointer);
	 rc = Comm_Send_Copy(mailer_passback,generic_pointer,mail_file);
      }
   if (rc != NotAbleToSendMail)
      Stat_Sent_Mail(w);  /* successful mail launch */
   Comm_Initialize_Mailer(mailer_passback,&generic_pointer,&mail_file);
   if (generic_pointer) /* ie, if fp != NULL */
      {
	 Comm_Write_Outgoing_Header(mailer_passback,generic_pointer);
	 Comm_Write_Outgoing_Body(mailer_passback,generic_pointer);
	 Comm_End_Write(mailer_passback,generic_pointer);
	 rc = Comm_Send_Mail(mailer_passback,generic_pointer,mail_file);
      }
   if (rc != NotAbleToSendMail)
      Stat_Sent_Mail(w);  /* for successful mail launch */
   Comm_Destructor(mailer_passback);
 }


/*******************************************************************

  And, this routine doesn't put any header on the message at all.
  Useful only for sending MH mail and a given form straight out.

*******************************************************************/

void Mailer_Send_Straight_Callback(w,client_data,call_data)
     Widget w;
     XtPointer client_data;
     XtPointer call_data;
{
   XtPointer generic_pointer;
   MailerPassback *mailer_passback;
   char *mail_file;
   int rc;

   mailer_passback = (MailerPassback *)client_data;

   Stat_Button_Hit(w);
   Comm_Initialize_Mailer(mailer_passback,&generic_pointer,&mail_file);
   if (generic_pointer) /* ie, if fp != NULL */
      {
	 Comm_Write_Outgoing_Body(mailer_passback,generic_pointer);
	 Comm_End_Write(mailer_passback,generic_pointer);
	 rc = Comm_Send_Mail(mailer_passback,generic_pointer,mail_file);
      }
   if (rc != NotAbleToSendMail)
      Stat_Sent_Mail(w);  /* for successful mail launch */
   Comm_Destructor(mailer_passback);
 }

MailerPassback *Comm_New()
{
   MailerPassback *mailer_passback;
   if ((mailer_passback =  (MailerPassback *)XtMalloc(sizeof(MailerPassback)))
       == NULL)
      return(NULL);

   mailer_passback->shell = NULL;
   mailer_passback->text = NULL;
   mailer_passback->internal_header = NULL;
   mailer_passback->node_info = NULL;
   return(mailer_passback);
}

MailerPassback *Comm_Initialize_Mailer_Passback(node_info)
  NodeInfoPtr node_info;
{
  MailerPassback *mailer_passback;
   
  if ((mailer_passback = Comm_New()) == NULL)
    {
      XtWarning("unable to allocate memory for mailer: 2");
      return (NULL);
    }
  mailer_passback->node_info = node_info;
  return(mailer_passback);
}



/***************************************************************************

  The site administrator (or someone) has turned off mail for some
  reason (such as not having e-mail or for security reasons).  Tell
  the user.  No questions here; AG is only a static information system.
  (How boring....)

***************************************************************************/

static Comm_Excuse_Me(w)
     Widget w;
{
  Widget shell;
  Widget dialog;
  XtPopdownIDRec rec;
  
  shell = XtVaCreatePopupShell("mailerExcuseShell",topLevelShellWidgetClass,
			       w,
			       XtNinput,(XtArgVal)True,
			       NULL);
  dialog = XtVaCreateManagedWidget("mailerExcuseDialog",dialogWidgetClass,
				   shell,NULL);
  rec.shell_widget = shell;
  rec.enable_widget = w;
  XawDialogAddButton(dialog,"mailerExcuseButton",
		     XtCallbackPopdown,
		     (XtPointer)&rec);
  XtRealizeWidget(shell);
  XtPopup(shell,XtGrabNone);
}



/**************************************************************************

  This routine is the routine that sets up the popup node in which the
  user can enter his question/comment.  

  You can specify changes to the user interface for this question popup
  from the appdefaults file.  

     1.  You can specify a different user instructions.  See the
         current appdefaults file; it specifies the default.
     2.  You can specify text for the user's question.  For example,
         you might want to specify a bug report outline as the
	 text for a bug report node.  (The current appdefaults
	 file has an example of this.)

  Remember that you can customize by node name if you wish or
  for all nodes.  You cannot customize by node type *except* by
  specifying a new callback.  (See the comments below.)  Only the
  questionCallback (as well as some other callbacks) are specifiable
  by node type.  (Don't ask why.)

  If you customize this routine, do not forget to:
     -  Check whether the user is allowed to use mail (the check
        on global_info.use_mail).  This routine pops up a dialog
	box telling the user he can't do anything if the use_mail
	Boolean is set to false in the appdefaults file.
     -  Create the mailer_passback struct dynamically.  You can
        use the Comm_Initialize_Mailer_Passback routine.  This
	struct is required for the various Comm routines.

  Note that this routine uses a Knedit widget to allow bold and italic
  text for instructional text to the user.  It could support buttons and
  all the other features of a Knedit widget as well.  If you decide to
  change the appearance of this popup, check the appdefaults file for
  settings for this Knedit.

***************************************************************************/

Widget Comm_Setup_Question_Node(w,mailer_name,mailform_string,
				node_info,
				cancel_callback,send_callback)
     Widget w;
     char *mailer_name;
     char *mailform_string;
     NodeInfoPtr node_info;
     void (*cancel_callback)();
     void (*send_callback)();
{
    Widget shell;
    Widget text;
    Widget form;
    Widget button1, button2;
    Widget box;
    Widget header_text;
    Widget layout;
    Widget titlebar;
    Arg arglist[10];
    MailerPassback *mailer_passback;


    Stat_Button_Hit(w);

    if (!global_info.use_mail)
      {
	Comm_Excuse_Me(w);
	return(NULL);
      }

    if ((mailer_passback = Comm_Initialize_Mailer_Passback(node_info))
	== NULL)
      return(NULL);

    if (mailer_name == NULL)
      mailer_name = "mailerShell";

    shell = XtVaCreatePopupShell(mailer_name,topLevelShellWidgetClass,
				 w,
				 XtNinput,(XtArgVal)True,
				 XtNallowShellResize, (XtArgVal)True,
				 NULL);

    form = XtCreateManagedWidget("mailerForm",formWidgetClass,shell,
				 NULL,0);

    titlebar = 
	XtVaCreateManagedWidget("mailerKnedit",kneditWidgetClass,form,
				XtNborderWidth, (XtArgVal)0,
				XtNnodeName, (XtArgVal)"noaction",
				XtNfromHoriz, (XtArgVal) NULL,
				XtNfromVert, (XtArgVal) NULL,
				NULL);

    if (mailform_string != NULL)
      text = 
	XtVaCreateManagedWidget("mailerText",asciiTextWidgetClass,
				form,
				XtNtype, (XtArgVal)XawAsciiString,
				XtNstring, (XtArgVal)mailform_string,
				XtNlength, (XtArgVal)AGstrlen(mailform_string),
				XtNscrollVertical, 
  				  (XtArgVal)XawtextScrollAlways,
				XtNeditType, (XtArgVal)XawtextEdit,
				XtNfromVert, (XtArgVal)titlebar,
				NULL);
    else
      text = 
	XtVaCreateManagedWidget("mailerText",asciiTextWidgetClass,
				form,
				XtNtype, (XtArgVal)XawAsciiString,
				XtNscrollVertical, 
  				  (XtArgVal)XawtextScrollAlways,
				XtNeditType, (XtArgVal)XawtextEdit,
				XtNfromVert, (XtArgVal)titlebar,
				NULL);


    mailer_passback->shell = shell;
    mailer_passback->text = text;

    XtSetArg(arglist[0],XtNfromVert,text);
    button1 = XtCreateManagedWidget("mailerButton1",commandWidgetClass,
				    form,arglist,1);
    XtSetArg(arglist[0],XtNfromVert,text);
    XtSetArg(arglist[1],XtNfromHoriz,button1);
    button2 = XtCreateManagedWidget("mailerButton2",commandWidgetClass,
				    form,arglist,2);
    XtAddCallback(button1,XtNcallback,cancel_callback,
		  (XtPointer)mailer_passback);
    XtAddCallback(button2,XtNcallback,send_callback,
		  (XtPointer)mailer_passback);
    XtRealizeWidget(shell);
    XtPopup(shell,XtGrabNone);
    return(shell);
 }

/*****************************************************************************
  
  The two standard question callbacks for the I'm Unhappy buttons.  There
  is one for a single copy to the appropriate expert; the other also
  sends a copy to the user.  

  If you wish to customize the appearance of this question popup in
  AG, you can do it at several levels:

    1.  You can change the instructions to the user in the appdefaults
        file.  See the comments in Comm_Setup_Question_Node.
    2.  You can specify a particular text form (such as a bug report
        or enhancement request) in the appdefaults file.  See the
	comments in Comm_Setup_Question_Node.
    3.  You can write a new callback in the same form as these callbacks.
        You gain four things by doing this.
	  a.  You can specify the name for the mailer popup.  This allows
	      you to specify resources for multiple mailer popups callable
	      from the same AG node.  (For example, you may trigger question
	      callbacks from SBrowser buttons.  Use 
	      @button(some label here, @dynamic(callback_name,params))
	  b.  You can specify a text form (such as a bug report format).
	      There's no great gain to doing this over specifying it in
	      the appdefaults file (#1 or #3a).
	  c.  You can specify a new Cancel callback.  You probably won't
	      want to do this since the Cancel Callback just destroys
	      the popup and deallocates some dynamic storage.
	  d.  You can specify a new Send callback.  This will be the
	      major reason to write a new callback.  If you want to
	      specify a new communications transport engine, this will
	      be the easiest way to do it.  
     4.  You can write a new callback that calls a different popup setup
         routine (ie, not Comm_Setup_Question_Node).  This will give you
	 complete flexibility to redo the user interface for the popup.

  If you write a new callback, I recomment registering a name using 
  Dynamic_Add_Callback in the CommService_Initialize routine below.  This
  allows authors to specify the callback in either the appdefaults file
  or in an @button (ie, in any markup specification that understands the
  @dynamic command).

*****************************************************************************/

void General_Question_Callback(w,client_data,call_data)
     Widget w;
     XtPointer client_data;
     XtPointer call_data;
{
  /* Send out the question to the node expert (with a heavy duty
     header). */
  NodeInfoPtr node_info = (NodeInfoPtr)client_data;
  Comm_Setup_Question_Node(global_info.main_shell,NULL,NULL,node_info,
			     Mailer_Cancel_Callback,Mailer_Send_Callback);

}

void General_Question_With_Copy_Callback(w,client_data,call_data)
     Widget w;
     XtPointer client_data;
     XtPointer call_data;
{
  /* Send out 2 copies, one to the user (without much header) and
     the other to the node expert (with a heavy header). */
  NodeInfoPtr node_info = (NodeInfoPtr)client_data;
  Comm_Setup_Question_Node(global_info.main_shell,NULL,NULL,node_info,
			   Mailer_Cancel_Callback,
			   Mailer_Send_With_Copy_Callback);

}

void General_Question_Straight_Callback(w,client_data,call_data)
     Widget w;
     XtPointer client_data;
     XtPointer call_data;
{
  /* Send out email without any fancy header, just as the user
     entered. */
  NodeInfoPtr node_info = (NodeInfoPtr)client_data;
  Comm_Setup_Question_Node(global_info.main_shell,NULL,NULL,node_info,
			   Mailer_Cancel_Callback,
			   Mailer_Send_Straight_Callback);

}

Boolean CommService_Initialize(shell)
     Widget shell;
{

  /* Add the 3 callbacks above, so they can be registered through
     the appdefaults file */
  Dynamic_Add_Callback("GeneralQuestionCallback",General_Question_Callback);
  Dynamic_Add_Callback("GeneralQuestionWithCopyCallback",
		       General_Question_With_Copy_Callback);
  Dynamic_Add_Callback("GeneralQuestionStraightCallback",
		       General_Question_Straight_Callback);
  return True;
}











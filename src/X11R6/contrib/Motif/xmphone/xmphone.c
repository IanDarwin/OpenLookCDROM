
/* 
 * xmphone.c - xmphone main
 * 
 * Author:	Richard Bingle
 * 		Dept. of Computer Sciences
 * 		Purdue University
 */

/***********************************************************************
* Copyright 1991-1994 by Richard Bingle and Purdue University.  All rights 
* reserved.  Some individual files may be covered by other copyrights.
* 
* Redistribution and use in source and binary forms are permitted
* provided that this entire copyright notice is duplicated in all such
* copies.  Neither the name of the University, nor the name of the author 
* may be used to endorse or promote products derived from this material 
* without specific prior written permission.
*
* THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
* WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
* MERCHANTIBILITY AND FITNESS FOR ANY PARTICULAR PURPOSE.
************************************************************************/

#include "xmphone.h"
#include <sys/wait.h>

void CreateApplication();
Widget CreateHelp();
struct msg_struct *CreateMsgArea();
void CenterManage();
void MakeSep();
void HelpCB();
void QuitCB();
void SendCB();
void ClearCB();
void DoneCB();
Boolean Validate();
String XmStringToString();
void FreeChild();

#define offset(field)	XtOffset(struct resource_struct *, field)

static XtResource Resources[] = {
{"bcc",		"Bcc",		XtRBoolean,	sizeof(Boolean),
	offset(bcc),		XtRString,	"False"},
{"verify",	"Verify",	XtRBoolean,	sizeof(Boolean),
	offset(verify),		XtRString,	"True"},
{"numtogs",     "Numtogs",      XtRInt,         sizeof(int),
        offset(numtogs),        XtRImmediate,   (caddr_t) 7},
{"msgwidth",	"Msgwidth",	XtRInt,		sizeof(int),
	offset(msgwidth),	XtRImmediate,	(caddr_t) MAX_WIDTH},
};

static XrmOptionDescRec Options[] = {
{"-bcc",	"bcc",			XrmoptionNoArg,		"TRUE"},
{"-nobcc",	"bcc",			XrmoptionNoArg,		"FALSE"},
{"-verify",	"verify",		XrmoptionNoArg,		"TRUE"},
{"-noverify",	"verify",		XrmoptionNoArg,		"FALSE"},
};

void main(argc, argv)
int argc;
char **argv;
{
	Widget toplevel;
	Widget main_window;
	XtAppContext app;

	toplevel = XtAppInitialize(&app, "XMphone", Options, XtNumber(Options),
				   &argc, argv, NULL, NULL, (Cardinal) NULL);

	XtGetApplicationResources(toplevel, (caddr_t) &resources,
				  Resources, XtNumber(Resources),
				  NULL, (Cardinal) 0);

	CreateApplication(toplevel);

	XtRealizeWidget(toplevel);

	XtAppMainLoop(app);
}

/*****************************************************************************/
/* CreateApplication(parent) - Creates (either directly or indirectly) the   */
/*                             widgets that make up the user interface       */
/*****************************************************************************/

void CreateApplication(parent)
Widget parent;
{
	Widget main_window;
	Widget menu_bar;
	Widget help;
	Widget file;
	Widget file_menu;
	Widget quit;
	Widget send;
	Widget clear;
	Widget frame;
	Widget swindow;
	struct msg_struct *message;
	Arg wargs[MAX_ARGS];
	int n;

	n = 0;
	main_window = XmCreateMainWindow(parent, "main", wargs, n);
	XtManageChild(main_window);

	n = 0;
	menu_bar = XmCreateMenuBar(main_window, "menu_bar", wargs, n);
	XtManageChild(menu_bar);
 
	n = 0;
	help = XmCreateCascadeButton(menu_bar, "Help", wargs, n);
	XtManageChild(help);
	XtAddCallback(help, XmNactivateCallback, HelpCB, NULL);

	n = 0;
	XtSetArg(wargs[n], XmNmenuHelpWidget, help); n++;
	XtSetValues(menu_bar, wargs, n);

	n = 0;
	file_menu = XmCreatePulldownMenu(menu_bar, "File_Menu", wargs, n);

	n = 0;
	send = XmCreatePushButton(file_menu, "Send", wargs, n);
	XtManageChild(send);

	n = 0;
	clear = XmCreatePushButton(file_menu, "Clear", wargs, n);
	XtManageChild(clear);

	MakeSep(file_menu);

	n = 0;
	quit = XmCreatePushButton(file_menu, "Quit", wargs, n);
	XtManageChild(quit);
	XtAddCallback(quit, XmNactivateCallback, QuitCB, NULL);

	n = 0;
	XtSetArg(wargs[n], XmNsubMenuId, file_menu); n++;
	file = XmCreateCascadeButton(menu_bar, "File", wargs, n);
	XtManageChild(file);
	
	n = 0;
	XtSetArg(wargs[n], XmNshadowType, XmSHADOW_OUT); n++;
	XtSetArg(wargs[n], XmNmarginWidth, 2); n++;
	XtSetArg(wargs[n], XmNmarginHeight, 2); n++;
	frame = XmCreateFrame(main_window, "frame", wargs, n);
	XtManageChild(frame);

	n = 0;
	XtSetArg(wargs[n], XmNscrollingPolicy, XmAPPLICATION_DEFINED); n++;
	XtSetArg(wargs[n], XmNvisualPolicy, XmVARIABLE);  
	swindow = XmCreateScrolledWindow(frame, "swindow", wargs, n);
	XtManageChild(swindow);

	XmMainWindowSetAreas(main_window, menu_bar, NULL, NULL, NULL, frame);

	message = CreateMsgArea(swindow);
	XtAddCallback(send, XmNactivateCallback, SendCB, message);
	XtAddCallback(clear, XmNactivateCallback, ClearCB, message);
}

/*****************************************************************************/
/* CreateMsgArea(w) - Creates the message pad portion of the user interface  */
/*                    and returns a new msg_struct for it                    */
/*****************************************************************************/

struct msg_struct *CreateMsgArea(w)
Widget w;
{
	static Widget form;
	static Widget to;
	static Widget to_text;
	static Widget who;
	static Widget who_text;
	static Widget of;
	static Widget of_text;
	static Widget pn;
	static Widget pn_text;
	static Widget msg;
	static Widget msg_scroll;
	static Widget msg_text;
	static Widget tog;
	static Widget togs[MAX_ARGS];
	Arg wargs[MAX_ARGS];
        char sb[BUFSIZ];
	int n;
	int i;
	static struct msg_struct *ret;

	ret = (struct msg_struct *) XtMalloc(sizeof(struct msg_struct));

	n = 0;
	form = XtCreateManagedWidget("form", xmpTableWidgetClass, w, wargs, n);

	n = 0;
	to = XmCreateLabel(form, "To", wargs, n);
	XtManageChild(to);

	n = 0;
	to_text = XmCreateText(form, "to_text", wargs, n); 
	XtManageChild(to_text);
	ret->to = to_text;

	n = 0;
	who = XmCreateLabel(form, "Who", wargs, n);
	XtManageChild(who);

	n = 0;
	who_text = XmCreateText(form, "who_text", wargs, n);
	XtManageChild(who_text);
	ret->who = who_text;

	n = 0;
	of = XmCreateLabel(form, "Of", wargs, n);
	XtManageChild(of);

	n = 0;
	of_text = XmCreateText(form, "of_text", wargs, n);
	XtManageChild(of_text);
	ret->of = of_text;

	n = 0;
	pn = XmCreateLabel(form, "Phone", wargs, n);
	XtManageChild(pn);

	n = 0;
	pn_text = XmCreateText(form, "pn_text", wargs, n);
	XtManageChild(pn_text);
	ret->pn = pn_text;

	n = 0;
	tog = XtCreateWidget("tog", xmpTableWidgetClass, form, wargs, n);
	XtManageChild(tog);

	for (i=0; i<resources.numtogs; i++) {
	    sprintf(sb, "tog%d", i);
	    n = 0;
	    togs[i] = XmCreateToggleButton(tog, sb, wargs, n);
	    ret->togs[i] = togs[i];
	}
	XtManageChildren(togs, resources.numtogs);

	n = 0;
	msg = XmCreateLabel(form, "Message", wargs, n);
	XtManageChild(msg);

	n = 0;
	msg_scroll = XmCreateScrolledWindow(form, "msg_scroll", wargs, n);
	XtManageChild(msg_scroll);

	n = 0;
	XtSetArg(wargs[n], XmNeditMode, XmMULTI_LINE_EDIT); n++;
	msg_text = XmCreateText(msg_scroll, "msg_text", wargs, n);
	XtManageChild(msg_text);
	ret->msg = msg_text;

	return(ret);
}

/*****************************************************************************/
/* HelpCB(w, client_data, call_data) - Create the help_box (if needed) and   */
/*                                     manage it (to pop it up)              */
/*****************************************************************************/

void HelpCB(w, client_data, call_data)
Widget w;
caddr_t client_data;
caddr_t call_data;
{
	static Widget help_box = NULL;

	if (!help_box)
	    help_box = CreateHelp(w);
	CenterManage(help_box);
}

/*****************************************************************************/
/* CreateHelp(w) - Create the help dialog as a child of w                    */
/*****************************************************************************/

Widget CreateHelp(w)
Widget w;
{
        Widget help_box;
	Arg wargs[MAX_ARGS];
	int n;

        n = 0;
	help_box = XmCreateInformationDialog(w, "help_box", wargs, n);
	XtUnmanageChild(XmMessageBoxGetChild(help_box,XmDIALOG_CANCEL_BUTTON));
	XtUnmanageChild(XmMessageBoxGetChild(help_box,XmDIALOG_HELP_BUTTON));
	return(help_box);
}

/*****************************************************************************/
/* QuitCB(w, client_data, call_data) - Quits the application                 */
/*****************************************************************************/

void QuitCB(w, client_data, call_data)
Widget w;
caddr_t client_data;
caddr_t call_data;
{
	exit(0);
}

/*****************************************************************************/
/* SendCB(w, client_data, call_data) - Formats the message and (possibly)    */
/*                                     sends it                              */
/*****************************************************************************/

void SendCB(w, client_data, call_data)
Widget w;
caddr_t client_data;
caddr_t call_data;
{
	 struct msg_struct *message = (struct msg_struct *) client_data;
	 struct passwd *pass;
	 int i;
	 char filename[BUFSIZ];
	 char line[BUFSIZ];
	 char user[BUFSIZ];
	 char bcc[BUFSIZ];
	 FILE *fp;
	 int pid;
	 int n;
	 Arg wargs[MAX_ARGS];
	 XmString xmsb;
	 char *sb;
	 char *msg;
	 char *tmp;
	 char *nl;
	 char *sp1, *sp2;
	
	 msg = (char *) XtMalloc(BUFSIZ);
	 strcpy(user, "");
	 strcpy(bcc, "");

	 sb = XmTextGetString(message->to);
	 strcpy(user, sb);
	 sprintf(line, "To: %s\n", sb);
	 strcpy(msg, line);
	 XtFree(sb);

	 if (resources.bcc) {
	     pass = getpwuid(getuid());
	     if (pass != NULL)
	         strcpy(bcc, pass->pw_name);
	 }

	 sb = XmTextGetString(message->who);
	 if (strcmp(sb,"")) {
	     sprintf(line, 
               "Subject: ***** Message From %s *****\n\nMessage From %s\n",
	       sb, sb);
	     strcat(msg, line);
	 }
	 else {
	     sprintf(line, "Subject: ***** Message *****\n\n");
	     strcat(msg, line);
	 }
	 XtFree(sb);

	 sb = XmTextGetString(message->of);
	 if (strcmp(sb,"")) {
	     sprintf(line, "Of %s\n", sb);
	     strcat(msg, line);
	 }
	 XtFree(sb);

	 sb = XmTextGetString(message->pn);
	 if (strcmp(sb,"")) {
	     sprintf(line, "Phone %s\n", sb);
	     strcat(msg, line);
	 }
	 XtFree(sb);

	 sprintf(line, "====\n");
	 strcat(msg, line);
	 for (i=0; i<resources.numtogs; i++) {
	     if (XmToggleButtonGetState(message->togs[i])) {
		 n = 0;
		 XtSetArg(wargs[n], XmNlabelString, &xmsb); n++;
		 XtGetValues(message->togs[i], wargs, n);
		 sb = XmStringToString(xmsb);
		 sprintf(line, "%s\n", sb);
		 XtFree(sb);
		 strcat(msg, line);
	     }
	 }
	 sprintf(line, "====\n");
	 strcat(msg, line);

	 sb = XmTextGetString(message->msg);

/* do something here to split long lines - fmt -s isn't standard */

	 sp1 = sb;
	 tmp = sb;
	 nl = 0;
	 while(nl < (sb + strlen(sb))) {
	     nl = index(tmp, '\n');
	     if (((nl - tmp) > resources.msgwidth) || (nl == 0)) {
		 sp1 = 0;
		 if (nl == 0) {
		     if ((int) strlen(tmp) > resources.msgwidth)
			 nl = tmp+resources.msgwidth;
		     else
			 break;
		 }
		 else
		     if (nl - tmp > resources.msgwidth)
			 nl = tmp+resources.msgwidth;
		 while((sp2 = index(tmp, ' ')) < nl) {
		     if (sp2 == 0) {
			 nl = sb + strlen(sb);
			 break;
		     }
		     sp1 = sp2;
		     tmp = ++sp2;
		 }
		 if (sp1 > (char *) 0) {
		     sp1[0] = '\n';
		     tmp = ++sp1;
		 }
	     }
	     else {
		 tmp = ++nl;
	     }
	 }

	 strcpy(filename, "/tmp/,xmphoneXXXXXX");
	 mktemp(filename);
	 fp = fopen(filename, "w");
	 if (!fp) {
	     fprintf(stderr, "error opening %s\n", filename);
	     return;
	 }
	 
	 msg = (char *) strcat(msg, sb);
	 fprintf(fp, "%s\n", msg);

	 fclose(fp);

/* if sending of the message is confirmed, fork off a process to send it */

	 if (Validate(w, msg)) {
	     XtFree(sb);
	     ClearCB(w, client_data, call_data);

	     if ((pid = fork()) == 0) {
		 if (freopen(filename, "r", stdin) == NULL) {
		     fprintf(stderr, "can't reopen for send\n");
		     exit(1);
		 }
		 unlink(filename);
#ifdef UTEK
		 i = execl("/usr/lib/mailers/binmail","binmail", user, bcc, 0);
#else
		 if (strlen(bcc) == 0)
		     i = execl("/bin/mail", "mail", user, 0);
		 else
		     i = execl("/bin/mail", "mail", user, bcc, 0);
#endif
		 if (i == -1) {
		     fprintf(stderr, "big trouble!!\n");
		     exit(1);
		 }
	     }
	     else if (pid < 0) {
		 fprintf(stderr, "error in forking off send\n");
		 return;
	     }
	     XtAppAddTimeOut(XtDisplayToApplicationContext(XtDisplay(w)),
		1*300*1000, FreeChild, NULL);
	 }
	 else {
	     unlink(filename);
	     XtFree(sb);
	 }
}

/*****************************************************************************/
/* Validate(w, msg) - Asks that the message be validated before              */
/*                             sending it                                    */
/*****************************************************************************/

Boolean Validate(w, msg)
Widget w;
char *msg;
{
        static Widget valid;
	Arg wargs[MAX_ARGS];
	int n;
	static int done;
	static Widget rc;
	static Widget text;
	static Widget quest;
	static Widget scroll;

	if (!resources.verify)
	    return(True);

	if (!valid) {
	    n = 0;
	    valid = XmCreatePromptDialog(w, "Valid", wargs, n); 

/* unmanage the buttons we don't want to appear */

	    XtUnmanageChild(XmSelectionBoxGetChild(valid, 
						   XmDIALOG_SELECTION_LABEL));
	    XtUnmanageChild(XmSelectionBoxGetChild(valid,
						   XmDIALOG_TEXT));
	    XtUnmanageChild(XmSelectionBoxGetChild(valid,
						   XmDIALOG_APPLY_BUTTON));
	    XtUnmanageChild(XmSelectionBoxGetChild(valid,
						   XmDIALOG_HELP_BUTTON));

	    XtAddCallback(valid, XmNokCallback, DoneCB, &done);
	    XtAddCallback(valid, XmNcancelCallback, DoneCB, &done); 

	    n = 0;
	    XtSetArg(wargs[n], XmNorientation, XmVERTICAL); n++;
	    XtSetArg(wargs[n], XmNcolumns, 1); n++;
	    rc = XmCreateRowColumn(valid, "rc", wargs, n);
	    XtManageChild(rc);

	    n = 0;
	    quest = XmCreateLabel(rc, "quest", wargs, n);
	    XtManageChild(quest);
	
	    n = 0;
	    scroll = XmCreateScrolledWindow(rc, "scroll", wargs, n);
	    XtManageChild(scroll);
	
	    n = 0;
	    XtSetArg(wargs[n], XmNvalue, msg); n++;
	    XtSetArg(wargs[n], XmNeditable, False); n++;
	    XtSetArg(wargs[n], XmNeditMode, XmMULTI_LINE_EDIT); n++;
	    XtSetArg(wargs[n], XmNcolumns, 80); n++;
	    XtSetArg(wargs[n], XmNrows, 10); n++;
	    text = XmCreateText(scroll, "text", wargs, n);
	    XtManageChild(text);
	}
	else {
	    n = 0;
	    XtSetArg(wargs[n], XmNvalue, msg); n++;
	    XtSetValues(text, wargs, n);
	    XtAddCallback(valid, XmNokCallback, DoneCB, &done);
	    XtAddCallback(valid, XmNcancelCallback, DoneCB, &done); 
	}

	CenterManage(valid);

	done = 0;
	while (!done)
	    XtAppProcessEvent(XtWidgetToApplicationContext(valid), XtIMAll);

	if (done == 1) 
	    return(True);
	else
	    return(False);
}

/*****************************************************************************/
/* CenterManage(w) - Center the dialog in the middle of the screen and then  */
/*                   pop it up by managing it                                */
/*****************************************************************************/

void CenterManage(w)
Widget w;
{
	Arg wargs[MAX_ARGS];
	int n;
	Dimension x, y;
	Dimension width, height;
	Dimension rwidth, rheight;

	if (XtIsManaged(w))
	    return;

	rwidth = WidthOfScreen(XtScreen(w));
	rheight = HeightOfScreen(XtScreen(w));

/* realize the dialog but don't manage it yet - we */
/* want to compute a new position for it first     */

	XtRealizeWidget(w); 

/* compute its new position */

	n = 0;
	XtSetArg(wargs[n], XmNwidth, &width); n++;
	XtSetArg(wargs[n], XmNheight, &height); n++;
	XtGetValues(w, wargs, n);
	x = rwidth/2 - width/2;
	y = rheight/2 - height/2;

/* position it */

	n = 0;
	XtSetArg(wargs[n], XmNx, x); n++;
	XtSetArg(wargs[n], XmNy, y); n++;
	XtSetValues(XtParent(w), wargs, n);

/* manage the dialog now that it has been positioned */

	XtManageChild(w);
}

/*****************************************************************************/
/* ClearCB(w, client_data, call_data) - Clears the template in preparation   */
/*                                      for the next message                 */
/*****************************************************************************/

void ClearCB(w, client_data, call_data)
Widget w;
caddr_t client_data;
caddr_t call_data;
{
        struct msg_struct *message = (struct msg_struct *) client_data;
	int i;
	
	XmTextSetString(message->to, "");
	XmTextSetString(message->who, "");
	XmTextSetString(message->of, "");
	XmTextSetString(message->pn, "");
	XmTextSetString(message->msg, ""); 
	for (i=0; i<resources.numtogs; i++) {
	    XmToggleButtonSetState(message->togs[i], False, False);
	}
}

/*****************************************************************************/
/* MakeSep(parent) - Makes a separator for its parent                        */
/*****************************************************************************/

void MakeSep(parent)
Widget parent;
{
	Arg wargs[MAX_ARGS];
	int n;

	n = 0;
	XtCreateManagedWidget("sep", xmSeparatorWidgetClass, parent, wargs, n);
}

/*****************************************************************************/
/* DoneCB(w, client_data, call_data) - Returns client_data dependent on      */
/*                                     which button was pressed              */
/*****************************************************************************/

void DoneCB(w, client_data, call_data)
Widget w;
caddr_t client_data;
caddr_t call_data;
{
        XmAnyCallbackStruct *cbs = (XmAnyCallbackStruct *) call_data;
	int *done = (int *) client_data;

	switch(cbs->reason) {
	    case XmCR_OK:
	        *done = 1;
		return;
	    case XmCR_CANCEL:
		*done = 2;
		return;
	}
}

/*****************************************************************************/
/* XmStringToString(xmsb) - Returns a String given a XmString                */
/*****************************************************************************/

String XmStringToString(string)
XmString string;
{
   XmStringContext string_context;
   char *text; 
   XmStringCharSet char_set; 
   XmStringDirection string_direction; 
   Boolean separator_flag; 
   char *result_string; 
   Boolean fDone;
   
   XmStringInitContext(&string_context, string);
   
   result_string = NULL; 
   fDone = FALSE;
   
   while (!fDone) {
      if(XmStringGetNextSegment(string_context, &text, &char_set,
				&string_direction, &separator_flag)) {
         if(separator_flag) /* Stop when next segment is a separator */
            fDone = TRUE;
	 
         if ( result_string != NULL ) {
            result_string =
               XtRealloc(result_string, strlen(result_string) +
			 strlen(text) + 2);
            strcat(result_string, text);			
            }
	 else {
            result_string = (char *) XtMalloc(strlen(text) +1);
            strcpy(result_string, text);
            }
         XtFree(text);
         }
      else
         fDone = TRUE;
      }
   
   XmStringFreeContext (string_context);
   return result_string;
}

void FreeChild(client_data, tid)
caddr_t client_data;
XtIntervalId *tid;
{
   int pid;
#if defined(SYSV) || defined(SVR4)
   waitpid(-1, NULL, WNOHANG);
#else
   union wait status;

   pid = wait3(&status, WNOHANG, (struct rusage *) NULL);
#endif
} 

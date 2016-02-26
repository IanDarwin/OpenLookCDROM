/*
 * Copyright (c) 1993 The Regents of the University of Texas System.
 * Copyright (c) 1994 The Regents of the University of Texas System.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted provided
 * that the above copyright notice and this paragraph are duplicated in all
 * such forms and that any documentation, advertising materials,  and other
 * materials  related to such  distribution  and use  acknowledge  that the
 * software  was  developed  by the  University of Texas.  The  name of the
 * University may not be  used to endorse or promote  products derived from
 * this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char copyright[] =
"@(#) Copyright (c) 1993 The Regents of the University of Texas System.\n\
 All rights reserved.\n";
static char rcsid[] =
"$Id: dialog.c,v 1.2 1994/05/20 20:12:12 jones Exp $\n";
static char rcspath[] =
"$Source: /mintaka/u0/xx/ss/jones/jones.old/mftp.src/moxftp-2.0/RCS/dialog.c,v $\n";
#endif

/* $Log: dialog.c,v $
 * Revision 1.2  1994/05/20  20:12:12  jones
 * Glob should select files not unselect them.
 *
 * Revision 1.1  1994/03/14  18:55:53  jones
 * Initial revision
 *
 */

#include "machine.h"
#include "defs.h"
#include "wc.h"
#include "proto/dialog.h"

extern Widget Top;
extern XtAppContext  App_context;

/*
 * Local definitions.
 */
static CB  *c_cb   = NULL;

static Widget Continue_Dialog_W = NULL;
static Widget Glob_Dialog_W = NULL;
static Widget Search_Dialog_W = NULL;
static Widget Search_Host_Dialog_W = NULL;

#define GLOB_GLOB   0
#define GLOB_REG    1
#define GLOB_CANCEL 2
#define GLOB_HELP   3

#define SEARCH_GLOB   0
#define SEARCH_REG    1
#define SEARCH_CANCEL 2
#define SEARCH_HELP   3

#define SEARCH_HOST_REG    0
#define SEARCH_HOST_CANCEL 1
#define SEARCH_HOST_HELP   2

#define CONTINUE_IGNORE    0
#define CONTINUE_CONTINUE  1
#define CONTINUE_CANCEL    2
#define CONTINUE_HELP      3

static void
CreateContinueDialog_cb(w, closure, call_data)	
Widget w;
XtPointer closure;
XtPointer call_data;
{
    Continue_Dialog_W = CreateSimpleDialog(w,
				          "Shellcontinuedialog",
				          "Temp",
					  0,
				          "Ignore Errors", CONTINUE_IGNORE, 
				          "Continue"     , CONTINUE_CONTINUE, 
				          "Cancel"       , CONTINUE_CANCEL,
				          "Help"         , CONTINUE_HELP,
				          continue_cb);
}


static void
CreateGlobDialog_cb(w, closure, call_data)	
Widget w;
XtPointer closure;
XtPointer call_data;
{

    if (Glob_Dialog_W != NULL) return;
    Glob_Dialog_W = CreateSimpleDialog(w,
				       "Shellglobdialog",
				       "Enter String:",
				       1,
				       "Glob",         GLOB_GLOB, 
				       "Regular exp" , GLOB_REG, 
				       "Cancel",       GLOB_CANCEL,
				       "Help"  ,       GLOB_HELP,
				        glob_cb);
}


static void
CreateSearchHostDialog_cb(w, closure, call_data)	
Widget w;
XtPointer closure;
XtPointer call_data;
{

    if (Search_Host_Dialog_W != NULL) return;
    Search_Host_Dialog_W = CreateSimpleDialog(w,
				        "Shellsearchhostdialog",
				        "Enter Search String:",
				        1,
				        "Search"       , SEARCH_HOST_REG,
				        "Cancel"       , SEARCH_HOST_CANCEL,
					 NULL	       , 0,
				        "Help"         , SEARCH_HOST_HELP,
				         search_host_cb);
}

static void
CreateSearchDialog_cb(w, closure, call_data)	
Widget w;
XtPointer closure;
XtPointer call_data;
{

    if (Search_Dialog_W != NULL) return;
    Search_Dialog_W = CreateSimpleDialog(w,
				        "Shellsearchdialog",
				        "Enter Search String:",
				        1,
				        "Glob"         , SEARCH_GLOB, 
				        "Regular exp"  , SEARCH_REG,
				        "Cancel"       , SEARCH_CANCEL,
				        "Help"         , SEARCH_HELP,
				         search_cb);
}

void
Continue_Dialog(label, cb)
char *label;
CB   *cb;
{

     c_cb   = cb;

     if (label != NULL) {
          SetDialogLabel(Continue_Dialog_W, label);
     } else {
          SetDialogLabel(Continue_Dialog_W, "Error");
     }
     PopDialog(Continue_Dialog_W, "*Shellcontinuedialog");
}

void
Search_Dialog()
{
     PopDialog(Search_Dialog_W, "*Shellsearchdialog");
}

void
Search_Host_Dialog()
{
     PopDialog(Search_Host_Dialog_W, "*Shellsearchhostdialog");
}

static void
PopDialog(w, name)
Widget w;
char *name;
{
#if defined(MOTIF)
     XtManageChild(w);
#else
     Position_Dialog_cb(Top, name, NULL);
     XtPopup(w, XtGrabNone);
#endif
}

void
Dialog_Restart(type, cb)
int type;
CB *cb;
{
    CB *cbp;
    extern int aborting;

    if (type < 0) type = CONTINUE_CONTINUE;
    if (type == DIALOG_RESTART_IGNORE) type = CONTINUE_IGNORE;
    if (type == DIALOG_RESTART_CONTINUE) type = CONTINUE_CONTINUE;
    aborting = 0;
    WcSetInsensitiveCB(Top, "*reconnect", NULL);
    cbp = link_callback(NULL, restart_cb, (DATA)type, cb);
    Start_Reconnect(cbp);
}

void
Reset_Search_String()
{
     Reset_D_String(Search_Dialog_W);
     Reset_D_String(Glob_Dialog_W);
}

void
Reset_Host_Search_String()
{
     Reset_D_String(Search_Host_Dialog_W);
}


static void
restart_cb(data, code, cb)
DATA  data;
int   code;
CB   *cb;
{
    int type = (int)data;

    if (ftp_response[code].status & FTP_ERROR) {
     	    Clean_Up(0, cb);
	    return;
    } else {
        switch (type) {
	    case CONTINUE_CANCEL:
     	    	Clean_Up(0, cb);
	    	break;
	    case CONTINUE_CONTINUE:
            	Restart_List(cb, 0);
	    	break;
	    case CONTINUE_IGNORE:
            	Restart_List(cb, 1);
		break;
	}
    }
}

static void
continue_cb(w, closure, call_data)
Widget w;
XtPointer closure;
XtPointer call_data;
{
    int type = (int)closure;
    CB *cbp;
    extern int aborting;


#if defined(MOTIF)
    XtUnmanageChild(Continue_Dialog_W);
#endif
#if defined(XAW)
    XtPopdown(Continue_Dialog_W);
#endif
    /*
     * This will mostly work.
     */
    if (connected == 0 && type != CONTINUE_CANCEL) {
	Dialog_Restart(type, c_cb);
	return;
    }

    switch (type) {
	case CONTINUE_CANCEL:
     	    Clean_Up(0, c_cb);
	    break;
	case CONTINUE_CONTINUE:
            Restart_List(c_cb, 0);
	    break;
	case CONTINUE_IGNORE:
            Restart_List(c_cb, 1);
    }

}

static void
glob_cb(w, closure, call_data)
Widget w;
XtPointer closure;
XtPointer call_data;
{
    char *glob;
    char *GetText();
    int type = (int)closure;

#if defined(MOTIF)
    XtUnmanageChild(Glob_Dialog_W);
#endif
#if defined(XAW)
    XtPopdown(Glob_Dialog_W);
#endif

    glob = GetDialogText(Glob_Dialog_W);

    switch (type) {
	case GLOB_GLOB:
            glob = makeglob(glob);
	    Glob_Files(glob, 1);
	    break;
	case GLOB_REG:
	    Glob_Files(glob, 1);
	    break;
    }
    XtFree(glob);
}

static char *
makeglob(reg)
char *reg;
{
    char *cp1, *cp2;
    char *glob;

    if (!reg || !*reg) return NULL;

    cp1 = reg;
    cp2 = glob = XtMalloc(2*strlen(reg)+2+1);
    *cp2 = '^';
    *cp2++;

    while (*cp1) {
	switch (*cp1) {
	    case '*': 
		*cp2 = '.';
		cp2++;
		*cp2 = '*';
		cp2++;
		break;
	    case '?':
		*cp2 = '.';
		cp2++;
		break;

	    case '^':
	    case '$':
		*cp2 = '\\';
		cp2++;
		*cp2 = *cp1;
		cp2++;
		break;

	    default:
		*cp2 = *cp1;
		cp2++;
		break;
	}
	cp1++;
    }
    *cp2 = '$';
    *cp2++;
    *cp2 = '\0';

    XtFree(reg);
    return glob;

}


static void
search_clear_act(w, event, params, nparams)
Widget          w;
XEvent         *event;
String         *params;
Cardinal       *nparams;
{
    Reset_Search();
}

static void
search_host_cb(w, closure, call_data)
Widget w;
XtPointer closure;
XtPointer call_data;
{
    char *search;
    char *GetText();
    int type = (int)closure;

#if defined(MOTIF)
    XtUnmanageChild(Search_Host_Dialog_W);
#endif
#if defined(XAW)
    XtPopdown(Search_Host_Dialog_W);
#endif

    search = GetDialogText(Search_Host_Dialog_W);

    switch (type) {
	case SEARCH_HOST_REG:
	    Search_Host(search);
	    break;
    }
    if(search) XtFree(search);
}

static void
search_cb(w, closure, call_data)
Widget w;
XtPointer closure;
XtPointer call_data;
{
    char *search;
    char *GetText();
    int type = (int)closure;

#if defined(MOTIF)
    XtUnmanageChild(Search_Dialog_W);
#endif
#if defined(XAW)
    XtPopdown(Search_Dialog_W);
#endif

    search = GetDialogText(Search_Dialog_W);

    switch (type) {
	case SEARCH_GLOB:
	    search = makeglob(search);
            Search_Files(search, 0);
	    break;
	case SEARCH_REG:
            Search_Files(search, 0);
	    break;
    }
    if(search) XtFree(search);
}

static void
set_glob_text_act(w, event, params, nparams)
Widget          w;
XEvent         *event;
String         *params;
Cardinal       *nparams;
{
    char *glob;
    char *GetText();

    glob = GetDialogText(Glob_Dialog_W);

    if (params && *params) {
	if (strcmp("glob", *params) == 0) {
            glob = makeglob(glob);
	}

    }
    if (glob) {
        Glob_Files(glob, 1);
        XtFree(glob);
    }
}

static void
set_search_host_act(w, event, params, nparams)
Widget          w;
XEvent         *event;
String         *params;
Cardinal       *nparams;
{
    char *search;
    char *GetText();

    search = GetDialogText(Search_Host_Dialog_W);

    if (params && *params) {
	if (strcmp("glob", *params) == 0) {
	    search = makeglob(search);
	} else if (strcmp("clear", *params) == 0) {
	    printf("Clearing\n");
	}

    }
    Search_Host(search);
    if (search) XtFree(search);
}

static void
set_search_text_act(w, event, params, nparams)
Widget          w;
XEvent         *event;
String         *params;
Cardinal       *nparams;
{
    char *search;
    char *GetText();

    search = GetDialogText(Search_Dialog_W);

    if (params && *params) {
	if (strcmp("glob", *params) == 0) {
	    search = makeglob(search);
	}

    }
    Search_Files(search, 0);
    if (search) XtFree(search);
}

static XtActionsRec Actions[] = {
    {"set_search_text", set_search_text_act},
    {"set_search_host", set_search_host_act},
    {"set_glob_text",   set_glob_text_act},
    {"search_clear"   , search_clear_act}
};

int
Register_dialog_c_a_a()
{
     XtAppAddActions(App_context, Actions,
                        sizeof(Actions)/sizeof(XtActionsRec));
    RCALL("CreateGlobDialog"       , CreateGlobDialog_cb);
    RCALL("CreateSearchDialog"     , CreateSearchDialog_cb);
    RCALL("CreateHostSearchDialog" , CreateSearchHostDialog_cb);
    RCALL("CreateContinueDialog"   , CreateContinueDialog_cb);
}

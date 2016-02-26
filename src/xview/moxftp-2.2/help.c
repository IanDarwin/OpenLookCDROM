/*
 * Copyright (c) 1992 The Regents of the University of Texas System.
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
"$Id: help.c,v 1.1 1994/03/14 18:55:53 jones Exp $\n";
static char rcspath[] =
"$Source: /mintaka/u0/xx/ss/jones/jones.old/mftp.src/moxftp-2.0/RCS/help.c,v $\n";
#endif

/* $Log: help.c,v $
 * Revision 1.1  1994/03/14  18:55:53  jones
 * Initial revision
 *
 */

#include "machine.h"
#include "defs.h"
#include "class.h"
#if defined(MOTIF)
#include <Xm/Xm.h>
#endif

#define HELP_TEXT_WINDOW "*help_text"

#include <X11/Xlib.h>
#include "wc.h"
#include "proto/help.h"

extern XtAppContext  App_context;
extern Widget Top;
extern char *Ftpname;
static int once = 1;
static void Set_Help_Title_cb();
Widget Help_Shell  = NULL;
Widget Help_w     = NULL;
Widget Help_title = NULL;

char *title_name[7] = {
#define T_GENERAL  0
    "General", 
#define T_SHELL    1
    "Shells", 
#define T_DIALOG   2    
    "Dialogs", 
#define T_BUTTON   3    
    "Buttons", 
#define T_WINODW   4
    "Window", 
#define T_MENU     5 
    "Menu", 
#define T_SUBMENU  6 
    "Submenus"
};


typedef struct _help_t {
	String help;
} help_t, *help_t_p; 

char *help_general = GENERAL_HELP;
static char *tran  = NULL;

help_t help_res;

XtResource r_help[] =
{ 
    { "help_text", "Help_text", XtRString, sizeof(String), 
       XtOffset(help_t_p, help), XtRString, (caddr_t)NULL },
};

char *help_text = NULL;
char *help_title = NULL;

#define MAX_HELP_MENU 100
struct _help_menu {
	char  *title;
	char   *name;
	Widget w;
} help_menu[MAX_HELP_MENU];
int NUM_HELP_MENU = 0;
static MENU_ONCE = 0;

void
Create_Help_Menu()
{ 
    int max;
    int last;
    int current;
    Widget w, parent, child;
#if defined(XAW)
    extern Widget CreateHelpPopupChild();
#endif
#if defined(OPENWINDOW)
    Widget pane;
#else
    Widget menu;
#endif

    if (MENU_ONCE) return;

#if defined(OPENWINDOW)
    w = WcFullNameToWidget(Top, "*helpmenu*pane"); 
#else
    w = WcFullNameToWidget(Top, "*helpmenu"); 
#endif

    qsort((void *)help_menu, NUM_HELP_MENU, sizeof(struct _help_menu), 
          compare_menu );
    MENU_ONCE++;
    max = NUM_HELP_MENU;
    last = title_type(help_menu[0].title);
#if defined(OPENWINDOW)
    parent = WcCreateChild(w, "help_sub");
    pane = WcFullNameToWidget(parent, "*pane"); 
    XtManageChild(parent);
    XtManageChild(pane);
#endif
#if defined(MOTIF)
    parent = WcCreateChild(w, "help_sub");
    menu   = WcCreateChild(w, "help_menu");
    XtVaSetValues(parent, XmNsubMenuId, menu, NULL);
    XtManageChild(parent);
#endif
#if defined(XAW)
    parent = WcCreateChild(w, "help_sub");
    menu   = CreateHelpPopupChild(w, "help_menu");
    Bind_Menu(parent, NULL, menu);
    XtManageChild(parent);
#endif
    SetLabel(parent, title_name[last]);
    for (NUM_HELP_MENU=0; NUM_HELP_MENU < max; NUM_HELP_MENU++)  {
        current  = title_type(help_menu[NUM_HELP_MENU].title);
	if (last != current) {
            last = current;
#if defined(OPENWINDOW)
            parent = WcCreateChild(w, "help_sub");
            pane = WcFullNameToWidget(parent, "*pane"); 
    	    XtManageChild(pane);
#endif
#if defined(MOTIF)
    	    parent = WcCreateChild(w, "help_sub");
    	    menu   = WcCreateChild(w, "help_menu");
            XtVaSetValues(parent, XmNsubMenuId, menu, NULL);
#endif
#if defined(XAW)
    	    parent = WcCreateChild(w, "help_sub");
            menu   = CreateHelpPopupChild(w, "help_menu");
            Bind_Menu(parent, NULL, menu);
#endif
            XtManageChild(parent);
            SetLabel(parent, title_name[last]);
	}
#if defined(OPENWINDOW)
    	child =  WcCreateChild(pane, "helpmenu1");
        Set_Help_Title_cb(child, NULL, NULL);
#else
    	child =  WcCreateChild(menu, "helpmenu1");
        Set_Help_Title_cb(child, NULL, NULL);
#endif
        XtManageChild(child);
    }
}

void
Help(w, string, flag)
Widget  w;
char    *string;
int     flag;
{
    char * WcWidgetToFullName();
    char *name;
    char *start, *end;
    char *cp;
  
    if (string) {
	name = start = XtNewString(string);
    } else {
	name = WcWidgetToFullName(w);
	start = RINDEX(name, '.');
    	if (start == NULL) {
	    start = name;
	} else {
	    start++;
	}
    }

    help_res.help = NULL;
    XtGetSubresources(Top, (XtPointer)&help_res, 
		      start, start, r_help, XtNumber(r_help),
		      NULL, 0);
    /*
     * Walk up the name tree.
     */
    while (help_res.help == NULL)  {
	cp = RINDEX(name, '.');
	if (cp) {
       	    *cp = '\0';
 	} else {
	    break;
	}
	start = RINDEX(name, '.');
    	if (start == NULL) {
	    start = name;
	} else {
	    start++;
	}
        XtGetSubresources(Top, (XtPointer)&help_res, start, start, r_help, 
			  XtNumber(r_help), NULL, 0);
    }
    if (help_res.help == NULL)  {
        XtGetSubresources(Top, (XtPointer)&help_res, help_general, 
			  help_general, r_help, XtNumber(r_help), NULL, 0);
    }
    if (help_res.help == NULL) return;
    if (help_text)  XtFree(help_text);
    if (help_title) XtFree(help_title);
    start = help_res.help;
    while (start = bsdstrstr(start, "XXXX")) {
	strncpy(start, Ftpname, 4);
	start += 4;
    }
    start = help_res.help;
    end = INDEX(help_res.help, '\n');
    if (end) {
	int n;
	n = end - start;
	help_title = XtMalloc(n+1);
	strncpy(help_title, start, n);
	help_title[n] = '\0';
	start = end+1;
    } else {
	help_title = XtNewString(" ");
    }
    help_text = XtNewString(start);
    if (flag) return;
    help_once(w);
}

static void
Help_Once_action(w, event, params, nparams)
Widget          w;
XEvent         *event;
String         *params;
Cardinal       *nparams;
{
    help_once(w);
}

static void
Help_Once_cb(w, stuff, unused)
Widget  w;
char    *stuff;
caddr_t unused;
{
    help_once(w);

}

static void
Help_by_title_cb(w, stuff, unused)
Widget  w;
char    *stuff;
caddr_t unused;
{
    char *cp;
    int   i;

    for (i=0; i<NUM_HELP_MENU; i++) {
	if (help_menu[i].w == w) {
      	    Help(w, help_menu[i].name, 0);
	    return;
	}
    }
    if (XtIsWidget(w))
	fprintf(stderr, "Could not find widget %s\n", w->core.name);
}

int
Help_dead_cb(w, stuff, unused)
Widget  w;
char    *stuff;
caddr_t unused;
{
     Help_Shell   = NULL;
     Help_w      = NULL;
     Help_title = NULL;
}

static void
Help_register_cb(w, stuff, unused)
Widget  w;
char    *stuff;
caddr_t unused;
{
     char *name, *start;
     char *cp;
     int i;
     char *WcWidgetToFullName();

     if (stuff && *stuff && *stuff != ' ') {
	cp = stuff;
     } else {
        name = WcWidgetToFullName(w);
	start = RINDEX(name, '.');
    	if (start == NULL) {
	    start = name;
	} else {
	    start++;
	}
	cp = start;
     }
     Help(w, cp, 1);
     for (i=0; i<NUM_HELP_MENU;i++) {
	if (strcmp(help_menu[i].title, help_title) == 0) return;
     }
     help_menu[NUM_HELP_MENU].name = XtNewString(cp);
     help_menu[NUM_HELP_MENU].title = XtNewString(help_title);
     NUM_HELP_MENU++;

}

void
Register_help_CallBacks_and_Actions()
{
     RACCT("set_help"	        , Set_help_action);
     RACCT("help_once"		, Help_Once_action);
     RCALL("help_register"	, Help_register_cb);
     RCALL("help_dead"	        , Help_dead_cb);
     RCALL("help_by_title"	, Help_by_title_cb);
     RCALL("set_help"		, Set_help_cb);
     RCALL("SetHelpTitle"	, Set_Help_Title_cb);
     RCALL("help_once"		, Help_Once_cb);
}

static void
Set_Help_Title_cb(w, stuff, unused)
Widget  w;
char    *stuff;
caddr_t unused;
{
    SetLabel(w, help_menu[NUM_HELP_MENU].title);
    help_menu[NUM_HELP_MENU].w = w;
}

static void
Set_help_action(w, event, params, nparams)
Widget          w;
XEvent         *event;
String         *params;
Cardinal       *nparams;
{
    set_help_text(w);
}

static void
Set_help_cb(w, stuff, unused)
Widget  w;
char    *stuff;
caddr_t unused;
{
    set_help_text(w);
}

static void
clear_help(w, stuff, xxx)
Widget w;
char   *stuff;
char   *xxx;
{
#if defined(XAW)||defined(OPENWINDOW)
    XtPopdown(Help_Shell);
#endif
}

static int
compare_menu(x,y)
void *x;
void *y;
{
    struct _help_menu *a = x;
    struct _help_menu *b = y;
    int   avalue;
    int   bvalue;
    int   n;

    avalue  = title_type(a->title);
    bvalue  = title_type(b->title);

    if (avalue > bvalue) return  1;
    if (avalue < bvalue) return -1;
    n = strcmp(a->title, b->title);
    if (n > 0) return  1;
    if (n < 0) return -1;
    return 0;
}

static void
help_once(w)
Widget w;
{
    XWMHints wmhints;

    if (!Help_Shell) {
        Help_Shell = WcFullNameToWidget(Top, "*Shellhelp"); 
        if (!Help_Shell) {
	    WcCreateNamedPopups ( Top, "Shellhelp" );
        }
        Help_Shell = WcFullNameToWidget(Top, "*Shellhelp"); 
	if (!Help_Shell) return;
    }
    Create_Help_Menu();
    XtPopup(Help_Shell, XtGrabNone);
    XRaiseWindow(XtDisplay(Help_Shell), XtWindow(Help_Shell));
    WM_DELETE(Help_Shell, clear_help, "help");
    Set_Icon(Help_Shell, "help");
    SetWidget(Help_w, HELP_TEXT_WINDOW, 0);
    set_help_text(Help_w);
}

static void
set_help_text(w)
Widget w;
{
    if (!Help_Shell) return;
    SetWidget(Help_w, HELP_TEXT_WINDOW, 0);
    if (help_text) {
	set_the_help_text(Help_w, help_text);
    } else {
	set_the_help_text(Help_w, "No help!");
    }
    if(!help_title) return;
    SetWidget(Help_title, "*help_title", 0);
    SetLabel(Help_title, help_title);
}

static void
set_the_help_text(w, text)
Widget w;
char   *text;
{
#if defined(OPENWINDOW)
   SetStringSource(w, text);
#else
   SetText(w, text);
#endif
}

static int
title_type(string)
char *string;
{
    int type;
    char *mystring();

    if (bsdstrstr(string, "SubMenu") != NULL) {
	type = T_SUBMENU;
    } else if (bsdstrstr(string, "Menu") != NULL) {
	type = T_MENU;
    } else if (bsdstrstr(string, "Window") != NULL) {
	type = T_WINODW;
    } else if (bsdstrstr(string, "Button") != NULL) {
	type = T_BUTTON;
    } else if (bsdstrstr(string, "Dialog") != NULL) {
	type = T_DIALOG;	
    } else if (bsdstrstr(string, "Shell") != NULL) {
	type = T_SHELL;	
    } else {
       	type = T_GENERAL ;
    }
    return type;
} 

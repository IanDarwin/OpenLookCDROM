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
"$Id: noop.c,v 1.2 1994/05/20 20:19:10 jones Exp $\n";
static char rcspath[] =
"$Source: /mintaka/u0/xx/ss/jones/jones.old/mftp.src/moxftp-2.0/RCS/noop.c,v $\n";
#endif

/* $Log: noop.c,v $
 * Revision 1.2  1994/05/20  20:19:10  jones
 * Do not depened on X11R5 NULL XtGetValues trick.
 *
 * Revision 1.1  1994/03/14  18:55:53  jones
 * Initial revision
 *
 */

#include "defs.h"
#include "wc.h"
#include <X11/Xlib.h>
#include <X11/cursorfont.h>
#include <ctype.h>

#if defined(MOTIF)
#include <Xm/Xm.h>
#include <Xm/Text.h>
#endif
#if defined(OPENWINDOW)
#include <Xol/OpenLook.h>
#endif

extern XtAppContext  App_context;
extern Widget        Top;

static int rootwidth = 0;
static int rootheight = 0;
static Cursor Busy = 0;

static struct _noop_flags {
	char *action;
	int  flag;
} noop_flags[] = {
    { "get",          NOOP_GET},
    { "put",          NOOP_PUT},
    { "dir",          NOOP_DIR},
    { "action",       NOOP_ACTION},
    { "connect",      NOOP_CONNECT},
    { "Sensitive",    NOOP_SENSITIVE},
    { "ifsensitive",  NOOP_IFSENSITIVE},
    { "notconnected", NOOP_NOTCONN}
};
    
static struct _noop {
     Widget w;
     Window Busy;
#if defined(MOTIF)||defined(OPENWINDOW)
     int    has_focus;
#endif
     int    flags;
     int    noop;
}  noop[20];
static int NUM_NOOP = 0;

#include "proto/noop.h"

void
Clear_Noop(mode)
int mode;
{
    int i;
    for(i=0; i<NUM_NOOP; i++) {
	if ((NOOP_WAITING&noop[i].flags) && (mode&noop[i].noop) && 
	    noop[i].w) {
	    noop[i].noop &= ~mode;
	    if(noop[i].noop) continue;
 	    if (XtIsRealized(noop[i].w) && noop[i].Busy) 
	      XUnmapWindow(XtDisplay(Top), noop[i].Busy);
	    if(noop[i].flags & NOOP_SENSITIVE) 
	      WcSetSensitiveCB(noop[i].w, "this", NULL);
#if defined(MOTIF)
	    else { 
	        XtVaSetValues(noop[i].w, XmNtraversalOn, TRUE, NULL);
	    }
  	    if (noop[i].has_focus)  {
		XmProcessTraversal(noop[i].w, XmTRAVERSE_CURRENT);
	    }
	    noop[i].has_focus = 0;
#endif
#if defined(OPENWINDOW)
            if (noop[i].has_focus) XSetInputFocus(XtDisplay(Top),
                                                  XtWindow(noop[i].w),
                                             	  RevertToNone,
                                             	  CurrentTime);

	    noop[i].has_focus = 0;
#endif
	    noop[i].flags &= ~NOOP_WAITING;
	
	}
    }
}

int 
Is_Noop(w)
Widget w;
{
    int i;

    for(i=0; i<NUM_NOOP; i++) {
       if (noop[i].w == w && (noop[i].flags & NOOP_WAITING)) return 1;
    }
    return 0;
}

void
Register_Noop_CallBacks_and_Actions()
{
     RACCT("Clear_noop"         , clear_noop_act);

     RCALL("Clear_noop"         , clear_noop_cb);
     RCALL("noop"               , noop_cb);
     RCALL("Set_noop"           , set_noop_cb);
}

void
Set_Noop(mode)
int mode;
{  
    int i;
    unsigned long               valuemask;
    XSetWindowAttributes        attributes;
    int hasfocus = 0;

    if(!Busy)  {
      	Busy = XCreateFontCursor (XtDisplay(Top), XC_watch);
#if defined(sgi)&&defined(MOTIF)
       	for(i=0; i<NUM_NOOP; i++) {
       	    if(noop[i].w == WcFullNameToWidget(Top, "*ftp")) {
	     	noop[i].flags |= NOOP_SENSITIVE;
		break;
	    }
	}
#endif
    }

    if(!rootwidth) {
        rootwidth = WidthOfScreen(XtScreen(Top));
        rootheight = HeightOfScreen(XtScreen(Top));
    }

    for(i=0; i<NUM_NOOP; i++) {
	if ((mode&noop[i].flags) && noop[i].w) {
	    if (!noop[i].noop && !XtIsSensitive(noop[i].w) &&
		(noop[i].flags&NOOP_IFSENSITIVE)) continue;
	    noop[i].noop |= mode;
	    if (noop[i].flags & NOOP_WAITING) continue;
 	    if (XtIsRealized(noop[i].w) && !noop[i].Busy) {
	        valuemask = CWDontPropagate | CWCursor;
    	      	attributes.do_not_propagate_mask =  
					 (KeyPressMask | KeyReleaseMask |
                                         ButtonPressMask | ButtonReleaseMask |
                                         Button1MotionMask|
				         Button2MotionMask|Button3MotionMask);
    	        attributes.cursor = Busy;
		noop[i].Busy =
		        XCreateWindow(XtDisplay(noop[i].w), 
				      XtWindow(noop[i].w), 0, 0,
                                      rootwidth, rootheight, 
				      (unsigned int) 0, CopyFromParent,
                      	              InputOnly, CopyFromParent, valuemask, 
				      &attributes);
	    }
   	    if(noop[i].Busy) {
#if defined(MOTIF)||defined(OPENWINDOW)
		if (hasfocus = Has_Focus(noop[i].w)) {
		    int j;

		    for(j=0; j<NUM_NOOP; j++) if (noop[j].has_focus) break;
	            if (j == NUM_NOOP) noop[i].has_focus = 1;
		}
#endif
		XMapWindow(XtDisplay(Top), noop[i].Busy);
#if defined(OPENWINDOW)
		if (hasfocus) XSetInputFocus(XtDisplay(Top), 
					     None, 
					     RevertToNone, 
			                     CurrentTime);
#endif
	    }
	    
	    if(noop[i].flags & NOOP_SENSITIVE) {
		WcSetInsensitiveCB(noop[i].w, "this", NULL);
	    }
#if defined(MOTIF)
	    else {
		/*
		 * If this widget has focus move
		 * focus.  Older motifs will core dump if you
		 * turn off XmNtraversalOn and the widget has focus.
		 */
		if (hasfocus) {
		    XtSetKeyboardFocus(Top, None);
		} 
	        XtVaSetValues(noop[i].w, XmNtraversalOn, FALSE, NULL);
	    }
#endif
	    noop[i].flags |= NOOP_WAITING;
	}
    }

    XSync(XtDisplay(Top), FALSE);
    while(XtAppPending(App_context) & XtIMXEvent)  
      XtAppProcessEvent(App_context, XtIMXEvent);

}

static void
clear_noop_act(w, event, params, num_params)
Widget w;
XEvent * event;
String * params;
Cardinal * num_params;
{
    if(*num_params == 1) clear_noop_xx(w, params[0]);
}

static void
clear_noop_cb(w, stuff, unused)
Widget  w;
char    *stuff;
caddr_t unused;
{
    clear_noop_xx(w, stuff);
}

static void
clear_noop_xx(w, stuff)
Widget  w;
char    *stuff;
{
    char *token;
    char *cp = stuff;
    int i;

    token = XtMalloc(strlen(cp)+1);
    while (cp && *cp) {
        cp = Next_Token(cp, token);
	for (i=0; i<(sizeof(noop_flags)/sizeof(struct _noop_flags)); i++) {
	    if (strcmp(noop_flags[i].action, token) == 0) {
		Clear_Noop(noop_flags[i].flag);
		break;
	    }
        }
    }
    XtFree(token);
}

static void
noop_cb(w, stuff, unused)
Widget  w;
char    *stuff;
caddr_t unused;
{
    char *cp = stuff;
    char *token;
    int i;

    if (NUM_NOOP >= (sizeof(noop)/sizeof(struct _noop))) {
	fprintf(stderr,"Too many widgets to noop %d\n", NUM_NOOP);
	return;
    }
    noop[NUM_NOOP].w = w;
    token = XtMalloc(strlen(cp)+1);
    while (cp && *cp) {
        cp = Next_Token(cp, token);
	for (i=0; i<(sizeof(noop_flags)/sizeof(struct _noop_flags)); i++) {
	    if (strcmp(noop_flags[i].action, token) == 0) {
		noop[NUM_NOOP].flags |= noop_flags[i].flag;
		break;
	    }
        }
    }
    XtFree(token);
    NUM_NOOP++;
}

static void
set_noop_cb(w, stuff, unused)
Widget  w;
char    *stuff;
caddr_t unused;
{
    char *token;
    char *cp = stuff;
    int i;

    token = XtMalloc(strlen(cp)+1);
    while (cp && *cp) {
        cp = Next_Token(cp, token);
	for (i=0; i<(sizeof(noop_flags)/sizeof(struct _noop_flags)); i++) {
	    if (strcmp(noop_flags[i].action, token) == 0) {
		Set_Noop(noop_flags[i].flag);
		break;
	    }
        }
    }
    XtFree(token);
}

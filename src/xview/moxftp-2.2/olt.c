/*
 * Copyright (c) 1992	 The Regents of the University of Texas System.
 * Copyright (c) 1993	 The Regents of the University of Texas System.
 * Copyright (c) 1994	 The Regents of the University of Texas System.
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
"$Id: olt.c,v 1.1 1994/03/14 18:55:53 jones Exp $\n";
static char rcspath[] =
"$Source: /mintaka/u0/xx/ss/jones/jones.old/mftp.src/mftpnew/RCS/olt.c,v $\n";
#endif

/* $Log: olt.c,v $
 * Revision 1.1  1994/03/14  18:55:53  jones
 * Initial revision
 *
 */
#include "machine.h"
#include "defs.h"

#include <X11/StringDefs.h>
#include <Xol/OpenLook.h>
#include <X11/IntrinsicP.h>
#if defined(sun)
#include <X11/ObjectP.h>
#endif

#include <Xol/OpenLookP.h>
#include <Xol/FormP.h>
#include <Xol/ManagerP.h>
#include <Xol/MenuButtoP.h>
#include <Xol/PopupWindo.h>
#include <Xol/OblongButt.h>
#include <Xol/TextField.h>
#include <Xol/StaticText.h>
#if OL_VERSION==2
#include <Xol/OpenLookI.h>
#include <Xol/Text.h>
#endif
#include <Xol/ScrolledWi.h>
#include <Xol/TextEdit.h>
#include <Xol/PrimitiveP.h>
#include <Xol/StubP.h>

#include <X11/Xlib.h>
#include <X11/Intrinsic.h>
#include <X11/cursorfont.h>


#include "wc.h"

#include "proto/olt.h"

extern char pass[];
extern Widget Top;
extern XtAppContext  App_context;
Widget WcCreatePopup();
Widget Retry_w = NULL;

char * GetText();

typedef struct _help_t {
        XFontStruct *font;
        int width_in_chars;
} get_font, *get_font_p;

get_font font_res;

static XtIntervalId help_context = 0;

static TextPosition StartPos = 0;

static XtResource r_font[] =
{
    {XtNfont,  XtCFont, XtRFontStruct, sizeof(XFontStruct *),
        XtOffset(get_font_p,font),XtRString, "XtDefaultFont"},
    {"width_in_chars", "Width_in_chars", XtRInt,  sizeof(int),
       XtOffset(get_font_p, width_in_chars), XtRImmediate, (XtPointer)55 },
};

static XFontStruct *FS;

static GC sepgc = NULL;

Widget password_w = NULL;

#define MAX_D 40
static struct {
    Widget w;
    void (*callback)();
} list_d[MAX_D];
 
char *view_file_name = NULL;


void
AppendDialogText(w, s)
Widget w;
char   *s;
{
    AppendTextEdit(w, s);
    OlTextEditGetLastPosition((TextEditWidget)w, &StartPos);
}

void
AppendStatusText(w, text) 
Widget w;
char *text;
{
    AppendTextEdit(w, text);
}

void
AppendTextEdit(w, text)
Widget w;
char   *text;
{
    TextPosition start;

    OlTextEditGetLastPosition((TextEditWidget)w, &start);
    if (start == 0) {
        OlTextEditSetCursorPosition((TextEditWidget)w, start, start, start);
    } else {
        OlTextEditSetCursorPosition((TextEditWidget)w, start, start, start+1);
    }
    OlTextEditInsert((TextEditWidget)w, text, strlen(text));
}

static void
Context_help()
{
    Position top_x = 0,
             top_y = 0,
             hw_x  = 0,
             hw_y  = 0;
    int      x,y;
    Time     lastTime;
    Cursor   crosshair = NULL;
    XEvent   event;
    Widget   w = NULL;
    Widget   top = NULL;
     

    XFlush(XtDisplay(Top));
    crosshair = XCreateFontCursor (XtDisplay(Top), XC_crosshair);
    lastTime = XtLastTimestampProcessed(XtDisplay(Top));
    XtGrabPointer(Top, True, (ButtonPressMask | ButtonReleaseMask),
                  GrabModeAsync, GrabModeAsync, NULL, crosshair, lastTime);
    /*
     * Give the status label a change to update.
     */
    while(XtAppPending(App_context) & XtIMXEvent)  
                      XtAppProcessEvent(App_context, XtIMXEvent);
    while(1) {
	XMaskEvent(XtDisplay(Top), (ButtonMotionMask | ButtonReleaseMask |
                   ButtonPressMask), &event);
	if (event.type == ButtonPress) {
	    top = w = XtWindowToWidget(event.xbutton.display, 
			         event.xbutton.window);
	    if (!top) break;
	    while(!XtIsShell(top)) {
		top = XtParent(top);
	    }
	    if (w) {
           	XtTranslateCoords(top, 0, 0, &top_x, &top_y);
                XtTranslateCoords(w , event.xbutton.x,
                                      event.xbutton.y,
                                      &hw_x,  &hw_y);
                x = hw_x - top_x;
                y = hw_y - top_y;
                w = findwidget(top, x, y);
            }
	}
	if (event.type == ButtonRelease) break;
    }
    XtUngrabPointer(Top, lastTime);
    if (w) Help(w,  NULL, 0);
    else   Help(Top, NULL, 0);
    Restore_Last_Status_Msg();
}

Widget
CreateSep(parent, name, arg, nargs)
Widget    parent;
String    name;
Arg      *arg;
Cardinal  nargs;
/*
 *  Dialog window has its own set of translations for editing.
 *  Special action procedures for keys Delete/Backspace, Carriage Return,
 */
{
    Arg         args[2];
    Cardinal    n = 0;
    Widget      stub;

    XtSetArg(args[n], XtNexpose, draw_sep_cb);                   n++;
    XtSetArg(args[n], XtNheight , 1);                        n++;
    stub = XtCreateManagedWidget(name, stubWidgetClass, parent, args, n );
    return stub;
}

Widget 
CreateSimpleDialog(parent,
		   name,
		   label,
		   input,
                   button1, 
		   v1, 
		   button2, 
		   v2, 
		   button3, 
		   v3, 
		   help, 
		   v4,
		   callback)
Widget parent;
char   *name;
char   *label;
int    input;
char   *button1;
int    v1;
char   *button2;
int    v2;
char   *button3;
int    v3;
char   *help;
int    v4;
void   (*callback)();
{
     Widget w, w_u, w_l;
     Widget temp;

     w = XtVaCreatePopupShell(name, popupWindowShellWidgetClass, parent, NULL);

     XtVaGetValues(w,
                   XtNupperControlArea,    &w_u,
                   XtNlowerControlArea,    &w_l,
                   NULL);

     if (label) {
        temp = XtVaCreateManagedWidget("diaglabel", 
				        staticTextWidgetClass,
				        w_u,
					XtNstring,
					label,
				        NULL);
     }

     if (input) {
         temp = XtVaCreateManagedWidget("text", 
				        textFieldWidgetClass,
				        w_u,
				        NULL);
     }
				    
     if (button1 != NULL) {
	temp = XtVaCreateManagedWidget(button1, 
				       oblongButtonWidgetClass,
				       w_l,  
				       NULL);
	XtAddCallback(temp, XtNselect, callback, (XtPointer)v1);	
     }

     if (button2 != NULL) {
	temp = XtVaCreateManagedWidget(button2	, 
				       oblongButtonWidgetClass,
				       w_l,  
				       NULL);
	XtAddCallback(temp, XtNselect, callback, (XtPointer)v2);	
     }

     if (button3 != NULL) {
	temp = XtVaCreateManagedWidget(button3, 
				       oblongButtonWidgetClass,
				       w_l,  
				       NULL);
	XtAddCallback(temp, XtNselect, callback, (XtPointer)v3);	
     }

     /* Ignore help */
     

    return w;
}

char *
GetDialogText(w)
Widget w;
{
    Widget v = NULL;

    v = WcFullNameToWidget(w,"*text");
    if (v) return GetText(v); 
    return NULL;
}

char *
GetText(w)
Widget w;
{
    Arg arg[1];
    String str;
    char *new;

    if (!w) return XtNewString("");
    XtSetArg(arg[0], XtNstring, &str);
    XtGetValues(w, arg, XtNumber(arg));
    new = XtNewString(str);
    return new;
}

int 
Has_Focus(top)
Widget top;
{
    int i;
    Window focus = NULL;
    Widget w;
    int revert_to;

    if (!XtIsWidget(top)) return 0;

    XGetInputFocus(XtDisplay(Top), &focus,  &revert_to);
    if (focus == NULL) return 0;

    if (XtWindow(top) == focus) return 1;

    if (XtIsComposite(top)) {
        CompositeWidget cw = (CompositeWidget) top;
        for (i = 0; i< cw->composite.num_children; i++) {
            Widget cld = cw->composite.children[i];

            if (!XtIsWidget (cld)) continue;
	    if (Has_Focus(cld)) return 1;
        }
    }

    if (0 < top->core.num_popups)  {
        for (i=0; i< top->core.num_popups; i++) {
            Widget pop = top->core.popup_list[i];
            if (!XtIsWidget(pop)) continue;
	    if (Has_Focus(pop)) return 1;
        }
    }
    return 0;
}

void
Mark_Menu(w)
Widget w;
{
     XtVaSetValues(w, XtNsensitive, FALSE, NULL);
}

void
clear_text_cb(w, closure, call_data)
Widget w;
XtPointer closure;
XtPointer call_data;
{
     char *name = (char *)closure;
     Widget v;

     v = WcFullNameToWidget(Top, name);
     if (!v) return;
     OlTextEditClearBuffer((TextEditWidget)v);
     if (strcmp(name, "*ftp") == 0) {
	Do_Prompt(NULL, 1);
     }
}

void
Position_Dialog_cb(w, closure, call_data)
Widget w;
XtPointer closure;
XtPointer call_data;
{
    Arg arg[1];
    Widget v;
    char *name = (char *)closure;
    Window root, child;
    unsigned int buttons;
    int root_x, root_y, child_x, child_y;
    Dimension width, height;
    Arg args[3];
    int n = 0;
   
    v = WcFullNameToWidget(w, name);
    XQueryPointer(XtDisplay(w), XtWindow(w), &root, &child,
                      &root_x, &root_y, &child_x, &child_y, &buttons);


    /*
     * Realize dialog make sure size is correct.
     */
    XtRealizeWidget(v);

    n = 0;
    XtSetArg(args[n], XtNwidth,  &width); n++;
    XtSetArg(args[n], XtNheight, &height); n++;
    XtGetValues(v, args, (Cardinal)n);

    root_y -= height/2;
    root_x -= width/2;
    n = 0;
    XtSetArg(args[n], XtNx, root_x); n++;
    XtSetArg(args[n], XtNy, root_y); n++; 
    XtSetValues(v, args, n);
}

void
RegisterWidgets(App_context)
XtAppContext App_context;
{
    /*
     * Register OpenLook Wigets
     */
    XopRegisterOpenLook (App_context);
#if OL_VERSION==2
    RCP  ("textWidgetClass"   , textWidgetClass);
#endif
    RCR  ("ScrolledText"   ,  create_scrolled_text);
}

void
Reset_D_String(w)
Widget w;
{
    Widget v = NULL;

    v = WcFullNameToWidget(w,"*text");
    if (v) SetText(v, "\0");
}

void
Reset_Gateway_Button()
{
    Arg arg[1];

    SetWidget(Retry_w,"*DoGatetway" , 1);
    XtSetArg(arg[0], XtNset, FALSE);
    XtSetValues(Retry_w, arg, XtNumber(arg));
}

void
Reset_Retry_Button()
{
    Arg arg[1];

    SetWidget(Retry_w,"*DoRetry" , 1);
    XtSetArg(arg[0], XtNset, FALSE);
    XtSetValues(Retry_w, arg, XtNumber(arg));
    
}

void
SetDialogLabel(w, label)
Widget w;
char   *label;
{
    Widget v = NULL;
    Arg arg[1];

    v = WcFullNameToWidget(w, "*diaglabel");
    if (v == NULL) return;
    XtSetArg(arg[0], XtNstring, label);
    XtSetValues(v, arg, XtNumber(arg));
}

void
SetLabel(w, string)
Widget w;
char   *string;
{
    Arg arg[1];

    XtSetArg(arg[0], XtNlabel, string);
    XtSetValues(w, arg, XtNumber(arg));
    XtSetArg(arg[0], XtNstring, string);
    XtSetValues(w, arg, XtNumber(arg));
}

void
SetStringSource(w, text)
Widget w;
char   *text;
{
    Arg arg[1];

    if (!w) return;
    XtSetArg(arg[0], XtNsource, text);
    XtSetValues(w, arg, XtNumber(arg));
}

void
SetText(w, text)
Widget w;
char   *text;
{
    Arg arg[1];

    if (!w) return;
    XtSetArg(arg[0], XtNstring, text);
    XtSetValues(w, arg, XtNumber(arg));
}

void
SetTextEdit(w, text)
Widget w;
char   *text;
{
    TextPosition start = 0;
    OlTextEditClearBuffer((TextEditWidget)w);
    OlTextEditGetLastPosition((TextEditWidget)w, &start);
    OlTextEditSetCursorPosition((TextEditWidget)w, start, start, start);
    OlTextEditInsert((TextEditWidget)w, text, strlen(text));
}

void
Unmark_Menu(w)
Widget w;
{
     XtVaSetValues(w, XtNsensitive, TRUE, NULL);
}

void
View_The_File(file, name)
char *file;
char *name;
{
    Widget  View_shell = NULL;

    view_file_name = file;

    View_shell = WcCreatePopup( Top, "Shellview" );
    XtPopup(View_shell, XtGrabNone);
    Set_Icon(View_shell, "view");
    WM_DELETE(View_shell, delete_view, "view");
    XSetIconName(XtDisplay(View_shell), XtWindow(View_shell), name);
}

static void
WM_ACTION_cb(w, closure, call_data)
Widget w;
XtPointer closure;
XtPointer call_data;
{
    int i = (int)closure;
    OlWMProtocolVerify *st = (OlWMProtocolVerify*)call_data;
    int action;

    if (i >= MAX_D) {
	fprintf(stderr, "Could not find wedit in action table\n");
	exit(1);
    }
    if ((st->msgtype&OL_WM_DELETE_WINDOW) == OL_WM_DELETE_WINDOW) {
	list_d[i].callback(list_d[i].w, NULL, NULL);
    } else  {
	OlWMProtocolAction(w, st, OL_DEFAULTACTION);
    }
}

void
WM_DELETE(w, callback, closure)
Widget w;
void (*callback)();
caddr_t closure;
{
    int i;
    char line[1000];
    Arg arg[1];

    for (i = 0; i<MAX_D; i++) {
	if (list_d[i].w == w) return;
	if (list_d[i].w == 0) {
	    list_d[i].w = w;
	    list_d[i].callback = callback;
	    break;
	}
    }

    if (i == MAX_D) {
	fprintf(stderr, "Too many top leval shells\n");
	exit(1);
    }

    OlAddCallback(w, (String)XtNwmProtocol, 
		  (XtCallbackProc)WM_ACTION_cb, (XtPointer)i);
}

static void
WM_DELETE_UNREGISTER(w) 
Widget w;
{
    int i;

    for (i = 0; i<MAX_D; i++) {
	if (list_d[i].w == w) {
	    list_d[i].w = NULL;
	    list_d[i].callback = NULL;
	    return;
	}
    }
}

static Widget
create_scrolled_text(parent, name, arg, nargs)
Widget    parent;
String    name;
Arg      *arg;
Cardinal  nargs;
{
    Widget w;
    char   *x;

    x = (char *)XtMalloc(strlen(name)+1+2);

    strcpy(x, name);
    strcat(x, "SW");
    
    w = XtCreateManagedWidget(x, 
			      scrolledWindowWidgetClass,
                              parent, 
			      NULL, 
			      0);
    w = XtCreateManagedWidget(name, 
			      textEditWidgetClass,
                              w, 
			      NULL, 
			      0);
    XtFree(x);
    return w; 
}

static void
delete_char_act(w, event, params, num_params)
Widget w;
XEvent *event;
String *params;
Cardinal *num_params;
{
    TextPosition end;

    OlTextEditGetLastPosition((TextEditWidget)w, &end);
    if (end == 0)  return;
    if (StartPos >= end) return;
    OlTextEditSetCursorPosition((TextEditWidget)w, end-1, end, end-1);
    OlTextEditInsert((TextEditWidget)w, NULL, 0);
}

static void
delete_view(w)
Widget w;
{
    WM_DELETE_UNREGISTER(w);
    XtDestroyWidget(w);
}

static void
delete_view_file_cb(w, closure, call_data)
Widget w;
XtPointer closure;
XtPointer call_data;
{
    char *file = (char *)closure;

    if (!file) return;
    unlink(file);
    XtFree(file);
}

static void
dispatch_act(w, event, params, num_params)
Widget w;
XEvent *event;
String *params;
Cardinal *num_params;
{
    char *dialog;
    char *s;
    TextPosition  end;

    OlTextEditGetLastPosition((TextEditWidget)w, &end);
    s = XtMalloc(end-StartPos+1+1);
    OlTextEditReadSubString((TextEditWidget)w,  &s, StartPos, end);
    AppendTextEdit(w, "\n");
    Peek(s);
    XtFree(s);
}

static void
draw_sep_cb(w, xevent, region)
Widget w;
XEvent *xevent;
Region region;
{
    StubRec *stub = (StubRec *)w; 
    XGCValues values;
    XtGCMask  valueMask;
    

    if (sepgc == NULL) {
    	values.foreground   = stub->primitive.foreground;
        valueMask = GCForeground;
	sepgc = XtGetGC(w, valueMask, &values);
    }
    XDrawLine(XtDisplay(w), XtWindow(w), sepgc, 0, 0, w->core.width, 0);
}

static Widget
findwidget(top, x, y)
Widget top;
int x;
int y;
{
    int i;
    int simple = 1;
    Widget w = NULL;



    if (!XtIsWidget (top)) return NULL;

    if (XtIsComposite(top)) {
	CompositeWidget cw = (CompositeWidget) top;
	simple = 0;
	for (i = 0; i< cw->composite.num_children; i++) {
	    Widget cld = cw->composite.children[i];
    	    if (!XtIsWidget (cld)) continue;
    	    if ((int)cld->core.x <= x && 
	        (int)(cld->core.x + cld->core.width) >= x && 
            	(int)cld->core.y <= y && 
	        (int)(cld->core.y + cld->core.height) >= y) {
	    	if (!XtIsComposite(cld))  return cld;
	    	w = findwidget(cld, x - (int)cld->core.x, y - (int)cld->core.y);
	        if (w) return w;
	    }
	}
    	if ((int)top->core.x <= x && 
	    (int)(top->core.x + top->core.width ) >= x && 
            (int)top->core.y <= y && 
	    (int)(top->core.y + top->core.height) >= y) 
		return top;
    }

    if (0 < top->core.num_popups)  {
	simple = 0;
	for (i=0; i< top->core.num_popups; i++) {
	    Widget pop = top->core.popup_list[i];
    	    if ((int)pop->core.x <= x && 
		(int)(pop->core.x +pop->core.width) >= x && 
        	(int)pop->core.y <= y && 
		(int)(pop->core.y +pop->core.height) >= y)  {
		if (XtIsManaged(top->core.popup_list[i])) {
	    	    w = findwidget(top->core.popup_list[i], 
		 	           x - pop->core.x, 
			           y - pop->core.y);
	    	    if (w) return w;
	 	}
	    }
        }
    }

    if (!simple) return NULL;
    if ((int)top->core.x <= x && 
	(int)(top->core.x +top->core.width) >= x && 
        (int)top->core.y <= y && 
	(int)(top->core.y +top->core.height) >= y) return top;
    return NULL;
}

static void
help_act(w, event, params, nparams)
Widget          w;
XEvent         *event;
String         *params;
Cardinal       *nparams;
{
    Widget hw  = NULL;
    Widget top = NULL;
    Position top_x = 0,
	     top_y = 0,
	     hw_x  = 0,
	     hw_y  = 0;
    int      x,y;
    char    *nameold, *cp;
    char     *WcWidgetToFullName();

    if (params && params[0]) {
	Help(w, params[0], 0);
	return;
    }
    if (!event) Help(w, NULL, 0);

    switch (event->type) {
   	case KeyPress:
		top = hw = XtWindowToWidget(XtDisplay(w),event->xkey.window);
		while(!XtIsShell(top)) {
			top = XtParent(top);
		}
		if (hw) {
	        	XtTranslateCoords(top, 0, 0, &top_x, &top_y);
	        	XtTranslateCoords(hw , event->xkey.x, 
					       event->xkey.y, 
					       &hw_x,  &hw_y);
			x = hw_x - top_x;
			y = hw_y - top_y; 
		        hw = findwidget(top, x, y);
		}
		if (!hw) {
			Help(Top, NULL, 0);
			return;
		}
  		nameold = WcWidgetToFullName(hw);
		if (nameold) {
		    while(bsdstrstr(nameold, "menu") != NULL) {
		    	cp = RINDEX(nameold, '.');
		    	if (cp) {
			    *cp = '\0';
		    	} else {
			    break;
			}
		    }
		    hw = WcFullNameToWidget(top, nameold);
		    XtFree(nameold);
		}
		if (hw) Help(hw,  NULL, 0);
                else    Help(Top, NULL, 0);
                return;
            default:
                Help(w, NULL, 0);
                return;
    }
}

static void
help_cb(w, stuff, unused)
Widget  w;
char    *stuff;
caddr_t unused;
{
    void  help_context_f();

    if (stuff && stuff[0]) {
        if (strcmp(stuff, "c") == 0) {
            /*
             * Could not figure out any other way to do this.
             */
            Set_Status_No_Log("Place cursor on window and click");
            help_context =  XtAppAddTimeOut(App_context,
                            (unsigned long)(1),
                            (XtTimerCallbackProc)help_context_f,
                            (XtPointer)NULL);
        } else {
            Help(w, stuff, 0);
        }
    } else
        Help(w, NULL , 0);
}

static void
help_context_f()
{
    Widget hw = NULL;;

    XtRemoveTimeOut(help_context);
    Context_help();
    return;
}

void
init_olt_acctions(App_context)
XtAppContext App_context;
{
    RACCT("help"              , help_act);
    RACCT("insert_the_char"   , insert_char_act);
    RACCT("clear_line"        , clear_line_act);
    RACCT("delete_the_char"   , delete_char_act);
    RACCT("Dispatch"          , dispatch_act);
    RACCT("MyPopup"           , my_popup_act);

    RCALL("Set_retry"         , set_retry_cb);
    RCALL("Set_gateway"       , set_gateway_cb);
    RCALL("Clear_text"	      , clear_text_cb);
    RCALL("delete_view_file"  , delete_view_file_cb);
    RCALL("help"              , help_cb);
    RCALL("set_width"         , set_width_cb);
    RCALL("set_view_file"     , set_view_file_cb);
    RCALL("take_over"         , take_over_cb);
    RCALL("PositionDialog"    , Position_Dialog_cb);
    RCR  ("CreateSep"         , CreateSep);
    XtRegisterGrabAction(my_popup_act, True,
                              (unsigned)(ButtonPressMask | ButtonReleaseMask),
                              GrabModeAsync,
                              GrabModeAsync);
}

static void
insert_char_act(w, event, params, num_params)
Widget w;
XEvent *event;
String *params;
Cardinal *num_params;
{
    int  n, i;
    char input[100];
    XComposeStatus status;

    n = XLookupString((XKeyEvent*)event, input, sizeof(input)-1, 
		     (KeySym *) NULL, &status);
    if(!n) return;

    input[n] = '\0';
    AppendTextEdit(w, input);
}

static void
clear_line_act(w, event, params, num_params)
Widget w;
XEvent *event;
String *params;
Cardinal *num_params;
{
    TextPosition end;

    OlTextEditGetLastPosition((TextEditWidget)w, &end);
    if (end == 0)  return;
    if (StartPos >= end) return;
    OlTextEditSetCursorPosition((TextEditWidget)w, StartPos, end, StartPos);
    OlTextEditInsert((TextEditWidget)w, NULL, 0);
}

static void
insert_char_cb(w, closure, call_data)
Widget w;
XtPointer closure;
XtPointer call_data;
{
    OlTextModifyCallData *cd = (OlTextModifyCallData *)call_data;
    int pos, n, l, i;
    char buff[1000];

    if (cd->text && cd->text[0]) {
	pos = cd->current_cursor;
  	n   = strlen(cd->text);
        if ((int)strlen(pass) >= pos+1) {
		strcpy(buff, &pass[pos+1]);
	} else {
		buff[0] = '\0';
	}
        strncpy((char *)&pass[pos], cd->text, n);
        pass[pos+n] = '\0';
	strcat(pass, buff);
	for (i=0; i<n ;i++) cd->text[i] = 'X';
    } else {
        pos = cd->select_start;
 	l = cd->select_end - cd->select_start;
	n = strlen(pass);
 	if (n == 0 || l<= 0) return;
	for (i=pos; i<(n-l) ;i++) pass[i] =  pass[i+l];
        pass[n-l] = '\0';
    }
}

static void
my_popup_act(w, event, params, num_params)
Widget w;
XEvent * event;
String * params;
Cardinal * num_params;
{
    if (Is_Noop(w)) return;
    XtMenuPopupAction(w, event, params, num_params);
}

static void
set_gateway_cb(w, client_data, call_data)
Widget  w;
caddr_t client_data;
caddr_t call_data;
{
    extern int use_gateway;
    char *state = (char *)client_data;

    use_gateway = atoi(state);
}

static void
set_retry_cb(w, client_data, call_data)
Widget  w;
caddr_t client_data;
caddr_t call_data;
{
    extern int retry;
    extern XtIntervalId timeout_retry;
    char *state = (char *)client_data;

    retry = atoi(state);
    if(timeout_retry) {
        XtRemoveTimeOut(timeout_retry);
        timeout_retry = 0;
    }
}

static void
set_view_file_cb(w, closure, call_data)
Widget w;
XtPointer closure;
XtPointer call_data;
{
    Arg args[2];
    char *file;

    if (view_file_name == NULL) return;
    file = XtNewString(view_file_name);
    XtSetArg(args[0], XtNsourceType, OL_DISK_SOURCE);
    XtSetArg(args[1], XtNsource, file);
    XtSetValues(w, args, XtNumber(args));
    XtAddCallback(w, XtNdestroyCallback, delete_view_file_cb, (XtPointer)file);
}

static void
set_width_cb(w, closure, call_data)
Widget w;
XtPointer closure;
XtPointer call_data;
{
     Arg arg[1];
     Dimension width = 0;
     int add = 0;
     char *cp1 = NULL;
     char *cp2 = NULL;
     int  ave;

     if (!FS) {
     	XtGetSubresources(Top, &font_res, "*font", "*font", r_font, 
			  XtNumber(r_font), NULL, 0);
        FS = font_res.font;
	if (!font_res.width_in_chars) font_res.width_in_chars = 55;
     }
     if (!FS) return;
     if (closure) {
	cp1 = XtNewString((char *)closure);
	if (cp2 = RINDEX(cp1, ',')) {
	    *cp2 = '\0';
	    cp2++;
	    add = atoi(cp2);
	}
	width = atoi(cp1);
  	XtFree(cp1);
     }
     if (width <= 0) width = 1;
     ave = FS->max_bounds.width;
     width = (font_res.width_in_chars*ave + add)/(int)width;
     XtSetArg(arg[0], XtNwidth, width);
     XtSetValues(w, arg, XtNumber(arg));
}

static void
take_over_cb(w, closure, call_data)
Widget w;
XtPointer closure;
XtPointer call_data;
{
     SetWidget(password_w, "*passwordtext.Textedit", 0);
     XtAddCallback(password_w,  XtNmodifyVerification,  insert_char_cb, 
		(XtPointer)w );
}

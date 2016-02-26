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
"$Id: xaw.c,v 1.3 1994/05/20 20:22:31 jones Exp $\n";
static char rcspath[] =
"$Source: /mintaka/u0/xx/ss/jones/jones.old/mftp.src/moxftp-2.0/RCS/xaw.c,v $\n";
#endif

/* $Log: xaw.c,v $
 * Revision 1.3  1994/05/20  20:22:31  jones
 * Do not depend on X11R5 NULL value XtGetValues trick.
 *
 * Revision 1.2  1994/03/21  21:34:00  jones
 * Use transientShellWidgetClass of dialogs instead of overrideShellWidgetClass.
 *
 * Revision 1.1  1994/03/14  18:55:53  jones
 * Initial revision
 *
 */

#include "machine.h"
#include "defs.h"
#include "connect_info.h"
#if defined(XAW3D)
#include <X11/Xaw3d/Toggle.h>
#include <X11/Xaw3d/AsciiText.h>
#include <X11/Xaw3d/TextP.h>
#include <X11/Xaw3d/Text.h>
#include <X11/Xaw3d/Form.h>
#include <X11/Xaw3d/Dialog.h>
#include <X11/Xaw3d/SimpleMenu.h>
#include <X11/Xaw3d/SmeBSB.h>
#else
#include <X11/Xaw/Toggle.h>
#include <X11/Xaw/AsciiText.h>
#include <X11/Xaw/TextP.h>
#include <X11/Xaw/Text.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/Dialog.h>
#include <X11/Xaw/SimpleMenu.h>
#include <X11/Xaw/SmeBSB.h>
#endif
#include <X11/Xlib.h>
#include <X11/Intrinsic.h>
#include <X11/cursorfont.h>
#include "xlogo11.xbm"
#include "rdblarrow.xbm"
#include "List.h"
#include "wc.h"

/*
 * Functions prototypes.
 */
#include "proto/xaw.h"

Widget WcFullNameToWidget();
extern char pass[];
extern struct _connect_info connect_info[];
extern int NO_CONNECT;
static XtIntervalId help_context = 0;

#define DIALOGSIZE      100000
static char DialogText[DIALOGSIZE];     /* text buffer for widget */
static char StatusText[DIALOGSIZE];     /* text buffer for StatusText */

static XawTextPosition  StartPos;       /* starting position of input text */

/*
 * pixmaps.
 */
static Pixmap mark = NULL;
static Pixmap right = NULL;

static Widget Retry_w = NULL; 
static Widget Gateway_w = NULL;
extern Widget Top;
extern XtAppContext  App_context;
Widget List_v = NULL;

typedef struct _help_t {
	XFontStruct *font;
 	int width_in_chars; 
} get_font, *get_font_p;

get_font font_res;

static XtResource r_font[] =
{
    {XtNfont,  XtCFont, XtRFontStruct, sizeof(XFontStruct *),
        XtOffset(get_font_p,font),XtRString, "XtDefaultFont"},
    {"width_in_chars", "Width_in_chars", XtRInt,  sizeof(int),
       XtOffset(get_font_p, width_in_chars), XtRImmediate, (XtPointer)55 },
};

static XFontStruct *FS;

static char rate[2000];

#define MAX_D 100
static struct {
    Widget w;
    void (*callback)();
} list_d[MAX_D];
 
static Atom wm_delete_window;

char *view_file_name = NULL;

/* 
 * Cascade menu subroutines.
 */

static int menu_bind_n = 0;
static int menu_bind_a = 0;
static struct  _menu_bind {
	Widget target;
	char   *popup;
	Widget menu;
	int    up;
} *menu_bind = NULL;


static XtActionsRec Actions[] = {
    {"WM_delete"         , wm_delete_act},
    {"DeleteWord"        , delete_word_act},
    {"InsertSpace"       , insert_space_act},
    {"my_insert_char"    , insert_char_act},
    {"delete_char"       , delete_char_act},
    {"MyPopup"           , my_popup_act},
    {"Dispatch"          , dispatch_act},
    {"DeleteLine"        , delete_line_act},
    {"warp_pointer"      , warp_pointer_act},
    {"help"              , help_act},
    {"menu_cascade"      , menu_cascade_act},
    {"PopDownSubs"	 , pop_down_subs_act},
    {"MenuNoop"          , MenuNoop_act} 
};


void 
AppendDialogText(w, s)
Widget w;
char   *s;
{
    XawTextPosition     i, lastPos;
    XawTextBlock        textblock, nullblock;
    Cardinal            n;

    if (!s || !strcmp(s, "")) return;
  
    textblock.firstPos = 0;
    textblock.length   = strlen(s);
    textblock.ptr      = s;

    lastPos = TextGetLastPos(w);
    if (textblock.length > DIALOGSIZE) {
        fprintf(stderr, 
	    "xftp/mftp error: cannot display string in dialogue window\n\
            string has %d bytes; dialogue window size limit is %d bytes\n",
            textblock.length, DIALOGSIZE);
        return;
    }

    if (lastPos + textblock.length > DIALOGSIZE) {
        nullblock.firstPos = 0;
        nullblock.length = 0;
        nullblock.ptr = "";

        i = textblock.length - (DIALOGSIZE - lastPos);
        if (i < 0.9*DIALOGSIZE)
            i += 0.1*DIALOGSIZE;
        while (DialogText[i] != '\n') i++;
        XawTextReplace(w, 0, i+1, &nullblock);
        lastPos = TextGetLastPos(w);
    }

    XawTextReplace(w, lastPos, lastPos, &textblock);
    StartPos = TextGetLastPos(w);
    XawTextSetInsertionPoint(w, StartPos);
}

void 
AppendStatusText(w, s)
Widget w;
char   *s;
{
    XawTextPosition     i, lastPos;
    XawTextBlock        textblock, nullblock;
    Cardinal            n;

    if (!s || !strcmp(s, "")) return;
  
    textblock.firstPos = 0;
    textblock.length   = strlen(s);
    textblock.ptr      = s;

    lastPos = TextGetLastPos(w);
    if (textblock.length > DIALOGSIZE) {
        fprintf(stderr, 
	    "xftp/mftp error: cannot display string in dialogue window\n\
            string has %d bytes; dialogue window size limit is %d bytes\n",
            textblock.length, DIALOGSIZE);
        return;
    }
    if (lastPos + textblock.length > DIALOGSIZE) {
        fflush(stdout);
        nullblock.firstPos = 0;
        nullblock.length = 0;
        nullblock.ptr = "";

        i = textblock.length - (DIALOGSIZE - lastPos);
        if (i < 0.9*DIALOGSIZE)
            i += 0.1*DIALOGSIZE;
        while (StatusText[i] != '\n') i++;
        XawTextReplace(w, 0, i+1, &nullblock);
        lastPos = TextGetLastPos(w);
    }
    XawTextReplace(w, lastPos, lastPos, &textblock);
    lastPos = TextGetLastPos(w);
    XawTextSetInsertionPoint(w, lastPos);
}

void
Bind_Menu(target, name, menu)
Widget target;
char *name;
Widget menu;
{
    if (right == NULL) {
	right = XCreateBitmapFromData(XtDisplay(Top), 
				      RootWindowOfScreen(XtScreen(Top)),
				      (char *)next_bits, 
				      next_width, 
				      next_height);
    }

    if (menu_bind == NULL) {
	menu_bind = (struct _menu_bind *)XtMalloc(50*sizeof(struct _menu_bind));
	ZERO((char *)menu_bind, sizeof(struct _menu_bind)*50);
	menu_bind_a = 50;
    } else if (menu_bind_n == menu_bind_a) {
	menu_bind = (struct _menu_bind*)
		     XtRealloc((char *)menu_bind, 
			        (menu_bind_a+50)*sizeof(struct _menu_bind));
	ZERO((char *)(menu_bind+menu_bind_a), sizeof(struct _menu_bind)*50);
	menu_bind_a += 50;
    }

    XtVaSetValues(target, 
		  XtNrightBitmap, right, 
		  XtNrightMargin, next_width+2,
		  NULL);
    menu_bind[menu_bind_n].target = target;
    if (name) menu_bind[menu_bind_n].popup  = XtNewString(name);
    if (menu) menu_bind[menu_bind_n].menu   = menu;
    menu_bind[menu_bind_n].up     = 0;
    menu_bind_n++;
}

static Widget 
CreateDialogWindow(parent, name, arg, nargs)
Widget    parent;
String    name;
Arg      *arg;   
Cardinal  nargs; 
/* 
 *  Dialog window has its own set of translations for editing.
 *  Special action procedures for keys Delete/Backspace, Carriage Return,
 */
{
    Arg 	args[5];
    Cardinal 	n;

    n = 0;
    XtSetArg(args[n], XtNuseStringInPlace, TRUE);                       n++;
    XtSetArg(args[n], XtNstring, (XtArgVal) DialogText);                n++;
    XtSetArg(args[n], XtNlength, (XtArgVal) DIALOGSIZE);                n++;
    XtSetArg(args[n], XtNeditType, (XtArgVal) XawtextAppend);           n++;
    XtSetArg(args[n], XtNscrollVertical, XawtextScrollAlways);          n++;
    DialogText[0] = 0;
    
    Status_text = XtCreateManagedWidget(name, asciiTextWidgetClass,
					 parent, args, n );
    return Status_text;
}

Widget 
CreateHelpPopupChild(pw, name)
Widget pw;
char *name;
{
    return XtCreatePopupShell(name, simpleMenuWidgetClass, pw, NULL, 0);
}

static Widget 
CreateRateWindow(parent, name, arg, nargs)
Widget    parent;
String    name;
Arg      *arg;   
Cardinal  nargs; 
/* 
 *  Rate window
 */
{
    Arg 	args[4];
    Cardinal 	n;
    Widget 	w;

    n = 0;
    XtSetArg(args[n], XtNuseStringInPlace, TRUE);                       n++;
    XtSetArg(args[n], XtNstring, (XtArgVal) rate);                      n++;
    XtSetArg(args[n], XtNlength, (XtArgVal) sizeof(rate));              n++;
    XtSetArg(args[n], XtNeditType, (XtArgVal) XawtextEdit);             n++;
    rate[0] = '\0';
    w = XtCreateManagedWidget(name, asciiTextWidgetClass, parent, args, n);
    return w;
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
    Arg args[2];
    int n = 0;
    Widget temp, w, v;


    XtSetArg(args[n], XtNallowShellResize, TRUE), n++;
    v = parent;

    while ( XtParent(v)) {
	if (XtIsShell(v)) break;
	v = XtParent(v);
	if (v == NULL) break;
    }

    XtSetArg(args[n], XtNtransientFor, v); n++;
    w  = XtCreatePopupShell(name, transientShellWidgetClass, v , args, n);

    n = 0;
    if (label) {
        XtSetArg(args[n], XtNlabel, label);n++;
    } 
    if (input)  {
       XtSetArg(args[n], XtNvalue, "");n++;

    }

    temp = XtCreateWidget("Dialog", dialogWidgetClass, w, args, n);

    n = 0;
    XtSetArg(args[0], XtNresizable, TRUE),n++;
    XtSetValues( XtNameToWidget(temp, "label"), args, n);

    if (button1 != NULL) {
        XawDialogAddButton(temp, button1, callback, (XtPointer)v1);
    }

    if (button2 != NULL) {
        XawDialogAddButton(temp, button2, callback, (XtPointer)v2);
    }

    if (button3 != NULL) {
        XawDialogAddButton(temp, button3, callback, (XtPointer)v3);
    }

    if (help) {
        XawDialogAddButton(temp, help, help_cb, NULL);
    }

    XtManageChild(temp);

    return w;
}

static Widget 
CreateStatusWindow(parent, name, arg, nargs)
Widget    parent;
String    name;
Arg      *arg;   
Cardinal  nargs; 
/* 
 *  Dialog window has its own set of translations for editing.
 *  Special action procedures for keys Delete/Backspace, Carriage Return,
 */
{
    Arg 	args[5];
    Cardinal 	n;
    Widget      w;

    n = 0;
    XtSetArg(args[n], XtNuseStringInPlace, TRUE);                       n++;
    XtSetArg(args[n], XtNstring, (XtArgVal) StatusText);                n++;
    XtSetArg(args[n], XtNlength, (XtArgVal) DIALOGSIZE);                n++;
    XtSetArg(args[n], XtNeditType, (XtArgVal) XawtextAppend);           n++;
    XtSetArg(args[n], XtNscrollVertical, XawtextScrollAlways);          n++;
    StatusText[0] = 0;
    w = XtCreateManagedWidget(name, asciiTextWidgetClass, parent, args, n );
    return w; 
}

char *
GetDialogText(w)
Widget w;
{
     Widget v;

     v = w;

     while ( XtParent(v)) {
	if (XtIsShell(v)) break;
	v = XtParent(v);
	if (v == NULL) break;
     }
     v = XtNameToWidget(v, "Dialog");
     if (v == NULL) return NULL;
     return XtNewString(XawDialogGetValueString(v));
}

char  *
GetLabel(w)
Widget w;
{
    Arg arg[1];
    char *cp;

    XtSetArg(arg[0], XtNlabel, &cp);
    XtGetValues(w, arg, XtNumber(arg));
    return cp;
}

char *
GetText(w)
Widget w;
{
    Arg arg[1];
    String str;
    char *new;

    XtSetArg(arg[0], XtNstring, &str);
    XtGetValues(w, arg, XtNumber(arg));
    new = XtNewString(str);
    return new;
}

void
Mark_Menu(w)
Widget w;
{
    Arg args[1];
 
    if (mark == NULL) {
	mark = XCreateBitmapFromData(XtDisplay(Top), 
				     RootWindowOfScreen(XtScreen(Top)),
				     (char *)xlogo11_bits, xlogo11_width, 
				     xlogo11_height);
    }

    XtVaSetValues(w, XtNleftBitmap, mark, NULL);
}

static void
MenuNoop_act(w, event, params, nparams)
Widget          w;
XEvent         *event;
String         *params;
Cardinal       *nparams;
{
    Widget v;
    int i;

    if (menu_bind_n == 0) {
        XawSimpleMenuClearActiveEntry(w);
	return;
    }
    v = XawSimpleMenuGetActiveEntry(w);
    if (v)  {
	for (i=0; i<menu_bind_n; i++) 
            if (menu_bind[i].target == v && menu_bind[i].up) return;
    }
    XawSimpleMenuClearActiveEntry(w);
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
    XtSetArg(args[n], XtNallowShellResize, True); n++;
    XtSetValues(v, args, XtNumber(args));
}

void
RegisterWidgets(App_context)
XtAppContext App_context;
{
    /*
     * Register Athena Widgets.
     */
    XpRegisterAll (App_context);
}

void
Reset_D_String(w)
Widget w;
{
    Arg arg[1];
    Widget v;

    v = XtNameToWidget(w, "Dialog");
    XtSetArg(arg[0], XtNvalue, "");
    XtSetValues(v, arg, XtNumber(arg));
}

void
Reset_Gateway_Button()
{
    Arg arg[1];

    SetWidget(Gateway_w,"*DoGateway" , 1);
    XtSetArg(arg[0], XtNstate, FALSE);
    XtSetValues(Gateway_w, arg, XtNumber(arg));
}

void
Reset_Retry_Button()
{
    Arg arg[1];

    SetWidget(Retry_w,"*DoRetry" , 1);
    XtSetArg(arg[0], XtNstate, FALSE);
    XtSetValues(Retry_w, arg, XtNumber(arg));
}

void
SetDialogLabel(w, label)
Widget w;
char   *label;
{
    Widget v = NULL;
    Arg arg[1];

    v = w;

    while ( XtParent(v)) {
	if (XtIsShell(v)) break;
	v = XtParent(v);
	if (v == NULL) return;
    }

    v = XtNameToWidget(v, "Dialog");
    if (v == NULL) return;
    XtSetArg(arg[0], XtNlabel, label);
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
}

void
SetText(w, text)
Widget w;
char   *text;
{
    Arg arg[1];

    XtSetArg(arg[0], XtNstring, text);
    XtSetValues(w , arg, XtNumber(arg));
}

void
SetTextRate(w, text)
Widget w;
char   *text;
{
    XawTextBlock        textblock;
    Cardinal            n;
    XawTextPosition     startPos, endPos;

     
    textblock.firstPos = 0;
    textblock.length   = strlen(text);
    textblock.ptr      = text;
    startPos = 0;
    endPos = strlen(text);
    XawTextReplace(w, startPos, endPos, &textblock);
}

static XawTextPosition 
TextGetLastPos(w)
Widget w;
{
    TextWidget ctx = (TextWidget) w;
    return (ctx->text.lastPos);
}

void
Unmark_Menu(w)
Widget w;
{
    XtVaSetValues(w, XtNleftBitmap, None, NULL);
}

void
View_The_File(file, name)
char *file;
char *name;
{
    Widget  View_shell = NULL;
    XWMHints wmhints;

    view_file_name = file;

    View_shell = WcCreatePopup( Top, "Shellview" );
    XtPopup(View_shell, XtGrabNone);
    Set_Icon(View_shell, "view");
    WM_DELETE(View_shell, delete_view, "view");
    XSetIconName(XtDisplay(View_shell), XtWindow(View_shell), name);
}

void
WM_DELETE(w, callback, closure)
Widget w;
void (*callback)();
caddr_t closure;
{
    int i;
    char line[1000];

 
    for (i = 0; i<MAX_D; i++) {
	if (list_d[i].w == w) return;
	if (list_d[i].w == 0) {
	    list_d[i].w = w;
	    list_d[i].callback = callback;
	    sprintf(line, "<Message>WM_PROTOCOLS: WM_delete(%d)", i);
	    break;
	}
    }

    if (i == MAX_D) {
	fprintf(stderr, "Too many top leval shells\n");
	exit(1);
    }
    wm_delete_window = XInternAtom(XtDisplay(w), "WM_DELETE_WINDOW", False);
    XtOverrideTranslations
      (w, XtParseTranslationTable (line));
    (void) XSetWMProtocols (XtDisplay(w), XtWindow(w),
                            &wm_delete_window, 1);
    XtAddCallback(w, XtNdestroyCallback, (XtCallbackProc)WM_DELETE_UNREGISTER, 
		  NULL);

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
            break;
        }
    }
}

static void
bind_menu_cb(w, closure, call_data)
Widget w;
XtPointer closure;
XtPointer call_data;
{
    Bind_Menu(w, closure, NULL);
}

void
clear_text_cb(w, closure, call_data)
Widget w;
XtPointer closure;
XtPointer call_data;
{
    char *name = (char *)closure;
    Widget v;
    XawTextBlock nullblock;
    XawTextPosition  lastPos;

    v = WcFullNameToWidget(Top, name);
    if (!v) return;
    lastPos = TextGetLastPos(v);
    nullblock.firstPos = 0;
    nullblock.length = 0;
    nullblock.ptr = "";
    XawTextReplace(v, 0, lastPos, &nullblock);
    if (strcmp(name, "*ftp") == 0) Do_Prompt(NULL, 1);
}

static  Cursor   crosshair = NULL;
static void
context_help()
{
    Position top_x = 0,
             top_y = 0,
             hw_x  = 0,
             hw_y  = 0;
    int      x,y;
    Time     lastTime;
    XEvent   event;
    Widget   w = NULL;
    Widget   top = NULL;
     

    XFlush(XtDisplay(Top));
    if (crosshair == NULL) 
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

static void
delete_char_act(w, event, params, num_params)
Widget w;
XEvent *event;
String *params;
Cardinal *num_params;
{
    XawTextPosition StartPos;
    XawTextBlock text;

    StartPos = TextGetLastPos(w);
    if (!StartPos) return;
    StartPos -= 1;
    text.firstPos = 0;
    text.length = 0;
    text.ptr = "";

    pass[StartPos] = '\0';
    XawTextReplace(w, StartPos, StartPos+1, &text);
    XawTextSetInsertionPoint(w, StartPos);
}

static void 
delete_line_act(w, event, params, num_params)
Widget w;
XEvent *event;
String *params;
Cardinal *num_params;
/*  
 * Deletes the entire current input line.
 *  simulates the action of the KILL character (ctrl-U).
 */
{
    XawTextBlock        textblock;
    XawTextPosition     pos, beginPos;
    Cardinal            i;
    char                *s;

    textblock.firstPos = 0;
    textblock.length   = 0;
    textblock.ptr      = "";

    pos = XawTextGetInsertionPoint(w); 
    if (w == Status_text) {
        s = DialogText;
        beginPos = StartPos;
        if (pos <= beginPos)
            pos = TextGetLastPos(w);
    }
    for (i=pos; i > beginPos && s[i-1] != '\n'; i--);
    XawTextReplace(w, i, pos, &textblock);
    XawTextSetInsertionPoint(w, i);
}

static void
delete_view(w)
Widget w;
{
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
delete_word_act(w, event, params, num_params)
Widget w;
XEvent *event;
String *params;
Cardinal *num_params;
{
/*  Erases the preceding word.
 *  Simulates the action of the WERASE character (ctrl-W).
 */
    XawTextBlock    	textblock;
    XawTextPosition	pos;
    Cardinal	 	i;

    textblock.firstPos = 0;
    textblock.length   = 0;
    textblock.ptr      = "";

    pos = XawTextGetInsertionPoint(w); 
    if (pos <= StartPos)
        pos = TextGetLastPos(w); 
    for (i=pos; i > StartPos && DialogText[i-1] == ' '; i--);
    for (; i > StartPos && DialogText[i-1] != ' '; i--);
    XawTextReplace(w, i, pos, &textblock);
    XawTextSetInsertionPoint(w, i);
}

static void 
dispatch_act(w, event, params, num_params)
Widget w;
XEvent *event;
String *params;
Cardinal *num_params; 
/*  Dispatch() is invoked on every <CR>.
 *  It collects text from the dialog window and sends it to dbx.
 *  If the string is a command to dbx (Prompt would be TRUE),
 *  it is stored in the global variable, Command.
 */
{
    char s[LINESIZ];
    
    strcpy(s, DialogText + StartPos);
    Peek(s);
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
		((int)(cld->core.x + cld->core.width)) >= x && 
            	(int)cld->core.y <= y && 
		((int)(cld->core.y + cld->core.height)) >= y) {
	    	if (!XtIsComposite(cld))  return cld;
	    	w = findwidget(cld, x - cld->core.x, y - cld->core.y);
	        if (w) return w;
	    }
	}
    	if ((int)top->core.x <= x && 
	    (int)(top->core.x + top->core.width ) >= x && 
            (int)top->core.y <= y && 
	    (int)(top->core.y + top->core.height) >= y) 
		return top;
    }

    if (!simple) return NULL;
    if ((int)top->core.x <= x && (int)(top->core.x +top->core.width) >= x && 
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
    char     *WcWidgetToFullName();

    if (!event) {
	Help(w, NULL, 0);
	return;
    }

    switch (event->type) {
        case KeyPress:
		top = hw = XtWindowToWidget(XtDisplay(w),event->xkey.window);
		while(!XtIsShell(top)) {
			top = XtParent(top);
		}
		/*
		 * Search for the widget.
		 */
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
		    if (top) {
			/*
		 	 *  Talk about parent
			 */
			Help(top, NULL, 0);
		    } else {
			/*
			 *  Talk about grand dadday
			 */
			Help(Top, NULL, 0);
		    }
		    return;
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
help_cb(w, closure, call_data)
Widget  w;
XtPointer closure;
XtPointer call_data;
{
    char *stuff = (char *)closure;

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
    context_help();
    return;
}

void
init_xaw_acctions()
{

    XtAppAddActions(App_context, Actions,
                             sizeof(Actions)/sizeof(XtActionsRec));

    RCALL("help"              , help_cb);
    RCALL("Clear_Text"	      , clear_text_cb);
    RCALL("set_width"         , set_width_cb);
    RCALL("set_view_file"     , set_view_file_cb);
    RCALL("PositionDialog"    , Position_Dialog_cb);
    RCALL("set_retry"         , set_retry_cb);
    RCALL("set_use_gateway"   , set_use_gateway_cb);
    RCALL("set_form_horiz"    , set_form_horiz_cb);
    RCALL("set_form_vert"     , set_form_vert_cb);
    RCALL("PopDownSubs"	      , pop_down_subs_cb);
    RCALL("bind_menu"         , bind_menu_cb);

    RCR  ("CreateDialogWindow", CreateDialogWindow);
    RCR  ("CreateStatusWindow", CreateStatusWindow);
    RCR  ("CreateRateWindow"  , CreateRateWindow);
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
    XawTextPosition StartPos;
    XawTextBlock text;

    n = XLookupString((XKeyEvent*)event, input, sizeof(input), (KeySym *) NULL,
                      &status);
    if(!n) return;
    StartPos = TextGetLastPos(w);
    strncpy((char *)&pass[StartPos], input, n);
    pass[StartPos+n] = '\0';
    for (i=0; i<n; i++) input[i] = 'X';
    input[n] = '\0';
    text.firstPos = 0;
    text.length = n;
    text.ptr = input;
    XawTextReplace(w, StartPos, StartPos, &text);
    XawTextSetInsertionPoint(w, StartPos+n);
}

static void 
insert_space_act(w, event, params, num_params)
Widget w;
XEvent *event;
String *params;
Cardinal *num_params;
/*  This procedure prevents the user from deleting past the prompt, or
 *  any text appended by AppendDialogText() to the dialog window.
 *  It checks the last position of text, if it matches StartPos, set
 *  by AppendDialogText(), it inserts a space so that delete-previous-
 *  character() can only delete the space character.
 */
{
    XawTextBlock    textblock;
    XawTextPosition lastPos;

    if (XawTextGetInsertionPoint(w) <= StartPos) {
    	lastPos = TextGetLastPos(w);
	if (lastPos == StartPos) {
	    textblock.firstPos = 0;
	    textblock.length   = 1;
	    textblock.ptr      = " ";
	    XawTextReplace(w, lastPos, lastPos, &textblock);
	    XawTextSetInsertionPoint(w, lastPos+1);
	}
    }
}

static void
menu_cascade_act(w, event, params, nparams)
Widget          w;
XEvent         *event;
String         *params;
Cardinal       *nparams;
{
    Widget v, mw;
    int i, n;
    Arg args[3];
    Dimension ww,rm;
    Window root,child;
    int x,y,cx,cy;
    unsigned int buttons;

    if (menu_bind_n == 0) return;
    v = XawSimpleMenuGetActiveEntry(w);

    if (v == NULL) {
	pop_down_subs(w);
	return;
    }
    for (i=0; i<menu_bind_n; i++) {
        if (menu_bind[i].target ==  v) {
            if (menu_bind[i].menu == NULL) {
                menu_bind[i].menu =  WcFullNameToWidget(Top, menu_bind[i].popup);
            }
	    if (menu_bind[i].menu == NULL) return;
	    if (menu_bind[i].up) return;
	    mw = menu_bind[i].menu;
	    /*
	     * Popdown old cacacde.
	     */
	    pop_down_subs(w);
	    /*
	     * Position new menu
	     */
	    n = 0;
	    XtSetArg(args[n], XtNwidth, &ww),n++;
	    XtSetArg(args[n], XtNrightMargin, &rm), n++;
	    XtGetValues(v, args, n);
	    if (rm == 0 || rm > ww) rm = (int)ww*20/100;
	    /*
	     * Position and popup menu. 
	     */
  	    x = w->core.x + ww;
	    y = w->core.y + v->core.y;
	    XtSetArg(args[0],XtNx,x);
    	    XtSetArg(args[1],XtNy,y);
    	    XtSetValues(mw,args,2);
	    menu_bind[i].up = 1;
    	    XtPopup(mw,XtGrabNonexclusive);
            return;
	}
    }
    pop_down_subs(w);
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
pop_down_subs(w)
Widget w;
{
    int i;

    for (i=0; i<menu_bind_n; i++) {
	if (menu_bind[i].up && XtParent(menu_bind[i].target) == w) {
	    pop_down_subs(menu_bind[i].menu);
            XtPopdown(menu_bind[i].menu);
	    menu_bind[i].up = 0;
	    return;
	}
    }
}

static void
pop_down_subs_act(w, event, params, nparams)
Widget          w;
XEvent         *event;
String         *params;
Cardinal       *nparams;
{
    Widget v;

    if (params == NULL) {
	pop_down_subs(w);
        XtPopdown(w);
    } else {
        v =  WcFullNameToWidget(Top, params[0]);
	pop_down_subs(v);
        XtPopdown(v);
    }
}

static void
pop_down_subs_cb(w, closure, call_data)
Widget w;
XtPointer closure;
XtPointer call_data;
{
    pop_down_subs(w);
    XtPopdown(w);
}

static void
set_form_horiz_cb(w, closure, call_data)
Widget w;
XtPointer closure;
XtPointer call_data;
{
    Arg arg[1];
    Widget h;
    Widget p;
    char *name = (char *)closure;

    h = WcFullNameToWidget(w, name);
    XtSetArg(arg[0], XtNfromHoriz, h);
    XtSetValues(w, arg, XtNumber(arg));
}

static void
set_form_vert_cb(w, closure, call_data)
Widget w;
XtPointer closure;
XtPointer call_data;
{
    Arg arg[1];
    Widget v;
    Widget p;
    char *name = (char *)closure;

    v = WcFullNameToWidget(w, name);
    XtSetArg(arg[0], XtNfromVert, v);
    XtSetValues(w, arg, XtNumber(arg));
}

static void
set_retry_cb(w, stuff, unused)
Widget  w;
char    *stuff;
caddr_t unused;
{
    Arg arg[1];
    extern XtIntervalId timeout_retry;
    extern int retry;
    Boolean state;

    XtSetArg(arg[0], XtNstate, &state);
    XtGetValues(w, arg, XtNumber(arg));
    retry = state;

    if(timeout_retry) {
        XtRemoveTimeOut(timeout_retry);
        timeout_retry = 0;
    }

}

static void
set_use_gateway_cb(w, closure, call_data)
Widget  w;
XtPointer closure;
XtPointer call_data;
{
    Arg arg[1];
    extern int use_gateway;
    Boolean state;

    XtSetArg(arg[0], XtNstate, &state);
    XtGetValues(w, arg, XtNumber(arg));
    use_gateway = state;
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
    XtSetArg(args[0], XtNtype, XawAsciiFile);
    XtSetArg(args[1], XtNstring, file);
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
     int width = 0;
     int add = 0;
     char *cp1 = NULL;
     char *cp2 = NULL;
     int ave;

     if (!FS) {
     	XtGetSubresources(Top, (XtPointer)&font_res, "*font", "*font", r_font, 
			  XtNumber(r_font), NULL, 0);
        FS = font_res.font;
	if (!font_res.width_in_chars) font_res.width_in_chars = 55;
     }
     if (!FS) return;
     if (closure) {
	cp1 = XtNewString((char *)closure);
	if (cp2 = INDEX(cp1, ',')) {
	    *cp2 = '\0';
	    cp2++;
	    add = atoi(cp2);
	}
	width = atoi(cp1);
  	XtFree(cp1);
     }
     if (width <= 0) width = 1;
     ave = (FS->max_bounds.width + FS->min_bounds.width)/2;
     width = (font_res.width_in_chars*ave + add)/width;
     XtSetArg(arg[0], XtNwidth, width);
     XtSetValues(w, arg, XtNumber(arg));
}

static void
warp_pointer_act(w, event, params, nparams)
Widget          w;
XEvent         *event;
String         *params;
Cardinal       *nparams;
{
    Arg args[2];
    int  i;
    Dimension width, height;
    
    if (*nparams != 1) return;
    for (i=0; i < NO_CONNECT; i++) 
	if(connect_info[i].w == w)  break;
    if (i == NO_CONNECT) return;
         if (strncmp(params[0],"up"  , 2) == 0) i--;
    else if (strncmp(params[0],"down", 4) == 0) i++;
    else return;
    if (i < 0) i = NO_CONNECT - 1;
    if (i == NO_CONNECT) i = 0;
    XtSetArg(args[0], XtNwidth, &width);
    XtSetArg(args[1], XtNheight, &height);
    XtGetValues(connect_info[i].w, args, XtNumber(args));
    XWarpPointer(XtDisplay(w), None, XtWindow(connect_info[i].w),
		 0, 0, 0,0, 0, height/2);
}

static void
wm_delete_act(w, event, params, num_params)
Widget w;
XEvent * event;
String * params;
Cardinal * num_params;
{
    int i;
    if (*num_params <= 0) return;
    
    i = atoi(params[0]);
    if (list_d[i].callback == NULL) {
	fprintf(stderr, "Wm_delete:No callback?\n");
	exit(1);
    } 
    list_d[i].callback(w, NULL, NULL);
}

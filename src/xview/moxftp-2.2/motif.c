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
"$Id: motif.c,v 1.2 1994/05/20 20:17:43 jones Exp $\n";
static char rcspath[] =
"$Source: /mintaka/u0/xx/ss/jones/jones.old/mftp.src/moxftp-2.0/RCS/motif.c,v $\n";
#endif

/* $Log: motif.c,v $
 * Revision 1.2  1994/05/20  20:17:43  jones
 * Do not depned on X11R5 NULL value XtGetValues trick.
 *
 * Revision 1.1  1994/03/14  18:55:53  jones
 * Initial revision
 *
 */

#include "machine.h"
#include "defs.h"

#include <sys/stat.h>
#include <X11/Xlib.h>
#include <X11/Intrinsic.h>
#include <X11/cursorfont.h>
#include <Xm/Xm.h>
#include <Xm/XmP.h>
#include <Xm/Text.h>
#include <Xm/ScrollBar.h>
#include <Xm/Protocols.h>
#include <Xm/SelectioB.h>
#include <Xm/MessageB.h>
#include "wc.h"
#include "List.h"

/*
 * Defines.
 */
#define XMSLR(string) \
 XmStringCreateLtoR(string, (XmStringCharSet)XmSTRING_DEFAULT_CHARSET)

/*
 * Prototype defintions.
 */
#include "proto/motif.h"

/*
 * Externals.
 */
extern Widget Top;
extern XtAppContext  App_context;
extern char pass[];
extern void cb_cd();
extern int use_gateway;
extern char *gateway;
extern int do_animate;


/*
 * Static
 */
static Widget Retry_w;
static Widget Sw = NULL;
static int item = -1;
static XmTextPosition  StartPos;

/*
 * Globals.
 */
XtIntervalId help_context = 0;


typedef struct _get_font_t {
	XmFontList      font;
 	int width_in_chars; 
} get_font, *get_font_p;

get_font font_res;

static XtResource r_font[] =
{
    {"width_in_chars", "Width_in_chars", XtRInt,  sizeof(int),
       XtOffset(get_font_p, width_in_chars), XtRImmediate, (XtPointer)55 },
};

static XFontStruct *FS;


char *view_file_name = NULL;


static XtActionsRec Actions[] = {
    {"MyPopupACT"         , my_popup_act},
    {"InsertSpace"        , insert_space_act},
    {"Dispatch"           , dispatch_act},
    {"insert_char"        , insert_char_act},
    {"delete_char"        , delete_char_act},
    {"resizebb"		  , resizebb_act},
    {"travers"	          , travers_act}
};


void
AppendDialogText(w, s)
Widget w;
char   *s;
{
    XmTextPosition LastPos;
    
    StartPos = XmTextGetLastPosition(w);
    XmTextInsert(w, StartPos, s);
    LastPos =  XmTextGetLastPosition(w);
    XmTextSetInsertionPosition(w, LastPos);
    StartPos = LastPos;
}

void
AppendStatusText(w, s)
Widget w;
char  *s;
{
    XmTextPosition LastPos;
    
    LastPos = XmTextGetLastPosition(w);
    XmTextInsert(w, LastPos, s);
    LastPos =  XmTextGetLastPosition(w);
    XmTextSetInsertionPosition(w, LastPos);
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
    Widget w;
    XmString ls, b1, b2, b3, b4;
    Arg args[5];
    int n = 0;

    ls  = XMSLR(label);
    XtSetArg(args[n], XmNselectionLabelString, ls); n++;

    if (button1 != NULL) {
	b1 = XMSLR(button1);
        XtSetArg(args[n], XmNokLabelString, b1); n++;
    }

    if (button2 != NULL) {
	b2 = XMSLR(button2);
	XtSetArg(args[n], XmNapplyLabelString, b2); n++;
    }

    if (button3 != NULL) {
	b3 = XMSLR(button3);
	XtSetArg(args[n], XmNcancelLabelString, b3); n++;
    }

    if (help != NULL) {
	b4 = XMSLR(help);
	XtSetArg(args[n], XmNhelpLabelString, b4); n++;
    }
    w = XmCreatePromptDialog(parent, name, args, n);

    if (button1 != NULL) {
	XmStringFree(b1);
	XtAddCallback(w, XmNokCallback, callback, (XtPointer)v1);
    } else {
        XtUnmanageChild (XmSelectionBoxGetChild(w, XmDIALOG_OK_BUTTON));
    }

    if (button2 != NULL) {
	XmStringFree(b2);
	XtManageChild (XmSelectionBoxGetChild(w, XmDIALOG_APPLY_BUTTON));
	XtAddCallback(w, XmNapplyCallback, callback, (XtPointer)v2);
    }

    if (button3 != NULL) {
	XmStringFree(b3);
	XtAddCallback(w, XmNcancelCallback, callback, (XtPointer)v3);
    } else {
        XtUnmanageChild (XmSelectionBoxGetChild(w, XmDIALOG_CANCEL_BUTTON));
    }
    if (help != NULL) {
	XmStringFree(b4);
	XtAddCallback(w, XmNhelpCallback, callback, (XtPointer)v3);
    } else {
        XtUnmanageChild (XmSelectionBoxGetChild(w, XmDIALOG_HELP_BUTTON));
    }

    if (!input) {
            XtUnmanageChild (XmSelectionBoxGetChild(w, XmDIALOG_TEXT));
    }

    if (help) {
	XtAddCallback(w, XmNhelpCallback, (XtCallbackProc)help_cb, NULL);
    }
    return w;
}

char *
GetDialogText(w)
Widget w;
{
   char *GetText();

   return(GetText(XmSelectionBoxGetChild(w, XmDIALOG_TEXT)));
}

char *
GetLabel(w)
Widget w;
{
    Arg arg[1];
    char text1[1000];
    char *text = text1;
    XmString x;

    XtSetArg(arg[0], XmNlabelString, x);
    XtGetValues(w, arg, XtNumber(arg));
    XmStringGetLtoR((XmString)x, 
		    (XmStringCharSet)XmSTRING_DEFAULT_CHARSET,
		    &text); 
    if (!text) return XtNewString("");
    return text;
}

char *
GetText(w)
Widget w;
{
    char *new;
    char *XmTextGetString();

    new = XmTextGetString(w);
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
    Widget shell = top;

    if (!XtIsWidget(top)) return 0;

    if (!XtIsShell(shell)) w = XtParent(shell);

    w = _XmGetActiveItem(shell);
    if (w == NULL) return 0;

    if (top == w) return 1;

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
    XtVaSetValues(w, XmNset, TRUE, NULL);
}

void
RegisterWidgets(App_context)
XtAppContext App_context;
{
    /*
     * Register Motif Widgets.
     */
    MriRegisterMotif ( App_context );
}

void
Reset_D_String(w)
Widget w;
{
    SetText(XmSelectionBoxGetChild(w, XmDIALOG_TEXT), "\0");
}

void
Reset_Gateway_Button()
{
    Arg arg[1];

    SetWidget(Retry_w,"*DoGateway" , 1);
    XtSetArg(arg[0], XmNset, FALSE);
    XtSetValues(Retry_w, arg, XtNumber(arg));
}

void
Reset_Retry_Button()
{
    Arg arg[1];

    SetWidget(Retry_w,"*DoRetry" , 1);
    XtSetArg(arg[0], XmNset, FALSE);
    XtSetValues(Retry_w, arg, XtNumber(arg));
}

void
SetDialogLabel(w, label)
Widget w;
char   *label;
{
    Arg arg[1];
    XmString l;
    int n = 0;

    l  = XMSLR(label);
    XtSetArg(arg[n], XmNselectionLabelString, l); n++;
    XtSetValues(w, arg, XtNumber(arg));
}

void
SetLabel(w, string)
Widget w;
char   *string;
{
    Arg arg[1];
    XmString label;

    label = XmStringCreateLtoR(string, 
		               (XmStringCharSet)XmSTRING_DEFAULT_CHARSET);
    XtSetArg(arg[0], XmNlabelString, label);
    XtSetValues(w, arg, XtNumber(arg));
    XmStringFree(label);
}

void
SetText(w, text)
Widget w;
char   *text;
{
    XmTextSetString(w, text);
}

void
Unmark_Menu(w)
Widget w;
{
    XtVaSetValues(w, XmNset, FALSE, NULL);
}

void
View_The_File(file, name)
char *file;
char *name;
{
    Widget  View_shell = NULL;

    view_file_name = XtNewString(file);

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
    Atom wm_delete_window;
    wm_delete_window = XInternAtom(XtDisplay(w), "WM_DELETE_WINDOW", False);
    XmAddWMProtocolCallback(w, wm_delete_window, callback, closure);
}

static void 
delete_char_act(w, event, params, num_params)
Widget w;
XEvent *event;
String *params;
Cardinal *num_params;
{
    XmTextPosition StartPos;

    StartPos = XmTextGetLastPosition(w); 
    if (!StartPos) return;
    StartPos -= 1;
    pass[StartPos] = '\0';
    XmTextFieldReplace(w, StartPos,StartPos+1, "");
    XmTextFieldSetInsertionPosition(w, StartPos);
}

static void
delete_view(w)
Widget w;
{
    XtDestroyWidget(w);
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
    char *dialog;
    char *s;

    dialog = XmTextGetString(w);
    s = XtNewString(dialog+StartPos);
    StartPos += strlen(s);
    Peek(s);
    XtFree(dialog);
    XtFree(s);
}

static void
help_cb(w, data, unused)
Widget  w;
caddr_t data;
caddr_t unused;
{
    char *stuff = (char *)data;
    XmAnyCallbackStruct *cb = (XmAnyCallbackStruct *)unused;
    Widget hw;
    void help_context_f();

    if (!stuff || !*stuff) {
	if (!cb->event) {
             Help(w, NULL , 0);
             return;
        }
        switch (cb->event->type) {
            case KeyPress:
                hw = XtWindowToWidget(XtDisplay(w), cb->event->xkey.window);
                if (hw) Help(hw, NULL, 0);
                else    Help(w , NULL, 0);
                return;
            default:
                Help(w, NULL, 0);
		return;
        }
    }
    /*
     * Could not figure out any other way to do this.
     */
    Set_Status_No_Log("Place cursor on window and click");
    help_context =  XtAppAddTimeOut(App_context,
                     (unsigned long)(1),
                     (XtTimerCallbackProc)help_context_f,
                     (XtPointer)NULL);
}

static void
help_context_f()
{
    Cursor Busy = NULL;
    Widget hw = NULL;;

    XtRemoveTimeOut(help_context);
    Busy = XCreateFontCursor (XtDisplay(Top), XC_crosshair);
    hw = XmTrackingLocate(Top,  Busy, False);
    if (hw != NULL) {
        Help(hw, NULL , 0);
    } else {
	Help(Top, NULL,  0);
    }
    Restore_Last_Status_Msg();
    return;
}

void
warning_msg(msg)
String msg;
{
    if (bsdstrstr(msg, "not supported in font") != NULL) return;
    if (bsdstrstr(msg, "Text widget is editable, Traversal_on") != NULL) return;
    _XtDefaultWarning(msg);
}


void
init_motif_acctions()
{
#if defined(HAS_EDITRES)
    extern void _XEditResCheckMessages();
#endif
 
    XtAppSetWarningHandler(App_context, warning_msg);
    XtAppAddActions(App_context, Actions,
                             sizeof(Actions)/sizeof(XtActionsRec));

    RCALL("Clear_Text"         , clear_text_cb);
    RCALL("set_width"	       , set_width_cb);
    RCALL("Set_retry"          , set_retry_cb);
    RCALL("set_view_file"      , set_view_file_cb);
    RCALL("Help"               , help_cb);
    RCALL("set_view_text_top"  , set_view_text_top_cb);
    RCALL("Set_use_gateway"    , set_use_gateway_cb);
    RCALL("PositionDialog"     , position_dialog_cb);
    RCALL("travers"	       , travers_cb);

    /* 
     * Cannot animate icon with motif window manager.
     */
    if (XmIsMotifWMRunning(Top)) {
	do_animate = 0;
    }

#if defined(HAS_EDITRES)
    XtAddEventHandler(Top, (EventMask)0, True,
		      _XEditResCheckMessages, (XtPointer)NULL);
#endif
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
    XmTextPosition StartPos;
     
    n = XLookupString((XKeyEvent*)event, input, sizeof(input), (KeySym *) NULL,
	              &status);
    if (!n) return;
    StartPos = XmTextGetLastPosition(w);
    strncpy((char *)&pass[StartPos], input, n);
    pass[StartPos+n] = '\0';
    for (i=0; i<n; i++) input[i] = 'X'; 
    input[n] = '\0';
    XmTextInsert(w, StartPos, input);
}

static void 
insert_space_act(w, event, params, num_params)
Widget w;
XEvent *event;
String *params;
Cardinal *num_params;
{
   if (XmTextGetInsertionPosition(w) <= StartPos) 
	XmTextInsert(w, StartPos, " ");
}

static void
my_popup_act(w, event, params, num_params )
Widget      w;
XEvent     *event;
String     *params;
Cardinal   *num_params;
{
    Widget  Popup;

    if (Is_Noop(w)) return;
    if (*num_params < 1) return;
    Popup = WcFullNameToWidget (Top, params[0] );
    XmMenuPosition(Popup, event);
    XtManageChild(Popup);
}

static void
position_dialog_cb(w, client_data, call_data)
Widget  w;
caddr_t client_data;
caddr_t call_data;
{
    int i, n;
    Arg args[3];
    Dimension ww,rm;
    Window root,child;
    int x,y,cx,cy;
    unsigned int buttons;
    Widget  v;

    /*
     * Position Dialog
     */
     XQueryPointer(XtDisplay(w),XtWindow(w),
                              &root,&child,&x,&y,&cx,&cy,&buttons);
     XtRealizeWidget(w);
 

    v = XmSelectionBoxGetChild(w, XmDIALOG_TEXT);
    if (v) {
        Position xw, yw;
        XtVaGetValues(v, XmNx, &xw, XmNy, &yw, NULL);
	x -= xw + 10 ;
	y -= yw + 10 ;
    } else if (w->core.width > 20 && w->core.height > 20) {
         x -= 20;
         y -= 20;
     } else {
         x -= 1;
         y -= 1;
     }
 
    XtSetArg(args[0],XtNx,x);
    XtSetArg(args[1],XtNy,y);
    XtSetValues(w,args,2);

}

static void 
resizebb_act(w, event, params, num_params)
Widget w;
XEvent     *event;
String *params;
Cardinal *num_params;
{
     XConfigureEvent *cf;
     WidgetList children;
     short margin_h, margin_w;
     Dimension width;
     Dimension ww;


     XtVaGetValues(w,
                   XmNchildren, &children,
                   XmNmarginHeight, &margin_h,
                   XmNmarginWidth, &margin_w,
                   XmNwidth, &ww,
                   NULL);

    if (event->type == ConfigureNotify) {
	cf = (XConfigureEvent *)event;
	ww  = cf->width;
    }

    XtVaSetValues(children[0], 
		  XmNx, margin_w, 
		  XmNy, margin_h, 
		  NULL);

    XtVaGetValues(children[1], 
		  XmNwidth, &width, 
		  NULL);
 
    XtVaSetValues(children[1], 
		  XmNx, (int)(ww - width)/2,
		  XmNy, margin_h,
		  NULL);

    XtVaSetValues(children[2], 
		  XmNx, ww - margin_w - width,
		  XmNy, margin_h,
		  NULL);
}

static void
set_retry_cb(w, client_data, call_data)
Widget  w;
caddr_t client_data;
caddr_t call_data;
{
    XmToggleButtonCallbackStruct *x = (XmToggleButtonCallbackStruct *)call_data;
    extern XtIntervalId timeout_retry;
    extern int retry;

    retry = x->set;
    if(timeout_retry) {
        XtRemoveTimeOut(timeout_retry);
        timeout_retry = 0;
    }
}

static void
set_use_gateway_cb(w, client_data, call_data)
Widget  w;
caddr_t client_data;
caddr_t call_data;
{
    Arg arg[1];
    XmToggleButtonCallbackStruct *x = (XmToggleButtonCallbackStruct *)call_data;

    use_gateway = x->set;
}

static void
set_view_file_cb(w, closure, call_data)
Widget w;
XtPointer closure;
XtPointer call_data;
{
    Arg args[2];
    char *file;
    char *buff;
    struct stat statbuf;
    int fd;

    if (view_file_name == NULL) return;
    stat(view_file_name, &statbuf);
    if (statbuf.st_size) {
	buff = XtMalloc(statbuf.st_size+1);
	fd = open(view_file_name, O_RDONLY);
	read(fd, buff, statbuf.st_size);
	close(fd);
	buff[statbuf.st_size] = '\0';
        XmTextSetString(w, buff);
	XtFree(buff);
    }
    unlink(view_file_name);
    XtFree(view_file_name);
    view_file_name = NULL;
}

static void
set_view_text_top_cb(w, closure, call_data)
Widget w;
XtPointer closure;
XtPointer call_data;
{
    Arg arg[1];
    Widget h;
    Widget p;
    char *name = (char *)closure;

    p = XtParent(w);
    h = WcFullNameToWidget(p, name);
    if (!h || !p) {
	fprintf(stderr, "not h %x  p %x\n",h,p);
	return;
    }
    XtSetArg(arg[0], XmNtopWidget, h);
    XtSetValues(p, arg, XtNumber(arg));
}

static void
set_width_cb(w, closure, call_data)
Widget w;
XtPointer closure;
XtPointer call_data;
{
     Arg arg[1];
     int width = 0;
     int sub = 0;
     char *cp1 = NULL;
     char *cp2 = NULL;
     int ave;

     if (!FS) {
	XmFontList fontlist;
     	XtGetSubresources(Top, (XtPointer)&font_res, 
			  "*mftp", "*Mftp", r_font, 
			  XtNumber(r_font), NULL, (Cardinal)0);
	
	fontlist = _XmGetDefaultFontList( (Widget) w, XmLABEL_FONTLIST);
	_XmFontListGetDefaultFont(fontlist, &FS);
	if (!font_res.width_in_chars) font_res.width_in_chars = 55;
     }
     if (!FS) return;
     if (closure) {
	cp1 = XtNewString((char *)closure);
	if (cp2 = INDEX(cp1, ',')) {
	    *cp2 = '\0';
	    cp2++;
	    sub = atoi(cp2);
	}
	width = atoi(cp1);
  	XtFree(cp1);
     }
     if (width <= 0) width = 1;
     ave = (FS->max_bounds.width + FS->min_bounds.width)/2;
     width = (font_res.width_in_chars*ave - sub)/width;
     XtSetArg(arg[0], XmNwidth, width);
     XtSetValues(w, arg, XtNumber(arg));
}

static void 
travers_act(w, event, params, num_params)
Widget w;
XEvent     *event;
String *params;
Cardinal *num_params;
{
    traverse(w);
}

static void
travers_cb(w, client_data, call_data)
Widget  w;
caddr_t client_data;
caddr_t call_data;
{
    Widget y;
    char *name = (char *)client_data;

    y = WcFullNameToWidget (Top, name);
    if (y == NULL) return;
    traverse(y);
}

static void
traverse(w)
Widget w;
{
    Widget v;

    v = XmSelectionBoxGetChild(w, XmDIALOG_TEXT);
    if (v) {
	if (XmProcessTraversal(v, XmTRAVERSE_CURRENT)) return;
	if (!XmProcessTraversal(v, XmTRAVERSE_NEXT_TAB_GROUP)) return;
	XmProcessTraversal(v, XmTRAVERSE_CURRENT);
    }
}

void
clear_text_cb(w, closure, call_data)
Widget w;
XtPointer closure;
XtPointer call_data;
{
    char *name = (char *)closure;
    Widget v;
    XmTextPosition EndPos;
    XmTextPosition StartPos;

    v = WcFullNameToWidget(Top, name);
    if (!v) return;
    EndPos = XmTextGetLastPosition(v); 
    StartPos = 0;
    XmTextReplace(v, StartPos, EndPos, "");
    XmTextSetInsertionPosition(v, StartPos);
    if (strcmp(name, "*ftp") == 0) Do_Prompt(NULL, 1);
}

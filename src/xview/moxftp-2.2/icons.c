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
"$Id: icons.c,v 1.2 1994/05/20 20:14:54 jones Exp $\n";
static char rcspath[] =
"$Source: /mintaka/u0/xx/ss/jones/jones.old/mftp.src/moxftp-2.0/RCS/icons.c,v $\n";
#endif

/* $Log: icons.c,v $
 * Revision 1.2  1994/05/20  20:14:54  jones
 * Load times bold 18 if times bold 20 fails.
 *
 * Revision 1.1  1994/03/14  18:55:53  jones
 * Initial revision
 *
 */

#define RATE  2
#define FRAME 8

#include "machine.h"
#include "defs.h"
#include "class.h"
#include <X11/Xlib.h>
#include "proto/icons.h"

#include "truck.xbm"

#define MAX_W 10
int num_w = 0;
static struct {
	Window  icon;
	Widget  w;
	char    *name;
	int     used;
} widget_to_window[MAX_W];

static struct {
     char   *name;
     char   *state;
     int    a;
     int    frame;
     XtIntervalId   timeout;
     Window icon;
     Widget w;
     char   *title;
     Pixmap pixmap;
     Pixmap bitmap;
     Pixmap animate;
     Pixmap animate_proto;
} shell_icons[] = {
{ "comm"      , "Command"     , 0, 0, 0, 0, NULL, NULL, 0, 0},
{"status"     , "Status"      , 0, 0, 0, 0, NULL, NULL, 0, 0},
{ "tran"      , "Translate"   , 0, 0, 0, 0, NULL, NULL, 0, 0},
{ "help"      , "Help"        , 0, 0, 0, 0, NULL, NULL, 0, 0},
{ "view"      , "View File"   , 0, 0, 0, 0, NULL, NULL, 0, 0},
{ "archie"    , "Archie"      , 0, 0, 0, 0, NULL, NULL, 0, 0},
{ "connect"   , "Connect"     , 0, 0, 0, 0, NULL, NULL, 0, 0},

{ "connecting", "Connecting"  , 1, 0, 0, 0, NULL, NULL, 0, 0},
{ "reconnect",  "Reconnect"   , 1, 0, 0, 0, NULL, NULL, 0, 0},
{ "connected" , "Connected"   , 1, 0, 0, 0, NULL, NULL, 0, 0},
{ "disconnect", "Ready"       , 1, 0, 0, 0, NULL, NULL, 0, 0},
{ "retry"     , "Retry"       , 1, 0, 0, 0, NULL, NULL, 0, 0},
{ "busy1"     , "Busy"	      , 1, 0, 0, 0, NULL, NULL, 0, 0},
{ "busy2"     , "Fetching"    , 1, 0, 0, 0, NULL, NULL, 0, 0},
{ "busy3"     , "Storing"     , 1, 0, 0, 0, NULL, NULL, 0, 0},
{ "busy4"     , "Listing"     , 1, 0, 0, 0, NULL, NULL, 0, 0},
{  NULL       ,  NULL         , 0, 0, 0, 0, NULL, NULL, 0, 0},
};


static GC gc_black = NULL;
static GC gc_white = NULL;
static GC gc_copy  = NULL;
static XFontStruct *font = NULL;
static int FH = 20;
static Pixmap pixmap_proto = 0;
static Pixmap animate;
static Pixmap truck = 0;
static int icon_h = 64;
static int icon_w = 64;
static int icon_info_h = 14;
       int do_animate = 0; 

/*
 * Externals.
 */
extern Widget Top;
extern XtAppContext App_context;

/*
 * Position macros.
 */
#define POSITION_TRUCK(x, y) \
    x = (icon_w - truck_width)/2; \
    y = icon_h/2 - truck_height/2 + 4;

#define POSITION_STATE(x, y) \
	x = (icon_w-sw)/2; \
	y = icon_h - icon_info_h - 4 + font->max_bounds.ascent;

#define POSITION_NAME(x, y, w) \
    x = (icon_w - w)/2; \
    y = 1 + font->max_bounds.ascent;

int indexx = -1;


void
Animate(w)
Widget w;
{
    int i;

    if (!do_animate) return;
    for (i=0; shell_icons[i].name != NULL; i++) { 
	if (shell_icons[i].w == w) break;
    }

    if (shell_icons[i].name == NULL) return;
    if (shell_icons[i].timeout) return;
    do_animate_icon((XtPointer)w, NULL);
}


void
Set_Icon(shell, name)
Widget  shell;
char   *name;
{
    Arg arg[1];
    int i,j;
    int sw;
    int x, y;
    Window  icon;


    for (i=0; shell_icons[i].name != NULL; i++) { 
	if (strcmp(shell_icons[i].name, name) == 0) break;
    }

    if (shell_icons[i].name == NULL) {
	fprintf(stderr, "Could not find Icon %s\n",name);
	return;
    }

    init_proto(shell);
	
    if (shell_icons[i].pixmap == 0) {
	shell_icons[i].bitmap = XCreatePixmap(XtDisplay(shell),
                                   	       	        XtWindow(shell),
					                icon_w,
					      	        icon_h,
                                   	      	        1);
	shell_icons[i].pixmap = XCreatePixmap(XtDisplay(shell),
                                   	       	        XtWindow(shell),
					                icon_w,
					      	        icon_h,
			                 DefaultDepth(XtDisplay(shell),
				        DefaultScreen(XtDisplay(shell))));
	XCopyArea(XtDisplay(shell), 
		  pixmap_proto, 
		  shell_icons[i].bitmap,
		  gc_black,
		  0, 0, icon_w, icon_h,
		  0, 0);
        sw = XTextWidth(font, 
			shell_icons[i].state, 
		        strlen(shell_icons[i].state));
        POSITION_STATE(x, y)
        XDrawImageString(XtDisplay(shell), 
			 shell_icons[i].bitmap,
			 gc_black, 
			 x,
			 y,
			 shell_icons[i].state,
			 strlen(shell_icons[i].state));
	if (do_animate && shell_icons[i].a) {
            shell_icons[i].animate_proto = XCreatePixmap(XtDisplay(shell),
                                   	   	         XtWindow(shell),
					   	         icon_w,
					   	         icon_h,
                                   	      	         1);
            shell_icons[i].animate = XCreatePixmap(XtDisplay(shell),
                                   	   	     XtWindow(shell),
					   	     icon_w,
					   	     icon_h,
                                   	      	     1);
	    XCopyArea(XtDisplay(shell), 
		      animate,
       		      shell_icons[i].animate_proto,
		      gc_black,
		      0, 0, icon_w, icon_h,
		      0, 0);

            POSITION_STATE(x, y)
	    XDrawImageString(XtDisplay(shell), 
       			     shell_icons[i].animate_proto,
			     gc_black, 
			     x,
			     y,
			     shell_icons[i].state,
			     strlen(shell_icons[i].state));
	}
    }
    for (j=0; shell_icons[j].name != NULL; j++) { 
	if (shell_icons[j].w == shell) {
	  shell_icons[j].w = NULL;
          if (shell_icons[j].timeout) 
		XtRemoveTimeOut(shell_icons[j].timeout);  
          shell_icons[j].timeout = 0;
	}
    }

    if (do_animate) {
	int notused = -1;
        for (j=0; j<num_w; j++) {
	    if (widget_to_window[j].used) notused = j;
	    if (widget_to_window[j].w == shell) break;
        }
        if (j == num_w) {
	    if (notused == -1) {
		num_w++;
	    } else {
		j = notused;
	    }
	    widget_to_window[j].name = NULL;
	    widget_to_window[j].w = shell;
	    widget_to_window[j].used = 1;
            widget_to_window[j].icon = XCreateSimpleWindow(XtDisplay(shell),
				RootWindowOfScreen(XtScreen(shell)),
						       0,0, icon_w, icon_h,
						       1, 
			  			       getpixel("black"),
			  			       getpixel("black"));

            XSetWindowBackgroundPixmap(XtDisplay(shell),  
				       widget_to_window[j].icon, 
			               shell_icons[i].pixmap);
	} 
        icon = widget_to_window[j].icon;
	if (shell_icons[i].a) 
	    shell_icons[i].icon = icon;
    }

    if (shell_icons[i].timeout) XtRemoveTimeOut(shell_icons[i].timeout);  
    indexx = -1;
    shell_icons[i].timeout = 0;
    shell_icons[i].frame = 0;
    shell_icons[i].w = shell;

    if (do_animate) {
          XCopyPlane(XtDisplay(shell), shell_icons[i].bitmap,  
	           shell_icons[i].pixmap, gc_copy,
                   0,0, icon_w, icon_h, 0, 0, 1 );
          XCopyArea(XtDisplay(shell), 
     	          shell_icons[i].pixmap, 
	          icon,
	      gc_copy,
	      0, 0, icon_w, icon_h,
	      0, 0);

         XSetWindowBackgroundPixmap(XtDisplay(shell), icon, 
			            shell_icons[i].pixmap);
         XtSetArg(arg[0], XtNiconWindow, icon);
         XtSetValues(shell, arg, 1);
    } else {
         XtSetArg(arg[0], XtNiconPixmap, shell_icons[i].bitmap);
         XtSetValues(shell, arg, 1);
    }
}

void
Set_Wn_Name(name)
char *name;
{
    char *title = name;

    if (!title) title = Ftpname;
    XSetIconName(XtDisplay(Top), XtWindow(Top), title);
}

static void
do_animate_icon(client_data, id)
caddr_t client_data;
XtIntervalId *id;
{
    Arg arg[1];
    int i;
    Widget w = (Widget) client_data;
    int x, y;

    for (i=0; shell_icons[i].name != NULL; i++) { 
	if (shell_icons[i].w == w) break;
    }

    if (shell_icons[i].name == NULL) return;
    POSITION_TRUCK(x, y)
    x += shell_icons[i].frame;
    if (x >= icon_w) {
        POSITION_TRUCK(x, y)
        shell_icons[i].frame = - x;
        x = 0;
    }
    XCopyArea(XtDisplay(w), 
       	      shell_icons[i].animate_proto,
       	      shell_icons[i].animate,
	      gc_black,
	      0, 0, icon_w, icon_h,
	      0, 0);
    XCopyArea(XtDisplay(w),
              truck,
       	      shell_icons[i].animate,
              gc_black,
              0, 0, truck_width, truck_height,
              x, y);
    if ((x + truck_width) > icon_w) {
            XCopyArea(XtDisplay(w),
                      truck,
       	              shell_icons[i].animate,
                      gc_black,
                      truck_width - (x + truck_width - icon_w), 0,
                      (x + truck_width - icon_w), truck_height,
                      0 , y);
    }

    XCopyPlane(XtDisplay(w), shell_icons[i].animate,  
	       shell_icons[i].pixmap, gc_copy,
               0,0, icon_w, icon_h, 0, 0, 1 );

    XCopyArea(XtDisplay(w), 
     	      shell_icons[i].pixmap, 
	      shell_icons[i].icon,
	      gc_copy,
	      0, 0, icon_w, icon_h,
	      0, 0);

    shell_icons[i].frame += FRAME;
    shell_icons[i].timeout =  XtAppAddTimeOut(App_context,
                                              (unsigned long)
					         ((int)((1./RATE)*1000)),
                                         (XtTimerCallbackProc)do_animate_icon,
                                              (XtPointer)w);
    XSync(XtDisplay(Top), False);
}

static Pixel
getpixel(name)
char *name;
{

    if (strcmp("black", name) == 0) {
	return (BlackPixel(XtDisplay(Top), 
				     DefaultScreen(XtDisplay(Top))));
    } else if (strcmp("white", name) == 0) {
	return (WhitePixel(XtDisplay(Top), 
				     DefaultScreen(XtDisplay(Top))));

    }
    return (BlackPixel(XtDisplay(Top), 
	  	       DefaultScreen(XtDisplay(Top))));
    
}

static void
init_proto(w)
Widget w;
{
    XGCValues    values;
    XtGCMask     valueMask;
    int x,y,sw,j;

    if (pixmap_proto != 0) return;

    truck = XCreateBitmapFromData(XtDisplay(w),
                                  XtWindow(w),
                                  (char *)truck_bits,
                                  truck_width,
                                  truck_height);
    pixmap_proto = XCreatePixmap(XtDisplay(w),
                                 XtWindow(w),
				 icon_w,
				 icon_h,
                                 1);
    font = XLoadQueryFont(XtDisplay(w), 
		          "-*-times-bold-r-*-*-20-*-*-*-*-*-*-*");
    if (font == NULL)
        font = XLoadQueryFont(XtDisplay(w), 
		              "-*-times-bold-r-*-*-18-*-*-*-*-*-*-*");

    valueMask = GCFunction | GCBackground | GCForeground | GCFont;
    values.font = font->fid;
    values.function     = GXcopy;
    values.background = 0;
    values.foreground = 1;
    gc_black = XCreateGC(XtDisplay(w), pixmap_proto,
			             valueMask, &values);
    
    values.background = 1;
    values.foreground = 0;
    gc_white = XCreateGC(XtDisplay(w),pixmap_proto, 
		   	 valueMask, &values);

    valueMask = GCFunction | GCBackground | GCForeground;
    values.function   = GXcopy;
    values.background = getpixel("white");
    values.foreground = getpixel("black");
    gc_copy = XCreateGC(XtDisplay(w), XtWindow(w), valueMask, &values);

    XFillRectangle(XtDisplay(w), pixmap_proto, gc_white, 0,0, 
		   icon_w, icon_h);
    sw = XTextWidth(font, XFTPICONNAME, strlen(XFTPICONNAME));

    x = (icon_w - sw)/2;
    y = (icon_h - (truck_height + 2) - icon_info_h)/2 + 
		      font->max_bounds.ascent - FH/2,
    POSITION_NAME(x, y, sw)
    XDrawImageString(XtDisplay(w), 
		     pixmap_proto, 
		     gc_black, 
		     x,
		     y,
		     XFTPICONNAME,
		     strlen(XFTPICONNAME));


    animate = XCreatePixmap(XtDisplay(w),
                            XtWindow(w),
			    icon_w,
			    icon_h,
                            1);
		
    XCopyArea(XtDisplay(w), 
	      pixmap_proto, 
	      animate,
	      gc_black,
	      0, 0, icon_w, icon_h,
	      0, 0);
    POSITION_TRUCK(x, y) 
    XCopyArea(XtDisplay(w), 
			truck, 
			pixmap_proto, 
			gc_black,
			0, 0, truck_width, truck_height,
			x, y);
    XFreeFont(XtDisplay(w), font);
    font = XLoadQueryFont(XtDisplay(w), 
			  "-*-helvetica-bold-r-*-*-12-*-*-*-*-*-*-*");
    XSetFont(XtDisplay(w), gc_black, font->fid);
}

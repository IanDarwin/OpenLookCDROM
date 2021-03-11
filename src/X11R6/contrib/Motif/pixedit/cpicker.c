/**********************************************************************
 Copyright (c) 1994 Mike Yang

 The X Consortium, and any party obtaining a copy of these files from
 the X Consortium, directly or indirectly, is granted, free of charge, a
 full and unrestricted irrevocable, world-wide, paid up, royalty-free,
 nonexclusive right and license to deal in this software and
 documentation files (the "Software"), including without limitation the
 rights to use, copy, modify, merge, publish, distribute, sublicense,
 and/or sell copies of the Software, and to permit persons who receive
 copies from any such party to do so.  This license includes without
 limitation a license to do the foregoing actions under any patents of
 the party supplying this software to the X Consortium.
**********************************************************************/

/*
 * cpicker.c - Colormap editor for X11
 * 
 * Author:	Mike Yang (mikey@sgi.com)
 *		Silicon Graphics, Inc.
 * Date:	Mon Jul 29 1991
 */

#include <stdio.h>
#include <math.h>
#include "Cpick.h"
#include <X11/Xproto.h>
#include <X11/cursorfont.h>
#include <Xm/Form.h>
#include <Xm/MessageB.h>

#define HELP_STRING	"Cpicker, a colormap editing utility.  Mike Yang, Silicon Graphics, mikey@sgi.com"

/* #define USE_COLORS	/* define to make the scale labels colored */

/* We do this because most servers support only the upper 8 bits for RGB */
#define SAME_COLOR(x,y)	((x)/256 == (y)/256 || (x)/256+1 == (y)/256 || (x)/256 == (y)/256+1)

XColor allocated, current;
Widget glevel, toplevel, cpick, dialog;
XColor mapv[MAXPIXELS];
Colormap cmap, oldMap;
Boolean picking = TRUE;
int maxpixels, numpixels, numfree, realfree;
Position choose_x = 200, choose_y = 200;
unsigned long avail[MAXPIXELS], got[MAXPIXELS];
Window win, cmapWin = NULL;
XmColorProc colorProc;
XColor fgColor, selectColor, topColor, bottomColor;
Boolean doMotif, haveFg, haveSelect, haveTop, haveBottom;

typedef unsigned long (*PixProc)();

struct vals {
  int ih, iw, x0, y0, rownum;
  PixProc pix;
} gval, nval;

Arg gargs[] = {
  {XmNborderColor, NULL},
  {XmNheight, 1},
  {XmNwidth, 1},
  {XmNtopAttachment, XmATTACH_FORM},
  {XmNbottomAttachment, XmATTACH_FORM},
  {XmNleftAttachment, XmATTACH_FORM},
  {XmNrightAttachment, XmATTACH_FORM},
};

Arg setargs[] = {
  {XmNallocated, (XtArgVal) &allocated},
};

static XrmOptionDescRec cmdOptions[] = {
  {
    "-motif", ".motif", XrmoptionIsArg, NULL,
  },
  {
    "-noMotif", ".noMotif", XrmoptionIsArg, NULL,
  },
  {
    "-multiple", ".multiple", XrmoptionIsArg, NULL,
  },
  {
    "-noMultiple", ".noMultiple", XrmoptionIsArg, NULL,
  },
};

static Arg args[20];
static int count;

changeColormap(dpy, win, frommap, tommap)
Display *dpy;
Window win;
Colormap frommap, tommap;
{
  XWindowAttributes xwa;
  Window *children, dummy;
  unsigned int nchildren, i;

  XGetWindowAttributes(dpy, win, &xwa);
  if (xwa.colormap == frommap) {
    XSetWindowColormap(dpy, win, tommap);
    if (XQueryTree(dpy, win, &dummy, &dummy, &children, &nchildren)) {
      for (i=0; i<nchildren; i++) {
	changeColormap(dpy, children[i], frommap, tommap);
      }
      XFree((char *) children);
    }
  }
}

Window
findTopLevelWindow(dpy, window)
Display *dpy;
Window window;
{
  static Atom WM_STATE = None;
  int rvalue;
  Atom actualtype;
  int actualformat;
  unsigned long nitems, bytesafter;
  unsigned char *propreturn;
  Window *children;
  unsigned int numchildren;
  Window returnroot, returnparent;

  if (WM_STATE == None) {
    WM_STATE = XInternAtom(dpy, "WM_STATE", False);
  }

  if (!window) {
    return NULL;
  }

  rvalue = XGetWindowProperty(dpy, window, WM_STATE,
			      0, 1, False,
                              AnyPropertyType, &actualtype, &actualformat,
                              &nitems, &bytesafter, &propreturn);
  if (rvalue == Success && actualtype != None) {
    if (rvalue == Success) {
      XtFree((char *) propreturn);
    }
    return window;
  }

  if (XQueryTree(dpy, window, &returnroot, &returnparent,
		 &children, &numchildren)) {
    return findTopLevelWindow(dpy, returnparent);
  } else {
    return NULL;
  }
}

usage()
{
    fprintf (stderr,
        "usage:  cpicker [-options ...]\n\n");
    fprintf (stderr,
        "where options include:\n");
    fprintf (stderr,
        "    -display host:dpy    X server to contact\n");
    fprintf (stderr,
        "    -root                use the root window\n");
    fprintf (stderr,
        "    -id windowid         use the window with the specified id\n");
    fprintf (stderr,
        "    -wname windowname    use the window with the specified name\n");
    fprintf (stderr,
	"    -motif               enable Motif color matching (default)\n");
    fprintf (stderr,
	"    -noMotif             disable Motif color matching\n");
    fprintf (stderr,
	"    -multiple            hardware supports multiple colormaps (default)\n");
    fprintf (stderr,
	"    -noMultiple          assume hardware supports single colormap\n");
}

/*
 * (code taken from xwininfo)
 *
 * Standard fatal error routine - call like printf but maximum of 7 arguments.
 * Does not require dpy or screen defined.
 */
void Fatal_Error(msg, arg0,arg1,arg2,arg3,arg4,arg5,arg6)
char *msg;
char *arg0, *arg1, *arg2, *arg3, *arg4, *arg5, *arg6;
{
        fflush(stdout);
        fflush(stderr);
        fprintf(stderr, "cpicker: error: ");
        fprintf(stderr, msg, arg0, arg1, arg2, arg3, arg4, arg5, arg6);
        fprintf(stderr, "\n");
        exit(1);
}

/*
 * (code taken from xwininfo)
 *
 * Window_With_Name: routine to locate a window with a given name on a display.
 *                   If no window with the given name is found, 0 is returned.
 *                   If more than one window has the given name, the first
 *                   one found will be returned.  Only top and its subwindows
 *                   are looked at.  Normally, top should be the RootWindow.
 */
Window Window_With_Name(dpy, top, name)
     Display *dpy;
     Window top;
     char *name;
{
	Window *children, dummy;
	unsigned int nchildren, i;
	Window w=0;
	char *window_name;

	if (XFetchName(dpy, top, &window_name) && !strcmp(window_name, name))
	  return(top);

	if (!XQueryTree(dpy, top, &dummy, &dummy, &children, &nchildren))
	  return(0);

	for (i=0; i<nchildren; i++) {
		w = Window_With_Name(dpy, children[i], name);
		if (w)
		  break;
	}
	XFree((char *) children);
	return(w);
}

/*
 * (code taken from xwininfo)
 *
 * Select_Window_Args: a rountine to provide a common interface for
 *                     applications that need to allow the user to select one
 *                     window on the screen for special consideration.
 *                     This routine implements the following command line
 *                     arguments:
 *
 *                       -root            Selects the root window.
 *                       -id <id>         Selects window with id <id>. <id> may
 *                                        be either in decimal or hex.
 *                       -wname <name>     Selects the window with name <name>.
 *
 *                     Call as Select_Window_Args(&argc, argv) in main before
 *                     parsing any of your program's command line arguments.
 *                     Select_Window_Args will remove its arguments so that
 *                     your program does not have to worry about them.
 *                     The window returned is the window selected or 0 if
 *                     none of the above arguments was present.  If 0 is
 *                     returned, Select_Window should probably be called after
 *                     all command line arguments, and other setup is done.
 *                     For examples of usage, see xwininfo, xwd, or xprop.
 */
Window Select_Window_Args(rargc, argv)
     int *rargc;
     char **argv;
#define ARGC (*rargc)
{
	int nargc=1;
	int argc;
	char **nargv;
	Window w=0;
	Display *dpy;
	int screen;

	nargv = argv+1; argc = ARGC;
#define OPTION argv[0]
#define NXTOPTP ++argv, --argc>0
#define NXTOPT if (++argv, --argc==0) usage()
#define COPYOPT nargv++[0]=OPTION; nargc++

	dpy =  XtDisplay(toplevel);
	screen =  XDefaultScreen(XtDisplay(toplevel));

	while (NXTOPTP) {
		if (!strcmp(OPTION, "-")) {
			COPYOPT;
			while (NXTOPTP)
			  COPYOPT;
			break;
		}
		if (!strcmp(OPTION, "-root")) {
			w=RootWindow(dpy, screen);
			continue;
		}
		if (!strcmp(OPTION, "-wname")) {
			NXTOPT;
			w = Window_With_Name(dpy, RootWindow(dpy, screen),
					     OPTION);
			if (!w)
			  Fatal_Error("No window with name %s exists!",OPTION);
			continue;
		}
		if (!strcmp(OPTION, "-id")) {
			NXTOPT;
			w=0;
			sscanf(OPTION, "0x%lx", &w);
			if (!w)
			  sscanf(OPTION, "%ld", &w);
			if (!w)
			  Fatal_Error("Invalid window id format: %s.", OPTION);
			continue;
		}
		COPYOPT;
	}
	ARGC = nargc;
	
	return(w);
}

void postDialog(msg)
char *msg;
{
  XmString xs;

  xs = XmStringCreateSimple(msg);
  count = 0;
  XtSetArg(args[count], XmNmessageString, xs);  count++;
  XtSetValues(dialog, args, count);
  XtManageChild(dialog);
}

Dimension getD(w, s)
Widget w;
String s;
{
  Dimension result;
  Arg args[1];
  int v;

  XtSetArg(args[0],s,(XtArgVal) &result);
  XtGetValues(w, args, XtNumber(args));
  v = result;

  return(v);
}

Position getP(w, s)
Widget w;
String s;
{
  Position result;
  Arg args[1];
  int v;

  XtSetArg(args[0],s,(XtArgVal) &result);
  XtGetValues(w, args, XtNumber(args));
  v = result;

  return(v);
}

gButtonHandler(w, val, event)
Widget w;
struct vals *val;
XButtonEvent *event;
{
  int newpix, each, each2;
  Widget bframe;
  XColor bgColor;

  picking = TRUE;
  newpix = val->pix((event->x-val->x0)/val->iw +
		    ((event->y-val->y0)/val->ih)*val->rownum);
  for (each=0; each<=realfree; each++) {
    if (newpix == avail[each]) {
      postDialog("That's one of the widget's colormap cells.  Pick another.");
      XBell(XtDisplay(w), 0);
      return;
    }
  }
  if (newpix < numpixels) {
    XtUnmapWidget(w);
    current.pixel = newpix;
    /* generates error if can't allocate */
    XQueryColor(XtDisplay(toplevel), cmap, &current);
    allocated.red = current.red;
    allocated.green = current.green;
    allocated.blue = current.blue;
    XSync(XtDisplay(toplevel), 0);
    if (picking) {
      /* generates error if can't allocate */
      XStoreColor(XtDisplay(toplevel), cmap, &current);
      XSync(XtDisplay(toplevel), 0);
      if (picking) {
	XtSetValues(cpick, setargs, XtNumber(setargs));
	XtUnmanageChild(dialog);
	XtMapWidget(cpick);
	if (doMotif) {
	  bgColor.pixel = current.pixel;
	  bgColor.red = mapv[current.pixel].red;
	  bgColor.green = mapv[current.pixel].green;
	  bgColor.blue = mapv[current.pixel].blue;
	  (*colorProc)(&bgColor, &fgColor, &selectColor, &topColor,
		       &bottomColor);
	  for (each=0; each<numpixels; each++) {
	    for (each2=0; each2<=realfree; each2++) {
	      if (mapv[each].pixel == avail[each2]) {
		break;
	      }
	    }
	    if (each2 > realfree) {
	      if (!haveFg &&
		  SAME_COLOR(mapv[each].red, fgColor.red) &&
		  SAME_COLOR(mapv[each].green, fgColor.green) &&
		  SAME_COLOR(mapv[each].blue, fgColor.blue)) {
		fgColor.pixel = mapv[each].pixel;
/* Don't know what this color is used for
   	        haveFg = True;
*/
	      } else if (!haveSelect &&
			 SAME_COLOR(mapv[each].red, selectColor.red) &&
			 SAME_COLOR(mapv[each].green, selectColor.green) &&
			 SAME_COLOR(mapv[each].blue, selectColor.blue)) {
		selectColor.pixel = mapv[each].pixel;
/* Don't know what this color is used for
		haveSelect = True;
*/
	      } else if (!haveTop &&
			 SAME_COLOR(mapv[each].red, topColor.red) &&
			 SAME_COLOR(mapv[each].green, topColor.green) &&
			 SAME_COLOR(mapv[each].blue, topColor.blue)) {
		topColor.pixel = mapv[each].pixel;
		haveTop = True;	
	      } else if (!haveBottom &&
			 SAME_COLOR(mapv[each].red, bottomColor.red) &&
			 SAME_COLOR(mapv[each].green, bottomColor.green) &&
			 SAME_COLOR(mapv[each].blue, bottomColor.blue)) {
		bottomColor.pixel = mapv[each].pixel;
		haveBottom = True;
	      }
	    }
	  }
	}
	bframe = CpickGetBoxFrame(cpick);
	count = 0;
	if (haveTop) {
	  XtSetArg(args[count], XmNtopShadowColor, topColor.pixel);  count++;
	  XtSetArg(args[count], XmNtopShadowPixmap,
		   XmUNSPECIFIED_PIXMAP);  count++;
	} else {
	  XtSetArg(args[count], XmNtopShadowColor, current.pixel);  count++;
	  XtSetArg(args[count], XmNtopShadowPixmap,
		   XmUNSPECIFIED_PIXMAP);  count++;
	}
	if (haveBottom) {
	  XtSetArg(args[count], XmNbottomShadowColor,
		   bottomColor.pixel);  count++;
	  XtSetArg(args[count], XmNbottomShadowPixmap,
		   XmUNSPECIFIED_PIXMAP);  count++;
	} else {
	  XtSetArg(args[count], XmNbottomShadowColor,
		   current.pixel);  count++;
	  XtSetArg(args[count], XmNbottomShadowPixmap,
		   XmUNSPECIFIED_PIXMAP);  count++;
	}
	XtSetValues(bframe, args, count);
      } else {
	XBell(XtDisplay(w), 0);
	XtMapWidget(w);
      }
    } else {
      XtMapWidget(w);
    }
  } else {
    postDialog("Can't allocate that colormap cell.  Pick another.");
    XBell(XtDisplay(w), 0);
  }
}

createGrid(wid, num, max, x0, y0, h, w, proc, val, pix)
Widget wid;
int num, max;
Position x0, y0;
Dimension h, w;
XtEventHandler proc;
struct vals *val;
PixProc pix;
{
  int each, x, y, hs, ws;
  double hsize, wsize;
  GC gc;
  XGCValues gcv;
  unsigned long black;

  val->pix = pix;

  wsize = sqrt((double) max);
  hsize = wsize;
  val->ih = (int) ((h-1)/hsize);
  val->iw = (int) ((w-1)/wsize);
  val->rownum = (int) wsize;
  hs = (int) hsize*val->ih+1;
  ws = (int) wsize*val->iw+1;

  XtMoveWidget(wid, x0, y0);
  XtResizeWidget(wid, w, h, (Dimension) 1);

  val->y0 = (h-hs)/2;
  val->x0 = (w-ws)/2;

  gcv.function = GXcopy;
  gcv.fill_style = FillSolid;
  black = XBlackPixel(XtDisplay(wid),
		      XDefaultScreen(XtDisplay(wid)));
  gcv.background = black;
  gc = XCreateGC(XtDisplay(wid), XtWindow(wid),
		 GCBackground | GCFunction | GCFillStyle, &gcv);

  XSync(XtDisplay(wid),0);
  for (each=0; each<num; each++) {
    x = (each % val->rownum)*val->iw;
    y = (each/val->rownum)*val->ih;
    XSetForeground(XtDisplay(wid), gc, pix(each));
    XFillRectangle(XtDisplay(wid), XtWindow(wid), gc, val->x0+x, val->y0+y,
		   val->iw, val->ih);
    XSetForeground(XtDisplay(wid), gc, black);
    XDrawRectangle(XtDisplay(wid), XtWindow(wid), gc, val->x0+x, val->y0+y,
		   val->iw, val->ih);
  }

  XtAddEventHandler(wid, ButtonPressMask, FALSE, (XtEventHandler) proc,
		    (XtPointer) val);
  XFreeGC(XtDisplay(wid), gc);
}

unsigned long gPixel(each)
int each;
{
  return each;
}

Boolean
ExposePending(display, event, arg)
Display *display;
XEvent *event;
Widget arg;
{
  return((event->type == Expose) &&
	 (((XExposeEvent *)event)->window == XtWindow(arg)));
}

doExpose()
{
  createGrid(glevel, numpixels, maxpixels, (Position) 0, (Position) 0,
	     getD(toplevel, XmNheight),
	     getD(toplevel, XmNwidth),
	     gButtonHandler, &gval, gPixel);
}

exposeHandler(w, client_data, event)
Widget w;
XtPointer client_data;
XEvent *event;
{
  XEvent ev;

/*
  XFlush(XtDisplay(w));
  while (XCheckIfEvent(XtDisplay(toplevel), &ev, ExposePending, w)) {
  }
*/
  
  if (((XExposeEvent *)event)->count == 0) {
    doExpose();
  }
}

resizeHandler(w, client_data, event)
Widget w;
XtPointer client_data;
XEvent *event;
{
  if (event->type == ConfigureNotify) {
    XClearWindow(XtDisplay(w), XtWindow(w));
    createGrid(glevel, numpixels, maxpixels, (Position) 0, (Position) 0,
	       getD(toplevel, XmNheight),
	       getD(toplevel, XmNwidth),
	       gButtonHandler, &gval, gPixel);
  }
}

saveCurrent()
{
  current.red = allocated.red;
  current.green = allocated.green;
  current.blue = allocated.blue;
  XStoreColor(XtDisplay(toplevel), cmap, &current);
}

restoreCmap()
{
  Display *dpy;
  int each, each2, count;
  Boolean found;
  XColor copy[MAXPIXELS];

  dpy = XtDisplay(toplevel);

  count = 0;
  for (each=0; each<numpixels; each++) {
    found = FALSE;
    for (each2=0; (each2<=realfree) && !found; each2++) {
      if (avail[each2] == got[each])
	found = TRUE;
    }
    if (!found)
      copy[count++] = mapv[got[each]];
  }

  XStoreColors(dpy,cmap,copy,count);
/*
  XInstallColormap(dpy,cmap);

  XStoreColors(dpy,cmap,copy,count);
  XInstallColormap(dpy,cmap);
*/
}

doSelect(cmd, cdata, position)
     Widget cmd;
     XtPointer cdata;
     float position;
{
  XtMapWidget(glevel);
  XtUnmapWidget(cpick);
}

doOk(cmd, cdata, position)
     Widget cmd;
     XtPointer cdata;
     float position;
{
  restoreCmap();
  if (cmapWin && oldMap != cmap) {
    changeColormap(XtDisplay(toplevel), cmapWin, cmap, oldMap);
  }
  exit(0);
}

doHelp(cmd, cdata, position)
     Widget cmd;
     XtPointer cdata;
     float position;
{
  static Widget info = NULL;
  XmString title, message;

  if (!info) {
    count = 0;
    title = XmStringCreateSimple("Cpicker Help");
    XtSetArg(args[count], XmNdialogTitle, title);  count++;
    message = XmStringCreateSimple(HELP_STRING);
    XtSetArg(args[count], XmNmessageString, message);  count++;
    info = XmCreateInformationDialog(toplevel, "info", args, count);
    XtUnmanageChild(XmMessageBoxGetChild(info, XmDIALOG_CANCEL_BUTTON));
    XtUnmanageChild(XmMessageBoxGetChild(info, XmDIALOG_HELP_BUTTON));
    XmStringFree(title);
    XmStringFree(message);
  }
  XtManageChild(info);
}

doChange(cmd, cdata, position)
     Widget cmd;
     XtPointer cdata;
     float position;
{
  saveCurrent();
  if (haveFg || haveSelect || haveTop || haveBottom) {
    XColor bgColor;

    bgColor.pixel = current.pixel;
    bgColor.red = current.red;
    bgColor.green = current.green;
    bgColor.blue = current.blue;
    (*colorProc)(&bgColor, &fgColor, &selectColor, &topColor, &bottomColor);
    if (haveFg) {
      XStoreColor(XtDisplay(toplevel), cmap, &fgColor);
    }
    if (haveSelect) {
      XStoreColor(XtDisplay(toplevel), cmap, &selectColor);
    }
    if (haveTop) {
      XStoreColor(XtDisplay(toplevel), cmap, &topColor);
    }
    if (haveBottom) {
      XStoreColor(XtDisplay(toplevel), cmap, &bottomColor);
    }
  }
}

doRestore(cmd, cdata, position)
     Widget cmd;
     XtPointer cdata;
     float position;
{
  restoreCmap();
  XQueryColor(XtDisplay(toplevel), cmap, &current);
  allocated.red = current.red;
  allocated.green = current.green;
  allocated.blue = current.blue;
  XtSetValues(cpick, setargs, XtNumber(setargs));
}

int createCmap(num, wcmap, visual)
int num;
Colormap wcmap;
Visual *visual;
{
  Display *dpy;
  int screen, each, real;

  real = num+1;

  dpy = XtDisplay(toplevel);
  screen = XDefaultScreen(XtDisplay(toplevel));

  for (each=0; each<maxpixels; each++) {
    mapv[each].pixel = each;
    mapv[each].flags = (DoRed | DoGreen | DoBlue);
  }
  XQueryColors(dpy, wcmap, mapv, numpixels);

  while ((real > 1) &&
	 (!XAllocColorCells(dpy, wcmap, FALSE, NULL, 0, avail,
			    real+1))) {
    real--;
  }

  if (real <= 1) {
    fprintf(stderr, "Sorry, can't allocate enough colormap cells.  There aren't enough free cells in that colormap.\n");
    exit(1);
  }

  cmap = XCreateColormap(dpy, XRootWindow(dpy,screen),
			 visual, AllocNone);

  XAllocColorCells(dpy, cmap, FALSE, NULL, 0, got, numpixels);
  XStoreColors(dpy,cmap,mapv,numpixels);
  XFreeColors(dpy, cmap, avail, real, 0);
  allocated = mapv[avail[real]];

  XInstallColormap(dpy,cmap);

  return real-1;
}

errorHandler(dpy, event)
Display *dpy;
XErrorEvent *event;
{
  char buf[256];

  if ((event->error_code == BadAccess) ||
      (event->request_code == X_QueryColors)) {
    postDialog("Can't allocate that colormap cell.  Pick another.");
    XBell(dpy, 0);
    picking = FALSE;
  } else {
    XGetErrorText(dpy, event->error_code, buf, 256);
    fprintf(stderr, "X protocol error detected by server: %s\n", buf);
    fprintf(stderr, "  Failed request major op code %d\n", (int) event->request_code);
    if (cmapWin && oldMap != cmap) {
      changeColormap(XtDisplay(toplevel), cmapWin, cmap, oldMap);
    }
    exit(1);
  }
}

Window chooseWindow(dpy)
Display *dpy;
{
  Window chosen, last, ignorew;
  int ignore;
  unsigned int ignoreu;
  XEvent ev;

  XGrabPointer(XtDisplay(toplevel),
	       XRootWindowOfScreen(XDefaultScreenOfDisplay(XtDisplay(toplevel))),
	       FALSE,
	       ButtonPressMask,
	       GrabModeAsync,
	       GrabModeSync,
	       None,
	       XCreateFontCursor(XtDisplay(toplevel), XC_crosshair),
	       CurrentTime);

  while (True) {
    XtAppNextEvent(XtWidgetToApplicationContext(toplevel), &ev);
    if (ev.type == ButtonPress) {
      break;
    } else {
      XtDispatchEvent(&ev);
    }
  }

  last = XDefaultRootWindow(XtDisplay(toplevel));
  chosen = last;
  while (chosen != None) {
    XQueryPointer(XtDisplay(toplevel), last,  &ignorew, &chosen,
		  &ignore, &ignore, &ignore, &ignore, &ignoreu);
    if (chosen != None)
      last = chosen;
  }
  XUngrabPointer(XtDisplay(toplevel), CurrentTime);
  choose_x = ev.xbutton.x;
  choose_y = ev.xbutton.y;
  return last;
}

main(argc, argv)
    int argc;
    char **argv;
{
  Widget realtop;
  int nvi, i;
  Dimension cw, ch;
  Position x, y;
  Boolean found;
  XVisualInfo *vip, viproto, *bestvip;
  XWindowAttributes xwa;
  XmString title;

  static Arg mainargs[] = {
    {XmNcmap, NULL},
#ifdef USE_COLORS
    {XmNuseColors, TRUE},
#else
    {XmNuseColors, FALSE},
#endif
    {XmNnearPixels, NULL},
    {XmNallocated, (XtArgVal) &allocated},
    {XmNtopAttachment, XmATTACH_FORM},
    {XmNbottomAttachment, XmATTACH_FORM},
    {XmNleftAttachment, XmATTACH_FORM},
    {XmNrightAttachment, XmATTACH_FORM},
    {XmNokLabel, (XtArgVal) "Quit"},
    {XmNborderWidth, (XtArgVal) 0},
  };
  
  allocated.flags = current.flags = DoRed | DoGreen | DoBlue;
  
  realtop = XtInitialize(argv[0], "Cpicker", cmdOptions, XtNumber(cmdOptions),
			 &argc, argv);

#ifdef USE_SCHEMES
  VkLoadScheme(XtDisplay(realtop), argv[0], "Pixedit");
#endif

  count = 0;
  toplevel = XmCreateForm(realtop, "form", args, count);
  XtManageChild(toplevel);
  
  if (XDisplayPlanes(XtDisplay(toplevel),
		     XDefaultScreen(XtDisplay(toplevel))) == 1) {
    fprintf(stderr, "Sorry, you need a color display.\n");
    exit(1);
  }

  XSetErrorHandler(errorHandler);

  doMotif = (XGetDefault(XtDisplay(toplevel), argv[0], "noMotif") == NULL);
  if (doMotif) {
    colorProc = XmGetColorCalculation();
    fgColor.flags = selectColor.flags = topColor.flags = bottomColor.flags =
      DoRed | DoGreen | DoBlue;
  }

  count = 0;
  title = XmStringCreateSimple("Cpicker");
  XtSetArg(args[count], XmNdialogTitle, title);  count++;
  dialog = XmCreateMessageDialog(toplevel, "dialog", args, count);
  XmStringFree(title);
  XtUnmanageChild(XmMessageBoxGetChild(dialog, XmDIALOG_DEFAULT_BUTTON));
  XtUnmanageChild(XmMessageBoxGetChild(dialog, XmDIALOG_CANCEL_BUTTON));
  XtUnmanageChild(XmMessageBoxGetChild(dialog, XmDIALOG_HELP_BUTTON));

  found = FALSE;
  if (win = Select_Window_Args(&argc, argv)) {
    XGetWindowAttributes(XtDisplay(toplevel), win, &xwa);
    viproto.visual = xwa.visual;
    viproto.class = PseudoColor;
    vip = XGetVisualInfo(XtDisplay(toplevel), VisualClassMask, &viproto, &nvi);
    if (nvi && xwa.colormap) {
      found = TRUE;
    } else if (!nvi) {
      postDialog("Sorry, that window doesn't have a colormap to edit (no PseudoColor visual).  Pick another.");
      XBell(XtDisplay(toplevel), 0);
    } else {
      postDialog("That window has an invalid (zero) colormap.  Please restart the window's client and try again.");
      XBell(XtDisplay(toplevel), 0);
    }
  } else {
    if (argc != 1) {
      usage();
      exit(1);
    }
    postDialog("Click on a window to edit its colormap.");
  }    

  while (!found) {
    win = chooseWindow(XtDisplay(toplevel));
    XGetWindowAttributes(XtDisplay(toplevel), win, &xwa);
    viproto.visual = xwa.visual;
    viproto.class = PseudoColor;
    vip = XGetVisualInfo(XtDisplay(toplevel), VisualClassMask, &viproto, &nvi);
    if (nvi && xwa.colormap) {
      found = TRUE;
    } else if (!nvi) {
      postDialog("Sorry, that window doesn't have a colormap to edit (no PseudoColor visual).  Pick another.");
      XBell(XtDisplay(toplevel), 0);
    } else {
      postDialog("That window has an invalid (zero) colormap.  Please restart the window's client and try again.");
      XBell(XtDisplay(toplevel), 0);
    }
  }

/* First try to find a visual that matches the selected window's visual */
  bestvip = (XVisualInfo *) NULL;
  for (i=0; i<nvi; i++) {
    if (xwa.visual == vip[i].visual) {
      bestvip = vip+i;
    }
  }
/* Otherwise, find the pseudocolor visual with the most colormap entries,
   but within our hardcoded limit */
  if (!bestvip || bestvip->colormap_size > MAXPIXELS) {
    bestvip = vip;
    for (i=1; i<nvi; i++) {
      if (bestvip->colormap_size < vip[i].colormap_size &&
	  vip[i].colormap_size <= MAXPIXELS) {
	bestvip = vip+i;
      }
    }
  }
  numpixels = bestvip->colormap_size;
  maxpixels = 1;
  while (maxpixels < numpixels) {
    maxpixels = maxpixels*2;
  }

  if (numpixels < 11) {
    fprintf(stderr, "Sorry, you don't have enough colormap cells.\n");
    exit(1);
  } else if (numpixels < 58) {
    mainargs[1].value = (XtArgVal) FALSE;
    mainargs[2].value = (XtArgVal) 4;
    numfree = COLORLESSPIXELS+4;
  } else if (numpixels < 136) {
#ifdef USE_COLORS
      mainargs[1].value = (XtArgVal) TRUE;
      mainargs[2].value = (XtArgVal) 25;
      numfree = NECESSARYPIXELS+25;
#else
      mainargs[1].value = (XtArgVal) FALSE;
      mainargs[2].value = (XtArgVal) 25;
      numfree = COLORLESSPIXELS+25;
#endif
  } else {
#ifdef USE_COLORS
      mainargs[1].value = (XtArgVal) TRUE;
      mainargs[2].value = (XtArgVal) 64;
      numfree = NECESSARYPIXELS+64;
#else
      mainargs[1].value = (XtArgVal) FALSE;
      mainargs[2].value = (XtArgVal) 64;
      numfree = COLORLESSPIXELS+64;
#endif
  }

  oldMap = xwa.colormap;
  realfree = createCmap(numfree, xwa.colormap, bestvip->visual);
  XFree((char *) vip);

  mainargs[0].value = (XtArgVal) cmap;
  if (realfree != numfree)
    if (mainargs[1].value == (XtArgVal) TRUE)
      mainargs[2].value = (XtArgVal) realfree-NECESSARYPIXELS;
    else
      mainargs[2].value = (XtArgVal) realfree-COLORLESSPIXELS;

  if (mainargs[2].value <= 0) {
    fprintf(stderr, "Sorry, you don't have enough colormap cells.\n");
    exit(1);
  }

  XtUnmanageChild(dialog);
  cpick = XtCreateManagedWidget(argv[0], cpickWidgetClass,
				toplevel, mainargs, XtNumber(mainargs));
  XtAddCallback(cpick, XmNselectProc, (XtCallbackProc) doSelect, NULL);
  XtAddCallback(cpick, XmNokProc, (XtCallbackProc) doOk, NULL);
  XtAddCallback(cpick, XmNhelpProc, (XtCallbackProc) doHelp, NULL);
  XtAddCallback(cpick, XmNchangeProc, (XtCallbackProc) doChange, NULL);
  XtAddCallback(cpick, XmNrestoreProc, (XtCallbackProc) doRestore, NULL);

  cw = getD(cpick, XmNwidth);
  ch = getD(cpick, XmNheight);
  XGetWindowAttributes(XtDisplay(cpick),
		       XDefaultRootWindow(XtDisplay(cpick)), &xwa);
  x = choose_x-(Position) (cw/2);
  y = choose_y-(Position) (ch/2);
  if (x < 0) {
    x = 0;
  } else if (x > xwa.width-cw) {
    x = xwa.width-cw;
  }
  if (y < 0) {
    y = 0;
  } else if (y > xwa.height-ch) {
    y = xwa.height-ch;
  }

  XtMoveWidget(realtop, x, y);
  XtRealizeWidget(realtop);
  XtUnmapWidget(cpick);

  glevel = XmCreateForm(toplevel, "selectForm", gargs, XtNumber(gargs));
  XtAddEventHandler(glevel, ExposureMask, TRUE,
		    (XtEventHandler) exposeHandler, NULL);
  XtAddEventHandler(glevel, StructureNotifyMask, TRUE,
		    (XtEventHandler) resizeHandler, NULL);
  XtManageChild(glevel);
  doExpose();

  if (XGetDefault(XtDisplay(toplevel), argv[0], "noMultiple") == NULL) {
    if (cmapWin = findTopLevelWindow(XtDisplay(toplevel), win)) {
      changeColormap(XtDisplay(toplevel), cmapWin, oldMap, cmap);
    }
  }

  XGetWindowAttributes(XtDisplay(realtop), XtWindow(realtop), &xwa);
  changeColormap(XtDisplay(toplevel), XtWindow(realtop), xwa.colormap, cmap);
  win = XtWindow(cpick);
  XSetWMColormapWindows(XtDisplay(realtop), XtWindow(realtop), &win, 1);

  XtMainLoop();
}

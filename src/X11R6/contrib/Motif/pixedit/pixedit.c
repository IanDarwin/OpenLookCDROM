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
 * pixedit.c - Pixel color editor for X11
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

#define HELP_STRING	"Pixedit, a colormap editing utility.  Mike Yang, Silicon Graphics, mikey@sgi.com"

/* #define USE_COLORS	/* define to make the scale labels colored */

/* We do this because most servers support only the upper 8 bits for RGB */
#define SAME_COLOR(x,y)	((x)/256 == (y)/256 || (x)/256+1 == (y)/256 || (x)/256 == (y)/256+1)

XColor allocated, current;
Widget realtop, toplevel, cpick, dialog;
XColor mapv[MAXPIXELS];
Colormap cmap, oldMap;
Boolean picking;
int maxpixels, numpixels, numfree, realfree;
unsigned long avail[MAXPIXELS], got[MAXPIXELS];
Window cmapWin = NULL;
char *argv0;
Position choose_x = 200, choose_y = 200;
XmColorProc colorProc;
XColor fgColor, selectColor, topColor, bottomColor;
Boolean doMotif, haveFg, haveSelect, haveTop, haveBottom;
char *argv0;

typedef unsigned long (*PixProc)();

struct vals {
  int ih, iw, x0, y0, rownum;
  PixProc pix;
} gval, nval;

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
	"    -motif               enable Motif color matching (default)\n");
    fprintf (stderr,
	"    -noMotif             disable Motif color matching\n");
    fprintf (stderr,
	"    -multiple            hardware supports multiple colormaps (default)\n");
    fprintf (stderr,
	"    -noMultiple          assume hardware supports single colormap\n");
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
  int screen, each, each2, count;
  Boolean found;
  XColor copy[MAXPIXELS];

  dpy = XtDisplay(toplevel);
  screen = XDefaultScreen(XtDisplay(toplevel));

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
  XInstallColormap(dpy,cmap);

  XStoreColors(dpy,cmap,copy,count);
  XInstallColormap(dpy,cmap);
}

doSelect(cmd, cdata, position)
     Widget cmd;
     XtPointer cdata;
     float position;
{
  Window chosen, ignorew, last;
  int ignore;
  unsigned int ignoreu;
  XEvent ev;
  int x, y, each, each2;
  unsigned long newpix;
  Widget bframe;
  XColor bgColor;
  Boolean first;
  XWindowAttributes xwa;

  if (cdata != NULL) {
    postDialog("Click on a pixel to edit its colormap value.");
  }
  haveFg = haveSelect = haveTop = haveBottom = False;
  picking = TRUE;
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

  if (ev.type != ButtonPress) {
    fprintf(stderr, "Impossible.\n");
    exit(1);
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
  if (!cpick) {
    createCpick(last);
    first = True;
  } else {
    first = False;
  }
  if (picking) {
    x = ((XButtonEvent *) &ev)->x_root;
    y = ((XButtonEvent *) &ev)->y_root;
    newpix = XGetPixel(XGetImage(XtDisplay(toplevel), 
				 XRootWindowOfScreen(XDefaultScreenOfDisplay(XtDisplay(toplevel))),
				 x, y, 1, 1,
				 512-1 /* yuck dependency */, ZPixmap),
		       0, 0);
    for (each=0; each<=realfree; each++) {
      if (newpix == avail[each]) {
	postDialog("That's one of the widget's colormap cells.  Pick another.");
	XBell(XtDisplay(cmd), 0);
	doSelect(cmd, NULL, position);
	return;
      }
    }
    current.pixel = newpix;
    XQueryColor(XtDisplay(toplevel), cmap, &current);
    allocated.red = current.red;
    allocated.green = current.green;
    allocated.blue = current.blue;
    XSync(XtDisplay(toplevel), 0);
  }
  if (picking) {
    /* generates error if can't allocate */
    XStoreColor(XtDisplay(toplevel), cmap, &current);
    XSync(XtDisplay(toplevel), 0);
    if (picking) {
      XtSetValues(cpick, setargs, XtNumber(setargs));
      XtUnmanageChild(dialog);
      if (first) {
	XtRealizeWidget(realtop);

	if (XGetDefault(XtDisplay(toplevel), argv0, "noMultiple") == NULL) {
	  if (cmapWin = findTopLevelWindow(XtDisplay(toplevel), last)) {
	    changeColormap(XtDisplay(toplevel), cmapWin, oldMap, cmap);
	  }
	}

	XGetWindowAttributes(XtDisplay(realtop), XtWindow(realtop), &xwa);
	changeColormap(XtDisplay(toplevel), XtWindow(realtop),
		       xwa.colormap, cmap);
	last = XtWindow(cpick);
	XSetWMColormapWindows(XtDisplay(realtop), XtWindow(realtop), &last, 1);
      }
      picking = FALSE;
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
      doSelect(cmd, NULL, position);
    }
  } else {
    doSelect(cmd, NULL, position);
  }
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
    title = XmStringCreateSimple("Pixedit Help");
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
  XSetWindowAttributes xswa;

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
    picking = FALSE;
  } else {
    XGetErrorText(dpy, event->error_code, buf, 256);
    fprintf(stderr, "X protocol error detected by server: %s\n", buf);
    fprintf(stderr, "  Failed request major op code %d\n",
	    (int) event->request_code);
    if (cmapWin && oldMap != cmap) {
      changeColormap(XtDisplay(toplevel), cmapWin, cmap, oldMap);
    }
    exit(1);
  }
}

createCpick(win)
Window win;
{
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
  
  XVisualInfo *vip, viproto, *bestvip;
  XWindowAttributes xwa;
  int nvi, i;
  Dimension cw, ch;
  Position x, y;

  XGetWindowAttributes(XtDisplay(toplevel), win, &xwa);
  viproto.visual = xwa.visual;
  viproto.class = PseudoColor;
  vip = XGetVisualInfo(XtDisplay(toplevel), VisualClassMask, &viproto, &nvi);
  if (!nvi) {
    postDialog("Sorry, that window doesn't have a colormap to edit (no PseudoColor visual).  Pick another.");
    XBell(XtDisplay(toplevel), 0);
    picking = FALSE;
  } else if (!xwa.colormap) {
    postDialog("That window has an invalid (zero) colormap.  Please restart the window's client and try again.");
    XBell(XtDisplay(toplevel), 0);
    picking = FALSE;
  } else {
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
    
    cpick = XtCreateManagedWidget(argv0, cpickWidgetClass,
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
  }
}

main(argc, argv)
    int argc;
    char **argv;
{
  int each;
  XmString title;
  
  argv0 = argv[0];
  allocated.flags = current.flags = DoRed | DoGreen | DoBlue;
  
  realtop = XtInitialize(argv[0], "Pixedit", cmdOptions, XtNumber(cmdOptions),
			 &argc, argv);
  argv0 = argv[0];

  if (argc != 1) {
    usage();
    exit(1);
  }

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
  title = XmStringCreateSimple("Pixedit");
  XtSetArg(args[count], XmNdialogTitle, title);  count++;
  dialog = XmCreateMessageDialog(toplevel, "dialog", args, count);
  XmStringFree(title);
  XtUnmanageChild(XmMessageBoxGetChild(dialog, XmDIALOG_DEFAULT_BUTTON));
  XtUnmanageChild(XmMessageBoxGetChild(dialog, XmDIALOG_CANCEL_BUTTON));
  XtUnmanageChild(XmMessageBoxGetChild(dialog, XmDIALOG_HELP_BUTTON));

  doSelect(toplevel, toplevel, NULL);

  XtMainLoop();
}

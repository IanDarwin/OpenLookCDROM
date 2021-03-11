/* main.c
 *
 * (c) Copyright 1990-1994 Adobe Systems Incorporated.
 * All rights reserved.
 * 
 * Permission to use, copy, modify, distribute, and sublicense this software
 * and its documentation for any purpose and without fee is hereby granted,
 * provided that the above copyright notices appear in all copies and that
 * both those copyright notices and this permission notice appear in
 * supporting documentation and that the name of Adobe Systems Incorporated
 * not be used in advertising or publicity pertaining to distribution of the
 * software without specific, written prior permission.  No trademark license
 * to use the Adobe trademarks is hereby granted.  If the Adobe trademark
 * "Display PostScript"(tm) is used to describe this software, its
 * functionality or for any other purpose, such use shall be limited to a
 * statement that this software works in conjunction with the Display
 * PostScript system.  Proper trademark attribution to reflect Adobe's
 * ownership of the trademark shall be given whenever any such reference to
 * the Display PostScript system is made.
 * 
 * ADOBE MAKES NO REPRESENTATIONS ABOUT THE SUITABILITY OF THE SOFTWARE FOR
 * ANY PURPOSE.  IT IS PROVIDED "AS IS" WITHOUT EXPRESS OR IMPLIED WARRANTY.
 * ADOBE DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL
 * IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NON- INFRINGEMENT OF THIRD PARTY RIGHTS.  IN NO EVENT SHALL ADOBE BE LIABLE
 * TO YOU OR ANY OTHER PARTY FOR ANY SPECIAL, INDIRECT, OR CONSEQUENTIAL
 * DAMAGES OR ANY DAMAGES WHATSOEVER WHETHER IN AN ACTION OF CONTRACT,
 * NEGLIGENCE, STRICT LIABILITY OR ANY OTHER ACTION ARISING OUT OF OR IN
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.  ADOBE WILL NOT
 * PROVIDE ANY TRAINING OR OTHER SUPPORT FOR THE SOFTWARE.
 * 
 * Adobe, PostScript, and Display PostScript are trademarks of Adobe Systems
 * Incorporated which may be registered in certain jurisdictions
 * 
 * Author:  Adobe Systems Incorporated
 */

/* C headers */
#include <stdio.h>

/* Xt toolkit headers */
#include <X11/IntrinsicP.h>
#include <X11/StringDefs.h>

/* Motif widget headers */
#include <Xm/Xm.h>
#include <Xm/Form.h>

/* DPS library headers */
#include <DPS/dpsXclient.h>
#include <DPS/dpsops.h>
#include <DPS/psops.h>

#include "globals.h"
#include "wraps.h"

#ifdef _NO_PROTO
#define ARGCAST int
#else
#define ARGCAST void *
#endif

/* global variables*/
Widget toplevel;
GC defaultGC;

void init_dps (display, debug)
    Display *display;
    int debug;
{
    DPSContext ctxt, txtctxt;

    /* Make it possible for this client to start a DPS NX agent,
       if "dpsnx.agent" is on the executable search path. */

    (void) XDPSNXSetClientArg(XDPSNX_AUTO_LAUNCH, (ARGCAST) True);

    ctxt = XDPSCreateSimpleContext(display, 0, 0, 0, 0,
          DPSDefaultTextBackstop, DPSDefaultErrorProc, NULL);

    if (ctxt == NULL) {
	fprintf(stderr, "\nfontview: DPS is not available.\n");
	fprintf(stderr,
          "You need an X server with the DPS extension, or a DPS NX agent.\n");
	exit(1);
    }
    DPSSetContext(ctxt);

    if (debug) {
        txtctxt = DPSCreateTextContext(DPSDefaultTextBackstop, 
              DPSDefaultErrorProc);
        DPSChainContext(ctxt, txtctxt);
    }
}

void main (argc, argv)
    int argc;
    char **argv;
{
    Arg args[10];
    DPSContext ctxt, txtctxt;
    Display *display;
    Screen *screen;
    Widget parent, fontPanel, demoCanvas;
    int i;
   
    toplevel = XtInitialize(NULL, "Fontview", NULL, 0, &argc, argv);


    display = XtDisplay(toplevel);
    screen = XtScreen(toplevel);

    defaultGC = XCreateGC(display, RootWindowOfScreen(screen), 0, NULL);
    XSetForeground(display, defaultGC, WhitePixelOfScreen(screen));
    XSetBackground(display, defaultGC, WhitePixelOfScreen(screen));

    init_dps(display, (argc > 1));

    parent = XmCreateForm(toplevel, "parent", NULL, 0);

    fontPanel = CreateFontPanel(parent);
   
    demoCanvas = CreateDemoCanvas(fontPanel, parent); 

    i=0; XtSetArg(args[i], XmNwidth, 700);
    i++; XtSetValues(parent, args, i);
    XtManageChild(parent);
    XtRealizeWidget(toplevel);

    XtMainLoop();
}

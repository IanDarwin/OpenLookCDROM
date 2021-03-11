/* dpsclock.c - Sample clock program

Copyright 1989 Massachusetts Institute of Technology

Permission to use, copy, modify, distribute, and sell this software and its
documentation for any purpose is hereby granted without fee, provided that
the above copyright notice appear in all copies and that both that
copyright notice and this permission notice appear in supporting
documentation, and that the name of M.I.T. not be used in advertising or
publicity pertaining to distribution of the software without specific,
written prior permission.  M.I.T. makes no representations about the
suitability of this software for any purpose.  It is provided "as is"
without express or implied warranty.

M.I.T. DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL M.I.T.
BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION
OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

*/

/*
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

#include <X11/Xatom.h>
#include <X11/Xlib.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Shell.h>
#include <DPS/dpsXclient.h>
#include <stdio.h>
#include "clock.h"
#include "clock.bit"

static XrmOptionDescRec options[] = {
  {"-chime",	"*chime",	XrmoptionNoArg,		"TRUE"},
  {"-update",	"*update",	XrmoptionSepArg,	NULL},
  {"-padding",	"*padding",	XrmoptionSepArg,	NULL},
  {"-d",	"*analog",	XrmoptionNoArg,		"FALSE"},
  {"-digital",	"*analog",	XrmoptionNoArg,		"FALSE"},
  {"-analog",	"*analog",	XrmoptionNoArg,		"TRUE"},
  {"-font",	"*font",	XrmoptionSepArg,	NULL},
  };

static XtResource resources[] = {
  {XtNfont, XtCString, XtRString, sizeof(char *),
  XtOffset(ClockParams *, font), XtRString, "Times-Roman"},
  {"analog", XtCBoolean, XtRBoolean, sizeof(Boolean),
  XtOffset(ClockParams *, analog), XtRString, "TRUE"},
  {"chime", XtCBoolean, XtRBoolean, sizeof(Boolean),
  XtOffset(ClockParams *, chime), XtRString, "FALSE"},
  {XtNupdate, XtCInterval, XtRInt, sizeof(int),
  XtOffset(ClockParams *, update), XtRString, "60"},
  {"padding", XtCMargin, XtRInt, sizeof(int),
  XtOffset(ClockParams *, padding), XtRString, "8"},
  {"dpsnxAgentExec", XtCString, XtRString, sizeof(char *),
  XtOffset(ClockParams *, nxAgentExec), XtRString, NULL}
  };
 
#ifdef _NO_PROTO
#define ARGCAST int
#else
#define ARGCAST void *
#endif

/*
 * Report the syntax for calling xclock.
 */
Syntax(call)
  char *call;
  {
  (void) printf("Usage: %s [-analog] [-digital]\n", call);
  (void) printf("       [-font <font_name>] [-help] [-padding <pixels>]\n");
  (void) printf("       [-update <seconds>] [-display displayname]\n");
  (void) printf("       [-bg <color>] [-geometry geom]\n\n");
  exit(1);
  }

void main(argc, argv)
  int argc;
  char **argv;
  {
  Arg arg;
  ClockParams params;
  Widget topLevel, w;

  topLevel = XtInitialize(
    argv[0], "DPSClock", options, XtNumber(options), &argc, argv);

  if (argc != 1)
    Syntax(argv[0]);

  arg.name = XtNiconPixmap;
  arg.value = (XtArgVal)XCreateBitmapFromData(
    XtDisplay(topLevel), XtScreen(topLevel)->root,
    (char *) clock_bits, clock_width, clock_height);
  XtSetValues(topLevel, &arg, 1);

  XtGetApplicationResources(
    topLevel, &params, resources, XtNumber(resources), NULL, 0);

  /*
    ******************************************************************
    The following two calls to XDPSSetClientArg demonstrate
    the recommended method for enabling your client to
    access Display PostScript NX Software. 
 
    The Client Library will automatically find and use DPS without
    any need for additional programming.  Adobe has made access to
    DPS completely transparent, so developers won't have to worry
    about availability, as they would for other extensions.  Adobe
    recommends that you simply use the DPS API, and let the Client
    Libary handle everything.  All that is necessary is that
    NX client options should be set before the first context is
    created, or before calling XDPSExtensionPresent.
  
    There are two ways to access DPS: either the X server has the DPS
    extension built in, or there is a DPS NX agent available which
    can provide DPS as a service to your X server.  The DPS NX agent
    is a "client side" program which allows DPS clients to display
    on X servers which do not have the DPS extension.
    
    Clients access a DPS NX agent transparently: if there is an
    agent servicing a display, a DPS client will use it automatically.
    This means that programmers can use the single unified DPS
    Client Library API, and be assured that the application will
    display on any X server.
    
    The possibility of DPS NX makes determining whether or not
    DPS is available more complicated than simply doing XQueryExtension.
    Since DPS NX exists precisely for X servers that don't have the
    DPS extension, querying for the extension won't tell you if
    DPS NX is available.  Fortunately, there is no need to
    determine if the extension exists.  The Client Library will
    do this for you automatically.
    
    Another twist to the story is that a client can automatically
    launch a DPS NX agent program if none is available, assuming
    the program is readable and executable by the user, and the
    client program knows where to find the agent program file.
    This demo, dpsclock, provides a resource 'dpsnxAgentExec' for
    specifying the location of the DPS NX program file.  If this
    resource is not defined, the default policy for the Client
    Library is to try to run 'dpsnx.agent' from the user's current
    search path.  You can use the XDPSNX_EXEC_FILE option to
    specify a specific agent program, if "dpsnx.agent" is not
    correct.
    
    If DPS is not available at all, context creation will fail.
    See clock.c for how to handle this.
    ******************************************************************
    */
     
  (void) XDPSNXSetClientArg(XDPSNX_AUTO_LAUNCH, (ARGCAST) True);
  if (params.nxAgentExec)
    (void) XDPSNXSetClientArg(XDPSNX_EXEC_FILE, (ARGCAST) params.nxAgentExec);

  w = CreateClock(topLevel, &params);

  XtRealizeWidget(topLevel);

  XtMainLoop ();
  }

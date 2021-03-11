/* xepsf.c
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

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <DPS/dpsXclient.h>
#include <DPS/dpsXcommon.h>
#include <stdio.h>
#include <DPS/dpsXpreview.h>
#include <stdlib.h>

typedef struct {
    String magnification;
} OptionsRec;

OptionsRec options;

XtResource resources[] = {
    {"magnification", "Magnification", XtRString, sizeof(String),
	     XtOffsetOf(OptionsRec, magnification),
	     XtRImmediate, (XtPointer) "1"}
};

XrmOptionDescRec optionDesc[] = {
    {"-magnification", "*magnification", XrmoptionSepArg, (XtPointer) NULL}
};

void Usage(name)
    char *name;
{
    fprintf(stderr, "Usage: %s [-magnification n] filename\n", name);
    fprintf(stderr, "   The magnification can be an integer or a floating point number\n");
    fprintf(stderr, "   -magnification can be abbreviated to -mag\n");
    exit(1);
}

#ifdef _NO_PROTO
#define ARGCAST int
#else
#define ARGCAST void *
#endif


void main(argc, argv)
    int argc;
    char **argv;
{
    Widget shell, wid;
    XtAppContext app;
    String file;
    FILE *f;
    int i;
    Arg args[10];
    int status;
    XRectangle bbox, pixelSize;
    Cardinal depth;
    Pixmap p;
    Bool dummy;
    float pixelsPerPoint;
    float magnification;

    shell = XtAppInitialize(&app, "XEPSF", optionDesc, XtNumber(optionDesc),
			    &argc, argv, (String *) NULL, (ArgList) NULL, 0);

    XtGetApplicationResources(shell, (XtPointer) &options,
			      resources, XtNumber(resources),
			      (ArgList) NULL, 0);

    /* Make it possible for this client to start a DPS NX agent,
       if "dpsnx.agent" is on the executable search path. */

    (void) XDPSNXSetClientArg(XDPSNX_AUTO_LAUNCH, (ARGCAST) True);

    pixelsPerPoint = XDPSPixelsPerPoint(XtScreen(shell));

    magnification = atof(options.magnification);

    if (magnification < 0.001) Usage(argv[0]);

    pixelsPerPoint *= magnification;

    if (argc <= 1) Usage(argv[0]);
    else file = argv[1];

    /* Choose an arbitrary size; we'll change it later */
    i = 0;	
    XtSetArg(args[i], XtNwidth, 400);		i++;
    XtSetArg(args[i], XtNheight, 400);		i++;
    wid = XtCreateManagedWidget("main", widgetClass, shell, args, i);

    /* Get the depth of the window we'll be displaying into */
    i = 0;
    XtSetArg(args[i], XtNdepth, &depth);	i++;
    XtGetValues(wid, args, i);

    /* Open the input file */
    f = fopen(file, "r");
    if (f == NULL) {
       fprintf(stderr, "Could not open file %s\n", file);
       exit(1);
    }

    /* Create a pixmap the appropriate size for the image */
    status = XDPSCreatePixmapForEPSF(
	     (DPSContext) NULL, 	/* Use shared context */
	     XtScreen(shell),		/* Create pixmap on this screen */
	     f, 			/* File to read for bounding box */
	     depth,			/* Depth of pixmap to create */
	     pixelsPerPoint,		/* Pixels per point */
	     &p,			/* Returned pixmap */
	     &pixelSize, 		/* Returned image size in pixels */
	     &bbox);			/* Returned image size in points */

    switch (status) {
	case dps_status_failure:
	    fprintf(stderr, "File is not EPSF\n");
	    exit(1);
	case dps_status_no_extension:
	    fprintf(stderr, "\nxepsf: DPS is not available.\n");
	    fprintf(stderr, "You need an X server with the DPS extension, or a -PS NX agent.\n");
	    exit(1);
    }


    /* Image the file into the pixmap */
    status = XDPSImageFileIntoDrawable(
	     (DPSContext) NULL, 	/* Use shared context */ 	
	     XtScreen(shell),		/* Pixmap is on this screen */
	     p,				/* Image into this pixmap */
	     f,				/* File containing image */
	     pixelSize.height,		/* Height of pixmap */
	     depth,			/* Depth of pixmap */
	     &bbox,			/* Bounding box of image */
	     -bbox.x, -bbox.y,		/* Offset of image in pixmap */
	     pixelsPerPoint,		/* Pixels per point */
	     True,			/* Clear bounding box before imaging */
	     False,			/* Don't create a mask */
	     True,			/* Wait for imaging to complete */
	     &dummy);			/* Ignored when waiting */

    switch (status) {
	case dps_status_no_extension:
	    fprintf(stderr, "\nxepsf: DPS is not available.\n");
	    fprintf(stderr, "You need an X server with the DPS extension, or a -PS NX agent.\n");
	    exit(1);
	case dps_status_success:
	    break;
	case dps_status_postscript_error:
	    fprintf(stderr, "PostScript execution error in EPSF file\n");
	    exit(1);
	default:
	    fprintf(stderr, "Internal error %d\n", status);
	    exit(1);
    }

    fclose(f);

    /* Set the size to be that of the image and the background to the image */
    i = 0;
    XtSetArg(args[i], XtNwidth, pixelSize.width);	i++;
    XtSetArg(args[i], XtNheight, pixelSize.height);	i++;
    XtSetArg(args[i], XtNbackgroundPixmap, p);		i++;
    XtSetValues(wid, args, i);

    XtRealizeWidget(shell);
    XtAppMainLoop(app);
}

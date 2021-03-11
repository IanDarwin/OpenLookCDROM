/*
 * Copyright (C) 1990-1991 by Adobe Systems Incorporated.
 * All rights reserved.
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted,
 * provided that the above copyright notices appear in all copies and that
 * both those copyright notices and this permission notice appear in
 * supporting documentation and that the name of Adobe Systems
 * Incorporated not be used in advertising or publicity pertaining to
 * distribution of the software without specific, written prior
 * permission.
 * 
 * ADOBE MAKES NO REPRESENTATIONS ABOUT THE SUITABILITY OF THE SOFTWARE FOR
 * ANY PURPOSE.  IT IS PROVIDED "AS IS" WITHOUT EXPRESS OR IMPLIED WARRANTY.
 * ADOBE DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL
 * IMPLIED WARRANTIES OF MERCHANTABILITY FITNESS FOR A PARTICULAR PURPOSE AND
 * NON-INFRINGEMENT OF THIRD PARTY RIGHTS.  IN NO EVENT SHALL ADOBE BE LIABLE
 * TO YOU OR ANY OTHER PARTY FOR ANY SPECIAL, INDIRECT, OR CONSEQUENTIAL
 * DAMAGES OR ANY DAMAGES WHATSOEVER WHETHER IN AN ACTION OF CONTRACT,
 * NEGLIGENCE, STRICT LIABILITY OR ANY OTHER ACTION ARISING OUT OF OR IN
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.  ADOBE WILL NOT
 * PROVIDE ANY TRAINING OR OTHER SUPPORT FOR THE SOFTWARE.
 * 
 * PostScript, Display PostScript, and Adobe are trademarks of Adobe Systems
 * Incorporated registered in the U.S.A. and other countries.
 *
 * Author: Adobe Systems Incorporated
 */

#ifdef _NO_PROTO
#define ARGCAST int
#else
#define ARGCAST void *
#endif

#include "wonderland.h"

char fallbackImage[] = " \
	/doimage {gsave -49 -32.5 translate \
	0 setgray 0 0 98 65 rectfill \
	/Helvetica 18 selectfont \
	1 1 0 setrgbcolor 10 40 moveto (Image ) show \
	1 0 1 setrgbcolor (not) show \
	0 1 1 setrgbcolor 10 20 moveto (installed) show \
	grestore} bind def \
";

static XtResource resources[] = {
  {"imageFile", "ImageFile", XtRString, sizeof(char *),
   XtOffset(AppData *, imageFileName), XtRString, "flowers90.ps"},
  {"labelFont", XtCFont, XtRString, sizeof(char *),
   XtOffset(AppData *, font), XtRString, XtDefaultFont},
  {"fallbackImage", "FallbackImage", XtRString, sizeof(char *),
   XtOffset(AppData *, fallbackImage), XtRString, fallbackImage},
  {"debug", "Debug", XtRBoolean, sizeof(Boolean),
   XtOffset(AppData *, debug), XtRImmediate, False}
};

static XrmOptionDescRec options[] = {
    {"-debug", "*debug", XrmoptionNoArg, "on"}
};

DPSContext context;
GC gc;

AppData appData;
Element square, curve, text, image;

static void init_context();

void main(argc, argv)
     int argc;
     char *argv[];
{
    XtAppContext app;
    Boolean debug;
    Widget toplevel;

    toplevel = XtAppInitialize(&app, "Wonderland", options, XtNumber(options),
			       &argc, argv, NULL, NULL, 0);

    XtGetApplicationResources(toplevel, &appData, resources,
			      XtNumber(resources), NULL, 0);

    gc = XCreateGC(XtDisplay(toplevel), RootWindowOfScreen(XtScreen(toplevel)),
		   0, NULL);
    
    init_context(toplevel);
    set_up_window(toplevel);

    XtRealizeWidget(toplevel);
    XtAppMainLoop(app);
}

static void init_context(toplevel)
    Widget toplevel;
{
    Display *dpy = XtDisplay(toplevel);
    unsigned int size;
    FILE *image_file;
    char *imagebuf;
    char *filename;

    /* Make it possible for this client to start a DPS NX agent,
       if "dpsnx.agent" is on the executable search path. */

    (void) XDPSNXSetClientArg(XDPSNX_AUTO_LAUNCH, (ARGCAST) True);

    context = XDPSGetSharedContext(dpy);
    if(context == NULL) {
	fprintf(stderr, "\nwonderland: DPS is not available.\n");
	fprintf(stderr,
          "You need an X server with the DPS extension, or a DPS NX agent.\n");
	exit(1);
    }
  
    DPSSetContext(context);
  
    if(appData.debug) XDPSChainTextContext(context, True);

    image_file = fopen(appData.imageFileName, "r");
    if(image_file == NULL) {
	filename = XtResolvePathname(XtDisplay(toplevel), "images", NULL,
				     ".ps", NULL, (Substitution) NULL, 0,
				     (XtFilePredicate) NULL);
	if (filename == NULL) {
	    filename = XtResolvePathname(XtDisplay(toplevel), NULL,
					 appData.imageFileName, NULL,
					 NULL, (Substitution) NULL, 0,
					 (XtFilePredicate) NULL);
	}
	if (filename != NULL) {
	    image_file = fopen(filename, "r");
	    XtFree(filename);
	}
    }

    if (image_file != NULL) {
	imagebuf = (char *) XtMalloc(8000);

	while(feof(image_file) == 0) {
	    size = fread(imagebuf, 1, 8000, image_file);
	    DPSWritePostScript(context, imagebuf, size);
	}
	fclose(image_file);
	XtFree((XtPointer) imagebuf);
    } else {
	DPSWritePostScript(context, appData.fallbackImage,
			   strlen(fallbackImage));
    }
}  

static void set_gstate(e)
    Element *e;
{
    Cardinal depth;

    if (e->gstate == 0) {
	XtVaGetValues(e->drawingArea, XtNwidth, &e->width,
		      XtNheight, &e->height, XtNdepth, &depth, NULL);
	e->pixmap = XCreatePixmap(XtDisplay(e->drawingArea),
				  XtWindow(e->drawingArea),
				  e->width, e->height, depth);
	XDPSSetContextParameters(context, XtScreen(e->drawingArea), depth,
				 e->pixmap, e->height, NULL, NULL,
				 XDPSContextScreenDepth | XDPSContextDrawable);
	PSsetXoffset(e->width/2, e->height/2);
	PSinitclip();
	XDPSCaptureContextGState(context, &e->gstate);
    } else XDPSSetContextGState(context, e->gstate);
}

static void draw_square(e)
    Element *e;
{
    set_gstate(e);
    PSWDrawSquare(e->red/1000.0, e->green/1000.0, e->blue/1000.0,
		  e->background/1000.0, e->size/100.0, (float) e->angle);
    DPSWaitContext(context);
    XCopyArea(XtDisplay(e->drawingArea), e->pixmap, XtWindow(e->drawingArea),
	      gc, 0, 0, e->width, e->height, 0, 0);
}

void draw_curve(e)
    Element *e;
{
    float x1, x2;

    set_gstate(e);

    x1 = (e->angle/360.0) * (-100.0) + 90.0;
    x2 = (e->angle /360.0) * 130.0 - 100.0;

    PSWDrawCurve(e->red/1000.0, e->green/1000.0, e->blue/1000.0,
		 e->background/1000.0, e->size/100.0, (float) e->angle,
		 x1, x2);
    DPSWaitContext(context);
    XCopyArea(XtDisplay(e->drawingArea), e->pixmap, XtWindow(e->drawingArea),
	      gc, 0, 0, e->width, e->height, 0, 0);
}

static void draw_text(e)
    Element *e;
{
    set_gstate(e);
    PSWDrawText(e->red/1000.0, e->green/1000.0, e->blue/1000.0,
		e->background/1000.0, e->size/100.0, (float) e->angle);
    DPSWaitContext(context);
    XCopyArea(XtDisplay(e->drawingArea), e->pixmap, XtWindow(e->drawingArea),
	      gc, 0, 0, e->width, e->height, 0, 0);
}

void draw_image(e)
    Element *e;
{
    Cardinal depth;

    if (e->gstate == 0) {
	XtVaGetValues(e->drawingArea, XtNwidth, &e->width,
		      XtNheight, &e->height, XtNdepth, &depth, NULL);
	e->pixmap = XCreatePixmap(XtDisplay(e->drawingArea),
				  XtWindow(e->drawingArea),
				  e->width, e->height, depth);
	XDPSSetContextParameters(context, XtScreen(e->drawingArea), depth,
				 e->pixmap, e->height, NULL, NULL,
				 XDPSContextScreenDepth | XDPSContextDrawable);
	PSsetXoffset(e->width/2, e->height/2);
	PSinitclip();
	XDPSCaptureContextGState(context, &e->gstate);

	XDPSSetContextParameters(context, XtScreen(e->drawingArea), depth,
				 XtWindow(e->drawingArea), e->height,
				 NULL, NULL,
				 XDPSContextScreenDepth | XDPSContextDrawable);
	PSsetXoffset(e->width/2, e->height/2);
	PSinitclip();
	XDPSCaptureContextGState(context, &e->windowGstate);
    }

    if (e->buffered) XDPSSetContextGState(context, e->gstate);
    else XDPSSetContextGState(context, e->windowGstate);

    PSWDrawImage(e->red/1000.0, e->green/1000.0, e->blue/1000.0,
		e->background/1000.0, e->size/100.0, (float) e->angle);
    DPSWaitContext(context);
    if (e->buffered) {
	XCopyArea(XtDisplay(e->drawingArea), e->pixmap,
		  XtWindow(e->drawingArea), gc,
		  0, 0, e->width, e->height, 0, 0);
    }
}

/*
 *  Callbacks
 */

void init_drawing(w, client_data, call_data)
    Widget w;
    Element *client_data;
    XtPointer call_data;
{
    if (client_data == &square) draw_square(client_data);
    else if (client_data == &curve) draw_curve(client_data);
    else if (client_data == &text) draw_text(client_data);
    else if (client_data == &image) draw_image(client_data);
}

void red_moved(w, client_data, call_data)
    Widget w;
    Element *client_data;
    XmScaleCallbackStruct *call_data;
{
    client_data->red = call_data->value;
    init_drawing(client_data->drawingArea, client_data, NULL);
}

void green_moved(w, client_data, call_data)
    Widget w;
    Element *client_data;
    XmScaleCallbackStruct *call_data;
{
    client_data->green = call_data->value;
    init_drawing(client_data->drawingArea, client_data, NULL);
}

void blue_moved(w, client_data, call_data)
    Widget w;
    Element *client_data;
    XmScaleCallbackStruct *call_data;
{
    client_data->blue = call_data->value;
    init_drawing(client_data->drawingArea, client_data, NULL);
}

void back_moved(w, client_data, call_data)
    Widget w;
    Element *client_data;
    XmScaleCallbackStruct *call_data;
{
    client_data->background = call_data->value;
    init_drawing(client_data->drawingArea, client_data, NULL);
}

void size_moved(w, client_data, call_data)
    Widget w;
    Element *client_data;
    XmScaleCallbackStruct *call_data;
{
    client_data->size = call_data->value;
    if (client_data->size == 0) client_data->size = 1;
    init_drawing(client_data->drawingArea, client_data, NULL);
}

void angle_moved(w, client_data, call_data)
    Widget w;
    Element *client_data;
    XmScaleCallbackStruct *call_data;
{
    client_data->angle = call_data->value;
    init_drawing(client_data->drawingArea, client_data, NULL);
}

void on_off(w, client_data, call_data)
    Widget w;
    Element *client_data;
    XmToggleButtonCallbackStruct *call_data;
{
    image.buffered = call_data->set;
}


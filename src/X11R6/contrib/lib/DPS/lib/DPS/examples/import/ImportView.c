/*
 * $RCSfile: ImportView.c,v $
 *
 * (c) Copyright 1992-1994 Adobe Systems Incorporated.
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

/***************************************************************
**
**	INCLUDES
**
***************************************************************/

#include "Import.h"
#include <X11/Xproto.h>
#include <sys/stat.h>

/***************************************************************
**
** FUNCTION:    setOrigin
**
** DESCRIPTION: Sets the origin to where it should by using translation and
**		updates the saved transformation matrices.  Re-initializes
**		the matrix and sets it to avoid cumulative roundoff error
**
** PARAMETERS:  None
**
** RETURN:      None.
**
***************************************************************/

void setOrigin()
{
    Point pt;
    XPoint xpt;

    xpt.x = AppData.originX;
    xpt.y = AppData.originY;
    convertToOrigDPS(&xpt, &pt);
    PSWSetMatrixAndGetTransform(pt.x, pt.y, 
				AppData.originX, AppData.originY,
				AppData.ctm, AppData.invctm,
				&AppData.xOffset, &AppData.yOffset);
} /* end setOrigin() */

/***************************************************************
**
** FUNCTION:    scrollProc 
**
** DESCRIPTION: Callback routine for hitting scroll bars
**
** PARAMETERS:  w           callback widget ID
**              clientData  callback client data
**              callData    callback Motif data structure
**
** RETURN:      None.
**
***************************************************************/

/* ARGSUSED */

void scrollProc (w, clientData, callData)
    Widget w;
    XtPointer clientData, callData;
{
    int		x, y;
    int		deltaX, deltaY;
    Widget	draw = AppData.drawingArea;

    /*
    ** Get the new values of the scroll bars
    */
    XtVaGetValues(AppData.hScroll, XmNvalue, &x, NULL);
    XtVaGetValues(AppData.vScroll, XmNvalue, &y, NULL);

    /*
    ** Calculate the delta in the scrolling
    */
    deltaX = x - AppData.scrollX;
    deltaY = y - AppData.scrollY;

    /*
    ** If we are not already scrolling, and there is some scrolling to do,
    ** then scroll the pixmap and copy the pixmap to the window
    */
    if (deltaX == 0 && deltaY == 0) return;

    if (!AppData.scrolling) {
	doScroll(deltaX, deltaY);
	XCopyArea(XtDisplay(draw), AppData.original,
		  XtWindow(draw), AppData.gc, 0, 0,
		  AppData.drawingWidth, AppData.drawingHeight, 0, 0);
	if (AppData.selected) drawSelectionMarks();
    }

    /*
    ** Set the scrolling flag; future scrolls will not update the window
    ** until the current one is done
    */
    AppData.scrolling = True;
} /* end scrollProc() */

/***************************************************************
**
** FUNCTION:    doScroll
**
** DESCRIPTION: Scroll the drawing area by some delta
**
** PARAMETERS:  deltaX	X displacement
**		deltaY	Y displacement
**
** RETURN:      None.
**
***************************************************************/

void doScroll(deltaX, deltaY)
    int deltaX, deltaY;
{
    /*
    ** Set the PS origin in the X window to the new settings of 
    ** the scrollbars
    */
    AppData.originX -= deltaX;
    AppData.originY -= deltaY;

    /*
    ** Update the graphics states to reflect new origin
    */
    (void) XDPSSetContextGState(AppData.dpsCtxt, AppData.origGState);
    setOrigin();
    (void) XDPSUpdateContextGState(AppData.dpsCtxt, AppData.origGState);

    (void) XDPSSetContextGState(AppData.dpsCtxt, AppData.compGState);
    setOrigin();
    (void) XDPSUpdateContextGState(AppData.dpsCtxt, AppData.compGState);

    (void) XDPSSetContextGState(AppData.dpsCtxt, AppData.winGState);
    setOrigin();
    (void) XDPSUpdateContextGState(AppData.dpsCtxt, AppData.winGState);

    /*
    ** Update the stored position of the scroll bars
    */
    AppData.scrollX += deltaX;
    AppData.scrollY += deltaY;

    /*
    ** Redisplay
    */
    drawSelf(NULL);
} /* end doScroll() */

/***************************************************************
**
** FUNCTION:    graphicExpose
**
** DESCRIPTION: Callback routine that handles GraphicsExpose and
**		NoExpose events.  We get one for each copy into the window
**		from the pixmap, so they should always be NoExpose events.
**		When we get one, call scrollProc to see if we need to
**		scroll any more.  Not handling further scroll callbacks
**		until the event arrives makes scrolling much more responsive.
**
** PARAMETERS:	w		window widget
**		clientData	clientdata
**		event		event information
**		goOn		continue to dispatch
**
** RETURN:      None.
**
***************************************************************/

/* ARGSUSED */

void graphicExpose (w, clientData, event, goOn)
    Widget 	w;
    XtPointer clientData;
    XEvent	*event;
    Boolean	*goOn;
{
    if (AppData.scrolling) {
	AppData.scrolling = False;
	scrollProc(w, NULL, NULL);
    }
} /* end graphicExpose() */

/***************************************************************
**
** FUNCTION:    positionDrawingArea 
**
** DESCRIPTION: Routine to position the origin so that the
**		point (ix, iy) on the image, in PS coordinates,
**		corresponds to the point (vx, vy) on the visible 
**		window, in X coords.
**
** PARAMETERS:
**	ix	x coord of point on DPS context (PS coordinates)
**	iy	y coord of point on DPS context (PS coordinates)
**	vx	x coors of point on drawing area (X coordinates)
**	vy	y coors of point on drawing area (X coordinates)
**
** RETURN:      None.
**
***************************************************************/

void positionDrawingArea (ix, iy, vx, vy)
    float	ix;
    float	iy;
    int		vx;
    int		vy;
{
    int		xoff, yoff;
    int 	hSize, vSize;
    float	scrollX, scrollY;

    /* Convert ix, iy into X units */

    ix *= ((float) AppData.scaledWidth) / PAGE_WIDTH;
    iy *= ((float) AppData.scaledHeight) / PAGE_HEIGHT;

    if ((int) AppData.drawingWidth >= AppData.scaledWidth) {
	/*
	** The scaled width is narrower than the view window, so
	** center the picture and set scroll bar to be unscrollable
	*/
	xoff = ((int) AppData.drawingWidth - AppData.scaledWidth) / 2.0;
	scrollX = 0;
	hSize = AppData.scaledWidth;
    } else {
	/*
	** The scaled width is larger than the view window, so
	** turn on the scroll bar, and set up its maximum and
	** slider size.  Do this by subtracting the view offset from the
	** image offset.
	*/
	scrollX = ix - vx;
	scrollX = MAX(scrollX, 0);
	scrollX = MIN(scrollX,
		      AppData.scaledWidth - (int) AppData.drawingWidth);
	hSize = AppData.drawingWidth;
	xoff = -(int) (scrollX + 0.5);
    }

    /*
    ** Now do the same thing for the height.  We want to compute the offset
    ** relative to the lower left corner, but X coordinates are relative
    ** to the upper left, so the drawing height must be added in.  Also, since
    ** the coordinates go in the other direction, the view offset must be
    ** added, not subtracted.
    */
    if ((int) AppData.drawingHeight >= AppData.scaledHeight) {
	yoff = ((int) AppData.drawingHeight - AppData.scaledHeight) / 2.0;
	scrollY = AppData.scaledHeight - (int) AppData.drawingHeight;
	vSize = AppData.scaledHeight;
	vy = AppData.drawingHeight / 2;
    } else {
	scrollY = iy + vy - (int) AppData.drawingHeight;
	scrollY = MAX(scrollY, 0);
	scrollY = MIN(scrollY,
		      AppData.scaledHeight - (int) AppData.drawingHeight);
	vSize = AppData.drawingHeight;
	yoff = -(int) (scrollY + 0.5);
    }

    /*
    ** Update the scrollbars
    */
    AppData.scrollX = (int) (scrollX + 0.5); 
    AppData.scrollY = (int) (AppData.scaledHeight -
			     (int) AppData.drawingHeight - scrollY + 0.5);

    XtVaSetValues(AppData.hScroll, XmNmaximum, AppData.scaledWidth,
		  XmNvalue, AppData.scrollX, XmNsliderSize, hSize, NULL);
    XtVaSetValues(AppData.vScroll, XmNmaximum, AppData.scaledHeight,
		  XmNvalue, AppData.scrollY, XmNsliderSize, vSize, NULL); 
    
    /*
    ** Store the new origin and update the gstates to use this origin
    */
    AppData.originX = xoff;
    AppData.originY = AppData.drawingHeight - yoff;

    (void) XDPSSetContextGState(AppData.dpsCtxt, AppData.origGState);
    setOrigin();
    (void) XDPSUpdateContextGState(AppData.dpsCtxt, AppData.origGState);

    (void) XDPSSetContextGState(AppData.dpsCtxt, AppData.compGState);
    setOrigin();
    (void) XDPSUpdateContextGState(AppData.dpsCtxt, AppData.compGState);

    (void) XDPSSetContextGState(AppData.dpsCtxt, AppData.winGState);
    setOrigin();
    (void) XDPSUpdateContextGState(AppData.dpsCtxt, AppData.winGState);
} /* end positionDrawingArea() */

/***************************************************************
**
** FUNCTION:    pixmapHandler
**
** DESCRIPTION: New error handler to detect pixmap allocation failure
**
** PARAMETERS:  dpy	Current display
**		error	Error event
**
** RETURN:      0
**
***************************************************************/

static Boolean pixmapError;
static int (*oldHandler)();

static int pixmapHandler(dpy, error)
    Display *dpy;
    XErrorEvent *error;
{
    if (error->error_code == BadAlloc &&
	error->request_code == X_CreatePixmap) {
	pixmapError = True;
	return 0;
    } else return (*oldHandler) (dpy, error);
} /* end pixmapHandler() */

/***************************************************************
**
** FUNCTION:    allocPixmap
**
** DESCRIPTION: Allocate a pixmap of the desired size, but if
**		it would exceed the memory limit or if the allocation
**		fails, return None
**
** PARAMETERS:  dpy	The display
**		win	A window on the screen
**		w, h	The pixmap dimensions
**		d	Pixmap depth
**
** RETURN:      The pixmap, or None
**
***************************************************************/

Pixmap allocPixmap(dpy, win, w, h, d)
    Display *dpy;
    Window win;
    unsigned int w, h, d;
{
    Pixmap p;
    unsigned int dBytes;

    dBytes = (d + 7) / 8;		/* Convert into bytes */
    if (w * h * dBytes > AppData.pixmapMaxSize) return None;

    XSync(dpy, False);
    oldHandler = XSetErrorHandler(pixmapHandler);
    pixmapError = False;
    p = XCreatePixmap(dpy, win, w, h, d);
    XSync(dpy, False);
    (void) XSetErrorHandler(oldHandler);
    if (pixmapError) return None;
    else return p;
} /* end allocPixmap() */

/*
** Constant for use with the next four procedures.  Tells
** XDPSSetContextParameters to set all context parameters.
*/

#define XDPSContextAll (XDPSContextScreenDepth | XDPSContextDrawable |\
			XDPSContextGrayMap | XDPSContextRGBMap)

/*************************************************************
**
** FUNCTION:	setDrawingParameters
**
** DESCRIPTION:	Sets up a context to draw into a pixmap.  Assumes that
**		it has the depth and screen of the drawing area and uses
**		the default color cube and gray ramp.  Puts the origin in the
**		lower left corner of the pixmap
**
** PARAMETERS:	d	Drawable (pixmap)
**		height	Height of pixmap
**
** RETURN:	None
**
*************************************************************/

static void setDrawingParameters(d, height)
    Drawable d;
    int height;
{
    (void) XDPSSetContextParameters(AppData.imageCtxt, 
		XtScreen(AppData.drawingArea), AppData.depth, d, height,
		(XDPSStandardColormap *) NULL, (XDPSStandardColormap *) NULL, 
		XDPSContextAll);
} /* end setDrawingParameters() */

/*************************************************************
**
** FUNCTION:	setMaskParameters
**
** DESCRIPTION:	Sets up a context to draw into a mask bitmap.  Assumes that
**		it has depth 1 and the screen of the drawing area.  Sets
**		a mask transfer and puts the origin in the
**		lower left corner of the bitmap
**
** PARAMETERS:	d	Drawable (bitmap)
**		height	Height of bitmap
**
** RETURN:	None
**
*************************************************************/

static void setMaskParameters(d, height)
    Drawable d;
    int height;
{
    XDPSStandardColormap maskMap;
    XDPSStandardColormap rgbMap;

    /*
    ** Drawing is with a gray ramp that has black = 1, white = 0
    */
    maskMap.colormap = None;
    maskMap.red_max = 1;
    maskMap.red_mult = -1;
    maskMap.base_pixel = 1;

    /*
    ** No color map, just draw in black and white
    */
    rgbMap.colormap = None;
    rgbMap.red_max = rgbMap.green_max = rgbMap.blue_max = 
	    rgbMap.red_mult = rgbMap.green_mult = rgbMap.blue_mult =
	    rgbMap.base_pixel = 0;

    (void) XDPSSetContextParameters(AppData.imageCtxt,
				    XtScreen(AppData.drawingArea), 1,
				    d, height, &rgbMap, &maskMap,
				    XDPSContextAll);

    /*
    ** Set transfer functions to that any color comes out as black
    */
    PSWSetMaskTransfer(AppData.imageCtxt);
} /* end setMaskParameters() */

/*************************************************************
**
** FUNCTION:	setEPSIBitmapParameters
**
** DESCRIPTION:	Sets up a context to draw into a preview bitmap.  Assumes
**		that it has depth 1 and the screen of the drawing area.  
**		Puts the origin in the lower left corner of the bitmap
**
** PARAMETERS:	d	Drawable (bitmap)
**		height	Height of bitmap
**
** RETURN:	None
**
*************************************************************/

void setEPSIBitmapParameters(d, height)
    Drawable d;
    int height;
{
    XDPSStandardColormap grayMap;
    XDPSStandardColormap rgbMap;

    /*
    ** Set gray ramp so that black = 1, white = 0
    */
    grayMap.colormap = None;
    grayMap.red_max = 1;
    grayMap.red_mult = -1;
    grayMap.base_pixel = 1;

    /*
    ** No color map, just draw in black and white
    */
    rgbMap.colormap = None;
    rgbMap.red_max = rgbMap.green_max = rgbMap.blue_max = 
	    rgbMap.red_mult = rgbMap.green_mult = rgbMap.blue_mult =
	    rgbMap.base_pixel = 0;

    (void) XDPSSetContextParameters(AppData.imageCtxt, 
				    XtScreen(AppData.drawingArea), 1,
				    d, height, &rgbMap, &grayMap,
				    XDPSContextAll);
} /* end setEPSIBitmapParameters() */

/*************************************************************
**
** FUNCTION:	setEPSIPixmapParameters
**
** DESCRIPTION:	Sets up a context to draw into a preview pixmap.  Assumes
**		that it has depth 8 and the screen of the drawing area.  
**		Puts the origin in the lower left corner of the bitmap
**
** PARAMETERS:	d	Drawable (pixmap)
**		height	Height of pixmap
**
** RETURN:	None
**
*************************************************************/

void setEPSIPixmapParameters(d, height)
    Drawable d;
    int height;
{
    XDPSStandardColormap grayMap;
    XDPSStandardColormap rgbMap;

    /*
    ** Set gray ramp with black = 255, white = 0, and 255 values in between
    */
    grayMap.colormap = None;
    grayMap.red_max = 255;
    grayMap.red_mult = -1;
    grayMap.base_pixel = 255;

    /*
    ** No color map, just draw in shades of gray
    */
    rgbMap.colormap = None;
    rgbMap.red_max = rgbMap.green_max = rgbMap.blue_max = 
	    rgbMap.red_mult = rgbMap.green_mult = rgbMap.blue_mult =
	    rgbMap.base_pixel = 0;

    (void) XDPSSetContextParameters(AppData.imageCtxt, 
				    XtScreen(AppData.drawingArea), 8,
				    d, height, &rgbMap, &grayMap,
				    XDPSContextAll);
} /* end setEPSIPixmapParameters() */

/*************************************************************
**
** FUNCTION:	calculatePositionParameters
**
** DESCRIPTION:	Compute scale, translation, and rotation values
**		for a newly added element.
**
** PARAMETERS:	e	Element being added
**
** RETURN:	None
**
*************************************************************/

static void calculatePositionParameters(e)
    Element *e;
{
    Point pt;
    XPoint xpt;
    int width, height;

    /*
    ** Compute size of picture in user space
    */
    width = e->origBBox.ur.x - e->origBBox.ll.x;
    height = e->origBBox.ur.y - e->origBBox.ll.y;

    /*
    ** Compute scale by dividing size of X bounding box by size of user space
    ** bounding box and converting from user space to X units
    */
    e->sx = (float) e->xBBox.width *
	    (PAGE_WIDTH / (float) AppData.scaledWidth) / (float) width;
    e->sy = (float) e->xBBox.height *
	    (PAGE_HEIGHT / (float) AppData.scaledHeight) / (float) height;

    /*
    ** Compute translation by taking origin of bounding box and converting
    ** into user space.  This gives the translation relative to the window,
    ** so factor in the window origin to obtain the translation in the page
    */
    xpt.x = e->sizeBox.x + AppData.originX;
    xpt.y = e->sizeBox.y - AppData.scaledHeight + AppData.originY;
    convertToDPS(&xpt, &pt);
    e->tx = pt.x;
    e->ty = pt.y;

    xpt.x = AppData.originX;
    xpt.y = AppData.originY;
    convertToDPS(&xpt, &pt);
    e->tx -= pt.x;
    e->ty -= pt.y;

    e->rotation = 0;
} /* end calculatePositionParameters() */

/*************************************************************
**
** FUNCTION:	imageToPixmap
**
** DESCRIPTION:	Renders into the image and mask pixmaps in an element
**
** PARAMETERS:	e	Element to render
**
** RETURN:	None
**
*************************************************************/

static Boolean imageToPixmap(e)
    Element *e;
{
    XPoint xpt;
    Point pt;

    /*
    ** Compute the user space coordinates of the origin of the pixmap.  This
    ** must be subtracted from the translation of the element to obtain
    ** the translation within the pixmap
    */
    xpt.x = e->xBBox.x + AppData.originX;
    xpt.y = e->xBBox.y + e->xBBox.height - AppData.scaledHeight +
	    AppData.originY;
    convertToDPS(&xpt, &pt);

    /*
    ** Set up context for rendering and transform the coordinate system
    ** for EPSF inclusion
    */
    setDrawingParameters(e->image, e->xBBox.height);
    PSWTransformBeforeEPSF(AppData.imageCtxt, e->tx - pt.x, e->ty - pt.y,
			   e->sx, e->sy, e->rotation,
			   -e->origBBox.ll.x, -e->origBBox.ll.y);
    DPSerasepage(AppData.imageCtxt);

    /*
    ** Execute the file using the current settings.  imageFile
    ** will return whether there was an error.  If there was no error,
    ** proceed to the mask rendering
    */
    if (!imageFile(e)) {
	DPSgsave(AppData.imageCtxt);
	setMaskParameters(e->mask, e->xBBox.height);
	PSWTransformBeforeEPSF(AppData.imageCtxt, e->tx - pt.x, e->ty - pt.y,
			       e->sx, e->sy, e->rotation,
			       -e->origBBox.ll.x, -e->origBBox.ll.y);
	/*
	** We need to make sure the mask starts out with 0's
	*/
	XFillRectangle(XtDisplay(AppData.drawingArea), e->mask,
		       AppData.bitmapgc, 0, 0,
		       e->xBBox.width, e->xBBox.height);
	(void) imageFile(e);
	DPSgrestore(AppData.imageCtxt);
	return False;
    } else return True;
} /* end imageToPixmap() */

/*************************************************************
**
** FUNCTION:	renderElement
**
** DESCRIPTION:	Allocate pixmaps and render an element
**
** PARAMETERS:	e	Element to render
**
** RETURN:	None
**
*************************************************************/

void renderElement(e)
    Element *e;
{
    Display *dpy = XtDisplay(AppData.drawingArea);
    Window w = XtWindow(AppData.drawingArea);

    /*
    ** Allocate image and mask pixmaps
    */
    e->image = allocPixmap(dpy, w, e->xBBox.width, e->xBBox.height,
			   AppData.depth);
    e->mask = allocPixmap(dpy, w, e->xBBox.width, e->xBBox.height, 1);

    if (e->image == None) {
	fprintf(stderr, "Could not allocate pixmap to hold file %s\n",
		e->filename);
	if (e->mask != None) {
	    XFreePixmap(dpy, e->mask);
	    e->mask = None;
	}
    } else if (e->mask == None) {
	fprintf(stderr, "Could not allocate mask for file %s\n",
		e->filename);
	XFreePixmap(dpy, e->image);
	e->image = None;
    } else {
	XDefineCursor(dpy, w, AppData.busyCursor);
	XFlush(dpy);
	
	/*
	** Execute the EPS file.  If unsucessful, give error and free pixmaps
	*/
	if (imageToPixmap(e)) {
	    fprintf(stderr, "PostScript language execution error in file %s\n",
		    e->filename);
	    XFreePixmap(dpy, e->image);
	    XFreePixmap(dpy, e->mask);
	    e->image = e->mask = None;
	}
	XDefineCursor(dpy, w, None);
    }
} /* end renderElement() */

/*************************************************************
**
** FUNCTION:	addElement
**
** DESCRIPTION:	Add an element to the picture.  Make it fit within
**		the specified rectangle
**
** PARAMETERS:	rect	Rectangle to hold element
**
** RETURN:	None
**
*************************************************************/

void addElement(rect)
    XRect *rect;
{
    /*
    ** The xBBox is the rectangle, but with the origin adjusted to be
    ** relative to the page.  This avoids having to move it whenever we
    ** scroll
    */
    AppData.adding->xBBox = *rect;
    AppData.adding->xBBox.x -= AppData.originX;
    AppData.adding->xBBox.y += AppData.scaledHeight - AppData.originY;

    /*
    ** The sizeBox starts out the same as the xBBox, but it has its origin
    ** coincident with the picture origin (and thus is not a valid X
    ** rectangle, since the height must be negative (towards the top
    ** of the screen)
    */
    AppData.adding->sizeBox = AppData.adding->xBBox;
    AppData.adding->sizeBox.y += AppData.adding->sizeBox.height;
    AppData.adding->sizeBox.height = -AppData.adding->sizeBox.height;

    /*
    ** Compute translation, scale, and rotation based on the rectangle
    */
    calculatePositionParameters(AppData.adding);

    /*
    ** If not using boxes, render the element.  If rendering was unsuccessful
    ** abort adding
    */
    if (!AppData.useBoxes) {
	renderElement(AppData.adding);
	if (AppData.adding->image == None) {
	    freeElement(AppData.adding);
	    AppData.adding = NULL;
	    return;
	}
    }

    /*
    ** Add element to the list and select it
    */
    if (AppData.elements == NULL) AppData.lastElement = AppData.adding;
    else AppData.elements->prev = AppData.adding;
    AppData.adding->next = AppData.elements;
    AppData.adding->prev = NULL;
    AppData.elements = AppData.adding;

    drawSelfAndUpdate(AppData.adding);
    selectElement(AppData.adding);
    AppData.adding = NULL;
} /* end addElement() */

/*************************************************************
**
** FUNCTION:	updateElement
**
** DESCRIPTION:	Update the bounding boxes of an element to reflect
**		changed translation, scale, or rotation
**
** PARAMETERS:	e	Element to update
**
** RETURN:	None
**
*************************************************************/

void updateElement(e)
    Element *e;
{
    Display *dpy = XtDisplay(AppData.drawingArea);
    int llx, lly, urx, ury;
    XPoint xpt1, xpt2;
    Point pt;
    int width, height;

    /*
    ** Compute the bounding box of the element and convert to X coordinates
    */
    computeBBox(e, &llx, &lly, &urx, &ury);
    pt.x = llx;
    pt.y = lly;
    convertToX(&xpt1, &pt);
    pt.x = urx;
    pt.y = ury;
    convertToX(&xpt2, &pt);
    
    /*
    ** The xBBox is exactly the same as the bounding box, but adjusted to
    ** be relative to the page instead of the origin
    */
    e->xBBox.x = xpt1.x - AppData.originX;
    e->xBBox.y = xpt2.y + AppData.scaledHeight - AppData.originY;
    e->xBBox.width = xpt2.x - xpt1.x;
    e->xBBox.height = xpt1.y - xpt2.y;

    /*
    ** The origin of sizeBox is the same as the origin of the picture
    ** (i.e. the translation of the picture)
    */
    pt.x = e->tx;
    pt.y = e->ty;
    convertToX(&xpt1, &pt);
    e->sizeBox.x = xpt1.x - AppData.originX;
    e->sizeBox.y = xpt1.y + AppData.scaledHeight - AppData.originY;

    /*
    ** The size of sizeBox is the size of the picture, scaled and converted
    ** to X coordinates
    */
    width = e->origBBox.ur.x - e->origBBox.ll.x;
    height = e->origBBox.ur.y - e->origBBox.ll.y;

    e->sizeBox.width = width * e->sx *
	    ((float) AppData.scaledWidth / PAGE_WIDTH);
    e->sizeBox.height = -height * e->sy *
	    ((float) AppData.scaledHeight / PAGE_HEIGHT);

    /*
    ** Free old pixmaps and re-render using new values
    */
    if (e->image != None) XFreePixmap(dpy, e->image);
    if (e->mask != None) XFreePixmap(dpy, e->mask);
    e->image = e->mask = None;

    if (!AppData.useBoxes) renderElement(e);
} /* end updateElement() */

/*************************************************************
**
** FUNCTION:	computeXrect
**
** DESCRIPTION:	Compute the X rectangle necessary to hold the element.
**		Assumes the scale is (1,1) and the rotation 0
**
** PARAMETERS:	e	Element to compute
**
** RETURN:	rect	Size of element
**
*************************************************************/

static void computeXrect(e, rect)
    Element *e;
    XRect *rect;
{
    XPoint xpt;
    Point pt;

    /*
    ** The origin of the rectangle is the lower left corner
    */
    pt.x = e->origBBox.ll.x;
    pt.y = e->origBBox.ll.y;
    convertToX(&xpt, &pt);
    rect->x = xpt.x;
    rect->y = xpt.y;

    /*
    ** Convert the upper right corner and subtract to get the size
    ** Negate the height to account for X coordinate direction and move
    ** the origin up to the upper right corner
    */
    pt.x = e->origBBox.ur.x;
    pt.y = e->origBBox.ur.y;
    convertToX(&xpt, &pt);
    rect->width = xpt.x - rect->x;
    rect->height = rect->y - xpt.y;

    rect->y -= rect->height;
} /* end computeXrect() */

/*************************************************************
**
** FUNCTION:	copyToFile
**
** DESCRIPTION:	Copy a string into a temporary file
**
** PARAMETERS:	buf		String to copy
**		len		Length of buffer
**		filename	Buffer to hold file name
**
** RETURN:	Whether copy was successful
**
*************************************************************/

static Boolean copyToFile(buf, len, filename)
    char *buf;
    unsigned long len;
    char *filename;
{
    static int suffix = 0;
    static int pid;
    FILE *f;

    /*
    ** Construct a unique file name based on the proccess id
    */
    if (suffix == 0) pid = (int) getpid();
    sprintf(filename, "/tmp/import-%d-%d", pid, suffix);
    suffix++;

    /*
    ** Open file and write buffer
    */
    f = fopen(filename, "w");
    if (f == NULL) {
	fprintf(stderr, "Couldn't create temporary file to hold selection.\n");
	XtFree(buf);
	return True;
    }
    if (fwrite(buf, 1, len-1, f) != len-1) {
	fprintf(stderr, "Couldn't write selection to temporary file.\n");
	XtFree(buf);
	fclose(f);
	return True;
    }
    fclose(f);
    XtFree(buf);
    return False;
} /* end copyToFile() */

/*************************************************************
**
** FUNCTION:	pasteEPS
**
** DESCRIPTION:	Paste an EPS buffer into the picture.  Copy it into
**		a temporary file then add the file
**
** PARAMETERS:	buf	String containing EPS description
**		len	Length of string
**
** RETURN:	None
**
*************************************************************/

void pasteEPS(buf, len)
    char *buf;
    unsigned long len;
{
    char filenamebuf[20];
    FILE *f;
    Element *e;
    struct stat sbuf;
    XRect rect;

    /*
    ** Copy buffer to a temporary file
    */
    if (copyToFile(buf, len, filenamebuf)) return;

    /*
    ** Reopen file and unlink it so it will go away when the application
    ** exits
    */
    f = fopen(filenamebuf, "r");
    if (f == NULL) {
	fprintf(stderr, "Couldn't reopen temporary file with selection.\n");
	return;
    }
    unlink(filenamebuf);

    /*
    ** Add file just like any other
    */
    if (AppData.adding != NULL) {
	e = AppData.adding;
	XtFree(e->filename);
	freeResourceList(e->resources);
	fclose(e->f);
    } else {
	e = XtNew(Element);
	e->tx = e->ty = e->sx = e->sy = e->rotation = 0.0;
	e->origBBox.ll.x = e->origBBox.ll.y =
		e->origBBox.ur.x = e->origBBox.ur.y = 0.0;
    }

    (void) fstat(fileno(f), &sbuf);
    e->filename = XtNewString(filenamebuf);
    e->length = sbuf.st_size;
    e->f = f;
    e->image = e->mask = None;
    e->next = NULL;
    e->resources = NULL;

    if (parseFileHeader(e)) {
	AppData.adding = e;
    } else {
	XtFree(e->filename);
	fclose(e->f);
	XtFree((XtPointer) e);
	AppData.adding = NULL;
	return;
    }

    if (AppData.selected != NULL) unselect();

    computeXrect(e, &rect);
    addElement(&rect);
} /* end pasteEPS() */

/*************************************************************
**
** FUNCTION:	selectElement
**
** DESCRIPTION:	Draw selection marks around an element
**
** PARAMETERS:	e	Element to select
**
** RETURN:	None
**
*************************************************************/

void selectElement(e)
    Element *e;
{
    if (AppData.selected == e) return;
    if (AppData.selected != NULL) unselect();
    AppData.selected = e;
    drawSelectionMarks();
} /* end selectElement() */

/*************************************************************
**
** FUNCTION:	unselect
**
** DESCRIPTION:	Unselects an element and updates the display
**		to erase selection marks
**
** PARAMETERS:	None
**
** RETURN:	None
**
*************************************************************/

void unselect()
{
    if (AppData.selected == NULL) return;

    AppData.selected = NULL;
    XCopyArea(XtDisplay(AppData.drawingArea), AppData.original,
	      XtWindow(AppData.drawingArea), AppData.gc, 0, 0,
	      AppData.drawingWidth, AppData.drawingHeight, 0, 0);
} /* end unselect() */

/*************************************************************
**
** FUNCTION:	freeElement
**
** DESCRIPTION:	Free the storage used in an element structure
**
** PARAMETERS:	e	Element to free
**
** RETURN:	None
**
*************************************************************/

void freeElement(e)
    Element *e;
{
    Display *dpy = XtDisplay(AppData.drawingArea);

    XtFree(e->filename);
    fclose(e->f);
    if (e->image != None) XFreePixmap(dpy, e->image);
    if (e->mask != None) XFreePixmap(dpy, e->mask);
    XtFree((XtPointer) e);
} /* end freeElement() */

/*************************************************************
**
** FUNCTION:	pointInElement
**
** DESCRIPTION:	Determines whether a point is within an element
**
** PARAMETERS:	xpt	X coordinates of point
**		e	Element to test
**
** RETURN:	None
**
*************************************************************/

Boolean pointInElement(xpt, e)
    XPoint *xpt;
    Element *e;
{
    int x, y;
    float r, theta;
    int minx, miny, maxx, maxy;

    /*
    ** If not within the bounding box, cannot be within the element
    */
    if (xpt->x < e->xBBox.x || xpt->x > e->xBBox.x + e->xBBox.width ||
	xpt->y < e->xBBox.y ||
	xpt->y > e->xBBox.y + e->xBBox.height) return False;

    /*
    ** If the rotation is 0, the xBBox is the true bounding box, so
    ** nothing more to test.
    */
    if (e->rotation == 0.0) return True;

    /*
    ** If we're at the origin, we're in.
    */
    if (xpt->x == e->sizeBox.x && xpt->y == e->sizeBox.y) return True;

    /*
    ** Rotate the point around the sizeBox origing by the element's rotation
    */
    x = xpt->x - e->sizeBox.x;
    y = xpt->y - e->sizeBox.y;

    r = sqrt((float) (x * x + y * y));
    theta = atan2((float) y, (float) x) + DTOR(e->rotation);

    x = r * cos(theta) + e->sizeBox.x;
    y = r * sin(theta) + e->sizeBox.y;

    /*
    ** See whether the rotated point is within the sizeBox
    */
    minx = MIN(e->sizeBox.x, e->sizeBox.x + e->sizeBox.width);
    maxx = MAX(e->sizeBox.x, e->sizeBox.x + e->sizeBox.width);
    miny = MIN(e->sizeBox.y, e->sizeBox.y + e->sizeBox.height);
    maxy = MAX(e->sizeBox.y, e->sizeBox.y + e->sizeBox.height);

    return (x >= minx && x <= maxx && y >= miny && y <= maxy);
} /* end pointInElement() */

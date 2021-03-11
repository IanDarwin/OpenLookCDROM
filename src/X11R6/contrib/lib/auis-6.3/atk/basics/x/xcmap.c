/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */

/*
	$Disclaimer: 
 * Permission to use, copy, modify, and distribute this software and its 
 * documentation for any purpose is hereby granted without fee, 
 * provided that the above copyright notice appear in all copies and that 
 * both that copyright notice, this permission notice, and the following 
 * disclaimer appear in supporting documentation, and that the names of 
 * IBM, Carnegie Mellon University, and other copyright holders, not be 
 * used in advertising or publicity pertaining to distribution of the software 
 * without specific, written prior permission.
 * 
 * IBM, CARNEGIE MELLON UNIVERSITY, AND THE OTHER COPYRIGHT HOLDERS 
 * DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
 * ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.  IN NO EVENT 
 * SHALL IBM, CARNEGIE MELLON UNIVERSITY, OR ANY OTHER COPYRIGHT HOLDER 
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
 * DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, 
 * WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS 
 * ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
 * OF THIS SOFTWARE.
 *  $
*/


#ifndef NORCSID
#define NORCSID
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/basics/x/RCS/xcmap.c,v 1.17 1994/02/17 16:45:39 rr2b Exp $";
#endif
#include <andrewos.h>
#include <X11/X.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <view.ih>
#include <image.ih>
#include <xcolor.ih>
#include <xgraphic.ih>
#include <xim.ih>
#include <cmap.ih>
#include <xcmap.eh>

boolean
xcolormap__InitializeClass( classID )
    struct classheader *classID;
{
    return(TRUE);
}

boolean
xcolormap__InitializeObject( classID, self )
    struct classheader *classID;
    struct xcolormap *self;
{
    self->XColorMap = NULL;
    self->display = NULL;
    return(TRUE);
}

void
xcolormap__FinalizeObject( classID, self )
struct classheader *classID;
struct xcolormap *self;
{
    int i;
    struct xcolor *xc=NULL;
    for(i = xcolormap_Used(self)-1; i>=0; i--) {
	xc = (struct xcolor *) xcolormap_NthColor(self, i);
	if(xc) {
	    xcolor_RemoveObserver(xc, self);
	    xcolormap_RemoveObserver(self, xc);
	    if(xcolor_ReferenceCount(xc) == 1)
		/* Last ref count on color -- it must be destroyed */
		xcolormap_DestroyColor(self, xc);
	    else
		/* This decrements the ref count */
		xcolor_Destroy(xc);
	}
    }
}

int
xcolormap__Copy( self, source )
    struct xcolormap *self, *source;
{
}

int
xcolormap__Merge( self, other )
    struct xcolormap *self, *other;
{
}

void
xcolormap_GetClosestColor( self, xc )
    struct xcolormap *self;
    struct xcolor *xc;
{
    struct xcolor *tmp, *best = NULL;
    unsigned short R, G, B;
    unsigned short xcR, xcG, xcB;
    register int i = -1, nc = xcolormap_Size(self), status, bestDef = -1;
    unsigned int diff = 0, bestDiff = ~0;
    XColor *defs;

    xcolor_GetRGB(xc, xcR, xcG, xcB); 
    if(defs = (XColor *) calloc(nc, sizeof(XColor))) {
	for( i = 0; i < nc; i++ ) defs[i].pixel = i;
	XQueryColors(xcolormap_XDisplay(self), self->XColorMap, defs, nc);
	for( i = 0; i < nc; i++ ) {
	    diff = abs(defs[i].red - xcR) +
	      abs(defs[i].green - xcG) +
	      abs(defs[i].blue - xcB);
	    if( diff < bestDiff) {
		bestDiff = diff;
		bestDef = i;
	    }
	}
    }
    for( i = 0; i < xcolormap_Used(self); i++ ) {
	tmp = (struct xcolor *) xcolormap_NthColor(self, i);
	if( tmp->haspixel ) {
	    R = tmp->color.red; G = tmp->color.green; B = tmp->color.blue;
	    diff = abs(R - xcR) + abs(G - xcG) + abs(B - xcB);
	    if( diff < bestDiff ) {
		bestDiff = diff;
		best = tmp;
	    }
	}
    }
    if( best )
	xc->color = best->color;
    else if( bestDef >= 0 ) {
	if( status = XAllocColor(xcolormap_XDisplay(self), self->XColorMap, defs + bestDef) )
	    xc->color = defs[bestDef];
    }
    if( defs ) free(defs);
}

struct xcolor *
xcolormap__AllocColor( self, name, r, g, b, needpixel )
    struct xcolormap *self;
    char *name;
    unsigned int r, g, b;
    boolean needpixel;
{
    struct xcolor *xc;

    if((xc = xcolormap_LookupColor(self, name, r, g, b, needpixel)) == NULL) {
	xc = xcolor_Create(name, r, g, b);
	if(xcolormap_SetColor(self, xc, needpixel) == FALSE)
	    xcolormap_GetClosestColor(self, xc);
    }
    return(xc);
}

struct xcolor *
xcolormap__LookupColor( self, name, r, g, b, needpixel )
    struct xcolormap *self;
    char *name;
    unsigned int r, g, b;
    boolean needpixel;
{
    struct xcolor *xc;
    Display *dpy = xcolormap_XDisplay(self);
    register int i;

    if(!dpy) return(NULL);
    for(i = 0; i < xcolormap_Used(self); i++) {
	xc = (struct xcolor *) xcolormap_NthColor(self, i);
	if(name) {
	    if((!needpixel || xc->haspixel) && (xc->dpy == dpy) && xcolor_Name(xc) && !strcmp(xcolor_Name(xc), name))
		return(xc);
	}
	else {
	    unsigned short R, G, B;
	    xcolor_GetRGB(xc, R, G, B);
	    if((!needpixel || xc->haspixel) && (xc->dpy == dpy) && (r == R) && (g == G) && (b == B))
		return(xc);
	}
    }
    return(NULL);
}

int
xcolormap__SetColor( self, xc, needpixel )
    struct xcolormap *self;
    struct xcolor *xc;
    boolean needpixel;
{
    XColor *color = &xc->color;
    Display *dpy = xcolormap_XDisplay(self);
    int scrn = DefaultScreen(dpy), status;
    unsigned int R, G, B;
    unsigned long pmr[1], pix[1];

    pix[0] = 0;
    color->flags = DoRed | DoGreen | DoBlue;
    super_SetColor(self, xc, needpixel);
    xcolor_SetColormap(xc, self);
    xcolormap_AddObserver(self, xc);
    xcolor_AddObserver(xc, self);
    xc->dpy = dpy;
    if (xcolor_Name(xc)) {
	status = XParseColor(dpy, self->XColorMap, xcolor_Name(xc), color);
	if (status) {
	    xcolor_SetRGB(xc, color->red, color->green, color->blue);
	    if(needpixel) {
		if(DefaultColormap(dpy, scrn) == self->XColorMap) {
		    if(status = XAllocColor(dpy, self->XColorMap, color))
			xc->haspixel = TRUE;
		    else return(status);
		}
		else {
		    if(status = XAllocColorCells(dpy, self->XColorMap, FALSE, pmr, 0, pix, 1)) {
			xcolor_Pixel(xc) = *pix;
			XStoreColor(dpy, self->XColorMap, color);
			xc->haspixel = TRUE;
		    }
		    else return(status);
		}
	    }
	    else return(TRUE);
	}
	else fprintf(stderr, "xcmap: couldn't parse color name %s\n", xcolor_Name(xc));
    }
    else {
	xcolor_GetRGB(xc, R, G, B);
	color->red = R;
	color->blue = B;
	color->green = G;
	if(needpixel) {
	    if(DefaultColormap(dpy, scrn) == self->XColorMap) {
		if(status = XAllocColor(dpy, self->XColorMap, color))
		    xc->haspixel = TRUE;
		else return(status);
	    }
	    else {
		if(status = XAllocColorCells(dpy, self->XColorMap, FALSE, pmr, 0, pix, 1)) {
		    xcolor_Pixel(xc) = *pix;
		    XStoreColor(dpy, self->XColorMap, color);
		    xc->haspixel = TRUE;
		}
		else return(status);
	    }
	}
	else return(TRUE);
    }
    return(status);
}

int
xcolormap__ChangeColor( self, xc )
    struct xcolormap *self;
    struct xcolor *xc;
{
    Display *dpy = xcolormap_XDisplay(self);
    int scrn = DefaultScreen(dpy), status = 0;

    if(DefaultColormap(dpy, scrn) != self->XColorMap) {
	XColor *c = &xc->color;
    unsigned int R, G, B;
	c->flags = DoRed | DoGreen | DoBlue;
	xcolor_GetRGB(xc, R, G, B);
	c->red = R;
	c->blue = B;
	c->green = G;
	XStoreColor(dpy, self->XColorMap, c);
	status = 1;
    }
    return(status);
}

struct xcolormap *
xcolormap__Create( classID, xim )
    struct classheader *classID;
    struct xim *xim;
{
    struct xcolormap *xcmap = xcolormap_New();
    xcmap->display = xim2display(xim);
    return(xcmap);
}

void xcolormap__DestroyColor( self, xc )
struct xcolormap *self;
struct xcolor *xc;
{
    unsigned long pixels[1];
    Display *disp = xcolormap_XDisplay(self);
    pixels[0] = xcolor_Pixel(xc);
    XFreeColors(disp, self->XColorMap, pixels, 1, 0);
    super_DestroyColor(self, xc);
    xcolor_Destroy(xc);
}


void xcolormap__ObservedChanged(self, changed, value)
struct xcolormap *self;
struct observable *changed;
long value;
{
    super_ObservedChanged(self, changed, value);
    /* an xgraphic is being destroyed, make sure we don't notify it of future changes. */
    if(value==observable_OBJECTDESTROYED) xcolormap_RemoveObserver(self, changed);
}

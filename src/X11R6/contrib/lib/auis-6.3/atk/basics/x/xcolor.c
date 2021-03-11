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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/basics/x/RCS/xcolor.c,v 1.10 1994/02/17 16:45:39 rr2b Exp $";
#endif
#include <andrewos.h>
#include <X11/X.h>
#include <X11/Xlib.h>
#include <xcmap.ih>
#include <xgraphic.ih>
#include <traced.ih>
#include <observe.ih>
#include <xcolor.eh>

boolean
xcolor__InitializeClass( classID )
    struct classheader *classID;
{
    return (TRUE);
}

boolean
xcolor__InitializeObject( classID, self )
    struct classheader *classID;
    struct xcolor *self;
{
    self->dpy = NULL;
    self->used = self->haspixel = FALSE;
    self->cmap = NULL;
    return(TRUE);
}


void
xcolor__FinalizeObject( classID, self )
struct classheader *classID;
struct xcolor *self;
{
}

struct xcolor *
xcolor__Create( classID, name, R, G, B )
    struct classheader *classID;
    char *name;
    unsigned short R, G, B;
{
    struct xcolor *self = NULL;
    if(self = xcolor_New()) {
	if(name)
	    xcolor_SetName(self, name);
	xcolor_SetRGB(self, R, G, B);
    }
    return(self);
}


void xcolor__ObservedChanged(self, changed, value)
struct xcolor *self;
struct observable *changed;
long value;
{
    struct classinfo *xcmapinfo=NULL;
    struct classinfo *xgraphicinfo=NULL;

    if(xcmapinfo==NULL) xcmapinfo=class_Load("xcolormap");
    if(xgraphicinfo==NULL) xgraphicinfo=class_Load("xgraphic");
    
    super_ObservedChanged(self, changed, value);
    switch(value) {
	case observable_OBJECTCHANGED:
	    break;
	case observable_OBJECTDESTROYED:
	    /* make sure we don't notify a non-existent object of any changes... */
	    xcolor_RemoveObserver(self, changed);
	    
	    if(class_IsType(changed, xgraphicinfo)) {
		/* an xgraphic is going away, free it's implicitly allocated colors. */
		if(xcolor_ReferenceCount(self)==1) {
		    struct xcolormap *xcmap = xcolor_GetColormap(self);
		    if(xcmap) xcolormap_DestroyColor(xcmap, self);
		}
		else { /* This decrements the ref count */
		    xcolor_Destroy(self);
		}
	    } else if(class_IsType(changed, xcmapinfo)) {
		/* clear the colormap field since the colormap doesn't exist anymore */
		xcolor_SetColormap(self, NULL);
	    }
	    break;
    }
}

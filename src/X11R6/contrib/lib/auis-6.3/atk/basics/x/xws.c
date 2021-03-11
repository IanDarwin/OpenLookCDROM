/*LIBS: -lcmenu -lcwm -lX11 -lerrors -lutil
*/

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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/basics/x/RCS/xws.c,v 1.19 1993/08/26 22:48:24 gk5g Exp $";
#endif

/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */

#include <andrewos.h>

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xatom.h>
#include <cmenu.h>

#include <xcursor.ih>
#include <xfontd.ih>
#include <xgraphic.ih>
#include <xim.ih>
#include <xcolor.ih>
#include <xcmap.ih>
#include <xws.eh>

	struct xcursor *
xws__CreateCursor(self)
	struct xws *self;
{
	return xcursor_New();
}

	struct xfontdesc *
xws__CreateFontdesc(self)
	struct xws *self;
{
	return xfontdesc_New();
}

	struct xgraphic *
xws__CreateGraphic(self)
	struct xws *self;
{
	return xgraphic_New();
}

	struct xim *
xws__CreateIM(self)
	struct xws *self;
{
	return xim_New();
}

	struct xoffscrwin *
xws__CreateOffScreenWindow(self,host,width,height)
	struct xws *self;
	char * host;
	long width, height;
{
/*
	return xoffscrwin_Create(host,width,height);
*/
	return NULL;
}

struct xcolor *
xws__CreateColor( self, name, r, g, b)
    struct xws *self;
    char *name;
    unsigned int r, g, b;
{
    return(xcolor_Create(name, r, g, b));
}

struct xcolormap *
xws__CreateColormap( self, xim )
    struct xws *self;
    struct xim *xim;
{
    return(xcolormap_Create(xim));
}

	boolean 
xws__HandleFiles(self, WaitTime, beCheap)
	struct xws *self;
	long WaitTime;
	boolean beCheap;
{
	return xim_HandleFiles(WaitTime, beCheap);
}

	void 
xws__FlushAllWindows(self)
	struct xws * self;
{
	xim_FlushAllWindows();
}

	boolean 
xws__InitializeClass(classID)
	struct classheader * classID;
{
	/* slimy way of getting all x related modules together */
	xim_StaticLoadOnlyThisClass();
	xfontdesc_StaticLoadOnlyThisClass();
	xgraphic_StaticLoadOnlyThisClass();
	xcursor_StaticLoadOnlyThisClass();
	xcolor_StaticLoadOnlyThisClass();
	xcolor_StaticLoadOnlyThisClass();
	xcolormap_StaticLoadOnlyThisClass();
	/**/

	return TRUE;
}

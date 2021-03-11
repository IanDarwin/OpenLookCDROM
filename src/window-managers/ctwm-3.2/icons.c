/*
 * Copyright 1989 Massachusetts Institute of Technology
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of M.I.T. not be used in advertising
 * or publicity pertaining to distribution of the software without specific,
 * written prior permission.  M.I.T. makes no representations about the
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * M.I.T. DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL M.I.T.
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION
 * OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN 
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */
/* 
 *  [ ctwm ]
 *
 *  Copyright 1992 Claude Lecommandeur.
 *            
 * Permission to use, copy, modify  and distribute this software  [ctwm] and
 * its documentation for any purpose is hereby granted without fee, provided
 * that the above  copyright notice appear  in all copies and that both that
 * copyright notice and this permission notice appear in supporting documen-
 * tation, and that the name of  Claude Lecommandeur not be used in adverti-
 * sing or  publicity  pertaining to  distribution of  the software  without
 * specific, written prior permission. Claude Lecommandeur make no represen-
 * tations  about the suitability  of this software  for any purpose.  It is
 * provided "as is" without express or implied warranty.
 *
 * Claude Lecommandeur DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
 * INCLUDING ALL  IMPLIED WARRANTIES OF  MERCHANTABILITY AND FITNESS.  IN NO
 * EVENT SHALL  Claude Lecommandeur  BE LIABLE FOR ANY SPECIAL,  INDIRECT OR
 * CONSEQUENTIAL  DAMAGES OR ANY  DAMAGES WHATSOEVER  RESULTING FROM LOSS OF
 * USE, DATA  OR PROFITS,  WHETHER IN AN ACTION  OF CONTRACT,  NEGLIGENCE OR
 * OTHER  TORTIOUS ACTION,  ARISING OUT OF OR IN  CONNECTION WITH THE USE OR
 * PERFORMANCE OF THIS SOFTWARE.
 *
 * Author:  Claude Lecommandeur [ lecom@sic.epfl.ch ][ April 1992 ]
 */

/**********************************************************************
 *
 * $XConsortium: icons.c,v 1.22 91/07/12 09:58:38 dave Exp $
 *
 * Icon releated routines
 *
 * 10-Apr-89 Tom LaStrange        Initial Version.
 *
 * Do the necessary modification to be integrated in ctwm.
 * Can no longer be used for the standard twm.
 *
 * 22-April-92 Claude Lecommandeur.
 *
 *
 **********************************************************************/

#include <stdio.h>
#include "twm.h"
#include "screen.h"
#include "icons.h"
#include "gram.h"
#include "list.h"
#include "parse.h"
#include "util.h"

extern Bool AnimationPending;
extern Bool AnimationActive;
extern Bool MaybeAnimate;

#define iconWidth(w)	(Scr->IconBorderWidth * 2 + w->icon->w_width)
#define iconHeight(w)	(Scr->IconBorderWidth * 2 + w->icon->w_height)

static
splitEntry (ie, grav1, grav2, w, h)
    IconEntry	*ie;
    int		grav1, grav2;
    int		w, h;
{
    IconEntry	*new;
    int		save;

    switch (grav1) {
    case D_NORTH:
    case D_SOUTH:
	save = ie->w;
	if (w != ie->w)
	    splitEntry (ie, grav2, grav1, w, ie->h);
	if (h != ie->h) {
	    new = (IconEntry *)malloc (sizeof (IconEntry));
	    new->twm_win = 0;
	    new->used = 0;
	    new->next = ie->next;
	    ie->next = new;
	    new->x = ie->x;
	    new->h = (ie->h - h);
	    new->w = /*save*/ ie->w;
	    ie->h = h;
	    if (grav1 == D_SOUTH) {
		new->y = ie->y;
		ie->y = new->y + new->h;
	    } else
		new->y = ie->y + ie->h;
	}
	break;
    case D_EAST:
    case D_WEST:
	save = ie->h;
	if (h != ie->h)
	    splitEntry (ie, grav2, grav1, ie->w, h);
	if (w != ie->w) {
	    new = (IconEntry *)malloc (sizeof (IconEntry));
	    new->twm_win = 0;
	    new->used = 0;
	    new->next = ie->next;
	    ie->next = new;
	    new->y = ie->y;
	    new->w = (ie->w - w);
	    new->h = /*save*/ ie->h;
	    ie->w = w;
	    if (grav1 == D_EAST) {
		new->x = ie->x;
		ie->x = new->x + new->w;
	    } else
		new->x = ie->x + ie->w;
	}
	break;
    }
}

roundUp (v, multiple)
{
    return ((v + multiple - 1) / multiple) * multiple;
}

static void PlaceIcon(tmp_win, def_x, def_y, final_x, final_y)
TwmWindow *tmp_win;
int def_x, def_y;
int *final_x, *final_y;
{
    IconRegion	*ir;
    IconEntry	*ie;
    int		w = 0, h = 0;

    /*
     * First, check to see if the window is in a region's client list
     */
    ie = 0;
    for (ir = Scr->FirstRegion; ir; ir = ir->next) {
	if (LookInList(ir->clientlist, tmp_win->full_name, &tmp_win->class))
	{
	    w = roundUp (iconWidth (tmp_win), ir->stepx);
	    h = roundUp (iconHeight (tmp_win), ir->stepy);
	    for (ie = ir->entries; ie; ie=ie->next) {
	        if (ie->used)
		    continue;
	        if (ie->w >= w && ie->h >= h)
		    break;
	    }
	    if (ie)
	        break;
	}
    }

    /*
     * If not found in any region's client list, place anywhere
     */
    if (!ie)
    {
        for (ir = Scr->FirstRegion; ir; ir = ir->next) {
	    w = roundUp (iconWidth (tmp_win), ir->stepx);
	    h = roundUp (iconHeight (tmp_win), ir->stepy);
	    for (ie = ir->entries; ie; ie=ie->next) {
	        if (ie->used)
		    continue;
	        if (ie->w >= w && ie->h >= h)
		    break;
	    }
	    if (ie)
	        break;
        }
    }
    if (ie) {
	splitEntry (ie, ir->grav1, ir->grav2, w, h);
	ie->used = 1;
	ie->twm_win = tmp_win;
	switch (Scr->IconRegionJustification) {
	    case J_LEFT :
		*final_x = ie->x;
		break;
	    case J_CENTER :
		*final_x = ie->x + (ie->w - iconWidth (tmp_win)) / 2;
		break;
	    case J_RIGHT :
		*final_x = ie->x + ie->w - iconWidth (tmp_win);
		break;
	    case J_BORDER :
		if (ir->grav2 == D_EAST)
		    *final_x = ie->x + ie->w - iconWidth (tmp_win);
		else
		    *final_x = ie->x;
		break;
	}
	*final_y = ie->y + (ie->h - iconHeight (tmp_win)) / 2;
    } else {
	*final_x = def_x;
	*final_y = def_y;
    }
    return;
}

static IconEntry *
FindIconEntry (tmp_win, irp)
    TwmWindow   *tmp_win;
    IconRegion	**irp;
{
    IconRegion	*ir;
    IconEntry	*ie;

    for (ir = Scr->FirstRegion; ir; ir = ir->next) {
	for (ie = ir->entries; ie; ie=ie->next)
	    if (ie->twm_win == tmp_win) {
		if (irp)
		    *irp = ir;
		return ie;
	    }
    }
    return 0;
}

IconUp (tmp_win)
    TwmWindow   *tmp_win;
{
    int		x, y;
    int		defx, defy;
    struct IconRegion *ir;

    /*
     * If the client specified a particular location, let's use it (this might
     * want to be an option at some point).  Otherwise, try to fit within the
     * icon region.
     */
    if (tmp_win->wmhints && (tmp_win->wmhints->flags & IconPositionHint))
      return (0);

    if (tmp_win->icon_moved) {
	if (!XGetGeometry (dpy, tmp_win->icon->w, &JunkRoot, &defx, &defy,
			   &JunkWidth, &JunkHeight, &JunkBW, &JunkDepth))
	  return (1);

	x = defx + ((int) JunkWidth) / 2;
	y = defy + ((int) JunkHeight) / 2;

	for (ir = Scr->FirstRegion; ir; ir = ir->next) {
	    if (x >= ir->x && x < (ir->x + ir->w) &&
		y >= ir->y && y < (ir->y + ir->h))
	      break;
	}
	if (!ir) return (0);		/* outside icon regions, leave alone */
    }

    defx = -100;
    defy = -100;
    PlaceIcon(tmp_win, defx, defy, &x, &y);
    if (x != defx || y != defy) {
	XMoveWindow (dpy, tmp_win->icon->w, x, y);
	tmp_win->icon_moved = FALSE;	/* since we've restored it */
    }
    MaybeAnimate = True;
    return (0);
}

static IconEntry *
prevIconEntry (ie, ir)
    IconEntry	*ie;
    IconRegion	*ir;
{
    IconEntry	*ip;

    if (ie == ir->entries)
	return 0;
    for (ip = ir->entries; ip->next != ie; ip=ip->next)
	;
    return ip;
}

/* old is being freed; and is adjacent to ie.  Merge
 * regions together
 */

static
mergeEntries (old, ie)
    IconEntry	*old, *ie;
{
    if (old->y == ie->y) {
	ie->w = old->w + ie->w;
	if (old->x < ie->x)
	    ie->x = old->x;
    } else {
	ie->h = old->h + ie->h;
	if (old->y < ie->y)
	    ie->y = old->y;
    }
}

IconDown (tmp_win)
    TwmWindow   *tmp_win;
{
    IconEntry	*ie, *ip, *in;
    IconRegion	*ir;

    ie = FindIconEntry (tmp_win, &ir);
    if (ie) {
	ie->twm_win = 0;
	ie->used = 0;
	ip = prevIconEntry (ie, ir);
	in = ie->next;
	for (;;) {
	    if (ip && ip->used == 0 &&
	       ((ip->x == ie->x && ip->w == ie->w) ||
	        (ip->y == ie->y && ip->h == ie->h)))
	    {
	    	ip->next = ie->next;
	    	mergeEntries (ie, ip);
	    	free ((char *) ie);
		ie = ip;
	    	ip = prevIconEntry (ip, ir);
	    } else if (in && in->used == 0 &&
	       ((in->x == ie->x && in->w == ie->w) ||
	        (in->y == ie->y && in->h == ie->h)))
	    {
	    	ie->next = in->next;
	    	mergeEntries (in, ie);
	    	free ((char *) in);
	    	in = ie->next;
	    } else
		break;
	}
    }
}

name_list **
AddIconRegion(geom, grav1, grav2, stepx, stepy)
char *geom;
int grav1, grav2;
{
    IconRegion *ir;
    int mask;

    ir = (IconRegion *)malloc(sizeof(IconRegion));
    ir->next = NULL;

    if (Scr->LastRegion) Scr->LastRegion->next = ir;
    Scr->LastRegion = ir;
    if (!Scr->FirstRegion) Scr->FirstRegion = ir;

    ir->entries = NULL;
    ir->clientlist = NULL;
    ir->grav1 = grav1;
    ir->grav2 = grav2;
    if (stepx <= 0)
	stepx = 1;
    if (stepy <= 0)
	stepy = 1;
    ir->stepx = stepx;
    ir->stepy = stepy;
    ir->x = ir->y = ir->w = ir->h = 0;

    mask = XParseGeometry(geom, &ir->x, &ir->y, (unsigned int *)&ir->w, (unsigned int *)&ir->h);

    if (mask & XNegative)
	ir->x += Scr->MyDisplayWidth - ir->w;

    if (mask & YNegative)
	ir->y += Scr->MyDisplayHeight - ir->h;
    ir->entries = (IconEntry *)malloc(sizeof(IconEntry));
    ir->entries->next = 0;
    ir->entries->x = ir->x;
    ir->entries->y = ir->y;
    ir->entries->w = ir->w;
    ir->entries->h = ir->h;
    ir->entries->twm_win = 0;
    ir->entries->used = 0;

    return(&(ir->clientlist));
}

#ifdef comment
FreeIconEntries (ir)
    IconRegion	*ir;
{
    IconEntry	*ie, *tmp;

    for (ie = ir->entries; ie; ie=tmp)
    {
	tmp = ie->next;
	free ((char *) ie);
    }
}
FreeIconRegions()
{
    IconRegion *ir, *tmp;

    for (ir = Scr->FirstRegion; ir != NULL;)
    {
	tmp = ir;
	FreeIconEntries (ir);
	ir = ir->next;
	free((char *) tmp);
    }
    Scr->FirstRegion = NULL;
    Scr->LastRegion = NULL;
}
#endif

CreateIconWindow(tmp_win, def_x, def_y)
TwmWindow *tmp_win;
int def_x, def_y;
{
    unsigned long event_mask;
    unsigned long valuemask;		/* mask for create windows */
    XSetWindowAttributes attributes;	/* attributes for create windows */
    int final_x, final_y;
    int x;
    Icon	*icon;
    Matchtype	match;
    Image	*image = None;

    icon = (Icon*) malloc (sizeof (struct Icon));

    icon->border	= Scr->IconBorderColor;
    icon->iconc.fore	= Scr->IconC.fore;
    icon->iconc.back	= Scr->IconC.back;

    GetColorFromList(Scr->IconBorderColorL, tmp_win->full_name, &tmp_win->class,
	&icon->border);
    GetColorFromList(Scr->IconForegroundL, tmp_win->full_name, &tmp_win->class,
	&icon->iconc.fore);
    GetColorFromList(Scr->IconBackgroundL, tmp_win->full_name, &tmp_win->class,
	&icon->iconc.back);
    if (Scr->use3Diconmanagers && !Scr->BeNiceToColormap) GetShadeColors (&icon->iconc);

    FB(icon->iconc.fore, icon->iconc.back);

    icon->match   = match_none;
    icon->pattern = NULL;
    icon->image   = None;

    tmp_win->forced = FALSE;
    tmp_win->icon_not_ours = FALSE;

    /* now go through the steps to get an icon window,  if ForceIcon is 
     * set, then no matter what else is defined, the bitmap from the
     * .twmrc file is used
     */
    if (Scr->ForceIcon) {
	char *icon_name;

	icon_name = LookInNameList (Scr->IconNames, tmp_win->icon_name);
        if (icon_name != NULL) {
	    icon->pattern = LookPatternInNameList (Scr->IconNames, tmp_win->icon_name);
	    icon->match = match_icon;
	}
        if (icon->match == match_none)
	    icon_name = LookInNameList(Scr->IconNames, tmp_win->full_name);
        if ((icon->match == match_none) && (icon_name != NULL)) {
	    icon->pattern = LookPatternInNameList (Scr->IconNames, tmp_win->full_name);
	    icon->match = match_name;
	}
        if (icon->match == match_none)
	    icon_name = LookInList(Scr->IconNames, tmp_win->full_name, &tmp_win->class);
        if ((icon->match == match_none) && (icon_name != NULL)) {
	    icon->pattern = LookPatternInList (Scr->IconNames, tmp_win->full_name, &tmp_win->class);
	    icon->match = match_class;
	}
	if ((image  = GetImage (icon_name, icon->iconc)) != None) {
	    icon->image  = image;
	    icon->width  = image->width;
	    icon->height = image->height;
	    tmp_win->forced = TRUE;
	}
    }

    /* if the pixmap is still NULL, we didn't get one from the above code,
     * that could mean that ForceIcon was not set, or that the window
     * was not in the Icons list, now check the WM hints for an icon
     */
    if (image == None && tmp_win->wmhints &&
	tmp_win->wmhints->flags & IconPixmapHint) {
    
	image = (Image*) malloc (sizeof (struct _Image));
	XGetGeometry(dpy, tmp_win->wmhints->icon_pixmap,
             &JunkRoot, &JunkX, &JunkY,
	     (unsigned int *)&image->width, (unsigned int *)&image->height, &JunkBW, &JunkDepth);

	image->pixmap = XCreatePixmap (dpy, Scr->Root, image->width, image->height, Scr->d_depth);
	image->mask   = None;
	image->next   = None;
	XCopyPlane (dpy, tmp_win->wmhints->icon_pixmap, image->pixmap, Scr->NormalGC,
			0, 0, image->width, image->height, 0, 0, 1 );

	icon->width   = image->width;
	icon->height  = image->height;
    }

    /* if we still haven't got an icon, let's look in the Icon list 
     * if ForceIcon is not set
     */
    if (image == None && !Scr->ForceIcon) {
	char *icon_name;

	icon->match   = match_none;
	icon->pattern = NULL;
	icon_name = LookInNameList(Scr->IconNames, tmp_win->icon_name);
        if (icon_name != NULL) {
	    icon->pattern = LookPatternInNameList (Scr->IconNames, tmp_win->icon_name);
	    icon->match = match_icon;
	}
        if (icon->match == match_none)
	    icon_name = LookInNameList(Scr->IconNames, tmp_win->full_name);
        if ((icon->match == match_none) && (icon_name != NULL)) {
	    icon->pattern = LookPatternInNameList (Scr->IconNames, tmp_win->full_name);
	    icon->match = match_name;
	}
        if (icon->match == match_none)
	    icon_name = LookInList(Scr->IconNames, tmp_win->full_name, &tmp_win->class);
        if ((icon->match == match_none) && (icon_name != NULL)) {
	    icon->pattern = LookPatternInList (Scr->IconNames, tmp_win->full_name, &tmp_win->class);
	    icon->match = match_class;
	}
	if ((image = GetImage (icon_name, icon->iconc)) != None) {
	    icon->image  = image;
	    icon->width  = image->width;
	    icon->height = image->height;
	    tmp_win->forced = TRUE;
	}
    }

    /* if we still don't have an icon, assign the UnknownIcon */
    if (image == None && Scr->UnknownImage != None)
    {
	image = Scr->UnknownImage;
	icon->width   = image->width;
	icon->height  = image->height;
	icon->image   = image;
    }

    if (image == None)
    {
	icon->height = 0;
	icon->width  = 0;
	valuemask    = 0;
    }
    else
    {
	valuemask = CWBackPixmap;
	attributes.background_pixmap = image->pixmap;
    }

    if (Scr->NoIconTitlebar ||
	LookInNameList (Scr->NoIconTitle, tmp_win->icon_name) ||
	LookInList (Scr->NoIconTitle, tmp_win->full_name, &tmp_win->class))
    {
	icon->w_width  = icon->width;
	icon->w_height = icon->height;
	icon->x = 0;
	icon->y = 0;
	icon->has_title = False;
    }
    else {
	icon->w_width = XTextWidth(Scr->IconFont.font,
		tmp_win->icon_name, strlen(tmp_win->icon_name));
	if (icon->w_width > Scr->MaxIconTitleWidth) icon->w_width = Scr->MaxIconTitleWidth;

	icon->w_width += 6;
	if (icon->w_width < icon->width)
	{
	    icon->x = (icon->width - icon->w_width)/2;
	    icon->x += 3;
	    icon->w_width = icon->width;
	}
	else
	{
	    icon->x = 3;
	}
	icon->y = icon->height + Scr->IconFont.height;
	icon->w_height = icon->height + Scr->IconFont.height + 6;
	icon->has_title = True;
    }

    event_mask = 0;
    if (tmp_win->wmhints && tmp_win->wmhints->flags & IconWindowHint)
    {
	icon->w = tmp_win->wmhints->icon_window;
	if (tmp_win->forced ||
	    XGetGeometry(dpy, icon->w, &JunkRoot, &JunkX, &JunkY,
		     (unsigned int *)&icon->w_width, (unsigned int *)&icon->w_height,
		     &JunkBW, &JunkDepth) == 0)
	{
	    icon->w = None;
	    tmp_win->wmhints->flags &= ~IconWindowHint;
	}
	else
	{
	    tmp_win->icon_not_ours = TRUE;
	    event_mask = EnterWindowMask | LeaveWindowMask;
	}
    }
    else
    {
	icon->w = None;
    }

    if (icon->w == None)
    {
	icon->w = XCreateSimpleWindow(dpy, Scr->Root,
	    0,0,
	    icon->w_width, icon->w_height,
	    Scr->IconBorderWidth, icon->border, icon->iconc.back);
	event_mask = ExposureMask;
    }

    XSelectInput (dpy, icon->w,
		  KeyPressMask | ButtonPressMask | ButtonReleaseMask |
		  event_mask);

    icon->bm_w = None;
    if (image != None && (! (tmp_win->wmhints && tmp_win->wmhints->flags & IconWindowHint))) {
	XRectangle rect [2];

	switch (Scr->IconJustification) {
	    case J_LEFT :
		x = 0;
		break;
	    case J_CENTER :
		x = (icon->w_width - icon->width) / 2;
		break;
	    case J_RIGHT :
		x = icon->w_width - icon->width;
		break;
	}
	icon->bm_w = XCreateWindow (dpy, icon->w, x, 0,
					    (unsigned int)icon->width,
					    (unsigned int)icon->height,
					    (unsigned int) 0, Scr->d_depth,
					    (unsigned int) CopyFromParent,
					    Scr->d_visual, valuemask,
					    &attributes);
	if ((image != None) && image->mask != None) {
	    XShapeCombineMask (dpy, icon->bm_w, ShapeBounding, 0, 0, image->mask, ShapeSet);
	    XShapeCombineMask (dpy, icon->w,    ShapeBounding, x, 0, image->mask, ShapeSet);
	    if (icon->has_title) {
		rect [0].x = 0;
		rect [0].y = icon->height;
		rect [0].width  = icon->w_width;
		rect [0].height = Scr->IconFont.height + 6;
		XShapeCombineRectangles (dpy, icon->w, ShapeBounding,  0, 0, rect, 1, ShapeUnion, 0);
	    }
	}
	else {
	    rect [0].x = x;
	    rect [0].y = 0;
	    rect [0].width  = icon->width;
	    rect [0].height = icon->height;
	    rect [1].x = 0;
	    rect [1].y = icon->height;
	    rect [1].width  = icon->w_width;
	    rect [1].height = Scr->IconFont.height + 6;
	    XShapeCombineRectangles (dpy, icon->w, ShapeBounding, 0, 0, rect, 2, ShapeSet, 0);
	}
    }

    if (icon->match != match_none) AddToList (&tmp_win->iconslist, icon->pattern, (char*) icon);

    tmp_win->icon = icon;
    /* I need to figure out where to put the icon window now, because 
     * getting here means that I am going to make the icon visible
     */
    if (tmp_win->wmhints &&
	tmp_win->wmhints->flags & IconPositionHint)
    {
	final_x = tmp_win->wmhints->icon_x;
	final_y = tmp_win->wmhints->icon_y;
    }
    else
    {
      if (OCCUPY (tmp_win, Scr->workSpaceMgr.activeWSPC))
	PlaceIcon(tmp_win, def_x, def_y, &final_x, &final_y);
    }

  if (OCCUPY (tmp_win, Scr->workSpaceMgr.activeWSPC)) {
    if (final_x > Scr->MyDisplayWidth)
	final_x = Scr->MyDisplayWidth - icon->w_width -
	    (2 * Scr->IconBorderWidth);
    if (final_x < 0) final_x = 0;

    if (final_y > Scr->MyDisplayHeight)
	final_y = Scr->MyDisplayHeight - icon->height -
	    Scr->IconFont.height - 6 - (2 * Scr->IconBorderWidth);
    if (final_y < 0) final_y = 0;

    XMoveWindow(dpy, icon->w, final_x, final_y);
  }
    tmp_win->iconified = TRUE;

    XMapSubwindows(dpy, icon->w);
    XSaveContext(dpy, icon->w, TwmContext, (caddr_t)tmp_win);
    XSaveContext(dpy, icon->w, ScreenContext, (caddr_t)Scr);
    XDefineCursor(dpy, icon->w, Scr->IconCursor);
    MaybeAnimate = True;
    return (0);
}

Bool AnimateIcons (scr, icon)
ScreenInfo *scr;
Icon	   *icon;
{
    Image	*image;
    XRectangle	rect;
    XSetWindowAttributes attr;
    int		x;
    static Window shapewin = (Window) 0;

    image = icon->image;
    attr.background_pixmap = image->pixmap;
    XChangeWindowAttributes (dpy, icon->bm_w, CWBackPixmap, &attr);

    if (image->mask != None) {
	XShapeCombineMask (dpy, icon->bm_w, ShapeBounding, 0, 0, image->mask, ShapeSet);
	switch (scr->IconJustification) {
	    case J_LEFT :
		x = 0;
		break;
	    case J_CENTER :
		x = (icon->w_width - icon->width) / 2;
		break;
	    case J_RIGHT :
		x = icon->w_width - icon->width;
		break;
	}
	if (icon->has_title) {
	    rect.x = 0;
	    rect.y = icon->height;
	    rect.width  = icon->w_width;
	    rect.height = scr->IconFont.height + 6;

	    XShapeCombineShape (dpy, scr->ShapeWindow, ShapeBounding, x, 0, icon->bm_w,
				ShapeBounding, ShapeSet);
	    XShapeCombineRectangles (dpy, scr->ShapeWindow, ShapeBounding, 0, 0, &rect, 1,
				ShapeUnion, 0);
	    XShapeCombineShape (dpy, icon->w, ShapeBounding, 0, 0, scr->ShapeWindow,
				ShapeBounding, ShapeSet);
	}
	else
	    XShapeCombineShape (dpy, icon->w, ShapeBounding, x, 0, icon->bm_w,
				ShapeBounding, ShapeSet);
    }
    XClearWindow (dpy, icon->bm_w);
    icon->image  = image->next;
    return (True);
}


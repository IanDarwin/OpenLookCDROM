/* panner.c - panner box view */

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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/supportviews/RCS/panner.c,v 1.6 1993/12/09 00:15:03 gk5g Exp $";
#endif

/*
	Copyright Carnegie Mellon University 1992 - All rights reserved
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

#include <andrewos.h>
#include <view.ih>
#include <cursor.ih>
#include <im.ih>
#include <updlist.ih>
#include <region.ih>
#include <environ.ih>
#include <sbuttonv.ih>
#include <rect.h>
#include <panner.eh>

#define sself ((struct scroll *)self)

#define EQRECT(a, b)  ((a)->left == (b)->left && (a)->top == (b)->top && (a)->width == (b)->width && (a)->height == (b)->height)

boolean panner__InitializeClass(classID)
struct classheader *classID;
{
    return TRUE;
}

static int cursortypes[panner_NumberOfCursors] = {Cursor_SmallCross, Cursor_Octagon, Cursor_Gunsight, Cursor_CrossHairs};

boolean panner__InitializeObject(classID, self)
struct classheader *classID;
struct panner *self;
{
    int ix;

    self->MotifStyle = environ_GetProfileSwitch("MotifPanners", FALSE);
    self->PannerShade = environ_GetProfileInt("PannerShade", 8);

    self->visible = FALSE;
    rectangle_EmptyRect(&self->panrect);
    self->oldvisible = FALSE;
    rectangle_EmptyRect(&self->oldpanrect);

    self->idealwidth = 100;
    self->idealheight = 100;

    self->childclip = region_New();
    self->pannerreg = region_New();

    for (ix=0; ix<panner_NumberOfCursors; ix++) {
	self->cursors[ix] = cursor_Create((struct view *)self);
	if (self->cursors[ix]) 
	    cursor_SetStandard(self->cursors[ix], cursortypes[ix]);
    }

    return TRUE;
}

void panner__FinalizeObject(classID, self)
struct classheader *classID;
struct panner *self;
{
    int ix;

    region_Destroy(self->childclip);
    region_Destroy(self->pannerreg);

    for(ix=0; ix<scroll_TYPES; ix++) 
	if(self->cursors[ix]) 
	    cursor_Destroy(self->cursors[ix]);
}

struct panner *panner__Create(classID, scrollee)
struct classheader *classID;
struct view *scrollee;
{
    struct panner *retval = panner_New();

    panner_SetView(retval, scrollee);
    panner_SetLocation(retval, scroll_TOP|scroll_LEFT);

    return retval;
}


void panner__SetLocation(self, location)
struct panner *self;
int location;
{
    super_SetLocation(self, scroll_TOP|scroll_LEFT);
}

void panner__PostCursor(self, rec, cursor)
struct panner *self;
struct rectangle *rec;
struct cursor *cursor;
{
    super_PostCursor(self, rec, cursor);
    if (cursor != self->cursors[0] && cursor != self->cursors[3]) {
	panner_RetractCursor(self, self->cursors[0]);
	panner_RetractCursor(self, self->cursors[3]);
	panner_PostCursor(self, &(self->panrect), self->cursors[0]);
	panner_PostCursor(self, &(self->gseenrect), self->cursors[3]);
    }
}

static void recompute_panrect(self)
struct panner *self;
{
    struct rectangle *r = &(((struct scroll *)self)->childrect);

    if (rectangle_IsEmptyRect(&self->panrect)) {

	if (r->width < self->idealwidth+10 || r->height < self->idealheight+10) {
	    self->visible = FALSE;
	    rectangle_EmptyRect(&self->panrect);
	    return;
	}

	self->panrect.width = self->idealwidth;
	self->panrect.height = self->idealheight;

	self->panrect.left = r->left + (r->width - self->panrect.width);
	if (self->panrect.left > 50) self->panrect.left = 50;

	self->panrect.top = r->top + (r->height - self->panrect.height);
	if (self->panrect.top > 50) self->panrect.top = 50;

	self->visible = TRUE;
	return;
    }

    self->panrect.left = MAX(self->panrect.left, 0);
    self->panrect.left = MIN(self->panrect.left, r->width - self->panrect.width);
    if (self->panrect.left < 0) {
	self->visible = FALSE;
	return;
    }

    self->panrect.top = MAX(self->panrect.top, 0);
    self->panrect.top = MIN(self->panrect.top, r->height - self->panrect.height);

    if (self->panrect.top < 0) {
	self->visible = FALSE;
	return;
    }

    self->visible = TRUE;
}

static char *InterfaceName[scroll_TYPES] = {"scroll,vertical", "scroll,horizontal"};

/* right now, only called from getinfo(); may put in-line */
static void get_interface(self, type)
struct panner *self;
int type; /* scroll_VERT or scroll_HORIZ */
{
    sself->force_get_interface = FALSE;
    if (sself->fns[type] == NULL)
        sself->fns[type] = (struct scrollfns *)view_GetInterface(sself->scrollee, InterfaceName[type]);
}

static void getinfo(self, type, total, seen, dot)
struct panner *self;
int type; /* scroll_VERT or scroll_HORIZ */
struct range *total, *seen, *dot;
{
    void (*real_getinfo)();

    get_interface(self, type);

    if (sself->fns[type] != NULL && (real_getinfo = sself->fns[type]->GetInfo) != NULL)
        real_getinfo(sself->scrollee, total, seen, dot);

    if (total->beg == total->end) {
        total->end++;
        *seen = *total;
        dot->beg = dot->end = total->beg;
    }
}

static void calc_desired(self)
struct panner *self;
{
    int i;

    for (i = 0; i < scroll_TYPES; i++) {
	struct scrollbar *bar = &(sself->desired.bar[i]);
	getinfo(self, i, &bar->total, &bar->seen, &bar->dot);
    }
}

static void update_everything(self, wipeold)
struct panner *self;
boolean wipeold;
{
    struct sbuttonv_view_info vi;
    long maxwid, maxhgt;
    struct scrollbar *tmp;
    static struct rectangle oldseenrect, olddotrect;

    sbuttonv_SaveViewState(self, &vi);

    oldseenrect = self->seenrect;
    olddotrect = self->dotrect;

    maxwid = self->panrect.width-5;
    maxhgt = self->panrect.height-5;

    if (sself->fns[scroll_HORIZ]) {
	tmp = &(sself->desired.bar[scroll_HORIZ]);
	self->seenrect.left =  (tmp->seen.beg - tmp->total.beg) * maxwid / (tmp->total.end - tmp->total.beg);
	self->seenrect.width =  (tmp->seen.end - tmp->seen.beg) * maxwid / (tmp->total.end - tmp->total.beg);

	self->dotrect.left =  (tmp->dot.beg - tmp->total.beg) * maxwid / (tmp->total.end - tmp->total.beg);
	self->dotrect.width =  (tmp->dot.end - tmp->dot.beg) * maxwid / (tmp->total.end - tmp->total.beg);

	if (self->seenrect.width < 2) {
	    self->seenrect.width = 2;
	    self->seenrect.left -= 1;
	}
	if (self->dotrect.width < 2) {
	    self->dotrect.width = 2;
	    self->dotrect.left -= 1;
	}
    }
    else {
	self->seenrect.left = 0;
	self->seenrect.width = maxwid;
	self->dotrect.left = 2;
	self->dotrect.width = maxwid-3;
    }

    if (sself->fns[scroll_VERT]) {
	tmp = &(sself->desired.bar[scroll_VERT]);
	self->seenrect.top =  (tmp->seen.beg - tmp->total.beg) * maxwid / (tmp->total.end - tmp->total.beg);
	self->seenrect.height =  (tmp->seen.end - tmp->seen.beg) * maxwid / (tmp->total.end - tmp->total.beg);

	self->dotrect.top =  (tmp->dot.beg - tmp->total.beg) * maxwid / (tmp->total.end - tmp->total.beg);
	self->dotrect.height =  (tmp->dot.end - tmp->dot.beg) * maxwid / (tmp->total.end - tmp->total.beg);

	if (self->seenrect.height < 2) {
	    self->seenrect.height = 2;
	    self->seenrect.top -= 1;
	}
	if (self->dotrect.height < 2) {
	    self->dotrect.height = 2;
	    self->dotrect.top -= 1;
	}
    }
    else {
	self->seenrect.top = 0;
	self->seenrect.height = maxhgt;
	self->dotrect.top = 2;
	self->dotrect.height = maxhgt-3;
    }

    self->gseenrect.width = self->seenrect.width;
    self->gseenrect.height = self->seenrect.height;
    self->gseenrect.left = self->panrect.left+2+self->seenrect.left;
    self->gseenrect.top = self->panrect.top+2+self->seenrect.top;

    if (!self->MotifStyle) {
	panner_SetTransferMode(self, graphic_INVERT);

	if (wipeold) {
	    if (!EQRECT(&self->seenrect, &oldseenrect)) {
		panner_DrawRectSize(self, self->panrect.left+2+oldseenrect.left, self->panrect.top+2+oldseenrect.top, oldseenrect.width, oldseenrect.height);
		panner_DrawRectSize(self, self->gseenrect.left, self->gseenrect.top, self->gseenrect.width, self->gseenrect.height);
	    }
	    if (!EQRECT(&self->dotrect, &olddotrect)) {
		panner_FillRectSize(self, self->panrect.left+2+olddotrect.left, self->panrect.top+2+olddotrect.top, olddotrect.width, olddotrect.height, panner_BlackPattern(self));
		panner_FillRectSize(self, self->panrect.left+2+self->dotrect.left, self->panrect.top+2+self->dotrect.top, self->dotrect.width, self->dotrect.height, panner_BlackPattern(self));
	    }
	}
	else {
	    /* just draw them */
	    panner_DrawRectSize(self, self->gseenrect.left, self->gseenrect.top, self->gseenrect.width, self->gseenrect.height);

	    panner_FillRectSize(self, self->panrect.left+2+self->dotrect.left, self->panrect.top+2+self->dotrect.top, self->dotrect.width, self->dotrect.height, panner_BlackPattern(self));
	}
    }
    else { /* Motif Style */
	panner_SetTransferMode(self, graphic_COPY);

	if (wipeold) {
	    panner_FillRectSize(self, self->panrect.left+1, self->panrect.top+1, self->panrect.width-2, self->panrect.height-2, panner_GrayPattern(self, self->PannerShade, 16));
	}
	panner_FillRectSize(self, self->panrect.left+2+self->dotrect.left, self->panrect.top+2+self->dotrect.top, self->dotrect.width, 1, panner_BlackPattern(self));
	panner_FillRectSize(self, self->panrect.left+2+self->dotrect.left, self->panrect.top+2+self->dotrect.top, 1, self->dotrect.height, panner_BlackPattern(self));
	panner_FillRectSize(self, self->panrect.left+2+self->dotrect.left, self->panrect.top+2+self->dotrect.top+self->dotrect.height, self->dotrect.width+1, 1, panner_WhitePattern(self));
	panner_FillRectSize(self, self->panrect.left+2+self->dotrect.left+self->dotrect.width, self->panrect.top+2+self->dotrect.top, 1, self->dotrect.height+1, panner_WhitePattern(self));

	panner_FillRectSize(self, self->gseenrect.left, self->gseenrect.top, self->gseenrect.width, 1, panner_WhitePattern(self));
	panner_FillRectSize(self, self->gseenrect.left, self->gseenrect.top, 1, self->gseenrect.height, panner_WhitePattern(self));
	panner_FillRectSize(self, self->gseenrect.left, self->gseenrect.top+self->gseenrect.height, self->gseenrect.width+1, 1, panner_BlackPattern(self));
	panner_FillRectSize(self, self->gseenrect.left+self->gseenrect.width, self->gseenrect.top, 1, self->gseenrect.height+1, panner_BlackPattern(self));
    }

    sbuttonv_RestoreViewState(self, &vi);
}

/* draw everything, assuming (or forcing) a blank background. sself->desired should already have been set, with calc_desired(). */
static void draw_everything(self, wipeback)
struct panner *self;
boolean wipeback;
{
    struct sbuttonv_view_info vi;

    if (self->visible) {

	sbuttonv_SaveViewState(self, &vi);

	if (!self->MotifStyle) {
	    /*if (wipeback) */ /* doesn't seem to do the right thing, so we'll always erase the background. */
	    panner_EraseRect(self, &self->panrect);
	    panner_DrawRectSize(self, self->panrect.left, self->panrect.top, self->panrect.width-1, self->panrect.height-1);
	}
	else {
	    panner_FillRect(self, &self->panrect, panner_GrayPattern(self, self->PannerShade, 16));
	    panner_FillRectSize(self, self->panrect.left, self->panrect.top, self->panrect.width-1, 1, panner_WhitePattern(self));
	    panner_FillRectSize(self, self->panrect.left, self->panrect.top, 1, self->panrect.height-1, panner_WhitePattern(self));
	    panner_FillRectSize(self, self->panrect.left, self->panrect.top+self->panrect.height-1, self->panrect.width, 1, panner_BlackPattern(self));
	    panner_FillRectSize(self, self->panrect.left+self->panrect.width-1, self->panrect.top, 1, self->panrect.height, panner_BlackPattern(self));
	}

	sbuttonv_RestoreViewState(self, &vi);

	update_everything(self, FALSE);
    }
}

/* assumes panrect, visible have already been set */
static void move_panner(self)
struct panner *self;
{
    region_RectRegion(self->childclip, &sself->childrect); 
    if (self->visible) {
	region_RectRegion(self->pannerreg, &self->panrect); 
	region_SubtractRegion(self->childclip, self->pannerreg, self->childclip);
    }
    else
	region_ClearRegion(self->pannerreg);
    view_InsertViewRegion(sself->child, self, self->childclip);

    /* ask for update in old region */
    if (self->oldvisible) {
	panner_EraseRect(self, &self->oldpanrect);
	view_FullUpdate(sself->child, view_LastPartialRedraw, self->oldpanrect.left, self->oldpanrect.top, self->oldpanrect.width, self->oldpanrect.height);
    }
    self->oldpanrect = self->panrect;
    self->oldvisible = self->visible;

    self->panrectchanged = FALSE;
    draw_everything(self, TRUE);
    panner_PostCursor(self, &(self->panrect), self->cursors[0]);
    panner_PostCursor(self, &(self->gseenrect), self->cursors[3]);
}

void panner__FullUpdate(self, type, left, top, width, height)
struct panner *self;
enum view_UpdateType type;
long left, top, width, height;
{
    struct rectangle r;
    int i;

    /*if(panner_GetIM(self) && !self->prefsready) {
	InitPrefs(self);
    }*/
    
    panner_GetLogicalBounds(self, &r);
    
    sself->pending_update = 0;
    
    if (sself->force_get_interface) {
	for (i = 0; i < scroll_TYPES; i++) {
            sself->fns[i] = NULL;
        }
    }

    if (type!=view_Remove 
	 && (sself->lastwidth!=r.width ||
	     sself->lastheight!=r.height)
	 && sself->child) {
	sself->lastwidth=r.width;
	sself->lastheight=r.height;
	sself->childrect=r;
	recompute_panrect(self);
	/* view_InsertView(sself->child, self, &sself->childrect); */
	move_panner(self);
    }

    sself->force_full_update = FALSE;
    switch(type) {
	case view_Remove:
	    panner_RetractCursor(self, self->cursors[0]);
	    if(sself->child) view_FullUpdate(sself->child, type, left, top, width, height);
	    return;
	case view_MoveNoRedraw:
	    if(sself->child) view_FullUpdate(sself->child, view_MoveNoRedraw, left, top, width, height);
	    break;
	case view_PartialRedraw:
	    if(sself->child) view_FullUpdate(sself->child, type, view_EnclosedXToLocalX(sself->child, left),  view_EnclosedYToLocalY(sself->child,top), width, height);
	    break;
	case view_LastPartialRedraw:
	    if(sself->child) view_FullUpdate(sself->child, type, view_EnclosedXToLocalX(sself->child, left),  view_EnclosedYToLocalY(sself->child,top), width, height);
	    calc_desired(self);
	    draw_everything(self, FALSE);
	    break;
	case view_FullRedraw:
	    if(sself->child) view_FullUpdate(sself->child, view_FullRedraw, r.left, r.top, r.width, r.height);
	    calc_desired(self);
	    draw_everything(self, FALSE);
	    break;
	default: 
	    break;
    }

    if(type!=view_Remove && type!=view_PartialRedraw) {
	panner_RetractCursor(self, self->cursors[0]);
	panner_RetractCursor(self, self->cursors[3]);
	panner_PostCursor(self, &(self->panrect), self->cursors[0]);
	panner_PostCursor(self, &(self->gseenrect), self->cursors[3]);
    }

    sself->current = sself->desired;
    panner_FlushGraphics(self);
}

void panner__Update(self)
struct panner *self;
{
    int i;
    struct rectangle r;
    panner_GetLogicalBounds(self, &r);

    if (r.width <= 0 || r.height <= 0) return;

    sself->pending_update = 0;
    /* Let the children modify their state however they want. */
    updatelist_Clear(sself->updatelist);
    calc_desired(self);

    if (sself->current.location != sself->desired.location || sself->force_full_update) {
	panner_EraseVisualRect(self);
	panner_FullUpdate(self, view_FullRedraw, r.left, r.top, r.width, r.height);
    } 
    else {
	if (self->panrectchanged) 
	    move_panner(self);
	else {
	    int type;
	    boolean refiddle = FALSE;

	    for (type=0; type<scroll_TYPES; type++) {
		struct scrollbar *des = &sself->desired.bar[type], *cur = &sself->current.bar[type];
		if (des->total.beg != cur->total.beg || des->total.end != cur->total.end || des->seen.beg != cur->seen.beg || des->dot.beg != cur->dot.beg || des->seen.end != cur->seen.end || des->dot.end != cur->dot.end) {
		    refiddle = TRUE;
		}
	    }
	    if (refiddle)
		update_everything(self, TRUE);
	}
	sself->current = sself->desired;
    }
}

static void HandleDragging(self, action, x, y, num_clicks)
struct panner *self;
enum view_MouseAction action;
long x, y, num_clicks;
{
    if (action==view_RightUp || action==view_LeftUp) {
	self->panrect.left = x - self->rockx;
	self->panrect.top = y - self->rocky;
	self->panrect.width = self->idealwidth;
	self->panrect.height = self->idealheight;
	if (self->panrect.left < 0) self->panrect.left = 0;
	if (self->panrect.top < 0) self->panrect.top = 0;
	if (self->panrect.left >= sself->childrect.width - self->panrect.width)
	    self->panrect.left = sself->childrect.width - self->panrect.width;
	if (self->panrect.top >= sself->childrect.height - self->panrect.height)
	    self->panrect.top = sself->childrect.height - self->panrect.height;
	self->panrectchanged = TRUE;
	self->visible = TRUE;
	panner_WantUpdate(self, self);
    }
}

static void HandleThumbing(self, action, x, y, num_clicks)
struct panner *self;
enum view_MouseAction action;
long x, y, num_clicks;
{
    void (*SetFrame)();
    struct scrollbar *tmp;
    long valx, valy;

    x -= self->rockx;
    y -= self->rocky;

    /* turn panner coords to view coords */

    if (sself->fns[scroll_HORIZ]) {
	tmp = &(sself->desired.bar[scroll_HORIZ]);
	valx = x * (tmp->total.end - tmp->total.beg) / (self->panrect.width-5);
	valx = MIN(valx, tmp->total.end);
	valx = MAX(valx, tmp->total.beg);
	SetFrame = sself->fns[scroll_HORIZ]->SetFrame;
	if (SetFrame) {
	    SetFrame(sself->scrollee, valx, self->isatx, sself->childrect.width);
	}
    }

    if (sself->fns[scroll_VERT]) {
	tmp = &(sself->desired.bar[scroll_VERT]);
	valy = y * (tmp->total.end - tmp->total.beg) / (self->panrect.height-5);
	valy = MIN(valy, tmp->total.end);
	valy = MAX(valy, tmp->total.beg);
	SetFrame = sself->fns[scroll_VERT]->SetFrame;
	if (SetFrame) {
	    SetFrame(sself->scrollee, valy, self->isaty, sself->childrect.height);
	}
    }
}

static void HandleDownHit(self, action, x, y, num_clicks)
struct panner *self;
enum view_MouseAction action;
long x, y, num_clicks;
{
    long (*WhatIsAt)();
    long valx, valy;

    if (action==view_RightDown) {
	self->rockx = x - self->panrect.left;
	self->rocky = y - self->panrect.top;
	self->visible = FALSE;
	self->panrectchanged = TRUE;
	sself->mousestate=scroll_DRAGGING;
	panner_RetractCursor(self, self->cursors[0]);
	im_SetWindowCursor(panner_GetIM(self), self->cursors[2]);
	panner_WantUpdate(self, self);
	return;
    }
    
    if (action==view_LeftDown) {
	x -= self->panrect.left+2+self->seenrect.left;
	y -= self->panrect.top+2+self->seenrect.top;
	/* x and y are now coordinates in the seen rectangle */

	if (x>=0 && y>=0 && x<self->seenrect.width && y<self->seenrect.height) {
	    /* turn panner coords to pixel coords */
	    self->isatx = x * sself->childrect.width / (self->seenrect.width);
	    self->isaty = y * sself->childrect.height / (self->seenrect.height);
	    self->rockx = self->panrect.left+2;
	    self->rocky = self->panrect.top+2;

	    sself->mousestate=scroll_THUMBING;
	    im_SetWindowCursor(panner_GetIM(self), self->cursors[1]);
	}
	else
	    sself->mousestate=scroll_NOTHING;
	return;
    }

    sself->mousestate=scroll_NOTHING;
}

struct view *panner__Hit(self, action, x, y, num_clicks)
struct panner *self;
enum view_MouseAction action;
long x, y, num_clicks;
{
    switch (sself->mousestate) {
	case scroll_DRAGGING:
	    HandleDragging(self, action, x, y, num_clicks);
	    break;
	case scroll_THUMBING:
	    HandleThumbing(self, action, x, y, num_clicks);
	    break;
	case scroll_NOTHING:
	    if (action==view_LeftUp || action==view_RightUp || action==view_LeftMovement || action==view_RightMovement) return (struct view *)self;
	    if (x >= self->panrect.left 
		&& x < self->panrect.left+self->panrect.width
		&& y >= self->panrect.top 
		&& y < self->panrect.top+self->panrect.height) {
		HandleDownHit(self, action, x, y, num_clicks);
		return (struct view *)self;
	    }
	    else if (sself->child) 
		return view_Hit(sself->child, action, view_EnclosedXToLocalX(sself->child, x), view_EnclosedYToLocalY(sself->child, y), num_clicks);
	    break;
	default: 
	    break;
    }

    if (action==view_RightUp || action==view_LeftUp) {
	switch (sself->mousestate) {
	    case scroll_THUMBING:
		im_SetWindowCursor(panner_GetIM(self), NULL);
		panner_RetractCursor(self, self->cursors[3]);
		panner_PostCursor(self, &(self->gseenrect), self->cursors[3]);
		break;
	    case scroll_DRAGGING:
		im_SetWindowCursor(panner_GetIM(self), NULL);
		break;
	    default:
		break;
	}
	sself->mousestate=scroll_NOTHING;
    }

    return (struct view *)self;
}

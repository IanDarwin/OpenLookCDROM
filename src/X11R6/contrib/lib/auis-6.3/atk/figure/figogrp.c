/* figogrp.c - fig element object: group */
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
#ifndef NORCSID
char *figogrp_c_rcsid = "$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/figure/RCS/figogrp.c,v 1.2 1992/12/14 20:45:16 rr2b R6tape $";
#endif

#include <figogrp.eh>

#include <figv.ih>
#include <figure.ih>
#include <figobj.ih>
#include <figattr.ih>

#include <rect.h>

#include <message.ih>
#include <proctbl.ih>
#include <bind.ih>

boolean figogrp__InitializeObject(ClassID, self)
struct classheader *ClassID;
struct figogrp *self;
{
    self->root = figure_NULLREF;
    self->bboxdirty = TRUE;
    figogrp_IsGroup(self) = TRUE;

    figogrp_SetNumHandles(self, 8);

    self->doconstraints=FALSE;

    return TRUE;
}

void figogrp__FinalizeObject(classID, self)
struct clasheader *classID;
struct figogrp *self;
{
    return;
}
/*  boolean callfun(struct figobj *o, long ref, struct figure *self, rock) */

boolean figogrp__InitializeClass(classID)
struct classheader *classID;
{
    return TRUE;
}

char *figogrp__ToolName(dummy, v, rock)
struct figogrp *dummy;
struct figtoolview *v;
long rock;
{
    return "<group>";
}

enum figobj_Status figogrp__Build(self, v, action, x, y, clicks)   
struct figogrp *self;
struct figview *v;
enum view_MouseAction action;
long x, y; 
long clicks;
{
    return figobj_Failed;
}

void figogrp__Draw(self, v) 
struct figogrp *self;
struct figview *v;
{
}

void figogrp__Sketch(self, v) 
struct figogrp *self;
struct figview *v;
{
    long x, y, w, h;
    struct rectangle *rec = &self->handlebox;

    x = figview_ToPixX(v, rec->left);
    y = figview_ToPixY(v, rec->top);
    w = figview_ToPixW(v, rec->width);
    h = figview_ToPixH(v, rec->height);
    if (w<0) {
	w = (-w);
	x -= w;
    }
    if (h<0) {
	h = (-h);
	y -= h;
    }
    figview_SetTransferMode(v, graphic_INVERT);
    figview_DrawRectSize(v, x, y, w, h);
}

void figogrp__Select(self, v)
struct figogrp *self;
struct figview *v;
{
    long ix;
    long x, y;
    /*struct figure *fig = figogrp_GetAncestorFig(self);*/

    figogrp_GetBounds(self, v); /* make sure handles are up-to-date */

    if (figogrp_GetHandles(self) && figogrp_GetNumHandles(self)) {
	figview_SetTransferMode(v, graphic_INVERT);

	for (ix=0; ix<figogrp_GetNumHandles(self); ix++) {
	    x = figview_ToPixX(v, point_X(&(figogrp_GetHandles(self)[ix])));
	    y = figview_ToPixY(v, point_Y(&(figogrp_GetHandles(self)[ix])));
	    figview_DrawRectSize(v, x-figview_SpotRad, y-figview_SpotRad, 2*figview_SpotRad, 2*figview_SpotRad);
	}
    }

    /*if (self->doconstraints)
	for (ix=self->root; ix!=figure_NULLREF; ix=fig->objs[ix].next) 
	    if (v->objs[ix].selected) {
		struct figobj *o = figure_FindObjectByRef(fig, ix);
		figobj_DrawAttachments(o, v);
	    }*/
}

/* basic procedure to move a handle -- used by figogrp__MoveHandle(), figogrp__Reshape() */
/* use when self->doconstraints is FALSE */
static void MoveHandleNocon(self, x, y, ptref)
struct figogrp *self;
long x, y, ptref;
{
    long dx, dy;

    figogrp_GetBounds(self, NULL); /* make sure handles are right */
    dx = x - figogrp_GetHandleX(self, ptref);
    dy = y - figogrp_GetHandleY(self, ptref);

    figogrp_Reposition(self, dx, dy);
}

/* basic procedure to move a handle -- used by figogrp__MoveHandle(), figogrp__Reshape() */
/* use when self->doconstraints is TRUE */
static void MoveHandleCon(self, x, y, ptref)
struct figogrp *self;
long x, y, ptref;
{
    long ix;
    struct rectangle *R = &(self)->handlebox;

    switch (ptref) {
	case 0:
	    R->width = x - R->left;
	    break;
	case 1:
	    R->width = x - R->left;
	    ix = R->top;
	    R->top = y;
	    R->height += (ix-y);
	    break;
	case 2:
	    ix = R->top;
	    R->top = y;
	    R->height += (ix-y);
	    break;
	case 3:
	    ix = R->top;
	    R->top = y;
	    R->height += (ix-y);
	    ix = R->left;
	    R->left = x;
	    R->width += (ix-x);
	    break;
	case 4:
	    ix = R->left;
	    R->left = x;
	    R->width += (ix-x);
	    break;
	case 5:
	    R->height = y - R->top;
	    ix = R->left;
	    R->left = x;
	    R->width += (ix-x);
	    break;
	case 6:
	    R->height = y - R->top;
	    break;
	case 7:
	    R->width = x - R->left;
	    R->height = y - R->top;
	    break;
    }
}

boolean figogrp__Reshape(self, action, v, x, y, handle, ptref)
struct figogrp *self;
enum view_MouseAction action;
struct figview *v;
boolean handle;
long x, y, ptref;
{
    if (!handle)
	return;

    if (!self->doconstraints) {
	switch (action) {
	    case view_LeftDown:
	    case view_RightDown:
		if (figogrp_GetReadOnly(self))
		    return FALSE;
		figogrp_Sketch(self, v);
		break;
	    case view_LeftMovement:
	    case view_RightMovement:
		figogrp_Sketch(self, v);
		MoveHandleNocon(self, x, y, ptref);
		figogrp_Sketch(self, v);
		break;
	    case view_LeftUp:
	    case view_RightUp:
		figogrp_Sketch(self, v);
		MoveHandleNocon(self, x, y, ptref);
		figogrp_RecomputeBounds(self);
		figogrp_SetModified(self);
		break;
	}
	return TRUE;
    }
    else {
	switch (action) {
	    case view_LeftDown:
	    case view_RightDown:
		if (figogrp_GetReadOnly(self))
		    return FALSE;
		figogrp_Sketch(self, v);
		break;
	    case view_LeftMovement:
	    case view_RightMovement:
		figogrp_Sketch(self, v);
		MoveHandleCon(self, x, y, ptref);
		figogrp_Sketch(self, v);
		break;
	    case view_LeftUp:
	    case view_RightUp:
		figogrp_Sketch(self, v);
		MoveHandleCon(self, x, y, ptref);
		figogrp_Reconfigure(self);
		figogrp_RecomputeBounds(self);
		figogrp_SetModified(self);
		break;
	}
	return TRUE;
    }
}

void figogrp__MoveHandle(self, x, y, ptref)
struct figogrp *self;
long x, y, ptref;
{
    if (figogrp_GetReadOnly(self))
	return;

    if (self->doconstraints) {
	MoveHandleCon(self, x, y, ptref);
	figogrp_Reconfigure(self);
    }
    else 
	MoveHandleNocon(self, x, y, ptref);
    figogrp_RecomputeBounds(self);
    figogrp_SetModified(self);
}

void figogrp__Reposition(self, xd, yd)
struct figogrp *self;
long xd, yd;
{
    if (!self->doconstraints) {
	int ix;
	struct figure *fig = figogrp_GetAncestorFig(self);
	if (!fig) return;

	for (ix=self->root; ix!=figure_NULLREF; ix=fig->objs[ix].next) {
	    figobj_Reposition(fig->objs[ix].o, xd, yd);
	}
    } 
    else {
	struct rectangle *R = (&(self)->handlebox);
	R->left+=xd;
	R->top+=yd;
	figogrp_Reconfigure(self);
	figogrp_RecomputeBounds(self);
	figogrp_SetModified(self);
    }
}

static boolean ReconfigureChild(o, ref, v, self)
struct figobj *o;
long ref;
struct figure *v;
struct figogrp *self;
{
    struct rectangle *R=(&(self)->handlebox);
    int i=0;
    for (i=0; i<figobj_GetNumHandles(o); i++) {
	if (figobj_IsAttachmentActive(o, i)) {
	    long posx = figobj_GetAttachmentPosX(o, i);
	    long posy = figobj_GetAttachmentPosY(o, i);
	    long offx = figobj_GetAttachmentOffX(o, i);
	    long offy = figobj_GetAttachmentOffY(o, i);

	    double resx = ((double)posx*R->width) / (double)figogrp_Range;
	    double resy = ((double)posy*R->height) / (double)figogrp_Range;

	    figobj_MoveHandle(o, R->left+(long)resx+offx, R->top+(long)resy+offy, i);
	    figobj_SetModified(o);
	}
    }
    return FALSE;
}

				 
void figogrp__Reconfigure(self)
struct figogrp *self;
{
    struct figure *fig = figogrp_GetAncestorFig(self);
    if (!self->doconstraints || !fig) return;
    figure_EnumerateObjectGroup(fig, figure_FindRefByObject(fig, self), NULL, TRUE, ReconfigureChild, self);
}


void figogrp__RecomputeBounds(self)
struct figogrp *self;
{
    self->bboxdirty = TRUE;
}

void figogrp__SetConstraintsActive(self, newval)
struct figogrp *self;
boolean newval;
{
    struct figure *fig = figogrp_GetAncestorFig(self);
    if (!fig) return;

    if (newval && !self->doconstraints) {
	/* turn on */
	figogrp_GetBounds(self, NULL); /* make sure handlebox is set to the bounding box */
	self->doconstraints = TRUE;
	figogrp_RecomputeBounds(self);
	figogrp_SetModified(self);
	figure_SetModified(fig);
	figure_NotifyObservers(fig, figure_DATACHANGED);
    }
    else if (!newval && self->doconstraints) {
	/* turn off */
	self->doconstraints = FALSE;
	figogrp_SetModified(self);
	figogrp_RecomputeBounds(self);
	/* figogrp_GetBounds(self, NULL); */ /* ### probably not necessary */
	figure_SetModified(fig);
	figure_NotifyObservers(fig, figure_DATACHANGED);
    }
}

struct rectangle *figogrp__GetBounds(self, vv)
struct figogrp *self;
struct figview *vv;
{
    struct rectangle *R = &((struct figobj *)self)->bounds;
    struct rectangle *HR = &self->handlebox;
    struct figure *fig;
    int ix;

    if (!self->bboxdirty)
	return R; /* cached bbox */

    self->bboxdirty = FALSE;

    rectangle_EmptyRect(R);
    fig = figogrp_GetAncestorFig(self);
    if (!fig) return NULL; /* a group has no contents unless it's within a fig */

    for (ix=self->root; ix!=figure_NULLREF; ix=fig->objs[ix].next) {
	rectangle_UnionRect(R, R, figobj_GetBounds(fig->objs[ix].o, vv));
    }

    if (!self->doconstraints) {
	*HR = *R;
    }
    else {
	struct rectangle tmp;
	tmp = *HR;
	if (tmp.width<0) {
	    tmp.width = (-tmp.width);
	    tmp.left -= tmp.width;
	}
	if (tmp.height<0) {
	    tmp.height = (-tmp.height);
	    tmp.top -= tmp.height;
	}

	rectangle_UnionRect(R, R, &tmp);
    };
    /*if (vv) printf("doconstr = %s; handlebox = (%d..%d) (%d..%d)\n", (self->doconstraints ? "true" : "false"), figview_ToPixX(vv, HR->left), figview_ToPixX(vv, HR->left+HR->width), figview_ToPixX(vv, HR->top), figview_ToPixX(vv, HR->top+HR->height));*/
    figogrp_SetHandle(self, 0, HR->left+HR->width, HR->top+HR->height/2);
    figogrp_SetHandle(self, 1, HR->left+HR->width, HR->top);
    figogrp_SetHandle(self, 2, HR->left+HR->width/2, HR->top);
    figogrp_SetHandle(self, 3, HR->left, HR->top);
    figogrp_SetHandle(self, 4, HR->left, HR->top+HR->height/2);
    figogrp_SetHandle(self, 5, HR->left, HR->top+HR->height);
    figogrp_SetHandle(self, 6, HR->left+HR->width/2, HR->top+HR->height);
    figogrp_SetHandle(self, 7, HR->left+HR->width, HR->top+HR->height);

    figogrp_ComputeSelectedBounds(self); /* this winds up calling figogrp_GetBounds() again, but self->bboxdirty will be FALSE, so no loop will result. */
    return R;
}

struct rectangle *figogrp__GetSelectedBounds(self, vv)
struct figogrp *self;
struct figview *vv;
{
    figogrp_GetBounds(self, vv); /* make sure bbox is cleaned up */
    return &(((struct figobj *)self)->selbounds);
}

void figogrp__StabilizeAttachments(self, keepproport)
struct figogrp *self;
boolean keepproport;
{
    if (((struct figobj *)self)->anyattachmentsactive 
	 && ((struct figobj *)self)->vas) {
	figogrp_RecomputeBounds(self);
	figogrp_GetBounds(self, NULL);
	super_StabilizeAttachments(self, keepproport);
    }
}

static enum figobj_HandleType handletypes[8]={
    figobj_MiddleRight,
    figobj_URCorner,
    figobj_MiddleTop,
    figobj_ULCorner,
    figobj_MiddleLeft,
    figobj_LLCorner,
    figobj_MiddleBottom,
    figobj_LRCorner
};

enum figobj_HandleType figogrp__GetHandleType(self, num)
struct figogrp *self;
long num;
{
    if (num>=0 && num<=7) return handletypes[num];
    else return figobj_None;
}

static long canonical_con[] = {
    2, 4, 6, 0, figobj_NULLREF
};
static long canonical_nocon[] = {
    3, figobj_NULLREF
};

long *figogrp__GetCanonicalHandles(self)
struct figogrp *self;
{
    if (self->doconstraints)
	return canonical_con;
    else
	return canonical_nocon;
}

void figogrp__SetParent(self, pref, ancfig)
struct figogrp *self;
long pref;
struct figure *ancfig;
{
    super_SetParent(self, pref, ancfig);
}

void figogrp__InheritVAttributes(self, attr, mask)
struct figogrp *self;
struct figattr *attr;
unsigned long mask;
{
    int ix;
    struct figure *fig;
    unsigned long tmpmask, cmask;
    struct figattr *selfattr = figogrp_GetVAttributes(self);
    struct figattr *selfiattr = figogrp_GetIVAttributes(self);

    figattr_CopyData(selfiattr, attr, mask);
    cmask = mask & (~(selfattr->active)) & figattr_MaskAll;
    /* cmask is now the list of things that have changed in self's working attributes, and thus the list of things that have changed in all children's inherited attributes. */
    if (cmask)
	figogrp_SetModified(self);

    fig = figogrp_GetAncestorFig(self);
    if (!fig) return;

    if (cmask) {
	/* gross hack approaching -- see comment in UpdateVAttributes() */
	tmpmask = selfattr->active;

	figattr_CopyData(selfattr, selfiattr, mask & (~tmpmask));

	for (ix=self->root; ix!=figure_NULLREF; ix=fig->objs[ix].next) {
	    figobj_InheritVAttributes(fig->objs[ix].o, selfattr, cmask);
	}

	/* put attr back the way we found it. */
	selfattr->active = tmpmask;
    }
}

unsigned long figogrp__UpdateVAttributes(self, attr, mask)
struct figogrp *self;
struct figattr *attr;
unsigned long mask;
{
    int ix;
    struct figure *fig;
    unsigned long tmpmask;
    struct figattr *selfattr = figogrp_GetVAttributes(self);
    struct figattr *selfiattr = figogrp_GetIVAttributes(self);

    if (figogrp_GetReadOnly(self))
	return 0;

    figattr_CopyData(selfattr, attr, mask);
    figogrp_SetModified(self);

    fig = figogrp_GetAncestorFig(self);
    if (!fig) return;

    /* we now need to create a new environment attr, which has all parts active. We do this in selfattr, replacing all inactive parts with parts from iattr. This is, of course, horrible -- but very easy. (However, don't even *think* of calling this reentrantly (with the same self).) */
    tmpmask = selfattr->active;
    
    figattr_CopyData(selfattr, selfiattr, ~tmpmask);

    for (ix=self->root; ix!=figure_NULLREF; ix=fig->objs[ix].next) {
	figobj_InheritVAttributes(fig->objs[ix].o, selfattr, mask);
    }

    /* put attr back the way we found it. */
    selfattr->active = tmpmask;
}

void figogrp__WriteBody(self, fp)
struct figogrp *self;
FILE *fp;
{
    fprintf(fp, "$ %d %d %d %d %d\n", self->doconstraints, self->handlebox.left, self->handlebox.top, self->handlebox.width, self->handlebox.height);
}

long figogrp__ReadBody(self, fp, recompute)
struct figogrp *self;
FILE *fp;
boolean recompute;
{
#define LINELENGTH (250)
    char buf[LINELENGTH+1];
    int ix;
    long val1, val2, val3, val4, val5;

    if (fgets(buf, LINELENGTH, fp) == NULL)
	return dataobject_PREMATUREEOF;
    ix = sscanf(buf, "$ %ld %ld %ld %ld %ld", &val1, &val2, &val3, &val4, &val5);

    if (ix != 5) return dataobject_BADFORMAT;

    if (val2 == (-1) && val3 == (-1) && val4 == (-1) && val5 == (-1)) {
	/* the group was written out as the root of a figure_WritePartial() operation. Thus, we should ignore these values, rather than overwrite the values of this (the focus group.) */
    }
    else {
	self->doconstraints = (val1) ? TRUE : FALSE;
	rectangle_SetRectSize(&self->handlebox, val2, val3, val4, val5);
    }

    if (recompute) {
	figogrp_RecomputeBounds(self);
	figogrp_SetModified(self);
    }

    return dataobject_NOREADERROR;
}

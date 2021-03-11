/* figobj.c - fig element object */
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
char *figobj_c_rcsid = "$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/figure/RCS/figobj.c,v 1.4 1993/05/04 01:18:42 susan Exp $";
#endif 

#include <figobj.eh>

#include <class.h>

#include <view.ih>
#include <figv.ih>
#include <figure.ih>
#include <figogrp.ih>
#include <figtoolv.ih>
#include <figattr.ih>
#include <message.ih>
#include <fontsel.ih>

#include <point.h>

boolean figobj__InitializeClass(ClassID)
struct classhdr *ClassID;
{
    return TRUE;
}

boolean figobj__InitializeObject(ClassID, self)
struct classhdr *ClassID;
struct figobj *self;
{
    self->numpts = 0;
    self->pts = NULL;
    self->pt_size = 0;
    self->parent = NULL;
    self->parentref = figobj_NULLREF;
    self->figo = NULL;
    self->isgroup = FALSE;
    self->isinset = FALSE;
    self->attrused = figattr_MaskAll;
    self->attr = figattr_New();
    self->iattr = figattr_New();
    self->vas = NULL;
    self->anyattachmentsactive = FALSE;
    rectangle_EmptyRect(&(self->bounds));

    return TRUE;
}

void figobj__FinalizeObject(ClassID, self)
struct classhdr *ClassID;
struct figobj *self;
{
    if (self->pts)
	free(self->pts);
    if(self->vas)
	free(self->vas);

    figattr_Destroy(self->attr);
    figattr_Destroy(self->iattr);
}

char *figobj__ToolName(dummy, v, rock)
struct figobj *dummy;
struct figtoolview *v;
long rock;
{
    return "<no-name>";
}

/* called when the tool is reclicked in the toolset */
void figobj__ToolModify(dummy, v, rock) 
struct figobj *dummy;
struct figtoolview *v;
long rock;
{
}

struct figobj *figobj__Instantiate(dummy, v, rock) 
struct figobj *dummy;
struct figtoolview *v;
long rock;
{
    struct figobj *res = (struct figobj *)class_NewObject(class_GetTypeName(dummy));
    return res;
}

void figobj__SetNumHandles(self, num)
struct figobj *self;
long num;
{
    int ix;
    if (num > self->pt_size) {
	if (self->pt_size == 0) {
	    self->pt_size = num;
	    self->pts = (struct point *)malloc(self->pt_size * sizeof(struct point));
	    self->vas = (struct figobj_Attachment *)malloc(self->pt_size * sizeof(struct figobj_Attachment));
	    for (ix=0; ix<self->pt_size; ix++) 
		self->vas[ix].on = FALSE;
	}
	else {
	    while (num > self->pt_size)
		self->pt_size *= 2;
	    self->pts = (struct point *)realloc(self->pts, self->pt_size * sizeof(struct point));
	    self->vas = (struct figobj_Attachment *)realloc(self->vas, self->pt_size * sizeof(struct figobj_Attachment));
	    for (ix=self->numpts; ix<self->pt_size; ix++) 
		self->vas[ix].on = FALSE;
	}
    }
    self->numpts = num;
}

enum figobj_HandleType figobj__GetHandleType(self, num)
struct figobj *self;
long num;
{
    return figobj_None;
}

static long canonical[] = {
    figobj_NULLREF
};

long *figobj__GetCanonicalHandles(self)
struct figobj *self;
{
    return canonical;
}

struct rectangle *figobj__GetBounds(self, vv)
struct figobj *self;
struct figview *vv;
{
    return &(self->bounds);
}

struct rectangle *figobj__GetSelectedBounds(self, vv)
struct figobj *self;
struct figview *vv;
{
    return &(self->selbounds);
}

void figobj__UpdateParentBounds(self)
struct figobj *self;
{
    struct figogrp *tmp;

    /* it would be somewhat more logical to do this recursively. Or even make it an override on figobj_SetModified(). For now, this is good enough and fast. */
    for (tmp=self->parent; tmp; tmp=figogrp_GetParent(tmp)) {
	figogrp_SetChildBoundMod(tmp);
	figogrp_SetModified(tmp);
    }
    if (self->figo) {
	figure_SetChildBoundMod(self->figo);
	/* figure_SetModified(self->figo); lovely idea, but no good. The figotext objects update their bounds unpredictably. */
    }
}

/* Should only be called by figobj_ methods. 
  set bounding box in fig coordinates */
void figobj__RecomputeBounds(self)
struct figobj *self;
{
    figobj_UpdateParentBounds(self);
    rectangle_SetRectSize(&(self->bounds), self->x-50, self->y-50, 101, 101);
}

void figobj__Draw(self, v) 
struct figobj *self;
struct figview *v;
{
    figview_SetTransferMode(v, graphic_COPY);
}

void figobj__Sketch(self, v) 
struct figobj *self;
struct figview *v;
{
    long x, y, w, h;
    struct rectangle *rec = figobj_GetBounds(self, v);

    x = figview_ToPixX(v, rec->left);
    y = figview_ToPixY(v, rec->top);
    w = figview_ToPixW(v, rec->width);
    h = figview_ToPixH(v, rec->height);
    figview_SetTransferMode(v, graphic_INVERT);
    figview_DrawRectSize(v, x, y, w, h);
}

/* draw handles or whatever. defaults to drawing all handles. */
void figobj__Select(self, v)
struct figobj *self;
struct figview *v;
{
    long ix;
    long x, y;
    struct graphic *BlackPattern;

    if (self->pts && self->numpts) {
	figview_SetTransferMode(v, graphic_INVERT);
	BlackPattern = figview_BlackPattern(v);

	for (ix=0; ix<self->numpts; ix++) {
	    x = figview_ToPixX(v, point_X(&self->pts[ix]));
	    y = figview_ToPixY(v, point_Y(&self->pts[ix]));
	    figview_FillRectSize(v, x-figview_SpotRad, y-figview_SpotRad, 1+2*figview_SpotRad, 1+2*figview_SpotRad, BlackPattern);
	}
    }
}

static void UnionRectanglePt(rec, x, y)
struct rectangle *rec;
long x, y;
{
    if (rectangle_IsEmptyRect(rec)) {
	rectangle_SetRectSize(rec, x, y, 1, 1);
	return;
    }

    if (x < rec->left) {
	rec->width += rec->left-x;
	rec->left = x;
    }
    else if (x >= rec->left+rec->width)
	rec->width = (x + 1 - rec->left);

    if (y < rec->top) {
	rec->height += rec->top-y;
	rec->top = y;
    }
    else if (y >= rec->top+rec->height)
	rec->height = (y + 1 - rec->top);
}

/* this assumes that self->bounds has already been set to the basic bounding box value, and the object's handles have been properly defined. */
void figobj__ComputeSelectedBounds(self)
struct figobj *self;
{
    long ix;
    long x, y;
    self->selbounds = self->bounds;

    /* self->anyattachmentsactive is only a hint; it may be TRUE even if no attachments are active. */
    if (self->anyattachmentsactive && self->vas && self->numpts) {
	for (ix=0; ix<self->numpts; ix++) 
	    if (self->vas[ix].on) {
		x = point_X(&self->pts[ix]) - self->vas[ix].offx;
		y = point_Y(&self->pts[ix]) - self->vas[ix].offy;
		UnionRectanglePt(&(self->selbounds), x, y);
	    }
    }
}

void figobj__ClearAttachments(self)
struct figobj *self;
{
    int ix;

    if (self->vas)
	for (ix=0; ix<figobj_GetNumHandles(self); ix++) {
	    figobj_SetAttachmentActive(self, ix, FALSE);
	}
    self->anyattachmentsactive = FALSE;
}

void figobj__StabilizeAttachments(self, keepproport)
struct figobj *self;
boolean keepproport;
{
    boolean didany = FALSE;
    struct figogrp *vg = figobj_GetParent(self);
    struct rectangle *R = (&(vg)->handlebox);
    long x, y;
    int ix;

    if (self->anyattachmentsactive && self->vas) {
	for (ix=0; ix<figobj_GetNumHandles(self); ix++) 
	    if (figobj_IsAttachmentActive(self, ix)) {
		didany = TRUE;

		if (!keepproport) {
		    x = figobj_GetHandleX(self, ix) - figobj_GetAttachmentOffX(self, ix);
		    y = figobj_GetHandleY(self, ix) - figobj_GetAttachmentOffY(self, ix);
		    figobj_SetAttachmentPosX(self, ix, ((x-R->left)*figogrp_Range)/R->width);
		    figobj_SetAttachmentPosY(self, ix, ((y-R->top)*figogrp_Range)/R->height);
		}
		else {
		    x = figobj_GetAttachmentPosX(self, ix)*R->width/figogrp_Range + R->left;
		    y = figobj_GetAttachmentPosY(self, ix)*R->height/figogrp_Range + R->top;
		    figobj_SetAttachmentOffX(self, ix, figobj_GetHandleX(self, ix) - x);
		    figobj_SetAttachmentOffY(self, ix, figobj_GetHandleY(self, ix) - y);
		} 
	    }

	if (didany) {
	    figobj_ComputeSelectedBounds(self);
	    figobj_SetModified(self); 
	}
    }
}

static short offsets[9][2] = {
    {0, 0},   /*figobj_None*/
    {1, -1},  /*figobj_ULCorner*/
    {-1, -1}, /*figobj_LLCorner*/
    {-1, 1},  /*figobj_LRCorner*/
    {1, 1},   /*figobj_URCorner*/
    {0, -1},  /*figobj_MiddleLeft*/
    {0, 1},   /*figobj_MiddleRight*/
    {1, 0},   /*figobj_MiddleTop*/
    {-1, 0}   /*figobj_MiddleBottom*/
};

void figobj__DrawAttachments(self, v) 
struct figobj *self;
struct figview *v;
{
    long ix;
    long x, y, dx, dy;
    short offx, offy; /* offsets to make things more visible. */
    struct graphic *BlackPattern;
    int typ;

    if (self->anyattachmentsactive && self->vas && self->numpts) {
	figview_SetTransferMode(v, graphic_INVERT);
	BlackPattern = figview_BlackPattern(v);

	for (ix=0; ix<self->numpts; ix++) 
	    if (self->vas[ix].on) {
		typ = (int)(figobj_GetHandleType(self, ix)) - (int)(figobj_None);
		if (typ >= 9) {
		    offx = 0;
		    offy = 0;
		}
		else {
		    offx = offsets[typ][0];
		    offy = offsets[typ][1];
		}
		x = offx + figview_ToPixX(v, point_X(&self->pts[ix]));
		y = offy + figview_ToPixY(v, point_Y(&self->pts[ix]));
		dx = -figview_ToPixW(v, self->vas[ix].offx);
		dy = -figview_ToPixH(v, self->vas[ix].offy);

		figview_DrawRectSize(v, x+dx-(figview_AnchRad), y+dy-(figview_AnchRad), 2*(figview_AnchRad), 2*(figview_AnchRad));

		figview_MoveTo(v, x+dx, y+dy);
		figview_DrawLineTo(v, x, y);
	    }
    }
}

/* zero clicks means user hit ctrl-G or switched modes. Build may draw further bits on the screen. */
enum figobj_Status figobj__Build(self, v, action, x, y, clicks)   
struct figobj *self;
struct figview *v;
enum view_MouseAction action;
long x, y; /* in fig coords */
long clicks;
{
    if (clicks==0) {
	message_DisplayString(v, 10, "Object aborted.");
	return figobj_Failed;
    }

    self->x = x;
    self->y = y;
    figobj_RecomputeBounds(self);
    figobj_SetModified(self);
    return figobj_Done;
}

enum figobj_HitVal figobj__HitMe(self, x, y, delta, ptref) 
struct figobj *self;
long x, y;
long delta;
long *ptref;
{
    return figobj_BasicHitMe(self, x, y, delta, ptref);
}

/* check if point is in bounds rectangle, with a little leeway */
/* the idea is to return figobj_HitHandle if it hits a handle, figobj_HitInside if it's in the bounding rectangle, figobj_Miss otherwise. */
enum figobj_HitVal figobj__BasicHitMe(self, x, y, delta, ptref) 
struct figobj *self;
long x, y;
long delta;
long *ptref;
{
    struct point pt;
    struct rectangle tmp, *bbox;
    long xd, yd;
    long ix, tmpt;

    point_SetPt(&pt, x, y);
    bbox = figobj_GetBounds(self, NULL);

    if (!self->pts || !self->numpts) {
	if (rectangle_IsPtInRect(&pt, bbox)) {
	    return figobj_HitInside;
	}
	else
	    return figobj_Miss;
    }

    tmp = *bbox;
    rectangle_InsetRect(&tmp, -delta, -delta);
    if (!rectangle_IsPtInRect(&pt, &tmp)) {
	return figobj_Miss;
    }

    /* see if handles match */
    tmpt = figobj_NULLREF;
    for (ix=figobj_GetNumHandles(self)-1; ix>=0; ix--) {
	xd = x - point_X(&(figobj_GetHandles(self)[ix]));
	yd = y - point_Y(&(figobj_GetHandles(self)[ix]));
	if (xd<=delta && xd>=(-delta) && yd<=delta && yd>=(-delta)) {
	    tmpt = ix;
	    break;
	}
    }

    if (tmpt != figobj_NULLREF) {
	if (ptref)
	    *ptref = tmpt;
	return figobj_HitHandle;
    }

    return figobj_HitInside; 
}

void figobj__MoveHandle(self, x, y, ptref)
struct figobj *self;
long x, y, ptref;
{
    
}

boolean figobj__AddParts(self, action, v, x, y, handle, ptref)
struct figobj *self;
enum view_MouseAction action;
struct figview *v;
boolean handle;
long x, y, ptref;
{
    return FALSE;
}

boolean figobj__DeleteParts(self, action, v, x, y, handle, ptref)
struct figobj *self;
enum view_MouseAction action;
struct figview *v;
boolean handle;
long x, y, ptref;
{
    return FALSE;
}

boolean figobj__Reshape(self, action, v, x, y, handle, ptref)
struct figobj *self;
enum view_MouseAction action;
struct figview *v;
boolean handle;
long x, y, ptref;
{
    if (!handle)
	return FALSE;

    switch (action) {
	case view_LeftDown:
	case view_RightDown:
	    if (figobj_GetReadOnly(self))
		return FALSE;
	    /*figobj_Sketch(self, v);*/
	    break;
	case view_LeftMovement:
	case view_RightMovement:
	    figobj_Sketch(self, v);
	    figobj_MoveHandle(self, x, y, ptref);
	    figobj_Sketch(self, v);
	    break;
	case view_LeftUp:
	case view_RightUp:
	    figobj_Sketch(self, v);
	    figobj_MoveHandle(self, x, y, ptref);
	    figobj_SetModified(self);
	    break;
    }
    return TRUE;
}

void figobj__Reposition(self, xd, yd)
struct figobj *self;
long xd, yd;
{
    if (figobj_GetReadOnly(self))
	return;

    self->x += xd;
    self->y += yd;
    figobj_RecomputeBounds(self);
    figobj_SetModified(self);
}

void figobj__SetParent(self, pref, ancfig)
struct figobj *self;
long pref;
struct figure *ancfig;
{
    struct figattr *tmp;

    self->figo = ancfig;
    self->parentref = pref;
    if (ancfig==NULL) {
	self->parent = NULL;
    }
    else {
	self->parent = (struct figogrp *)figure_FindObjectByRef(ancfig, pref);
	if (self->parent==NULL) {
	    figattr_SetShade(self->iattr, 0);
	    figattr_SetTextPos(self->iattr, figattr_PosCenter);
	    figattr_SetLineWidth(self->iattr, 0);
	    figattr_SetRRectCorner(self->iattr, 10);
	    figattr_SetColor(self->iattr, "black");
	    figattr_SetFontFamily(self->iattr, fontsel_default_Family);
	    figattr_SetFontSize(self->iattr, fontsel_default_Size);
	    figattr_SetFontStyle(self->iattr, fontsel_default_Style);
	}
	else {
	    figattr_CopyData(self->iattr, figogrp_GetIVAttributes(self->parent), figattr_MaskAll);
	    tmp = figogrp_GetVAttributes(self->parent);
	    figattr_CopyData(self->iattr, tmp, tmp->active);
	}
    }
    figobj_SetModified(self);
}

/* notify self that an ancestor group's attributes have changed. mask indicates which ones; all mask-active parts will be active. */
void figobj__InheritVAttributes(self, attr, mask)
struct figobj *self;
struct figattr *attr;
unsigned long mask;
{
    figattr_CopyData(self->iattr, attr, mask);
    if (mask & (~(self->attr->active)) & figobj_AttributesUsed(self))
	figobj_SetModified(self);
}

/* update self's personal attributes. mask indicates which parts of attr to use; ignore all others. Returns a mask of which attributes were actually set. */
unsigned long figobj__UpdateVAttributes(self, attr, mask)
struct figobj *self;
struct figattr *attr;
unsigned long mask;
{
    if (figobj_GetReadOnly(self))
	return 0;
    figattr_CopyData(self->attr, attr, mask);
    if (mask & figobj_AttributesUsed(self)) {
	figobj_SetModified(self);
	if (self->figo) {
	    figure_SetModified(self->figo);
	}
    }
    return mask;
}

#define LINELENGTH (250)
static char buf[LINELENGTH+1];

void figobj__WriteBody(self, fp)
struct figobj *self;
FILE *fp;
{
    fprintf(fp, "$ %d %d\n", figobj_PosX(self), figobj_PosY(self));
}

long figobj__ReadBody(self, fp, recompute)
struct figobj *self;
FILE *fp;
boolean recompute;
{
    int	ix; 
    long x, y;

    if (fgets(buf, LINELENGTH, fp) == NULL)
	return dataobject_PREMATUREEOF;
    ix = sscanf(buf, "$ %ld %ld", &x, &y);
    if (ix!=2) return dataobject_BADFORMAT;

    self->x = x;
    self->y = y;

    if (recompute) {
	figobj_RecomputeBounds(self);
	figobj_SetModified(self);
    }

    return dataobject_NOREADERROR;
}

long figobj__Write(self, fp, writeid, level)
struct figobj *self;
FILE *fp;
long writeid;
int level;
{
    int ix;
    if (figobj_GetWriteID(self) != writeid) {
	figobj_SetWriteID(self, writeid);

	fprintf(fp, "\\begindata{%s,%ld}\n", class_GetTypeName(self), figobj_GetID(self));
	figattr_Write(figobj_GetVAttributes(self), fp, writeid, level+1);
	figobj_WriteBody(self, fp);
	if (self->numpts && self->vas) {
	    for (ix=0; ix<self->numpts; ix++) 
		if (self->vas[ix].on) {
		    fprintf(fp, "$# %d %d %d %d %d\n", ix, self->vas[ix].rposx, self->vas[ix].rposy, self->vas[ix].offx, self->vas[ix].offy);
		}
	}
	fprintf(fp, "$endatt\n");
	fprintf(fp, "\\enddata{%s,%ld}\n", class_GetTypeName(self), figobj_GetID(self));
    }

    return figobj_GetID(self);
}

long figobj__Read(self, fp, id)
struct figobj *self;
FILE *fp;
long id;
{
    long unid, tid, ix;
    long val1, val2, val3, val4, val5;
    char namebuf[100];

    if (id==0) 
	unid = figobj_UniqueID(self);
    else
	unid = id;
    figobj_SetID(self, unid); 

    ix = figattr_Read(figobj_GetVAttributes(self), fp, 0);
    if (ix!=dataobject_NOREADERROR) return ix;
    ix = figobj_ReadBody(self, fp, TRUE);
    if (ix!=dataobject_NOREADERROR) return ix;

    if (self->numpts && self->vas) {
	for (ix=0; ix<self->numpts; ix++) 
	    self->vas[ix].on = FALSE;
    }
    
    if (fgets(buf, LINELENGTH, fp) == NULL)
	return dataobject_PREMATUREEOF;

    while (strncmp(buf, "$endatt", 7)) {
	ix = sscanf(buf, "$# %ld %ld %ld %ld %ld", &val1, &val2, &val3, &val4, &val5);
	if (ix!=5) return dataobject_BADFORMAT;
	figobj_SetAttachmentActive(self, val1, TRUE);
	self->vas[val1].rposx = val2;
	self->vas[val1].rposy = val3;
	self->vas[val1].offx = val4;
	self->vas[val1].offy = val5;
	if (fgets(buf, LINELENGTH, fp) == NULL)
	    return dataobject_PREMATUREEOF;
    }

    if (fgets(buf, LINELENGTH, fp) == NULL)
	return dataobject_PREMATUREEOF;
    ix = sscanf(buf, "\\enddata{%[^,],%ld}", namebuf, &tid);
    if (ix!=2) return dataobject_MISSINGENDDATAMARKER;

    if (tid!=id || strcmp(namebuf, class_GetTypeName(self)))
	return dataobject_MISSINGENDDATAMARKER;

    return dataobject_NOREADERROR;     
}

void figobj__PrintObject(self, v, file, prefix)
struct figobj *self;
struct figview *v;
FILE *file;
char *prefix;
{
}

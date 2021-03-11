/* figorect.c - fig element object: rectangle */
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
char *figorect_c_rcsid = "$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/figure/RCS/figorect.c,v 1.4 1993/05/04 01:18:42 susan Exp $";
#endif

#include <figorect.eh>

#include <figattr.ih>
#include <view.ih>
#include <figv.ih>
#include <figure.ih>
#include <figtoolv.ih>
#include <message.ih>
#include <print.ih>

#include <rect.h>

boolean figorect__InitializeObject(ClassID, self)
struct classhdr *ClassID;
struct figorect *self;
{
    figorect_PosW(self) = 0;
    figorect_PosH(self) = 0;

    figorect_AttributesUsed(self) = (1<<figattr_Shade) | (1<<figattr_LineWidth) | (1<<figattr_Color);

    figorect_SetNumHandles(self, 9);
    return TRUE;
}

struct figorect *figorect__Create(classID, left, top, width, height)
struct classheader *classID;
long left, top, width, height;
{
    struct figorect *res = figorect_New();
    if (!res) return NULL;

    figorect_PosX(res) = left;
    figorect_PosY(res) = top;
    figorect_PosW(res) = width;
    figorect_PosH(res) = height;
    figorect_RecomputeBounds(res);

    return res;
}

char *figorect__ToolName(dummy, v, rock)
struct figorect *dummy;
struct figtoolview *v;
long rock;
{
    return "Rectangle";
}

/* set bounding box and handle list in fig coordinates */
void figorect__RecomputeBounds(self)
struct figorect *self;
{
    long left, width, top, height;
    long lwid;

    if (figorect_PosW(self) >= 0) {
	left = figorect_PosX(self);
	width = figorect_PosW(self);
    }
    else {
	left = figorect_PosX(self)+figorect_PosW(self);
	width = -figorect_PosW(self);
    }

    if (figorect_PosH(self) >= 0) {
	top = figorect_PosY(self);
	height = figorect_PosH(self);
    }
    else {
	top = figorect_PosY(self)+figorect_PosH(self);
	height = -figorect_PosH(self);
    }

    lwid = figattr_GetLineWidth(figorect_GetVAttributes(self), figorect_GetIVAttributes(self));
    lwid = lwid*figview_FigUPerPix;

    figorect_SetBoundsRect(self, left-lwid/2, top-lwid/2, width+lwid+1, height+lwid+1);

    figorect_SetHandle(self, 0, figorect_PosX(self)+figorect_PosW(self)/2, figorect_PosY(self)+figorect_PosH(self)/2);
    figorect_SetHandle(self, 1, figorect_PosX(self)+figorect_PosW(self), figorect_PosY(self)+figorect_PosH(self)/2);
    figorect_SetHandle(self, 2, figorect_PosX(self)+figorect_PosW(self), figorect_PosY(self));
    figorect_SetHandle(self, 3, figorect_PosX(self)+figorect_PosW(self)/2, figorect_PosY(self));
    figorect_SetHandle(self, 4, figorect_PosX(self), figorect_PosY(self));
    figorect_SetHandle(self, 5, figorect_PosX(self), figorect_PosY(self)+figorect_PosH(self)/2);
    figorect_SetHandle(self, 6, figorect_PosX(self), figorect_PosY(self)+figorect_PosH(self));
    figorect_SetHandle(self, 7, figorect_PosX(self)+figorect_PosW(self)/2, figorect_PosY(self)+figorect_PosH(self));
    figorect_SetHandle(self, 8, figorect_PosX(self)+figorect_PosW(self), figorect_PosY(self)+figorect_PosH(self));

    figorect_ComputeSelectedBounds(self);

    figorect_UpdateParentBounds(self);
}

static enum figobj_HandleType handletypes[9] = {
   figobj_Center,
   figobj_MiddleRight,
   figobj_URCorner,
   figobj_MiddleTop,
   figobj_ULCorner,
   figobj_MiddleLeft,
   figobj_LLCorner,
   figobj_MiddleBottom,
   figobj_LRCorner
};

enum figobj_HandleType figorect__GetHandleType(self, num)
struct figorect *self;
long num;
{
    if(num>=0 && num<=8) return handletypes[num];
    else return figobj_None;
}

static long canonical[] = {
    3, 5, 7, 1, figobj_NULLREF
};

long *figorect__GetCanonicalHandles(self)
struct figorect *self;
{
    return canonical;
}

void figorect__Draw(self, v) 
struct figorect *self;
struct figview *v;
{
    long x, y, w, h;
    long shad, lw;
    char *col;

    if (figorect_PosW(self) >= 0) {
	x = figview_ToPixX(v, figorect_PosX(self));
	w = figview_ToPixW(v, figorect_PosW(self));
    }
    else {
	x = figview_ToPixX(v, figorect_PosX(self)+figorect_PosW(self));
	w = figview_ToPixW(v, -figorect_PosW(self));
    }
    
    if (figorect_PosH(self) >= 0) {
	y = figview_ToPixY(v, figorect_PosY(self));
	h = figview_ToPixH(v, figorect_PosH(self));
    }
    else {
	y = figview_ToPixY(v, figorect_PosY(self)+figorect_PosH(self));
	h = figview_ToPixH(v, -figorect_PosH(self));
    }

    figview_SetTransferMode(v, graphic_COPY);

    col = figattr_GetColor(figorect_GetVAttributes(self), figorect_GetIVAttributes(self));
    figview_SetForegroundColor(v, col, 0, 0, 0); 

    shad = figattr_GetShade(figorect_GetVAttributes(self), figorect_GetIVAttributes(self));
    if (shad != figattr_ShadeClear)
	figview_FillRectSize(v, x, y, w, h, figview_GrayPattern(v, shad, figattr_ShadeDenom));
    lw = figattr_GetLineWidth(figorect_GetVAttributes(self), figorect_GetIVAttributes(self));
    lw = figview_ToPixW(v, lw*figview_FigUPerPix);
    if (lw <= 0) lw = 1;
    if (lw != 1)
	figview_SetLineWidth(v, lw);

    figview_DrawRectSize(v, x, y, w, h); 

    if (lw != 1)
	figview_SetLineWidth(v, 1);
}

void figorect__Sketch(self, v) 
struct figorect *self;
struct figview *v;
{
    long x, y, w, h;

    if (figorect_PosW(self) >= 0) {
	x = figview_ToPixX(v, figorect_PosX(self));
	w = figview_ToPixW(v, figorect_PosW(self));
    }
    else {
	x = figview_ToPixX(v, figorect_PosX(self)+figorect_PosW(self));
	w = figview_ToPixW(v, -figorect_PosW(self));
    }
    
    if (figorect_PosH(self) >= 0) {
	y = figview_ToPixY(v, figorect_PosY(self));
	h = figview_ToPixH(v, figorect_PosH(self));
    }
    else {
	y = figview_ToPixY(v, figorect_PosY(self)+figorect_PosH(self));
	h = figview_ToPixH(v, -figorect_PosH(self));
    }

    figview_SetTransferMode(v, graphic_INVERT);
    figview_DrawRectSize(v, x, y, w, h); 
}

enum figobj_HitVal figorect__HitMe(self, x, y, delta, ptref) 
struct figorect *self;
long x, y;
long delta;
long *ptref;
{
    enum figobj_HitVal res = figorect_BasicHitMe(self, x, y, delta, ptref);
    long x0, y0, x1, y1;
    long xin, yin;

    if (res==figobj_HitHandle)
	return res;

    x0 = figorect_PosX(self);
    x1 = x0 + figorect_PosW(self);
    y0 = figorect_PosY(self);
    y1 = y0 + figorect_PosH(self);

    xin = !((x<x0 && x<x1) || (x>x0 && x>x1));
    yin = !((y<y0 && y<y1) || (y>y0 && y>y1));

    if (x>=x0-delta && x<=x0+delta && yin) {
	if (ptref)
	    *ptref = 2;
	return figobj_HitBody;
    }
    if (x>=x1-delta && x<=x1+delta && yin) {
	if (ptref)
	    *ptref = 0;
	return figobj_HitBody;
    }
    if (y>=y0-delta && y<=y0+delta && xin) {
	if (ptref)
	    *ptref = 1;
	return figobj_HitBody;
    }
    if (y>=y1-delta && y<=y1+delta && xin) {
	if (ptref)
	    *ptref = 3;
	return figobj_HitBody;
    }

    return res;
}

enum figobj_Status figorect__Build(self, v, action, x, y, clicks)   
struct figorect *self;
struct figview *v;
enum view_MouseAction action;
long x, y; /* in fig coords */
long clicks;
{
    long px, py;

    switch (action) {
	case view_LeftDown:
	    figorect_PosX(self) = x;
	    figorect_PosY(self) = y;
	    figorect_RecomputeBounds(self);
	    figorect_Sketch(self, v);
	    return figobj_NotDone;
	case view_LeftMovement:
	    figorect_Sketch(self, v);
	    figorect_PosW(self) = x - figorect_PosX(self);
	    figorect_PosH(self) = y - figorect_PosY(self);
	    figorect_Sketch(self, v);
	    return figobj_NotDone;
	case view_LeftUp:
	    figorect_Sketch(self, v);
	    figorect_PosW(self) = x - figorect_PosX(self);
	    figorect_PosH(self) = y - figorect_PosY(self);
	    figorect_RecomputeBounds(self);
	    px = figview_ToPixW(v, figorect_PosW(self));
	    py = figview_ToPixH(v, figorect_PosH(self));
	    if (px < figview_MouseHysteresis && px > -figview_MouseHysteresis && py < figview_MouseHysteresis && py > -figview_MouseHysteresis) {
		/* point click */
		message_DisplayString(v, 10, "Zero size; object aborted.");
		return figobj_Failed;
	    }
	    else {
		figorect_SetModified(self);
		return figobj_Done;
	    }
	default:
	    return figobj_Failed;
    }
}

/* basic procedure to move a handle -- used by figorect__MoveHandle(), figorect__Reshape() */
static void MoveHandle(self, x, y, ptref)
struct figorect *self;
long x, y, ptref;
{
    long ix;

    switch (ptref) {
	case 0:
	    figorect_PosX(self) = x - figorect_PosW(self)/2;
	    figorect_PosY(self) = y - figorect_PosH(self)/2;
	    break;
	case 1:
	    figorect_PosW(self) = x - figorect_PosX(self);
	    break;
	case 2:
	    figorect_PosW(self) = x - figorect_PosX(self);
	    ix = figorect_PosY(self);
	    figorect_PosY(self) = y;
	    figorect_PosH(self) += (ix-y);
	    break;
	case 3:
	    ix = figorect_PosY(self);
	    figorect_PosY(self) = y;
	    figorect_PosH(self) += (ix-y);
	    break;
	case 4:
	    ix = figorect_PosY(self);
	    figorect_PosY(self) = y;
	    figorect_PosH(self) += (ix-y);
	    ix = figorect_PosX(self);
	    figorect_PosX(self) = x;
	    figorect_PosW(self) += (ix-x);
	    break;
	case 5:
	    ix = figorect_PosX(self);
	    figorect_PosX(self) = x;
	    figorect_PosW(self) += (ix-x);
	    break;
	case 6:
	    figorect_PosH(self) = y - figorect_PosY(self);
	    ix = figorect_PosX(self);
	    figorect_PosX(self) = x;
	    figorect_PosW(self) += (ix-x);
	    break;
	case 7:
	    figorect_PosH(self) = y - figorect_PosY(self);
	    break;
	case 8:
	    figorect_PosW(self) = x - figorect_PosX(self);
	    figorect_PosH(self) = y - figorect_PosY(self);
	    break;
    }
}

boolean figorect__Reshape(self, action, v, x, y, handle, ptref)
struct figorect *self;
enum view_MouseAction action;
struct figview *v;
boolean handle;
long x, y, ptref;
{
    if (!handle)
	return;

    switch (action) {
	case view_LeftDown:
	case view_RightDown:
	    if (figorect_GetReadOnly(self))
		return FALSE;
	    break;
	case view_LeftMovement:
	case view_RightMovement:
	    figorect_Sketch(self, v);
	    MoveHandle(self, x, y, ptref);
	    figorect_Sketch(self, v);
	    break;
	case view_LeftUp:
	case view_RightUp:
	    figorect_Sketch(self, v);
	    MoveHandle(self, x, y, ptref);
	    figorect_RecomputeBounds(self);
	    figorect_SetModified(self);
	    break;
    }
    return TRUE;
}

void figorect__MoveHandle(self, x, y, ptref)
struct figorect *self;
long x, y, ptref;
{
    if (figorect_GetReadOnly(self))
	return;
    MoveHandle(self, x, y, ptref);
    figorect_RecomputeBounds(self);
    figorect_SetModified(self);
}

void figorect__Reposition(self, xd, yd)
struct figorect *self;
long xd, yd;
{
    if (figorect_GetReadOnly(self))
	return;
    figorect_PosX(self) += xd;
    figorect_PosY(self) += yd;
    figorect_RecomputeBounds(self);
    figorect_SetModified(self);
}

void figorect__InheritVAttributes(self, attr, mask)
struct figorect *self;
struct figattr *attr;
unsigned long mask;
{
    super_InheritVAttributes(self, attr, mask);

    if (mask & (~(figorect_GetVAttributes(self)->active)) & (1<<figattr_LineWidth)) {
	figorect_RecomputeBounds(self);
    }
}

unsigned long figorect__UpdateVAttributes(self, attr, mask)
struct figorect *self;
struct figattr *attr;
unsigned long mask;
{
    mask = super_UpdateVAttributes(self, attr, mask);
    
    if (mask & (1<<figattr_LineWidth))
	figorect_RecomputeBounds(self);

    return mask;
}

void figorect__WriteBody(self, fp)
struct figorect *self;
FILE *fp;
{
    super_WriteBody(self, fp);

    fprintf(fp, "$ %d %d\n", self->w, self->h);
}

long figorect__ReadBody(self, fp, recompute)
struct figorect *self;
FILE *fp;
boolean recompute;
{
    int	ix; 
    long w, h;

#define LINELENGTH (250)
    char buf[LINELENGTH+1];

    ix = super_ReadBody(self, fp, FALSE);
    if (ix!=dataobject_NOREADERROR) return ix;

    if (fgets(buf, LINELENGTH, fp) == NULL)
	return dataobject_PREMATUREEOF;
    ix = sscanf(buf, "$ %ld %ld", &w, &h);
    if (ix!=2) return dataobject_BADFORMAT;

    self->w = w;
    self->h = h;
    if (recompute) {
	figorect_RecomputeBounds(self);
	figorect_SetModified(self);
    }

    return dataobject_NOREADERROR;
}

#define FadeColor(col, shad)  (1.0 - (1.0-(shad)) * (1.0-(col)))

void figorect__PrintObject(self, v, file, prefix)
struct figorect *self;
struct figview *v;
FILE *file;
char *prefix;
{
    long x, y, w, h;
    long shad, lw;
    char *col;
    double rcol, bcol, gcol, shadcol;

    fprintf(file, "%s  %% rectangle\n", prefix);

    if (figorect_PosW(self) >= 0) {
	x = figview_ToPrintPixX(v, figorect_PosX(self));
	w = figview_ToPrintPixW(v, figorect_PosW(self));
    }
    else {
	x = figview_ToPrintPixX(v, figorect_PosX(self)+figorect_PosW(self));
	w = figview_ToPrintPixW(v, -figorect_PosW(self));
    }
    
    if (figorect_PosH(self) >= 0) {
	y = figview_ToPrintPixY(v, figorect_PosY(self));
	h = figview_ToPrintPixH(v, figorect_PosH(self));
    }
    else {
	y = figview_ToPrintPixY(v, figorect_PosY(self)+figorect_PosH(self));
	h = figview_ToPrintPixH(v, -figorect_PosH(self));
    }

    fprintf(file, "%s  %d %d moveto  %d %d lineto  %d %d lineto  %d %d lineto closepath\n", prefix, x, y,  x, y+h,  x+w, y+h,  x+w, y);

    col = figattr_GetColor(figorect_GetVAttributes(self), figorect_GetIVAttributes(self));
    print_LookUpColor(col, &rcol, &gcol, &bcol);

    shad = figattr_GetShade(figorect_GetVAttributes(self), figorect_GetIVAttributes(self));
    if (shad != figattr_ShadeClear) {
	fprintf(file, "%s  gsave\n", prefix);
	shadcol = (double)(figattr_ShadeDenom-shad) / (double)figattr_ShadeDenom;
	fprintf(file, "%s  %f %f %f setrgbcolor\n", prefix, FadeColor(rcol, shadcol), FadeColor(gcol, shadcol), FadeColor(bcol, shadcol));
	/*fprintf(file, "%s  %f setgray\n", prefix, shadcol);*/
	fprintf(file, "%s  fill grestore\n", prefix);
    }
    lw = figattr_GetLineWidth(figorect_GetVAttributes(self), figorect_GetIVAttributes(self));
    lw = figview_ToPrintPixW(v, lw*figview_FigUPerPix);
    if (lw <= 0) lw = 0;
    fprintf(file, "%s  %d setlinewidth\n", prefix, lw);
    fprintf(file, "%s  %f %f %f setrgbcolor\n", prefix, rcol, gcol, bcol);
    /*fprintf(file, "%s  0 setgray\n", prefix);*/
    fprintf(file, "%s  stroke\n", prefix);
}

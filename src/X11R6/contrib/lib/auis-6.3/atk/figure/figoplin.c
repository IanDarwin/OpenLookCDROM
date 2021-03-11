/* figoplin.c - fig element object: polyline */
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
char *figoplin_c_rcsid = "$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/figure/RCS/figoplin.c,v 1.7 1993/05/04 01:18:42 susan Exp $";
#endif

#include <math.h>

#include <figoplin.eh>

#include <figattr.ih>
#include <view.ih>
#include <figv.ih>
#include <figure.ih>
#include <figtoolv.ih>
#include <message.ih>
#include <print.ih>

#include <rect.h>

#define TWOPI (6.28318530718)
#define ClearOldPoints(self)  ((((self)->orpts) ? (free((self)->orpts), 1) : 0), ((self)->orpts) = NULL)

static void SetNumPts();

static struct point *ptemp;
static int ptemp_size;

boolean figoplin__InitializeClass(ClassID)
struct classhdr *ClassID;
{
    ptemp_size = 0;
    ptemp = NULL;

    return TRUE;
}

boolean figoplin__InitializeObject(ClassID, self)
struct classhdr *ClassID;
struct figoplin *self;
{
    figoplin_AttributesUsed(self) = (1<<figattr_LineWidth) | (1<<figattr_Color); /* add (1<<figattr_Shade) if closed is true */

    figoplin_SetNumHandles(self, 8);
    figoplin_SetNumHandles(self, 4);

    self->stickysketch = FALSE;
    self->shortmode = FALSE;
    self->closed = FALSE;
    self->regular = 0;
    self->fliph = FALSE;
    self->flipv = FALSE;
    self->dummysides = 6;

    self->buildstate = 0;
    self->pts = NULL;
    self->orpts = NULL;
    self->numpts = 0;
    self->pts_size = 0;
    SetNumPts(self, 4);
    SetNumPts(self, 0);

    return TRUE;
}

struct figoplin *figoplin__Create(classID, pointlist, numpoints, isclosed)
struct classheader *classID;
struct point *pointlist;
long numpoints;
boolean isclosed;
{
    int ix;
    struct figoplin *res = figoplin_New();
    if (!res) return NULL;
    if (numpoints<2 || !pointlist) return NULL;

    SetNumPts(res, numpoints);
    for (ix=0; ix<numpoints; ix++) {
	figoplin_Pts(res)[ix].x = pointlist[ix].x;
	figoplin_Pts(res)[ix].y = pointlist[ix].y;
    }
    figoplin_Closed(res) = isclosed;
    if (isclosed) {
	figoplin_AttributesUsed(res) |= (1<<figattr_Shade);
    }

    figoplin_RecomputeBounds(res);

    return res;
}

void figoplin__FinalizeObject(ClassID, self)
struct classhdr *ClassID;
struct figoplin *self;
{
    if (self->pts)
	free(self->pts);
    if (self->orpts)
	free(self->orpts);
}

char *figoplin__ToolName(dummy, v, rock)
struct figoplin *dummy;
struct figtoolview *v;
long rock;
{
    if (rock & 2) {
	if (rock & 1)
	    return "Reg N-gon";
	else
	    return "Polygon";
    }

    if (rock & 1)
	return "Line";
    else
	return "Polyline";
}

/* ### ought to be inherited from figobj, probably. Or maybe not. */
void figoplin__CopyData(self, src) 
struct figoplin *self;
struct figoplin *src;
{   
    int ix, num;
    struct figattr *vtmp;

    num = figoplin_NumPts(src);
    SetNumPts(self, num);

    for (ix=0; ix<num; ix++) {
	self->pts[ix].x = src->pts[ix].x;
	self->pts[ix].y = src->pts[ix].y;
    }
    self->closed = src->closed;
    figoplin_AttributesUsed(self) = figoplin_AttributesUsed(src);

    vtmp = figoplin_GetVAttributes(src);
    figoplin_UpdateVAttributes(self, vtmp, figattr_MaskAll);

    figoplin_RecomputeBounds(self);
    figoplin_SetModified(self);
}   

void figoplin__ToolModify(dummy, v, rock) 
struct figoplin *dummy;
struct figtoolview *v;
long rock;
{
    char buffer[32];
    char obuffer[256];
    int ix;

    sprintf(obuffer, "How many sides? [%d]: ", dummy->dummysides);
    ix = message_AskForString(v, 40, obuffer, NULL, buffer, 30); 

    if (ix<0) {
	message_DisplayString(v, 10, "Cancelled.");
	return;
    }

    if (strlen(buffer)!=0) {
	ix = atol(buffer);
	if (ix<3)  {
	    message_DisplayString(v, 10, "The value must be 3 or greater.");
	    return;
	}
	if (ix>32)  {
	    message_DisplayString(v, 10, "The value must be 32 or less.");
	    return;
	}
	dummy->dummysides = ix;
    }

    sprintf(obuffer, "%d-sided polygons.", dummy->dummysides);
    message_DisplayString(v, 10, obuffer);
}

struct figobj *figoplin__Instantiate(dummy, v, rock) 
struct figoplin *dummy;
struct figtoolview *v;
long rock;
{
    struct figoplin *res = (struct figoplin *)super_Instantiate(dummy, v, rock);

    if (rock & 1)
	res->shortmode = TRUE;

    if (rock & 2) {
	res->closed = TRUE;
	figoplin_AttributesUsed(res) |= (1<<figattr_Shade);
	if (res->shortmode)
	    res->regular = dummy->dummysides;
    }

    return (struct figobj *)res;
}

static void SetNumPts(self, num)
struct figoplin *self;
long num;
{
    ClearOldPoints(self);

    if (num > self->pts_size) {
	if (self->pts_size == 0) {
	    self->pts_size = num;
	    self->pts = (struct point *)malloc(self->pts_size * sizeof(struct point));
	}
	else {
	    while (num > self->pts_size)
		self->pts_size *= 2;
	    self->pts = (struct point *)realloc(self->pts, self->pts_size * sizeof(struct point));
	}
    }
    self->numpts = num;

    num += 1;
    if (num > ptemp_size) {
	if (ptemp_size == 0) {
	    ptemp_size = num;
	    ptemp = (struct point *)malloc(ptemp_size * sizeof(struct point));
	}
	else {
	    while (num > ptemp_size)
		ptemp_size *= 2;
	    ptemp = (struct point *)realloc(ptemp, ptemp_size * sizeof(struct point));
	}
    }
}

/* set bounding box and handle list in fig coordinates */
void figoplin__RecomputeBounds(self)
struct figoplin *self;
{
    long basex, basey, left, right, top, bot, wid, hgt;
    long lwid;
    int ix;
    short bleft, btop;

    figoplin_SetNumHandles(self, self->numpts+4);
    basex = self->pts[0].x;
    basey = self->pts[0].y;
    figoplin_SetHandle(self, 4, basex, basey);

    left = right = top = bot = 0;
    for (ix=1; ix<self->numpts; ix++) {
	if (self->pts[ix].x < left)
	    left = self->pts[ix].x;
	if (self->pts[ix].x > right)
	    right = self->pts[ix].x;
	if (self->pts[ix].y < top)
	    top = self->pts[ix].y;
	if (self->pts[ix].y > bot)
	    bot = self->pts[ix].y;
	figoplin_SetHandle(self, ix+4, basex + self->pts[ix].x, basey + self->pts[ix].y);
    }
    bleft = (self->fliph) ? 1 : 0;
    btop = (self->flipv) ? 2 : 0;
    figoplin_SetHandle(self, bleft+btop, basex+left, basey+top);
    figoplin_SetHandle(self, (1-bleft)+btop, basex+right, basey+top);
    figoplin_SetHandle(self, bleft+(2-btop), basex+left, basey+bot);
    figoplin_SetHandle(self, (1-bleft)+(2-btop), basex+right, basey+bot);
    wid = right - left;
    hgt = bot - top;

    lwid = figattr_GetLineWidth(figoplin_GetVAttributes(self), figoplin_GetIVAttributes(self));
    lwid = lwid*figview_FigUPerPix;

    figoplin_SetBoundsRect(self, left+basex-lwid/2, top+basey-lwid/2, wid+lwid+1, hgt+lwid+1);

    figoplin_ComputeSelectedBounds(self);

    figoplin_UpdateParentBounds(self);
}

static enum figobj_HandleType handletypes[4]={
    figobj_ULCorner,
    figobj_URCorner,
    figobj_LLCorner,
    figobj_LRCorner
};

enum figobj_HandleType figoplin__GetHandleType(self, num)
struct figoplin *self;
long num;
{
    if(num>=0 && num<=3) return handletypes[num];
    else return figobj_None;
}

static long canonical_poly[] = {
    0, 3, figobj_NULLREF
};
static long canonical_line[] = {
    4, 5, figobj_NULLREF
};

long *figoplin__GetCanonicalHandles(self)
struct figoplin *self;
{
    if (figoplin_NumPts(self)==2)
	return canonical_line;
    else
	return canonical_poly;
}

void figoplin__Draw(self, v) 
struct figoplin *self;
struct figview *v;
{
    long basex, basey;
    long ix, shad, lw;
    char *col;

    figview_SetTransferMode(v, graphic_COPY);

    col = figattr_GetColor(figoplin_GetVAttributes(self), figoplin_GetIVAttributes(self));
    figview_SetForegroundColor(v, col, 0, 0, 0); 

    figview_SetLineJoin(v, graphic_JoinBevel);
    lw = figattr_GetLineWidth(figoplin_GetVAttributes(self), figoplin_GetIVAttributes(self));
    lw = figview_ToPixW(v, lw*figview_FigUPerPix);
    if (lw <= 0) lw = 1;
    if (lw != 1)
	figview_SetLineWidth(v, lw);

    basex = self->pts[0].x;
    basey = self->pts[0].y;

    ptemp[0].x = figview_ToPixX(v, basex);
    ptemp[0].y = figview_ToPixY(v, basey);

    if (self->numpts < 2) {
	figview_MoveTo(v, ptemp[0].x, ptemp[0].y);
	figview_DrawLineTo(v, ptemp[0].x+1, ptemp[0].y+1);
    }
    else {

	for (ix=1; ix<self->numpts; ix++) {
	    ptemp[ix].x = figview_ToPixX(v, basex+self->pts[ix].x);
	    ptemp[ix].y = figview_ToPixY(v, basey+self->pts[ix].y);
	}
	if (self->closed) {
	    shad = figattr_GetShade(figoplin_GetVAttributes(self), figoplin_GetIVAttributes(self));
	    if (shad != figattr_ShadeClear)
		figview_FillPolygon(v, ptemp, self->numpts, figview_GrayPattern(v, shad, figattr_ShadeDenom));
	}

	if (!figoplin_Closed(self)) {
	    figview_DrawPath(v, ptemp, self->numpts);
	}
	else {
	    ptemp[self->numpts].x = ptemp[0].x;
	    ptemp[self->numpts].y = ptemp[0].y;
	    figview_DrawPath(v, ptemp, self->numpts+1);
	}
    }

    if (lw != 1)
	figview_SetLineWidth(v, 1);
    figview_SetLineJoin(v, graphic_JoinMiter);
}

void figoplin__Sketch(self, v) 
struct figoplin *self;
struct figview *v;
{
    long x, y, basex, basey;
    long ix;

    if (self->numpts < 2) {
	return;
    }

    basex = self->pts[0].x;
    basey = self->pts[0].y;

    figview_SetTransferMode(v, graphic_INVERT);

    figview_MoveTo(v, figview_ToPixX(v, basex), figview_ToPixY(v, basey));
    for (ix=1; ix<self->numpts; ix++) {
	x = basex + self->pts[ix].x;
	y = basey + self->pts[ix].y;
	figview_DrawLineTo(v, figview_ToPixX(v, x), figview_ToPixY(v, y));
    }
    if (self->closed)
	figview_DrawLineTo(v, figview_ToPixX(v, basex), figview_ToPixY(v, basey));
}

/* sketch all lines touching ptref */
static void PartialSketch(self, v, ptref)
struct figoplin *self;
struct figview *v;
long ptref;
{
    long x, y, basex, basey;

    basex = self->pts[0].x;
    basey = self->pts[0].y;

    figview_SetTransferMode(v, graphic_INVERT);

    if (ptref==0 || ptref==1) {
	figview_MoveTo(v, figview_ToPixX(v, basex), figview_ToPixY(v, basey));
	x = basex + self->pts[1].x;
	y = basey + self->pts[1].y;
	figview_DrawLineTo(v, figview_ToPixX(v, x), figview_ToPixY(v, y));
    }

    if ((ptref==0 || ptref==self->numpts-1) && self->closed) {
	figview_MoveTo(v, figview_ToPixX(v, basex), figview_ToPixY(v, basey));
	x = basex + self->pts[self->numpts-1].x;
	y = basey + self->pts[self->numpts-1].y;
	figview_DrawLineTo(v, figview_ToPixX(v, x), figview_ToPixY(v, y));
    }

    if (ptref>0 && ptref<self->numpts-1) {
	x = basex + self->pts[ptref].x;
	y = basey + self->pts[ptref].y;
	figview_MoveTo(v, figview_ToPixX(v, x), figview_ToPixY(v, y));
	x = basex + self->pts[ptref+1].x;
	y = basey + self->pts[ptref+1].y;
	figview_DrawLineTo(v, figview_ToPixX(v, x), figview_ToPixY(v, y));
    }

    if (ptref>1 && ptref<self->numpts) {
	x = basex + self->pts[ptref].x;
	y = basey + self->pts[ptref].y;
	figview_MoveTo(v, figview_ToPixX(v, x), figview_ToPixY(v, y));
	x = basex + self->pts[ptref-1].x;
	y = basey + self->pts[ptref-1].y;
	figview_DrawLineTo(v, figview_ToPixX(v, x), figview_ToPixY(v, y));
    }
}

void figoplin__Select(self, v)
struct figoplin *self;
struct figview *v;
{
    long ix;
    long x, y;
    struct graphic *BlackPattern;

    if (figoplin_GetHandles(self) && figoplin_GetNumHandles(self)) {
	figview_SetTransferMode(v, graphic_INVERT);
	BlackPattern = figview_BlackPattern(v); /* ### should cache this! */

	for (ix=4; ix<figoplin_GetNumHandles(self); ix++) {
	    x = figview_ToPixX(v, point_X(&(figoplin_GetHandles(self)[ix])));
	    y = figview_ToPixY(v, point_Y(&(figoplin_GetHandles(self)[ix])));
	    figview_FillRectSize(v, x-figview_SpotRad, y-figview_SpotRad, 1+2*figview_SpotRad, 1+2*figview_SpotRad, BlackPattern);
	}

	/* if there are three or more points, draw the box handles as well */
	if (figoplin_GetNumHandles(self)>6)
	    for (ix=0; ix<4; ix++) {
		x = figview_ToPixX(v, point_X(&(figoplin_GetHandles(self)[ix])));
		y = figview_ToPixY(v, point_Y(&(figoplin_GetHandles(self)[ix])));
		figview_DrawRectSize(v, x-figview_SpotRad, y-figview_SpotRad, 2*figview_SpotRad, 2*figview_SpotRad);
	    }
    }
}

/* create a regular polygon, using self->cen{x,y} as the center and (endx, endy) as the [offset] vector. */
static void RegularizePolygon(self, endx, endy)
struct figoplin *self;
long endx, endy;
{
    int ix;
    double radius, divvy, offset;

    SetNumPts(self, self->regular);
    radius = sqrt((double)(endx*endx+endy*endy));
    divvy = TWOPI / (double)(self->regular);
    offset = atan2((double)(endy), (double)(endx));
    self->pts[0].x = self->cenx + (long)(radius * cos(offset));
    self->pts[0].y = self->ceny + (long)(radius * sin(offset));

    for (ix=1; ix<self->regular; ix++) {
	self->pts[ix].x = self->cenx + (long)(radius * cos(offset + divvy * (double)ix)) - self->pts[0].x;
	self->pts[ix].y = self->ceny + (long)(radius * sin(offset + divvy * (double)ix)) - self->pts[0].y;
    }
}

enum figobj_Status figoplin__Build(self, v, action, x, y, clicks)   
struct figoplin *self;
struct figview *v;
enum view_MouseAction action;
long x, y; /* in fig coords */
long clicks;
{
    long px, py, apx, apy;
    int ix;

    if (clicks==0) {
	if (self->numpts<2 || (self->regular>=3 && self->buildstate!=3)) {
	    message_DisplayString(v, 10, "Only one point; object aborted.");
	    return figobj_Failed;
	}
	else {
	    figoplin_SetModified(self);
	    return figobj_Done;
	}
    }

    switch (action) {
	case view_LeftDown:
	    figview_BlockUpdates(v, TRUE);
	    if (self->regular >= 3) {
		if (self->buildstate==0) {
		    self->buildstate = 1;
		    SetNumPts(self, 2);
		    self->cenx = self->pts[0].x = x;
		    self->ceny = self->pts[0].y = y;
		    RegularizePolygon(self, 4, 4);
		    figoplin_RecomputeBounds(self);
		    figoplin_Sketch(self, v);
		}
		else {
		    figoplin_Sketch(self, v);
		    RegularizePolygon(self, x-self->cenx, y-self->ceny);
		    figoplin_RecomputeBounds(self);
		    figoplin_Sketch(self, v);
		}
	    }
	    else {
		if (self->buildstate==0) {
		    self->buildstate = 1;
		    SetNumPts(self, 1);
		    self->pts[0].x = x;
		    self->pts[0].y = y;
		    self->rockx = figview_ToPixX(v, x);
		    self->rocky = figview_ToPixY(v, y);
		    self->lastx = self->rockx;
		    self->lasty = self->rocky;
		    figoplin_RecomputeBounds(self);
		    figview_SetTransferMode(v, graphic_INVERT);
		    figview_MoveTo(v, self->rockx, self->rocky);
		    figview_DrawLineTo(v, self->lastx, self->lasty);
		}
		else {
		    ix = self->numpts-1;
		    if (ix==0) {
			self->rockx = figview_ToPixX(v, self->pts[0].x);
			self->rocky = figview_ToPixY(v, self->pts[0].y);
		    }
		    else {
			self->rockx = figview_ToPixX(v, self->pts[0].x+self->pts[ix].x);
			self->rocky = figview_ToPixY(v, self->pts[0].y+self->pts[ix].y);
		    }
		    self->lastx = figview_ToPixX(v, x);
		    self->lasty = figview_ToPixY(v, y);
		    figview_SetTransferMode(v, graphic_INVERT);
		    figview_MoveTo(v, self->rockx, self->rocky);
		    figview_DrawLineTo(v, self->lastx, self->lasty);
		}
	    }
	    return figobj_NotDone;
	case view_LeftMovement:
	    if (self->regular >= 3) {
		figoplin_Sketch(self, v);
		px = x - self->cenx;
		py = y - self->ceny;
		RegularizePolygon(self, px, py);
		figoplin_RecomputeBounds(self);
		figoplin_Sketch(self, v);
	    }
	    else {
		figview_SetTransferMode(v, graphic_INVERT);
		figview_MoveTo(v, self->rockx, self->rocky);
		figview_DrawLineTo(v, self->lastx, self->lasty);
		self->lastx = figview_ToPixX(v, x);
		self->lasty = figview_ToPixY(v, y);
		figview_MoveTo(v, self->rockx, self->rocky);
		figview_DrawLineTo(v, self->lastx, self->lasty);
	    }
	    return figobj_NotDone;
	case view_LeftUp:
	    figview_BlockUpdates(v, FALSE);
	    if (self->regular >= 3) {
		px = x - self->cenx;
		py = y - self->ceny;
		apx = figview_ToPixW(v, px);
		apy = figview_ToPixH(v, py);
		if (self->buildstate == 1
		    && (apx < figview_MouseHysteresis)
		    && (apy < figview_MouseHysteresis) 
		    && (apx > -figview_MouseHysteresis)
		    && (apy > -figview_MouseHysteresis)) {
		    /* cursor didn't move far enough; we have a point click. */
		    message_DisplayString(v, 10, "Center point placed; use left button for radius end.");
		    self->buildstate=2;
		    return figobj_NotDone;
		}
		else {
		    self->buildstate=3;
		    figoplin_Sketch(self, v);
		    RegularizePolygon(self, x-self->cenx, y-self->ceny);
		    figoplin_RecomputeBounds(self);
		    figoplin_SetModified(self);
		    return figobj_Done;
		}
	    }
	    else {
		px = x - self->pts[0].x;
		py = y - self->pts[0].y;
		apx = figview_ToPixW(v, px);
		apy = figview_ToPixH(v, py);
		if (self->buildstate==1 
		    && (apx < figview_MouseHysteresis)
		    && (apy < figview_MouseHysteresis) 
		    && (apx > -figview_MouseHysteresis)
		    && (apy > -figview_MouseHysteresis)) {
		    /* cursor didn't move far enough; we have a point click. */
		    figview_SetTransferMode(v, graphic_INVERT);
		    figview_MoveTo(v, self->rockx, self->rocky);
		    figview_DrawLineTo(v, self->lastx, self->lasty);
		    figview_MoveTo(v, self->rockx-1, self->rocky-1);
		    figview_DrawLineTo(v, self->rockx+1, self->rocky+1);
		    figview_MoveTo(v, self->rockx+1, self->rocky-1);
		    figview_DrawLineTo(v, self->rockx-1, self->rocky+1);
		    if (!self->shortmode)
			message_DisplayString(v, 10, "First point placed; use left button for more points, right button when done.");
		    else
			message_DisplayString(v, 10, "First point placed; use left button for other end.");
		    self->buildstate=2;
		}
		else {
		    if (self->buildstate==1 && !self->shortmode) {
			message_DisplayString(v, 10, "2 points placed; use left button for more points, right button when done.");
			self->buildstate=2;
		    }
		    ix = self->numpts;
		    SetNumPts(self, ix+1);
		    self->pts[ix].x = px;
		    self->pts[ix].y = py;
		    figoplin_RecomputeBounds(self);
		}
		if (self->shortmode && self->numpts==2) {
		    figoplin_SetModified(self);
		    return figobj_Done;
		} else
		    return figobj_NotDone;
	    }
	case view_RightDown:
	case view_RightMovement:
	case view_RightUp:
	    if (self->numpts<2 || (self->regular>=3 && self->buildstate!=3)) {
		message_DisplayString(v, 10, "Only one point; object aborted.");
		return figobj_Failed;
	    }
	    figoplin_SetModified(self);
	    return figobj_Done;
	default:
	    return figobj_Failed;
    }
}

/* return the ptref of the first point on the line segment within delta of (x, y). This will be in [0, numpts-1] if the polygon is closed, in [0, numpts-2] if it's open. If no segment is found, return figobj_NULLREF. */
static int FindLineHit(self, x, y, delta)
struct figoplin *self;
long x, y;
long delta;
{

#define IABS(v) (((v) < 0) ? (-(v)) : (v))
    int ix;
    long val, basex, basey, x0, y0, x1, y1;

    basex = self->pts[0].x;
    basey = self->pts[0].y;
    x0 = basex;
    y0 = basey;
    for (ix=1; ix<=self->numpts; ix++) {
	if (ix==self->numpts) {
	    if (!self->closed)
		continue;
	    x1 = basex;
	    y1 = basey;
	}
	else {
	    x1 = self->pts[ix].x + basex;
	    y1 = self->pts[ix].y + basey;
	}

	if (x1-x0 == 0 && y1-y0 == 0) {
	    /* forget it */
	}
	else if (IABS(x1-x0) > IABS(y1-y0)) {
	    val = y0 + ((x-x0) * (y1-y0)) / (x1-x0);
	    if (!((x<x0 && x<x1) || (x>x0 && x>x1)) && IABS(y-val) <= delta) {
		return ix-1;	
	    }
	}
	else {
	    val = x0 + ((y-y0) * (x1-x0)) / (y1-y0);
	    if (!((y<y0 && y<y1) || (y>y0 && y>y1)) && IABS(x-val) <= delta) {
		return ix-1;	
	    }
	}

	x0 = x1;
	y0 = y1;
    }

    return figobj_NULLREF;
}

enum figobj_HitVal figoplin__HitMe(self, x, y, delta, ptref) 
struct figoplin *self;
long x, y;
long delta;
long *ptref;
{
    int ix;
    enum figobj_HitVal res = figoplin_BasicHitMe(self, x, y, delta, ptref);

    if (res!=figobj_HitInside)
	return res;

    /* click hit bbox but no handles */
    ix = FindLineHit(self, x, y, delta);
    if (ix!=figobj_NULLREF) {
	if (ptref)
	    *ptref = ix;
	return figobj_HitBody;
    }

    return figobj_Miss;
}

/* basic procedure to move a handle -- used by figoplin__MoveHandle(), figoplin__Reshape() */
static void MoveHandle(self, x, y, ptref)
struct figoplin *self;
long x, y, ptref;
{
    long ix;
    long noffx, noffy, offx, offy;

    if (ptref<0 || ptref>=self->numpts+4)
	return;

    if (ptref<4) {
	long oldwidth, oldheight, newwidth, newheight;
	long nbasex, nbasey;

	if (!self->orpts) {
	    self->orpts = (struct point *)malloc(sizeof(struct point) * self->numpts);
	    for (ix=0; ix<self->numpts; ix++)
		self->orpts[ix] = self->pts[ix];
	    for (ix=0; ix<4; ix++)
		self->orhandles[ix] = figoplin_GetHandles(self)[ix];
	}

	offx = self->orhandles[3-ptref].x;
	offy = self->orhandles[3-ptref].y;
	noffx = figoplin_GetHandles(self)[3-ptref].x;
	noffy = figoplin_GetHandles(self)[3-ptref].y;
	oldwidth = self->orhandles[ptref].x - offx;
	oldheight = self->orhandles[ptref].y - offy;
	newwidth = x - noffx;
	newheight = y - noffy;
	switch (ptref) {
	    case 0:
		self->fliph = !(newwidth<0);
		self->flipv = !(newheight<0);
		break;
	    case 1:
		self->fliph = (newwidth<0);
		self->flipv = !(newheight<0);
		break;
	    case 2:
		self->fliph = !(newwidth<0);
		self->flipv = (newheight<0);
		break;
	    case 3:
		self->fliph = (newwidth<0);
		self->flipv = (newheight<0);
		break;
	}
	if (oldwidth==0) {
	    oldwidth=1; 
	    newwidth=0;
	}
	if (oldheight==0) {
	    oldheight=1; 
	    newheight=0;
	}

	nbasex = ((self->orpts[0].x-offx) * newwidth) / oldwidth + noffx;
	nbasey = ((self->orpts[0].y-offy) * newheight) / oldheight + noffy;
	self->pts[0].x = nbasex;
	self->pts[0].y = nbasey;
	for (ix=1; ix<self->numpts; ix++) {
	    self->pts[ix].x = ((self->orpts[0].x+self->orpts[ix].x-offx) * newwidth) / oldwidth + noffx - nbasex;
	    self->pts[ix].y = ((self->orpts[0].y+self->orpts[ix].y-offy) * newheight) / oldheight + noffy - nbasey;
	}
    }
    else {
	ClearOldPoints(self);
	ptref -= 4;
	if (ptref==0) {
	    offx = x - self->pts[0].x;
	    offy = y - self->pts[0].y;
	    self->pts[0].x += offx;
	    self->pts[0].y += offy;
	    for (ix=1; ix<self->numpts; ix++) {
		self->pts[ix].x -= offx;
		self->pts[ix].y -= offy;
	    }
	}
	else {
	    self->pts[ptref].x = x - self->pts[0].x;
	    self->pts[ptref].y = y - self->pts[0].y;
	}
    }
}

boolean figoplin__Reshape(self, action, v, x, y, handle, ptref)
struct figoplin *self;
enum view_MouseAction action;
struct figview *v;
boolean handle; 
long x, y, ptref;
{
    if (!handle)
	return FALSE;

    if (ptref>=0 && ptref<4) {
	switch (action) {
	    case view_LeftDown:
	    case view_RightDown:
		if (figoplin_GetReadOnly(self))
		    return FALSE;
		if (self->stickysketch)
		    figoplin_Sketch(self, v);
		break;
	    case view_LeftMovement:
	    case view_RightMovement:
		figoplin_Sketch(self, v);
		MoveHandle(self, x, y, ptref);
		figoplin_RecomputeBounds(self);
		figoplin_Sketch(self, v);
		break;
	    case view_LeftUp:
	    case view_RightUp:
		figoplin_Sketch(self, v);
		MoveHandle(self, x, y, ptref);
		figoplin_RecomputeBounds(self);
		figoplin_SetModified(self);
		break;
	}
	return TRUE;
    }
    else {
	switch (action) {
	    case view_LeftDown:
	    case view_RightDown:
		if (figoplin_GetReadOnly(self))
		    return FALSE;
		if (self->stickysketch)
		    PartialSketch(self, v, ptref-4);
		break;
	    case view_LeftMovement:
	    case view_RightMovement:
		PartialSketch(self, v, ptref-4);
		MoveHandle(self, x, y, ptref);
		PartialSketch(self, v, ptref-4);
		break;
	    case view_LeftUp:
	    case view_RightUp:
		PartialSketch(self, v, ptref-4);
		MoveHandle(self, x, y, ptref);
		figoplin_RecomputeBounds(self);
		figoplin_SetModified(self);
		break;
	}
	return TRUE;
    }
}

void figoplin__MoveHandle(self, x, y, ptref)
struct figoplin *self;
long x, y, ptref;
{
    if (figoplin_GetReadOnly(self))
	return;
    MoveHandle(self, x, y, ptref);
    figoplin_SetModified(self);
    figoplin_RecomputeBounds(self);
}

boolean figoplin__AddParts(self, action, v, x, y, handle, ptref)
struct figoplin *self;
enum view_MouseAction action;
struct figview *v;
boolean handle; 
long x, y, ptref;
{
    int ix;
    long offx, offy;

    if (ptref>=0 && handle) {
	if (ptref<4)
	    return FALSE;
	else
	    ptref -= 4;
    }

    switch (action) {
	case view_LeftDown:
	case view_RightDown:
	    if (figoplin_GetReadOnly(self))
		return FALSE;
	    if (ptref==0) {
		x = self->pts[0].x;
		y = self->pts[0].y;
	    }
	    else {
		x = self->pts[ptref].x + self->pts[0].x;
		y = self->pts[ptref].y + self->pts[0].y;
	    }
	    if (ptref == 0 && handle == TRUE) {
		SetNumPts(self, self->numpts+1);
		offx = self->pts[0].x - x;
		offy = self->pts[0].y - y;
		for (ix=self->numpts-2; ix>=1; ix--) {
		    self->pts[ix+1].x = self->pts[ix].x + offx;
		    self->pts[ix+1].y = self->pts[ix].y + offy;
		}
		self->pts[1].x = offx;
		self->pts[1].y = offy;
		self->pts[0].x -= offx;
		self->pts[0].y -= offy;
		figoplin_RecomputeBounds(self);
		self->rock = 4;
		return figoplin_Reshape(self, action, v, x, y, TRUE, self->rock);
	    }
	    else {
		SetNumPts(self, self->numpts+1);
		for (ix=self->numpts-2; ix>ptref; ix--) {
		    self->pts[ix+1].x = self->pts[ix].x;
		    self->pts[ix+1].y = self->pts[ix].y;
		}
		self->pts[ptref+1].x = x - self->pts[0].x;
		self->pts[ptref+1].y = y - self->pts[0].y;
		figoplin_RecomputeBounds(self);
		self->rock = 4+ptref+1;
		return figoplin_Reshape(self, action, v, x, y, TRUE, self->rock);
	    }
	case view_LeftMovement:
	case view_RightMovement:
	case view_LeftUp:
	case view_RightUp:
	    return figoplin_Reshape(self, action, v, x, y, TRUE, self->rock);
    }
}

boolean figoplin__DeleteParts(self, action, v, x, y, handle, ptref)
struct figoplin *self;
enum view_MouseAction action;
struct figview *v;
boolean handle; 
long x, y, ptref;
{
    int ix;
    long offx, offy;

    if (!handle || figoplin_GetReadOnly(self))
	return FALSE;

    if (ptref>=0) {
	if (ptref<4)
	    return FALSE;
	else
	    ptref -= 4;
    }

    if (ptref == figobj_NULLREF)
	return FALSE;
    if (self->numpts <= 2) {
	message_DisplayString(v, 10, "There are only two points left.");
	return FALSE;
    };

    switch (action) {
	case view_LeftDown:
	case view_RightDown:
	case view_LeftMovement:
	case view_RightMovement:
	    break;
	case view_LeftUp:
	case view_RightUp:
	    if (ptref==0) {
		offx = self->pts[1].x;
		offy = self->pts[1].y;
		self->pts[0].x += offx;
		self->pts[0].y += offy;
		for (ix=2; ix<self->numpts; ix++) {
		    self->pts[ix-1].x = self->pts[ix].x - offx;
		    self->pts[ix-1].y = self->pts[ix].y - offy;
		}
		SetNumPts(self, self->numpts-1);	
	    }
	    else {
		for (ix=ptref+1; ix<self->numpts; ix++) {
		    self->pts[ix-1].x = self->pts[ix].x;
		    self->pts[ix-1].y = self->pts[ix].y;
		}
		SetNumPts(self, self->numpts-1);
	    }
	    figoplin_RecomputeBounds(self);
	    figoplin_SetModified(self);
	    break;
    }
    return TRUE;
}

void figoplin__Reposition(self, xd, yd)
struct figoplin *self;
long xd, yd;
{
    if (figoplin_GetReadOnly(self))
	return;
    self->pts[0].x += xd;
    self->pts[0].y += yd;
    figoplin_RecomputeBounds(self);
    figoplin_SetModified(self);
}

void figoplin__InheritVAttributes(self, attr, mask)
struct figoplin *self;
struct figattr *attr;
unsigned long mask;
{
    super_InheritVAttributes(self, attr, mask);

    if (mask & (~(figoplin_GetVAttributes(self)->active)) & (1<<figattr_LineWidth)) {
	figoplin_RecomputeBounds(self);
    }
}

unsigned long figoplin__UpdateVAttributes(self, attr, mask)
struct figoplin *self;
struct figattr *attr;
unsigned long mask;
{
    mask = super_UpdateVAttributes(self, attr, mask);
    
    if (mask & (1<<figattr_LineWidth))
	figoplin_RecomputeBounds(self);

    return mask;
}

void figoplin__WriteBody(self, fp)
struct figoplin *self;
FILE *fp;
{
   /* we don't call super_WriteBody() because that just outputs PosX and PosY, which aren't being used */
    int ix;

    fprintf(fp, "$$ %d %d %d %d\n", self->closed, self->numpts, self->fliph, self->flipv);
    for (ix=0; ix<self->numpts; ix++)
	fprintf(fp, "$ %d %d\n", self->pts[ix].x, self->pts[ix].y);
}

long figoplin__ReadBody(self, fp, recompute)
struct figoplin *self;
FILE *fp;
boolean recompute;
{
    int	ix, jx; 
    long num, num2, xp, yp;
    long numh, numv;

#define LINELENGTH (250)
    char buf[LINELENGTH+1];

    if (fgets(buf, LINELENGTH, fp) == NULL)
	return dataobject_PREMATUREEOF;
    ix = sscanf(buf, "$$ %ld %ld %ld %ld", &num2, &num, &numh, &numv);
    if (ix!=4) return dataobject_BADFORMAT;

    self->closed = (boolean)num2;
    if (self->closed)
	figoplin_AttributesUsed(self) |= (1<<figattr_Shade);
    self->fliph = numh;
    self->flipv = numv;
    self->numpts = num;
    SetNumPts(self, num);

    for (jx=0; jx<self->numpts; jx++) {
	if (fgets(buf, LINELENGTH, fp) == NULL)
	    return dataobject_PREMATUREEOF;
	ix = sscanf(buf, "$ %ld %ld", &xp, &yp);
	if (ix!=2) return dataobject_BADFORMAT;
	self->pts[jx].x = xp;
	self->pts[jx].y = yp;
    }

    if (recompute) {
	figoplin_RecomputeBounds(self);
	figoplin_SetModified(self);
    }

    return dataobject_NOREADERROR;
}

#define FadeColor(col, shad)  (1.0 - (1.0-(shad)) * (1.0-(col)))

void figoplin__PrintObject(self, v, file, prefix)
struct figoplin *self;
struct figview *v;
FILE *file;
char *prefix;
{
    long ix, x, y, xbase, ybase, nump;
    struct point *pts;
    long lw, shad;
    char *col;
    double rcol, bcol, gcol, shadcol;

    fprintf(file, "%s  %% polyline\n", prefix);

    nump = figoplin_NumPts(self);
    pts = figoplin_Pts(self);

    if (nump < 2)
	return;

    xbase = pts[0].x;
    ybase = pts[0].y;
    
    fprintf(file, "%s  2 setlinejoin\n", prefix);
    fprintf(file, "%s  %d %d moveto\n", prefix, figview_ToPrintPixX(v, xbase), figview_ToPrintPixY(v, ybase));
    for (ix=1; ix<nump; ix++) {
	x = pts[ix].x + xbase;
	y = pts[ix].y + ybase;
	fprintf(file, "%s  %d %d lineto\n", prefix, figview_ToPrintPixX(v, x), figview_ToPrintPixY(v, y));
    }

    col = figattr_GetColor(figoplin_GetVAttributes(self), figoplin_GetIVAttributes(self));
    print_LookUpColor(col, &rcol, &gcol, &bcol);

    if (figoplin_Closed(self)) {
	fprintf(file, "%s  closepath\n", prefix);

	shad = figattr_GetShade(figoplin_GetVAttributes(self), figoplin_GetIVAttributes(self));
	if (shad != figattr_ShadeClear) {
	    fprintf(file, "%s  gsave\n", prefix);
	    shadcol = (double)(figattr_ShadeDenom-shad) / (double)figattr_ShadeDenom;
	    fprintf(file, "%s  %f %f %f setrgbcolor\n", prefix, FadeColor(rcol, shadcol), FadeColor(gcol, shadcol), FadeColor(bcol, shadcol));
	    /*fprintf(file, "%s  %f setgray\n", prefix, shadcol);*/
	    fprintf(file, "%s  eofill grestore\n", prefix);
	}
    }

    lw = figattr_GetLineWidth(figoplin_GetVAttributes(self), figoplin_GetIVAttributes(self));
    lw = figview_ToPrintPixW(v, lw*figview_FigUPerPix);
    if (lw <= 0) lw = 0;
    fprintf(file, "%s  %d setlinewidth\n", prefix, lw);
    fprintf(file, "%s  %f %f %f setrgbcolor\n", prefix, rcol, gcol, bcol);
    /*fprintf(file, "%s  0 setgray\n", prefix);*/
    fprintf(file, "%s  stroke\n", prefix);
}

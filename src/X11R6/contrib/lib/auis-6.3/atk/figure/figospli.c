/* figospli.c - fig element object: spline */
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
char *figospli_c_rcsid = "$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/figure/RCS/figospli.c,v 1.4 1993/01/28 19:27:46 Zarf Exp $";
#endif

#include <figospli.eh>

#include <figoplin.ih>
#include <figattr.ih>
#include <view.ih>
#include <figv.ih>
#include <figtoolv.ih>
#include <message.ih>
#include <print.ih>

#define figospli_Segments (8)

struct figospli_temp {
    double t1, t2, t3, t4;
    double ss;
};

static struct figospli_temp *ctemp;
static int ctemp_size;
static struct point *ptemp;
static int ptemp_size;

boolean figospli__InitializeClass(ClassID)
struct classhdr *ClassID;
{
    ctemp_size = 0;
    ctemp = NULL;
    ptemp_size = 0;
    ptemp = NULL;
    return TRUE;
}

boolean figospli__InitializeObject(ClassID, self)
struct classhdr *ClassID;
struct figospli *self;
{
    self->cubit = NULL;
    self->cubit_size = 0;
    ((struct figoplin *)self)->stickysketch = TRUE;
    self->tmppts = NULL;
    self->tmppts_size = 0;
    return TRUE;
}

/* efficient? well, no. */
struct figospli *figospli__Create(classID, pointlist, numpoints, isclosed)
struct classheader *classID;
struct point *pointlist;
long numpoints;
boolean isclosed;
{
    struct figoplin *tmp = figoplin_Create(pointlist, numpoints, isclosed);
    struct figospli *res;

    if (!tmp) return NULL;
    res = figospli_New();
    if (!res) return NULL;

    figospli_CopyData(res, tmp);
    figoplin_Destroy(tmp);

    return res;
}

void figospli__FinalizeObject(ClassID, self)
struct classhdr *ClassID;
struct figospli *self;
{
    if (self->cubit)
	free(self->cubit);
    if (self->tmppts)
	free(self->tmppts);
}

char *figospli__ToolName(dummy, v, rock)
struct figospli *dummy;
struct figtoolview *v;
long rock;
{
    if (rock & 2)
	return "Closed Spline";
    else
	return "Spline";
}

static void spline(pts, n, cubit, closed)
struct point *pts;
int n;
struct figospli_cubit *cubit;
boolean closed; /* if TRUE, pts[n] doesn't exist; use pts[0] instead. */
{
    /* uses pts[0...n], ctemp[0...n], cubit[0...n-1] */
    int i;
    double dy1, dy2;
    int first, last;

    /* do y coords */
    dy1 = (pts[1].y) * 6.0;
    for (i=0; i<n-1; i++) {
	if (closed && i+2==n)
	    dy2 = (-pts[i+1].y) * 6.0;
	else
	    dy2 = (pts[i+2].y - pts[i+1].y) * 6.0;
	ctemp[i+1].t1 = 1.0;
	ctemp[i+1].t2 = 4.0;
	ctemp[i+1].t3 = 1.0;
	ctemp[i+1].t4 = dy2 - dy1;
	dy1 = dy2;
    }
    first = 1;
    last = n-2;


    for (i=first; i<=last; i++) {
	ctemp[i+1].t1 /= ctemp[i].t2;
	ctemp[i+1].t2 -= (ctemp[i+1].t1 * ctemp[i].t3);
	ctemp[i+1].t4 -= (ctemp[i+1].t1 * ctemp[i].t4);
    }

    ctemp[last+1].t4 /= ctemp[last+1].t2;
    for (i=last-1; i>=first-1; i--) {
	ctemp[i+1].t4 = (ctemp[i+1].t4 - ctemp[i+1].t3*ctemp[i+2].t4) / ctemp[i+1].t2;
    }

    for (i=first-1; i<=last; i++) {
	ctemp[i+1].ss = ctemp[i+1].t4;
    };

    if (!closed || n==1) {
	ctemp[0].ss = 0.0;
	ctemp[n].ss = 0.0;
    }
    else {
	double lhs = pts[1].y - (-pts[n-1].y) - (ctemp[1].ss + ctemp[n-1].ss)/6.0;
	ctemp[0].ss = 1.5 * lhs;
	ctemp[n].ss = 1.5 * lhs;
    }

    for (i=0; i<n; i++) {  
	cubit[i].ya = (ctemp[i+1].ss - ctemp[i].ss) / (6.0);
	cubit[i].yb = ctemp[i].ss / 2.0; 
	if (i==0) {
	    if (closed && i+1==n)
		cubit[i].yc = 0 - (2.0*ctemp[i].ss + ctemp[i+1].ss) / 6.0;
	    else
		cubit[i].yc = (pts[1].y) - (2.0*ctemp[i].ss + ctemp[i+1].ss) / 6.0;
	    cubit[i].yd = pts[0].y;
	}
	else {
	    if (closed && i+1==n)
		cubit[i].yc = (-pts[i].y) - (2.0*ctemp[i].ss + ctemp[i+1].ss) / 6.0;
	    else
		cubit[i].yc = (pts[i+1].y - pts[i].y) - (2.0*ctemp[i].ss + ctemp[i+1].ss) / 6.0;
	    cubit[i].yd = pts[0].y + pts[i].y;
	}
    }

    /* do x coords */
    dy1 = (pts[1].x) * 6.0;
    for (i=0; i<n-1; i++) {
	if (closed && i+2==n)
	    dy2 = (-pts[i+1].x) * 6.0;
	else
	    dy2 = (pts[i+2].x - pts[i+1].x) * 6.0;
	ctemp[i+1].t1 = 1.0;
	ctemp[i+1].t2 = 4.0;
	ctemp[i+1].t3 = 1.0;
	ctemp[i+1].t4 = dy2 - dy1;
	dy1 = dy2;
    }
    first = 1;
    last = n-2;

    for (i=first; i<=last; i++) {
	ctemp[i+1].t1 /= ctemp[i].t2;
	ctemp[i+1].t2 -= (ctemp[i+1].t1 * ctemp[i].t3);
	ctemp[i+1].t4 -= (ctemp[i+1].t1 * ctemp[i].t4);
    }

    ctemp[last+1].t4 /= ctemp[last+1].t2;
    for (i=last-1; i>=first-1; i--) {
	ctemp[i+1].t4 = (ctemp[i+1].t4 - ctemp[i+1].t3*ctemp[i+2].t4) / ctemp[i+1].t2;
    }

    for (i=first-1; i<=last; i++) {
	ctemp[i+1].ss = ctemp[i+1].t4;
    };

    if (!closed || n==1) {
	ctemp[0].ss = 0.0;
	ctemp[n].ss = 0.0;
    }
    else {
	double lhs = pts[1].x - (-pts[n-1].x) - (ctemp[1].ss + ctemp[n-1].ss)/6.0;
	ctemp[0].ss = 1.5 * lhs;
	ctemp[n].ss = 1.5 * lhs;
    }

    for (i=0; i<n; i++) {  
	cubit[i].xa = (ctemp[i+1].ss - ctemp[i].ss) / (6.0);
	cubit[i].xb = ctemp[i].ss / 2.0; 
	if (i==0) {
	    if (closed && i+1==n)
		cubit[i].xc = 0 - (2.0*ctemp[i].ss + ctemp[i+1].ss) / 6.0;
	    else
		cubit[i].xc = (pts[1].x) - (2.0*ctemp[i].ss + ctemp[i+1].ss) / 6.0;
	    cubit[i].xd = pts[0].x;
	}
	else {
	    if (closed && i+1==n)
		cubit[i].xc = (-pts[i].x) - (2.0*ctemp[i].ss + ctemp[i+1].ss) / 6.0;
	    else   
		cubit[i].xc = (pts[i+1].x - pts[i].x) - (2.0*ctemp[i].ss + ctemp[i+1].ss) / 6.0;
	    cubit[i].xd = pts[0].x + pts[i].x;
	}
    }
}

static void SetNumCubits(self, inum)
struct figospli *self;
long inum;
{
    int num;

    num = inum;
    if (num > self->cubit_size) {
	if (self->cubit_size == 0) {
	    self->cubit_size = num;
	    self->cubit = (struct figospli_cubit *)malloc(self->cubit_size * sizeof(struct figospli_cubit));
	}
	else {
	    while (num > self->cubit_size)
		self->cubit_size *= 2;
	    self->cubit = (struct figospli_cubit *)realloc(self->cubit, self->cubit_size * sizeof(struct figospli_cubit));
	}
    }

    num = inum+2;
    if (num > ctemp_size) {
	if (ctemp_size == 0) {
	    ctemp_size = num;
	    ctemp = (struct figospli_temp *)malloc(ctemp_size * sizeof(struct figospli_temp));
	}
	else {
	    while (num > ctemp_size)
		ctemp_size *= 2;
	    ctemp = (struct figospli_temp *)realloc(ctemp, ctemp_size * sizeof(struct figospli_temp));
	}
    }

    num = 1 + inum * figospli_Segments;
    if (num > self->tmppts_size) {
	if (self->tmppts_size == 0) {
	    self->tmppts_size = num;
	    self->tmppts = (struct point *)malloc(self->tmppts_size * sizeof(struct point));
	}
	else {
	    while (num > self->tmppts_size)
		self->tmppts_size *= 2;
	    self->tmppts = (struct point *)realloc(self->tmppts, self->tmppts_size * sizeof(struct point));
	}
    }
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

void figospli__RecomputeBounds(self)
struct figospli *self;
{
    int ix, jx, realnum, num, lwid;
    long px, py;
    long xmin, xmax, ymin, ymax, wid, hgt;
    double t;
    boolean closed = figospli_Closed(self);

    super_RecomputeBounds(self);

    realnum = figospli_NumPts(self);

    if (realnum==0 || !figospli_Pts(self)) {
	fprintf(stderr, "figospli: impossible RecomputeBounds()\n");
	return;
    }

    if (closed) 
	num = realnum+1;
    else
	num = realnum;

    SetNumCubits(self, num); 

    if (num>1) {
	spline(figospli_Pts(self), num-1, self->cubit, closed);

	if (closed) {
	    xmin = xmax = figospli_Pts(self)[0].x;
	    ymin = ymax = figospli_Pts(self)[0].y;
	}
	else {
	    xmin = xmax = figospli_Pts(self)[num-1].x + figospli_Pts(self)[0].x;
	    ymin = ymax = figospli_Pts(self)[num-1].y + figospli_Pts(self)[0].y;
	}
    }
    else {
	xmin = xmax = figospli_Pts(self)[0].x;
	ymin = ymax = figospli_Pts(self)[0].y;
    }

    self->tmppts[(num-1)*figospli_Segments].x = xmin;
    self->tmppts[(num-1)*figospli_Segments].y = ymin;

    for (ix=0; ix<num-1; ix++) {
	struct figospli_cubit *cb = &(self->cubit[ix]);
	for (t=0.0, jx=0; 
	     jx<figospli_Segments; 
	     jx++, t += (1.0 / (double)figospli_Segments)) {
	    px = (long)(cb->xa*t*t*t + cb->xb*t*t + cb->xc*t + cb->xd);
	    py = (long)(cb->ya*t*t*t + cb->yb*t*t + cb->yc*t + cb->yd);
	    self->tmppts[ix*figospli_Segments + jx].x = px;
	    self->tmppts[ix*figospli_Segments + jx].y = py;
	    if (px<xmin) xmin = px;
	    if (px>xmax) xmax = px;
	    if (py<ymin) ymin = py;
	    if (py>ymax) ymax = py;
	}
    }

    wid = xmax - xmin;
    hgt = ymax - ymin;

    lwid = figattr_GetLineWidth(figospli_GetVAttributes(self), figospli_GetIVAttributes(self));
    lwid = lwid*figview_FigUPerPix;

    figospli_SetBoundsRect(self, xmin-lwid/2, ymin-lwid/2, wid+lwid+1, hgt+lwid+1);
    figospli_ComputeSelectedBounds(self); /* ### inefficient -- this gets called twice, once in super_ and once here. */
}

void figospli__Draw(self, v) 
struct figospli *self;
struct figview *v;
{
    long ix, shad, lw;
    char *col;
    int num = figospli_NumPts(self);
    if (figospli_Closed(self)) {
	if (num<2) return;
	num++;
    }

    figview_SetTransferMode(v, graphic_COPY);

    col = figattr_GetColor(figospli_GetVAttributes(self), figospli_GetIVAttributes(self));
    figview_SetForegroundColor(v, col, 0, 0, 0); 

    figview_SetLineJoin(v, graphic_JoinBevel);
    lw = figattr_GetLineWidth(figospli_GetVAttributes(self), figospli_GetIVAttributes(self));
    lw = figview_ToPixW(v, lw*figview_FigUPerPix);
    if (lw <= 0) lw = 1;
    if (lw != 1)
	figview_SetLineWidth(v, lw);

    for (ix=0; ix<figospli_Segments*(num-1)+1; ix++) {
	ptemp[ix].x = figview_ToPixX(v,	self->tmppts[ix].x);	
	ptemp[ix].y = figview_ToPixY(v, self->tmppts[ix].y);
    }

    if (figospli_Closed(self)) {
	shad = figattr_GetShade(figospli_GetVAttributes(self), figospli_GetIVAttributes(self));
	if (shad != figattr_ShadeClear) {
	    figview_FillPolygon(v, ptemp, figospli_Segments*(num-1)+1, figview_GrayPattern(v, shad, figattr_ShadeDenom));
	}
    }

    figview_DrawPath(v, ptemp, figospli_Segments*(num-1)+1);

    if (lw != 1)
	figview_SetLineWidth(v, 1);
    figview_SetLineJoin(v, graphic_JoinMiter);
}

static int FindLineHit(self, x, y, delta)
struct figospli *self;
long x, y;
long delta;
{
    int ix;
    long x0, y0, x1, y1;
    int num = figospli_NumPts(self);
    if (figospli_Closed(self))
	num++;

    for (ix=0; ix<figospli_Segments*(num-1); ix++) {
	if (self->tmppts[ix].x < self->tmppts[ix+1].x) {
	    x0 = self->tmppts[ix].x;
	    x1 = self->tmppts[ix+1].x;
	}
	else {
	    x1 = self->tmppts[ix].x;
	    x0 = self->tmppts[ix+1].x;
	}
	if (self->tmppts[ix].y < self->tmppts[ix+1].y) {
	    y0 = self->tmppts[ix].y;
	    y1 = self->tmppts[ix+1].y;
	}
	else {
	    y1 = self->tmppts[ix].y;
	    y0 = self->tmppts[ix+1].y;
	}
	if (x>=x0-delta && x<=x1+delta && y>=y0-delta && y<=y1+delta) {
	    return ix / figospli_Segments;
	}
    }
    return figobj_NULLREF;
}

enum figobj_HitVal figospli__HitMe(self, x, y, delta, ptref) 
struct figospli *self;
long x, y;
long delta;
long *ptref;
{
    int ix;
    enum figobj_HitVal res = figospli_BasicHitMe(self, x, y, delta, ptref);

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

#define FadeColor(col, shad)  (1.0 - (1.0-(shad)) * (1.0-(col)))

/* notes: from the PostScript Red Book,
to draw a*t^3 + b*t^2 + c*t + d  (t from 0 to 1), we must do
(dx, dy) moveto
  and then let
P = (dx+cx/3, dy+cy/3) 
Q = (Px + (cx+bx)/3, Py + (cy+by)/3)
R = (dx+cx+bx+ax, dy+cy+by+ay)
  and then
P Q R curveto
*/
void figospli__PrintObject(self, v, file, prefix)
struct figospli *self;
struct figview *v;
FILE *file;
char *prefix;
{
    long ix, x, y, xbase, ybase, nump;
    struct point *pts;
    long lw, shad;
    char *col;
    struct figospli_cubit *cb;
    long Px, Py, Qx, Qy, Rx, Ry;
    double rcol, bcol, gcol, shadcol;

    fprintf(file, "%s  %% polyline\n", prefix);

    nump = figospli_NumPts(self);
    pts = figospli_Pts(self);

    if (figospli_Closed(self))
	nump++;
    if (nump < 2)
	return;

    xbase = pts[0].x;
    ybase = pts[0].y;
    
    fprintf(file, "%s  2 setlinejoin\n", prefix);
    
    fprintf(file, "%s  %d %d moveto\n", prefix, figview_ToPrintPixX(v, xbase), figview_ToPrintPixY(v, ybase));

    for (ix=0; ix<nump-1; ix++) {
	cb = &(self->cubit[ix]);
	Px = cb->xd + cb->xc/3;
	Py = cb->yd + cb->yc/3;
	Qx = Px + (cb->xc + cb->xb)/3;
	Qy = Py + (cb->yc + cb->yb)/3;
	Rx = (cb->xa + cb->xb + cb->xc + cb->xd);
	Ry = (cb->ya + cb->yb + cb->yc + cb->yd);
	fprintf(file, "%s  %d %d %d %d %d %d curveto\n", prefix, figview_ToPrintPixX(v, Px), figview_ToPrintPixY(v, Py), figview_ToPrintPixX(v, Qx), figview_ToPrintPixY(v, Qy), figview_ToPrintPixX(v, Rx), figview_ToPrintPixY(v, Ry));
    }

    col = figattr_GetColor(figospli_GetVAttributes(self), figospli_GetIVAttributes(self));
    print_LookUpColor(col, &rcol, &gcol, &bcol);

    if (figospli_Closed(self)) {
	fprintf(file, "%s  closepath\n", prefix);

	shad = figattr_GetShade(figospli_GetVAttributes(self), figospli_GetIVAttributes(self));
	if (shad != figattr_ShadeClear) {
	    fprintf(file, "%s  gsave\n", prefix);
	    shadcol = (double)(figattr_ShadeDenom-shad) / (double)figattr_ShadeDenom;
	    fprintf(file, "%s  %f %f %f setrgbcolor\n", prefix, FadeColor(rcol, shadcol), FadeColor(gcol, shadcol), FadeColor(bcol, shadcol));
	    /*fprintf(file, "%s  %f setgray\n", prefix, shadcol);*/
	    fprintf(file, "%s  eofill grestore\n", prefix);
	}
    }

    lw = figattr_GetLineWidth(figospli_GetVAttributes(self), figospli_GetIVAttributes(self));
    lw = figview_ToPrintPixW(v, lw*figview_FigUPerPix);
    if (lw <= 0) lw = 0;
    fprintf(file, "%s  %d setlinewidth\n", prefix, lw);
    fprintf(file, "%s  %f %f %f setrgbcolor\n", prefix, rcol, gcol, bcol);
    /*fprintf(file, "%s  0 setgray\n", prefix);*/
    fprintf(file, "%s  stroke\n", prefix);
}

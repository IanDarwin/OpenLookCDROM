/* figoell.c - fig element object: ellipse */
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
char *figoell_c_rcsid = "$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/figure/RCS/figoell.c,v 1.2 1992/12/14 20:45:16 rr2b R6tape $";
#endif

#include <figoell.eh>

#include <view.ih>
#include <figv.ih>
#include <figtoolv.ih>
#include <figattr.ih>
#include <print.ih>

boolean figoell__InitializeObject(ClassID, self)
struct classhdr *ClassID;
struct figoell *self;
{
    return TRUE;
}

struct figoell *figoell__Create(classID, left, top, width, height)
struct classheader *classID;
long left, top, width, height;
{
    struct figoell *res = figoell_New();
    if (!res) return NULL;

    figoell_PosX(res) = left;
    figoell_PosY(res) = top;
    figoell_PosW(res) = width;
    figoell_PosH(res) = height;
    figoell_RecomputeBounds(res);

    return res;
}

char *figoell__ToolName(dummy, v, rock)
struct figoell *dummy;
struct figtoolview *v;
long rock;
{
    return "Ellipse";
}

enum figobj_HitVal figoell__HitMe(self, x, y, delta, ptref) 
struct figoell *self;
long x, y;
long delta;
long *ptref;
{
    enum figobj_HitVal res = figoell_BasicHitMe(self, x, y, delta, ptref);
    long x0, y0, x1, y1;
    double dx, dy, sum, ddel;

    if (res!=figobj_HitInside)
	return res;

    x1 = figoell_PosW(self)/2;
    y1 = figoell_PosH(self)/2;

    x0 = x - (figoell_PosX(self) + x1);
    y0 = y - (figoell_PosY(self) + y1);

    if (x1<0) x1 = (-x1);
    if (y1<0) y1 = (-y1);

    dx = (double)(x0) / (double)(x1);
    dy = (double)(y0) / (double)(y1);
    sum = dx*dx + dy*dy;
    if (x1<y1)
	ddel = 2.0 * (double)delta / (double)x1;
    else
	ddel = 2.0 * (double)delta / (double)y1;
    if (sum >= 1.0-ddel && sum <= 1.0+ddel) {
	if (ptref)
	    *ptref = 0;
	return figobj_HitBody;
    }
    if (sum < 1.0) {
	return figobj_HitInside;
    }

    return figobj_Miss;
}

void figoell__Draw(self, v) 
struct figoell *self;
struct figview *v;
{
    long x, y, w, h;
    long shad, lw;
    char *col;

    if (figoell_PosW(self) >= 0) {
	x = figview_ToPixX(v, figoell_PosX(self));
	w = figview_ToPixW(v, figoell_PosW(self));
    }
    else {
	x = figview_ToPixX(v, figoell_PosX(self)+figoell_PosW(self));
	w = figview_ToPixW(v, -figoell_PosW(self));
    }
    
    if (figoell_PosH(self) >= 0) {
	y = figview_ToPixY(v, figoell_PosY(self));
	h = figview_ToPixH(v, figoell_PosH(self));
    }
    else {
	y = figview_ToPixY(v, figoell_PosY(self)+figoell_PosH(self));
	h = figview_ToPixH(v, -figoell_PosH(self));
    }

    figview_SetTransferMode(v, graphic_COPY);

    col = figattr_GetColor(figoell_GetVAttributes(self), figoell_GetIVAttributes(self));
    figview_SetForegroundColor(v, col, 0, 0, 0); 

    shad = figattr_GetShade(figoell_GetVAttributes(self), figoell_GetIVAttributes(self));
    if (shad != figattr_ShadeClear)
	figview_FillOvalSize(v, x, y, w, h, figview_GrayPattern(v, shad, figattr_ShadeDenom));
    lw = figattr_GetLineWidth(figoell_GetVAttributes(self), figoell_GetIVAttributes(self));
    lw = figview_ToPixW(v, lw*figview_FigUPerPix);
    if (lw <= 0) lw = 1;
    if (lw != 1)
	figview_SetLineWidth(v, lw);

    figview_DrawOvalSize(v, x, y, w, h); 

    if (lw != 1)
	figview_SetLineWidth(v, 1);
}

void figoell__Sketch(self, v) 
struct figoell *self;
struct figview *v;
{
    long x, y, w, h;

    if (figoell_PosW(self) >= 0) {
	x = figview_ToPixX(v, figoell_PosX(self));
	w = figview_ToPixW(v, figoell_PosW(self));
    }
    else {
	x = figview_ToPixX(v, figoell_PosX(self)+figoell_PosW(self));
	w = figview_ToPixW(v, -figoell_PosW(self));
    }
    
    if (figoell_PosH(self) >= 0) {
	y = figview_ToPixY(v, figoell_PosY(self));
	h = figview_ToPixH(v, figoell_PosH(self));
    }
    else {
	y = figview_ToPixY(v, figoell_PosY(self)+figoell_PosH(self));
	h = figview_ToPixH(v, -figoell_PosH(self));
    }

    figview_SetTransferMode(v, graphic_INVERT);
    figview_DrawOvalSize(v, x, y, w, h); 
    /*figview_DrawRectSize(v, x, y, w, h); */
}

#define FadeColor(col, shad)  (1.0 - (1.0-(shad)) * (1.0-(col)))

void figoell__PrintObject(self, v, file, prefix)
struct figoell *self;
struct figview *v;
FILE *file;
char *prefix;
{
    long x, y, w, h;
    long shad, lw;
    char *col;
    double rcol, bcol, gcol, shadcol;

    fprintf(file, "%s  %% ellipse\n", prefix);

    w = figview_ToPrintPixW(v, figoell_PosW(self));
    x = figview_ToPrintPixX(v, figoell_PosX(self)) + w/2;
    if (w<0)
	w = (-w);
    else if (w==0)
	w = 1;

    h = figview_ToPrintPixH(v, figoell_PosH(self));
    y = figview_ToPrintPixY(v, figoell_PosY(self)) + h/2;
    if (h<0)
	h = (-h);
    else if (h==0)
	h = 1;

    fprintf(file, "%s  %d %d translate  %d %d scale  0 0 0.5 0 360 arc\n", prefix, x, y, w, h);
    fprintf(file, "%s  1.0 %d div  1.0 %d div  scale\n", prefix, w, h);

    col = figattr_GetColor(figoell_GetVAttributes(self), figoell_GetIVAttributes(self));
    print_LookUpColor(col, &rcol, &gcol, &bcol);

    shad = figattr_GetShade(figoell_GetVAttributes(self), figoell_GetIVAttributes(self));
    if (shad != figattr_ShadeClear) {
	fprintf(file, "%s  gsave\n", prefix);
	shadcol = (double)(figattr_ShadeDenom-shad) / (double)figattr_ShadeDenom;
	fprintf(file, "%s  %f %f %f setrgbcolor\n", prefix, FadeColor(rcol, shadcol), FadeColor(gcol, shadcol), FadeColor(bcol, shadcol));
	/*fprintf(file, "%s  %f setgray\n", prefix, shadcol);*/
	fprintf(file, "%s  fill grestore\n", prefix);
    }
    lw = figattr_GetLineWidth(figoell_GetVAttributes(self), figoell_GetIVAttributes(self));
    lw = figview_ToPrintPixW(v, lw*figview_FigUPerPix);
    if (lw <= 0) lw = 0;
    fprintf(file, "%s  %d setlinewidth\n", prefix, lw);
    fprintf(file, "%s  %f %f %f setrgbcolor\n", prefix, rcol, gcol, bcol);
    /*fprintf(file, "%s  0 setgray\n", prefix);*/
    fprintf(file, "%s  stroke\n", prefix);
}

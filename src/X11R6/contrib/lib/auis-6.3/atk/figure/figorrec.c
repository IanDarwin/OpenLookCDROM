/* figorrec.c - fig element object: rounded rectangle */
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
char *figorrec_c_rcsid = "$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/figure/RCS/figorrec.c,v 1.2 1992/12/14 20:46:05 rr2b R6tape $";
#endif

#include <figorrec.eh>

#include <view.ih>
#include <figv.ih>
#include <figtoolv.ih>
#include <figattr.ih>
#include <print.ih>

boolean figorrec__InitializeObject(ClassID, self)
struct classhdr *ClassID;
struct figorrec *self;
{
    figorrec_AttributesUsed(self) = (1<<figattr_Shade) | (1<<figattr_LineWidth) | (1<<figattr_Color) | (1<<figattr_RRectCorner);

    return TRUE;
}

struct figorrec *figorrec__Create(classID, left, top, width, height)
struct classheader *classID;
long left, top, width, height;
{
    struct figorrec *res = figorrec_New();
    if (!res) return NULL;

    figorrec_PosX(res) = left;
    figorrec_PosY(res) = top;
    figorrec_PosW(res) = width;
    figorrec_PosH(res) = height;
    figorrec_RecomputeBounds(res);

    return res;
}

char *figorrec__ToolName(dummy, v, rock)
struct figorrec *dummy;
struct figtoolview *v;
long rock;
{
    return "Round Rect";
}

void figorrec__Draw(self, v) 
struct figorrec *self;
struct figview *v;
{
    long x, y, w, h;
    long shad, lw, corn;
    char *col;

    if (figorrec_PosW(self) >= 0) {
	x = figview_ToPixX(v, figorrec_PosX(self));
	w = figview_ToPixW(v, figorrec_PosW(self));
    }
    else {
	x = figview_ToPixX(v, figorrec_PosX(self)+figorrec_PosW(self));
	w = figview_ToPixW(v, -figorrec_PosW(self));
    }
    
    if (figorrec_PosH(self) >= 0) {
	y = figview_ToPixY(v, figorrec_PosY(self));
	h = figview_ToPixH(v, figorrec_PosH(self));
    }
    else {
	y = figview_ToPixY(v, figorrec_PosY(self)+figorrec_PosH(self));
	h = figview_ToPixH(v, -figorrec_PosH(self));
    }

    figview_SetTransferMode(v, graphic_COPY);

    col = figattr_GetColor(figorrec_GetVAttributes(self), figorrec_GetIVAttributes(self));
    figview_SetForegroundColor(v, col, 0, 0, 0); 

    shad = figattr_GetShade(figorrec_GetVAttributes(self), figorrec_GetIVAttributes(self));
    corn = figattr_GetRRectCorner(figorrec_GetVAttributes(self), figorrec_GetIVAttributes(self));
    corn = figview_ToPixW(v, corn*figview_FigUPerPix);

    if (shad != figattr_ShadeClear)
	figview_FillRRectSize(v, x, y, w, h, corn, corn, figview_GrayPattern(v, shad, figattr_ShadeDenom));
    lw = figattr_GetLineWidth(figorrec_GetVAttributes(self), figorrec_GetIVAttributes(self));
    lw = figview_ToPixW(v, lw*figview_FigUPerPix);
    if (lw <= 0) lw = 1;
    if (lw != 1)
	figview_SetLineWidth(v, lw);

    figview_DrawRRectSize(v, x, y, w, h, corn, corn); 

    if (lw != 1)
	figview_SetLineWidth(v, 1);
}

void figorrec__Sketch(self, v) 
struct figorrec *self;
struct figview *v;
{
    long x, y, w, h, corn;

    if (figorrec_PosW(self) >= 0) {
	x = figview_ToPixX(v, figorrec_PosX(self));
	w = figview_ToPixW(v, figorrec_PosW(self));
    }
    else {
	x = figview_ToPixX(v, figorrec_PosX(self)+figorrec_PosW(self));
	w = figview_ToPixW(v, -figorrec_PosW(self));
    }
    
    if (figorrec_PosH(self) >= 0) {
	y = figview_ToPixY(v, figorrec_PosY(self));
	h = figview_ToPixH(v, figorrec_PosH(self));
    }
    else {
	y = figview_ToPixY(v, figorrec_PosY(self)+figorrec_PosH(self));
	h = figview_ToPixH(v, -figorrec_PosH(self));
    }

    corn = figattr_GetRRectCorner(figorrec_GetVAttributes(self), figorrec_GetIVAttributes(self));
    corn = figview_ToPixW(v, corn*figview_FigUPerPix);

    figview_SetTransferMode(v, graphic_INVERT);
    figview_DrawRRectSize(v, x, y, w, h, corn, corn); 
    /*figview_DrawRectSize(v, x, y, w, h);  */
}

#define FadeColor(col, shad)  (1.0 - (1.0-(shad)) * (1.0-(col)))

void figorrec__PrintObject(self, v, file, prefix)
struct figorrec *self;
struct figview *v;
FILE *file;
char *prefix;
{
    long x, y, w, h;
    long shad, lw, corn;
    char *col;
    double rcol, bcol, gcol, shadcol;

    fprintf(file, "%s  %% rounded rectangle\n", prefix);

    if (figorrec_PosW(self) >= 0) {
	x = figview_ToPrintPixX(v, figorrec_PosX(self));
	w = figview_ToPrintPixW(v, figorrec_PosW(self));
    }
    else {
	x = figview_ToPrintPixX(v, figorrec_PosX(self)+figorrec_PosW(self));
	w = figview_ToPrintPixW(v, -figorrec_PosW(self));
    }
    
    if (figorrec_PosH(self) >= 0) {
	y = figview_ToPrintPixY(v, figorrec_PosY(self));
	h = figview_ToPrintPixH(v, figorrec_PosH(self));
    }
    else {
	y = figview_ToPrintPixY(v, figorrec_PosY(self)+figorrec_PosH(self));
	h = figview_ToPrintPixH(v, -figorrec_PosH(self));
    }
    if (w==0)
	w=1;
    if (h==0)
	h=1;

    corn = figattr_GetRRectCorner(figorrec_GetVAttributes(self), figorrec_GetIVAttributes(self));
    corn = figview_ToPrintPixW(v, corn*figview_FigUPerPix);

    if (corn==0) {
	fprintf(file, "%s  %d %d moveto  %d %d lineto  %d %d lineto  %d %d lineto closepath\n", prefix, x, y,  x, y+h,  x+w, y+h,  x+w, y);
    }
    else if ((2*corn >= h) || (2*corn >= w)) {
	/* Bizarre -- corners are bigger than rectangle, so 
	 make an appropriate looking oval */
	if ((2*corn >= h) && (2*corn >= w)) {
	    fprintf(file, "%s  %d %d translate  %d %d scale  0 0 0.5 0 360 arc\n", prefix, x+w/2, y+h/2, w, h);
	    fprintf(file, "%s  1.0 %d div  1.0 %d div  scale\n", prefix, w, h);
	}
	else if (2*corn >= h) {
	    /* Draw left semi-oval */
	    fprintf(file, "%s  %d %d translate  %d %d scale  0 0 0.5 90 270 arc\n", prefix, x+corn, y+h/2, 2*corn, h);
	    fprintf(file, "%s  1.0 %d div  1.0 %d div  scale\n", prefix, 2*corn, h);
	    /* Draw right semi-oval */
	    fprintf(file, "%s  %d %d translate  %d %d scale  0 0 0.5 270 450 arc\n", prefix, w-2*corn, 0, 2*corn, h);
	    fprintf(file, "%s  1.0 %d div  1.0 %d div  scale\n", prefix, 2*corn, h);
	    fprintf(file, "%s  closepath\n", prefix);
	}
	else { /* assuming (2*corn >= w) */
	    /* Draw top semi-oval */
	    fprintf(file, "%s  %d %d translate  %d %d scale  0 0 0.5 -180 0 arc\n", prefix, x+w/2, y+corn, w, 2*corn);
	    fprintf(file, "%s  1.0 %d div  1.0 %d div  scale\n", prefix, w, 2*corn);
	    /* Draw bottom semi-oval */
	    fprintf(file, "%s  %d %d translate  %d %d scale  0 0 0.5 0 180 arc\n", prefix, 0, h-2*corn, w, 2*corn);
	    fprintf(file, "%s  1.0 %d div  1.0 %d div  scale\n", prefix, w, 2*corn);
	    fprintf(file, "%s  closepath\n", prefix);
	}
	
    }
    else {
	fprintf(file, "%s  %d %d translate  %d %d scale  0 0 0.5 180 270 arc\n", prefix, x+corn, y+corn, 2*corn, 2*corn);
	fprintf(file, "%s  1.0 %d div  1.0 %d div  scale\n", prefix, 2*corn, 2*corn);
	fprintf(file, "%s  %d %d translate  %d %d scale  0 0 0.5 270 360 arc\n", prefix, w-2*corn, 0, 2*corn, 2*corn);
	fprintf(file, "%s  1.0 %d div  1.0 %d div  scale\n", prefix, 2*corn, 2*corn);
	fprintf(file, "%s  %d %d translate  %d %d scale  0 0 0.5 360 450 arc\n", prefix, 0, h-2*corn, 2*corn, 2*corn);
	fprintf(file, "%s  1.0 %d div  1.0 %d div  scale\n", prefix, 2*corn, 2*corn);
	fprintf(file, "%s  %d %d translate  %d %d scale  0 0 0.5 450 540 arc\n", prefix, -(w-2*corn), 0, 2*corn, 2*corn);
	fprintf(file, "%s  1.0 %d div  1.0 %d div  scale\n", prefix, 2*corn, 2*corn);
	fprintf(file, "%s  closepath\n", prefix);
    }

    col = figattr_GetColor(figorrec_GetVAttributes(self), figorrec_GetIVAttributes(self));
    print_LookUpColor(col, &rcol, &gcol, &bcol);

    shad = figattr_GetShade(figorrec_GetVAttributes(self), figorrec_GetIVAttributes(self));
    if (shad != figattr_ShadeClear) {
	fprintf(file, "%s  gsave\n", prefix);
	shadcol = (double)(figattr_ShadeDenom-shad) / (double)figattr_ShadeDenom;
	fprintf(file, "%s  %f %f %f setrgbcolor\n", prefix, FadeColor(rcol, shadcol), FadeColor(gcol, shadcol), FadeColor(bcol, shadcol));
	/*fprintf(file, "%s  %f setgray\n", prefix, shadcol);*/
	fprintf(file, "%s  fill grestore\n", prefix);
    }
    lw = figattr_GetLineWidth(figorrec_GetVAttributes(self), figorrec_GetIVAttributes(self));
    lw = figview_ToPrintPixW(v, lw*figview_FigUPerPix);
    if (lw <= 0) lw = 0;
    fprintf(file, "%s  %d setlinewidth\n", prefix, lw);
    fprintf(file, "%s  %f %f %f setrgbcolor\n", prefix, rcol, gcol, bcol);
    /*fprintf(file, "%s  0 setgray\n", prefix);*/
    fprintf(file, "%s  stroke\n", prefix);
}

/* figoins.c - fig element object: inset */
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
char *figoins_c_rcsid = "$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/figure/RCS/figoins.c,v 1.3 1994/04/17 18:02:49 rr2b Exp $";
#endif

#include <figoins.eh>

#include <view.ih>
#include <dataobj.ih>
#include <figv.ih>
#include <figtoolv.ih>
#include <figattr.ih>
#include <text.ih>
#include <message.ih>

boolean figoins__InitializeObject(ClassID, self)
struct classhdr *ClassID;
struct figoins *self;
{
    figoins_IsInset(self) = TRUE;
    figoins_AttributesUsed(self) = 0;
    self->dobj = NULL;
    self->moribund = FALSE;
    return TRUE;
}

struct figoins *figoins__Create(classID, left, top, width, height, dataobjectname)
struct classheader *classID;
long left, top, width, height;
char *dataobjectname;
{
    struct figoins *res = figoins_New();
    if (!res) return NULL;
    if (!class_IsTypeByName(dataobjectname, "dataobject")) return NULL;

    figoins_PosX(res) = left;
    figoins_PosY(res) = top;
    figoins_PosW(res) = width;
    figoins_PosH(res) = height;
    res->dobj = (struct dataobject *)class_NewObject(dataobjectname);
    if (!res->dobj) return NULL;

    figoins_RecomputeBounds(res);

    return res;
}

char *figoins__ToolName(dummy, v, rock)
struct figoins *dummy;
struct figtoolview *v;
long rock;
{
    return "Inset";
}

/* set bounding box and handle list in fig coordinates */
void figoins__RecomputeBounds(self)
struct figoins *self;
{
    long left, width, top, height;

    if (figoins_PosW(self) >= 0) {
	left = figoins_PosX(self);
	width = figoins_PosW(self);
    }
    else {
	left = figoins_PosX(self)+figoins_PosW(self);
	width = -figoins_PosW(self);
    }

    if (figoins_PosH(self) >= 0) {
	top = figoins_PosY(self);
	height = figoins_PosH(self);
    }
    else {
	top = figoins_PosY(self)+figoins_PosH(self);
	height = -figoins_PosH(self);
    }

    figoins_SetBoundsRect(self, left, top, width, height);

    figoins_SetHandle(self, 0, figoins_PosX(self)+figoins_PosW(self)/2, figoins_PosY(self)+figoins_PosH(self)/2);
    figoins_SetHandle(self, 1, figoins_PosX(self)+figoins_PosW(self), figoins_PosY(self)+figoins_PosH(self)/2);
    figoins_SetHandle(self, 2, figoins_PosX(self)+figoins_PosW(self), figoins_PosY(self));
    figoins_SetHandle(self, 3, figoins_PosX(self)+figoins_PosW(self)/2, figoins_PosY(self));
    figoins_SetHandle(self, 4, figoins_PosX(self), figoins_PosY(self));
    figoins_SetHandle(self, 5, figoins_PosX(self), figoins_PosY(self)+figoins_PosH(self)/2);
    figoins_SetHandle(self, 6, figoins_PosX(self), figoins_PosY(self)+figoins_PosH(self));
    figoins_SetHandle(self, 7, figoins_PosX(self)+figoins_PosW(self)/2, figoins_PosY(self)+figoins_PosH(self));
    figoins_SetHandle(self, 8, figoins_PosX(self)+figoins_PosW(self), figoins_PosY(self)+figoins_PosH(self));

    figoins_ComputeSelectedBounds(self);

    figoins_UpdateParentBounds(self);
}

void figoins__Draw(self, v) 
struct figoins *self;
struct figview *v;
{
    long x, y, w, h;

    if (figoins_PosW(self) >= 0) {
	x = figview_ToPixX(v, figoins_PosX(self));
	w = figview_ToPixW(v, figoins_PosW(self));
    }
    else {
	x = figview_ToPixX(v, figoins_PosX(self)+figoins_PosW(self));
	w = figview_ToPixW(v, -figoins_PosW(self));
    }
    
    if (figoins_PosH(self) >= 0) {
	y = figview_ToPixY(v, figoins_PosY(self));
	h = figview_ToPixH(v, figoins_PosH(self));
    }
    else {
	y = figview_ToPixY(v, figoins_PosY(self)+figoins_PosH(self));
	h = figview_ToPixH(v, -figoins_PosH(self));
    }

    figview_SetTransferMode(v, graphic_COPY);
    figview_SetForegroundColor(v, "black", 0, 0, 0); 
    if (self->dobj) {
	figview_DrawRectSize(v, x-1, y-1, w+1, h+1); 
    }
    else {
	figview_FillRectSize(v, x, y, w, h, figview_WhitePattern(v));
	figview_DrawRectSize(v, x, y, w, h); 
    }
}

enum figobj_Status figoins__Build(self, v, action, x, y, clicks)   
struct figoins *self;
struct figview *v;
enum view_MouseAction action;
long x, y; /* in fig coords */
long clicks;
{
    enum figobj_Status res;

    if (self->moribund)
	return figobj_NotDone;

    if (clicks==0) {
	self->moribund = TRUE;
	message_CancelQuestion(v);
	message_DisplayString(v, 10, "Object aborted.");
	return figobj_Failed;
    }

    res = super_Build(self, v, action, x, y, clicks);
    if (res==figobj_Done) {
	char buffer[32];
	int ix;

	ix = message_AskForString(v, 40, "Data object to insert:  ", NULL, buffer, 30); 
	if (self->moribund) {
	    /* oh dear, someone cancelled the question. Us, probably. Time ro cut and run like hell. */
	    return figobj_NotDone;
	}
	if (ix<0 || strlen(buffer)==0) {
	    message_DisplayString(v, 10, "Cancelled.");
	    return figobj_Failed;
	}
	if (!class_IsTypeByName(buffer, "dataobject")) {
	    char buf2[70];
	    sprintf(buf2, "%s is not a dataobject.", buffer);
	    message_DisplayString(v, 10, buf2);
	    return figobj_Failed;
	}
	self->dobj = (struct dataobject *)class_NewObject(buffer);
	if (!self->dobj) {
	    char buf2[70];
	    sprintf(buf2, "Cannot load %s; object aborted.", buffer);
	    message_DisplayString(v, 10, buf2);
	    return figobj_Failed;
	}
	if (class_IsTypeByName(buffer, "text")) {
	    message_DisplayString(v, 0, "Reading template...");
	    text_ReadTemplate((struct text *)self->dobj, "default", FALSE);
	}

	figoins_SetModified(self);
	return figobj_Done;
    }
    else
	return res;
}

void figoins__WriteBody(self, fp)
struct figoins *self;
FILE *fp;
{
    super_WriteBody(self, fp);

    if (self->dobj) {
	dataobject_Write(self->dobj, fp, figoins_GetWriteID(self), 2); /* Two?! Two?! Look, man, it's an arbitrary non-zero number. Just deal with it. */
    }
    else {
	fprintf(fp, "\\nodata\n");
    }
}

long figoins__ReadBody(self, fp, recompute)
struct figoins *self;
FILE *fp;
boolean recompute;
{
#define LINELENGTH (250)
    char buf[LINELENGTH+1];
    long tid, ix, ref;
    char namebuf[100], *np=namebuf;

    ix = super_ReadBody(self, fp, FALSE);
    if (ix!=dataobject_NOREADERROR) return ix;

    if (fgets(buf, LINELENGTH, fp) == NULL)
	return dataobject_PREMATUREEOF;
    if (!strncmp(buf, "\\nodata", 7)) {
	self->dobj = NULL;
    }
    else {
	ix = sscanf(buf, "\\begindata{%[^,],%ld}", namebuf, &tid);
	if (ix!=2) return dataobject_BADFORMAT;
	if(class_Load(np)==NULL) {
	    np="unknown";
	}
	if (!class_IsTypeByName(np, "dataobject")) {
	    np="unknown";
	}
	self->dobj = (struct dataobject *)class_NewObject(np);
	if (!self->dobj)
	    return dataobject_OBJECTCREATIONFAILED;
	dataobject_Read(self->dobj, fp, tid);
    }

    if (recompute) {
	figoins_RecomputeBounds(self);
	figoins_SetModified(self);
    }

    return dataobject_NOREADERROR;
}

void figoins__PrintObject(self, v, file, prefix)
struct figoins *self;
struct figview *v;
FILE *file;
char *prefix;
{
    long x, y, w, h;
    long shad, lw;
    char *col;
    double rcol, bcol, gcol, shadcol;

    fprintf(file, "%s  %% inset\n", prefix);

    if (figoins_PosW(self) >= 0) {
	x = figview_ToPrintPixX(v, figoins_PosX(self));
	w = figview_ToPrintPixW(v, figoins_PosW(self));
    }
    else {
	x = figview_ToPrintPixX(v, figoins_PosX(self)+figoins_PosW(self));
	w = figview_ToPrintPixW(v, -figoins_PosW(self));
    }
    
    if (figoins_PosH(self) >= 0) {
	y = figview_ToPrintPixY(v, figoins_PosY(self));
	h = figview_ToPrintPixH(v, figoins_PosH(self));
    }
    else {
	y = figview_ToPrintPixY(v, figoins_PosY(self)+figoins_PosH(self));
	h = figview_ToPrintPixH(v, -figoins_PosH(self));
    }

    x--;
    y--;
    w+=2;
    h+=2;
    fprintf(file, "%s  %d %d moveto  %d %d lineto  %d %d lineto  %d %d lineto closepath\n", prefix, x, y,  x, y+h,  x+w, y+h,  x+w, y);

    fprintf(file, "%s  gsave\n", prefix);
    fprintf(file, "%s  1 setgray\n", prefix);
    fprintf(file, "%s  fill grestore\n", prefix);

    fprintf(file, "%s  1 setlinewidth\n", prefix);
    fprintf(file, "%s  0 setgray\n", prefix);
    fprintf(file, "%s  stroke\n", prefix);
}

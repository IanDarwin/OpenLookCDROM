/* figotext.c - fig element object: text */
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
char *figotext_c_rcsid = "$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/figure/RCS/figotext.c,v 1.6 1993/05/04 01:18:42 susan Exp $";
#endif 

#include <ctype.h>
#include <andrewos.h>

#include <figotext.eh>

#include <figattr.ih>
#include <view.ih>
#include <figv.ih>
#include <figure.ih>
#include <figtoolv.ih>
#include <fontdesc.ih>
#include <message.ih>
#include <region.ih>
#include <keymap.ih>
#include <keystate.ih>
#include <proctbl.ih>
#include <print.ih>

static struct keymap *EmbeddedKeymap;
static struct region *tmpreg;

#define figotext_Leading (1)

static void InsertProc(), DeleteProc(), KillDotProc(), KillLineProc(), MoveDot(), MoveDotProc(), TwiddleCharsProc(), CompleteProc();
static void IncreaseNumChars();

boolean figotext__InitializeClass(ClassID)
struct classhdr *ClassID;
{
    struct proctable_Entry *proc = NULL;
    int ix;
    char str[2];

    tmpreg = region_CreateEmptyRegion();

    EmbeddedKeymap = keymap_New();

    proc = proctable_DefineProc("figotext-insert-char", InsertProc, &figotext_classinfo, NULL, "Insert a character into this object.");
    keymap_BindToKey(EmbeddedKeymap, "\015", proc, '\n'); /* ctrl-M */
    str[1] = '\0';
    for (ix = 32; ix < 127; ix++)  {
	str[0] = ix;
	keymap_BindToKey(EmbeddedKeymap, str, proc, ix);
    }
    proc = proctable_DefineProc("figotext-delete-char", DeleteProc, &figotext_classinfo, NULL, "Delete a character from this object.");
    keymap_BindToKey(EmbeddedKeymap, "\010", proc, 0); /* ctrl-H */
    keymap_BindToKey(EmbeddedKeymap, "\177", proc, 0); /* DEL */

    proc = proctable_DefineProc("figotext-kill-line", KillLineProc, &figotext_classinfo, NULL, "Delete chars starting at dot.");
    keymap_BindToKey(EmbeddedKeymap, "\013", proc, 0); /* ctrl-K */

    proc = proctable_DefineProc("figotext-move-dot", MoveDotProc, &figotext_classinfo, NULL, "Move the dot in this object.");
    keymap_BindToKey(EmbeddedKeymap, "\002", proc, 0);  /* ctrl-B */
    keymap_BindToKey(EmbeddedKeymap, "\033D", proc, 0); /* esc-D */
    keymap_BindToKey(EmbeddedKeymap, "\006", proc, 1);  /* ctrl-F */
    keymap_BindToKey(EmbeddedKeymap, "\033C", proc, 1); /* esc-C */
    keymap_BindToKey(EmbeddedKeymap, "\001", proc, 2);  /* ctrl-A */
    keymap_BindToKey(EmbeddedKeymap, "\005", proc, 3);  /* ctrl-E */

    proc = proctable_DefineProc("figotext-twiddle-chars", TwiddleCharsProc, &figotext_classinfo, NULL, "Move the dot in this object.");
    keymap_BindToKey(EmbeddedKeymap, "\024", proc, 0);  /* ctrl-T */

    proc = proctable_DefineProc("figotext-complete-entry", CompleteProc, &figotext_classinfo, NULL, "Finish entering text into this object.");
    keymap_BindToKey(EmbeddedKeymap, "\033\015", proc, 0);  /* esc-ctrl-M */

    return TRUE;
}

boolean figotext__InitializeObject(ClassID, self)
struct classhdr *ClassID;
struct figotext *self;
{
    self->Keystate = keystate_Create(self, EmbeddedKeymap);

    figotext_AttributesUsed(self) = (1<<figattr_FontSize) | (1<<figattr_FontStyle) | (1<<figattr_FontFamily) | (1<<figattr_Color) | (1<<figattr_TextPos);
    figotext_SetNumHandles(self, 4);

    self->basis = NULL;
    self->buildstate = 0;
    self->text_size = 8;
    self->text = malloc(self->text_size * sizeof(char));
    self->textdirty = TRUE;
    self->dotpos = (-1);
    self->excessx = 0;
    self->excessy = 0;

    return TRUE;
}

struct figotext *figotext__Create(classID, chars, xpos, ypos)
struct classheader *classID;
char *chars;
long xpos, ypos;
{
    struct figotext *res = figotext_New();
    if (!res) return NULL;

    figotext_PosX(res) = xpos;
    figotext_PosY(res) = ypos;

    IncreaseNumChars(res, strlen(chars)+2);
    strcpy(res->text, chars);
    res->textdirty = TRUE;
    figotext_RecomputeBounds(res);

    return res;
}

void figotext__FinalizeObject(ClassID, self)
struct classhdr *ClassID;
struct figotext *self;
{
    if (self->text)
	free(self->text);
}

char *figotext__ToolName(dummy, v, rock)
struct figotext *dummy;
struct figtoolview *v;
long rock;
{
    return "Text";
}

/* set bounding box and handle list in fig coordinates. Note that the bounding box is computed by scaling a normal-size font's box, whereas the actual drawing is done with a scaled-size font.  */
void figotext__RecomputeBounds(self)
struct figotext *self;
{   
    long x, y, w, h, texw, texh;
    struct rectangle altrec;
    long x0, y0;
    char *fam;
    long size, style, textpos;
    int ix;

    fam = figattr_GetFontFamily(figotext_GetVAttributes(self), figotext_GetIVAttributes(self));
    size = figattr_GetFontSize(figotext_GetVAttributes(self), figotext_GetIVAttributes(self));
    style = figattr_GetFontStyle(figotext_GetVAttributes(self), figotext_GetIVAttributes(self));
    textpos = figattr_GetTextPos(figotext_GetVAttributes(self), figotext_GetIVAttributes(self));

    self->fdesc = fontdesc_Create(fam, style, size);
    if (!self->fdesc) return;

    if (self->textdirty) {
	if (self->basis) {
	    int tmp1, tmp2, count;
	    char *cx, *cxend;

	    self->textw = 0;
	    self->texth = 0;

	    count=0;
	    cx = self->text;

	    self->yoffset = 0;
	    do {
		cxend = index(cx, '\n');
		if (cxend) 
		    *cxend = '\0';
		if (!fontdesc_StringBoundingBox(self->fdesc, figview_GetDrawable(self->basis), cx, &tmp1, &tmp2)) {
		    tmp1 = 10;
		    tmp2 = 10;
		}

		if (self->textw < tmp1)
		    self->textw = tmp1;
		if (count==0) {
		    self->texth = tmp2;
		    self->yoffset = tmp2 / 2;
		}
		else {
		    tmp2 = count*(size+figotext_Leading) + tmp2/2;
		    self->texth = tmp2 + self->yoffset;
		}

		if (cxend) {
		    *cxend = '\n';
		    cx = cxend+1;
		}
		count++;
	    } while (cxend);
	}
	else {
	    self->textw = 10;
	    self->texth = 10;
	}
	self->textdirty = FALSE;
    }

    texw = (self->textw+1) * figview_FigUPerPix;
    texh = (self->texth+1) * figview_FigUPerPix;
    w = texw + self->excessx;
    h = texh + self->excessy;

    altrec.width = texw;
    altrec.height = texh;

    x = figotext_PosX(self);
    y = figotext_PosY(self);
    switch (textpos) {
	case figattr_PosCenter:
	    x0 = x-w/2;
	    altrec.left = x - texw/2;
	    break;
	case figattr_PosLeft:
	    x0 = x-self->excessx/2;
	    altrec.left = x;
	    break;
	case figattr_PosRight:
	    x0 = x-w+self->excessx/2;
	    altrec.left = x - texw;
	    break;
    }

    y0 = y - (self->yoffset * figview_FigUPerPix + self->excessy/2);
    altrec.top = y - (self->yoffset * figview_FigUPerPix);

    figotext_SetHandle(self, 0, x0+w, y0);
    figotext_SetHandle(self, 1, x0, y0);
    figotext_SetHandle(self, 2, x0, y0+h);
    figotext_SetHandle(self, 3, x0+w, y0+h);

    if (w>=0) {
	self->handlerect.left = x0;
	self->handlerect.width = w;
    }
    else {
	self->handlerect.left = x0 + w;
	self->handlerect.width = (-w);
    }

    if (h>=0) {
	self->handlerect.top = y0;
	self->handlerect.height = h;
    }
    else {
	self->handlerect.top = y0 + h;
	self->handlerect.height = (-h);
    }

    rectangle_UnionRect(&altrec, &altrec, &self->handlerect);
    figotext_SetBoundsRect(self, altrec.left, altrec.top, altrec.width, altrec.height);

    figotext_ComputeSelectedBounds(self);
    figotext_UpdateParentBounds(self);
}

static enum figobj_HandleType handletypes[4]={
    figobj_URCorner,
    figobj_ULCorner,
    figobj_LLCorner,
    figobj_LRCorner
};

enum figobj_HandleType figotext__GetHandleType(self, num)
struct figotext *self;
long num;
{
    if(num>=0 && num<=3) return handletypes[num];
    else return figobj_None;
}

static long canonical[] = {
    1, 3, figobj_NULLREF
};

long *figotext__GetCanonicalHandles(self)
struct figotext *self;
{
    return canonical;
}

struct rectangle *figotext__GetBounds(self, vv)
struct figotext *self;
struct figview *vv;
{
    if (!vv) {
	if (!self->basis) {
	    figotext_RecomputeBounds(self);
	}
    }
    else if (vv != self->basis) {
	self->basis = vv;
	self->textdirty = TRUE;
	figotext_RecomputeBounds(self);
    }

    return super_GetBounds(self, vv);
}

void figotext__Draw(self, v) 
struct figotext *self;
struct figview *v;
{
    long gray, count;
    char *fam, *col, *cx, *cxend;
    long size, style, textpos, grapos, grax;
    struct rectangle *rec = figotext_GetBounds(self, v);
    struct region *viewclip;
    struct rectangle bb;

    fam = figattr_GetFontFamily(figotext_GetVAttributes(self), figotext_GetIVAttributes(self));
    size = figattr_GetFontSize(figotext_GetVAttributes(self), figotext_GetIVAttributes(self));
    style = figattr_GetFontStyle(figotext_GetVAttributes(self), figotext_GetIVAttributes(self));
    textpos = figattr_GetTextPos(figotext_GetVAttributes(self), figotext_GetIVAttributes(self));
    size = figview_ToPixW(v, size * figview_FigUPerPix);

    self->fdesc = fontdesc_Create(fam, style, size);
    if (!self->fdesc) return;

    figview_SetTransferMode(v, graphic_COPY);
    col = figattr_GetColor(figotext_GetVAttributes(self), figotext_GetIVAttributes(self));
    figview_SetForegroundColor(v, col, 0, 0, 0); 

    bb.left = figview_ToPixX(v, rec->left) - 2;
    bb.top = figview_ToPixY(v, rec->top) - 2;
    bb.width = figview_ToPixW(v, rec->width) + 4;
    bb.height = figview_ToPixH(v, rec->height) + 4;

    viewclip = figview_GetCurrentClipRegion(v);

    region_RectRegion(tmpreg, &bb); 
    if (viewclip)
	region_IntersectRegion(tmpreg, viewclip, tmpreg);
    figview_SetClippingRegion(v, tmpreg); 

    figview_SetFont(v, self->fdesc);
    
    gray = figview_ToPixY(v, figotext_PosY(self));
    switch (textpos) {
	case figattr_PosCenter:
	    grapos = graphic_BETWEENLEFTANDRIGHT | graphic_BETWEENTOPANDBOTTOM;
	    grax = figview_ToPixX(v, figotext_PosX(self));
	    break;
	case figattr_PosLeft:
	    grapos = graphic_ATLEFT | graphic_BETWEENTOPANDBOTTOM;
	    grax = figview_ToPixX(v, figotext_PosX(self));
	    break;
	case figattr_PosRight:
	    grapos = graphic_ATRIGHT | graphic_BETWEENTOPANDBOTTOM;
	    grax = figview_ToPixX(v, figotext_PosX(self));
	    break;
    }
    cx = self->text;
    count = 0;
    do {
	figview_MoveTo(v, grax, gray+count*(size+figotext_Leading));
	cxend = index(cx, '\n');
	if (cxend)
	    figview_DrawText(v, cx, (cxend-cx), grapos);
	else
	    figview_DrawString(v, cx, grapos);
	count++;
	if (cxend)
	    cx = cxend+1;
    } while (cxend);
    /*figview_DrawString(v, self->text, graphic_BETWEENLEFTANDRIGHT | graphic_BETWEENTOPANDBOTTOM);*/

    if (viewclip)
	figview_SetClippingRegion(v, viewclip);
    else
	figview_ClearClippingRect(v);
}

void figotext__Sketch(self, v) 
struct figotext *self;
struct figview *v;
{
    /*super_Sketch(self, v);*/
    long x, y, w, h;

    x = figview_ToPixX(v, self->handlerect.left);
    y = figview_ToPixY(v, self->handlerect.top);
    w = figview_ToPixW(v, self->handlerect.width);
    h = figview_ToPixH(v, self->handlerect.height);
    figview_SetTransferMode(v, graphic_INVERT);
    figview_DrawRectSize(v, x, y, w, h);
}

static void IncreaseNumChars(self, val)
struct figotext *self;
int val;
{
    if (val > self->text_size) {
	while (val > self->text_size)
	    self->text_size *= 2;

	self->text = realloc(self->text, self->text_size);
    }
}

enum figobj_Status figotext__Build(self, v, action, x, y, clicks)   
struct figotext *self;
struct figview *v;
enum view_MouseAction action;
long x, y; /* in fig coords */
long clicks;
{
    int ix;

    if (clicks==0) {
	KillDotProc(self);
	ix = strlen(self->text);
	if (ix) {
	    figotext_RecomputeBounds(self);
	    figotext_SetModified(self);
	    return figobj_Done;
	}
	else {
	    message_DisplayString(v, 10, "No string entered; object aborted.");
	    return figobj_Failed;
	}
    }

    switch (action) {
	case view_LeftDown:
	    if (self->buildstate==0) {
		figotext_PosX(self) = x;
		figotext_PosY(self) = y;
		strcpy(self->text, "|");
		self->dotpos = 0;
		self->buildstate = 1;
		self->textdirty = TRUE;
		figview_SetBuildKeystate(v, self->Keystate);
		self->buildview = v;
		figotext_RecomputeBounds(self);
		figotext_SetModified(self);
		figview_WantUpdate(v, v);
		return figobj_NotDone;
	    }
	    else {
		figotext_PosX(self) = x;
		figotext_PosY(self) = y;
		figotext_RecomputeBounds(self);
		figotext_SetModified(self);
		figview_WantUpdate(v, v);
		return figobj_NotDone;
	    }
	case view_LeftMovement:
	    if (figotext_PosX(self) != x || figotext_PosY(self) != y) {
		figotext_PosX(self) = x;
		figotext_PosY(self) = y;
		figotext_RecomputeBounds(self);
		figotext_SetModified(self);
		figview_WantUpdate(v, v);
	    }
	    return figobj_NotDone;
	case view_LeftUp:
	    return figobj_NotDone;
	case view_RightDown:
	    KillDotProc(self);
	    ix = strlen(self->text);
	    if (ix)  {
		figotext_RecomputeBounds(self);
		figotext_SetModified(self);
		return figobj_Done;
	    }
	    else {
		message_DisplayString(v, 10, "No string entered; object aborted.");
		return figobj_Failed;
	    }
	default:
	    return figobj_Failed;
    }
}

enum figobj_HitVal figotext__HitMe(self, x, y, delta, ptref) 
struct figotext *self;
long x, y;
long delta;
long *ptref;
{
    enum figobj_HitVal res;
    res = figotext_BasicHitMe(self, x, y, delta, ptref);

    if (res==figobj_HitInside) {
	res = figobj_HitBody;
	if (ptref)
	    *ptref = 0; /* only one body part */
    }
    return res;
}

static void CompleteProc(self, rock)
struct figotext *self;
int rock;
{
    if (self->buildview && self->buildview->toolset) {
	figtoolview_AbortObjectBuilding(self->buildview->toolset);
    }
}

static void KillDotProc(self)
struct figotext *self;
{
    char *ch;
    if (self->dotpos == (-1))
	return;

    for (ch = self->text+self->dotpos; *ch; ch++)
	*ch = *(ch+1);
    self->dotpos = (-1);
    self->textdirty = TRUE;
}

static void MoveDot(self, pos)
struct figotext *self;
int pos;    /* -1 to remove dot */
{
    int ix, len;

    if (pos == self->dotpos)
	return;

    if (pos == (-1)) {
	KillDotProc(self);
	return;
    }

    if (self->dotpos == (-1)) {
	len = strlen(self->text);

	if (len+2 >= self->text_size)
	    IncreaseNumChars(self, len+2);

	self->dotpos = pos;
	for (ix = len+1; ix>self->dotpos; ix--)
	    self->text[ix] = self->text[ix-1];
	self->text[self->dotpos] = '|';
    }
    else {
	if (pos < self->dotpos) {
	    for (ix=self->dotpos; ix>pos; ix--)
		self->text[ix] = self->text[ix-1];
	    self->dotpos = pos;
	    self->text[self->dotpos] = '|';
	}
	else {
	    for (ix=self->dotpos; ix<pos; ix++)
		self->text[ix] = self->text[ix+1];
	    self->dotpos = pos;
	    self->text[self->dotpos] = '|';
	}
    }
}

static void MoveDotProc(self, towhere)
struct figotext *self;
int towhere;
{
    if (self->dotpos == (-1))
	return;
    switch (towhere) {
	case 0:
	    if (self->dotpos == 0)
		return;
	    MoveDot(self, self->dotpos-1);
	    break;
	case 1:
	    if (self->text[self->dotpos+1] == '\0')
		return;
	    MoveDot(self, self->dotpos+1);
	    break;
	case 2:
	    MoveDot(self, 0);
	    break;
	case 3:
	    MoveDot(self, strlen(self->text)-1);
	    break;
    }

    self->textdirty = TRUE;
    figotext_RecomputeBounds(self);
    figotext_SetModified(self);
    figview_WantUpdate(self->buildview, self->buildview);
}

static void KillLineProc(self, rock)
struct figotext *self;
int rock;
{
    if (self->dotpos == (-1))
	return;

    self->text[self->dotpos+1] = '\0';

    self->textdirty = TRUE;
    figotext_RecomputeBounds(self);
    figotext_SetModified(self);
    figview_WantUpdate(self->buildview, self->buildview);
}

static void TwiddleCharsProc(self, rock)
struct figotext *self;
int rock;
{
    char tmp;
    if (self->dotpos < 2)
	return;

    tmp = self->text[self->dotpos-1];
    self->text[self->dotpos-1] = self->text[self->dotpos-2];
    self->text[self->dotpos-2] = tmp;

    self->textdirty = TRUE;
    figotext_RecomputeBounds(self);
    figotext_SetModified(self);
    figview_WantUpdate(self->buildview, self->buildview);
}

static void InsertProc(self, rock)
struct figotext *self;
int rock;
{
    int ix;
    int len = strlen(self->text);
    char *ch;

    if (self->dotpos == (-1))
	return;

    if (len+2 >= self->text_size)
	IncreaseNumChars(self, len+2);

    for (ix = len+1; ix>self->dotpos+1; ix--)
	self->text[ix] = self->text[ix-1];

    self->text[self->dotpos] = rock;
    self->dotpos++;
    self->text[self->dotpos] = '|';

    self->textdirty = TRUE;
    figotext_RecomputeBounds(self);
    figotext_SetModified(self);
    figview_WantUpdate(self->buildview, self->buildview);
}

static void DeleteProc(self, rock)
struct figotext *self;
int rock;
{
    char *ch;
    if (self->dotpos<=0)
	return;

    for (ch = self->text+self->dotpos; *ch; ch++)
	*ch = *(ch+1);
    self->dotpos--;
    self->text[self->dotpos] = '|';

    self->textdirty = TRUE;
    figotext_RecomputeBounds(self);
    figotext_SetModified(self);
    figview_WantUpdate(self->buildview, self->buildview);
}

static void MoveHandle(self, x, y, ptref)
struct figotext *self;
long x, y, ptref;
{
    struct point *pt = &(figotext_GetHandles(self)[ptref]);
    int val;

    if (ptref==1 || ptref==2) {
	val = pt->x - x;
	self->excessx += val;
	figotext_PosX(self) -= val/2;
    }
    else {
	val = x - pt->x;
	self->excessx += val;
	figotext_PosX(self) += val/2;
    }

    if (ptref==1 || ptref==0) {
	val = pt->y - y;
	self->excessy += val;
	figotext_PosY(self) -= val/2;
    }
    else {
	val = y - pt->y;
	self->excessy += val;
	figotext_PosY(self) += val/2;
    }

    /* figotext_Reposition(self, x - pt->x, y - pt->y); */
}

boolean figotext__Reshape(self, action, v, x, y, handle, ptref)
struct figotext *self;
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
	    if (figotext_GetReadOnly(self))
		return FALSE;
	    figotext_Sketch(self, v);
	    break;
	case view_LeftMovement:
	case view_RightMovement:
	    figotext_Sketch(self, v);
	    MoveHandle(self, x, y, ptref);
	    figotext_RecomputeBounds(self);
	    figotext_Sketch(self, v);
	    break;
	case view_LeftUp:
	case view_RightUp:
	    figotext_Sketch(self, v);
	    MoveHandle(self, x, y, ptref);
	    figotext_RecomputeBounds(self);
	    figotext_SetModified(self);
	    break;
    }
    return TRUE;
}

void figotext__MoveHandle(self, x, y, ptref)
struct figotext *self;
long x, y, ptref;
{
    if (figotext_GetReadOnly(self))
	return;
    MoveHandle(self, x, y, ptref);
    figotext_SetModified(self);
    figotext_RecomputeBounds(self);
}

void figotext__InheritVAttributes(self, attr, mask)
struct figotext *self;
struct figattr *attr;
unsigned long mask;
{
    super_InheritVAttributes(self, attr, mask);

    if (mask & (~(figotext_GetVAttributes(self)->active)) & ((1<<figattr_FontFamily) | (1<<figattr_FontStyle) | (1<<figattr_FontSize) | (1<<figattr_TextPos))) {
	self->textdirty = TRUE;
	figotext_RecomputeBounds(self);
    }
}

unsigned long figotext__UpdateVAttributes(self, attr, mask)
struct figotext *self;
struct figattr *attr;
unsigned long mask;
{
    mask = super_UpdateVAttributes(self, attr, mask);

    if (mask & ((1<<figattr_FontFamily) | (1<<figattr_FontStyle) | (1<<figattr_FontSize) | (1<<figattr_TextPos))) {
	self->textdirty = TRUE;
	figotext_RecomputeBounds(self);
    }
    return mask;
}

/* the format for writing the text:
nonprintable characters, including space, \, =: =000 (octal)
others: inserted normally.
A newline is inserted every LINESIZE output characters. The end is marked with =000.
*/
void figotext__WriteBody(self, fp)
struct figotext *self;
FILE *fp;
{
    int ix, ch;
    int count;

    super_WriteBody(self, fp);
#define LINESIZE (70)

    fprintf(fp, "$ %d %d\n", self->excessx, self->excessy);

    count=0;
    for (ix=0; 1; ix++) {
	ch = self->text[ix];
	if (ch=='\\' || ch=='=' || !isgraph(ch)) {
	    fprintf(fp, "=%03o", ch);
	    count+=4;
	}
	else {
	    fputc(ch, fp);
	    count++;
	}
	if (count >= LINESIZE || ch=='\0') {
	    fputc('\n', fp);
	    count=0;
	}

	if (ch=='\0')
	    break;
    }
}

long figotext__ReadBody(self, fp, recompute)
struct figotext *self;
FILE *fp;
boolean recompute;
{
    int	ix; 
    int count, ch;
    long tmp1, tmp2;

#define LINELENGTH (250)
    char buf[LINELENGTH+1];

    ix = super_ReadBody(self, fp, FALSE);
    if (ix!=dataobject_NOREADERROR) return ix;

    if (fgets(buf, LINELENGTH, fp) == NULL)
	return dataobject_PREMATUREEOF;
    ix = sscanf(buf, "$ %ld %ld", &tmp1, &tmp2);
    if (ix!=2) return dataobject_BADFORMAT;
    self->excessx = tmp1;
    self->excessy = tmp2;

    count = 0;
    while (1) {
	if (fgets(buf, LINELENGTH, fp) == NULL)
	    return dataobject_PREMATUREEOF;

	for (ix=0; buf[ix]; ix++) {
	    if (buf[ix]=='\n') 
		continue;
	    IncreaseNumChars(self, count+2);
	    if (buf[ix]=='=') {
		ch = (buf[ix+1]-'0')*64 + (buf[ix+2]-'0')*8 + (buf[ix+3]-'0');
		ix += 3;
	    }
	    else {
		ch = buf[ix];
	    }
	    self->text[count] = ch;
	    count++;
	    if (ch=='\0')
		break;
	}
	if (ch=='\0')
	    break;
    }
    self->textdirty = TRUE;

    if (recompute) {
	figotext_RecomputeBounds(self);
	figotext_SetModified(self);
    }

    return dataobject_NOREADERROR;
}

static boolean StringMatch(str1, str2)
char *str1, *str2;
{
    char c1, c2;

    if (str1==NULL && str2==NULL)
	return TRUE; 
    if (str1==NULL || str2==NULL)
	return TRUE; 
    while (!(*str1=='\0' && *str2=='\0')) {
	if ((c1=(*str1)) != (c2=(*str2))) {
	    if (isupper(c1))
		c1 = tolower(c1);
	    if (isupper(c2))
		c2 = tolower(c2);
	    if (c1 != c2)
		return FALSE;
	}
	str1++;
	str2++;
    }
    return TRUE;
}

void figotext__PrintObject(self, v, file, prefix)
struct figotext *self;
struct figview *v;
FILE *file;
char *prefix;
{
    int ix, ch, count;
    char *fam, *psfam;
    long size, style, textpos;
    long x, y, w, h;
    long shad, lw;
    char *col;
    boolean adddash = FALSE, addroman = FALSE;
    char *mod1, *mod2, *posmod;
    double rcol, bcol, gcol;

    fprintf(file, "%s  %% text\n", prefix);
 
    col = figattr_GetColor(figotext_GetVAttributes(self), figotext_GetIVAttributes(self));
    print_LookUpColor(col, &rcol, &gcol, &bcol);

    x = figview_ToPrintPixX(v, figotext_PosX(self));
    y = figview_ToPrintPixY(v, figotext_PosY(self));
    fam = figattr_GetFontFamily(figotext_GetVAttributes(self), figotext_GetIVAttributes(self));
    size = figattr_GetFontSize(figotext_GetVAttributes(self), figotext_GetIVAttributes(self));
    style = figattr_GetFontStyle(figotext_GetVAttributes(self), figotext_GetIVAttributes(self));
    textpos = figattr_GetTextPos(figotext_GetVAttributes(self), figotext_GetIVAttributes(self));
    size = figview_ToPrintPixW(v, size * figview_FigUPerPix);
    if (StringMatch(fam, "Andy")) {
	psfam = "Times";
	addroman = TRUE;
    }
    else if (StringMatch(fam, "andysans"))
	psfam = "Helvetica";
    else
	psfam = "Courier";

    mod1 = "";
    mod2 = "";
    if (style & fontdesc_Bold) {
	mod1 = "Bold";
	adddash = TRUE;
    }
    if (style & fontdesc_Italic) {
	if (addroman)
	    mod2 = "Italic";
	else
	    mod2 = "Oblique";
	adddash = TRUE;
    }
    if (style == fontdesc_Plain && addroman) {
	mod1 = "Roman";
	adddash = TRUE;
    }

    switch (textpos) {
	case figattr_PosLeft:
	    posmod = "0";
	    break;
	case figattr_PosRight:
	    posmod = "dup stringwidth pop neg";
	    break;
	case figattr_PosCenter:
	default:
	    posmod = "dup stringwidth pop 2 div neg";
	    break;
    }
    fprintf(file, "%s  gsave\n", prefix);
    fprintf(file, "%s  /%s%c%s%s findfont %d scalefont setfont\n", prefix, psfam, (adddash ? '-' : ' '), mod1, mod2, size);
    fprintf(file, "%s  %f %f %f setrgbcolor\n", prefix, rcol, gcol, bcol);
    /*fprintf(file, "%s  0 setgray\n", prefix);*/
    fprintf(file, "%s  %d %d translate 1 -1 scale\n", prefix, x, y);

    fprintf(file, "%s  (", prefix);
    count = 0;
    for (ix=0; TRUE; ix++) {
	ch = self->text[ix];
	if (ch=='\0' || ch=='\n') {
	    fprintf(file, ") %s %d moveto show\n", posmod, -count * (size+figotext_Leading));
	    if (ch=='\n') {
		fprintf(file, "%s  (", prefix);
		count++;
	    }
	    else
		break;
	}
	else {
	    if (ch=='\\' || ch=='(' || ch==')' || !isgraph(ch)) {
		fprintf(file, "\\%03o", ch);
	    }
	    else {
		fputc(ch, file);
	    }
	}
    }

    fprintf(file, "%s  grestore\n", prefix);
}

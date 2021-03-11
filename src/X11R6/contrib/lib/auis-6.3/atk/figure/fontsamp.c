/* fontsamp.c - font sample view */
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
char *fontsamp_c_rcsid = "$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/figure/RCS/fontsamp.c,v 1.2 1992/12/14 20:44:19 rr2b R6tape $";
#endif

#include <fontsamp.eh>

#include <fontdesc.ih>

#include <fontsel.ih>

#define INITTESTSTRING "Sample"

boolean fontsample__InitializeClass(ClassID)
struct classhdr *ClassID;
{
    return TRUE;
}

boolean fontsample__InitializeObject(ClassID, self)
struct classhdr *ClassID;
struct fontsample *self;
{
    self->teststring = malloc(strlen(INITTESTSTRING)+1);
    strcpy(self->teststring, INITTESTSTRING);
    self->dirty = TRUE;
    self->fdesc = NULL;

    return TRUE;
}

void fontsample__FinalizeObject(ClassID, self)
struct classhdr *ClassID;
struct fontsample *self;
{
    free(self->teststring);
}

static void RedrawView(self)
struct fontsample *self;
{
    long x, y, w, h;
    struct fontdesc *fdesc;

    x = fontsample_GetLogicalLeft(self);
    y = fontsample_GetLogicalTop(self);
    w = fontsample_GetLogicalWidth(self);
    h = fontsample_GetLogicalHeight(self);
    fdesc = fontsample_GetFontDesc(self);
    if (!fdesc) 
	return;

    fontsample_SetFont(self, fdesc); 
    fontsample_MoveTo(self, x+w/2, y+h/2);
    fontsample_DrawString(self, self->teststring, graphic_BETWEENLEFTANDRIGHT | graphic_BETWEENTOPANDBOTTOM);
}

void fontsample__FullUpdate(self, type, left, top, width, height)
struct fontsample *self;
enum view_UpdateType type;
long left, top, width, height;
{
    RedrawView(self);
}

void fontsample__Update(self)
struct fontsample *self;
{
    fontsample_EraseVisualRect(self); 
    RedrawView(self);
}

void fontsample__ObservedChanged(self, dobj, status)
struct fontsample *self;
struct fontsel *dobj;
long status;
{
    if (status == observable_OBJECTDESTROYED) {
    }
    else if (status == fontsel_DATACHANGED) {
	self->dirty = TRUE;
	fontsample_WantUpdate(self, self);
    }
}

struct view *fontsample__Hit(self, action, x, y, clicks)
struct fontsample *self;
enum view_MouseAction action;
long x, y, clicks;
{
    return NULL;
}

void fontsample__SetString(self, val)
struct fontsample *self;
char *val;
{
    free(self->teststring);
    self->teststring = malloc(strlen(val)+1);
    strcpy(self->teststring, val);
}

struct fontdesc *fontsample__GetFontDesc(self)
struct fontsample *self;
{
    struct fontsel *dobj = (struct fontsel *)fontsample_GetDataObject(self);

    if (self->dirty || !self->fdesc) {
	/* You'd think you'd want to clean up around here, but no, fontdesc_Destroy() segfaults. Sometimes. I will assume this is because of font caching, and ignore it. */
	/*if (self->fdesc)
	    fontdesc_Destroy(self->fdesc);*/

	self->fdesc = fontdesc_Create(fontsel_GetFamily(dobj), fontsel_GetStyle(dobj), fontsel_GetSize(dobj));
	if (!self->fdesc) return NULL;

	self->dirty = FALSE;
    }
    return self->fdesc;
}


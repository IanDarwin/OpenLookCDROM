
/* Copyright 1992 by the Andrew Toolkit Consortium and Carnegie Mellon University. All rights Reserved. */

/*
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
#define NORCSID
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/prefed/RCS/wrapv.c,v 1.4 1992/12/15 21:39:11 rr2b R6tape $";
#endif



 

#include <andrewos.h>
#include <class.h>

#include "wrapv.eh"

#include <dataobj.ih>
#include <view.ih>
#include <menulist.ih>
#include <keystate.ih>

boolean wrapv__InitializeObject(classID, self)
struct classheader *classID;
struct wrapv *self;
{
    self->intv=self->tv=NULL;
    self->t=NULL;
    return TRUE;
}

void wrapv__FinalizeObject(classID, self)
struct classheader *classID;
struct wrapv *self;
{
  if(self->intv!=(struct view *)self->tv) {
	view_DeleteApplicationLayer(self->tv, self->intv);
	self->intv=NULL;
    }
    if(self->t) {
	dataobject_Destroy(self->t);
	self->t=NULL;
    }
    if(self->tv) {
	view_Destroy(self->tv);
	self->tv=NULL;
    }
}
 
void wrapv__LinkTree(self, parent)
struct wrapv *self;
struct view *parent;
{
    super_LinkTree(self, parent);
    if(self->intv) view_LinkTree(self->intv, self);
}

void wrapv__InitChildren(self)
struct wrapv *self;
{
    if(self->intv) view_InitChildren(self->intv);
    else super_InitChildren(self);
}

void wrapv__FullUpdate(self, type, left, top, width, height)
struct wrapv *self;
enum view_UpdateType type;
long left, top, width, height;
{
    struct rectangle r;
    int i;
    
    if(self->intv) {
	wrapv_GetLogicalBounds(self, &r);

	view_InsertView(self->intv, self, &r);

	view_FullUpdate(self->intv, type, left, top, width, height);
    } else super_FullUpdate(self, type, left, top, width, height);
}

void wrapv__Update(self)
struct wrapv *self;
{
    if(self->intv) view_Update(self->intv);
    if(self->tv) view_Update(self->tv);
    else super_Update(self);
}


struct view *wrapv__Hit(self, action, x, y, numberOfClicks)
struct wrapv *self;
enum view_MouseAction action;
long x;
long y;
long numberOfClicks;
{
    if(self->intv) return view_Hit(self->intv, action, x, y, numberOfClicks);
    else return super_Hit(self, action, x, y, numberOfClicks);
}

enum view_DSattributes wrapv__DesiredSize(self, width, height, pass, dWidth, dHeight)
struct wrapv *self;
long width;
long height;
enum view_DSpass pass;
long *dWidth;
long *dHeight;
{
    if(self->intv) return view_DesiredSize(self->intv, width, height, pass, dWidth, dHeight);
    else return super_DesiredSize(self, width, height, pass, dWidth, dHeight);
}

void wrapv__GetOrigin(self, width, height, originX, originY)
struct wrapv *self;
long width;
long height;
long *originX;
long *originY;
{
    if(self->intv) view_GetOrigin(self->intv, width, height, originX, originY);
    else super_GetOrigin(self, width, height, originX, originY);
}

void wrapv__WantInputFocus(self, requestor)
struct wrapv *self;
struct view *requestor;
{
    if((struct view *)self == requestor && self->tv) view_WantInputFocus(self->tv, self->tv);
    else super_WantInputFocus(self, requestor);
}

void wrapv__PostMenus(self, ml)
struct wrapv *self;
struct menulist *ml;
{
    struct menulist *lml=wrapv_Menus(self);

    if(lml) {
	if(ml) menulist_ChainAfterML(lml, ml, ml);
	super_PostMenus(self, lml);
    } else super_PostMenus(self, ml);
}

void wrapv__PostKeyState(self, ks)
struct wrapv *self;
struct keystate *ks;
{
    struct keystate *lks=wrapv_Keys(self);
    if(lks) {
	keystate_AddBefore(lks, ks);
	super_PostKeyState(self, lks);
    } else super_PostKeyState(self, ks);
}

struct keystate *wrapv__Keys(self)
struct wrapv *self;
{
    return NULL;
}

struct menulist *wrapv__Menus(self)
struct wrapv *self;
{
    return NULL;
}

void wrapv__WantUpdate(self, req)
struct wrapv *self;
struct view *req;
{
    super_WantUpdate(self, self);
}

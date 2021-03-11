/* $Author: rr2b $ */

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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/neos/RCS/eframe.c,v 1.3 1992/12/15 21:55:51 rr2b R6tape $";
#endif


 

/* eframe.c 
 * This is a silly little view so we
 * can create a buffer window with our own local
 * command set.
 */

/* ************************************************************
 *  Copyright (C) 1989, 1990, 1991
 *  by the Massachusetts Institute of Technology
 *   For full copyright information see:'mit-copyright.h'     *
 ************************************************************ */

#include <mit-copyright.h>
#include <bind.ih>
#include <buffer.ih>
#include <class.h>
#include <keymap.ih>
#include <keystate.ih>
#include <menulist.ih>
#include <environ.ih>
#include <fontdesc.ih>
#include <frame.ih>
#include <im.ih>
#include <proctbl.ih>
#include <style.ih>
#include <text.ih>
#include <textv.ih>

#include <eframe.eh>

static struct keymap *eframeKeys;
static struct menulist *eframeMenu;

void eframe_SetDefault(self)
    struct eframe *self;
{
    struct textview *tv;
    long dsize, usize, fontStyle;
    enum style_FontSize basis;
    char *font, fontname[100];
    struct buffer *buffer;

    /* tickle the font size if it's different from the default */
    tv = (struct textview *) frame_GetView(self->frame);
    if (strcmp (class_GetTypeName(tv), "textview") == 0) {
	struct style *ds = textview_GetDefaultStyle(tv);

	if ((font = environ_GetProfile("bodyfont")) == NULL || ! fontdesc_ExplodeFontName(font, fontname, sizeof(fontname), &fontStyle, &dsize)) {
	    dsize = 12;
	}

	usize = environ_GetProfileInt("DisplayFontsize", 20);
	style_GetFontSize(ds, &basis, &usize);
	if (basis == style_ConstantFontSize && dsize != usize)
	    style_SetFontSize(ds, style_ConstantFontSize, dsize);
	eframe_WantUpdate(self, self);
    }
}

void eframe_SetDisplay(self)
    struct eframe *self;
{
    struct textview *tv;
    long dsize, usize;
    enum style_FontSize basis;
    struct buffer *buffer;

    /* tickle the font size if it's different from the default */
    tv = (struct textview *) frame_GetView(self->frame);
    if (strcmp (class_GetTypeName(tv), "textview") == 0) {
	struct style *ds = textview_GetDefaultStyle(tv);
	usize = environ_GetProfileInt("DisplayFontsize", 20);
	style_GetFontSize(ds, &basis, &dsize);
	if (basis == style_ConstantFontSize && dsize != usize)
	    style_SetFontSize(ds, style_ConstantFontSize, usize);
	eframe_WantUpdate(self, self);
    }
}

void eframe_NewWindow(self)
    struct eframe *self;
{
    struct eframe *new;
    register struct buffer *buffer;
    struct im *window;
    new = eframe_New();

    window = im_Create(NULL);
    im_SetView(window, new);

    buffer = frame_GetBuffer(self->frame);
    frame_SetBuffer(new->frame, buffer, TRUE);
    eframe_WantInputFocus(new, new);
}

static struct bind_Description eframeBindings[]={
/*  { proc-name,     keybinding,rock, Menu name,        Menu rock, menuflag,
      function, documentation [, module-name]}
 */
    {"eframe-new-window",    "\0302", 0, NULL,         0, EFRAMEMENUS_general,   eframe_NewWindow,      "Creates a new window"},
    {"eframe-set-display-font",    NULL, 0, "View in BIG Font",         0, EFRAMEMENUS_general,   eframe_SetDisplay,      "View buffer in Display Font"},
    {"eframe-set-default-font",    NULL, 0, "View in Default Font",         0, EFRAMEMENUS_general,   eframe_SetDefault,      "View buffer in Default Font"},
    NULL
};

boolean eframe__InitializeClass(classID)
struct classheader *classID;
{
    eframeMenu = menulist_New();
    eframeKeys = keymap_New();
    bind_BindList(eframeBindings, eframeKeys, eframeMenu, &eframe_classinfo);

    return TRUE;
}

void eframe__PostMenus(self, menulist)
struct eframe *self;
struct menulist *menulist;
{
    /* Received the children's menus. Want to add our own options,
           then pass menulist up to superclass */
    menulist_ClearChain(self->menus);
    menulist_SetMask(self->menus, self->menuflags);
    if (menulist) menulist_ChainAfterML(self->menus, menulist, menulist);

    super_PostMenus(self,self->menus); 
}


void eframe__ReceiveInputFocus(self)
struct eframe *self;
/* 
  We want to ensure that when we receive the focus, the focus is passed on
  into the editing window
 */

{
    eframe_WantInputFocus(self, frame_GetView(self->frame));
}

boolean eframe__InitializeObject(classID, self)
struct classheader *classID;
register struct eframe *self;
{
    self->frame = frame_New();
    frame_SetCommandEnable(self->frame, TRUE);

    /* Menus */
    self->menuflags = EFRAMEMENUS_general;
    self->menus = menulist_DuplicateML(eframeMenu, self);

    self->keys = keystate_Create(self, eframeKeys);

   /* Place the end result into the view-tree */
    frame_LinkTree(self->frame, self);
   
    return TRUE;
}

void eframe__FinalizeObject(classID, self)
struct classheader *classID;
register struct eframe *self;
{
    if (!self) return;

    if (self->menus) menulist_Destroy(self->menus);
    if (self->keys) keystate_Destroy(self->keys);
}

void eframe__LinkTree(self, parent)
struct eframe *self;
struct view *parent;
{
    super_LinkTree(self, parent);

    if (self->frame)
        frame_LinkTree(self->frame, self);
}

void eframe__FullUpdate(self, type, left, top, width, height)
register struct eframe *self;
enum view_UpdateType type;
long left, top, width, height;
/* Replace the 'eframe' view with the view wanted: self->frame */
{
    struct rectangle childRect;

    eframe_GetLogicalBounds(self, &childRect);
    frame_InsertView(self->frame, self, &childRect);
    frame_FullUpdate(self->frame, type, left, top, width, height);

}

struct view *eframe__Hit(self, action, x, y, clicks)
struct eframe *self;
enum view_MouseAction action;
long x, y, clicks;
/*
  All we want to do is to pass the hit into the sub-views
 */
{
    return frame_Hit(self->frame, action, x, y, clicks);
}

void eframe__PostKeyState(self, ks)
struct eframe *self;
struct keystate *ks;
/* Want to add our own keybindings into the chain that gets passed to us */
{
    if (!ks) return;

    self->keys->next = NULL;
    keystate_AddBefore(self->keys, ks); 
    super_PostKeyState(self, self->keys);
}

void eframe__Update(self)
register struct eframe *self;
{
    eframe_EraseVisualRect(self);
    eframe_FullUpdate(self, view_FullRedraw, eframe_GetLogicalTop(self), eframe_GetLogicalLeft(self), eframe_GetLogicalWidth(self), eframe_GetLogicalHeight(self));
}


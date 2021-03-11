/*****************************************************************************/
/**       Copyright 1988 by Evans & Sutherland Computer Corporation,        **/
/**                          Salt Lake City, Utah                           **/
/**  Portions Copyright 1989 by the Massachusetts Institute of Technology   **/
/**                        Cambridge, Massachusetts                         **/
/**                                                                         **/
/**                           All Rights Reserved                           **/
/**                                                                         **/
/**    Permission to use, copy, modify, and distribute this software and    **/
/**    its documentation  for  any  purpose  and  without  fee is hereby    **/
/**    granted, provided that the above copyright notice appear  in  all    **/
/**    copies and that both  that  copyright  notice  and  this  permis-    **/
/**    sion  notice appear in supporting  documentation,  and  that  the    **/
/**    names of Evans & Sutherland and M.I.T. not be used in advertising    **/
/**    in publicity pertaining to distribution of the  software  without    **/
/**    specific, written prior permission.                                  **/
/**                                                                         **/
/**    EVANS & SUTHERLAND AND M.I.T. DISCLAIM ALL WARRANTIES WITH REGARD    **/
/**    TO THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES  OF  MERCHANT-    **/
/**    ABILITY  AND  FITNESS,  IN  NO  EVENT SHALL EVANS & SUTHERLAND OR    **/
/**    M.I.T. BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL  DAM-    **/
/**    AGES OR  ANY DAMAGES WHATSOEVER  RESULTING FROM LOSS OF USE, DATA    **/
/**    OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER    **/
/**    TORTIOUS ACTION, ARISING OUT OF OR IN  CONNECTION  WITH  THE  USE    **/
/**    OR PERFORMANCE OF THIS SOFTWARE.                                     **/
/*****************************************************************************/
/* 
 *  [ ctwm ]
 *
 *  Copyright 1992 Claude Lecommandeur.
 *            
 * Permission to use, copy, modify  and distribute this software  [ctwm] and
 * its documentation for any purpose is hereby granted without fee, provided
 * that the above  copyright notice appear  in all copies and that both that
 * copyright notice and this permission notice appear in supporting documen-
 * tation, and that the name of  Claude Lecommandeur not be used in adverti-
 * sing or  publicity  pertaining to  distribution of  the software  without
 * specific, written prior permission. Claude Lecommandeur make no represen-
 * tations  about the suitability  of this software  for any purpose.  It is
 * provided "as is" without express or implied warranty.
 *
 * Claude Lecommandeur DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
 * INCLUDING ALL  IMPLIED WARRANTIES OF  MERCHANTABILITY AND FITNESS.  IN NO
 * EVENT SHALL  Claude Lecommandeur  BE LIABLE FOR ANY SPECIAL,  INDIRECT OR
 * CONSEQUENTIAL  DAMAGES OR ANY  DAMAGES WHATSOEVER  RESULTING FROM LOSS OF
 * USE, DATA  OR PROFITS,  WHETHER IN AN ACTION  OF CONTRACT,  NEGLIGENCE OR
 * OTHER  TORTIOUS ACTION,  ARISING OUT OF OR IN  CONNECTION WITH THE USE OR
 * PERFORMANCE OF THIS SOFTWARE.
 *
 * Author:  Claude Lecommandeur [ lecom@sic.epfl.ch ][ April 1992 ]
 */


/***********************************************************************
 *
 * $XConsortium: menus.c,v 1.186 91/07/17 13:58:00 dave Exp $
 *
 * twm menu code
 *
 * 17-Nov-87 Thomas E. LaStrange		File created
 *
 * Do the necessary modification to be integrated in ctwm.
 * Can no longer be used for the standard twm.
 *
 * 22-April-92 Claude Lecommandeur.
 *
 *
 ***********************************************************************/

#if defined(USE_SIGNALS) && defined(__sgi)
#  define _BSD_SIGNALS
#endif

#include <stdio.h>
#include <signal.h>
#ifdef VMS
#include <string.h>
#include <unixio.h>
#include <file.h>
#include <decw$include/Xos.h>
#else
#include <X11/Xos.h>
#endif
#include "twm.h"
#include "gc.h"
#include "menus.h"
#include "resize.h"
#include "events.h"
#include "util.h"
#include "parse.h"
#include "gram.h"
#include "screen.h"
#ifdef VMS
#include <X11Xmu/CharSet.h>
#include <decw$bitmaps/menu12.xbm>
#include "vms_cmd_services.h"
#include <lib$routines.h>
#else
#include <X11/Xmu/CharSet.h>
#include <X11/bitmaps/menu12>
#endif
#include "version.h"

#if defined(MACH) || defined(sony_news)
#define lrand48 random
#endif
#ifdef VMS
#define lrand48 rand
#endif

#ifndef VMS
#define MAX(x,y) ((x)>(y)?(x):(y))
#define MIN(x,y) ((x)<(y)?(x):(y))
#endif
#define ABS(x) ((x)<0?-(x):(x))

#ifdef SOUNDS
extern int toggle_sound();
extern int reread_sounds();
#endif

extern XEvent Event;

int RootFunction = 0;
MenuRoot *ActiveMenu = NULL;		/* the active menu */
MenuItem *ActiveItem = NULL;		/* the active menu item */
int MoveFunction;			/* either F_MOVE or F_FORCEMOVE */
int WindowMoved = FALSE;
int menuFromFrameOrWindowOrTitlebar = FALSE;
char *CurrentSelectedWorkspace;

extern int captive;

int ConstMove = FALSE;		/* constrained move variables */
int ConstMoveDir;
int ConstMoveX;
int ConstMoveY;
int ConstMoveXL;
int ConstMoveXR;
int ConstMoveYT;
int ConstMoveYB;
 
/* Globals used to keep track of whether the mouse has moved during
   a resize function. */
int ResizeOrigX;
int ResizeOrigY;

int MenuDepth = 0;		/* number of menus up */
static struct {
    int x;
    int y;
} MenuOrigins[MAXMENUDEPTH];
static Cursor LastCursor;

void WarpAlongRing(), WarpToWindow();
void DisplayPosition ();

extern char *Action;
extern int Context;
extern TwmWindow *ButtonWindow, *Tmp_win;
extern XEvent Event, ButtonEvent;
extern char *InitFile;
static void Identify();

#define SHADOWWIDTH 5			/* in pixels */



/***********************************************************************
 *
 *  Procedure:
 *	InitMenus - initialize menu roots
 *
 ***********************************************************************
 */

void
InitMenus()
{
    Scr->DefaultFunction.func = 0;
    Scr->WindowFunction.func  = 0;
    Scr->ChangeWorkspaceFunction.func  = 0;
    Scr->DeIconifyFunction.func  = 0;
    Scr->IconifyFunction.func  = 0;

    Scr->FuncKeyRoot.next = NULL;
    Scr->FuncButtonRoot.next = NULL;
}



/***********************************************************************
 *
 *  Procedure:
 *	AddFuncKey - add a function key to the list
 *
 *  Inputs:
 *	name	- the name of the key
 *	cont	- the context to look for the key press in
 *	mods	- modifier keys that need to be pressed
 *	func	- the function to perform
 *	win_name- the window name (if any)
 *	action	- the action string associated with the function (if any)
 *
 ***********************************************************************
 */

Bool AddFuncKey (name, cont, mods, func, menu, win_name, action)
    char *name;
    int cont, mods, func;
    MenuRoot *menu;
    char *win_name;
    char *action;
{
    FuncKey *tmp;
    KeySym keysym;
    KeyCode keycode;

    /*
     * Don't let a 0 keycode go through, since that means AnyKey to the
     * XGrabKey call in GrabKeys().
     */
    if ((keysym = XStringToKeysym(name)) == NoSymbol ||
	(keycode = XKeysymToKeycode(dpy, keysym)) == 0)
    {
	return False;
    }

    /* see if there already is a key defined for this context */
    for (tmp = Scr->FuncKeyRoot.next; tmp != NULL; tmp = tmp->next)
    {
	if (tmp->keysym == keysym &&
	    tmp->cont == cont &&
	    tmp->mods == mods)
	    return (True);
    }

    if (tmp == NULL)
    {
	tmp = (FuncKey *) malloc(sizeof(FuncKey));
	tmp->next = Scr->FuncKeyRoot.next;
	Scr->FuncKeyRoot.next = tmp;
    }

    tmp->name = name;
    tmp->keysym = keysym;
    tmp->keycode = keycode;
    tmp->cont = cont;
    tmp->mods = mods;
    tmp->func = func;
    tmp->menu = menu;
    tmp->win_name = win_name;
    tmp->action = action;

    return True;
}

/***********************************************************************
 *
 *  Procedure:
 *	AddFuncButton - add a function button to the list
 *
 *  Inputs:
 *	num	- the num of the button
 *	cont	- the context to look for the key press in
 *	mods	- modifier keys that need to be pressed
 *	func	- the function to perform
 *	menu    - the menu (if any)
 *	item	- the menu item (if any)
 *
 ***********************************************************************
 */

Bool AddFuncButton (num, cont, mods, func, menu, item)
    int  num, cont, mods, func;
    MenuRoot *menu;
    MenuItem *item;
{
    FuncButton *tmp;

    /* see if there already is a key defined for this context */
    for (tmp = Scr->FuncButtonRoot.next; tmp != NULL; tmp = tmp->next) {
	if ((tmp->num == num) && (tmp->cont == cont) && (tmp->mods == mods))
	    return (True);
    }

    if (tmp == NULL) {
	tmp = (FuncButton*) malloc (sizeof (FuncButton));
	tmp->next = Scr->FuncButtonRoot.next;
	Scr->FuncButtonRoot.next = tmp;
    }

    tmp->num  = num;
    tmp->cont = cont;
    tmp->mods = mods;
    tmp->func = func;
    tmp->menu = menu;
    tmp->item = item;

    return True;
}



static TitleButton *cur_tb = NULL;

void ModifyCurrentTB(button, func, action, menuroot)
    int button;
    int func;
    char *action;
    MenuRoot *menuroot;
{
    if (!cur_tb) {
        fprintf (stderr, "%s: can't find titlebutton\n", ProgramName);
      return;
    }
    cur_tb->funs[button - 1].func = func;
    cur_tb->funs[button - 1].action = action;
    cur_tb->funs[button - 1].menuroot = menuroot;
}

int CreateTitleButton (name, func, action, menuroot, rightside, append)
    char *name;
    int func;
    char *action;
    MenuRoot *menuroot;
    Bool rightside;
    Bool append;
{
    int button;
    cur_tb = (TitleButton *) malloc (sizeof(TitleButton));

    if (!cur_tb) {
	fprintf (stderr,
		 "%s:  unable to allocate %d bytes for title button\n",
		 ProgramName, sizeof(TitleButton));
	return 0;
    }

    cur_tb->next = NULL;
    cur_tb->name = name;                      /* note that we are not copying */
    cur_tb->image = None;                     /* WARNING, values not set yet */
    cur_tb->width = 0;                        /* see InitTitlebarButtons */
    cur_tb->height = 0;                       /* ditto */
    cur_tb->rightside = rightside;
    if (rightside) {
	Scr->TBInfo.nright++;
    } else {
	Scr->TBInfo.nleft++;
    }

    for(button = 0; button < MAX_BUTTONS; button++){
	cur_tb->funs[button].func = func;
	cur_tb->funs[button].action = action;
	cur_tb->funs[button].menuroot = menuroot;
    }

    /*
     * Cases for list:
     * 
     *     1.  empty list, prepend left       put at head of list
     *     2.  append left, prepend right     put in between left and right
     *     3.  append right                   put at tail of list
     *
     * Do not refer to widths and heights yet since buttons not created
     * (since fonts not loaded and heights not known).
     */
    if ((!Scr->TBInfo.head) || ((!append) && (!rightside))) {	/* 1 */
	cur_tb->next = Scr->TBInfo.head;
	Scr->TBInfo.head = cur_tb;
    } else if (append && rightside) {	/* 3 */
	register TitleButton *t;
	for /* SUPPRESS 530 */
	  (t = Scr->TBInfo.head; t->next; t = t->next);
	t->next = cur_tb;
	cur_tb->next = NULL;
    } else {				/* 2 */
	register TitleButton *t, *prev = NULL;
	for (t = Scr->TBInfo.head; t && !t->rightside; t = t->next) {
	    prev = t;
	}
	if (prev) {
	    cur_tb->next = prev->next;
	    prev->next = cur_tb;
	} else {
	    cur_tb->next = Scr->TBInfo.head;
	    Scr->TBInfo.head = cur_tb;
	}
    }

    return 1;
}



/*
 * InitTitlebarButtons - Do all the necessary stuff to load in a titlebar
 * button.  If we can't find the button, then put in a question; if we can't
 * find the question mark, something is wrong and we are probably going to be
 * in trouble later on.
 */
void InitTitlebarButtons ()
{
    TitleButton *tb;
    int h;

    /*
     * initialize dimensions
     */
    Scr->TBInfo.width = (Scr->TitleHeight -
			 2 * (Scr->FramePadding + Scr->ButtonIndent));
    if (Scr->use3Dtitles) 
	Scr->TBInfo.pad = ((Scr->TitlePadding > 1)
		       ? ((Scr->TitlePadding + 1) / 2) : 0);
    else
	Scr->TBInfo.pad = ((Scr->TitlePadding > 1)
		       ? ((Scr->TitlePadding + 1) / 2) : 1);
    h = Scr->TBInfo.width - 2 * Scr->TBInfo.border;

    /*
     * add in some useful buttons and bindings so that novices can still
     * use the system.
     */
    if (!Scr->NoDefaults) {
	/* insert extra buttons */
	if (Scr->use3Dtitles) {
	    if (!CreateTitleButton (TBPM_3DDOT, F_ICONIFY, "", (MenuRoot *) NULL,
				False, False)) {
	        fprintf (stderr, "%s:  unable to add iconify button\n", ProgramName);
	    }
	    if (!CreateTitleButton (TBPM_3DRESIZE, F_RESIZE, "", (MenuRoot *) NULL,
				True, True)) {
	        fprintf (stderr, "%s:  unable to add resize button\n", ProgramName);
	    }
	}
	else {
	    if (!CreateTitleButton (TBPM_ICONIFY, F_ICONIFY, "", (MenuRoot *) NULL,
				False, False)) {
	        fprintf (stderr, "%s:  unable to add iconify button\n", ProgramName);
	    }
	    if (!CreateTitleButton (TBPM_RESIZE, F_RESIZE, "", (MenuRoot *) NULL,
				True, True)) {
	        fprintf (stderr, "%s:  unable to add resize button\n", ProgramName);
	    }
	}
	AddDefaultBindings ();
    }
    ComputeCommonTitleOffsets ();

    /*
     * load in images and do appropriate centering
     */

    for (tb = Scr->TBInfo.head; tb; tb = tb->next) {
	tb->image = GetImage (tb->name, Scr->TitleC);
	if (!tb->image) {
	    tb->image = GetImage (TBPM_QUESTION, Scr->TitleC);
	    if (!tb->image) {		/* cannot happen (see util.c) */
		fprintf (stderr, "%s:  unable to add titlebar button \"%s\"\n",
			 ProgramName, tb->name);
	    }
	}
	tb->width  = tb->image->width;
	tb->height = tb->image->height;
	tb->dstx = (h - tb->width + 1) / 2;
	if (tb->dstx < 0) {		/* clip to minimize copying */
	    tb->srcx = -(tb->dstx);
	    tb->width = h;
	    tb->dstx = 0;
	} else {
	    tb->srcx = 0;
	}
	tb->dsty = (h - tb->height + 1) / 2;
	if (tb->dsty < 0) {
	    tb->srcy = -(tb->dsty);
	    tb->height = h;
	    tb->dsty = 0;
	} else {
	    tb->srcy = 0;
	}
    }
}


PaintEntry(mr, mi, exposure)
MenuRoot *mr;
MenuItem *mi;
int exposure;
{
    if (Scr->use3Dmenus)
	Paint3DEntry (mr, mi, exposure);
    else 
	PaintNormalEntry (mr, mi, exposure);
    if (mi->state) mr->lastactive = mi;
}

Paint3DEntry(mr, mi, exposure)
MenuRoot *mr;
MenuItem *mi;
int exposure;
{
    int y_offset;
    int text_y;
    GC gc;

    y_offset = mi->item_num * Scr->EntryHeight + 2;
    text_y = y_offset + Scr->MenuFont.y + 2;

    if (mi->func != F_TITLE) {
	int x, y;

	if (mi->state) {
	    Draw3DBorder (mr->w, 2, y_offset, mr->width - 4, Scr->EntryHeight, 1, 
				mi->highlight, off, True, False);
	    FBF(mi->highlight.fore, mi->highlight.back, Scr->MenuFont.font->fid);
	    XDrawImageString(dpy, mr->w, Scr->NormalGC, mi->x + 2, text_y, mi->item, mi->strlen);
	    gc = Scr->NormalGC;
	}
	else {
	    if (mi->user_colors || !exposure) {
		XSetForeground (dpy, Scr->NormalGC, mi->normal.back);
		XFillRectangle (dpy, mr->w, Scr->NormalGC, 2, y_offset,
			mr->width - 4, Scr->EntryHeight);
		FBF (mi->normal.fore, mi->normal.back, Scr->MenuFont.font->fid);
		gc = Scr->NormalGC;
	    }
	    else {
		gc = Scr->MenuGC;
	    }
	    XDrawImageString (dpy, mr->w, gc, mi->x + 2, text_y, mi->item, mi->strlen);
	    if (mi->separated) {
		FB (Scr->MenuC.shadd, Scr->MenuC.shadc);
		XDrawLine (dpy, mr->w, Scr->NormalGC, 1, y_offset + Scr->MenuFont.y + 5,
				mr->width - 2, y_offset + Scr->MenuFont.y + 5);
		FB (Scr->MenuC.shadc, Scr->MenuC.shadd);
		XDrawLine (dpy, mr->w, Scr->NormalGC, 2, y_offset + Scr->MenuFont.y + 6,
				mr->width - 3, y_offset + Scr->MenuFont.y + 6);
	    }
	}

	if (mi->func == F_MENU) {
	    /* create the pull right pixmap if needed */
	    if (Scr->pullPm == None)
	    {
		Scr->pullPm = Create3DMenuIcon (Scr->MenuFont.height, &Scr->pullW,
				&Scr->pullH, Scr->MenuC);
	    }
	    x = mr->width - Scr->pullW - 5;
	    y = y_offset + ((Scr->MenuFont.height - Scr->pullH) / 2) + 2;
	    XCopyArea (dpy, Scr->pullPm, mr->w, gc, 0, 0, Scr->pullW, Scr->pullH, x, y);
	}
    }
    else
    {
	Draw3DBorder (mr->w, 2, y_offset, mr->width - 4, Scr->EntryHeight, 1, 
			mi->normal, off, True, False);
	FBF (mi->normal.fore, mi->normal.back, Scr->MenuFont.font->fid);
	XDrawImageString (dpy, mr->w, Scr->NormalGC, mi->x + 2, text_y, mi->item, mi->strlen);
    }
}
    


PaintNormalEntry(mr, mi, exposure)
MenuRoot *mr;
MenuItem *mi;
int exposure;
{
    int y_offset;
    int text_y;
    GC gc;

    y_offset = mi->item_num * Scr->EntryHeight;
    text_y = y_offset + Scr->MenuFont.y;

    if (mi->func != F_TITLE)
    {
	int x, y;

	if (mi->state)
	{
	    XSetForeground(dpy, Scr->NormalGC, mi->highlight.back);

	    XFillRectangle(dpy, mr->w, Scr->NormalGC, 0, y_offset,
		mr->width, Scr->EntryHeight);
	    FBF(mi->highlight.fore, mi->highlight.back, Scr->MenuFont.font->fid);

	    XDrawString(dpy, mr->w, Scr->NormalGC, mi->x,
		text_y, mi->item, mi->strlen);

	    gc = Scr->NormalGC;
	}
	else
	{
	    if (mi->user_colors || !exposure)
	    {
		XSetForeground(dpy, Scr->NormalGC, mi->normal.back);

		XFillRectangle(dpy, mr->w, Scr->NormalGC, 0, y_offset,
		    mr->width, Scr->EntryHeight);

		FBF(mi->normal.fore, mi->normal.back, Scr->MenuFont.font->fid);
		gc = Scr->NormalGC;
	    }
	    else {
		gc = Scr->MenuGC;
	    }

	    XDrawString(dpy, mr->w, gc, mi->x,
		text_y, mi->item, mi->strlen);
	    if (mi->separated)
		XDrawLine (dpy, mr->w, gc, 0, y_offset + Scr->MenuFont.y + 5,
				mr->width, y_offset + Scr->MenuFont.y + 5);
	}

	if (mi->func == F_MENU)
	{
	    /* create the pull right pixmap if needed */
	    if (Scr->pullPm == None)
	    {
		Scr->pullPm = CreateMenuIcon (Scr->MenuFont.height,
					     &Scr->pullW, &Scr->pullH);
	    }
	    x = mr->width - Scr->pullW - 5;
	    y = y_offset + ((Scr->MenuFont.height - Scr->pullH) / 2);
	    XCopyPlane(dpy, Scr->pullPm, mr->w, gc, 0, 0,
		Scr->pullW, Scr->pullH, x, y, 1);
	}
    }
    else
    {
	int y;

	XSetForeground(dpy, Scr->NormalGC, mi->normal.back);

	/* fill the rectangle with the title background color */
	XFillRectangle(dpy, mr->w, Scr->NormalGC, 0, y_offset,
	    mr->width, Scr->EntryHeight);

	{
	    XSetForeground(dpy, Scr->NormalGC, mi->normal.fore);
	    /* now draw the dividing lines */
	    if (y_offset)
	      XDrawLine (dpy, mr->w, Scr->NormalGC, 0, y_offset,
			 mr->width, y_offset);
	    y = ((mi->item_num+1) * Scr->EntryHeight)-1;
	    XDrawLine(dpy, mr->w, Scr->NormalGC, 0, y, mr->width, y);
	}

	FBF(mi->normal.fore, mi->normal.back, Scr->MenuFont.font->fid);
	/* finally render the title */
	XDrawString(dpy, mr->w, Scr->NormalGC, mi->x,
	    text_y, mi->item, mi->strlen);
    }
}
    
PaintMenu(mr, e)
MenuRoot *mr;
XEvent *e;
{
    MenuItem *mi;

    if (Scr->use3Dmenus) {
	Draw3DBorder (mr->w, 0, 0, mr->width, mr->height, 2, Scr->MenuC, off, False, False);
    }
    for (mi = mr->first; mi != NULL; mi = mi->next)
    {
	int y_offset = mi->item_num * Scr->EntryHeight;

	/* be smart about handling the expose, redraw only the entries
	 * that we need to
	 */
	if (e->xexpose.y <= (y_offset + Scr->EntryHeight) &&
	    (e->xexpose.y + e->xexpose.height) >= y_offset)
	{
	    PaintEntry(mr, mi, True);
	}
    }
    XSync(dpy, 0);
}



MakeWorkspacesMenu () {
    static char **actions = NULL;
    WorkSpaceList *wlist;
    char **act;

    if (! Scr->Workspaces) return;
    AddToMenu (Scr->Workspaces, "TWM Workspaces", NULLSTR, NULL, F_TITLE, NULLSTR, NULLSTR);
    if (! actions) {
	int count = 0;

        for (wlist = Scr->workSpaceMgr.workSpaceList; wlist != NULL; wlist = wlist->next) {
            count++;
        }
	count++;
	actions = (char**) malloc (count * sizeof (char*));
	act = actions;
        for (wlist = Scr->workSpaceMgr.workSpaceList; wlist != NULL; wlist = wlist->next) {
	    *act = (char*) malloc (strlen ("WGOTO : ") + strlen (wlist->name) + 1);
	    sprintf (*act, "WGOTO : %s", wlist->name);
	    act++;
	}
	*act = NULL;
    }
    act = actions;
    for (wlist = Scr->workSpaceMgr.workSpaceList; wlist != NULL; wlist = wlist->next) {
        AddToMenu (Scr->Workspaces, wlist->name, *act, Scr->Windows, F_MENU, NULL, NULL);
	act++;
    }
    Scr->Workspaces->pinned = False;
    MakeMenu (Scr->Workspaces);
}

static Bool fromMenu;

UpdateMenu()
{
    MenuItem *mi;
    int i, x, y, x_root, y_root, entry;
    int done;
    MenuItem *badItem = NULL;

    fromMenu = TRUE;

    while (TRUE)
    {
	/* block until there is an event */
        if (!menuFromFrameOrWindowOrTitlebar) {
	  XMaskEvent(dpy,
		     ButtonPressMask | ButtonReleaseMask |
		     KeyPressMask | KeyReleaseMask |
		     EnterWindowMask | ExposureMask |
		     VisibilityChangeMask | LeaveWindowMask |
		     ButtonMotionMask, &Event);
	}
	if (Event.type == MotionNotify) {
	    /* discard any extra motion events before a release */
	    while(XCheckMaskEvent(dpy,
		ButtonMotionMask | ButtonReleaseMask, &Event))
		if (Event.type == ButtonRelease)
		    break;
	}

	if (!DispatchEvent ())
	    continue;

	if ((! ActiveMenu) || Cancel) {
	  menuFromFrameOrWindowOrTitlebar = FALSE;
	  fromMenu = FALSE;
	  return (0);
	}

	if (Event.type != MotionNotify)
	    continue;

	done = FALSE;
	XQueryPointer( dpy, ActiveMenu->w, &JunkRoot, &JunkChild,
	    &x_root, &y_root, &x, &y, &JunkMask);

	/* if we haven't recieved the enter notify yet, wait */
	if (ActiveMenu && !ActiveMenu->entered)
	    continue;

	XFindContext(dpy, ActiveMenu->w, ScreenContext, (caddr_t *)&Scr);

	if (x < 0 || y < 0 ||
	    x >= ActiveMenu->width || y >= ActiveMenu->height)
	{
	    if (ActiveItem && ActiveItem->func != F_TITLE)
	    {
		ActiveItem->state = 0;
		PaintEntry(ActiveMenu, ActiveItem, False);
	    }
	    ActiveItem = NULL;
	    continue;
	}

	/* look for the entry that the mouse is in */
	entry = y / Scr->EntryHeight;
	for (i = 0, mi = ActiveMenu->first; mi != NULL; i++, mi=mi->next)
	{
	    if (i == entry)
		break;
	}

	/* if there is an active item, we might have to turn it off */
	if (ActiveItem)
	{
	    /* is the active item the one we are on ? */
	    if (ActiveItem->item_num == entry && ActiveItem->state)
		done = TRUE;

	    /* if we weren't on the active entry, let's turn the old
	     * active one off 
	     */
	    if (!done && ActiveItem->func != F_TITLE)
	    {
		ActiveItem->state = 0;
		PaintEntry(ActiveMenu, ActiveItem, False);
	    }
	}

	/* if we weren't on the active item, change the active item and turn
	 * it on 
	 */
	if (!done)
	{
	    ActiveItem = mi;
	    if (ActiveItem && ActiveItem->func != F_TITLE && !ActiveItem->state)
	    {
		ActiveItem->state = 1;
		PaintEntry(ActiveMenu, ActiveItem, False);
	    }
	}

	/* now check to see if we were over the arrow of a pull right entry */
	if (ActiveItem && ActiveItem->func == F_MENU && 
	   ((ActiveMenu->width - x) < (ActiveMenu->width / 3)))
	{
	    MenuRoot *save = ActiveMenu;
	    int savex = MenuOrigins[MenuDepth - 1].x; 
	    int savey = MenuOrigins[MenuDepth - 1].y;

	    if (MenuDepth < MAXMENUDEPTH) {
		if (ActiveMenu == Scr->Workspaces)
		    CurrentSelectedWorkspace = ActiveItem->item;
		PopUpMenu (ActiveItem->sub, 
			   (savex + (((2 * ActiveMenu->width) / 3) - 1)), 
			   (savey + ActiveItem->item_num * Scr->EntryHeight)
			   /*(savey + ActiveItem->item_num * Scr->EntryHeight +
			    (Scr->EntryHeight >> 1))*/, False);
		CurrentSelectedWorkspace = NULL;
	    } else if (!badItem) {
		XBell (dpy, 0);
		badItem = ActiveItem;
	    }

	    /* if the menu did get popped up, unhighlight the active item */
	    if (save != ActiveMenu && ActiveItem->state)
	    {
		ActiveItem->state = 0;
		PaintEntry(save, ActiveItem, False);
		ActiveItem = NULL;
	    }
	}
	if (badItem != ActiveItem) badItem = NULL;
	XFlush(dpy);
    }
}



/***********************************************************************
 *
 *  Procedure:
 *	NewMenuRoot - create a new menu root
 *
 *  Returned Value:
 *	(MenuRoot *)
 *
 *  Inputs:
 *	name	- the name of the menu root
 *
 ***********************************************************************
 */

MenuRoot *
NewMenuRoot(name)
    char *name;
{
    MenuRoot *tmp;

#define UNUSED_PIXEL ((unsigned long) (~0))	/* more than 24 bits */

    tmp = (MenuRoot *) malloc(sizeof(MenuRoot));
    tmp->highlight.fore = UNUSED_PIXEL;
    tmp->highlight.back = UNUSED_PIXEL;
    tmp->name = name;
    tmp->prev = NULL;
    tmp->first = NULL;
    tmp->last = NULL;
    tmp->items = 0;
    tmp->width = 0;
    tmp->mapped = NEVER_MAPPED;
    tmp->pull = FALSE;
    tmp->w = None;
    tmp->shadow = None;
    tmp->real_menu = FALSE;

    if (Scr->MenuList == NULL)
    {
	Scr->MenuList = tmp;
	Scr->MenuList->next = NULL;
    }

    if (Scr->LastMenu == NULL)
    {
	Scr->LastMenu = tmp;
	Scr->LastMenu->next = NULL;
    }
    else
    {
	Scr->LastMenu->next = tmp;
	Scr->LastMenu = tmp;
	Scr->LastMenu->next = NULL;
    }

    if (strcmp(name, TWM_WINDOWS) == 0)
	Scr->Windows = tmp;

    if (strcmp(name, TWM_WORKSPACES) == 0)
	Scr->Workspaces = tmp;

    if (strcmp(name, TWM_ALLWINDOWS) == 0)
	Scr->AllWindows = tmp;

    return (tmp);
}



/***********************************************************************
 *
 *  Procedure:
 *	AddToMenu - add an item to a root menu
 *
 *  Returned Value:
 *	(MenuItem *)
 *
 *  Inputs:
 *	menu	- pointer to the root menu to add the item
 *	item	- the text to appear in the menu
 *	action	- the string to possibly execute
 *	sub	- the menu root if it is a pull-right entry
 *	func	- the numeric function
 *	fore	- foreground color string
 *	back	- background color string
 *
 ***********************************************************************
 */

MenuItem *
AddToMenu(menu, item, action, sub, func, fore, back)
    MenuRoot *menu;
    char *item, *action;
    MenuRoot *sub;
    int func;
    char *fore, *back;
{
    MenuItem *tmp;
    int width;

#ifdef DEBUG_MENUS
    fprintf(stderr, "adding menu item=\"%s\", action=%s, sub=%d, f=%d\n",
	item, action, sub, func);
#endif

    tmp = (MenuItem *) malloc(sizeof(MenuItem));
    tmp->root = menu;

    if (menu->first == NULL)
    {
	menu->first = tmp;
	tmp->prev = NULL;
    }
    else
    {
	menu->last->next = tmp;
	tmp->prev = menu->last;
    }
    menu->last = tmp;

    tmp->item = item;
    tmp->strlen = strlen(item);
    tmp->action = action;
    tmp->next = NULL;
    tmp->sub = NULL;
    tmp->state = 0;
    tmp->func = func;
    tmp->separated = 0;

    if (!Scr->HaveFonts) CreateFonts();
    width = XTextWidth(Scr->MenuFont.font, item, tmp->strlen);
    if (width <= 0)
	width = 1;
    if (width > menu->width)
	menu->width = width;

    tmp->user_colors = FALSE;
    if (Scr->Monochrome == COLOR && fore != NULL)
    {
	int save;

	save = Scr->FirstTime;
	Scr->FirstTime = TRUE;
	GetColor(COLOR, &tmp->normal.fore, fore);
	GetColor(COLOR, &tmp->normal.back, back);
	if (Scr->use3Dmenus && !Scr->BeNiceToColormap) GetShadeColors (&tmp->normal);
	Scr->FirstTime = save;
	tmp->user_colors = TRUE;
    }
    if (sub != NULL)
    {
	tmp->sub = sub;
	menu->pull = TRUE;
    }
    tmp->item_num = menu->items++;

    return (tmp);
}



MakeMenus()
{
    MenuRoot *mr;

    for (mr = Scr->MenuList; mr != NULL; mr = mr->next)
    {
	if (mr->real_menu == FALSE)
	    continue;

	mr->pinned = False;
	MakeMenu(mr);
    }
}



MakeMenu(mr)
MenuRoot *mr;
{
    MenuItem *start, *end, *cur, *tmp;
    XColor f1, f2, f3;
    XColor b1, b2, b3;
    XColor save_fore, save_back;
    int num, i;
    int fred, fgreen, fblue;
    int bred, bgreen, bblue;
    int width, borderwidth;
    unsigned long valuemask;
    XSetWindowAttributes attributes;
    Colormap cmap = Scr->TwmRoot.cmaps.cwins[0]->colormap->c;

    Scr->EntryHeight = Scr->MenuFont.height + 4;

    /* lets first size the window accordingly */
    if (mr->mapped == NEVER_MAPPED)
    {
	if (mr->pull == TRUE) {
	    mr->width += 16 + 10;
	}
	width = mr->width + 10;
	for (cur = mr->first; cur != NULL; cur = cur->next) {
	    if (cur->func != F_TITLE)
		cur->x = 5;
	    else {
		cur->x = width - XTextWidth(Scr->MenuFont.font, cur->item,
				cur->strlen);
		cur->x /= 2;
	    }
	}
	mr->height = mr->items * Scr->EntryHeight;
	if (Scr->use3Dmenus) mr->height += 4;
	mr->width += 10;
	if (Scr->Shadow && ! mr->pinned)
	{
	    /*
	     * Make sure that you don't draw into the shadow window or else
	     * the background bits there will get saved
	     */
	    valuemask = (CWBackPixel | CWBorderPixel);
	    attributes.background_pixel = Scr->MenuShadowColor;
	    attributes.border_pixel = Scr->MenuShadowColor;
	    if (Scr->SaveUnder) {
		valuemask |= CWSaveUnder;
		attributes.save_under = True;
	    }
	    mr->shadow = XCreateWindow (dpy, Scr->Root, 0, 0,
					(unsigned int) mr->width, 
					(unsigned int) mr->height,
					(unsigned int)0,
					CopyFromParent, 
					(unsigned int) CopyFromParent,
					(Visual *) CopyFromParent,
					valuemask, &attributes);
	}

	valuemask = (CWBackPixel | CWBorderPixel | CWEventMask);
	attributes.background_pixel = Scr->MenuC.back;
	attributes.border_pixel = Scr->MenuC.fore;
	if (mr->pinned) {
	    attributes.event_mask = (ExposureMask | EnterWindowMask
				| LeaveWindowMask | ButtonPressMask
				| ButtonReleaseMask | PointerMotionMask
				| ButtonMotionMask
				);
	    attributes.cursor = Scr->MenuCursor;
	    valuemask |= CWCursor;
	}
	else
	    attributes.event_mask = (ExposureMask | EnterWindowMask);

	if (Scr->SaveUnder && ! mr->pinned) {
	    valuemask |= CWSaveUnder;
	    attributes.save_under = True;
	}
	if (Scr->BackingStore) {
	    valuemask |= CWBackingStore;
	    attributes.backing_store = Always;
	}
	borderwidth = Scr->use3Dmenus ? 0 : 1;
	mr->w = XCreateWindow (dpy, Scr->Root, 0, 0, (unsigned int) mr->width,
			       (unsigned int) mr->height, (unsigned int) borderwidth,
			       CopyFromParent, (unsigned int) CopyFromParent,
			       (Visual *) CopyFromParent,
			       valuemask, &attributes);


	XSaveContext(dpy, mr->w, MenuContext, (caddr_t)mr);
	XSaveContext(dpy, mr->w, ScreenContext, (caddr_t)Scr);

	mr->mapped = UNMAPPED;
    }

    if (Scr->use3Dmenus && (Scr->Monochrome == COLOR) &&  (mr->highlight.back == UNUSED_PIXEL)) {
	XColor xcol;
	char colname [32];
	short save;

	xcol.pixel = Scr->MenuC.back;
	XQueryColor (dpy, cmap, &xcol);
	sprintf (colname, "#%04x%04x%04x", 
		5 * (xcol.red / 6), 5 * (xcol.green / 6), 5 * (xcol.blue / 6));
	save = Scr->FirstTime;
	Scr->FirstTime = True;
	GetColor (Scr->Monochrome, &mr->highlight.back, colname);
	Scr->FirstTime = save;
    }

    if (Scr->use3Dmenus && (Scr->Monochrome == COLOR) && (mr->highlight.fore == UNUSED_PIXEL)) {
	XColor xcol;
	char colname [32];
	short save;

	xcol.pixel = Scr->MenuC.fore;
	XQueryColor (dpy, cmap, &xcol);
	sprintf (colname, "#%04x%04x%04x",
		5 * (xcol.red / 6), 5 * (xcol.green / 6), 5 * (xcol.blue / 6));
	save = Scr->FirstTime;
	Scr->FirstTime = True;
	GetColor (Scr->Monochrome, &mr->highlight.fore, colname);
	Scr->FirstTime = save;
    }
    if (Scr->use3Dmenus && !Scr->BeNiceToColormap) GetShadeColors (&mr->highlight);

    /* get the default colors into the menus */
    for (tmp = mr->first; tmp != NULL; tmp = tmp->next)
    {
	if (!tmp->user_colors) {
	    if (tmp->func != F_TITLE) {
		tmp->normal.fore = Scr->MenuC.fore;
		tmp->normal.back = Scr->MenuC.back;
	    } else {
		tmp->normal.fore = Scr->MenuTitleC.fore;
		tmp->normal.back = Scr->MenuTitleC.back;
	    }
	}

	if (mr->highlight.fore != UNUSED_PIXEL)
	{
	    tmp->highlight.fore = mr->highlight.fore;
	    tmp->highlight.back = mr->highlight.back;
	}
	else
	{
	    tmp->highlight.fore = tmp->normal.back;
	    tmp->highlight.back = tmp->normal.fore;
	}
	if (Scr->use3Dmenus && !Scr->BeNiceToColormap) {
	    if (tmp->func != F_TITLE)
		GetShadeColors (&tmp->highlight);
	    else
		GetShadeColors (&tmp->normal);
	}
    }
    mr->pmenu = NULL;

    if (Scr->Monochrome == MONOCHROME || !Scr->InterpolateMenuColors)
	return (0);

    start = mr->first;
    while (TRUE)
    {
	for (; start != NULL; start = start->next)
	{
	    if (start->user_colors)
		break;
	}
	if (start == NULL)
	    break;

	for (end = start->next; end != NULL; end = end->next)
	{
	    if (end->user_colors)
		break;
	}
	if (end == NULL)
	    break;

	/* we have a start and end to interpolate between */
	num = end->item_num - start->item_num;

	f1.pixel = start->normal.fore;
	XQueryColor(dpy, cmap, &f1);
	f2.pixel = end->normal.fore;
	XQueryColor(dpy, cmap, &f2);

	b1.pixel = start->normal.back;
	XQueryColor(dpy, cmap, &b1);
	b2.pixel = end->normal.back;
	XQueryColor(dpy, cmap, &b2);

	fred = ((int)f2.red - (int)f1.red) / num;
	fgreen = ((int)f2.green - (int)f1.green) / num;
	fblue = ((int)f2.blue - (int)f1.blue) / num;

	bred = ((int)b2.red - (int)b1.red) / num;
	bgreen = ((int)b2.green - (int)b1.green) / num;
	bblue = ((int)b2.blue - (int)b1.blue) / num;

	f3 = f1;
	f3.flags = DoRed | DoGreen | DoBlue;

	b3 = b1;
	b3.flags = DoRed | DoGreen | DoBlue;

	start->highlight.back = start->normal.fore;
	start->highlight.fore = start->normal.back;
	num -= 1;
	for (i = 0, cur = start->next; i < num; i++, cur = cur->next)
	{
	    f3.red += fred;
	    f3.green += fgreen;
	    f3.blue += fblue;
	    save_fore = f3;

	    b3.red += bred;
	    b3.green += bgreen;
	    b3.blue += bblue;
	    save_back = b3;

	    XAllocColor(dpy, cmap, &f3);
	    XAllocColor(dpy, cmap, &b3);
	    cur->highlight.back = cur->normal.fore = f3.pixel;
	    cur->highlight.fore = cur->normal.back = b3.pixel;
	    cur->user_colors = True;

	    f3 = save_fore;
	    b3 = save_back;
	}
	start = end;
	start->highlight.back = start->normal.fore;
	start->highlight.fore = start->normal.back;
    }
}



/***********************************************************************
 *
 *  Procedure:
 *	PopUpMenu - pop up a pull down menu
 *
 *  Inputs:
 *	menu	- the root pointer of the menu to pop up
 *	x, y	- location of upper left of menu
 *      center	- whether or not to center horizontally over position
 *
 ***********************************************************************
 */

Bool PopUpMenu (menu, x, y, center)
    MenuRoot *menu;
    int x, y;
    Bool center;
{
    int WindowNameOffset, WindowNameCount;
    TwmWindow **WindowNames;
    TwmWindow *tmp_win2,*tmp_win3;
    int i;
    int (*compar)() = 
      (Scr->CaseSensitive ? strcmp : XmuCompareISOLatin1);
    static char **actions = NULL;

    if (!menu) return False;

    InstallRootColormap();

    if ((menu == Scr->Windows) || (menu == Scr->AllWindows))
    {
	TwmWindow *tmp_win;
	WorkSpaceList *wlist;
	Boolean all;
	int func;

	/* this is the twm windows menu,  let's go ahead and build it */

	all = (menu == Scr->AllWindows);
	DestroyMenu (menu);

	menu->first = NULL;
	menu->last = NULL;
	menu->items = 0;
	menu->width = 0;
	menu->mapped = NEVER_MAPPED;
	menu->highlight.fore = UNUSED_PIXEL;
	menu->highlight.back = UNUSED_PIXEL;
  	AddToMenu(menu, "TWM Windows", NULLSTR, NULL, F_TITLE,NULLSTR,NULLSTR);
  
	wlist = NULL;
	if (! all && CurrentSelectedWorkspace) {
	    for (wlist = Scr->workSpaceMgr.workSpaceList; wlist != NULL; wlist = wlist->next) {
        	if (strcmp (wlist->name, CurrentSelectedWorkspace) == 0) break;
	    }
	}
	if (! wlist) wlist = Scr->workSpaceMgr.activeWSPC;

        WindowNameOffset=(char *)Scr->TwmRoot.next->name -
                               (char *)Scr->TwmRoot.next;
        for(tmp_win = Scr->TwmRoot.next , WindowNameCount=0;
            tmp_win != NULL;
            tmp_win = tmp_win->next) {
	  if (all || OCCUPY (tmp_win, wlist)) WindowNameCount++;
	}
        WindowNames =
          (TwmWindow **)malloc(sizeof(TwmWindow *)*WindowNameCount);
	WindowNameCount = 0;
        for(tmp_win = Scr->TwmRoot.next;
            tmp_win != NULL;
            tmp_win = tmp_win->next)
        {
	    if (!all && ! OCCUPY (tmp_win, wlist)) continue;
            tmp_win2 = tmp_win;
            for (i=0;i<WindowNameCount;i++)
            {
                if ((*compar)(tmp_win2->name,WindowNames[i]->name) < 0)
                {
                    tmp_win3 = tmp_win2;
                    tmp_win2 = WindowNames[i];
                    WindowNames[i] = tmp_win3;
                }
            }
            WindowNames[WindowNameCount] = tmp_win2;
	    WindowNameCount++;
        }
	func = (all || CurrentSelectedWorkspace) ? F_WINWARP : F_POPUP;
        for (i=0; i<WindowNameCount; i++)
        {
            AddToMenu(menu, WindowNames[i]->name, (char *)WindowNames[i],
                      NULL, func,NULL,NULL);
        }
        free(WindowNames);

	menu->pinned = False;
	MakeMenu(menu);
    }

    if (menu->w == None || menu->items == 0) return False;

    /* Prevent recursively bringing up menus. */
    if ((!menu->pinned) && (menu->mapped == MAPPED)) return False;

    /*
     * Dynamically set the parent;  this allows pull-ups to also be main
     * menus, or to be brought up from more than one place.
     */
    menu->prev = ActiveMenu;

    if (menu->pinned) {
	ActiveMenu    = menu;
	menu->mapped  = MAPPED;
	menu->entered = TRUE;
	MenuOrigins [MenuDepth].x = menu->x;
	MenuOrigins [MenuDepth].y = menu->y;
	MenuDepth++;

	XRaiseWindow (dpy, menu->w);
	return (True);
    }

    XGrabPointer(dpy, Scr->Root, True,
	ButtonPressMask | ButtonReleaseMask | PointerMotionMask |
	ButtonMotionMask | PointerMotionHintMask,
	GrabModeAsync, GrabModeAsync,
	Scr->Root,
	Scr->MenuCursor, CurrentTime);

    XGrabKeyboard (dpy, Scr->Root, True, GrabModeAsync, GrabModeAsync, CurrentTime);

    ActiveMenu = menu;
    menu->mapped = MAPPED;
    menu->entered = FALSE;

    if (center) {
	x -= (menu->width / 2);
	y -= (Scr->EntryHeight / 2);	/* sticky menus would be nice here */
    }

    /*
    * clip to screen
    */
    if (x + menu->width > Scr->MyDisplayWidth) {
	x = Scr->MyDisplayWidth - menu->width;
    }
    if (x < 0) x = 0;
    if (y + menu->height > Scr->MyDisplayHeight) {
	y = Scr->MyDisplayHeight - menu->height;
    }
    if (y < 0) y = 0;
    MenuOrigins[MenuDepth].x = x;
    MenuOrigins[MenuDepth].y = y;
    MenuDepth++;

    XMoveWindow(dpy, menu->w, x, y);
    if (Scr->Shadow) {
	XMoveWindow (dpy, menu->shadow, x + SHADOWWIDTH, y + SHADOWWIDTH);
    }
    if (Scr->Shadow) {
	XRaiseWindow (dpy, menu->shadow);
    }
    XMapRaised(dpy, menu->w);
    if (Scr->Shadow) {
	XMapWindow (dpy, menu->shadow);
    }
    XSync(dpy, 0);

    return True;
}



/***********************************************************************
 *
 *  Procedure:
 *	PopDownMenu - unhighlight the current menu selection and
 *		take down the menus
 *
 ***********************************************************************
 */

PopDownMenu()
{
    MenuRoot *tmp;

    if (ActiveMenu == NULL)
	return (1);

    if (ActiveItem)
    {
	ActiveItem->state = 0;
	PaintEntry(ActiveMenu, ActiveItem, False);
    }

    for (tmp = ActiveMenu; tmp != NULL; tmp = tmp->prev)
    {
	if (! tmp->pinned) HideMenu (tmp);
	UninstallRootColormap();
    }

    XFlush(dpy);
    ActiveMenu = NULL;
    ActiveItem = NULL;
    MenuDepth = 0;
    XUngrabKeyboard (dpy, CurrentTime);
    if (Context == C_WINDOW || Context == C_FRAME || Context == C_TITLE || Context == C_ICON)
      menuFromFrameOrWindowOrTitlebar = TRUE;
}



HideMenu (menu)
MenuRoot *menu;
{
    if (!menu) return False;

    if (Scr->Shadow) {
	XUnmapWindow (dpy, menu->shadow);
    }
    XUnmapWindow (dpy, menu->w);
    menu->mapped = UNMAPPED;
}

/***********************************************************************
 *
 *  Procedure:
 *	FindMenuRoot - look for a menu root
 *
 *  Returned Value:
 *	(MenuRoot *)  - a pointer to the menu root structure 
 *
 *  Inputs:
 *	name	- the name of the menu root 
 *
 ***********************************************************************
 */

MenuRoot *
FindMenuRoot(name)
    char *name;
{
    MenuRoot *tmp;

    for (tmp = Scr->MenuList; tmp != NULL; tmp = tmp->next)
    {
	if (strcmp(name, tmp->name) == 0)
	    return (tmp);
    }
    return NULL;
}



static Bool belongs_to_twm_window (t, w)
    register TwmWindow *t;
    register Window w;
{
    if (!t) return False;

    if (w == t->frame || w == t->title_w || w == t->hilite_wl || w == t->hilite_wr ||
	(t->icon && (w == t->icon->w || w == t->icon->bm_w))) return True;
    
    if (t && t->titlebuttons) {
	register TBWindow *tbw;
	register int nb = Scr->TBInfo.nleft + Scr->TBInfo.nright;
	for (tbw = t->titlebuttons; nb > 0; tbw++, nb--) {
	    if (tbw->window == w) return True;
	}
    }
    return False;
}




/***********************************************************************
 *
 *  Procedure:
 *	resizeFromCenter -
 *
 ***********************************************************************
 */


extern int AddingX;
extern int AddingY;
extern int AddingW;
extern int AddingH;

void resizeFromCenter(w, tmp_win)
     Window w;
     TwmWindow *tmp_win;
{
  int lastx, lasty, width, height, bw2;
  int namelen;
  int stat;
  Window junk;

  namelen = strlen (tmp_win->name);
  bw2 = tmp_win->frame_bw * 2;
  AddingW = tmp_win->attr.width + bw2 + 2 * tmp_win->frame_bw3D;
  AddingH = tmp_win->attr.height + tmp_win->title_height + bw2 + 2 * tmp_win->frame_bw3D;
  width = (SIZE_HINDENT + XTextWidth (Scr->SizeFont.font,
				      tmp_win->name, namelen));
  height = Scr->SizeFont.height + SIZE_VINDENT * 2;
  XGetGeometry(dpy, w, &JunkRoot, &origDragX, &origDragY,
	       (unsigned int *)&DragWidth, (unsigned int *)&DragHeight, 
	       &JunkBW, &JunkDepth);

  XWarpPointer(dpy, None, w,
	       0, 0, 0, 0, DragWidth/2, DragHeight/2);   
  XQueryPointer (dpy, Scr->Root, &JunkRoot, 
		 &JunkChild, &JunkX, &JunkY,
		 &AddingX, &AddingY, &JunkMask);
/*****
  Scr->SizeStringOffset = width +
    XTextWidth(Scr->SizeFont.font, ": ", 2);
  XResizeWindow (dpy, Scr->SizeWindow, Scr->SizeStringOffset +
		 Scr->SizeStringWidth, height);
  XDrawImageString (dpy, Scr->SizeWindow, Scr->NormalGC, width,
		    SIZE_VINDENT + Scr->SizeFont.font->ascent,
		    ": ", 2);
*****/
  lastx = -10000;
  lasty = -10000;
/*****
  MoveOutline(Scr->Root,
	      origDragX - JunkBW, origDragY - JunkBW,
	      DragWidth * JunkBW, DragHeight * JunkBW,
	      tmp_win->frame_bw,
	      tmp_win->title_height + tmp_win->frame_bw3D);
*****/
  MenuStartResize(tmp_win, origDragX, origDragY, DragWidth, DragHeight);
  while (TRUE)
    {
      XMaskEvent(dpy,
		 ButtonPressMask | PointerMotionMask | ExposureMask, &Event);
      
      if (Event.type == MotionNotify) {
	/* discard any extra motion events before a release */
	while(XCheckMaskEvent(dpy,
			      ButtonMotionMask | ButtonPressMask, &Event))
	  if (Event.type == ButtonPress)
	    break;
      }
      
      if (Event.type == ButtonPress)
	{
	  MenuEndResize(tmp_win);
	  XMoveResizeWindow(dpy, w, AddingX, AddingY, AddingW, AddingH);
	  break;
	}
      
      if (Event.type != MotionNotify) {
	DispatchEvent2 ();
	continue;
      }
      
      /*
       * XXX - if we are going to do a loop, we ought to consider
       * using multiple GXxor lines so that we don't need to 
       * grab the server.
       */
      XQueryPointer(dpy, Scr->Root, &JunkRoot, &JunkChild,
		    &JunkX, &JunkY, &AddingX, &AddingY, &JunkMask);
      
      if (lastx != AddingX || lasty != AddingY)
	{
	  MenuDoResize(AddingX, AddingY, tmp_win);
	  
	  lastx = AddingX;
	  lasty = AddingY;
	}
      
    }
} 



/***********************************************************************
 *
 *  Procedure:
 *	ExecuteFunction - execute a twm root function
 *
 *  Inputs:
 *	func	- the function to execute
 *	action	- the menu action to execute 
 *	w	- the window to execute this function on
 *	tmp_win	- the twm window structure
 *	event	- the event that caused the function
 *	context - the context in which the button was pressed
 *	pulldown- flag indicating execution from pull down menu
 *
 *  Returns:
 *	TRUE if should continue with remaining actions else FALSE to abort
 *
 ***********************************************************************
 */

int
ExecuteFunction(func, action, w, tmp_win, eventp, context, pulldown)
    int func;
    char *action;
    Window w;
    TwmWindow *tmp_win;
    XEvent *eventp;
    int context;
    int pulldown;
{
    static Time last_time = 0;
    char tmp[200];
    char *ptr;
    char buff[MAX_FILE_SIZE];
    int count, fd;
    Window rootw;
    int origX, origY;
    int do_next_action = TRUE;
    int moving_icon = FALSE;
    Bool fromtitlebar = False;
    Bool from3dborder = False;
    extern int ConstrainedMoveTime;
    TwmWindow *t;

    RootFunction = 0;
    if (Cancel)
	return TRUE;			/* XXX should this be FALSE? */

    switch (func)
    {
    case F_UPICONMGR:
    case F_LEFTICONMGR:
    case F_RIGHTICONMGR:
    case F_DOWNICONMGR:
    case F_FORWICONMGR:
    case F_BACKICONMGR:
    case F_NEXTICONMGR:
    case F_PREVICONMGR:
    case F_NOP:
    case F_TITLE:
    case F_DELTASTOP:
    case F_RAISELOWER:
    case F_WARPTOSCREEN:
    case F_WARPTO:
    case F_WARPRING:
    case F_WARPTOICONMGR:
    case F_COLORMAP:
	break;

    default:
        XGrabPointer(dpy, Scr->Root, True,
            ButtonPressMask | ButtonReleaseMask,
            GrabModeAsync, GrabModeAsync,
            Scr->Root, Scr->WaitCursor, CurrentTime);
	break;
    }

    switch (func)
    {
#ifdef SOUNDS
    case F_TOGGLESOUND:
	toggle_sound();
	break;
    case F_REREADSOUNDS:
	reread_sounds();
	break;
#endif
    case F_NOP:
    case F_TITLE:
	break;

    case F_DELTASTOP:
	if (WindowMoved) do_next_action = FALSE;
	break;

    case F_RESTART:
	StopAnimation ();
	XSync (dpy, 0);
	Reborder (eventp->xbutton.time);
	XSync (dpy, 0);
#ifdef VMS
	fprintf (stderr, "%s:  restart capabilities not yet supported\n",
		 ProgramName);
#else
	execvp(*Argv, Argv);
#endif
	fprintf (stderr, "%s:  unable to restart:  %s\n", ProgramName, *Argv);
	break;

    case F_UPICONMGR:
    case F_DOWNICONMGR:
    case F_LEFTICONMGR:
    case F_RIGHTICONMGR:
    case F_FORWICONMGR:
    case F_BACKICONMGR:
	MoveIconManager(func);
        break;

    case F_NEXTICONMGR:
    case F_PREVICONMGR:
	JumpIconManager(func);
        break;

    case F_SHOWLIST:
	if (Scr->NoIconManagers) break;
	ShowIconManager ();
	break;

    case F_STARTANIMATION :
	StartAnimation ();
	break;

    case F_STOPANIMATION :
	StopAnimation ();
	break;

    case F_SPEEDUPANIMATION :
	ModifyAnimationSpeed (1);
	break;

    case F_SLOWDOWNANIMATION :
	ModifyAnimationSpeed (-1);
	break;

    case F_HIDELIST:
	if (Scr->NoIconManagers) break;
	HideIconManager ();
	break;

    case F_SHOWWORKMGR:
	if (! Scr->workSpaceManagerActive) break;
	DeIconify (Scr->workSpaceMgr.workspaceWindow.twm_win);
	XRaiseWindow(dpy, Scr->workSpaceMgr.workspaceWindow.twm_win->frame);
	break;

    case F_HIDEWORKMGR:
	if (! Scr->workSpaceManagerActive) break;
	Iconify (Scr->workSpaceMgr.workspaceWindow.twm_win, eventp->xbutton.x_root - 5,
		     eventp->xbutton.y_root - 5);
	break;

    case F_TOGGLESTATE :
	WMapToggleState ();
	break;

    case F_SETBUTTONSTATE :
	WMapSetButtonsState ();
	break;

    case F_SETMAPSTATE :
	WMapSetMapState ();
	break;

    case F_PIN :
	if (! ActiveMenu) break;
	if (ActiveMenu->pinned) {
	    XUnmapWindow (dpy, ActiveMenu->w);
	    ActiveMenu->mapped = UNMAPPED;
	}
	else {
	    XWindowAttributes attr;
	    MenuRoot *menu;

	    if (ActiveMenu->pmenu == NULL) {
		menu  = (MenuRoot*) malloc (sizeof (struct MenuRoot));
		*menu = *ActiveMenu;
		menu->pinned = True;
		menu->mapped = NEVER_MAPPED;
		menu->width -= 10;
		if (menu->pull) menu->width -= 16 + 10;
		MakeMenu (menu);
		ActiveMenu->pmenu = menu;
	    }
	    else menu = ActiveMenu->pmenu;
	    if (menu->mapped == MAPPED) break;
	    XGetWindowAttributes (dpy, ActiveMenu->w, &attr);
	    menu->x = attr.x;
	    menu->y = attr.y;
	    XMoveWindow (dpy, menu->w, menu->x, menu->y);
	    XMapRaised  (dpy, menu->w);
	    menu->mapped = MAPPED;
	}
	break;

    case F_MOVEMENU:
	break;

    case F_VANISH:
	if (DeferExecution (context, func, Scr->SelectCursor)) return TRUE;

	WMgrRemoveFromCurrentWosksace (tmp_win);
	break;

    case F_WARPHERE:
	WMgrAddToCurrentWosksaceAndWarp (action);
	break;

    case F_SORTICONMGR:
	if (DeferExecution(context, func, Scr->SelectCursor))
	    return TRUE;

	{
	    int save_sort;

	    save_sort = Scr->SortIconMgr;
	    Scr->SortIconMgr = TRUE;

	    if (context == C_ICONMGR)
		SortIconManager((IconMgr *) NULL);
	    else if (tmp_win->iconmgr)
		SortIconManager(tmp_win->iconmgrp);
	    else
		XBell(dpy, 0);

	    Scr->SortIconMgr = save_sort;
	}
	break;

    case F_IDENTIFY:
	if (DeferExecution(context, func, Scr->SelectCursor))
	    return TRUE;

	Identify(tmp_win);
	break;

    case F_VERSION:
	Identify ((TwmWindow *) NULL);
	break;

    case F_AUTORAISE:
	if (DeferExecution(context, func, Scr->SelectCursor))
	    return TRUE;

	tmp_win->auto_raise = !tmp_win->auto_raise;
	if (tmp_win->auto_raise) ++(Scr->NumAutoRaises);
	else --(Scr->NumAutoRaises);
	break;

    case F_BEEP:
	XBell(dpy, 0);
	break;

    case F_POPUP:
	tmp_win = (TwmWindow *)action;
	if (Scr->WindowFunction.func != 0)
	{
	   ExecuteFunction(Scr->WindowFunction.func,
			   Scr->WindowFunction.item->action,
			   w, tmp_win, eventp, C_FRAME, FALSE);
	}
	else
	{
	    DeIconify(tmp_win);
	    XRaiseWindow (dpy, tmp_win->frame);
	}
	break;

    case F_WINWARP:
	tmp_win = (TwmWindow *)action;

	if (! tmp_win) break;
	if (Scr->WarpUnmapped || tmp_win->mapped) {
	    if (!tmp_win->mapped) DeIconify (tmp_win);
	    if (!Scr->NoRaiseWarp) XRaiseWindow (dpy, tmp_win->frame);
	    WarpToWindow (tmp_win);
	}
	break;

    case F_RESIZE:
	EventHandler[EnterNotify] = HandleUnknown;
	EventHandler[LeaveNotify] = HandleUnknown;
	if (DeferExecution(context, func, Scr->MoveCursor))
	    return TRUE;

	PopDownMenu();

	if (tmp_win->OpaqueResize) {
	    int sw, ss;

	    sw = tmp_win->frame_width * tmp_win->frame_height;
	    ss = Scr->MyDisplayWidth  * Scr->MyDisplayHeight;
	    if (sw > ((ss * Scr->OpaqueResizeThreshold) / 100))
		Scr->OpaqueResize = FALSE;
	    else
		Scr->OpaqueResize = TRUE;
	}
	else
	    Scr->OpaqueResize = FALSE;

	if (pulldown)
	    XWarpPointer(dpy, None, Scr->Root, 
		0, 0, 0, 0, eventp->xbutton.x_root, eventp->xbutton.y_root);

	if (!tmp_win->icon || (w != tmp_win->icon->w)) {	/* can't resize icons */

/*	  fromMenu = False;  ????? */
	  if ((Context == C_FRAME || Context == C_WINDOW || Context == C_TITLE)
	      && fromMenu)
	    resizeFromCenter(w, tmp_win);
	  else {
	    /*
	     * see if this is being done from the titlebar
	     */
	    fromtitlebar = 
	      belongs_to_twm_window (tmp_win, eventp->xbutton.window);
	    from3dborder = (eventp->xbutton.window == tmp_win->frame) ? True : False;
	    
	    /* Save pointer position so we can tell if it was moved or
	       not during the resize. */
	    ResizeOrigX = eventp->xbutton.x_root;
	    ResizeOrigY = eventp->xbutton.y_root;
	    
	    StartResize (eventp, tmp_win, fromtitlebar, from3dborder);
	    
	    do {
	      XMaskEvent(dpy,
			   ButtonPressMask | ButtonReleaseMask |
			   EnterWindowMask | LeaveWindowMask |
			   ButtonMotionMask | VisibilityChangeMask | ExposureMask, &Event);
		
		if (fromtitlebar && Event.type == ButtonPress) {
		  fromtitlebar = False;
		    continue;
		  }
		
	    	if (Event.type == MotionNotify) {
		  /* discard any extra motion events before a release */
		  while
		    (XCheckMaskEvent
		     (dpy, ButtonMotionMask | ButtonReleaseMask, &Event))
		      if (Event.type == ButtonRelease)
			break;
		}
	      
	      if (!DispatchEvent2 ()) continue;
	      
	    } while (!(Event.type == ButtonRelease || Cancel));
	    return TRUE;
	  }
	} 
	break;


    case F_ZOOM:
    case F_HORIZOOM:
    case F_FULLZOOM:
    case F_LEFTZOOM:
    case F_RIGHTZOOM:
    case F_TOPZOOM:
    case F_BOTTOMZOOM:
	if (DeferExecution(context, func, Scr->SelectCursor))
	    return TRUE;
	fullzoom(tmp_win, func);
	break;

    case F_MOVE:
    case F_FORCEMOVE:
	if (DeferExecution(context, func, Scr->MoveCursor))
	    return TRUE;

	PopDownMenu();
	if (tmp_win->OpaqueMove) {
	    int sw, ss;

	    sw = tmp_win->frame_width * tmp_win->frame_height;
	    ss = Scr->MyDisplayWidth  * Scr->MyDisplayHeight;
	    if (sw > ((ss * Scr->OpaqueMoveThreshold) / 100))
		Scr->OpaqueMove = FALSE;
	    else
		Scr->OpaqueMove = TRUE;
	}
	else
	    Scr->OpaqueMove = FALSE;

	rootw = eventp->xbutton.root;
	MoveFunction = func;

	if (pulldown)
	    XWarpPointer(dpy, None, Scr->Root, 
		0, 0, 0, 0, eventp->xbutton.x_root, eventp->xbutton.y_root);

	EventHandler[EnterNotify] = HandleUnknown;
	EventHandler[LeaveNotify] = HandleUnknown;

	if (!Scr->NoGrabServer || !Scr->OpaqueMove) {
	    XGrabServer(dpy);
	}

	Scr->SizeStringOffset = SIZE_HINDENT;
	XResizeWindow (dpy, Scr->SizeWindow,
		   Scr->SizeStringWidth + SIZE_HINDENT * 2, 
		   Scr->SizeFont.height + SIZE_VINDENT * 2);
	XMapRaised (dpy, Scr->SizeWindow);

	XGrabPointer(dpy, eventp->xbutton.root, True,
	    ButtonPressMask | ButtonReleaseMask |
	    ButtonMotionMask | PointerMotionMask, /* PointerMotionHintMask */
	    GrabModeAsync, GrabModeAsync,
	    Scr->Root, Scr->MoveCursor, CurrentTime);

	if (context == C_ICON && tmp_win->icon && tmp_win->icon->w)
	{
	    w = tmp_win->icon->w;
	    DragX = eventp->xbutton.x;
	    DragY = eventp->xbutton.y;
	    moving_icon = TRUE;
	    if (tmp_win->OpaqueMove) Scr->OpaqueMove = TRUE;
	}

	else if (! tmp_win->icon || w != tmp_win->icon->w)
	{
	    XTranslateCoordinates(dpy, w, tmp_win->frame,
		eventp->xbutton.x, 
		eventp->xbutton.y, 
		&DragX, &DragY, &JunkChild);

	    w = tmp_win->frame;
	}

	DragWindow = None;

	XGetGeometry(dpy, w, &JunkRoot, &origDragX, &origDragY,
	    (unsigned int *)&DragWidth, (unsigned int *)&DragHeight, &DragBW,
	    &JunkDepth);

	JunkBW = DragBW;
	origX = eventp->xbutton.x_root;
	origY = eventp->xbutton.y_root;
	CurrentDragX = origDragX;
	CurrentDragY = origDragY;

	/*
	 * only do the constrained move if timer is set; need to check it
	 * in case of stupid or wicked fast servers
	 */
	if (ConstrainedMoveTime && 
	    (eventp->xbutton.time - last_time) < ConstrainedMoveTime)
	{
	    int width, height;

	    ConstMove = TRUE;
	    ConstMoveDir = MOVE_NONE;
	    ConstMoveX = eventp->xbutton.x_root - DragX - JunkBW;
	    ConstMoveY = eventp->xbutton.y_root - DragY - JunkBW;
	    width = DragWidth + 2 * JunkBW;
	    height = DragHeight + 2 * JunkBW;
	    ConstMoveXL = ConstMoveX + width/3;
	    ConstMoveXR = ConstMoveX + 2*(width/3);
	    ConstMoveYT = ConstMoveY + height/3;
	    ConstMoveYB = ConstMoveY + 2*(height/3);

	    XWarpPointer(dpy, None, w,
		0, 0, 0, 0, DragWidth/2, DragHeight/2);

	    XQueryPointer(dpy, w, &JunkRoot, &JunkChild,
		&JunkX, &JunkY, &DragX, &DragY, &JunkMask);
	}
	last_time = eventp->xbutton.time;

	if (!Scr->OpaqueMove)
	{
	    InstallRootColormap();
	    if (!Scr->MoveDelta)
	    {
		/*
		 * Draw initial outline.  This was previously done the
		 * first time though the outer loop by dropping out of
		 * the XCheckMaskEvent inner loop down to one of the
		 * MoveOutline's below.
		 */
		MoveOutline(rootw,
		    origDragX - JunkBW, origDragY - JunkBW,
		    DragWidth + 2 * JunkBW, DragHeight + 2 * JunkBW,
		    tmp_win->frame_bw,
		    moving_icon ? 0 : tmp_win->title_height + tmp_win->frame_bw3D);
		/*
		 * This next line causes HandleReleaseNotify to call
		 * XRaiseWindow().  This is solely to preserve the
		 * previous behaviour that raises a window being moved
		 * on button release even if you never actually moved
		 * any distance (unless you move less than MoveDelta or
		 * NoRaiseMove is set or OpaqueMove is set).
		 */
		DragWindow = w;
	    }
	}

	/*
	 * see if this is being done from the titlebar
	 */
	fromtitlebar = belongs_to_twm_window (tmp_win, eventp->xbutton.window);

	if (menuFromFrameOrWindowOrTitlebar) {
	  /* warp the pointer to the middle of the window */
	  XWarpPointer(dpy, None, Scr->Root, 0, 0, 0, 0, 
		       origDragX + DragWidth / 2, 
		       origDragY + DragHeight / 2);
	  XFlush(dpy);
	}
	
	DisplayPosition (tmp_win, CurrentDragX, CurrentDragY);
	while (TRUE)
	{
	    long releaseEvent = menuFromFrameOrWindowOrTitlebar ? 
	                          ButtonPress : ButtonRelease;
	    long movementMask = menuFromFrameOrWindowOrTitlebar ?
	                          PointerMotionMask : ButtonMotionMask;

	    /* block until there is an interesting event */
	    XMaskEvent(dpy, ButtonPressMask | ButtonReleaseMask |
				    EnterWindowMask | LeaveWindowMask |
				    ExposureMask | movementMask |
				    VisibilityChangeMask, &Event);

	    /* throw away enter and leave events until release */
	    if (Event.xany.type == EnterNotify ||
		Event.xany.type == LeaveNotify) continue; 

	    if (Event.type == MotionNotify) {
		/* discard any extra motion events before a logical release */
		while(XCheckMaskEvent(dpy,
		    movementMask | releaseEvent, &Event))
		    if (Event.type == releaseEvent)
			break;
	    }

	    /* test to see if we have a second button press to abort move */
	    if (!menuFromFrameOrWindowOrTitlebar)
	      if (Event.type == ButtonPress && DragWindow != None) {
		if (Scr->OpaqueMove)
		  XMoveWindow (dpy, DragWindow, origDragX, origDragY);
		else
		  MoveOutline(Scr->Root, 0, 0, 0, 0, 0, 0);
		DragWindow = None;
	      }

	    if (fromtitlebar && Event.type == ButtonPress) {
		fromtitlebar = False;
		CurrentDragX = origX = Event.xbutton.x_root;
		CurrentDragY = origY = Event.xbutton.y_root;
		XTranslateCoordinates (dpy, rootw, tmp_win->frame,
				       origX, origY,
				       &DragX, &DragY, &JunkChild);
		continue;
	    }

	    if (!DispatchEvent2 ()) continue;

	    if (Cancel)
	    {
		WindowMoved = FALSE;
		if (!Scr->OpaqueMove)
		    UninstallRootColormap();
		    return TRUE;	/* XXX should this be FALSE? */
	    }
	    if (Event.type == releaseEvent)
	    {
		MoveOutline(rootw, 0, 0, 0, 0, 0, 0);
		if (moving_icon &&
		    ((CurrentDragX != origDragX ||
		      CurrentDragY != origDragY)))
		  tmp_win->icon_moved = TRUE;
		if (!Scr->OpaqueMove && menuFromFrameOrWindowOrTitlebar)
		  XMoveWindow(dpy, DragWindow, 
			      Event.xbutton.x_root - DragWidth / 2,
			      Event.xbutton.y_root - DragHeight / 2);
		if (menuFromFrameOrWindowOrTitlebar) DragWindow = None;
		break;
	    }

	    /* something left to do only if the pointer moved */
	    if (Event.type != MotionNotify)
		continue;

	    XQueryPointer(dpy, rootw, &(eventp->xmotion.root), &JunkChild,
		&(eventp->xmotion.x_root), &(eventp->xmotion.y_root),
		&JunkX, &JunkY, &JunkMask);

	    if (captive) FixRootEvent (eventp);

	    if (DragWindow == None &&
		abs(eventp->xmotion.x_root - origX) < Scr->MoveDelta &&
	        abs(eventp->xmotion.y_root - origY) < Scr->MoveDelta)
		continue;

	    WindowMoved = TRUE;
	    DragWindow = w;

	    if (!Scr->NoRaiseMove && Scr->OpaqueMove)	/* can't restore... */
	      XRaiseWindow(dpy, DragWindow);

	    if (ConstMove)
	    {
		switch (ConstMoveDir)
		{
		    case MOVE_NONE:
			if (eventp->xmotion.x_root < ConstMoveXL ||
			    eventp->xmotion.x_root > ConstMoveXR)
			    ConstMoveDir = MOVE_HORIZ;

			if (eventp->xmotion.y_root < ConstMoveYT ||
			    eventp->xmotion.y_root > ConstMoveYB)
			    ConstMoveDir = MOVE_VERT;

			XQueryPointer(dpy, DragWindow, &JunkRoot, &JunkChild,
			    &JunkX, &JunkY, &DragX, &DragY, &JunkMask);
			break;

		    case MOVE_VERT:
			ConstMoveY = eventp->xmotion.y_root - DragY - JunkBW;
			break;

		    case MOVE_HORIZ:
			ConstMoveX= eventp->xmotion.x_root - DragX - JunkBW;
			break;
		}

		if (ConstMoveDir != MOVE_NONE)
		{
		    int xl, yt, xr, yb, w, h;

		    xl = ConstMoveX;
		    yt = ConstMoveY;
		    w = DragWidth + 2 * JunkBW;
		    h = DragHeight + 2 * JunkBW;

		    if (Scr->DontMoveOff && MoveFunction != F_FORCEMOVE)
		    {
			xr = xl + w;
			yb = yt + h;

			if ((xl < 0) && ((Scr->MoveOffResistance < 0) 
					 || (xl > -Scr->MoveOffResistance)))
			    xl = 0;
			if ((xr > Scr->MyDisplayWidth) 
			    && ((Scr->MoveOffResistance < 0) 
				|| (xr < Scr->MyDisplayWidth + Scr->MoveOffResistance)))
			    xl = Scr->MyDisplayWidth - w;

			if ((yt < 0) && ((Scr->MoveOffResistance < 0) 
					 || (yt > -Scr->MoveOffResistance)))
			    yt = 0;
			if ((yb > Scr->MyDisplayHeight)
			    && ((Scr->MoveOffResistance < 0) 
				|| (yb < Scr->MyDisplayHeight + Scr->MoveOffResistance)))
			    yt = Scr->MyDisplayHeight - h;
		    }
		    CurrentDragX = xl;
		    CurrentDragY = yt;
		    if (Scr->OpaqueMove) {
			XMoveWindow(dpy, DragWindow, xl, yt);
			WMapSetupWindow (tmp_win, xl, yt, -1, -1);
		    }
		    else
			MoveOutline(eventp->xmotion.root, xl, yt, w, h,
			    tmp_win->frame_bw, 
			    moving_icon ? 0 : tmp_win->title_height + tmp_win->frame_bw3D);
		}
	    }
	    else if (DragWindow != None)
	    {
		int xl, yt, xr, yb, w, h;
		if (!menuFromFrameOrWindowOrTitlebar) {
		  xl = eventp->xmotion.x_root - DragX - JunkBW;
		  yt = eventp->xmotion.y_root - DragY - JunkBW;
		}
		else {
		  xl = eventp->xmotion.x_root - (DragWidth / 2);
		  yt = eventp->xmotion.y_root - (DragHeight / 2);
		}		  
		w = DragWidth + 2 * JunkBW;
		h = DragHeight + 2 * JunkBW;

		if (Scr->DontMoveOff && MoveFunction != F_FORCEMOVE)
		{
		    xr = xl + w;
		    yb = yt + h;

		    if ((xl < 0) && ((Scr->MoveOffResistance < 0) 
				     || (xl > -Scr->MoveOffResistance)))
			xl = 0;
		    if ((xr > Scr->MyDisplayWidth) 
			&& ((Scr->MoveOffResistance < 0)
			    || (xr < Scr->MyDisplayWidth + Scr->MoveOffResistance)))
			xl = Scr->MyDisplayWidth - w;

		    if ((yt < 0) && ((Scr->MoveOffResistance < 0)
				     || (yt > -Scr->MoveOffResistance)))
			yt = 0;
		    if ((yb > Scr->MyDisplayHeight)
			&& ((Scr->MoveOffResistance < 0)
			    || (yb < Scr->MyDisplayHeight + Scr->MoveOffResistance)))
			yt = Scr->MyDisplayHeight - h;
		}

		CurrentDragX = xl;
		CurrentDragY = yt;
		if (Scr->OpaqueMove) {
		    XMoveWindow(dpy, DragWindow, xl, yt);
		    if (! moving_icon) WMapSetupWindow (tmp_win, xl, yt, -1, -1);
		}
		else
		    MoveOutline(eventp->xmotion.root, xl, yt, w, h,
			tmp_win->frame_bw,
			moving_icon ? 0 : tmp_win->title_height + tmp_win->frame_bw3D);
	    }
	    DisplayPosition (tmp_win, CurrentDragX, CurrentDragY);
	}
	XUnmapWindow (dpy, Scr->SizeWindow);

	if (!Scr->OpaqueMove && DragWindow == None)
	    UninstallRootColormap();
        break;

    case F_FUNCTION:
	{
	    MenuRoot *mroot;
	    MenuItem *mitem;

	    if ((mroot = FindMenuRoot(action)) == NULL)
	    {
		fprintf (stderr, "%s: couldn't find function \"%s\"\n", 
			 ProgramName, action);
		return TRUE;
	    }

	    if (NeedToDefer(mroot) && DeferExecution(context, func, Scr->SelectCursor))
		return TRUE;
	    else
	    {
		for (mitem = mroot->first; mitem != NULL; mitem = mitem->next)
		{
		    if (!ExecuteFunction (mitem->func, mitem->action, w,
					  tmp_win, eventp, context, pulldown))
		      break;
		}
	    }
	}
	break;

    case F_DEICONIFY:
    case F_ICONIFY:
	if (DeferExecution(context, func, Scr->SelectCursor))
	    return TRUE;

	if (tmp_win->isicon)
	{
	    DeIconify(tmp_win);
	}
        else if (func == F_ICONIFY)
	{
	    Iconify (tmp_win, eventp->xbutton.x_root - 5,
		     eventp->xbutton.y_root - 5);
	}
	break;

    case F_RAISELOWER:
	if (DeferExecution(context, func, Scr->SelectCursor))
	    return TRUE;

	if (!WindowMoved) {
	    XWindowChanges xwc;
	    int	sp, sc;

	    if (!tmp_win->icon || w != tmp_win->icon->w) {
	      w = tmp_win->frame;
	    }
	    sp = tmp_win->frame_width * tmp_win->frame_height;
	    xwc.stack_mode = Below;
	    xwc.sibling    = w;
	    for (t = Scr->TwmRoot.next; t != NULL; t = t->next) {
		if ((t->transient && t->transientfor == tmp_win->w) ||
		    ((tmp_win->group == tmp_win->w) && (tmp_win->group == t->group) &&
		    (tmp_win->group != t->w))) {
		    if (t->frame) {
			sc = t->frame_width * t->frame_height;
			if (sc < ((sp * Scr->TransientOnTop) / 100))
			    XConfigureWindow (dpy, t->frame, CWStackMode | CWSibling, &xwc);
		    }
		}
	    }
	    xwc.stack_mode = Opposite;
	    XConfigureWindow (dpy, w, CWStackMode, &xwc);
	    xwc.stack_mode = Above;
	    xwc.sibling    = w;
	    for (t = Scr->TwmRoot.next; t != NULL; t = t->next) {
		if ((t->transient && t->transientfor == tmp_win->w) ||
		    ((tmp_win->group == tmp_win->w) && (tmp_win->group == t->group) &&
		    (tmp_win->group != t->w))) {
		    if (t->frame) {
			sc = t->frame_width * t->frame_height;
			if (sc < ((sp * Scr->TransientOnTop) / 100))
			    XConfigureWindow (dpy, t->frame, CWStackMode | CWSibling, &xwc);
		    }
		}
	    }
	    if (!tmp_win->icon || w != tmp_win->icon->w) {
	      WMapRaiseLower (tmp_win);
	    }
	}
	break;
	
    case F_RAISE:
	if (DeferExecution(context, func, Scr->SelectCursor))
	    return TRUE;

	/* check to make sure raise is not from the WindowFunction */
	if (tmp_win->icon && (w == tmp_win->icon->w) && Context != C_ROOT) 
	    XRaiseWindow(dpy, tmp_win->icon->w);
	else {
	    int	sp, sc;

	    XRaiseWindow(dpy, tmp_win->frame);
	    sp = tmp_win->frame_width * tmp_win->frame_height;
	    for (t = Scr->TwmRoot.next; t != NULL; t = t->next) {
		if ((t->transient && t->transientfor == tmp_win->w) ||
		    ((tmp_win->group == tmp_win->w) && (tmp_win->group == t->group) &&
		    (tmp_win->group != t->w))) {
		    if (t->frame) {
			sc = t->frame_width * t->frame_height;
			if (sc < ((sp * Scr->TransientOnTop) / 100)) XRaiseWindow(dpy, t->frame);
		    }
		}
	    }
	    WMapRaise (tmp_win);
	}
	break;

    case F_LOWER:
	if (DeferExecution(context, func, Scr->SelectCursor))
	    return TRUE;

	if (tmp_win->icon && (w == tmp_win->icon->w))
	    XLowerWindow(dpy, tmp_win->icon->w);
	else {
	    XWindowChanges xwc;
	    int	sp, sc;

	    XLowerWindow(dpy, tmp_win->frame);
	    sp = tmp_win->frame_width * tmp_win->frame_height;
	    xwc.stack_mode = Above;
	    xwc.sibling    = tmp_win->frame;
	    for (t = Scr->TwmRoot.next; t != NULL; t = t->next) {
		if ((t->transient && t->transientfor == tmp_win->w) ||
		    ((tmp_win->group == tmp_win->w) && (tmp_win->group == t->group) &&
		    (tmp_win->group != t->w))) {
		    if (t->frame) {
			sc = t->frame_width * t->frame_height;
			if (sc < ((sp * Scr->TransientOnTop) / 100))
			    XConfigureWindow (dpy, t->frame, CWStackMode | CWSibling, &xwc);
		    }
		}
	    }
	    WMapLower (tmp_win);
	}
	break;

    case F_FOCUS:
	if (DeferExecution(context, func, Scr->SelectCursor))
	    return TRUE;

	if (tmp_win->isicon == FALSE)
	{
	    if (!Scr->FocusRoot && Scr->Focus == tmp_win)
	    {
		FocusOnRoot();
	    }
	    else
	    {
		InstallWindowColormaps (0, tmp_win);
		SetFocus (tmp_win, eventp->xbutton.time);
		Scr->FocusRoot = FALSE;
	    }
	}
	break;

    case F_DESTROY:
	if (DeferExecution(context, func, Scr->DestroyCursor))
	    return TRUE;

	if (tmp_win->iconmgr)
	    XBell(dpy, 0);
	else {
	    XKillClient(dpy, tmp_win->w);

	    if (ButtonPressed != -1) {
		XEvent kev;

		XMaskEvent (dpy, ButtonReleaseMask, &kev);
		if (kev.xbutton.window == tmp_win->w) kev.xbutton.window = Scr->Root;
		XPutBackEvent (dpy, &kev);
	    }
	}
	break;

    case F_DELETE:
	if (DeferExecution(context, func, Scr->DestroyCursor))
	    return TRUE;

	if (tmp_win->iconmgr)		/* don't send ourself a message */
	  HideIconManager ();
	else if (tmp_win->protocols & DoesWmDeleteWindow)
	  SendDeleteWindowMessage (tmp_win, LastTimestamp());
	  if (ButtonPressed != -1) {
	    XEvent kev;

	    XMaskEvent (dpy, ButtonReleaseMask, &kev);
	    if (kev.xbutton.window == tmp_win->w) kev.xbutton.window = Scr->Root;
	    XPutBackEvent (dpy, &kev);
	  }
	else
	  XBell (dpy, 0);
	break;

    case F_SAVEYOURSELF:
	if (DeferExecution (context, func, Scr->SelectCursor))
	  return TRUE;

	if (tmp_win->protocols & DoesWmSaveYourself)
	  SendSaveYourselfMessage (tmp_win, LastTimestamp());
	else
	  XBell (dpy, 0);
	break;

    case F_CIRCLEUP:
	XCirculateSubwindowsUp(dpy, Scr->Root);
	break;

    case F_CIRCLEDOWN:
	XCirculateSubwindowsDown(dpy, Scr->Root);
	break;

    case F_EXEC:
	PopDownMenu();
	if (!Scr->NoGrabServer) {
	    XUngrabServer (dpy);
	    XSync (dpy, 0);
	}
	Execute(action);
	break;

    case F_UNFOCUS:
	FocusOnRoot();
	break;

    case F_CUT:
	strcpy(tmp, action);
	strcat(tmp, "\n");
	XStoreBytes(dpy, tmp, strlen(tmp));
	break;

    case F_CUTFILE:
	ptr = XFetchBytes(dpy, &count);
	if (ptr) {
	    if (sscanf (ptr, "%s", tmp) == 1) {
		XFree (ptr);
		ptr = ExpandFilename(tmp);
		if (ptr) {
#ifdef VMS
		    fd = open (ptr, O_RDONLY, 0);
#else
		    fd = open (ptr, 0);
#endif
		    if (fd >= 0) {
			count = read (fd, buff, MAX_FILE_SIZE - 1);
			if (count > 0) XStoreBytes (dpy, buff, count);
			close(fd);
		    } else {
			fprintf (stderr, 
				 "%s:  unable to open cut file \"%s\"\n", 
				 ProgramName, tmp);
		    }
		    if (ptr != tmp) free (ptr);
		} 
	    } else {
		XFree(ptr);
	    }
	} else {
	    fprintf(stderr, "%s:  cut buffer is empty\n", ProgramName);
	}
	break;

    case F_WARPTOSCREEN:
	{
	    if (strcmp (action, WARPSCREEN_NEXT) == 0) {
		WarpToScreen (Scr->screen + 1, 1);
	    } else if (strcmp (action, WARPSCREEN_PREV) == 0) {
		WarpToScreen (Scr->screen - 1, -1);
	    } else if (strcmp (action, WARPSCREEN_BACK) == 0) {
		WarpToScreen (PreviousScreen, 0);
	    } else {
		WarpToScreen (atoi (action), 0);
	    }
	}
	break;

    case F_COLORMAP:
	{
	    if (strcmp (action, COLORMAP_NEXT) == 0) {
		BumpWindowColormap (tmp_win, 1);
	    } else if (strcmp (action, COLORMAP_PREV) == 0) {
		BumpWindowColormap (tmp_win, -1);
	    } else {
		BumpWindowColormap (tmp_win, 0);
	    }
	}
	break;

    case F_WARPTO:
	{
	    register TwmWindow *t;
	    int len;

	    len = strlen(action);

	    for (t = Scr->TwmRoot.next; t != NULL; t = t->next) {
		if (!strncmp(action, t->full_name, len)) break;
		if (match (action, t->full_name)) break;
	    }
	    if (!t) {
		for (t = Scr->TwmRoot.next; t != NULL; t = t->next) {
		    if (!strncmp(action, t->class.res_name, len)) break;
		    if (match (action, t->class.res_name)) break;
		}
		if (!t) {
		    for (t = Scr->TwmRoot.next; t != NULL; t = t->next) {
			if (!strncmp(action, t->class.res_class, len)) break;
			if (match (action, t->class.res_class)) break;
		    }
		}
	    }

	    if (t) {
		if (Scr->WarpUnmapped || t->mapped) {
		    if (!t->mapped) DeIconify (t);
		    if (!Scr->NoRaiseWarp) XRaiseWindow (dpy, t->frame);
		    WarpToWindow (t);
		}
	    } else {
		XBell (dpy, 0);
	    }
	}
	break;

    case F_WARPTOICONMGR:
	{
	    TwmWindow *t;
	    int len;
	    Window raisewin = None, iconwin = None;

	    len = strlen(action);
	    if (len == 0) {
		if (tmp_win && tmp_win->list) {
		    raisewin = tmp_win->list->iconmgr->twm_win->frame;
		    iconwin = tmp_win->list->icon;
		} else if (Scr->iconmgr->active) {
		    raisewin = Scr->iconmgr->twm_win->frame;
		    iconwin = Scr->iconmgr->active->w;
		}
	    } else {
		for (t = Scr->TwmRoot.next; t != NULL; t = t->next) {
		    if (strncmp (action, t->icon_name, len) == 0) {
			if (t->list && t->list->iconmgr->twm_win->mapped) {
			    raisewin = t->list->iconmgr->twm_win->frame;
			    iconwin = t->list->icon;
			    break;
			}
		    }
		}
	    }

	    if (raisewin) {
		XRaiseWindow (dpy, raisewin);
		XWarpPointer (dpy, None, iconwin, 0,0,0,0, 5, 5);
	    } else {
		XBell (dpy, 0);
	    }
	}
	break;
	
    case F_WARPRING:
	switch (action[0]) {
	  case 'n':
	    WarpAlongRing (&eventp->xbutton, True);
	    break;
	  case 'p':
	    WarpAlongRing (&eventp->xbutton, False);
	    break;
	  default:
	    XBell (dpy, 0);
	    break;
	}
	break;

    case F_FILE:
	action = ExpandFilename(action);
#ifdef VMS
	fd = open (action, O_RDONLY, 0);
#else
	fd = open(action, 0);
#endif
	if (fd >= 0)
	{
	    count = read(fd, buff, MAX_FILE_SIZE - 1);
	    if (count > 0)
		XStoreBytes(dpy, buff, count);

	    close(fd);
	}
	else
	{
	    fprintf (stderr, "%s:  unable to open file \"%s\"\n", 
		     ProgramName, action);
	}
	break;

    case F_REFRESH:
	{
	    XSetWindowAttributes attributes;
	    unsigned long valuemask;

	    valuemask = (CWBackPixel | CWBackingStore | CWSaveUnder);
	    attributes.background_pixel = Scr->Black;
	    attributes.backing_store = NotUseful;
	    attributes.save_under = False;
	    w = XCreateWindow (dpy, Scr->Root, 0, 0,
			       (unsigned int) Scr->MyDisplayWidth,
			       (unsigned int) Scr->MyDisplayHeight,
			       (unsigned int) 0,
			       CopyFromParent, (unsigned int) CopyFromParent,
			       (Visual *) CopyFromParent, valuemask,
			       &attributes);
	    XMapWindow (dpy, w);
	    XDestroyWindow (dpy, w);
	    XFlush (dpy);
	}
	break;

    case F_OCCUPY:
	if (DeferExecution(context, func, Scr->SelectCursor))
	    return TRUE;
	Occupy (tmp_win);
	break;

    case F_OCCUPYALL:
	if (DeferExecution(context, func, Scr->SelectCursor))
	    return TRUE;
	OccupyAll (tmp_win);
	break;

    case F_GOTOWORKSPACE:
	GotoWorkSpaceByName (action);
	break;

    case F_PREVWORKSPACE:
	GotoPrevWorkSpace ();
	break;

    case F_NEXTWORKSPACE:
	GotoNextWorkSpace ();
	break;

    case F_MENU:
	if (action && ! strncmp (action, "WGOTO : ", 8)) {
	    GotoWorkSpaceByName (action + 8);
	}
	break;

    case F_WINREFRESH:
	if (DeferExecution(context, func, Scr->SelectCursor))
	    return TRUE;

	if (context == C_ICON && tmp_win->icon && tmp_win->icon->w)
	    w = XCreateSimpleWindow(dpy, tmp_win->icon->w,
		0, 0, 9999, 9999, 0, Scr->Black, Scr->Black);
	else
	    w = XCreateSimpleWindow(dpy, tmp_win->frame,
		0, 0, 9999, 9999, 0, Scr->Black, Scr->Black);

	XMapWindow(dpy, w);
	XDestroyWindow(dpy, w);
	XFlush(dpy);
	break;

    case F_ADOPTWINDOW:
	adoptWindow ();
	break;

    case F_TESTFUNC:
	testfunc ();
	break;
	
    case F_QUIT:
	Done();
	break;
    }

    if (ButtonPressed == -1) XUngrabPointer(dpy, CurrentTime);
    return do_next_action;
}



/***********************************************************************
 *
 *  Procedure:
 *	DeferExecution - defer the execution of a function to the
 *	    next button press if the context is C_ROOT
 *
 *  Inputs:
 *	context	- the context in which the mouse button was pressed
 *	func	- the function to defer
 *	cursor	- the cursor to display while waiting
 *
 ***********************************************************************
 */

int
DeferExecution(context, func, cursor)
int context, func;
Cursor cursor;
{
  if (context == C_ROOT)
    {
	LastCursor = cursor;
	if (func == F_ADOPTWINDOW)
	    XGrabPointer(dpy, Scr->Root, True,
		ButtonPressMask | ButtonReleaseMask,
		GrabModeAsync, GrabModeAsync,
		None, cursor, CurrentTime);
	else
	    XGrabPointer(dpy, Scr->Root, True,
		ButtonPressMask | ButtonReleaseMask,
		GrabModeAsync, GrabModeAsync,
		Scr->Root, cursor, CurrentTime);
	RootFunction = func;

	return (TRUE);
    }
    
    return (FALSE);
}



/***********************************************************************
 *
 *  Procedure:
 *	ReGrab - regrab the pointer with the LastCursor;
 *
 ***********************************************************************
 */

ReGrab()
{
    XGrabPointer(dpy, Scr->Root, True,
	ButtonPressMask | ButtonReleaseMask,
	GrabModeAsync, GrabModeAsync,
	Scr->Root, LastCursor, CurrentTime);
}



/***********************************************************************
 *
 *  Procedure:
 *	NeedToDefer - checks each function in the list to see if it
 *		is one that needs to be defered.
 *
 *  Inputs:
 *	root	- the menu root to check
 *
 ***********************************************************************
 */

NeedToDefer(root)
MenuRoot *root;
{
    MenuItem *mitem;

    for (mitem = root->first; mitem != NULL; mitem = mitem->next)
    {
	switch (mitem->func)
	{
	case F_IDENTIFY:
	case F_RESIZE:
	case F_MOVE:
	case F_FORCEMOVE:
	case F_DEICONIFY:
	case F_ICONIFY:
	case F_RAISELOWER:
	case F_RAISE:
	case F_LOWER:
	case F_FOCUS:
	case F_DESTROY:
	case F_WINREFRESH:
	case F_ZOOM:
	case F_FULLZOOM:
	case F_HORIZOOM:
        case F_RIGHTZOOM:
        case F_LEFTZOOM:
        case F_TOPZOOM:
        case F_BOTTOMZOOM:
	case F_AUTORAISE:
	    return TRUE;
	}
    }
    return FALSE;
}



/***********************************************************************
 *
 *  Procedure:
 *	Execute - execute the string by /bin/sh
 *
 *  Inputs:
 *	s	- the string containing the command
 *
 ***********************************************************************
 */

void
Execute(s)
    char *s;
{
#ifdef VMS
    createProcess(s);
#else
    static char buf[256];
    char *ds = DisplayString (dpy);
    char *colon, *dot1;
    char oldDisplay[256];
    char *doisplay;
    int restorevar = 0;
    SigProc	sig;

    oldDisplay[0] = '\0';
    doisplay=getenv("DISPLAY");
    if (doisplay)
	strcpy (oldDisplay, doisplay);

    /*
     * Build a display string using the current screen number, so that
     * X programs which get fired up from a menu come up on the screen
     * that they were invoked from, unless specifically overridden on
     * their command line.
     */
    colon = strrchr (ds, ':');
    if (colon) {			/* if host[:]:dpy */
	strcpy (buf, "DISPLAY=");
	strcat (buf, ds);
	colon = buf + 8 + (colon - ds);	/* use version in buf */
	dot1 = strchr (colon, '.');	/* first period after colon */
	if (!dot1) dot1 = colon + strlen (colon);  /* if not there, append */
	(void) sprintf (dot1, ".%d", Scr->screen);
	putenv (buf);
	restorevar = 1;
    }
#ifdef USE_SIGNALS
    sig = signal (SIGALRM, SIG_IGN);
    (void) system (s);
    signal (SIGALRM, sig);
#else  /* USE_SIGNALS */
    (void) system (s);
#endif  /* USE_SIGNALS */

    if (restorevar) {		/* why bother? */
	(void) sprintf (buf, "DISPLAY=%s", oldDisplay);
	putenv (buf);
    }
#endif
}



/***********************************************************************
 *
 *  Procedure:
 *	FocusOnRoot - put input focus on the root window
 *
 ***********************************************************************
 */

void
FocusOnRoot()
{
    SetFocus ((TwmWindow *) NULL, LastTimestamp());
    InstallWindowColormaps(0, &Scr->TwmRoot);
    if (! Scr->ClickToFocus) Scr->FocusRoot = TRUE;
}

DeIconify(tmp_win)
TwmWindow *tmp_win;
{
    TwmWindow *t;
    WList *wl;

    /* de-iconify the main window */
    if (Scr->WindowMask) XRaiseWindow (dpy, Scr->WindowMask);
    if (tmp_win->isicon)
    {
	if (tmp_win->icon_on)
	    Zoom(tmp_win->icon->w, tmp_win->frame);
	else if (tmp_win->group != (Window) 0)
	{
	    for (t = Scr->TwmRoot.next; t != NULL; t = t->next)
	    {
		if (tmp_win->group == t->w && t->icon_on)
		{
		    Zoom(t->icon->w, tmp_win->frame);
		    break;
		}
	    }
	}
    }

    XMapWindow(dpy, tmp_win->w);
    tmp_win->mapped = TRUE;
    if (Scr->NoRaiseDeicon)
	XMapWindow(dpy, tmp_win->frame);
    else
	XMapRaised(dpy, tmp_win->frame);
    SetMapStateProp(tmp_win, NormalState);

    if (tmp_win->icon && tmp_win->icon->w) {
	XUnmapWindow(dpy, tmp_win->icon->w);
	IconDown (tmp_win);
    }
    if (tmp_win->list)
      for (wl = tmp_win->list; wl != NULL; wl = wl->nextv)
	XUnmapWindow(dpy, wl->icon);
    if ((Scr->WarpCursor ||
	 LookInList(Scr->WarpCursorL, tmp_win->full_name, &tmp_win->class)) &&
	tmp_win->isicon)
      WarpToWindow (tmp_win);
    tmp_win->isicon = FALSE;
    tmp_win->icon_on = FALSE;


    /* now de-iconify and window group transients */
	for (t = Scr->TwmRoot.next; t != NULL; t = t->next)
	{
	  if ((t->transient && t->transientfor == tmp_win->w) ||
	      ((tmp_win->group == tmp_win->w) && (tmp_win->group == t->group) &&
	       (tmp_win->group != t->w) && t->isicon))
	    {
	      if (t->icon_on)
		Zoom(t->icon->w, t->frame);
	      else
	      if (tmp_win->icon)
		Zoom(tmp_win->icon->w, t->frame);
	      
	      XMapWindow(dpy, t->w);
	      t->mapped = TRUE;
	      if (Scr->NoRaiseDeicon)
		XMapWindow(dpy, t->frame);
	      else
		XMapRaised(dpy, t->frame);
	      SetMapStateProp(t, NormalState);
	      
	      if (t->icon && t->icon->w) {
		XUnmapWindow(dpy, t->icon->w);
		IconDown (t);
	      }
	      if (t->list) XUnmapWindow(dpy, t->list->icon);
	      t->isicon = FALSE;
	      t->icon_on = FALSE;
	      WMapDeIconify (t);
	    }
	}
    WMapDeIconify (tmp_win);
    if (! Scr->WindowMask && Scr->DeIconifyFunction.func != 0) {
	char *action;
	XEvent event;

	action = Scr->DeIconifyFunction.item ?
		Scr->DeIconifyFunction.item->action : NULL;
	ExecuteFunction (Scr->DeIconifyFunction.func, action,
			   (Window) 0, tmp_win, &event, C_ROOT, FALSE);
    }
    XSync (dpy, 0);
}



Iconify(tmp_win, def_x, def_y)
TwmWindow *tmp_win;
int def_x, def_y;
{
    TwmWindow *t;
    int iconify;
    XWindowAttributes winattrs;
    unsigned long eventMask;
    WList *wl;
    Window leader;
    Window blanket;
    int i;

    iconify = (!tmp_win->iconify_by_unmapping);
    t = (TwmWindow*) 0;
    if (tmp_win->transient) {
	leader = tmp_win->transientfor;
	for (t = Scr->TwmRoot.next; t != NULL; t = t->next) {
	    if (t->w == leader) break;
	}
    }
    else
    if (tmp_win->group != tmp_win->w) {
	leader = tmp_win->group;
	for (t = Scr->TwmRoot.next; t != NULL; t = t->next) {
	    if ((t->w == leader) && (t->group == t->w)) break;
	}
    }
    if (t && t->icon_on) iconify = False;
    if (iconify)
    {
	if (!tmp_win->icon || !tmp_win->icon->w)
	    CreateIconWindow(tmp_win, def_x, def_y);
	else
	    IconUp(tmp_win);
	if (OCCUPY (tmp_win, Scr->workSpaceMgr.activeWSPC)) {
	    if (Scr->WindowMask) {
		XRaiseWindow (dpy, Scr->WindowMask);
		XMapWindow(dpy, tmp_win->icon->w);
	    }
	    else
		XMapRaised(dpy, tmp_win->icon->w);
	}
    }
    if (tmp_win->list)
      for (wl = tmp_win->list; wl != NULL; wl = wl->nextv)
	XMapWindow(dpy, wl->icon);

    XGetWindowAttributes(dpy, tmp_win->w, &winattrs);
    eventMask = winattrs.your_event_mask;

    /* iconify transients and window group first */
    for (t = Scr->TwmRoot.next; t != NULL; t = t->next)
      {
	if ((t->transient && (t->transientfor == tmp_win->w)) ||
	    ((tmp_win->group == tmp_win->w) && (tmp_win->group == t->group) &&
		(tmp_win->group != t->w)))
	  {
	    if (iconify)
	      {
		if (t->icon_on)
			Zoom(t->icon->w, tmp_win->icon->w);
		else
		if (tmp_win->icon)
		  Zoom(t->frame, tmp_win->icon->w);
	      }
	    
	    /*
	     * Prevent the receipt of an UnmapNotify, since that would
	     * cause a transition to the Withdrawn state.
	     */
	    t->mapped = FALSE;
	    XSelectInput(dpy, t->w, eventMask & ~StructureNotifyMask);
	    XUnmapWindow(dpy, t->w);
	    XUnmapWindow(dpy, t->frame);
	    XSelectInput(dpy, t->w, eventMask);
	    if (t->icon && t->icon->w)
	      XUnmapWindow(dpy, t->icon->w);
	    SetMapStateProp(t, IconicState);
	    if (t == Scr->Focus)
	      {
		SetFocus ((TwmWindow *) NULL, LastTimestamp());
		if (! Scr->ClickToFocus) Scr->FocusRoot = TRUE;
	      }
	    if (t->list) XMapWindow(dpy, t->list->icon);
	    t->isicon = TRUE;
	    t->icon_on = FALSE;
	    WMapIconify (t);
	  }
      } 
    
    if (iconify)
	Zoom(tmp_win->frame, tmp_win->icon->w);

    /*
     * Prevent the receipt of an UnmapNotify, since that would
     * cause a transition to the Withdrawn state.
     */
    tmp_win->mapped = FALSE;

    if (! Scr->WindowMask && iconify && Scr->DoZoom && Scr->SmartIconify) {
	XSetWindowAttributes attr;

	XGetWindowAttributes(dpy, tmp_win->frame, &winattrs);
	attr.backing_store = NotUseful;
	attr.save_under    = False;
	blanket = XCreateWindow (dpy, Scr->Root, winattrs.x, winattrs.y, winattrs.width, winattrs.height,
			(unsigned int) 0,
			CopyFromParent, (unsigned int) CopyFromParent,
			(Visual *) CopyFromParent, CWBackingStore | CWSaveUnder,
			&attr);
	XMapWindow (dpy, blanket);
    }
    XSelectInput(dpy, tmp_win->w, eventMask & ~StructureNotifyMask);
    XUnmapWindow(dpy, tmp_win->w);
    XUnmapWindow(dpy, tmp_win->frame);
    XSelectInput(dpy, tmp_win->w, eventMask);
    SetMapStateProp(tmp_win, IconicState);

    if (! Scr->WindowMask && iconify && Scr->DoZoom && Scr->SmartIconify) {
	MosaicFade (blanket, winattrs.width, winattrs.height);
	XDestroyWindow (dpy, blanket);
    }
    if (tmp_win == Scr->Focus)
    {
	SetFocus ((TwmWindow *) NULL, LastTimestamp());
	if (! Scr->ClickToFocus) Scr->FocusRoot = TRUE;
    }
    tmp_win->isicon = TRUE;
    if (iconify)
	tmp_win->icon_on = TRUE;
    else
	tmp_win->icon_on = FALSE;
    WMapIconify (tmp_win);
    if (! Scr->WindowMask && Scr->IconifyFunction.func != 0) {
	char *action;
	XEvent event;

	action = Scr->IconifyFunction.item ?
		Scr->IconifyFunction.item->action : NULL;
	ExecuteFunction (Scr->IconifyFunction.func, action,
			   (Window) 0, tmp_win, &event, C_ROOT, FALSE);
    }
    XSync (dpy, 0);
}

static void Identify (t)
TwmWindow *t;
{
    int i, n, twidth, width, height;
    int x, y;
    unsigned int wwidth, wheight, bw, depth;
    Window junk;
    int px, py, dummy;
    unsigned udummy;

    n = 0;
    (void) sprintf(Info[n++], "Twm version:  %s", Version);
    (void) sprintf(Info[n], "Compile time options :");
#ifdef XPM
    (void) strcat (Info[n], " XPM");
#endif
#ifdef IMCONV
    (void) strcat (Info[n], ", IMCONV");
#endif
#ifdef USEM4
    (void) strcat (Info[n], ", USEM4");
#endif
#ifdef SOUNDS
    (void) strcat (Info[n], ", SOUNDS");
#endif
    n++;
    Info[n++][0] = '\0';

    if (t) {
	XGetGeometry (dpy, t->w, &JunkRoot, &JunkX, &JunkY,
		      &wwidth, &wheight, &bw, &depth);
	(void) XTranslateCoordinates (dpy, t->w, Scr->Root, 0, 0,
				      &x, &y, &junk);
	(void) sprintf(Info[n++], "Name             = \"%s\"", t->full_name);
	(void) sprintf(Info[n++], "Class.res_name   = \"%s\"", t->class.res_name);
	(void) sprintf(Info[n++], "Class.res_class  = \"%s\"", t->class.res_class);
	Info[n++][0] = '\0';
	(void) sprintf(Info[n++], "Geometry/root    = %dx%d+%d+%d", wwidth, wheight,
		x, y);
	(void) sprintf(Info[n++], "Border width     = %d", bw);
	(void) sprintf(Info[n++], "Depth            = %d", depth);
    }

    Info[n++][0] = '\0';
    (void) sprintf(Info[n++], "Click to dismiss....");

    /* figure out the width and height of the info window */
    height = n * (Scr->DefaultFont.height+2);
    width = 1;
    for (i = 0; i < n; i++)
    {
	twidth = XTextWidth(Scr->DefaultFont.font, Info[i],
	    strlen(Info[i]));
	if (twidth > width)
	    width = twidth;
    }
    if (InfoLines) XUnmapWindow(dpy, Scr->InfoWindow);

    width += 10;		/* some padding */
    if (XQueryPointer (dpy, Scr->Root, &JunkRoot, &JunkChild,
		       &dummy, &dummy, &px, &py, &udummy)) {
	px -= (width / 2);
	py -= (height / 3);
	if (px + width + BW2 >= Scr->MyDisplayWidth) 
	  px = Scr->MyDisplayWidth - width - BW2;
	if (py + height + BW2 >= Scr->MyDisplayHeight) 
	  py = Scr->MyDisplayHeight - height - BW2;
	if (px < 0) px = 0;
	if (py < 0) py = 0;
    } else {
	px = py = 0;
    }
    XMoveResizeWindow(dpy, Scr->InfoWindow, px, py, width, height);
    XMapRaised(dpy, Scr->InfoWindow); 
    InfoLines = n;
}



SetMapStateProp(tmp_win, state)
TwmWindow *tmp_win;
int state;
{
    unsigned long data[2];		/* "suggested" by ICCCM version 1 */
  
    data[0] = (unsigned long) state;
    data[1] = (unsigned long) (tmp_win->iconify_by_unmapping ? None : 
			   (tmp_win->icon ? tmp_win->icon->w : None));

    XChangeProperty (dpy, tmp_win->w, _XA_WM_STATE, _XA_WM_STATE, 32, 
		 PropModeReplace, (unsigned char *) data, 2);
}



Bool GetWMState (w, statep, iwp)
    Window w;
    int *statep;
    Window *iwp;
{
    Atom actual_type;
    int actual_format;
    unsigned long nitems, bytesafter;
    unsigned long *datap = NULL;
    Bool retval = False;

    if (XGetWindowProperty (dpy, w, _XA_WM_STATE, 0L, 2L, False, _XA_WM_STATE,
			    &actual_type, &actual_format, &nitems, &bytesafter,
			    (unsigned char **) &datap) != Success || !datap)
      return False;

    if (nitems <= 2) {			/* "suggested" by ICCCM version 1 */
	*statep = (int) datap[0];
	*iwp = (Window) datap[1];
	retval = True;
    }

    XFree ((char *) datap);
    return retval;
}



WarpToScreen (n, inc)
    int n, inc;
{
    Window dumwin;
    int x, y, dumint;
    unsigned int dummask;
    ScreenInfo *newscr = NULL;

    while (!newscr) {
					/* wrap around */
	if (n < 0) 
	  n = NumScreens - 1;
	else if (n >= NumScreens)
	  n = 0;

	newscr = ScreenList[n];
	if (!newscr) {			/* make sure screen is managed */
	    if (inc) {			/* walk around the list */
		n += inc;
		continue;
	    }
	    fprintf (stderr, "%s:  unable to warp to unmanaged screen %d\n", 
		     ProgramName, n);
	    XBell (dpy, 0);
	    return (1);
	}
    }

    if (Scr->screen == n) return (0);	/* already on that screen */

    PreviousScreen = Scr->screen;
    XQueryPointer (dpy, Scr->Root, &dumwin, &dumwin, &x, &y,
		   &dumint, &dumint, &dummask);

    XWarpPointer (dpy, None, newscr->Root, 0, 0, 0, 0, x, y);
    Scr = newscr;
    return (0);
}




/*
 * BumpWindowColormap - rotate our internal copy of WM_COLORMAP_WINDOWS
 */

BumpWindowColormap (tmp, inc)
    TwmWindow *tmp;
    int inc;
{
    int i, j, previously_installed;
    ColormapWindow **cwins;

    if (!tmp) return (1);

    if (inc && tmp->cmaps.number_cwins > 0) {
	cwins = (ColormapWindow **) malloc(sizeof(ColormapWindow *)*
					   tmp->cmaps.number_cwins);
	if (cwins) {		
	    if (previously_installed =
		/* SUPPRESS 560 */(Scr->cmapInfo.cmaps == &tmp->cmaps &&
	        tmp->cmaps.number_cwins)) {
		for (i = tmp->cmaps.number_cwins; i-- > 0; )
		    tmp->cmaps.cwins[i]->colormap->state = 0;
	    }

	    for (i = 0; i < tmp->cmaps.number_cwins; i++) {
		j = i - inc;
		if (j >= tmp->cmaps.number_cwins)
		    j -= tmp->cmaps.number_cwins;
		else if (j < 0)
		    j += tmp->cmaps.number_cwins;
		cwins[j] = tmp->cmaps.cwins[i];
	    }

	    free((char *) tmp->cmaps.cwins);

	    tmp->cmaps.cwins = cwins;

	    if (tmp->cmaps.number_cwins > 1)
		memset (tmp->cmaps.scoreboard, 0, 
		       ColormapsScoreboardLength(&tmp->cmaps));

	    if (previously_installed)
		InstallWindowColormaps(PropertyNotify, (TwmWindow *) NULL);
	}
    } else
	FetchWmColormapWindows (tmp);
}



ShowIconManager () {
    IconMgr		*i;
    WorkSpaceList	*wl;

    if (! Scr->workSpaceManagerActive) return;

    if (Scr->NoIconManagers) return;
    for (wl = Scr->workSpaceMgr.workSpaceList; wl != NULL; wl = wl->next) {
	for (i = wl->iconmgr; i != NULL; i = i->next) {
	    if (i->count == 0) continue;
	    if (OCCUPY (i->twm_win, Scr->workSpaceMgr.activeWSPC)) {
		SetMapStateProp (i->twm_win, NormalState);
		XMapWindow (dpy, i->twm_win->w);
		XMapRaised (dpy, i->twm_win->frame);
		if (i->twm_win->icon && i->twm_win->icon->w)
		    XUnmapWindow (dpy, i->twm_win->icon->w);
	    }
	    i->twm_win->mapped = TRUE;
	    i->twm_win->isicon = FALSE;
	}
    }
}


HideIconManager () {
    IconMgr		*i;
    WorkSpaceList	*wl;

    if (Scr->NoIconManagers) return;
    for (wl = Scr->workSpaceMgr.workSpaceList; wl != NULL; wl = wl->next) {
	for (i = wl->iconmgr; i != NULL; i = i->next) {
	    SetMapStateProp (i->twm_win, WithdrawnState);
	    XUnmapWindow(dpy, i->twm_win->frame);
	    if (i->twm_win->icon && i->twm_win->icon->w) XUnmapWindow (dpy, i->twm_win->icon->w);
	    i->twm_win->mapped = FALSE;
	    i->twm_win->isicon = TRUE;
	}
    }
}




DestroyMenu (menu)
    MenuRoot *menu;
{
    MenuItem *item;

    if (menu->w) {
	XDeleteContext (dpy, menu->w, MenuContext);
	XDeleteContext (dpy, menu->w, ScreenContext);
	if (Scr->Shadow) XDestroyWindow (dpy, menu->shadow);
	XDestroyWindow(dpy, menu->w);
    }

    for (item = menu->first; item; ) {
	MenuItem *tmp = item;
	item = item->next;
	free ((char *) tmp);
    }
}



/*
 * warping routines
 */
#define ONSCREEN(r)  OCCUPY((r), Scr->workSpaceMgr.activeWSPC)

void WarpAlongRing (ev, forward)
    XButtonEvent *ev;
    Bool forward;
{
    TwmWindow *r, *head;

    if (Scr->RingLeader)
      head = Scr->RingLeader;
    else if (!(head = Scr->Ring)) 
      return;

    if (forward) {
	for (r = head->ring.next; r != head; r = r->ring.next) {
	    if (!r) break;
	    if (r->mapped && (Scr->WarpRingAnyWhere || ONSCREEN(r))) break;
	}
    } else {
	for (r = head->ring.prev; r != head; r = r->ring.prev) {
	    if (!r) break;
	    if (r->mapped && (Scr->WarpRingAnyWhere || ONSCREEN(r))) break;
	}
    }

    if (r && r != head) {
	TwmWindow *p = Scr->RingLeader, *t;

	Scr->RingLeader = r;
	WarpToWindow (r);

	if (p && p->mapped &&
	    XFindContext (dpy, ev->window, TwmContext, (caddr_t *)&t) == XCSUCCESS &&
	    p == t) {
	    p->ring.cursor_valid = True;
	    p->ring.curs_x = ev->x_root - t->frame_x;
	    p->ring.curs_y = ev->y_root - t->frame_y;
	    if (p->ring.curs_x < -p->frame_bw || 
		p->ring.curs_x >= p->frame_width + p->frame_bw ||
		p->ring.curs_y < -p->frame_bw || 
		p->ring.curs_y >= p->frame_height + p->frame_bw) {
		/* somehow out of window */
		p->ring.curs_x = p->frame_width / 2;
		p->ring.curs_y = p->frame_height / 2;
	    }
	}
    }
}



void WarpToWindow (t)
    TwmWindow *t;
{
    int x, y;

    if (t->auto_raise || !Scr->NoRaiseWarp) AutoRaiseWindow (t);
    if (t->ring.cursor_valid) {
	x = t->ring.curs_x;
	y = t->ring.curs_y;
    } else {
	x = t->frame_width / 2;
	y = t->frame_height / 2;
    }
    if (! OCCUPY (t, Scr->workSpaceMgr.activeWSPC)) {
	WorkSpaceList *wlist;

	for (wlist = Scr->workSpaceMgr.workSpaceList; wlist != NULL; wlist = wlist->next) {
	    if (OCCUPY (t, wlist)) break;
	}
	if (wlist != NULL) GotoWorkSpace (wlist);
    }
    XWarpPointer (dpy, None, t->frame, 0, 0, 0, 0, x, y);
}




/*
 * ICCCM Client Messages - Section 4.2.8 of the ICCCM dictates that all
 * client messages will have the following form:
 *
 *     event type	ClientMessage
 *     message type	_XA_WM_PROTOCOLS
 *     window		tmp->w
 *     format		32
 *     data[0]		message atom
 *     data[1]		time stamp
 */
static void send_clientmessage (w, a, timestamp)
    Window w;
    Atom a;
    Time timestamp;
{
    XClientMessageEvent ev;

    ev.type = ClientMessage;
    ev.window = w;
    ev.message_type = _XA_WM_PROTOCOLS;
    ev.format = 32;
    ev.data.l[0] = a;
    ev.data.l[1] = timestamp;
    XSendEvent (dpy, w, False, 0L, (XEvent *) &ev);
}

SendDeleteWindowMessage (tmp, timestamp)
    TwmWindow *tmp;
    Time timestamp;
{
    send_clientmessage (tmp->w, _XA_WM_DELETE_WINDOW, timestamp);
}

SendSaveYourselfMessage (tmp, timestamp)
    TwmWindow *tmp;
    Time timestamp;
{
    send_clientmessage (tmp->w, _XA_WM_SAVE_YOURSELF, timestamp);
}


SendTakeFocusMessage (tmp, timestamp)
    TwmWindow *tmp;
    Time timestamp;
{
    send_clientmessage (tmp->w, _XA_WM_TAKE_FOCUS, timestamp);
}

MoveMenu (eventp)
XEvent *eventp;
{
    int    XW, YW, newX, newY, cont;
    Bool   newev;
    XEvent ev;

    if (! ActiveMenu) return (1);
    if (! ActiveMenu->pinned) return (1);

    XW = eventp->xbutton.x_root - ActiveMenu->x;
    YW = eventp->xbutton.y_root - ActiveMenu->y;
    XGrabPointer (dpy, ActiveMenu->w, True,
		ButtonPressMask  | ButtonReleaseMask | ButtonMotionMask,
		GrabModeAsync, GrabModeAsync,
		None, Scr->MoveCursor, CurrentTime);

    newX = ActiveMenu->x;
    newY = ActiveMenu->y;
    cont = TRUE;
    XMaskEvent (dpy, ButtonPressMask | ButtonMotionMask | ButtonReleaseMask, &ev);
    while (cont) {
	ev.xbutton.x_root -= Scr->MyDisplayX;
	ev.xbutton.y_root -= Scr->MyDisplayY;
	switch (ev.xany.type) {
	    case ButtonRelease :
        	newX = ev.xbutton.x_root - XW;
        	newY = ev.xbutton.y_root - YW;
		cont = FALSE;
		break;
	    case MotionNotify :
		newev = False;
		while (XCheckMaskEvent (dpy, ButtonMotionMask | ButtonReleaseMask, &ev)) {
		    newev = True;
		    if (ev.type == ButtonRelease) break;
		}
		if (ev.type == ButtonRelease) continue;
		if (newev) {
		    ev.xbutton.x_root -= Scr->MyDisplayX;
		    ev.xbutton.y_root -= Scr->MyDisplayY;
		}
        	newX = ev.xbutton.x_root - XW;
        	newY = ev.xbutton.y_root - YW;
		XMoveWindow (dpy, ActiveMenu->w, newX, newY);
		XMaskEvent (dpy, ButtonPressMask | ButtonMotionMask | ButtonReleaseMask, &ev);
		break;
	    case ButtonPress :
		cont = FALSE;
		newX = ActiveMenu->x;
		newY = ActiveMenu->y;
		break;
	}
    }
    XUngrabPointer (dpy, CurrentTime);
    if (ev.xany.type == ButtonRelease) ButtonPressed = -1;
    /*XPutBackEvent (dpy, &ev);*/
    XMoveWindow (dpy, ActiveMenu->w, newX, newY);
    ActiveMenu->x = newX;
    ActiveMenu->y = newY;
    MenuOrigins [MenuDepth - 1].x = newX;
    MenuOrigins [MenuDepth - 1].y = newY;
}

/***********************************************************************
 *
 *  Procedure:
 *      DisplayPosition - display the position in the dimensions window
 *
 *  Inputs:
 *      tmp_win - the current twm window
 *      x, y    - position of the window
 *
 ***********************************************************************
 */

void DisplayPosition (tmp_win, x, y)
TwmWindow *tmp_win;
int x, y;
{
    char str [100];
    char signx = '+';
    char signy = '+';

    if (x < 0) {
	x = -x;
	signx = '-';
    }
    if (y < 0) {
	y = -y;
	signy = '-';
    }
    (void) sprintf (str, " %c%-4d %c%-4d ", signx, x, signy, y);
    XRaiseWindow (dpy, Scr->SizeWindow);
    FBF (Scr->DefaultC.fore, Scr->DefaultC.back, Scr->SizeFont.font->fid);
    XDrawImageString (dpy, Scr->SizeWindow, Scr->NormalGC,
		      Scr->SizeStringOffset,
		      Scr->SizeFont.font->ascent + SIZE_VINDENT,
		      str, 13);
}

MosaicFade (w, width, height)
Window	w;
int	width, height;
{
    int		srect;
    int		i, j, x, y, usec, nrects;
    Pixmap	mask;
    GC		gc;
    XGCValues	gcv;
#ifdef VMS
    float  timeout;
#else
    struct timeval timeout;
#endif
    XRectangle *rectangles;

    srect = (width < height) ? (width / 20) : (height / 20);
    mask  = XCreatePixmap (dpy, w, width, height, 1);

    gcv.foreground = 1;
    gc = XCreateGC (dpy, mask, GCForeground, &gcv);
    XFillRectangle (dpy, mask, gc, 0, 0, width, height);
    gcv.function = GXclear;
    XChangeGC (dpy, gc, GCFunction, &gcv);

#ifdef VMS
    timeout = 0.010;
#else
    usec = 10000;
    timeout.tv_usec = usec % (unsigned long) 1000000;
    timeout.tv_sec  = usec / (unsigned long) 1000000;
#endif

    nrects = ((width * height) / (srect * srect)) / 10;
    rectangles = (XRectangle*) malloc (nrects * sizeof (XRectangle));
    for (j = 0; j < nrects; j++) {
	rectangles [j].width  = srect;
	rectangles [j].height = srect;
    }

    for (i = 0; i < 10; i++) {
	for (j = 0; j < nrects; j++) {
	    rectangles [j].x = ((lrand48 () %  width) / srect) * srect;
	    rectangles [j].y = ((lrand48 () % height) / srect) * srect;
	}
	XFillRectangles (dpy, mask, gc, rectangles, nrects);
	XShapeCombineMask (dpy, w, ShapeBounding, 0, 0, mask, ShapeSet);
	XFlush (dpy);
#ifdef VMS
        lib$wait(&timeout);
#else
	select (0, (void *) 0, (void *) 0, (void *) 0, &timeout);
#endif
    }
    XFreePixmap (dpy, mask);
    XFreeGC (dpy, gc);
    free (rectangles);
}


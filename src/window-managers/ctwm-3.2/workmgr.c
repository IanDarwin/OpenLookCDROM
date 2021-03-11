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
#include <stdio.h>
#if defined(sony_news)
#  include <ctype.h>
#endif
#include "twm.h"
#include "util.h"
#include "parse.h"
#include "screen.h"
#include "resize.h"
#include "add_window.h"
#include "events.h"
#include "gram.h"
#include "siconify.bm"
#ifdef VMS
#include <ctype.h>
#include <string.h>
#include <decw$include/Xos.h>
#include <decw$include/Xatom.h>
#include <X11Xmu/CharSet.h>
#include <decw$include/Xresource.h>
#else
#include <X11/Xos.h>
#include <X11/Xatom.h>
#include <X11/Xmu/CharSet.h>
#include <X11/Xresource.h>
#endif
#ifdef macII
int strcmp(); /* missing from string.h in AUX 2.0 */
#endif
#ifdef BUGGY_HP700_SERVER
static void fakeRaiseLower ();
#endif

/***********************************************************************
 *
 *  Procedure:
 *	CreateWorkSpaceManager - create teh workspace manager window
 *		for this screen.
 *
 *  Returned Value:
 *	none
 *
 *  Inputs:
 *	none
 *
 ***********************************************************************
 */
#define WSPCWINDOW    0
#define OCCUPYWINDOW  1
#define OCCUPYBUTTON  2

static void Vanish			();
static void DisplayWin			();
static void CreateWorkSpaceManagerWindow ();
static void CreateOccupyWindow		();
static unsigned int GetMaskFromResource	();
unsigned int GetMaskFromProperty	();
static   int GetPropertyFromMask	();
static void PaintWorkSpaceManagerBorder	();
static void PaintButton			();
static void WMapRemoveFromList		();
static void WMapAddToList		();
static void ResizeWorkSpaceManager	();
static void ResizeOccupyWindow		();

Atom _XA_WM_OCCUPATION;
Atom _XA_WM_CURRENTWORKSPACE;
Atom _XA_WM_WORKSPACESLIST;
Atom _OL_WIN_ATTR;

int       fullOccupation    = 0;
int       useBackgroundInfo = False;
XContext  MapWListContext = (XContext) 0;
static Cursor handCursor  = (Cursor) 0;

extern Bool MaybeAnimate;

void InitWorkSpaceManager ()
{
    Scr->workSpaceMgr.count			= 0;
    Scr->workSpaceMgr.workSpaceList		= NULL;

    Scr->workSpaceMgr.workspaceWindow.name	= "WorkSpaceManager";
    Scr->workSpaceMgr.workspaceWindow.icon_name	= "WorkSpaceManager Icon";
    Scr->workSpaceMgr.workspaceWindow.geometry	= NULL;
    Scr->workSpaceMgr.workspaceWindow.columns	= 0;
    Scr->workSpaceMgr.workspaceWindow.twm_win	= (TwmWindow*) 0;
    Scr->workSpaceMgr.workspaceWindow.state	= BUTTONSSTATE;
    Scr->workSpaceMgr.workspaceWindow.noshowoccupyall = FALSE;
    Scr->workSpaceMgr.workspaceWindow.windowFont.name	=
			"-adobe-courier-bold-r-normal--8-80-75-75-m-50-iso8859-1";
    Scr->workSpaceMgr.workspaceWindow.windowcp.back  = Scr->White;
    Scr->workSpaceMgr.workspaceWindow.windowcp.fore  = Scr->Black;

    Scr->workSpaceMgr.workspaceWindow.curImage       = None;
    Scr->workSpaceMgr.workspaceWindow.curColors.back = Scr->Black;
    Scr->workSpaceMgr.workspaceWindow.curColors.fore = Scr->White;

    Scr->workSpaceMgr.workspaceWindow.defImage       = None;
    Scr->workSpaceMgr.workspaceWindow.defColors.back = Scr->White;
    Scr->workSpaceMgr.workspaceWindow.defColors.fore = Scr->Black;


    Scr->workSpaceMgr.occupyWindow.name		= "Occupy Window";
    Scr->workSpaceMgr.occupyWindow.icon_name	= "Occupy Window Icon";
    Scr->workSpaceMgr.occupyWindow.geometry	= NULL;
    Scr->workSpaceMgr.occupyWindow.columns	= 0;
    Scr->workSpaceMgr.occupyWindow.twm_win	= (TwmWindow*) 0;

    XrmInitialize ();
    if (MapWListContext == (XContext) 0) MapWListContext = XUniqueContext ();
}

ConfigureWorkSpaceManager () {
    Scr->workSpaceMgr.workspaceWindow.vspace	= Scr->WMgrVertButtonIndent;
    Scr->workSpaceMgr.workspaceWindow.hspace	= Scr->WMgrHorizButtonIndent;
    Scr->workSpaceMgr.occupyWindow.vspace	= Scr->WMgrVertButtonIndent;
    Scr->workSpaceMgr.occupyWindow.hspace	= Scr->WMgrHorizButtonIndent;
}

void CreateWorkSpaceManager () {
    WorkSpaceList *wlist;
    char wrkSpcList [512];
    int len;

    if (! Scr->workSpaceManagerActive) return;

    _XA_WM_OCCUPATION       = XInternAtom (dpy, "WM_OCCUPATION",       False);
    _XA_WM_CURRENTWORKSPACE = XInternAtom (dpy, "WM_CURRENTWORKSPACE", False);
    _XA_WM_WORKSPACESLIST   = XInternAtom (dpy, "WM_WORKSPACESLIST",   False);
    _OL_WIN_ATTR            = XInternAtom (dpy, "_OL_WIN_ATTR",        False);

    NewFontCursor (&handCursor, "hand2");

    Scr->workSpaceMgr.activeWSPC = Scr->workSpaceMgr.workSpaceList;
    CreateWorkSpaceManagerWindow ();
    CreateOccupyWindow ();

    wlist = Scr->workSpaceMgr.activeWSPC;
    if (Scr->workSpaceMgr.workspaceWindow.curImage == None)
	XSetWindowBackground (dpy, wlist->mapSubwindow.w,
		Scr->workSpaceMgr.workspaceWindow.curColors.back);
    else
	XSetWindowBackgroundPixmap (dpy, wlist->mapSubwindow.w,
		Scr->workSpaceMgr.workspaceWindow.curImage->pixmap);
    XClearWindow (dpy, wlist->mapSubwindow.w);

    len = GetPropertyFromMask (0xFFFFFFFF, wrkSpcList);
    XChangeProperty (dpy, Scr->Root, _XA_WM_WORKSPACESLIST, XA_STRING, 8, 
		     PropModeReplace, (unsigned char *) wrkSpcList, len);
}

void GotoWorkSpaceByName (wname)
char *wname;
{
    WorkSpaceList *wlist;

    for (wlist = Scr->workSpaceMgr.workSpaceList; wlist != NULL; wlist = wlist->next) {
	if (strcmp (wlist->label, wname) == 0) break;
    }
    if (wlist == NULL) {
	for (wlist = Scr->workSpaceMgr.workSpaceList; wlist != NULL; wlist = wlist->next) {
	    if (strcmp (wlist->name, wname) == 0) break;
	}
    }
    if (wlist == NULL) return;
    GotoWorkSpace (wlist);
}

void GotoPrevWorkSpace () {
    WorkSpaceList *wlist1, *wlist2;

    wlist1 = Scr->workSpaceMgr.workSpaceList;
    if (wlist1 == NULL) return;

    wlist2 = wlist1->next;
    while ((wlist2 != Scr->workSpaceMgr.activeWSPC) && (wlist2 != NULL)) {
	wlist1 = wlist2;
	wlist2 = wlist2->next;
    }
    GotoWorkSpace (wlist1);
}

void GotoNextWorkSpace () {
    WorkSpaceList *wlist;

    wlist = Scr->workSpaceMgr.activeWSPC;
    wlist = (wlist->next != NULL) ? wlist->next : Scr->workSpaceMgr.workSpaceList;
    GotoWorkSpace (wlist);
}

void GotoWorkSpace (wlist)
WorkSpaceList *wlist;
{
    TwmWindow	  *twmWin;
    WorkSpaceList *oldscr, *newscr;
    WList	  *wl, *wl1;
    XSetWindowAttributes attr;
    XWindowAttributes    winattrs;
    unsigned long	 eventMask;
    Window	  w;
    unsigned long valuemask;

    oldscr = Scr->workSpaceMgr.activeWSPC;
    newscr = wlist;
    if (oldscr == newscr) return;

    valuemask = (CWBackingStore | CWSaveUnder);
    attr.backing_store = NotUseful;
    attr.save_under    = False;
    w = XCreateWindow (dpy, Scr->Root, 0, 0,
			(unsigned int) Scr->MyDisplayWidth,
			(unsigned int) Scr->MyDisplayHeight,
			(unsigned int) 0,
			CopyFromParent, (unsigned int) CopyFromParent,
			(Visual *) CopyFromParent, valuemask,
			&attr);
    XMapWindow (dpy, w);

    if (useBackgroundInfo && ! Scr->DontPaintRootWindow) {
	if (newscr->image == None)
	    XSetWindowBackground (dpy, Scr->Root, newscr->backcp.back);
	else
	    XSetWindowBackgroundPixmap (dpy, Scr->Root, newscr->image->pixmap);

	XClearWindow (dpy, Scr->Root);
    }
    for (twmWin = &(Scr->TwmRoot); twmWin != NULL; twmWin = twmWin->next) {
	if (OCCUPY (twmWin, oldscr)) {
	    if (twmWin->mapped == FALSE) {
		if ((twmWin->icon_on == TRUE) && (twmWin->icon) && (twmWin->icon->w)) {
		    XUnmapWindow(dpy, twmWin->icon->w);
		    IconDown (twmWin);
		}
	    }
	    else
	    if (! OCCUPY (twmWin, newscr)) {
		Vanish (twmWin);
	    }
	}
    }
    for (twmWin = &(Scr->TwmRoot); twmWin != NULL; twmWin = twmWin->next) {
	if (OCCUPY (twmWin, newscr)) {
	    if (twmWin->mapped == FALSE) {
		if ((twmWin->icon_on == TRUE) && (twmWin->icon) && (twmWin->icon->w)) {
		    IconUp (twmWin);
		    XMapWindow (dpy, twmWin->icon->w);
		}
	    }
	    else
	    if (! OCCUPY (twmWin, oldscr)) {
		DisplayWin (twmWin);
	    }
	}
    }
/*
   Reorganize window lists
*/
    for (twmWin = &(Scr->TwmRoot); twmWin != NULL; twmWin = twmWin->next) {
	wl = twmWin->list;
	if (wl == NULL) continue;
	if (OCCUPY (wl->iconmgr->twm_win, newscr)) continue;
	wl1 = wl;
	wl  = wl->nextv;
	while (wl != NULL) {
	    if (OCCUPY (wl->iconmgr->twm_win, newscr)) break;
	    wl1 = wl;
	    wl  = wl->nextv;
	}
	if (wl != NULL) {
	    wl1->nextv = wl->nextv;
	    wl->nextv  = twmWin->list;
	    twmWin->list = wl;
	}
    }
    Scr->workSpaceMgr.activeWSPC = newscr;
    PaintButton (WSPCWINDOW, oldscr->buttonw, oldscr->label, oldscr->cp, off);
    PaintButton (WSPCWINDOW, newscr->buttonw, newscr->label, newscr->cp,  on);
    oldscr->iconmgr = Scr->iconmgr;
    Scr->iconmgr = newscr->iconmgr;

    if (useBackgroundInfo) {
	if (oldscr->image == None)
	    XSetWindowBackground (dpy, oldscr->mapSubwindow.w, oldscr->backcp.back);
	else
	    XSetWindowBackgroundPixmap (dpy, oldscr->mapSubwindow.w, oldscr->image->pixmap);
    }
    else {
	if (Scr->workSpaceMgr.workspaceWindow.defImage == None)
	    XSetWindowBackground (dpy, oldscr->mapSubwindow.w,
			Scr->workSpaceMgr.workspaceWindow.defColors.back);
	else
	    XSetWindowBackgroundPixmap (dpy, oldscr->mapSubwindow.w,
			Scr->workSpaceMgr.workspaceWindow.defImage->pixmap);
    }
    attr.border_pixel = Scr->workSpaceMgr.workspaceWindow.defBorderColor;
    XChangeWindowAttributes (dpy, oldscr->mapSubwindow.w, CWBorderPixel, &attr);

    if (Scr->workSpaceMgr.workspaceWindow.curImage == None) {
	XSetWindowBackground (dpy, newscr->mapSubwindow.w,
			Scr->workSpaceMgr.workspaceWindow.curColors.back);
    }
    else {
	XSetWindowBackgroundPixmap (dpy, newscr->mapSubwindow.w,
			Scr->workSpaceMgr.workspaceWindow.curImage->pixmap);
    }
    attr.border_pixel = Scr->workSpaceMgr.workspaceWindow.curBorderColor;
    XChangeWindowAttributes (dpy, newscr->mapSubwindow.w, CWBorderPixel, &attr);

    XClearWindow (dpy, oldscr->mapSubwindow.w);
    XClearWindow (dpy, newscr->mapSubwindow.w);

    XGetWindowAttributes(dpy, Scr->Root, &winattrs);
    eventMask = winattrs.your_event_mask;
    XSelectInput(dpy, Scr->Root, eventMask & ~PropertyChangeMask);

    XChangeProperty (dpy, Scr->Root, _XA_WM_CURRENTWORKSPACE, XA_STRING, 8, 
		     PropModeReplace, (unsigned char *) newscr->name, strlen (newscr->name));
    XSelectInput(dpy, Scr->Root, eventMask);

    XDestroyWindow (dpy, w);
    if (Scr->ChangeWorkspaceFunction.func != 0) {
	char *action;
	XEvent event;

	action = Scr->ChangeWorkspaceFunction.item ?
		Scr->ChangeWorkspaceFunction.item->action : NULL;
	ExecuteFunction (Scr->ChangeWorkspaceFunction.func, action,
			   (Window) 0, (TwmWindow*) 0, &event, C_ROOT, FALSE);
    }
    /*XFlush (dpy);*/
    XSync (dpy, 0);
    MaybeAnimate = True;
}

void AddWorkSpace (name, background, foreground, backback, backfore, backpix)
char *name, *background, *foreground, *backback, *backfore, *backpix;
{
    WorkSpaceList *wlist, *wl;
    int	  	  scrnum;
    Pixmap	  pix;
    unsigned int  width, height, depth;
    XGCValues	  gcvalues;
    Image	  *image;

    scrnum = Scr->workSpaceMgr.count;
    if (scrnum == MAXWORKSPACE) return;

    fullOccupation     |= (1 << scrnum);
    wlist		= (WorkSpaceList*) malloc (sizeof (WorkSpaceList));
#ifdef VMS
    {
       char *ftemp;
       ftemp = (char *) malloc((strlen(name)+1)*sizeof(char));
       wlist->name = strcpy (ftemp,name);
       ftemp = (char *) malloc((strlen(name)+1)*sizeof(char));
       wlist->label = strcpy (ftemp,name);
    }
#else
    wlist->name		= (char*) strdup (name);
    wlist->label	= (char*) strdup (name);
    wlist->clientlist	= NULL;
#endif

    if (background == NULL)
	wlist->cp.back = Scr->IconManagerC.back;
    else
	GetColor (Scr->Monochrome, &(wlist->cp.back), background);

    if (foreground == NULL)
	wlist->cp.fore = Scr->IconManagerC.fore;
    else
	GetColor (Scr->Monochrome, &(wlist->cp.fore), foreground);

#ifdef COLOR_BLIND_USER
    wlist->cp.shadc   = Scr->White;
    wlist->cp.shadd   = Scr->Black;
#else
    if (!Scr->BeNiceToColormap) GetShadeColors (&wlist->cp);
#endif

    if (backback == NULL)
	GetColor (Scr->Monochrome, &(wlist->backcp.back), "Black");
    else {
	GetColor (Scr->Monochrome, &(wlist->backcp.back), backback);
	useBackgroundInfo = True;
    }

    if (backfore == NULL)
	GetColor (Scr->Monochrome, &(wlist->backcp.fore), "White");
    else {
	GetColor (Scr->Monochrome, &(wlist->backcp.fore), backfore);
	useBackgroundInfo = True;
    }
    if ((image = GetImage (backpix, wlist->backcp)) != None) {
	wlist->image = image;
	useBackgroundInfo = True;
    }
    else {
	wlist->image = None;
    }
    wlist->next        = NULL;
    wlist->number      = scrnum;
    Scr->workSpaceMgr.count++;

    if (Scr->workSpaceMgr.workSpaceList == NULL) {
	Scr->workSpaceMgr.workSpaceList = wlist;
    }
    else {
	wl = Scr->workSpaceMgr.workSpaceList;
	while (wl->next != NULL) {wl = wl->next;}
	wl->next = wlist;
    }
    Scr->workSpaceManagerActive = 1;
}

static XrmDatabase db;
static XrmOptionDescRec table [] = {
    {"-xrm",		NULL,		XrmoptionResArg, (caddr_t) NULL},
};

void SetupOccupation (twm_win)
TwmWindow *twm_win;
{
    TwmWindow		*t;
    unsigned char	*prop;
    unsigned long	nitems, bytesafter;
    Atom		actual_type;
    int			actual_format;
    int			state;
    Window		icon;
    char		**cliargv = NULL;
    int			cliargc;
    Bool		status;
    char		*str_type;
    XrmValue		value;
    char		wrkSpcList [512];
    int			len;
    WorkSpaceList	*wlist;
    XWindowAttributes   winattrs;
    unsigned long	eventMask;

    if (! Scr->workSpaceManagerActive) {
	twm_win->occupation = 1;
	return;
    }
    twm_win->occupation = 0;

    for (wlist = Scr->workSpaceMgr.workSpaceList; wlist != NULL; wlist = wlist->next) {
	if (LookInList (wlist->clientlist, twm_win->full_name, &twm_win->class)) {
            twm_win->occupation |= 1 << wlist->number;
	}
    }

    if (LookInList (Scr->OccupyAll, twm_win->full_name, &twm_win->class)) {
        twm_win->occupation = fullOccupation;
    }

    if (XGetCommand (dpy, twm_win->w, &cliargv, &cliargc)) {
	XrmParseCommand (&db, table, 1, "ctwm", &cliargc, cliargv);
	status = XrmGetResource (db, "ctwm.workspace", "Ctwm.Workspace", &str_type, &value);
	if ((status == True) && (value.size != 0)) {
	    strncpy (wrkSpcList, value.addr, value.size);
	    twm_win->occupation = GetMaskFromResource (twm_win, wrkSpcList);
	}
	XrmDestroyDatabase (db);
	db = NULL;
    }

    if (RestartPreviousState) {
	if (XGetWindowProperty (dpy, twm_win->w, _XA_WM_OCCUPATION, 0L, 2500, False,
				XA_STRING, &actual_type, &actual_format, &nitems,
				&bytesafter, &prop) == Success) {
	    if (nitems != 0) twm_win->occupation = GetMaskFromProperty (prop, nitems);
	}
    }

    if (twm_win->iconmgr) /* someone tried to modify occupation of icon managers */
	twm_win->occupation = 1 << Scr->workSpaceMgr.activeWSPC->number;

    if (! Scr->TransientHasOccupation) {
	if (twm_win->transient) {
	    for (t = Scr->TwmRoot.next; t != NULL; t = t->next) {
		if (twm_win->transientfor == t->w) break;
	    }
	    if (t != NULL) twm_win->occupation = t->occupation;
	}
	else
	if (twm_win->group != twm_win->w) {
	    for (t = Scr->TwmRoot.next; t != NULL; t = t->next) {
		if (t->w == twm_win->group) break;
	    }
	    if (t != NULL) twm_win->occupation = t->occupation;
	}
    }
    if ((twm_win->occupation & fullOccupation) == 0)
	twm_win->occupation = 1 << Scr->workSpaceMgr.activeWSPC->number;

    len = GetPropertyFromMask (twm_win->occupation, wrkSpcList);

    XGetWindowAttributes(dpy, twm_win->w, &winattrs);
    eventMask = winattrs.your_event_mask;
    XSelectInput(dpy, twm_win->w, eventMask & ~PropertyChangeMask);

    XChangeProperty (dpy, twm_win->w, _XA_WM_OCCUPATION, XA_STRING, 8, 
		     PropModeReplace, (unsigned char *) wrkSpcList, len);
    XSelectInput(dpy, twm_win->w, eventMask);

/* kludge */
    state = NormalState;
    if (!(RestartPreviousState && GetWMState (twm_win->w, &state, &icon) &&
	 (state == NormalState || state == IconicState || state == InactiveState))) {
	if (twm_win->wmhints && (twm_win->wmhints->flags & StateHint))
	    state = twm_win->wmhints->initial_state;
    }
    if (OCCUPY (twm_win, Scr->workSpaceMgr.activeWSPC)) {
	if (state == InactiveState) SetMapStateProp (twm_win, NormalState);
    }
    else {
	if (state == NormalState) SetMapStateProp (twm_win, InactiveState);
    }
}

static TwmWindow *occupyWin = (TwmWindow*) 0;

void Occupy (twm_win)
TwmWindow *twm_win;
{
    int			x, y, junkX, junkY;
    unsigned int	junkB, junkD;
    unsigned int	width, height;
    int			xoffset, yoffset;
    Window		junkW, w;
    unsigned int	junkK;
    WorkSpaceList	*wlist;

    if (! Scr->workSpaceManagerActive) return;
    if (occupyWin != (TwmWindow*) 0) return;
    if (twm_win->iconmgr) return;
    if (! Scr->TransientHasOccupation) {
	if (twm_win->transient) return;
	if ((twm_win->group != (Window) 0) && (twm_win->group != twm_win->w)) return;
    }
    for (wlist = Scr->workSpaceMgr.workSpaceList; wlist != NULL; wlist = wlist->next) {
	if (OCCUPY (twm_win, wlist)) {
	    PaintButton (OCCUPYWINDOW, wlist->obuttonw, wlist->label, wlist->cp, on);
	    Scr->workSpaceMgr.occupyWindow.tmpOccupation |= (1 << wlist->number);
	}
	else {
	    PaintButton (OCCUPYWINDOW, wlist->obuttonw, wlist->label, wlist->cp, off);
	    Scr->workSpaceMgr.occupyWindow.tmpOccupation &= ~(1 << wlist->number);
	}
    }
    w = Scr->workSpaceMgr.occupyWindow.w;
    XGetGeometry  (dpy, w, &junkW, &x, &y, &width, &height, &junkB, &junkD);
    XQueryPointer (dpy, Scr->Root, &junkW, &junkW, &junkX, &junkY, &x, &y, &junkK);
    x -= (width  / 2);
    y -= (height / 2);
    if (x < 0) x = 0;
    if (y < 0) y = 0;
    xoffset = width  + 2 * Scr->BorderWidth;
    yoffset = height + 2 * Scr->BorderWidth + Scr->TitleHeight;
    if ((x + xoffset) > Scr->MyDisplayWidth)  x = Scr->MyDisplayWidth  - xoffset;
    if ((y + yoffset) > Scr->MyDisplayHeight) y = Scr->MyDisplayHeight - yoffset;

    Scr->workSpaceMgr.occupyWindow.twm_win->occupation = twm_win->occupation;
    XMoveWindow  (dpy, Scr->workSpaceMgr.occupyWindow.twm_win->frame, x, y);
    SetMapStateProp (Scr->workSpaceMgr.occupyWindow.twm_win, NormalState);
    XMapWindow      (dpy, Scr->workSpaceMgr.occupyWindow.w);
    XMapRaised      (dpy, Scr->workSpaceMgr.occupyWindow.twm_win->frame);
    Scr->workSpaceMgr.occupyWindow.twm_win->mapped = TRUE;
    Scr->workSpaceMgr.occupyWindow.x = x;
    Scr->workSpaceMgr.occupyWindow.y = y;
    occupyWin = twm_win;
}

void OccupyHandleButtonEvent (event)
XEvent *event;
{
    TwmWindow		*twmWin;
    WorkSpaceList	*wlist;
    OccupyWindow	*occupyW;
    Window		buttonW;

    if (! Scr->workSpaceManagerActive) return;
    if (occupyWin == (TwmWindow*) 0) return;

    buttonW = event->xbutton.window;
    if (buttonW == 0) return; /* icon */

    XGrabPointer (dpy, Scr->Root, True,
		  ButtonPressMask | ButtonReleaseMask,
		  GrabModeAsync, GrabModeAsync,
		  Scr->Root, None, CurrentTime);

    occupyW = &Scr->workSpaceMgr.occupyWindow;
    for (wlist = Scr->workSpaceMgr.workSpaceList; wlist != NULL; wlist = wlist->next) {
	if (wlist->obuttonw == buttonW) break;
    }
    if (wlist != NULL) {
	if ((occupyW->tmpOccupation & (1 << wlist->number)) == 0) {
	    PaintButton (OCCUPYWINDOW, wlist->obuttonw, wlist->label, wlist->cp, on);
	    occupyW->tmpOccupation |= (1 << wlist->number);
	}
	else {
	    PaintButton (OCCUPYWINDOW, wlist->obuttonw, wlist->label, wlist->cp, off);
	    occupyW->tmpOccupation &= ~(1 << wlist->number);
	}
    }
    else
    if (buttonW == occupyW->OK) {
	if (occupyW->tmpOccupation == 0) return;
	ChangeOccupation (occupyWin, occupyW->tmpOccupation);
	Vanish (occupyW->twm_win);
	occupyW->twm_win->mapped = FALSE;
	occupyW->twm_win->occupation = 0;
	occupyWin = (TwmWindow*) 0;
	XSync (dpy, 0);
    }
    else
    if (buttonW == occupyW->cancel) {
	Vanish (occupyW->twm_win);
	occupyW->twm_win->mapped = FALSE;
	occupyW->twm_win->occupation = 0;
	occupyWin = (TwmWindow*) 0;
	XSync (dpy, 0);
    }
    else
    if (buttonW == occupyW->allworkspc) {
	for (wlist = Scr->workSpaceMgr.workSpaceList; wlist != NULL; wlist = wlist->next) {
	    PaintButton (OCCUPYWINDOW, wlist->obuttonw, wlist->label, wlist->cp, on);
	}
	occupyW->tmpOccupation = fullOccupation;
    }
    if (ButtonPressed == -1) XUngrabPointer (dpy, CurrentTime);
}

void OccupyAll (twm_win)
TwmWindow *twm_win;
{
    IconMgr *save;

    if (! Scr->workSpaceManagerActive) return;
    if (twm_win->iconmgr)   return;
    if ((! Scr->TransientHasOccupation) && twm_win->transient) return;
    save = Scr->iconmgr;
    Scr->iconmgr = Scr->workSpaceMgr.workSpaceList->iconmgr;
    ChangeOccupation (twm_win, fullOccupation);
    Scr->iconmgr = save;
}

void AllocateOthersIconManagers () {
    IconMgr		*p, *ip, *oldp, *oldv;
    WorkSpaceList	*wlist;

    if (! Scr->workSpaceManagerActive) return;

    oldp = Scr->iconmgr;
    for (wlist = Scr->workSpaceMgr.workSpaceList->next; wlist != NULL; wlist = wlist->next) {
	wlist->iconmgr  = (IconMgr *) malloc (sizeof (IconMgr));
	*wlist->iconmgr = *oldp;
	oldv = wlist->iconmgr;
	oldp->nextv = wlist->iconmgr;
	oldv->nextv = NULL;

	for (ip = oldp->next; ip != NULL; ip = ip->next) {
	    p  = (IconMgr *) malloc (sizeof (IconMgr));
	    *p = *ip;
	    ip->nextv  = p;
	    p->next    = NULL;
	    p->prev    = oldv;
	    p->nextv   = NULL;
	    oldv->next = p;
	    oldv = p;
        }
	for (ip = wlist->iconmgr; ip != NULL; ip = ip->next) {
	    ip->lasti = p;
        }
	oldp = wlist->iconmgr;
    }
    Scr->workSpaceMgr.workSpaceList->iconmgr = Scr->iconmgr;
}

static void Vanish (tmp_win)
TwmWindow *tmp_win;
{
    TwmWindow		*t;
    XWindowAttributes	winattrs;
    unsigned long	eventMask;

    XGetWindowAttributes(dpy, tmp_win->w, &winattrs);
    eventMask = winattrs.your_event_mask;

    /*
     * Prevent the receipt of an UnmapNotify, since that would
     * cause a transition to the Withdrawn state.
     */
    XSelectInput    (dpy, tmp_win->w, eventMask & ~StructureNotifyMask);
    XUnmapWindow    (dpy, tmp_win->w);
    XUnmapWindow    (dpy, tmp_win->frame);
    XSelectInput    (dpy, tmp_win->w, eventMask);
    SetMapStateProp (tmp_win, InactiveState);

    if (tmp_win->icon_on && tmp_win->icon && tmp_win->icon->w) {
	XUnmapWindow (dpy, tmp_win->icon->w);
	IconDown (tmp_win);
    }
    for (t = Scr->TwmRoot.next; t != NULL; t = t->next) {
	if ((t->transient && t->transientfor == tmp_win->w) ||
	    ((tmp_win->group == tmp_win->w) && (tmp_win->group == t->group) &&
	     (tmp_win->group != t->w) && t->isicon)) {
	    if (t->icon_on && t->icon && t->icon->w) {
		XUnmapWindow (dpy, t->icon->w);
		IconDown (t);
	    }
	}
    }
}

static void DisplayWin (tmp_win)
TwmWindow *tmp_win;
{
    TwmWindow		*t;
    XWindowAttributes	winattrs;
    unsigned long	eventMask;

    if (tmp_win->icon_on) {
	if (tmp_win->icon && tmp_win->icon->w) {
	    IconUp (tmp_win);
	    XMapWindow (dpy, tmp_win->icon->w);
	    return;
	}
    }
    XMapWindow      (dpy, tmp_win->w);
    XMapWindow      (dpy, tmp_win->frame);
    SetMapStateProp (tmp_win, NormalState);
}

void ChangeOccupation (tmp_win, newoccupation)
TwmWindow *tmp_win;
int newoccupation;
{
    TwmWindow	  *t;
    WorkSpaceList *wlist;
    int		  oldoccupation;
    char	  namelist [512];
    int		  len;
    XWindowAttributes    winattrs;
    unsigned long	 eventMask;

    if ((newoccupation == 0) || /* in case the property has been broken by another client */
	(newoccupation == tmp_win->occupation)) {
	len = GetPropertyFromMask (tmp_win->occupation, namelist);
	XGetWindowAttributes(dpy, tmp_win->w, &winattrs);
	eventMask = winattrs.your_event_mask;
	XSelectInput(dpy, tmp_win->w, eventMask & ~PropertyChangeMask);

	XChangeProperty (dpy, tmp_win->w, _XA_WM_OCCUPATION, XA_STRING, 8, 
		     PropModeReplace, (unsigned char *) namelist, len);
	XSelectInput(dpy, tmp_win->w, eventMask);
	return;
    }
    oldoccupation = tmp_win->occupation;
    tmp_win->occupation = newoccupation & ~oldoccupation;
    AddIconManager (tmp_win);
    tmp_win->occupation = newoccupation;
    RemoveIconManager (tmp_win);
    if ((oldoccupation & (1 << Scr->workSpaceMgr.activeWSPC->number)) &&
        (! OCCUPY (tmp_win, Scr->workSpaceMgr.activeWSPC))) Vanish (tmp_win);
    if (! (oldoccupation & (1 << Scr->workSpaceMgr.activeWSPC->number)) &&
        OCCUPY (tmp_win, Scr->workSpaceMgr.activeWSPC)) DisplayWin (tmp_win);

    len = GetPropertyFromMask (newoccupation, namelist);

    XGetWindowAttributes(dpy, tmp_win->w, &winattrs);
    eventMask = winattrs.your_event_mask;
    XSelectInput(dpy, tmp_win->w, eventMask & ~PropertyChangeMask);

    XChangeProperty (dpy, tmp_win->w, _XA_WM_OCCUPATION, XA_STRING, 8, 
		     PropModeReplace, (unsigned char *) namelist, len);
    XSelectInput(dpy, tmp_win->w, eventMask);

    if ((Scr->workSpaceMgr.workspaceWindow.noshowoccupyall) &&
		(newoccupation == fullOccupation)) {
	for (wlist = Scr->workSpaceMgr.workSpaceList; wlist != NULL; wlist = wlist->next) {
	    if ((oldoccupation & (1 << wlist->number)) != 0) {
		WMapRemoveFromList (tmp_win, wlist);
	    }
	}
	return;
    }

    if ((Scr->workSpaceMgr.workspaceWindow.noshowoccupyall) &&
		(oldoccupation == fullOccupation)) {
	for (wlist = Scr->workSpaceMgr.workSpaceList; wlist != NULL; wlist = wlist->next) {
	    if ((newoccupation & (1 << wlist->number)) != 0) {
		WMapAddToList (tmp_win, wlist);
	    }
	}
	return;
    }

    for (wlist = Scr->workSpaceMgr.workSpaceList; wlist != NULL; wlist = wlist->next) {
	if (((oldoccupation & (1 << wlist->number)) != 0) &&
	    ((newoccupation & (1 << wlist->number)) == 0)) {
	    WMapRemoveFromList (tmp_win, wlist);
	}
	else
	if (((newoccupation & (1 << wlist->number)) != 0) &&
	    ((oldoccupation & (1 << wlist->number)) == 0)) {
	    WMapAddToList (tmp_win, wlist);
	}
    }
    if (! Scr->TransientHasOccupation) {
	for (t = Scr->TwmRoot.next; t != NULL; t = t->next) {
	    if ((t->transient && t->transientfor == tmp_win->w) ||
		((tmp_win->group == tmp_win->w) && (tmp_win->group == t->group) &&
		(tmp_win->group != t->w))) {
		ChangeOccupation (t, newoccupation);
	    }
	}
    }
}

void WmgrRedoOccupation (win)
TwmWindow *win;
{
    WorkSpaceList	*wlist;
    int			newoccupation;

    if (LookInList (Scr->OccupyAll, win->full_name, &win->class)) {
	newoccupation = fullOccupation;
    }
    else {
	newoccupation = 0;
	for (wlist = Scr->workSpaceMgr.workSpaceList; wlist != NULL; wlist = wlist->next) {
	    if (LookInList (wlist->clientlist, win->full_name, &win->class)) {
		newoccupation |= 1 << wlist->number;
	    }
	}
    }
    if (newoccupation != 0) ChangeOccupation (win, newoccupation);
}

void WMgrRemoveFromCurrentWosksace (win)
TwmWindow *win;
{
    WorkSpaceList *wlist;
    int		  newoccupation;

    wlist = Scr->workSpaceMgr.activeWSPC;
    if (! OCCUPY (win, wlist)) return;

    newoccupation = win->occupation & ~(1 << wlist->number);
    if (newoccupation == 0) return;

    ChangeOccupation (win, newoccupation);
}

void WMgrAddToCurrentWosksaceAndWarp (winname)
char *winname;
{
    TwmWindow *tw;
    int       newoccupation;

    for (tw = Scr->TwmRoot.next; tw != NULL; tw = tw->next) {
	if (match (winname, tw->full_name)) break;
    }
    if (!tw) {
	for (tw = Scr->TwmRoot.next; tw != NULL; tw = tw->next) {
	    if (match (winname, tw->class.res_name)) break;
	}
	if (!tw) {
	    for (tw = Scr->TwmRoot.next; tw != NULL; tw = tw->next) {
		if (match (winname, tw->class.res_class)) break;
	    }
	}
    }
    if (!tw) {
	XBell (dpy, 0);
	return;
    }
    if ((! Scr->WarpUnmapped) && (! tw->mapped)) {
	XBell (dpy, 0);
	return;
    }
    if (! OCCUPY (tw, Scr->workSpaceMgr.activeWSPC)) {
	newoccupation = tw->occupation | (1 << Scr->workSpaceMgr.activeWSPC->number);
	ChangeOccupation (tw, newoccupation);
    }

    if (! tw->mapped) DeIconify (tw);
    if (! Scr->NoRaiseWarp) XRaiseWindow (dpy, tw->frame);
    WarpToWindow (tw);
}

static void CreateWorkSpaceManagerWindow ()
{
    int		  mask;
    int		  lines, vspace, hspace, count, columns;
    int		  width, height, bwidth, bheight, wwidth, wheight;
    char	  *name, *icon_name, *geometry;
    int		  i, j;
    Window	  w;
    ColorPair	  cp;
    MyFont	  font;
    WorkSpaceList *wlist;
    Window	  junkW;
    int		  x, y, strWid, wid;
    unsigned long border;
    TwmWindow	  *tmp_win;
    unsigned long valuemask;
    XSetWindowAttributes	attr;
    XWindowAttributes		wattr;
    unsigned long		attrmask;
    XSizeHints	  sizehints;
    XWMHints	  wmhints;
    int		  gravity;

    Scr->workSpaceMgr.workspaceWindow.buttonFont = Scr->IconManagerFont;
    Scr->workSpaceMgr.workspaceWindow.cp	 = Scr->IconManagerC;
#ifdef COLOR_BLIND_USER
    Scr->workSpaceMgr.workspaceWindow.cp.shadc   = Scr->White;
    Scr->workSpaceMgr.workspaceWindow.cp.shadd   = Scr->Black;
#else
    if (!Scr->BeNiceToColormap) GetShadeColors (&Scr->workSpaceMgr.workspaceWindow.cp);
#endif

    name      = Scr->workSpaceMgr.workspaceWindow.name;
    icon_name = Scr->workSpaceMgr.workspaceWindow.icon_name;
    geometry  = Scr->workSpaceMgr.workspaceWindow.geometry;
    columns   = Scr->workSpaceMgr.workspaceWindow.columns;
    vspace    = Scr->workSpaceMgr.workspaceWindow.vspace;
    hspace    = Scr->workSpaceMgr.workspaceWindow.hspace;
    font      = Scr->workSpaceMgr.workspaceWindow.buttonFont;
    cp        = Scr->workSpaceMgr.workspaceWindow.cp;
    border    = Scr->workSpaceMgr.workspaceWindow.defBorderColor;

    count = 0;
    for (wlist = Scr->workSpaceMgr.workSpaceList; wlist != NULL; wlist = wlist->next) count++;
    Scr->workSpaceMgr.count = count;

    if (columns == 0) {
	lines   = 2;
	columns = ((count - 1) / lines) + 1;
    }
    else {
	lines   = ((count - 1) / columns) + 1;
    }
    Scr->workSpaceMgr.workspaceWindow.lines     = lines;
    Scr->workSpaceMgr.workspaceWindow.columns   = columns;

    strWid = 0;
    for (wlist = Scr->workSpaceMgr.workSpaceList; wlist != NULL; wlist = wlist->next) {
	wid = XTextWidth (font.font, wlist->label, strlen (wlist->label));
	if (wid > strWid) strWid = wid;
    }
    if (geometry != NULL) {
	mask = XParseGeometry (geometry, &x, &y, (unsigned int *) &width, (unsigned int *) &height);
	bwidth  = (mask & WidthValue)  ? ((width - ((columns + 1) * hspace)) / columns) : strWid + 10;
	bheight = (mask & HeightValue) ? ((height - ((lines  + 1) * vspace)) / lines) : 22;
	width   = (columns * bwidth) + ((columns + 1) * hspace);
	height  = (lines *  bheight) + ((lines   + 1) * vspace);

	if (! (mask & YValue)) {
	    y = 0;
	    mask |= YNegative;
	}
	if (mask & XValue) {
	    if (mask & XNegative) {
		x += Scr->MyDisplayWidth  - width;
		gravity = (mask & YNegative) ? SouthEastGravity : NorthEastGravity;
	    }
	    else {
		gravity = (mask & YNegative) ? SouthWestGravity : NorthWestGravity;
	    }
	}
	else {
	    x  = (Scr->MyDisplayWidth - width) / 2;
	    gravity = (mask & YValue) ? ((mask & YNegative) ? SouthGravity : NorthGravity) : SouthGravity;
	}
	if (mask & YNegative) y += Scr->MyDisplayHeight - height;
    }
    else {
	bwidth  = strWid + 10;
	bheight = 22;
	width   = (columns * bwidth) + ((columns + 1) * hspace);
	height  = (lines * bheight) + ((lines + 1) * vspace);
	x       = (Scr->MyDisplayWidth - width) / 2;
	y       = Scr->MyDisplayHeight - height;
	gravity = NorthWestGravity;
    }
    wwidth  = width  / columns;
    wheight = height / lines;

    Scr->workSpaceMgr.workspaceWindow.width     = width;
    Scr->workSpaceMgr.workspaceWindow.height    = height;
    Scr->workSpaceMgr.workspaceWindow.bwidth    = bwidth;
    Scr->workSpaceMgr.workspaceWindow.bheight   = bheight;
    Scr->workSpaceMgr.workspaceWindow.wwidth    = wwidth;
    Scr->workSpaceMgr.workspaceWindow.wheight   = wheight;

    Scr->workSpaceMgr.workspaceWindow.w		= XCreateSimpleWindow (dpy, Scr->Root,
						x, y, width, height, 0, Scr->Black, cp.back);
    i = 0;
    j = 0;
    for (wlist = Scr->workSpaceMgr.workSpaceList; wlist != NULL; wlist = wlist->next) {
	wlist->buttonw = XCreateSimpleWindow (dpy, Scr->workSpaceMgr.workspaceWindow.w,
			i * (bwidth  + hspace) + hspace, j * (bheight + vspace) + vspace,
			bwidth, bheight, 0,
			Scr->Black, wlist->cp.back);

	wlist->mapSubwindow.x = i * wwidth;
	wlist->mapSubwindow.y = j * wheight;
	wlist->mapSubwindow.w = XCreateSimpleWindow (dpy, Scr->workSpaceMgr.workspaceWindow.w,
			wlist->mapSubwindow.x, wlist->mapSubwindow.y,
			wwidth - 2, wheight - 2, 1, border, wlist->cp.back);

	if (Scr->workSpaceMgr.workspaceWindow.state == BUTTONSSTATE)
	    XMapWindow (dpy, wlist->buttonw);
	else
	    XMapWindow (dpy, wlist->mapSubwindow.w);

	wlist->mapSubwindow.wl = NULL;
	if (useBackgroundInfo) {
	    if (wlist->image == None)
		XSetWindowBackground (dpy, wlist->mapSubwindow.w, wlist->backcp.back);
	    else
		XSetWindowBackgroundPixmap (dpy, wlist->mapSubwindow.w, wlist->image->pixmap);
	}
	else {
	    if (Scr->workSpaceMgr.workspaceWindow.defImage == None)
		XSetWindowBackground (dpy, wlist->mapSubwindow.w,
			Scr->workSpaceMgr.workspaceWindow.defColors.back);
	    else
		XSetWindowBackgroundPixmap (dpy, wlist->mapSubwindow.w,
			Scr->workSpaceMgr.workspaceWindow.defImage->pixmap);
	}
	XClearWindow (dpy, wlist->mapSubwindow.w);
	i++;
	if (i == columns) {i = 0; j++;};
    }

    sizehints.flags       = USPosition | PBaseSize | PMinSize | PResizeInc | PWinGravity;
    sizehints.x           = x;
    sizehints.y           = y;
    sizehints.base_width  = (columns + 1) * hspace;
    sizehints.base_height = (lines   + 1) * vspace;
    sizehints.width_inc   = columns;
    sizehints.height_inc  = lines;
    sizehints.min_width   = (columns + 1) * hspace + 2 * columns;
    sizehints.min_height  = (lines   + 1) * vspace + 2 * lines;
    sizehints.win_gravity = gravity;

    XSetStandardProperties (dpy, Scr->workSpaceMgr.workspaceWindow.w,
			name, icon_name, None, NULL, 0, NULL);
    XSetWMSizeHints (dpy, Scr->workSpaceMgr.workspaceWindow.w, &sizehints, XA_WM_NORMAL_HINTS);

    wmhints.initial_state = NormalState;
    wmhints.input         = True;
    XSetWMHints (dpy, Scr->workSpaceMgr.workspaceWindow.w, &wmhints);
    tmp_win = AddWindow (Scr->workSpaceMgr.workspaceWindow.w, FALSE, Scr->iconmgr);
    if (! tmp_win) {
	fprintf (stderr, "cannot create workspace manager window, exiting...\n");
	exit (1);
    }
    tmp_win->occupation = fullOccupation;

    attrmask = 0;
/*
    if (Scr->BackingStore) {
	attr.backing_store = WhenMapped;
	attrmask |= CWBackingStore;
    }
*/
    attr.cursor = Scr->ButtonCursor;
    attrmask |= CWCursor;
    attr.win_gravity = gravity;
    attrmask |= CWWinGravity;
    XChangeWindowAttributes (dpy, Scr->workSpaceMgr.workspaceWindow.w, attrmask, &attr);

    XGetWindowAttributes (dpy, Scr->workSpaceMgr.workspaceWindow.w, &wattr);
    attrmask = wattr.your_event_mask | KeyPressMask | KeyReleaseMask | ExposureMask;
    XSelectInput (dpy, Scr->workSpaceMgr.workspaceWindow.w, attrmask);

    for (wlist = Scr->workSpaceMgr.workSpaceList; wlist != NULL; wlist = wlist->next) {
	if (Scr->BackingStore) {
	    attr.backing_store = WhenMapped;
	    XChangeWindowAttributes (dpy, wlist->buttonw, CWBackingStore, &attr);
	}
	XSelectInput (dpy, wlist->buttonw, ButtonPressMask | ButtonReleaseMask | ExposureMask);
	XSaveContext (dpy, wlist->buttonw, TwmContext,    (caddr_t) tmp_win);
	XSaveContext (dpy, wlist->buttonw, ScreenContext, (caddr_t) Scr);

	XSelectInput (dpy, wlist->mapSubwindow.w, ButtonPressMask | ButtonReleaseMask);
	XSaveContext (dpy, wlist->mapSubwindow.w, TwmContext,    (caddr_t) tmp_win);
	XSaveContext (dpy, wlist->mapSubwindow.w, ScreenContext, (caddr_t) Scr);
    }
    SetMapStateProp (tmp_win, WithdrawnState);
    Scr->workSpaceMgr.workspaceWindow.twm_win = tmp_win;

    wlist = Scr->workSpaceMgr.workSpaceList;
    if (useBackgroundInfo && ! Scr->DontPaintRootWindow) {
	if (wlist->image == None)
	    XSetWindowBackground (dpy, Scr->Root, wlist->backcp.back);
	else
	    XSetWindowBackgroundPixmap (dpy, Scr->Root, wlist->image->pixmap);
	XClearWindow (dpy, Scr->Root);
    }
    PaintWorkSpaceManager ();
}

void WMgrHandleExposeEvent (event)
XEvent *event;
{
    WorkSpaceList *wlist;

    if (Scr->workSpaceMgr.workspaceWindow.state == BUTTONSSTATE) {
	for (wlist = Scr->workSpaceMgr.workSpaceList; wlist != NULL; wlist = wlist->next) {
	    if (event->xexpose.window == wlist->buttonw) break;
	}
	if (wlist == NULL) {
	    PaintWorkSpaceManagerBorder ();
	}
	else
	if (wlist == Scr->workSpaceMgr.activeWSPC)
	    PaintButton (WSPCWINDOW, wlist->buttonw, wlist->label, wlist->cp, on);
	else
	    PaintButton (WSPCWINDOW, wlist->buttonw, wlist->label, wlist->cp, off);
    }
    else {
	WinList	  wl;

        if (XFindContext (dpy, event->xexpose.window, MapWListContext, (caddr_t *) &wl) == XCNOENT) return;
	if (wl->twm_win->mapped) WMapRedrawName (wl);
    }
}

void PaintWorkSpaceManager () {
    Window        w;
    int           bwidth, bheight, width, height;
    WorkSpaceList *wlist;

    PaintWorkSpaceManagerBorder ();
    for (wlist = Scr->workSpaceMgr.workSpaceList; wlist != NULL; wlist = wlist->next) {
	if (wlist == Scr->workSpaceMgr.activeWSPC)
	    PaintButton (WSPCWINDOW, wlist->buttonw, wlist->label, wlist->cp, on);
	else
	    PaintButton (WSPCWINDOW, wlist->buttonw, wlist->label, wlist->cp, off);
    }
}

static void PaintWorkSpaceManagerBorder () {
    int           width, height;

    width   = Scr->workSpaceMgr.workspaceWindow.width;
    height  = Scr->workSpaceMgr.workspaceWindow.height;

    Draw3DBorder (Scr->workSpaceMgr.workspaceWindow.w, 0, 0, width, height, 2,
			Scr->workSpaceMgr.workspaceWindow.cp, off, True, False);
}

ColorPair occupyButtoncp;

char *ok_string		= "OK",
     *cancel_string	= "Cancel",
     *everywhere_string	= "All";

static void CreateOccupyWindow () {
    int		  width, height, lines, columns, x, y;
    int		  bwidth, bheight, owidth, oheight, hspace, vspace;
    int		  i, j;
    Window	  w, OK, cancel, allworkspc;
    char	  *name, *icon_name;
    ColorPair	  cp;
    TwmWindow	  *tmp_win;
    WorkSpaceList *wlist;
    XSizeHints	  sizehints;
    XWMHints      wmhints;
    XSetWindowAttributes	attr;
    XWindowAttributes		wattr;
    unsigned long attrmask;

    Scr->workSpaceMgr.occupyWindow.font     = Scr->IconManagerFont;
    Scr->workSpaceMgr.occupyWindow.cp       = Scr->IconManagerC;
#ifdef COLOR_BLIND_USER
    Scr->workSpaceMgr.occupyWindow.cp.shadc = Scr->White;
    Scr->workSpaceMgr.occupyWindow.cp.shadd = Scr->Black;
#else
    if (!Scr->BeNiceToColormap) GetShadeColors (&Scr->workSpaceMgr.occupyWindow.cp);
#endif
    name      = Scr->workSpaceMgr.occupyWindow.name;
    icon_name = Scr->workSpaceMgr.occupyWindow.icon_name;
    lines     = Scr->workSpaceMgr.workspaceWindow.lines;
    columns   = Scr->workSpaceMgr.workspaceWindow.columns;
    bwidth    = Scr->workSpaceMgr.workspaceWindow.bwidth;
    bheight   = Scr->workSpaceMgr.workspaceWindow.bheight;
    oheight   = Scr->workSpaceMgr.workspaceWindow.bheight;
    vspace    = Scr->workSpaceMgr.occupyWindow.vspace;
    hspace    = Scr->workSpaceMgr.occupyWindow.hspace;
    cp        = Scr->workSpaceMgr.occupyWindow.cp;

    height = ((bheight + vspace) * lines) + oheight + (3 * vspace);
    if (columns < 3) {
	MyFont font;
	int min_bwidth, min_width;

	font       = Scr->workSpaceMgr.occupyWindow.font;
        min_bwidth = XTextWidth (font.font, ok_string, strlen (ok_string));
	i = XTextWidth (font.font, cancel_string, strlen (cancel_string));
	if (i > min_bwidth) min_bwidth = i;
	i = XTextWidth (font.font, everywhere_string, strlen (everywhere_string));
	if (i > min_bwidth) min_bwidth = i;
	min_bwidth = (min_bwidth + 2 * hspace); /* normal width calculation */
	width = (bwidth * columns) + (hspace * (columns + 1)); 
	min_width = (3 * min_bwidth) + (4 * hspace); /* width by text width */
	if (min_width > width) width = min_width;

	owidth = bwidth;
	bwidth = (width - (columns + 1) * hspace) / columns;
    }
    else {
	width  = (bwidth * columns) + (hspace * (columns + 1));
	owidth = (width - 4 * hspace) / 3;
    }
    Scr->workSpaceMgr.occupyWindow.lines      = lines;
    Scr->workSpaceMgr.occupyWindow.columns    = columns;
    Scr->workSpaceMgr.occupyWindow.width      = width;
    Scr->workSpaceMgr.occupyWindow.height     = height;
    Scr->workSpaceMgr.occupyWindow.bwidth     = bwidth;
    Scr->workSpaceMgr.occupyWindow.bheight    = bheight;
    Scr->workSpaceMgr.occupyWindow.owidth     = owidth;
    Scr->workSpaceMgr.occupyWindow.oheight    = oheight;

    w = XCreateSimpleWindow (dpy, Scr->Root, 0, 0, width, height, 1, Scr->Black, cp.back);
    Scr->workSpaceMgr.occupyWindow.w = w;
    i = 0;
    j = 0;
    for (wlist = Scr->workSpaceMgr.workSpaceList; wlist != NULL; wlist = wlist->next) {
	wlist->obuttonw = XCreateSimpleWindow(dpy, w,
		       i * (bwidth  + hspace) + hspace,
		       j * (bheight + vspace) + vspace, bwidth, bheight, 0,
		       Scr->Black, wlist->cp.back);
	XMapWindow (dpy, wlist->obuttonw);
	i++;
	if (i == columns) {i = 0; j++;}
    }
    GetColor (Scr->Monochrome, &(occupyButtoncp.back), "gray50");
    occupyButtoncp.fore = Scr->White;
    if (!Scr->BeNiceToColormap) GetShadeColors (&occupyButtoncp);

    x = vspace;
    y = ((bheight + vspace) * lines) + ( 2 * vspace);
    OK         = XCreateSimpleWindow(dpy, w, x, y, owidth, oheight, 0, Scr->Black, occupyButtoncp.back);
    XMapWindow (dpy, OK);
    x += owidth + vspace;
    cancel     = XCreateSimpleWindow(dpy, w, x, y, owidth, oheight, 0, Scr->Black, occupyButtoncp.back);
    XMapWindow (dpy, cancel);
    x += owidth + vspace;
    allworkspc = XCreateSimpleWindow(dpy, w, x, y, owidth, oheight, 0, Scr->Black, occupyButtoncp.back);
    XMapWindow (dpy, allworkspc);

    Scr->workSpaceMgr.occupyWindow.OK         = OK;
    Scr->workSpaceMgr.occupyWindow.cancel     = cancel;
    Scr->workSpaceMgr.occupyWindow.allworkspc = allworkspc;

    sizehints.flags       = PBaseSize | PMinSize | PResizeInc;
    sizehints.base_width  = columns;
    sizehints.base_height = lines;
    sizehints.width_inc   = columns;
    sizehints.height_inc  = lines;
    sizehints.min_width   = 2 * columns;
    sizehints.min_height  = 2 * lines;
    XSetStandardProperties (dpy, w, name, icon_name, None, NULL, 0, &sizehints);

    wmhints.initial_state = NormalState;
    wmhints.input         = True;
    XSetWMHints (dpy, Scr->workSpaceMgr.workspaceWindow.w, &wmhints);
    tmp_win = AddWindow (w, FALSE, Scr->iconmgr);
    if (! tmp_win) {
	fprintf (stderr, "cannot create occupy window, exiting...\n");
	exit (1);
    }

    XGetWindowAttributes (dpy, Scr->workSpaceMgr.workspaceWindow.w, &wattr);
    attrmask = wattr.your_event_mask | KeyPressMask | KeyReleaseMask | ExposureMask;
    XSelectInput (dpy, Scr->workSpaceMgr.workspaceWindow.w, attrmask);

    for (wlist = Scr->workSpaceMgr.workSpaceList; wlist != NULL; wlist = wlist->next) {
	XSelectInput (dpy, wlist->obuttonw, ButtonPressMask | ButtonReleaseMask);
	XSaveContext (dpy, wlist->obuttonw, TwmContext,    (caddr_t) tmp_win);
	XSaveContext (dpy, wlist->obuttonw, ScreenContext, (caddr_t) Scr);
    }
    XSelectInput (dpy, Scr->workSpaceMgr.occupyWindow.OK, ButtonPressMask | ButtonReleaseMask);
    XSaveContext (dpy, Scr->workSpaceMgr.occupyWindow.OK, TwmContext,    (caddr_t) tmp_win);
    XSaveContext (dpy, Scr->workSpaceMgr.occupyWindow.OK, ScreenContext, (caddr_t) Scr);
    XSelectInput (dpy, Scr->workSpaceMgr.occupyWindow.cancel, ButtonPressMask | ButtonReleaseMask);
    XSaveContext (dpy, Scr->workSpaceMgr.occupyWindow.cancel, TwmContext,    (caddr_t) tmp_win);
    XSaveContext (dpy, Scr->workSpaceMgr.occupyWindow.cancel, ScreenContext, (caddr_t) Scr);
    XSelectInput (dpy, Scr->workSpaceMgr.occupyWindow.allworkspc, ButtonPressMask | ButtonReleaseMask);
    XSaveContext (dpy, Scr->workSpaceMgr.occupyWindow.allworkspc, TwmContext,    (caddr_t) tmp_win);
    XSaveContext (dpy, Scr->workSpaceMgr.occupyWindow.allworkspc, ScreenContext, (caddr_t) Scr);

    XSelectInput (dpy, w, ExposureMask);
    SetMapStateProp (tmp_win, WithdrawnState);

    attrmask = 0;
/* for some reason, it doesn't work
    if (Scr->BackingStore) {
	attr.backing_store = WhenMapped;
	attrmask |= CWBackingStore;
    }
*/
    attr.cursor = Scr->ButtonCursor;
    attrmask |= CWCursor;
    XChangeWindowAttributes (dpy, w, attrmask, &attr);
    tmp_win->occupation = 0;
    Scr->workSpaceMgr.occupyWindow.twm_win = tmp_win;
}

void PaintOccupyWindow () {
    WorkSpaceList *wlist;
    OccupyWindow  *occupyW;
    int 	  width, height;

    width   = Scr->workSpaceMgr.occupyWindow.width;
    height  = Scr->workSpaceMgr.occupyWindow.height;

    Draw3DBorder (Scr->workSpaceMgr.occupyWindow.w, 0, 0, width, height, 2,
			Scr->workSpaceMgr.occupyWindow.cp, off, True, False);

    occupyW = &Scr->workSpaceMgr.occupyWindow;
    for (wlist = Scr->workSpaceMgr.workSpaceList; wlist != NULL; wlist = wlist->next) {
	if (occupyW->tmpOccupation & (1 << wlist->number))
	    PaintButton (OCCUPYWINDOW, wlist->obuttonw, wlist->label, wlist->cp, on);
	else
	    PaintButton (OCCUPYWINDOW, wlist->obuttonw, wlist->label, wlist->cp, off);
    }
    PaintButton (OCCUPYBUTTON, Scr->workSpaceMgr.occupyWindow.OK,
		ok_string, occupyButtoncp, off);
    PaintButton (OCCUPYBUTTON, Scr->workSpaceMgr.occupyWindow.cancel,
		cancel_string, occupyButtoncp, off);
    PaintButton (OCCUPYBUTTON, Scr->workSpaceMgr.occupyWindow.allworkspc,
		everywhere_string, occupyButtoncp, off);
}

static void PaintButton (which, w, label, cp, state)
int       which;
Window    w;
char      *label;
ColorPair cp;
int       state;
{
    int        bwidth, bheight, width, height;
    MyFont     font;
    int        strWid, strHei, hspace, vspace;

    if (which == WSPCWINDOW) {
	bwidth  = Scr->workSpaceMgr.workspaceWindow.bwidth;
	bheight = Scr->workSpaceMgr.workspaceWindow.bheight;
	font    = Scr->workSpaceMgr.workspaceWindow.buttonFont;
    }
    else
    if (which == OCCUPYWINDOW) {
	bwidth  = Scr->workSpaceMgr.occupyWindow.bwidth;
	bheight = Scr->workSpaceMgr.occupyWindow.bheight;
	font    = Scr->workSpaceMgr.occupyWindow.font;
    }
    else
    if (which == OCCUPYBUTTON) {
	bwidth  = Scr->workSpaceMgr.occupyWindow.owidth;
	bheight = Scr->workSpaceMgr.occupyWindow.oheight;
	font    = Scr->workSpaceMgr.occupyWindow.font;
    }
    else return;

    strHei = font.font->max_bounds.ascent + font.font->max_bounds.descent;
    vspace = ((bheight + strHei) / 2) - font.font->max_bounds.descent;
    strWid = XTextWidth (font.font, label, strlen (label));
    hspace = (bwidth - strWid) / 2;
    if (hspace < 3) hspace = 3;
    XClearWindow (dpy, w);

    if (Scr->Monochrome == COLOR) {
	Draw3DBorder (w, 0, 0, bwidth, bheight, Scr->WMgrButtonShadowDepth,
			cp, state, True, False);
	FBF (cp.fore, cp.back, font.font->fid);
	XDrawString (dpy, w, Scr->NormalGC, hspace, vspace, label, strlen (label));
    }
    else {
	Draw3DBorder (w, 0, 0, bwidth, bheight, Scr->WMgrButtonShadowDepth,
		cp, state, True, False);
	if (state == on) {
	    FB  (cp.back, cp.fore);
	    XFillRectangle (dpy, w, Scr->NormalGC, 2, 2, bwidth - 4, bheight - 4);
	    FBF (cp.fore, cp.back, font.font->fid);
	    XDrawImageString (dpy, w, Scr->NormalGC, hspace, vspace, label, strlen (label));
	}
	else {
	    FBF (cp.back, cp.fore, font.font->fid);
	    XDrawImageString (dpy, w, Scr->NormalGC, hspace, vspace, label, strlen (label));
	}
    }
}

static unsigned int GetMaskFromResource (win, res)
TwmWindow *win;
char      *res;
{
    char          *name;
    char          wrkSpcName [64];
    WorkSpaceList *wlist;
    int           mask, num, mode;

    mode = 0;
    if (*res == '+') {
	mode = 1;
	res++;
    }
    else
    if (*res == '-') {
	mode = 2;
	res++;
    }
    mask = 0;
    while (*res != '\0') {
	while (*res == ' ') res++;
	if (*res == '\0') break;
	name = wrkSpcName;
	while ((*res != '\0') && (*res != ' ')) {
	    if (*res == '\\') res++;
	    *name = *res;
	    name++; res++;
	}
	*name = '\0';
	if (strcmp (wrkSpcName, "all") == 0) {
	    mask = fullOccupation;
	    break;
	}
	if (strcmp (wrkSpcName, "current") == 0) {
	    mask |= (1 << Scr->workSpaceMgr.activeWSPC->number);
	    continue;
	}
	num  = 0;
	for (wlist = Scr->workSpaceMgr.workSpaceList; wlist != NULL; wlist = wlist->next) {
	    if (strcmp (wrkSpcName, wlist->label) == 0) break;
	    num++;
	}
	if (wlist != NULL) mask |= (1 << num);
	else {
	    twmrc_error_prefix ();
	    fprintf (stderr, "unknown workspace : %s\n", wrkSpcName);
	}
    }
    switch (mode) {
	case 0 :
	    return (mask);
	case 1 :
	    return (mask | win->occupation);
	case 2 :
	    return (win->occupation & ~mask);
    }
}

unsigned int GetMaskFromProperty (prop, len)
char *prop;
int  len;
{
    char          wrkSpcName [256];
    WorkSpaceList *wlist;
    unsigned int  mask;
    int           num, l;

    mask = 0;
    l = 0;
    while (l < len) {
	strcpy (wrkSpcName, prop);
	l    += strlen (prop) + 1;
	prop += strlen (prop) + 1;
	if (strcmp (wrkSpcName, "all") == 0) {
	    mask = fullOccupation;
	    break;
	}
	num = 0;
	for (wlist = Scr->workSpaceMgr.workSpaceList; wlist != NULL; wlist = wlist->next) {
	    if (strcmp (wrkSpcName, wlist->label) == 0) break;
	    num++;
	}
	if (wlist == NULL) {
	    fprintf (stderr, "unknown workspace : %s\n", wrkSpcName);
	}
	else {
	    mask |= (1 << num);
	}
    }
    return (mask);
}

static int GetPropertyFromMask (mask, prop)
unsigned int mask;
char *prop;
{
    WorkSpaceList *wlist;
    int           len;
    char	  *p;

    if (mask == fullOccupation) {
	strcpy (prop, "all");
	return (3);
    }
    len = 0;
    p = prop;
    for (wlist = Scr->workSpaceMgr.workSpaceList; wlist != NULL; wlist = wlist->next) {
	if (mask & (1 << wlist->number)) {
	    strcpy (p, wlist->label);
	    p   += strlen (wlist->label) + 1;
	    len += strlen (wlist->label) + 1;
	}
    }
    return (len);
}

void AddToClientsList (workspace, client)
char *workspace, *client;
{
    WorkSpaceList *wlist;
    name_list     *nl;

    if (strcmp (workspace, "all") == 0) {
	for (wlist = Scr->workSpaceMgr.workSpaceList; wlist != NULL; wlist = wlist->next) {
	    AddToList (&wlist->clientlist, client, "");
	}
	return;
    }

    for (wlist = Scr->workSpaceMgr.workSpaceList; wlist != NULL; wlist = wlist->next) {
	if (strcmp (wlist->label, workspace) == 0) break;
    }
    if (wlist == NULL) return;
    AddToList (&wlist->clientlist, client, "");    
}

void WMapToggleState () {
    WorkSpaceList *wlist;

    if (Scr->workSpaceMgr.workspaceWindow.state == BUTTONSSTATE) {
	WMapSetMapState ();
    }
    else {
	WMapSetButtonsState ();
    }
}

void WMapSetMapState () {
    WorkSpaceList *wlist;

    if (Scr->workSpaceMgr.workspaceWindow.state == MAPSTATE) return;
    for (wlist = Scr->workSpaceMgr.workSpaceList; wlist != NULL; wlist = wlist->next) {
	XUnmapWindow (dpy, wlist->buttonw);
	XMapWindow   (dpy, wlist->mapSubwindow.w);
    }
    Scr->workSpaceMgr.workspaceWindow.state = MAPSTATE;
    MaybeAnimate = True;
}

void WMapSetButtonsState () {
    WorkSpaceList *wlist;

    if (Scr->workSpaceMgr.workspaceWindow.state == BUTTONSSTATE) return;
    for (wlist = Scr->workSpaceMgr.workSpaceList; wlist != NULL; wlist = wlist->next) {
	XUnmapWindow (dpy, wlist->mapSubwindow.w);
	XMapWindow   (dpy, wlist->buttonw);
    }
    Scr->workSpaceMgr.workspaceWindow.state = BUTTONSSTATE;
}

void WMapAddWindow (win)
TwmWindow *win;
{
    WorkSpaceList *wlist;

    if (win->iconmgr) return;
    if (strcmp (win->name, Scr->workSpaceMgr.workspaceWindow.name) == 0) return;
    if (strcmp (win->name, Scr->workSpaceMgr.occupyWindow.name) == 0) return;
    if ((Scr->workSpaceMgr.workspaceWindow.noshowoccupyall) &&
	(win->occupation == fullOccupation)) return;
    for (wlist = Scr->workSpaceMgr.workSpaceList; wlist != NULL; wlist = wlist->next) {
	if (OCCUPY (win, wlist)) WMapAddToList (win, wlist);
    }
}

void WMapDestroyWindow (win)
TwmWindow *win;
{
    WorkSpaceList *wlist;

    for (wlist = Scr->workSpaceMgr.workSpaceList; wlist != NULL; wlist = wlist->next) {
	if (OCCUPY (win, wlist)) WMapRemoveFromList (win, wlist);
    }
    if (win == occupyWin) {
	OccupyWindow *occupyW = &Scr->workSpaceMgr.occupyWindow;

	Vanish (occupyW->twm_win);
	occupyW->twm_win->mapped = FALSE;
	occupyW->twm_win->occupation = 0;
	occupyWin = (TwmWindow*) 0;
    }
}

void WMapMapWindow (win)
TwmWindow *win;
{
    WorkSpaceList *wlist;
    WinList wl;

    for (wlist = Scr->workSpaceMgr.workSpaceList; wlist != NULL; wlist = wlist->next) {
	for (wl = wlist->mapSubwindow.wl; wl != NULL; wl = wl->next) {
	    if (wl->twm_win == win) {
		XMapWindow (dpy, wl->w);
		WMapRedrawName (wl);
		break;
	    }
	}
    }
}

void WMapSetupWindow (win, x, y, w, h)
TwmWindow *win;
int x, y, w, h;
{
    WorkSpaceList *wlist;
    WinList	wl;
    TwmWindow	*tw;
    int		wwidth, wheight, i, j;
    float	wf, hf;

    if (win->iconmgr) return;
    if (strcmp (win->name, Scr->workSpaceMgr.workspaceWindow.name) == 0) {
	Scr->workSpaceMgr.workspaceWindow.x = x;
	Scr->workSpaceMgr.workspaceWindow.y = y;
	if (w == -1) return;
	ResizeWorkSpaceManager (w, h);
	return;
    }
    if (strcmp (win->name, Scr->workSpaceMgr.occupyWindow.name) == 0) {
	Scr->workSpaceMgr.occupyWindow.x = x;
	Scr->workSpaceMgr.occupyWindow.y = y;
	if (w == -1) return;
	ResizeOccupyWindow (w, h);
	return;
    }
    wf = (float) (Scr->workSpaceMgr.workspaceWindow.wwidth  - 2) / (float) Scr->MyDisplayWidth;
    hf = (float) (Scr->workSpaceMgr.workspaceWindow.wheight - 2) / (float) Scr->MyDisplayHeight;
    for (wlist = Scr->workSpaceMgr.workSpaceList; wlist != NULL; wlist = wlist->next) {
	for (wl = wlist->mapSubwindow.wl; wl != NULL; wl = wl->next) {
	    if (win == wl->twm_win) {
		wl->x = (int) (x * wf);
		wl->y = (int) (y * hf);
		if (w == -1) {
		    XMoveWindow (dpy, wl->w, wl->x, wl->y);
		}
		else {
		    wl->width  = (unsigned int) (w * wf);
		    wl->height = (unsigned int) (h * hf);
		    XMoveResizeWindow (dpy, wl->w, wl->x, wl->y, wl->width, wl->height);
		}
		break;
	    }
	}
    }
}

void WMapIconify (win)
TwmWindow *win;
{
    WorkSpaceList *wlist;
    WinList    wl;

    for (wlist = Scr->workSpaceMgr.workSpaceList; wlist != NULL; wlist = wlist->next) {
	for (wl = wlist->mapSubwindow.wl; wl != NULL; wl = wl->next) {
	    if (win == wl->twm_win) {
		XUnmapWindow (dpy, wl->w);
		break;
	    }
	}
    }
}

void WMapDeIconify (win)
TwmWindow *win;
{
    WorkSpaceList *wlist;
    WinList    wl;

    for (wlist = Scr->workSpaceMgr.workSpaceList; wlist != NULL; wlist = wlist->next) {
	for (wl = wlist->mapSubwindow.wl; wl != NULL; wl = wl->next) {
	    if (win == wl->twm_win) {
		if (Scr->NoRaiseDeicon)
		    XMapWindow (dpy, wl->w);
		else
		    XMapRaised (dpy, wl->w);
		WMapRedrawName (wl);
		break;
	    }
	}
    }
}

void WMapRaiseLower (win)
TwmWindow *win;
{
    WorkSpaceList *wlist;
    WinList    wl;
    XWindowChanges xwc;

    for (wlist = Scr->workSpaceMgr.workSpaceList; wlist != NULL; wlist = wlist->next) {
	if (OCCUPY (win, wlist)) WMapRestack (wlist);
    }
}

void WMapLower (win)
TwmWindow *win;
{
    WorkSpaceList *wlist;
    WinList    wl;

    for (wlist = Scr->workSpaceMgr.workSpaceList; wlist != NULL; wlist = wlist->next) {
	if (OCCUPY (win, wlist)) WMapRestack (wlist);
    }
}

void WMapRaise (win)
TwmWindow *win;
{
    WorkSpaceList *wlist;
    WinList    wl;

    for (wlist = Scr->workSpaceMgr.workSpaceList; wlist != NULL; wlist = wlist->next) {
	if (OCCUPY (win, wlist)) WMapRestack (wlist);
    }
}

void WMapRestack (wlist)
WorkSpaceList *wlist;
{
    TwmWindow	*win;
    WinList	wl;
    Window	root;
    Window	parent;
    Window	*children, *smallws;
    unsigned int number;
    int		i, j;

    number = 0;
    XQueryTree (dpy, Scr->Root, &root, &parent, &children, &number);
    smallws = (Window*) malloc (number * sizeof (Window));

    j = 0;
    for (i = number - 1; i >= 0; i--) {
	if (XFindContext (dpy, children [i], TwmContext, (caddr_t *) &win) == XCNOENT) {
	    continue;
	}
	if (! OCCUPY (win, wlist)) continue;
	for (wl = wlist->mapSubwindow.wl; wl != NULL; wl = wl->next) {
	    if (win == wl->twm_win) {
		smallws [j++] = wl->w;
		break;
	    }
	}
    }
    XRestackWindows (dpy, smallws, j);

    XFree ((char *) children);
    free  (smallws);
    return;
}

void WMapUpdateIconName (win)
TwmWindow *win;
{
    WorkSpaceList *wlist;
    WinList    wl;

    for (wlist = Scr->workSpaceMgr.workSpaceList; wlist != NULL; wlist = wlist->next) {
	for (wl = wlist->mapSubwindow.wl; wl != NULL; wl = wl->next) {
	    if (win == wl->twm_win) {
		WMapRedrawName (wl);
		break;
	    }
	}
    }
}

void WMgrHandleKeyReleaseEvent (event)
XEvent *event;
{
    WorkSpaceList	*wlist;
    int			len, i, lname;
    char		key [16], k;
    char		name [128];
    char		*keyname;
    KeySym		keysym;

    keysym  = XLookupKeysym   ((XKeyEvent*) event, 0);
    keyname = XKeysymToString (keysym);
    if ((strcmp (keyname, "Control_R") == 0) || (strcmp (keyname, "Control_L") == 0)) {
	WMapToggleState ();
	return;
    }
}

void WMgrHandleKeyPressEvent (event)
XEvent *event;
{
    WorkSpaceList	*wlist;
    int			len, i, lname;
    char		key [16], k;
    char		name [128];
    char		*keyname;
    KeySym		keysym;

    keysym  = XLookupKeysym   ((XKeyEvent*) event, 0);
    keyname = XKeysymToString (keysym);
    if ((strcmp (keyname, "Control_R") == 0) || (strcmp (keyname, "Control_L") == 0)) {
	WMapToggleState ();
	return;
    }
    if (Scr->workSpaceMgr.workspaceWindow.state == MAPSTATE) return;

    for (wlist = Scr->workSpaceMgr.workSpaceList; wlist != NULL; wlist = wlist->next) {
	if (wlist->buttonw == event->xkey.subwindow) break;
    }
    if (wlist == NULL) return;

    strcpy (name, wlist->label);
    lname = strlen (name);
    len   = XLookupString (&(event->xkey), key, 16, NULL, NULL);
    for (i = 0; i < len; i++) {
        k = key [i];
	if (isprint (k)) {
	    name [lname++] = k;
	}
	else
	if ((k == 127) || (k == 8)) {
	    if (lname != 0) lname--;
	}
	else
	    break;
    }
    name [lname] = '\0';
    wlist->label = realloc (wlist->label, (strlen (name) + 1));
    strcpy (wlist->label, name);
    if (wlist == Scr->workSpaceMgr.activeWSPC)
	PaintButton (WSPCWINDOW, wlist->buttonw, wlist->label, wlist->cp,  on);
    else
	PaintButton (WSPCWINDOW, wlist->buttonw, wlist->label, wlist->cp, off);
}

void WMgrHandleButtonEvent (event)
XEvent *event;
{
    WorkSpaceWindow	*mw;
    WorkSpaceList	*wlist, *oldwlist, *newwlist;
    WinList		wl, newwl;
    TwmWindow		*win;
    int			occupation;
    unsigned int	W0, H0;
    int			found, cont;
    XEvent		ev;
    Window		w, sw, parent;
    int			X0, Y0, X1, Y1, XW, YW;
    Position		newX, newY;
    int			status;
    Window		junkW;
    unsigned int	junk;
    unsigned int	button;
    XWindowAttributes winattrs;

    parent = event->xbutton.window;
    sw     = event->xbutton.subwindow;
    mw     = &(Scr->workSpaceMgr.workspaceWindow);
    button = event->xbutton.button;

    if (Scr->workSpaceMgr.workspaceWindow.state == BUTTONSSTATE) {
	for (wlist = Scr->workSpaceMgr.workSpaceList; wlist != NULL; wlist = wlist->next) {
	    if (wlist->buttonw == parent) break;
	}
	if (wlist == NULL) return;
	GotoWorkSpace (wlist);
	return;
    }

    for (wlist = Scr->workSpaceMgr.workSpaceList; wlist != NULL; wlist = wlist->next) {
	if (wlist->mapSubwindow.w == parent) break;
    }
    if (wlist == NULL) return;
    if (sw == (Window) 0) {
	GotoWorkSpace (wlist);
	return;
    }
    oldwlist = wlist;

    if (XFindContext (dpy, sw, MapWListContext, (caddr_t *) &wl) == XCNOENT) return;
    win = wl->twm_win;
    if ((! Scr->TransientHasOccupation) && win->transient) return;

    XTranslateCoordinates (dpy, Scr->Root, sw, event->xbutton.x_root, event->xbutton.y_root,
				&XW, &YW, &junkW);
    switch (button) {
	case 1 :
	    XUnmapWindow (dpy, sw);

	case 2 :
	    XGetGeometry (dpy, sw, &junkW, &X0, &Y0, &W0, &H0, &junk, &junk);
	    XTranslateCoordinates (dpy, oldwlist->mapSubwindow.w, mw->w, X0, Y0, &X1, &Y1, &junkW);
	    w = XCreateSimpleWindow (dpy, mw->w, X1, Y1, W0, H0, 1, Scr->Black, Scr->White);
/* for an unknown reason, this window creation fails when in captive mode
and welcome on, same thing for icon managers subwindows
printf ("mw->w = %x, coord = %d, %d, %d, %d\n", mw->w, X1, Y1, W0, H0);
status = XGetWindowAttributes(dpy, w, &winattrs);
if (! status) printf ("XGetWindowAttributes failed on %x\n", w);
*/
	    XMapRaised (dpy, w);
	    break;

	case 3 :
	    occupation = win->occupation & ~(1 << oldwlist->number);
	    ChangeOccupation (win, occupation);
	    return;
	default :
	    return;
    }

    XGrabPointer (dpy, mw->w, False, ButtonPressMask | ButtonMotionMask | ButtonReleaseMask,
		  GrabModeAsync, GrabModeAsync, mw->w, Scr->MoveCursor, CurrentTime);

    cont = TRUE;
    XMaskEvent (dpy, ButtonPressMask | ButtonMotionMask | ButtonReleaseMask, &ev);
    while (cont) {
	switch (ev.xany.type) {
	    case ButtonPress :
	    case ButtonRelease :
		newX = ev.xbutton.x;
		newY = ev.xbutton.y;
		cont = FALSE;
		break;
	    case MotionNotify :
		XMoveWindow (dpy, w, ev.xbutton.x - XW, ev.xbutton.y - YW);
		XMaskEvent (dpy, ButtonPressMask | ButtonMotionMask | ButtonReleaseMask, &ev);
		break;
	}
    }
    ev.xbutton.subwindow = (Window) 0;
    ev.xbutton.window = parent;
    XPutBackEvent   (dpy, &ev);
    XUngrabPointer  (dpy, CurrentTime);

    if ((ev.xbutton.time - event->xbutton.time) < 250) {
	float wf, hf;
	KeyCode control_L_code, control_R_code;
	KeySym  control_L_sym,  control_R_sym;
	char keys [32];

	XMapWindow (dpy, sw);
	XDestroyWindow (dpy, w);
	GotoWorkSpace (wlist);
	WarpToWindow (win);
	control_L_sym  = XStringToKeysym  ("Control_L");
	control_R_sym  = XStringToKeysym  ("Control_R");
	control_L_code = XKeysymToKeycode (dpy, control_L_sym);
	control_R_code = XKeysymToKeycode (dpy, control_R_sym);
	XQueryKeymap (dpy, keys);
	if ((keys [control_L_code / 8] & ((char) 0x80 >> (control_L_code % 8))) ||
	     keys [control_R_code / 8] & ((char) 0x80 >> (control_R_code % 8))) {
	    WMapToggleState ();
	}
	return;
    }

    for (wlist = Scr->workSpaceMgr.workSpaceList; wlist != NULL; wlist = wlist->next) {
	if ((newX >= wlist->mapSubwindow.x) && (newX < wlist->mapSubwindow.x + mw->wwidth) &&
	    (newY >= wlist->mapSubwindow.y) && (newY < wlist->mapSubwindow.y + mw->wheight)) {
	    break;
	}
    }
    newwlist = wlist;
    switch (button) {
	case 1 :
	    if ((newwlist == NULL) || (newwlist == oldwlist) || OCCUPY (wl->twm_win, newwlist)) {
		XMapWindow (dpy, sw);
		break;
	    }
	    occupation = (win->occupation | (1 << newwlist->number)) & ~(1 << oldwlist->number);
	    ChangeOccupation (win, occupation);
	    if (newwlist == Scr->workSpaceMgr.activeWSPC) {
		XRaiseWindow (dpy, win->frame);
		WMapRaise (win);
	    }
	    else WMapRestack (newwlist);
	    break;

	case 2 :
	    if ((newwlist == NULL) || (newwlist == oldwlist) ||
		OCCUPY (wl->twm_win, newwlist)) break;

	    occupation = win->occupation | (1 << newwlist->number);
	    ChangeOccupation (win, occupation);
	    if (newwlist == Scr->workSpaceMgr.activeWSPC) {
		XRaiseWindow (dpy, win->frame);
		WMapRaise (win);
	    }
	    else WMapRestack (newwlist);
	    break;

	default :
	    return;
    }
    XDestroyWindow (dpy, w);
}

void WMapRedrawName (wl)
WinList   wl;
{
    MyFont    font;
    int       strhei, strwid, x, y;
    char      *label;

    XClearWindow (dpy, wl->w);

    label  = wl->twm_win->icon_name;
    font   = Scr->workSpaceMgr.workspaceWindow.windowFont;
    strhei = font.font->max_bounds.ascent + font.font->max_bounds.descent;
    if (strhei > wl->height) return;

    strwid = XTextWidth (font.font, label, strlen (label));
    x = (wl->width  - strwid) / 2;
    if (x < 1) x = 1;
    y = ((wl->height + strhei) / 2) - font.font->max_bounds.descent;

    FBF (wl->cp.fore, wl->cp.back, font.font->fid);
    XDrawString (dpy, wl->w, Scr->NormalGC, x, y, label, strlen (label));
}

static void WMapAddToList (win, wlist)
TwmWindow     *win;
WorkSpaceList *wlist;
{
    WinList wl;
    float   wf, hf;
    ColorPair cp;
    XSetWindowAttributes attr;
    unsigned long attrmask;

    cp.back = Scr->workSpaceMgr.workspaceWindow.windowcp.back;
    GetColorFromList (Scr->workSpaceMgr.workspaceWindow.windowBackgroundL,
			win->full_name, &win->class, &cp.back);

    cp.fore = Scr->workSpaceMgr.workspaceWindow.windowcp.fore;
    GetColorFromList (Scr->workSpaceMgr.workspaceWindow.windowForegroundL,
		      win->full_name, &win->class, &cp.fore);

    wf = (float) (Scr->workSpaceMgr.workspaceWindow.wwidth  - 2) / (float) Scr->MyDisplayWidth;
    hf = (float) (Scr->workSpaceMgr.workspaceWindow.wheight - 2) / (float) Scr->MyDisplayHeight;
    wl = (WinList) malloc (sizeof (struct winList));
    wl->x      = (int) (win->frame_x * wf);
    wl->y      = (int) (win->frame_y * hf);
    wl->width  = (unsigned int) (win->frame_width  * wf);
    wl->height = (unsigned int) (win->frame_height * hf);
    wl->w = XCreateSimpleWindow (dpy, wlist->mapSubwindow.w, wl->x, wl->y, wl->width, wl->height,
					     1, Scr->Black, cp.back);
    attrmask = 0;
    if (Scr->BackingStore) {
	attr.backing_store = WhenMapped;
	attrmask |= CWBackingStore;
    }
    attr.cursor = handCursor;
    attrmask |= CWCursor;
    XChangeWindowAttributes (dpy, wl->w, attrmask, &attr);
    XSelectInput (dpy, wl->w, ExposureMask);
    XSaveContext (dpy, wl->w, TwmContext,      (caddr_t) Scr->workSpaceMgr.workspaceWindow.twm_win);
    XSaveContext (dpy, wl->w, ScreenContext,   (caddr_t) Scr);
    XSaveContext (dpy, wl->w, MapWListContext, (caddr_t) wl);
    wl->twm_win = win;
    wl->cp      = cp;
    wl->next    = wlist->mapSubwindow.wl;
    wlist->mapSubwindow.wl = wl;
    if (win->mapped) XMapWindow (dpy, wl->w);
}

static void WMapRemoveFromList (win, wlist)
TwmWindow *win;
WorkSpaceList *wlist;
{
    WinList wl, wl1;

    wl = wlist->mapSubwindow.wl;
    if (wl == NULL) return;
    if (win == wl->twm_win) {
	wlist->mapSubwindow.wl = wl->next;
	XDeleteContext (dpy, wl->w, TwmContext);
	XDeleteContext (dpy, wl->w, ScreenContext);
	XDeleteContext (dpy, wl->w, MapWListContext);
	XDestroyWindow (dpy, wl->w);
	free (wl);
	return;
    }
    wl1 = wl;
    wl  = wl->next;
    while (wl != NULL) {
	if (win == wl->twm_win) {
	    wl1->next = wl->next;
	    XDeleteContext (dpy, wl->w, TwmContext);
	    XDeleteContext (dpy, wl->w, ScreenContext);
	    XDeleteContext (dpy, wl->w, MapWListContext);
	    XDestroyWindow (dpy, wl->w);
	    free (wl);
	    break;
	}
	wl1 = wl;
	wl  = wl->next;
    }
}

static void ResizeWorkSpaceManager (w, h)
int w, h;
{
    int           bwidth, bheight;
    int		  wwidth, wheight;
    int           hspace, vspace;
    int           lines, columns;
    WorkSpaceList *wlist;
    TwmWindow	  *tmp_win;
    WinList	  wl;
    int           i, j;
    static int    oldw = 0;
    static int    oldh = 0;
    float	  wf, hf;

    if ((w == oldw) && (h == oldh)) return;
    oldw = w;
    oldh = h;

    hspace  = Scr->workSpaceMgr.workspaceWindow.hspace;
    vspace  = Scr->workSpaceMgr.workspaceWindow.vspace;
    lines   = Scr->workSpaceMgr.workspaceWindow.lines;
    columns = Scr->workSpaceMgr.workspaceWindow.columns;
    bwidth  = (w - ((columns + 1) * hspace)) / columns;
    bheight = (h - ((lines   + 1) * vspace)) / lines;
    wwidth  = w / Scr->workSpaceMgr.workspaceWindow.columns;
    wheight = h / Scr->workSpaceMgr.workspaceWindow.lines;
    wf = (float) (wwidth  - 2) / (float) Scr->MyDisplayWidth;
    hf = (float) (wheight - 2) / (float) Scr->MyDisplayHeight;

    i = 0;
    j = 0;
    for (wlist = Scr->workSpaceMgr.workSpaceList; wlist != NULL; wlist = wlist->next) {
	XMoveResizeWindow (dpy, wlist->buttonw, i * (bwidth  + hspace) + hspace,
				j * (bheight + vspace) + vspace, bwidth, bheight);
	wlist->mapSubwindow.x = i * wwidth;
	wlist->mapSubwindow.y = j * wheight;
	XMoveResizeWindow (dpy, wlist->mapSubwindow.w, wlist->mapSubwindow.x,
				wlist->mapSubwindow.y, wwidth - 2, wheight - 2);
	for (wl = wlist->mapSubwindow.wl; wl != NULL; wl = wl->next) {
	    tmp_win    = wl->twm_win;
	    wl->x      = (int) (tmp_win->frame_x * wf);
	    wl->y      = (int) (tmp_win->frame_y * hf);
	    wl->width  = (unsigned int) (tmp_win->frame_width  * wf);
	    wl->height = (unsigned int) (tmp_win->frame_height * hf);
	    XMoveResizeWindow (dpy, wl->w, wl->x, wl->y, wl->width, wl->height);
	}
	i++;
	if (i == columns) {i = 0; j++;};
    }
    Scr->workSpaceMgr.workspaceWindow.bwidth    = bwidth;
    Scr->workSpaceMgr.workspaceWindow.bheight   = bheight;
    Scr->workSpaceMgr.workspaceWindow.width     = w;
    Scr->workSpaceMgr.workspaceWindow.height    = h;
    Scr->workSpaceMgr.workspaceWindow.wwidth	= wwidth;
    Scr->workSpaceMgr.workspaceWindow.wheight	= wheight;
    PaintWorkSpaceManager ();
}

static void ResizeOccupyWindow (w, h)
int w, h;
{
    int           bwidth, bheight, owidth, oheight;
    int           hspace, vspace;
    int           lines, columns;
    WorkSpaceList *wlist;
    int           i, j, x, y;
    static int    oldw = 0;
    static int    oldh = 0;

    if ((w == oldw) && (h == oldh)) return;
    oldw = w;
    oldh = h;

    hspace  = Scr->workSpaceMgr.occupyWindow.hspace;
    vspace  = Scr->workSpaceMgr.occupyWindow.vspace;
    lines   = Scr->workSpaceMgr.occupyWindow.lines;
    columns = Scr->workSpaceMgr.occupyWindow.columns;
    bwidth  = (w - ((columns + 1) * hspace)) / columns;
    bheight = (h - ((lines   + 4) * vspace)) / (lines + 1);
    owidth  = (w - (4 * hspace)) / 3;
    oheight = bheight;

    i = 0;
    j = 0;
    for (wlist = Scr->workSpaceMgr.workSpaceList; wlist != NULL; wlist = wlist->next) {
	XMoveResizeWindow (dpy, wlist->obuttonw, i * (bwidth  + hspace) + hspace,
				j * (bheight + vspace) + vspace, bwidth, bheight);
	i++;
	if (i == columns) {i = 0; j++;}
    }
    x = hspace;
    y = ((bheight + vspace) * lines) + ( 2 * vspace);
    XMoveResizeWindow (dpy, Scr->workSpaceMgr.occupyWindow.OK, x, y, owidth, oheight);
    x += owidth + hspace;
    XMoveResizeWindow (dpy, Scr->workSpaceMgr.occupyWindow.cancel, x, y, owidth, oheight);
    x += owidth + hspace;
    XMoveResizeWindow (dpy, Scr->workSpaceMgr.occupyWindow.allworkspc, x, y, owidth, oheight);
    Scr->workSpaceMgr.occupyWindow.width      = w;
    Scr->workSpaceMgr.occupyWindow.height     = h;
    Scr->workSpaceMgr.occupyWindow.bwidth     = bwidth;
    Scr->workSpaceMgr.occupyWindow.bheight    = bheight;
    Scr->workSpaceMgr.occupyWindow.owidth     = owidth;
    Scr->workSpaceMgr.occupyWindow.oheight    = oheight;
    PaintOccupyWindow ();
}

void WMapCreateCurrentBackGround (border, background, foreground, pixmap)
char *border, *background, *foreground, *pixmap;
{
    XGCValues	 gcvalues;
    Image	 *image;

    Scr->workSpaceMgr.workspaceWindow.curBorderColor = Scr->Black;
    Scr->workSpaceMgr.workspaceWindow.curColors.back = Scr->White;
    Scr->workSpaceMgr.workspaceWindow.curColors.fore = Scr->Black;
    Scr->workSpaceMgr.workspaceWindow.curImage       = None;

    if (border == NULL) return;
    GetColor (Scr->Monochrome, &(Scr->workSpaceMgr.workspaceWindow.curBorderColor), border);
    if (background == NULL) return;
    GetColor (Scr->Monochrome, &(Scr->workSpaceMgr.workspaceWindow.curColors.back), background);
    if (foreground == NULL) return;
    GetColor (Scr->Monochrome, &(Scr->workSpaceMgr.workspaceWindow.curColors.fore), foreground);

    if (pixmap == NULL) return;
    if ((image = GetImage (pixmap, Scr->workSpaceMgr.workspaceWindow.curColors)) == None) {
	fprintf (stderr, "Can't find pixmap %s\n", pixmap);
	return;
    }
    Scr->workSpaceMgr.workspaceWindow.curImage = image;
}

void WMapCreateDefaultBackGround (border, background, foreground, pixmap)
char *border, *background, *foreground, *pixmap;
{
    Image	 *image;
    XGCValues	 gcvalues;

    Scr->workSpaceMgr.workspaceWindow.defBorderColor = Scr->Black;
    Scr->workSpaceMgr.workspaceWindow.defColors.back = Scr->White;
    Scr->workSpaceMgr.workspaceWindow.defColors.fore = Scr->Black;
    Scr->workSpaceMgr.workspaceWindow.defImage       = None;

    if (border == NULL) return;
    GetColor (Scr->Monochrome, &(Scr->workSpaceMgr.workspaceWindow.defBorderColor), border);
    if (background == NULL) return;
    GetColor (Scr->Monochrome, &(Scr->workSpaceMgr.workspaceWindow.defColors.back), background);
    if (foreground == NULL) return;
    GetColor (Scr->Monochrome, &(Scr->workSpaceMgr.workspaceWindow.defColors.fore), foreground);

    if (pixmap == NULL) return;
    if ((image = GetImage (pixmap, Scr->workSpaceMgr.workspaceWindow.defColors)) == None)
	return;
    Scr->workSpaceMgr.workspaceWindow.defImage = image;
}

Bool AnimateRoot () {
    ScreenInfo	  *scr;
    int		  scrnum;
    Image	  *image;
    WorkSpaceList *wlist;
    Bool	  maybeanimate;

    maybeanimate = False;
    for (scrnum = 0; scrnum < NumScreens; scrnum++) {
	if ((scr = ScreenList [scrnum]) == NULL) continue;
	if (! scr->workSpaceManagerActive) continue;
	if (! scr->workSpaceMgr.activeWSPC) continue; /* une securite de plus */

	image = scr->workSpaceMgr.activeWSPC->image;
	if ((image == None) || (image->next == None)) continue;
	if (scr->DontPaintRootWindow) continue;

	XSetWindowBackgroundPixmap (dpy, scr->Root, image->pixmap);
	XClearWindow (dpy, scr->Root);
	scr->workSpaceMgr.activeWSPC->image = image->next;
	maybeanimate = True;
    }
    for (scrnum = 0; scrnum < NumScreens; scrnum++) {
	if ((scr = ScreenList [scrnum]) == NULL) continue;
	if (scr->workSpaceMgr.workspaceWindow.state == BUTTONSSTATE) continue;

	for (wlist = scr->workSpaceMgr.workSpaceList; wlist != NULL; wlist = wlist->next) {
	    image = wlist->image;

	    if ((image == None) || (image->next == None)) continue;
	    if (wlist == scr->workSpaceMgr.activeWSPC) continue;
	    XSetWindowBackgroundPixmap (dpy, wlist->mapSubwindow.w, image->pixmap);
	    XClearWindow (dpy, wlist->mapSubwindow.w);
	    wlist->image = image->next;
	    maybeanimate = True;
	}
    }
    return (maybeanimate);
}

#ifdef BUGGY_HP700_SERVER
static void fakeRaiseLower (display, window)
Display *display;
Window   window;
{
    Window          root;
    Window          parent;
    Window          grandparent;
    Window         *children;
    unsigned int    number;
    XWindowChanges  changes;

    number = 0;
    XQueryTree (display, window, &root, &parent, &children, &number);
    XFree ((char *) children);
    XQueryTree (display, parent, &root, &grandparent, &children, &number);

    changes.stack_mode = (children [number-1] == window) ? Below : Above;
    XFree ((char *) children);
    XConfigureWindow (display, window, CWStackMode, &changes);
}
#endif




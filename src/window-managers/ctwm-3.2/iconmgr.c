/*
 * Copyright 1989 Massachusetts Institute of Technology
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of M.I.T. not be used in advertising
 * or publicity pertaining to distribution of the software without specific,
 * written prior permission.  M.I.T. makes no representations about the
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * M.I.T. DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL M.I.T.
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION
 * OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN 
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */
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
 * $XConsortium: iconmgr.c,v 1.48 91/09/10 15:27:07 dave Exp $
 *
 * Icon Manager routines
 *
 * 09-Mar-89 Tom LaStrange		File Created
 *
 * Do the necessary modification to be integrated in ctwm.
 * Can no longer be used for the standard twm.
 *
 * 22-April-92 Claude Lecommandeur.
 *
 *
 ***********************************************************************/

#include <stdio.h>
#include "twm.h"
#include "util.h"
#include "parse.h"
#include "screen.h"
#include "resize.h"
#include "add_window.h"
#include "siconify.bm"
#ifdef VMS
#include <decw$include/Xos.h>
#include <X11Xmu/CharSet.h>
#else
#include <X11/Xos.h>
#include <X11/Xmu/CharSet.h>
#endif
#ifdef macII
int strcmp(); /* missing from string.h in AUX 2.0 */
#endif

int iconmgr_textx = siconify_width+11;
WList *Active = NULL;
WList *Current = NULL;
WList *DownIconManager = NULL;
int iconifybox_width = siconify_width;
int iconifybox_height = siconify_height;

/***********************************************************************
 *
 *  Procedure:
 *	CreateIconManagers - creat all the icon manager windows
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

void CreateIconManagers()
{
    IconMgr *p, *q;
    int mask;
    char str[100];
    char str1[100];
    Pixel background;
    char *icon_name;
    WorkSpaceList *wlist;
    XWMHints	  wmhints;
    int bw;

    if (Scr->NoIconManagers)
	return;

    if (Scr->siconifyPm == None)
    {
	Scr->siconifyPm = XCreatePixmapFromBitmapData(dpy, Scr->Root,
	    (char *)siconify_bits, siconify_width, siconify_height, 1, 0, 1);
    }

    wlist = Scr->workSpaceMgr.workSpaceList;
    for (q = Scr->iconmgr; q != NULL; q = q->nextv) {
      for (p = q; p != NULL; p = p->next)
      {
	sprintf(str, "%s Icon Manager", p->name);
	sprintf(str1, "%s Icons", p->name);
	if (p->icon_name)
	    icon_name = p->icon_name;
	else
	    icon_name = str1;

	if (!p->geometry || !strlen(p->geometry)) p->geometry = "+0+0";
	mask = XParseGeometry(p->geometry, &JunkX, &JunkY,
			      (unsigned int *) &p->width, (unsigned int *)&p->height);

	bw = LookInList (Scr->NoBorder, str, NULL) ? 0 :
		(Scr->ThreeDBorderWidth ? Scr->ThreeDBorderWidth : Scr->BorderWidth);

	if (mask & XNegative)
	    JunkX += Scr->MyDisplayWidth - p->width - 2 * bw;

	if (mask & YNegative)
	    JunkY += Scr->MyDisplayHeight - p->height - 2 * bw;

	background = Scr->IconManagerC.back;
	GetColorFromList(Scr->IconManagerBL, p->name, (XClassHint *)NULL,
			 &background);

	if (p->width  < 1) p->width  = 1;
	if (p->height < 1) p->height = 1;
	p->w = XCreateSimpleWindow(dpy, Scr->Root,
	    JunkX, JunkY, p->width, p->height, 1,
	    Scr->Black, background);

	XSetStandardProperties(dpy, p->w, str, icon_name, None, NULL, 0, NULL);

	Scr->workSpaceMgr.activeWSPC = wlist;
	wmhints.initial_state = NormalState;
	wmhints.input         = True;
	XSetWMHints (dpy, p->w, &wmhints);
	p->twm_win = AddWindow(p->w, TRUE, p);

	p->twm_win->mapped = FALSE;
	SetMapStateProp (p->twm_win, WithdrawnState);
	if (p->twm_win && p->twm_win->wmhints &&
	    (p->twm_win->wmhints->initial_state == IconicState)) {
	    p->twm_win->isicon = TRUE;
	}
	else
	if (!Scr->NoIconManagers && Scr->ShowIconManager) {
	    p->twm_win->isicon = FALSE;
	}
	else {
	    p->twm_win->isicon = TRUE;
	}
      }
      if (wlist != NULL) wlist = wlist->next;
    }
    Scr->workSpaceMgr.activeWSPC = Scr->workSpaceMgr.workSpaceList;
    if (Scr->workSpaceManagerActive)
	Scr->workSpaceMgr.workSpaceList->iconmgr = Scr->iconmgr;

    for (q = Scr->iconmgr; q != NULL; q = q->nextv) {
      for (p = q; p != NULL; p = p->next) {
	GrabButtons(p->twm_win);
	GrabKeys(p->twm_win);
      }
    }

}

/***********************************************************************
 *
 *  Procedure:
 *	AllocateIconManager - allocate a new icon manager
 *
 *  Inputs:
 *	name	- the name of this icon manager
 *	icon_name - the name of the associated icon
 *	geom	- a geometry string to eventually parse
 *	columns	- the number of columns this icon manager has
 *
 ***********************************************************************
 */

IconMgr *AllocateIconManager(name, icon_name, geom, columns)
    char *name;
    char *geom;
    char *icon_name;
    int columns;
{
    IconMgr *p;

#ifdef DEBUG_ICONMGR
    fprintf(stderr, "AllocateIconManager\n");
    fprintf(stderr, "  name=\"%s\" icon_name=\"%s\", geom=\"%s\", col=%d\n",
	name, icon_name, geom, columns);
#endif

    if (Scr->NoIconManagers)
	return NULL;

    if (columns < 1) columns = 1;
    p = (IconMgr *)malloc(sizeof(IconMgr));
    p->name = name;
    p->icon_name = icon_name;
    p->geometry = geom;
    p->columns = columns;
    p->first = NULL;
    p->last = NULL;
    p->active = NULL;
    p->scr = Scr;
    p->count = 0;
    p->x = 0;
    p->y = 0;
    p->width = 150;
    p->height = 10;
    p->next = NULL;
    p->prev = NULL;
    p->nextv = NULL;

    if (Scr->iconmgr == NULL) {
	Scr->iconmgr = p;
	Scr->iconmgr->lasti = p;
    }
    else {
	Scr->iconmgr->lasti->next = p;
	p->prev = Scr->iconmgr->lasti;
	Scr->iconmgr->lasti = p;
    }
    return(p);
}

/***********************************************************************
 *
 *  Procedure:
 *	MoveIconManager - move the pointer around in an icon manager
 *
 *  Inputs:
 *	dir	- one of the following:
 *			F_FORWICONMGR	- forward in the window list
 *			F_BACKICONMGR	- backward in the window list
 *			F_UPICONMGR	- up one row
 *			F_DOWNICONMGR	- down one row
 *			F_LEFTICONMGR	- left one column
 *			F_RIGHTICONMGR	- right one column
 *
 *  Special Considerations:
 *	none
 *
 ***********************************************************************
 */

void MoveIconManager(dir)
    int dir;
{
    IconMgr *ip;
    WList *tmp = NULL;
    int cur_row, cur_col, new_row, new_col;
    int row_inc, col_inc;
    int got_it;

    if (!Current) return;

    cur_row = Current->row;
    cur_col = Current->col;
    ip = Current->iconmgr;

    row_inc = 0;
    col_inc = 0;
    got_it = FALSE;

    switch (dir)
    {
	case F_FORWICONMGR:
	    if ((tmp = Current->next) == NULL)
		tmp = ip->first;
	    got_it = TRUE;
	    break;

	case F_BACKICONMGR:
	    if ((tmp = Current->prev) == NULL)
		tmp = ip->last;
	    got_it = TRUE;
	    break;

	case F_UPICONMGR:
	    row_inc = -1;
	    break;

	case F_DOWNICONMGR:
	    row_inc = 1;
	    break;

	case F_LEFTICONMGR:
	    col_inc = -1;
	    break;

	case F_RIGHTICONMGR:
	    col_inc = 1;
	    break;
    }

    /* If got_it is FALSE ast this point then we got a left, right,
     * up, or down, command.  We will enter this loop until we find
     * a window to warp to.
     */
    new_row = cur_row;
    new_col = cur_col;

    while (!got_it)
    {
	new_row += row_inc;
	new_col += col_inc;
	if (new_row < 0)
	    new_row = ip->cur_rows - 1;
	if (new_col < 0)
	    new_col = ip->cur_columns - 1;
	if (new_row >= ip->cur_rows)
	    new_row = 0;
	if (new_col >= ip->cur_columns)
	    new_col = 0;
	    
	/* Now let's go through the list to see if there is an entry with this
	 * new position
	 */
	for (tmp = ip->first; tmp != NULL; tmp = tmp->next)
	{
	    if (tmp->row == new_row && tmp->col == new_col)
	    {
		got_it = TRUE;
		break;
	    }
	}
    }

    if (!got_it)
    {
	fprintf (stderr, 
		 "%s:  unable to find window (%d, %d) in icon manager\n", 
		 ProgramName, new_row, new_col);
	return;
    }

    if (tmp == NULL)
      return;

    /* raise the frame so the icon manager is visible */
    if (ip->twm_win->mapped) {
	XRaiseWindow(dpy, ip->twm_win->frame);
	XWarpPointer(dpy, None, tmp->icon, 0,0,0,0, 5, 5);
    } else {
	if (tmp->twm->title_height) {
	    int tbx = Scr->TBInfo.titlex;
	    int x = tmp->twm->highlightxr;
	    XWarpPointer (dpy, None, tmp->twm->title_w, 0, 0, 0, 0,
			  tbx + (x - tbx) / 2,
			  Scr->TitleHeight / 4);
	} else {
	    XWarpPointer (dpy, None, tmp->twm->w, 0, 0, 0, 0, 5, 5);
	}
    }
}

/***********************************************************************
 *
 *  Procedure:
 *	JumpIconManager - jump from one icon manager to another,
 *		possibly even on another screen
 *
 *  Inputs:
 *	dir	- one of the following:
 *			F_NEXTICONMGR	- go to the next icon manager 
 *			F_PREVICONMGR	- go to the previous one
 *
 ***********************************************************************
 */

void JumpIconManager(dir)
    register int dir;
{
    IconMgr *ip, *tmp_ip = NULL;
    int got_it = FALSE;
    ScreenInfo *sp;
    int screen;

    if (!Current) return;


#define ITER(i) (dir == F_NEXTICONMGR ? (i)->next : (i)->prev)
#define IPOFSP(sp) (dir == F_NEXTICONMGR ? sp->iconmgr : sp->iconmgr->lasti)
#define TEST(ip) if ((ip)->count != 0 && (ip)->twm_win->mapped) \
		 { got_it = TRUE; break; }

    ip = Current->iconmgr;
    for (tmp_ip = ITER(ip); tmp_ip; tmp_ip = ITER(tmp_ip)) {
	TEST (tmp_ip);
    }

    if (!got_it) {
	int origscreen = ip->scr->screen;
	int inc = (dir == F_NEXTICONMGR ? 1 : -1);

	for (screen = origscreen + inc; ; screen += inc) {
	    if (screen >= NumScreens)
	      screen = 0;
	    else if (screen < 0)
	      screen = NumScreens - 1;

	    sp = ScreenList[screen];
	    if (sp) {
		for (tmp_ip = IPOFSP (sp); tmp_ip; tmp_ip = ITER(tmp_ip)) {
		    TEST (tmp_ip);
		}
	    }
	    if (got_it || screen == origscreen) break;
	}
    }

#undef ITER
#undef IPOFSP
#undef TEST

    if (!got_it) {
	XBell (dpy, 0);
	return;
    }

    /* raise the frame so it is visible */
    XRaiseWindow(dpy, tmp_ip->twm_win->frame);
    if (tmp_ip->active)
	XWarpPointer(dpy, None, tmp_ip->active->icon, 0,0,0,0, 5, 5);
    else
	XWarpPointer(dpy, None, tmp_ip->w, 0,0,0,0, 5, 5);
}

/***********************************************************************
 *
 *  Procedure:
 *	AddIconManager - add a window to an icon manager
 *
 *  Inputs:
 *	tmp_win	- the TwmWindow structure
 *
 ***********************************************************************
 */

WList *AddIconManager(tmp_win)
    TwmWindow *tmp_win;
{
    WList *tmp, *old;
    int h;
    unsigned long valuemask;		/* mask for create windows */
    XSetWindowAttributes attributes;	/* attributes for create windows */
    IconMgr *ip;

    if (tmp_win->iconmgr || tmp_win->transient || Scr->NoIconManagers ||
	tmp_win->w == Scr->workSpaceMgr.workspaceWindow.w ||
	tmp_win->w == Scr->workSpaceMgr.occupyWindow.w)
	return NULL;

    if (LookInList(Scr->IconMgrNoShow, tmp_win->full_name, &tmp_win->class))
	return NULL;
    if (Scr->IconManagerDontShow &&
	!LookInList(Scr->IconMgrShow, tmp_win->full_name, &tmp_win->class))
	return NULL;
    if ((ip = (IconMgr *)LookInList(Scr->IconMgrs, tmp_win->full_name,
	    &tmp_win->class)) == NULL)
	if (Scr->workSpaceManagerActive)
	    ip = Scr->workSpaceMgr.workSpaceList->iconmgr;
	else
	    ip = Scr->iconmgr;

  tmp = NULL;
  old = tmp_win->list;
  while (ip != NULL) {
    if ((tmp_win->occupation & ip->twm_win->occupation) == 0) {ip = ip->nextv; continue;}
    tmp = (WList *) malloc(sizeof(WList));
    tmp->iconmgr = ip;
    tmp->next = NULL;
    tmp->active = FALSE;
    tmp->down = FALSE;

    InsertInIconManager(ip, tmp, tmp_win);

    tmp->twm = tmp_win;

    tmp->cp.fore = Scr->IconManagerC.fore;
    tmp->cp.back = Scr->IconManagerC.back;
    tmp->highlight = Scr->IconManagerHighlight;

    GetColorFromList(Scr->IconManagerFL, tmp_win->full_name, &tmp_win->class,
	&tmp->cp.fore);
    GetColorFromList(Scr->IconManagerBL, tmp_win->full_name, &tmp_win->class,
	&tmp->cp.back);
    GetColorFromList(Scr->IconManagerHighlightL, tmp_win->full_name,
	&tmp_win->class, &tmp->highlight);

    if (Scr->use3Diconmanagers) {
	if (!Scr->BeNiceToColormap) GetShadeColors (&tmp->cp);
	tmp->iconifypm = Create3DIconManagerIcon (tmp->cp);
    }
    h = Scr->IconManagerFont.height + 10;
    if (h < (siconify_height + 4))
	h = siconify_height + 4;

    ip->height = h * ip->count;
    tmp->me = ip->count;
    tmp->x = -1;
    tmp->y = -1;
    
    valuemask = (CWBackPixel | CWBorderPixel | CWEventMask | CWCursor);
    attributes.background_pixel = tmp->cp.back;
    attributes.border_pixel = tmp->cp.back;
    attributes.event_mask = (KeyPressMask | ButtonPressMask |
			     ButtonReleaseMask | ExposureMask |
			     EnterWindowMask | LeaveWindowMask);
    attributes.cursor = Scr->IconMgrCursor;
    tmp->w = XCreateWindow (dpy, ip->w, 0, 0, (unsigned int) 1, 
			    (unsigned int) h, (unsigned int) 0, 
			    CopyFromParent, (unsigned int) CopyFromParent,
			    (Visual *) CopyFromParent, valuemask, &attributes);

/* for an unknown reason, this window creation fails when in captive mode
and welcome on, same thing for workspace manager subwindows
{
    XWindowAttributes winattrs;
    Status status;
    
printf ("tmp->w = %x\n", tmp->w);
status = XGetWindowAttributes(dpy, tmp->w, &winattrs);
if (! status) printf ("XGetWindowAttributes failed on %x\n", tmp->w);
}*/
    valuemask = (CWBackPixel | CWBorderPixel | CWEventMask | CWCursor);
    attributes.background_pixel = tmp->cp.back;
    attributes.border_pixel = Scr->Black;
    attributes.event_mask = (ButtonReleaseMask| ButtonPressMask |
			     ExposureMask);
    attributes.cursor = Scr->ButtonCursor;
    tmp->icon = XCreateWindow (dpy, tmp->w, 5, (int) (h - siconify_height)/2,
			       (unsigned int) siconify_width,
			       (unsigned int) siconify_height,
			       (unsigned int) 0, CopyFromParent,
			       (unsigned int) CopyFromParent,
			       (Visual *) CopyFromParent,
			       valuemask, &attributes);

    ip->count += 1;
    PackIconManager(ip);
    if (Scr->WindowMask) XRaiseWindow (dpy, Scr->WindowMask);
    XMapWindow(dpy, tmp->w);

    XSaveContext(dpy, tmp->w, IconManagerContext, (caddr_t) tmp);
    XSaveContext(dpy, tmp->w, TwmContext, (caddr_t) tmp_win);
    XSaveContext(dpy, tmp->w, ScreenContext, (caddr_t) Scr);
    XSaveContext(dpy, tmp->icon, TwmContext, (caddr_t) tmp_win);
    XSaveContext(dpy, tmp->icon, ScreenContext, (caddr_t) Scr);

    if (!ip->twm_win->isicon)
    {
      if (OCCUPY (ip->twm_win, Scr->workSpaceMgr.activeWSPC)) {
	SetMapStateProp (ip->twm_win, NormalState);
	XMapWindow (dpy, ip->w);
	XMapWindow (dpy, ip->twm_win->frame);
      }
	ip->twm_win->mapped = TRUE;
    }
    tmp->nextv = old;
    old = tmp;
    ip = ip->nextv;
  }
    if (tmp == NULL) return NULL;
    tmp_win->list = tmp;
    if (! OCCUPY (tmp->iconmgr->twm_win, Scr->workSpaceMgr.activeWSPC)) {
	old = tmp;
	tmp = tmp->nextv;
	while (tmp != NULL) {
	    if (OCCUPY (tmp->iconmgr->twm_win, Scr->workSpaceMgr.activeWSPC)) break;
	    old = tmp;
	    tmp = tmp->nextv;
	}
	if (tmp != NULL) {
	    old->nextv = tmp->nextv;
	    tmp->nextv = tmp_win->list;
	    tmp_win->list = tmp;
	}
    }
    return (tmp_win->list);
}

/***********************************************************************
 *
 *  Procedure:
 *	InsertInIconManager - put an allocated entry into an icon 
 *		manager
 *
 *  Inputs:
 *	ip	- the icon manager pointer
 *	tmp	- the entry to insert
 *
 ***********************************************************************
 */

void InsertInIconManager(ip, tmp, tmp_win)
    IconMgr *ip;
    WList *tmp;
    TwmWindow *tmp_win;
{
    WList *tmp1;
    int added;
    int (*compar)() = (Scr->CaseSensitive ? strcmp : XmuCompareISOLatin1);

    added = FALSE;
    if (ip->first == NULL)
    {
	ip->first = tmp;
	tmp->prev = NULL;
	ip->last = tmp;
	added = TRUE;
    }
    else if (Scr->SortIconMgr)
    {
	for (tmp1 = ip->first; tmp1 != NULL; tmp1 = tmp1->next)
	{
	    if ((*compar)(tmp_win->icon_name, tmp1->twm->icon_name) < 0)
	    {
		tmp->next = tmp1;
		tmp->prev = tmp1->prev;
		tmp1->prev = tmp;
		if (tmp->prev == NULL)
		    ip->first = tmp;
		else
		    tmp->prev->next = tmp;
		added = TRUE;
		break;
	    }
	}
    }

    if (!added)
    {
	ip->last->next = tmp;
	tmp->prev = ip->last;
	ip->last = tmp;
    }
}

void RemoveFromIconManager(ip, tmp)
    IconMgr *ip;
    WList *tmp;
{
    if (tmp->prev == NULL)
	ip->first = tmp->next;
    else
	tmp->prev->next = tmp->next;

    if (tmp->next == NULL)
	ip->last = tmp->prev;
    else
	tmp->next->prev = tmp->prev;
    if (Current == tmp) Current = NULL;
}

/***********************************************************************
 *
 *  Procedure:
 *	RemoveIconManager - remove a window from the icon manager
 *
 *  Inputs:
 *	tmp_win	- the TwmWindow structure
 *
 ***********************************************************************
 */

void RemoveIconManager(tmp_win)
    TwmWindow *tmp_win;
{
    IconMgr *ip;
    WList *tmp, *tmp1, *save;

    if (tmp_win->list == NULL)
	return;

    tmp  = tmp_win->list;
    tmp1 = NULL;

  while (tmp != NULL) {
    ip = tmp->iconmgr;
    if ((tmp_win->occupation & ip->twm_win->occupation) != 0) {
	tmp1 = tmp;
	tmp  = tmp->nextv;
	continue;
    }
    RemoveFromIconManager(ip, tmp);
    
    XDeleteContext(dpy, tmp->icon, TwmContext);
    XDeleteContext(dpy, tmp->icon, ScreenContext);
    XDestroyWindow(dpy, tmp->icon);
    XDeleteContext(dpy, tmp->w, IconManagerContext);
    XDeleteContext(dpy, tmp->w, TwmContext);
    XDeleteContext(dpy, tmp->w, ScreenContext);
    XDestroyWindow(dpy, tmp->w);
    ip->count -= 1;

    PackIconManager(ip);

    if (ip->count == 0)
    {
	XUnmapWindow(dpy, ip->twm_win->frame);
	ip->twm_win->mapped = FALSE;
    }
    if (tmp1 == NULL) tmp_win->list = tmp_win->list->nextv;
    else tmp1->nextv = tmp->nextv;

    save = tmp;
    tmp = tmp->nextv;
    free((char *) save);
  }
}

void CurrentIconManagerEntry (current)
WList *current;
{
    Current = current;
}

void ActiveIconManager(active)
    WList *active;
{
    active->active = TRUE;
    Active = active;
    Active->iconmgr->active = active;
    Current = Active;
    DrawIconManagerBorder(active, False);
}

void NotActiveIconManager(active)
    WList *active;
{
    active->active = FALSE;
    DrawIconManagerBorder(active, False);
}

void DrawIconManagerBorder(tmp, fill)
    WList *tmp;
    int fill;
{
    if (Scr->use3Diconmanagers) {
	int shadow_width;

	shadow_width = 2;
	if (tmp->active && Scr->Highlight)
	    Draw3DBorder (tmp->w, 0, 0, tmp->width, tmp->height, shadow_width,
				tmp->cp, on, fill, False);
	else
	    Draw3DBorder (tmp->w, 0, 0, tmp->width, tmp->height, shadow_width,
				tmp->cp, off, fill, False);
    }
    else {
	XSetForeground(dpy, Scr->NormalGC, tmp->cp.fore);
	XDrawRectangle(dpy, tmp->w, Scr->NormalGC, 2, 2, tmp->width-5, tmp->height-5);

	if (tmp->active && Scr->Highlight)
	    XSetForeground(dpy, Scr->NormalGC, tmp->highlight);
	else
	    XSetForeground(dpy, Scr->NormalGC, tmp->cp.back);

        XDrawRectangle(dpy, tmp->w, Scr->NormalGC, 0, 0, tmp->width-1, tmp->height-1);
        XDrawRectangle(dpy, tmp->w, Scr->NormalGC, 1, 1, tmp->width-3, tmp->height-3);
    }
}

/***********************************************************************
 *
 *  Procedure:
 *	SortIconManager - sort the dude
 *
 *  Inputs:
 *	ip	- a pointer to the icon manager struture
 *
 ***********************************************************************
 */

void SortIconManager(ip)
    IconMgr *ip;
{
    WList *tmp1, *tmp2;
    int done;
    int (*compar)() = (Scr->CaseSensitive ? strcmp : XmuCompareISOLatin1);

    if (ip == NULL)
	ip = Active->iconmgr;

    done = FALSE;
    do
    {
	for (tmp1 = ip->first; tmp1 != NULL; tmp1 = tmp1->next)
	{
	    if ((tmp2 = tmp1->next) == NULL)
	    {
		done = TRUE;
		break;
	    }
	    if ((*compar)(tmp1->twm->icon_name, tmp2->twm->icon_name) > 0)
	    {
		/* take it out and put it back in */
		RemoveFromIconManager(ip, tmp2);
		InsertInIconManager(ip, tmp2, tmp2->twm);
		break;
	    }
	}
    }
    while (!done);
    PackIconManager(ip);
}

/***********************************************************************
 *
 *  Procedure:
 *	PackIconManager - pack the icon manager windows following
 *		an addition or deletion
 *
 *  Inputs:
 *	ip	- a pointer to the icon manager struture
 *
 ***********************************************************************
 */

void PackIconManager(ip)
    IconMgr *ip;
{
    int newwidth, i, row, col, maxcol,  colinc, rowinc, wheight, wwidth;
    int new_x, new_y;
    int savewidth;
    WList *tmp;
    int mask, bw;
    unsigned int JunkW, JunkH;

    wheight = Scr->IconManagerFont.height + 10;
    if (wheight < (siconify_height + 4))
	wheight = siconify_height + 4;

    wwidth = ip->width / ip->columns;

    rowinc = wheight;
    colinc = wwidth;

    row = 0;
    col = ip->columns;
    maxcol = 0;
    for (i = 0, tmp = ip->first; tmp != NULL; i++, tmp = tmp->next)
    {
	tmp->me = i;
	if (++col >= ip->columns)
	{
	    col = 0;
	    row += 1;
	}
	if (col > maxcol)
	    maxcol = col;

	new_x = col * colinc;
	new_y = (row-1) * rowinc;

	/* if the position or size has not changed, don't touch it */
	if (tmp->x != new_x || tmp->y != new_y ||
	    tmp->width != wwidth || tmp->height != wheight)
	{
	    XMoveResizeWindow(dpy, tmp->w, new_x, new_y, wwidth, wheight);

	    tmp->row = row-1;
	    tmp->col = col;
	    tmp->x = new_x;
	    tmp->y = new_y;
	    tmp->width = wwidth;
	    tmp->height = wheight;
	}
    }
    maxcol += 1;

    ip->cur_rows = row;
    ip->cur_columns = maxcol;
    ip->height = row * rowinc;
    if (ip->height == 0)
    	ip->height = rowinc;
    newwidth = maxcol * colinc;
    if (newwidth == 0)
	newwidth = colinc;

    XResizeWindow(dpy, ip->w, newwidth, ip->height);

    mask = XParseGeometry (ip->geometry, &JunkX, &JunkY, &JunkW, &JunkH);
    if (mask & XNegative) {
	ip->twm_win->frame_x += ip->twm_win->frame_width - newwidth -
				2 * ip->twm_win->frame_bw3D;
    }
    if (mask & YNegative) {
	ip->twm_win->frame_y += ip->twm_win->frame_height - ip->height -
				2 * ip->twm_win->frame_bw3D - ip->twm_win->title_height;
    }
    savewidth = ip->width;
    if (ip->twm_win)
      SetupWindow (ip->twm_win,
		   ip->twm_win->frame_x, ip->twm_win->frame_y,
		   newwidth + 2 * ip->twm_win->frame_bw3D,
		   ip->height + ip->twm_win->title_height + 2 * ip->twm_win->frame_bw3D, -1);
    ip->width = savewidth;
}

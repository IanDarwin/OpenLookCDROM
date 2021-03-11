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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/neos/RCS/papersaux.c,v 1.3 1992/12/15 21:56:26 rr2b R6tape $";
#endif


 
/*
 * papersaux.c
 *
 * This is the overflow from the papers.c module.
*/

/* ************************************************************
 *  Copyright (C) 1989, 1990, 1991
 *  by the Massachusetts Institute of Technology
 *  For full copyright information see:'mit-copyright.h'     *
 *************************************************************/

#include <mit-copyright.h>
#include <class.h>
#include <cursor.ih>
#include <eosbutt.ih>
#include <eos.h>
#include <fontdesc.ih>
#include <im.ih>
#include <lpair.ih>
#include <menulist.ih>
#include <newbttnv.ih>
#include <pshbttn.ih>
#include <scroll.ih>
#include <text.ih>
#include <textv.ih>
#include <view.ih>
#include <blank.ih>

/* sys/types.h in AIX PS2 defines "struct label",  causing a type name clash.
  Avoid this by temporarily redefining "label" to be something else. */
#define label gezornenplatz
#include <eosfx.ih> /* eosfx.ih includes sys/types.h */
#undef label

#include <label.ih>
#include <labelv.ih>

#define AUXMODULE 1
#include <papers.eh>   /* papers.eh uses struct label */

extern	void papers__SetTitle();
extern	void SetSortOrder();
extern	void MakePaper();
extern	void papers_Edit();
extern	void papers_Keep();
extern	void papers_Hide();
extern	void papers_Display();
extern	void papers_GradingToggleAndList();

extern	struct menulist *papers_global_menus;

static char *buttonNames[]=
{ "EDIT", "KEEP", "HIDE", NULL };
static void (*buttonFuncs[])()=
{  papers_Edit, papers_Keep, papers_Hide, NULL };

static char *subdispbuttons[]=
{ "SUBMIT", "DISPLAY", "EDIT", "KEEP", "HIDE", NULL };
static void (*subdispFuncs[])()=
{ MakePaper, papers_Display, papers_Edit, papers_Keep, papers_Hide, NULL };

static char *altbuttons[]=
{ "OLD DOCS", "DISPLAY", "EDIT", "KEEP", "HIDE", NULL };
static void (*altFuncs[])()=
{ papers_GradingToggleAndList, papers_Display, papers_Edit, papers_Keep, papers_Hide, NULL };

boolean papers__InitializeObject(classID, self)
struct classheader *classID;
struct papers *self;
/* This is getting a bit messy... */
/* Possible configurations of view:
    papers[screen[main[titleV, scroll[textv]], buttons]]
    papers[screen[main[titleV, scroll[textv]], altbuttons]]
    papers[submitscreen[main[titleV, scroll[textv]], xtrabuttons]]
 */
 /* A nicer way of doing this would be to abstract all the junk into a main papers object and then subclass the different views. The current code is blech 
   -njw 6/19/90 */
{
    int i, numbuttons;
    struct lplist     *lplist;
    struct buttonList *blist;
    struct lplist     *altlplist;
    struct buttonList *altblist;
    struct lplist     *sd_lplist;
    struct buttonList *sd_blist;
    struct buttonList *miscbuttons;
    struct lpair      *tmpbuts;
    struct blank      *blank;

    lplist    = NULL;
    blist     = NULL;
    altlplist = NULL;
    altblist  = NULL;
    sd_lplist = NULL;
    sd_blist  = NULL;

    self->list         = NULL;
    self->Positions    = NULL;
    self->wantbuttons  = papers_PLAIN;
    self->thiswindow   = papersNotKnown;
    self->daddy        = NULL;
    self->course       = (char *) malloc(33);
    if (!self->course) return FALSE;
    strcpy(self->course, "no course");
    self->toggled      = papers_OLD;
    self->IDoNotExist  = FALSE;
    self->assignment = 0;
    self->student = NULL;

    /* Create and initialise the text */
    self->textv = textview_New();
    self->textobj = text_New();
    if (!self->textv | !self->textobj)
	return FALSE;
    textview_SetDataObject(self->textv, self->textobj);
    self->scroll = scroll_Create(self->textv, scroll_LEFT);
    if (!self->scroll)
	return FALSE;

    /* Create & initialise the title bar */
    self->title = label_New();
    self->titleV = labelview_New();
    labelview_SetDataObject(self->titleV, self->title);
    label_SetFlags(self->title, label_CENTERED);
    papers_SetTitle(self, "Available papers");
    label_SetFont(self->title, "helvetica", fontdesc_Plain, 24);

    /* Yucky code follows! Make the sets of buttons */
    for ( i=0 ; buttonNames[i] != NULL ; i++ ) {
	blist = eosbutton_MakeButton(blist, buttonNames[i], buttonFuncs[i], (struct view *) self);
	if (buttonNames[i+1] != NULL) lplist = eosbutton_MakeLpair(lplist);
    } /* for - initializing buttons */

    for ( i=0 ; altbuttons[i] != NULL ; i++ ) {
	altblist = eosbutton_MakeButton(altblist, altbuttons[i], altFuncs[i], (struct view *) self);
	if (!strcmp(altbuttons[i], "OLD DOCS"))
	    self->toggle = altblist->buttv;
	if (altbuttons[i+1] != NULL) altlplist = eosbutton_MakeLpair(altlplist);
    } /* for - initializing buttons */

    for ( i=0 ; subdispbuttons[i] != NULL ; i++ ) {
	sd_blist = eosbutton_MakeButton(sd_blist, subdispbuttons[i], subdispFuncs[i], (struct view *) self);
	if (subdispbuttons[i+1] != NULL) sd_lplist = eosbutton_MakeLpair(sd_lplist);
    } /* for - initializing buttons */

    /*
      * The below lines glue the buttons into their lpairs, using the
      * linked lists blist and lplist
      * The lpairs are split so as each button is the same size
      * Making all buttons the same size is done by the code in the while loop, making
	  * the n'th button be 1/n fraction of the size of the panel. 
  * i.e. The lpair_HSplit splits it 1/(n+1) to button and n/(n+1) to the previous lpair.
  * The entire panel is finally placed into self->buttons
  */

    lpair_VSplit(lplist->lp, blist->next->buttv, blist->buttv, 50, FALSE);
    lpair_VSplit(altlplist->lp, altblist->next->buttv, altblist->buttv, 50, FALSE);
    lpair_VSplit(sd_lplist->lp, sd_blist->next->buttv, sd_blist->buttv, 50, FALSE);

    blist  = blist->next->next;
    altblist = altblist->next->next;
    sd_blist = sd_blist->next->next;
    i = 2;
    while (altblist != NULL) {
	lpair_VSplit(altlplist->next->lp, altblist->buttv, altlplist->lp, (long int) 100*i/(i+1), FALSE);
	altlplist = altlplist->next;
	altblist  = altblist->next;
	i++;
    } 
    i = 2;
    while (sd_blist != NULL) {
	lpair_VSplit(sd_lplist->next->lp, sd_blist->buttv, sd_lplist->lp, (long int) 100*i/(i+1), FALSE);
	sd_lplist = sd_lplist->next;
	sd_blist  = sd_blist->next;
	i++;
    } 
    i = 2;
    while (blist != NULL) {
	lpair_VSplit(lplist->next->lp, blist->buttv, lplist->lp, (long int) 100*i/(i+1), FALSE);
	lplist = lplist->next;
	blist  = blist->next;
	i++;
    } 
    numbuttons = i;

    self->buttons = lplist->lp;
/*    self->buttons = lplist->lp; */


    miscbuttons = NULL;
    miscbuttons = eosbutton_MakeButton(miscbuttons, "SUBMIT", MakePaper, (struct view *) self);

    self->xtrabuttons = lpair_New();
    self->xtrabuttons = lpair_VSplit(self->xtrabuttons, miscbuttons->buttv, self->buttons, (long int) 100*numbuttons/(numbuttons+1), FALSE);

    /* There are several different views possible:
      * So, the lpairs below are all the different configurations
      */
    self->main         = lpair_New();
    self->submitscreen = lpair_New();
    self->screen       = lpair_New();

    /* HTFixed is used so that resizes keep the buttons a constant size. */
    /* The blank is a simple view used to pad out the space for the buttons. */
    /* It just redraws itself with blank space */
    blank = blank_New();
    tmpbuts = lpair_New();
    lpair_VTFixed(self->main, self->titleV, self->scroll, 30, FALSE);
    lpair_VTFixed(tmpbuts, self->buttons, blank, 150, FALSE);
    lpair_HFixed(self->screen, self->main, tmpbuts, 100, FALSE);

    tmpbuts = lpair_New();
    blank = blank_New();

    lpair_VTFixed(tmpbuts, self->xtrabuttons, blank, 200, FALSE);
    lpair_HFixed(self->submitscreen, self->main, tmpbuts, 100, FALSE);

    tmpbuts = lpair_New();
    blank = blank_New();

    self->altbuttons = lpair_New();
    lpair_VTFixed(self->altbuttons, altlplist->lp, blank, 230, FALSE);

    tmpbuts = lpair_New();
    blank = blank_New();

    self->subdispbuttons = lpair_New();
    lpair_VTFixed(self->subdispbuttons, sd_lplist->lp, blank, 230, FALSE);

    /* we don't want the user altering the lpair boundaries... */
    lpair_SetMovable(self->main, 0);
    lpair_SetMovable(self->screen, 0);
    lpair_SetMovable(self->submitscreen, 0);

    self->menuflags = MENUS_general;
    self->menus = menulist_DuplicateML(papers_global_menus, self);

    SetSortOrder();

    self->display = self->screen;
    self->maincursor = cursor_Create(self->textv);
    cursor_SetStandard(self->maincursor, Cursor_LeftPointer);
    return TRUE;
}


void papers__SetTitle(self, title)
struct papers *self;
char *title;
/* Set the text of the title bar. If course-in-title is TRUE, then add the name
   of the course into the text */
{
    char string[80];
    strcpy(string, title);
    if (environ_GetProfileSwitch("course-in-title", TRUE)) {
	strcat(string, ": ");
	strcat(string, self->course);
    }
    label_SetText(self->title, string);
}

void papers__SetDisplay(self, displaytype, wintype)
struct papers *self;
enum papers_DisplayType displaytype;
enum papers_Types wintype;
/* Papers can take on different aspects - mainly, it can show different
   sets of buttons at the side. This routine decides which buttons to use.
 */
{
    self->thiswindow  = wintype;
    self->wantbuttons = displaytype;

    if (wintype == papersExchange) self->menuflags |= MENUS_instructor;

    switch (displaytype) {
	case papers_SIDE:
	    self->display = self->screen;
	    break;
	case papers_ALTSIDE:
	    lpair_SetNth(self->screen, 1, self->altbuttons);
	    self->display = self->screen;
	    break;
	case papers_SIDESUBMIT:
	    self->display = self->submitscreen;
	    break;
	case papers_SIDESUBDISP:
	    lpair_SetNth(self->screen, 1, self->subdispbuttons);
	    self->display = self->screen;
	    break;
	default:
	    self->display = self->main;
    }

    lpair_LinkTree(self->display, self);
    papers_WantUpdate(self, self);
    im_ForceUpdate();
}

void papers__FinalizeObject(classID, self)
struct classheader *classID;
register struct papers *self;
{
    /* does nothing for now */
}

void papers__LinkTree(self, parent)
struct papers *self;
struct view *parent;
{
    super_LinkTree(self, parent);

    if (self->display)
	lpair_LinkTree(self->display, self);
}

void papers__FullUpdate(self, type, left, top, width, height)
register struct papers *self;
enum view_UpdateType type;
long left, top, width, height;
{
    struct rectangle childRect, crect;

    /* Ensure we exist! */
    if (self->IDoNotExist) self->IDoNotExist = FALSE;

    /* We want to replace the 'papers' view with self->display,
      which is the actual view wanted, so insert self->display into
      the logical area of self
      */
    papers_GetLogicalBounds(self, &childRect);
    textview_GetLogicalBounds(self->textv, &crect);
    textview_PostCursor(self->textv, &crect, self->maincursor);
    lpair_InsertView(self->display, self, &childRect);
    lpair_FullUpdate(self->display, type, left, top, width, height);
}

struct view *papers__Hit(self, action, x, y, clicks)
struct papers *self;
enum view_MouseAction action;
long x, y, clicks;
/*
  If the hit occurred within the textview, then we want to process it
  ourselves, otherwise, just pass it into the lpairs to let them deal with it...
  Also, if we notice the list of papers is empty, we don't even
      bother with
      our own processing when the mouse is in the textview
      this code is based on the code in the captions object, used by andrew messages
      */
{
    struct rectangle clickrect, labelrect;
    struct point *mouse;
    long textx, texty;

    mouse = point_CreatePoint(x, y);
    textview_GetEnclosedBounds(self->textv, &clickrect);
    labelview_GetEnclosedBounds(self->titleV, &labelrect);
    clickrect.top += labelrect.height;
    /* Translate the (x,y) into local coordinates for the textview */
    textx = x - clickrect.left + textview_GetLogicalLeft(self->textv);
    texty = y - clickrect.top  + scroll_GetLogicalTop(self->scroll);

    if (rectangle_IsPtInRect(mouse, &clickrect) && self->list != NULL) {
	long thisdot;

	switch (action) {
	    case view_LeftDown:
	    case view_RightDown:
		textview_Hit(self->textv, view_LeftDown, textx, texty, 1);
		self->downdot = textview_GetDotPosition(self->textv);
		break;
	    case view_LeftUp:
		textview_Hit(self->textv, view_LeftUp, textx, texty, 1);
		SimulateClick(self, TRUE);
		break;
	    case view_RightUp:
		textview_Hit(self->textv, view_LeftUp, textx, texty, 1);
		thisdot = textview_GetDotPosition(self->textv);
		if (thisdot == self->downdot)
		    thisdot += textview_GetDotLength(self->textv);
		if (thisdot != self->downdot) {
		    /* We have a range! */
		    struct paperPositions *start, *end;

		    start = eosfx_LocatePaper(self->Positions, self->downdot, NULL);
		    end   = eosfx_LocatePaper(self->Positions, thisdot, NULL);
		    if (start != end) {
			/* Yep, it's confirmed - many papers to select */
			struct paperPositions *node, *stopnode;
			if (end->textpos > start->textpos) {
			    node = end;
			    stopnode = start;
			} else {
			    node = start;
			    stopnode = end;
			}
			while (node != stopnode) {
			    if (!(node->flags & eos_PaperMarked) && !(node->flags & eos_PaperDeleted)) ToggleMark(self, node);
			    node = node->next;
			}
			textview_SetDotPosition(self->textv, thisdot);
			textview_SetDotLength(self->textv, 0);
		    } else SimulateClick(self, FALSE);
		} else SimulateClick(self, FALSE);
		break;
	    case view_LeftMovement:
	    case view_RightMovement:
		textview_Hit(self->textv, view_LeftMovement, textx, texty, 1);
		break;
	}
	return (struct view *) self;
    } else 
	return lpair_Hit(self->display, action, x, y, clicks);

}

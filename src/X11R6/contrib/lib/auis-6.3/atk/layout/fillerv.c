/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */

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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/layout/RCS/fillerv.c,v 1.9 1993/05/04 01:22:34 susan Exp $";
#endif

/* $ACIS$ */

 

#define fillerview_MINIMUMSIZE 100
#define LEFTMARGIN 5
#define TOPMARGIN 5

#define viewnamestring(v) ((v) == NULL ? "<NO VIEW>" : atom_Name(atomlist_First(view_GetName(v))))

#ifndef _IBMR2
extern char *malloc();
extern char *realloc();
#endif /* _IBMR2 */

#include <class.h>
#include <assert.h>

#include <atomlist.ih>
#include <dataobj.ih>
#include <environ.ih>
#include <fontdesc.ih>
#include <menulist.ih>
#include <message.ih>
#include <proctbl.ih>
#include <rect.h>
#include <view.ih>

#include <filler.ih>

#include <fillerv.eh>

static boolean debug=FALSE;
/* graphic information */

struct graphicstuff {
    struct fontdesc *writingFont;
    int lineheight;
    int linewidth;
};

#define DEBUGPRINTF(x)

/* list of possible replacement objects */

static char **Insets;

/* initialize entire class */

static struct menulist *mainmenus = (struct menulist *) NULL;
static struct proctable_Entry *replaceProc = (struct proctable_Entry *) NULL;

/* add an inset to the list of candidates */

static void
addInset(newposs)
char *newposs;
{
    int n;
    char *b;

    for (n = 0; Insets[n] != NULL; n++)
	if (strcmp(Insets[n], newposs) == 0)
	    return;
    b = malloc(strlen(newposs)+1);
    assert(b != NULL);
    strcpy(b, newposs);
    Insets = (char **)realloc((char *)Insets, (n+2)*sizeof (char *));
    assert(Insets != NULL);
    Insets[n] = b;
    Insets[n+1] = NULL;
}

static void
initializeInsets()
{
    char buff[81];
    char *insetlist;
    char *c, *d;
    int i;

    Insets = (char **)malloc(sizeof (char *));
    assert(Insets != NULL);
    Insets[0] = NULL;

    insetlist = environ_GetProfile("FillerInsets");
    if (insetlist == NULL || *insetlist == '\0')
	insetlist = "Ask...:box:text:raster:table:zip:fad:pshbttn:clock:timeoday:link";
    for (c = insetlist, d = buff; *c != '\0'; c++) {
	if (*c == ':') {
	    assert(d-buff < sizeof buff);
	    *d++ = '\0';
	    addInset(buff);
	    d = buff;
	}
	else if (*c == ' ')
	    ;
	else
	    *d++ = *c;
    }
    assert(d-buff < sizeof buff);
    *d++ = '\0';
    addInset(buff);

    /* do menus in opposite order so they appear in right order on card */

    for (i = 0; Insets[i] != NULL; i++) ;
    while (--i >= 0) {
	sprintf(buff, "Insets,%s", Insets[i]);
	assert(strlen(buff) < sizeof buff);
	menulist_AddToML(mainmenus, buff, replaceProc, Insets[i], 0);
    }
}

/* initialize entire class */

boolean				    /* returns TRUE for success */
fillerview__InitializeClass(classID)
struct classheader *classID;	    /* ignored */
{
    DEBUGPRINTF(("fillerview__InitializeClass(%x)\n", classID));

    mainmenus = menulist_New();

    replaceProc = proctable_DefineProc("fillerview-set-data-object-by-name", fillerview__SetDataObjectByName, &fillerview_classinfo, NULL, "Insert named inset");
    initializeInsets();

    return TRUE;
}

/* initialize filler view */

boolean				    /* retrurns TRUE for success */
fillerview__InitializeObject(classID, self)
struct classheader *classID;	    /* ignored */
struct fillerview *self;
{
    DEBUGPRINTF(("fillerview__InitializeObject(%x)\n", classID));

    self->hasInputFocus = FALSE;
    self->menulist = menulist_DuplicateML(mainmenus, self);
    self->hitindex = -1;
    fillerview_WantInputFocus(self, self);
    return TRUE;
}

/* initialize graphic-dependent data */

static void
InitializeGraphics(self, gc)
struct fillerview *self;
struct graphicstuff *gc;
{
    static char *wfontname = NULL;
    struct FontSummary *fs;
    int i;
    int w;

    DEBUGPRINTF(("fillerview_InitializeGraphics\n"));

    if (getDrawable(self) == NULL) {
	printf("InitializeGraphics called without drawable.\n");
	return;
    }
    if (wfontname == NULL) {
	wfontname = environ_GetProfile("bodyfontfamily");
	if (wfontname == NULL || !*wfontname) wfontname = "andy";
    }
    gc->writingFont = fontdesc_Create(wfontname, fontdesc_Plain, 12);
    fs = fontdesc_FontSummary(gc->writingFont, getDrawable(self));
    gc->lineheight = fs->maxHeight;
    gc->linewidth = 0;
    for (i = 0; Insets[i] != NULL; i++) {
	fontdesc_StringSize(gc->writingFont, getDrawable(self), Insets[i], &w, NULL);
	if (w > gc->linewidth)
	    gc->linewidth = w;
    }
}

/* compute top of line hit */

static int			/* returns y position of top of button */
TopOfTheMark(self, gc, i)
struct fillerview *self;
struct graphicstuff *gc;
int i;				/* index of this button */
{
    return TOPMARGIN * 2 - 2 + (i + 1) * gc->lineheight;
}

/* highlight hit box */

static void
showhit(self, gc)
struct fillerview *self;
struct graphicstuff *gc;
{
    short savetransfermode;

    if (self->hitindex >= 0) {
	savetransfermode = fillerview_GetTransferMode(self);
	fillerview_SetTransferMode(self, graphic_INVERT);
	fillerview_DrawRectSize(self, LEFTMARGIN, TopOfTheMark(self, gc, self->hitindex), gc->linewidth + 3 * LEFTMARGIN, gc->lineheight);
	fillerview_SetTransferMode(self, savetransfermode);
    }
}

/* process mouse hit */

struct view *				/* returns view to get subsequent hits */
fillerview__Hit(self, action, x, y, numberOfClicks)
struct fillerview *self;
enum view_MouseAction action;		/* button and what it did */
long x,	y;				/* coordinates of mouse */
long numberOfClicks;			/* number of clicks at this location */
{
    int i;
    struct graphicstuff realgc, *gc = &realgc;

    DEBUGPRINTF(("fillerview_Hit(%d, %ld, %ld, %ld)\n", (int) action, x, y, numberOfClicks));

    if (fillerview_GetTrueChild(self) != NULL)
	return super_Hit(self, action, x, y, numberOfClicks);

    InitializeGraphics(self, gc);

    if (self->hitindex >= 0) {
	showhit(self, gc);  /* turns it off */
	self->hitindex = -1;
    }

    if (y >= TopOfTheMark(self, gc, 0) && x >= 0 && x < gc->linewidth + 3 * LEFTMARGIN) {
	for (i = 0; Insets[i] != NULL; i++) {
	    if (y < TopOfTheMark(self, gc, i + 1)) {
		self->hitindex = i;
		break;
	    }
	}
    }

    if (self->hitindex >= 0 && (action == view_LeftDown || action == view_LeftMovement || action == view_RightDown || action == view_RightMovement)) {
	showhit(self, gc);
    }

    else if (self->hitindex >= 0 && (action == view_LeftUp || action == view_RightUp)) {
	showhit(self, gc);
	fillerview_SetDataObjectByName(self, Insets[self->hitindex]);
	return NULL;
    }

    else
	self->hitindex = -1;

    if (!self->hasInputFocus)
	fillerview_WantInputFocus(self, &getView(self));

    return &getView(self);
}

/* update all components */

static void
UpdateScreen(self, how, updateRect)
struct fillerview *self;
enum view_UpdateType how;		    /* kind of update */
struct rectangle *updateRect;		    /* rectangle affected */
{
    int i;
    struct graphicstuff realgc, *gc = &realgc;

    InitializeGraphics(self, gc);

    fillerview_SetTransferMode(self, graphic_COPY);
    fillerview_SetClippingRect(self, updateRect);
    fillerview_EraseRect(self, updateRect);
    fillerview_MoveTo(self, LEFTMARGIN, TOPMARGIN);
    fillerview_DrawString(self, "Select an inset", graphic_ATTOP | graphic_ATLEFT);
    for (i = 0; Insets[i] != NULL; i++) {
	fillerview_MoveTo(self, LEFTMARGIN * 2, TOPMARGIN * 2 + (i + 1) * gc->lineheight);
	fillerview_DrawString(self, Insets[i], graphic_ATTOP | graphic_ATLEFT);
    }
    showhit(self, gc);
}

/* full update when window changes */

void
fillerview__FullUpdate(self, how, left, top, width, height)
struct fillerview *self;
enum view_UpdateType how;		    /* kind of update */
long left, top, width, height;		    /* rectangle affected (in some cases; */
{
    struct rectangle vb;

    DEBUGPRINTF(("fillerview_FullUpdate(%d, %d, %d, %d, %d)\n", (int)how, left, top, width, height));

    super_FullUpdate(self, how, left, top, width, height);
    if (fillerview_GetTrueChild(self) != NULL)
	return;

    fillerview_GetVisualBounds(self, &vb);

    switch(how) {

	case view_MoveNoRedraw:
	case view_Remove:
	    break;

	case view_PartialRedraw:
	case view_LastPartialRedraw:
	    rectangle_SetRectSize(&vb, left, top, width, height);
	    /* fall through into default case */
	default:
	    UpdateScreen(self, how, &vb);
	    break;
    }
}

/* partial update */

void
fillerview__Update(self)
struct fillerview *self;
{
    struct rectangle visualRect;

    DEBUGPRINTF(("fillerview_Update\n"));

    super_Update(self);
    if (fillerview_GetTrueChild(self) != NULL)
	return;

    fillerview_GetVisualBounds(self, &visualRect);
    UpdateScreen(self, -1, &visualRect);
}

/* input focus obtained; highlight something */

void
fillerview__ReceiveInputFocus(self)
struct fillerview *self;
{
    DEBUGPRINTF(("fillerview_ReceiveInputFocus\n"));

    super_ReceiveInputFocus(self);
    if (fillerview_GetTrueChild(self) != NULL)
	return;

    if (!(self->hasInputFocus)) {
	self->hasInputFocus = 1;
	fillerview_PostMenus(self, self->menulist);
    }
}

/* input focus lost; remove highlighting */

void
fillerview__LoseInputFocus(self)
struct fillerview *self;
{
    DEBUGPRINTF(("fillerview_LoseInputFocus\n"));

    super_LoseInputFocus(self);
    if (fillerview_GetTrueChild(self) != NULL)
	return;

    if (self->hasInputFocus) {
	self->hasInputFocus = 0;
    }
}

/* tear down a fillerview */

void
fillerview__FinalizeObject(classID, self)
struct classheader *classID;
struct fillerview *self;
{
    if (debug)
	printf("fillerview_FinalizeObject\n");

    menulist_Destroy(self->menulist);
}

/* set contained data object */

void
fillerview__SetDataObjectByName(self, dataname)
struct fillerview *self;
char *dataname;				/*class  dataname of replacement dataobject */
{
    DEBUGPRINTF(("fillerview_SetDataObjectByName(,%s)\n", dataname));

    if (strncmp(dataname, "Ask", 3) == 0) {
	char newname[100];

	newname[0] = '\0';
	if (message_AskForString(&getView(self), 0, "Data object to enter here (text): ", "", newname, sizeof newname))
	    return;
	if (newname[0] == 0)
	    dataname = "text";
	else
	    dataname = newname;
    }
    filler_SetObjectByName(MyFiller(self), dataname);
    filler_SetViewName(MyFiller(self), NULL, TRUE);
    filler_SetRefName(MyFiller(self), dataname);
    ((struct celview *) self)->arb = NULL;
    fillerview_ObservedChanged(self, MyFiller(self), observable_OBJECTCHANGED);
    fillerview_WantInputFocus(self, self);
}

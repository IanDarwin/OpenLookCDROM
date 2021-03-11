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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/layout/RCS/layoutv.c,v 1.12 1994/04/17 19:33:20 rr2b Exp $";
#endif

/* $ACIS$ */

 

#define layoutview_MINIMUMSIZE 100
#define CHILD_MENULIST_KEY 999

#define viewname(v) ((v) == NULL ? "<NO VIEW>" : atom_Name(atomlist_First(view_GetName(v))))

#ifndef _IBMR2
extern char *malloc();
#endif /* _IBMR2 */

#include <class.h>
#include <rect.h>
#include <assert.h>

#include <atomlist.ih>
#include <bind.ih>
#include <cel.ih>
#include <cursor.ih>
#include <dataobj.ih>
#include <environ.ih>
#include <filetype.ih>
#include <im.ih>
#include <keymap.ih>
#include <keystate.ih>
#include <menulist.ih>
#include <message.ih>
#include <region.ih>
#include <txttroff.ih>
#include <view.ih>

#include <layout.ih>
#include <layoutv.eh>

static char layout_debug=0;

#define	POINTINGTHRESHHOLD 6	    /* max distance away from object pointed at */
#define	MOTIONTHRESHHOLD 10	    /* min distance for dragging */

/* menu mode mask */

#define layoutview_EXEC_MASK 1
#define layoutview_AUTHOR_MASK 2

#define layoutview_SELECTION_MASK 4

#define layoutview_VARIABLE_MASK 8
#define layoutview_FIXED_MASK 16

/* static data for entire class */

static struct keymap *mainmap = (struct keymap *) NULL;
static struct menulist *mainmenus = (struct menulist *) NULL;
static struct bind_Description layoutview_bindings[] = {

    {"layout-run", "\033x", 0, "Exit Authoring~60", 0, layoutview_AUTHOR_MASK, layoutview__SetRunMode, "Enter execution mode"},
    {"layout-author", NULL, 0, "Enter Authoring~60", 0, layoutview_EXEC_MASK, layoutview__SetAuthoringMode, "Enter authoring mode"},

    {"layout-toggle-debug", "\033\033", 0, NULL, 0, 0, layoutview__ToggleDebug, "Toggle Debug"},
    {"layout-paste-object", "\031", 0, "Authoring~1,Paste object~20", 0, layoutview_SELECTION_MASK, layoutview__Paste, "Paste into current selection"},
    {"layout-imbed-object", "\033\t", 0, "Authoring~1,Imbed object~21", 0, layoutview_SELECTION_MASK, layoutview__SetChild, "Prompt for object and place in current selection"},
    {"layout-destroy-object", "\004", 0, "Authoring~1,Cut object~22", 0, layoutview_SELECTION_MASK, layoutview__DestroyComponent, "Destroy current selection"},

    {"layout-move-to-back", "\033d", 0, "Authoring~1,Move to back~30", 0, layoutview_SELECTION_MASK, layoutview__Demote, "Move current selection to back"},
    {"layout-background", "\033b", 0, "Authoring~1,Background~31", 0, layoutview_SELECTION_MASK, layoutview__MakeBackground, "Expand current selection and move to back"},
    {"layout-make-variable", "\033v", 0, "Authoring~1,Make variable~35", 0, layoutview_FIXED_MASK, layoutview__MakeVariable, "Allow component to vary"},
    {"layout-make-fixed", "\033f", 0, "Authoring~1,Make fixed~35", 0, layoutview_VARIABLE_MASK, layoutview__MakeFixed, "Component varies only in authoring mode"},

    {"layout-create-null", NULL, 0, "Authoring~1,Null on create~40", CREATE_NULL, layoutview_AUTHOR_MASK, layoutview__SetCreateMode, "Do not fill in newly created data objects"},
    {"layout-create-filler", NULL, 0, "Authoring~1,Filler on create~41", CREATE_FILLER, layoutview_AUTHOR_MASK, layoutview__SetCreateMode, "Use filler for newly created data objects"},
    {"layout-create-paste", NULL, 0, "Authoring~1,Paste on create~42", CREATE_PASTE, layoutview_AUTHOR_MASK, layoutview__SetCreateMode, "Paste into newly created data objects"},

    {"layout-fine-granularity", NULL, 0, "Authoring~1,Fine granularity~50", 0, layoutview_AUTHOR_MASK, layoutview__SetGranularity, "Fine (1 pixel) granularity of object placement"},
    {"layout-medium-granularity", NULL, 0, "Authoring~1,Medium granularity~51", 6, layoutview_AUTHOR_MASK, layoutview__SetGranularity, "Medium (6 pixel) granularity of object placement"},
    {"layout-coarse-granularity", NULL, 0, "Authoring~1,Coarse granularity~52", 18, layoutview_AUTHOR_MASK, layoutview__SetGranularity, "Coarse (18 pixel) granularity of object placement"},

    {NULL, NULL, 0, NULL, 0, NULL, NULL}
};

/* initialize static data shared by all instances of layoutview */

boolean					/* always returns TRUE */
layoutview__InitializeClass(classID)
struct classheader *classID;		/* unused */
{

    if (layout_debug)
	printf("layoutview_InitializeClass(%x)\n", classID);

    mainmap = keymap_New();
    mainmenus = menulist_New();
    bind_BindList(layoutview_bindings, mainmap, mainmenus, &layoutview_classinfo);

    return TRUE;
}

/* initialize layout view */

boolean					/* always returns TRUE */
layoutview__InitializeObject(classID, self)
struct classheader *classID;		/* unused */
struct layoutview *self;
{
    if (layout_debug)
	printf("layoutview_InitializeObject(%x)\n", classID);

    self->hasInputFocus = FALSE;
    self->updateRequested = FALSE;
    self->lastUpdate = 0;
    self->menulist = menulist_DuplicateML(mainmenus, self);
    self->keystate = keystate_Create(self, mainmap);
    self->authoringCursor = NULL;
    self->subviews = NULL;
    self->selection = NULL;
    self->hitmode = HIT_EXEC;
    self->createmode = CREATE_FILLER;
    self->granularity = 0;
    menulist_SetMask(self->menulist, layoutview_EXEC_MASK);
    return TRUE;
}

/* initialize graphic-dependent data */

static void 
InitializeGraphic(self)
struct layoutview *self;
{
    if (layout_debug)
	printf("layoutview_InitializeGraphic\n");

    if (layoutview_GetDrawable(self) == NULL) {
	printf("InitializeGraphic called without drawable.\n");
	return;
    }
    self->authoringCursor = cursor_Create(self);
    cursor_SetStandard(self->authoringCursor, Cursor_SmallCross);
}

/* get printable class name of an object */

static char *
GetClassName(self, object)
struct layoutview *self;
struct dataobject *object;
{
    char *result;

    if (object == NULL)
	result = "<NO OBJECT>";
    else {
	result = class_GetTypeName(object);
	if (strcmp(result, "filler") == 0 || strcmp(result, "cel") == 0)
	    result = cel_GetObjectName((struct cel *)object);
    }

    return result;
}

/* find or create new viewlist entry */

struct view *		    /* returns view, or NULL if error */
layoutview__FindSubview(self, c)
struct layoutview *self;
struct component *c;		    /* component for which view is needed */
{
    struct viewlist *vl;
    char *subviewname;			/* name for new view */

    if (c == NULL)
	return NULL;
    if (cData(c) == NULL)		/* can not create view for non-object */
	return NULL;

    forallsubviews(self, vl)
	if (vComponent(vl) == c)
	    return vChild(vl);		/* it already exists, so return its child view */

    /* construct new viewlist entry */

    vl = (struct viewlist *) malloc(sizeof *vl);
    if (vl == NULL) {
	printf("out of space for subview list\n");
	return NULL;
    }
    subviewname = dataobject_ViewName(cData(c));
    vl->component = c;
    vl->child = (struct view *) class_NewObject(subviewname);
    if (vl->child == NULL) {
	printf("unable to create child view\n");
	free(vl);
	return NULL;
    }
    view_SetDataObject(vChild(vl), cData(c));
    view_LinkTree(vChild(vl), &getView(self));
    view_AddObserver(vChild(vl), self);

    if (layout_debug)
	printf(".. created subview %s\n", subviewname); 
    vl->nextview = self->subviews;
    self->subviews = vl;

    return vChild(vl);
}

/* remove viewlist entry */

struct component *
layoutview__RemoveSubview(self, child)
struct layoutview *self;
struct view *child;
{
    struct viewlist *vl, *uvl;
    struct component *c;

    uvl = NULL;
    if (self->subviews != NULL && child == vChild(self->subviews)) {
	uvl = self->subviews;
	self->subviews = uvl->nextview;
    }
    else {
	forallsubviews(self, vl) {
	    if (vl->nextview != NULL && child == vChild(vl->nextview)) {
		uvl = vl->nextview;
		vl->nextview = uvl->nextview;
		break;
	    }
	}
    }
    if (uvl != NULL) {
	if (layout_debug)
	    printf(".. removed subview %s\n", viewname(child)); 
	c = vComponent(uvl);
	free((char *)uvl);
    }
    else
	c = NULL;
    view_RemoveObserver(child, self);
    return c;
}

/* replace contents of a component */

void
layoutview__ReplaceComponent(self, c, dataname)
struct layoutview *self;
struct component *c;
char *dataname;
{
    struct view *child;
    char foo[81];
    struct viewlist *vl;

    if(! class_IsTypeByName(dataname, "dataobject")){
        sprintf(foo, "%s is not a dataobject", dataname);
	message_DisplayString(&getView(self), 0, foo);
        return;
    }

    child = NULL;
    forallsubviews(self, vl)
      if (vComponent(vl) == layoutview_Selection(self)) {
	  child = vChild(vl);
	  break;
      }
    if (child != NULL) {
	layoutview_RemoveSubview(self, child); /* prevents removing component in view_Destroy */
	view_UnlinkTree(child);
	view_Destroy(child);
    }

    layout_FillInComponent(getLayout(self), dataname, c);
}

/* Init new component */

static void
InitComponent(self)
struct layoutview *self;
{
    struct view *child;
    child = layoutview_FindSubview(self, layoutview_Selection(self));
    if (child != NULL)
	view_WantInputFocus(child, child);
}

/* negotiate size of view */

enum view_DSattributes			/* returns indication of what it wants */
layoutview__DesiredSize(self, width, height, pass, dWidth, dHeight)
struct layoutview *self;
long width;				/* width being offered by parent */
long height;				/* height being offered */
enum view_DSpass pass;			/* what parent is willing to give */
long *dWidth;				/* set to desired width */
long *dHeight;				/* set to desired height */

/*  layoutview asks for just enough space to display all contained components */

{
    struct component *c;
    long desiredWidth;
    long desiredHeight;

    if (layout_debug)
	printf("layoutview_DesiredSize(, %d, %d, %d, .. )\n", width, height, (int)pass);

    desiredWidth = desiredHeight = layoutview_MINIMUMSIZE;
    forallcomponents(getLayout(self), c) {
	if (cWidth(c) > 0 && desiredWidth < cRight(c))
	    desiredWidth = cRight(c);
	if (cHeight(c) > 0 && desiredHeight < cBottom(c))
	    desiredHeight = cBottom(c);
    }

    *dWidth = (pass == view_WidthSet) ? width : desiredWidth;
    *dHeight = (pass == view_HeightSet) ? height : desiredHeight;

    return (enum view_DSattributes)
      ((int)((*dWidth > desiredWidth) ? view_WidthFlexible : view_WidthLarger)
      | (int)((*dHeight > desiredHeight) ? view_HeightFlexible : view_HeightLarger));
}

/* draw rubber-band box */

static void
DrawRubberBox(self)
struct layoutview *self;
{
    short savetransfermode;

    savetransfermode = layoutview_GetTransferMode(self);
    layoutview_SetTransferMode(self, graphic_INVERT);

    layoutview_DrawRectSize(self, self->rubberleft - 1, self->rubbertop - 1, self->rubberwidth + 1, self->rubberheight + 1);
 
    layoutview_SetTransferMode(self, savetransfermode);
}

/* update image */

#define ReallyDrawing(how, updateRect) (updateRect == NULL || how != view_Remove && how != view_MoveNoRedraw)

static void
Update(self, how, updateRect, geometryChanged)
struct layoutview *self;
enum view_UpdateType how;		/* kind of update */
struct rectangle *updateRect;		/* rectangle affected; or NULL for update */
boolean geometryChanged;		/* geometry changed since last update */
{
    struct component *c;
    struct view *child;
    struct region *visualRegion;	/* visual region of entire layout */
    struct region *remainingRegion;	/* region still to be updated */
    struct rectangle childRect;		/* rectangle containing child */
    struct region *childRegion;		/* visual region available to child */
    struct rectangle frameRect;		/* rectangle containing child and frame */
    struct region *frameRegion;		/* region containing child plus frame */
    struct rectangle vb;		/* visual bounds for cursor */
    struct graphic *fillpattern;	/* fill pattern bitmap */

    /* deal with cursors */

    if (self->hasInputFocus && how != view_Remove && layoutview_Hitmode(self) != HIT_EXEC) {
	layoutview_GetVisualBounds(self, &vb);
	layoutview_PostCursor(self, &vb, self->authoringCursor);
    }
    else
	layoutview_RetractCursor(self, self->authoringCursor);

    /* initialize region being updated */

    visualRegion = layoutview_GetVisualRegion(self, region_CreateEmptyRegion());
    if (updateRect != NULL) {
	remainingRegion = region_CreateRectRegion(updateRect);
	region_IntersectRegion(remainingRegion, visualRegion, remainingRegion);
    }
    else {
	remainingRegion = region_CreateEmptyRegion();
	region_CopyRegion(remainingRegion, visualRegion);
    }
    layoutview_SetClippingRegion(self, remainingRegion);
    if (ReallyDrawing(how, updateRect) && geometryChanged)
	layoutview_EraseVisualRect(self);
    else if (layoutview_Hitmode(self) == HIT_DRAGGING || layoutview_Hitmode(self) == HIT_CREATING)
	DrawRubberBox(self);

    forallcomponents(getLayout(self), c) {

	rectangle_SetRectSize(&childRect, vLeft(self, c), vTop(self, c), vWidth(self, c), vHeight(self, c));
	childRegion = region_CreateRectRegion(&childRect);
	region_IntersectRegion(childRegion, remainingRegion, childRegion);

	/* draw frame around child */

	if (layoutview_Hitmode(self) != HIT_EXEC) {
	    if (ReallyDrawing(how, updateRect)) {
		layoutview_SetClippingRegion(self, remainingRegion);
		if (c != layoutview_Selection(self))
		    fillpattern = layoutview_GrayPattern(self, 8, 16);
		else if (layoutview_Hitmode(self) != HIT_DRAGGING && layoutview_Hitmode(self) != HIT_CREATING)
		    fillpattern = layoutview_BlackPattern(self);
		else 
		    fillpattern = layoutview_WhitePattern(self);
		if (cTop(c) > 0)
		    layoutview_FillRectSize(self, vLeft(self, c) - 1, cTop(c) - 1, vWidth(self, c) + 1, 1, fillpattern);
		if (cWidth(c) > 0)
		    layoutview_FillRectSize(self, cRight(c), vTop(self, c) - 1, 1, vHeight(self, c) + 1, fillpattern);
		if (cHeight(c) > 0)
		    layoutview_FillRectSize(self, vLeft(self, c), cBottom(c), vWidth(self, c) + 1, 1, fillpattern);
		if (cLeft(c) > 0)
		    layoutview_FillRectSize(self, cLeft(c) - 1, vTop(self, c), 1, vHeight(self, c) + 1, fillpattern);
	    }
	    rectangle_SetRectSize(&frameRect, vLeft(self, c) - 1, vTop(self, c) - 1, vWidth(self, c) + 2, vHeight(self, c) + 2);
	    frameRegion = region_CreateRectRegion(&frameRect);
	    region_SubtractRegion(remainingRegion, frameRegion, remainingRegion);
	    region_Destroy(frameRegion);
	}
	else
	    region_SubtractRegion(remainingRegion, childRegion, remainingRegion);

	/* update child itself */

	layoutview_SetClippingRegion(self, childRegion);

	child  = layoutview_FindSubview(self, c);
	if (!geometryChanged)
	    /* do nothing */ ;
	else if (child == NULL) {
	    if (ReallyDrawing(how, updateRect))
		layoutview_FillRect(self, &childRect, layoutview_BlackPattern(self));
	}
	else if (updateRect != NULL) {
	    if (layout_debug)
		printf("FullUpdating %s\n", dataobject_ViewName(cData(c)));
	    view_InsertView(child, self, &childRect);
	    region_OffsetRegion(childRegion, -vLeft(self, c), -vTop(self, c));
	    view_SetVisualRegion(child, childRegion);
	    view_FullUpdate(child, how, rectangle_Left(updateRect), rectangle_Top(updateRect), rectangle_Width(updateRect), rectangle_Height(updateRect));
	}
	else {
	    if (layout_debug)
		printf("Redrawing %s\n", dataobject_ViewName(cData(c)));
	    view_InsertView(child, self, &childRect);
	    region_OffsetRegion(childRegion, -vLeft(self, c), -vTop(self, c));
	    view_SetVisualRegion(child, childRegion);
	    view_FullUpdate(child, view_FullRedraw, 0, 0, vWidth(self, c), vHeight(self, c));
	}
	region_Destroy(childRegion);

    }

    /* clean up clipping region, memory allocation, and active rubberbanding */

    region_Destroy(remainingRegion);
    layoutview_SetClippingRegion(self, visualRegion);
    region_Destroy(visualRegion);

    if (layoutview_Hitmode(self) == HIT_DRAGGING || layoutview_Hitmode(self) == HIT_CREATING)
	DrawRubberBox(self);
}

/* full update when window changes */

void
layoutview__FullUpdate(self, how, left, top, width, height)
struct layoutview *self;
enum view_UpdateType how;		/* kind of update */
long left;				/* updated rectangle (for certain kinds) */
long top;
long width;
long height;
{
    struct rectangle cliprect;		/* actual updated rectangle */

    if (layout_debug)
	printf("layoutview_FullUpdate(%d, %d, %d, %d, %d)\n", (int)how, left, top, width, height);

    self->updateRequested = FALSE;

    /* deferred initialization of graphic */

    if (self->authoringCursor == NULL)
	InitializeGraphic(self);

    /* force authoring mode if the layout is empty */

    if (layoutview_Hitmode(self) == HIT_EXEC && layout_GetFirstComponent(getLayout(self)) == NULL)
	layoutview_SetAuthoringMode(self);

    /* define rectangle actually being updated */

    if (how == view_PartialRedraw || how == view_LastPartialRedraw)
	rectangle_SetRectSize(&cliprect, left, top, width, height);
    else {
	rectangle_SetRectSize(&cliprect, layoutview_GetVisualLeft(self), layoutview_GetVisualTop(self), layoutview_GetVisualWidth(self), layoutview_GetVisualHeight(self));
    }
    /* perform the update */

    layoutview_SetTransferMode(self, graphic_COPY);
    Update(self, how, &cliprect, TRUE);
    if (how == view_FullRedraw)
	self->lastUpdate = layout_GetModified(getLayout(self));
}

/* partial update in response to WantUpdate request */

void
layoutview__Update(self)
struct layoutview *self;
{
    if (layout_debug)
	printf("layoutview_Update requested=%d\n", self->updateRequested);

    Update(self, -1, NULL, (self->lastUpdate < layout_GetModified(getLayout(self))));
    self->lastUpdate = layout_GetModified(getLayout(self));
}

/* request update */

void
layoutview__WantUpdate(self, requestor)
register struct layoutview *self;
struct view *requestor;
{

    if (layout_debug)
	printf("layoutview_WantUpdate(%x,%x) requested = %d\n", self, requestor, self->updateRequested);

    if (&getView(self) == requestor) {
	if (self->updateRequested)
	    return;
	self->updateRequested = TRUE;
    }	
    super_WantUpdate(self, requestor);
}


/* set authoring menu mask */

static void
SetAuthoringMask(self)
struct layoutview *self;
{
    int m;

    m = layoutview_AUTHOR_MASK;

    if (layoutview_Selection(self) != NULL) {
	m |= layoutview_SELECTION_MASK;
	if (cVaries(layoutview_Selection(self)))
	    m |= layoutview_VARIABLE_MASK;
	else
	    m |= layoutview_FIXED_MASK;

	if (layout_debug)
	    printf("  authoring mask = %d  varies=%d\n", m, cVaries(layoutview_Selection(self)));
    }

    if (menulist_SetMask(self->menulist, m))
	layoutview_PostMenus(self, NULL);
}

/* make new selection */

void
layoutview__SetSelection(self, c)
struct layoutview *self;
struct component *c;			/* component to selected, or NULL */
{
    if (layoutview_Selection(self) != c) {
	layoutview_Selection(self) = c;
	if (layoutview_Hitmode(self) != HIT_EXEC)
	    layoutview_WantUpdate(self, &getView(self));
    }
    if (layoutview_Hitmode(self) != HIT_INITIALIZING && layoutview_Hitmode(self) != HIT_EXEC)
	SetAuthoringMask(self);
}

/* set new selection size and position */

boolean				/*  returns TRUE if real selection created */
layoutview__SetSelectionSize(self, x, y, w, h)
struct layoutview *self;
long x, y, w, h;
{
    if (w < 0) {
	w = -w;
	x = x - w;
    }
    if (h < 0) {
	h = -h;
	y = y - h;
    }

    if (w < POINTINGTHRESHHOLD || h < POINTINGTHRESHHOLD) {	/* too small - ditch it */
	if (layoutview_Selection(self) != NULL)
	    layoutview_DestroyComponent(self);
    }
    else {
	if (x + w >= layoutview_GetVisualRight(self) - POINTINGTHRESHHOLD)
	    w = 0;
	if (y + h >= layoutview_GetVisualBottom(self) - POINTINGTHRESHHOLD)
	    h = 0;
	if (x <= POINTINGTHRESHHOLD)
	    x = 0;
	if (y <= POINTINGTHRESHHOLD)
	    y = 0;
	if (layoutview_Selection(self) == NULL)
	    layoutview_SetSelection(self, layout_CreateComponent(getLayout(self)));
	if (layoutview_Selection(self) != NULL) {
	    layout_SetComponentSize(getLayout(self), layoutview_Selection(self), x, y, w, h);
	    return TRUE;
	}
    }
    return FALSE;
}

/* set rubber band box size and position */

static void
SetRubberBox(self, x, y)
struct layoutview *self;
long x;			/* current position relative to self->dragx */
long y;			/* current position relative to self->dragy */
{
    struct component *c = layoutview_Selection(self);
    long xx, yy, rr, bb;

    if (c != layout_GetFirstComponent(getLayout(self)))
	layoutview_Promote(self);
    xx = (self->dragleft ? x - self->dragx : 0);
    yy = (self->dragtop ? y - self->dragy : 0);
    rr = (self->dragright ? x - self->dragx : 0);
    bb = (self->dragbottom ? y - self->dragy : 0);
    self->rubberleft = layoutview_ApplyGranularity(self, vLeft(self, c) + xx);
    self->rubbertop = layoutview_ApplyGranularity(self, vTop(self, c) + yy);
    self->rubberwidth = layoutview_ApplyGranularity(self, vWidth(self, c) - xx + rr);
    self->rubberheight = layoutview_ApplyGranularity(self, vHeight(self, c) - yy + bb);
}

/* Find component containing (x, y) */

static struct component *	/* returns component containing x, y or NULL */
FindContainingComponent(self, x, y, thresh)
struct layoutview *self;
long x;				/* point to be found */
long y;
long thresh;			/* tolerance outside component allowed */
{
    struct component *c;

    forallcomponents(getLayout(self), c) {
	if ((cLeft(c) <= 0 || cLeft(c) - thresh <= x) && (cWidth(c) <= 0 || cRight(c) + thresh > x) && (cTop(c) <= 0 || cTop(c) - thresh <= y) && (cHeight(c) <= 0 || cBottom(c) + thresh > y)) {
	    return c;
	}
    }
    return NULL;
}

/* process mouse hit */

struct view *			/* returns view which should get follow-up events*/
layoutview__Hit(self, action, x, y, numberOfClicks)
struct layoutview *self;
enum view_MouseAction action;	/* which button; what it did */
long x;				/* where the mouse points */
long y;
long numberOfClicks;		/* number of hits at same place */
{
    struct component *c;
    struct view *result;
    struct view *child;
    char whereline[80];

    if (layout_debug)
	printf("layoutview_Hit(%d, %ld, %ld, %ld)\n", (int) action, x, y, numberOfClicks);

    if (layoutview_Hitmode(self) == HIT_DRAGGING || layoutview_Hitmode(self) == HIT_CREATING)
	DrawRubberBox(self);

    c = FindContainingComponent(self, x, y, 0);

    /* pass hit to child when running or initializing */

    if (layoutview_Hitmode(self) == HIT_INITIALIZING && c != layoutview_Selection(self)) {
	layoutview_SetAuthoringMode(self);
	layoutview_SetSelection(self, c);
    }
    if (layoutview_Hitmode(self) == HIT_AUTHORING && (action == view_LeftDown || action == view_RightDown) && c != NULL && numberOfClicks > 1) {
	layoutview_SetInitMode(self);
	numberOfClicks -= 1;
    }

    if (c != NULL && (layoutview_Hitmode(self) == HIT_EXEC && cVaries(c) || layoutview_Hitmode(self) == HIT_INITIALIZING)) {
	layoutview_SetSelection(self, c);

	child = layoutview_FindSubview(self, c);

	if (child == NULL) {
	    if (layout_debug)
		printf("Null hit at %d %d\n", x - vLeft(self, c), y - vTop(self, c));
	    result = NULL;
	}

	else {
	    if (layout_debug)
		printf("Passing hit to child %x at %d %d\n", child, x - vLeft(self, c), y - vTop(self, c));
	    result = view_Hit(child, action, x - vLeft(self, c), y - vTop(self, c), numberOfClicks);
	}

	if (layout_debug)
	    printf("Child hit returned %x\n", result);
	if (result != NULL)
	    return result;
    }

    /* create new component or drag existing one */

    else if (action == view_LeftDown || action == view_RightDown && layoutview_Hitmode(self) == HIT_AUTHORING) {
	c = FindContainingComponent(self, x, y, POINTINGTHRESHHOLD);
	layoutview_SetSelection(self, c);
	self->dragx = x;
	self->dragy = y;
	if (c == NULL) {

	    /* initialize create */
	    self->dragleft = self->dragtop = FALSE;
	    self->dragright = self->dragbottom = TRUE;
	    self->rubberleft = layoutview_ApplyGranularity(self, self->dragx);
	    self->rubbertop = layoutview_ApplyGranularity(self, self->dragy);
	    self->rubberheight = self->rubberwidth = 0;
	}
	else {

	    /* initialize drag */

	    self->dragleft = x < vLeft(self, c) + POINTINGTHRESHHOLD && x < vRight(self, c);
	    self->dragright = x >= vRight(self, c) - POINTINGTHRESHHOLD && x >= vLeft(self, c);
	    self->dragtop = y < vTop(self, c) + POINTINGTHRESHHOLD && y < vBottom(self, c);
	    self->dragbottom = y >= vBottom(self, c) - POINTINGTHRESHHOLD && y >= vTop(self, c);
	    if (!self->dragleft && !self->dragright && !self->dragtop && !self->dragbottom)
		self->dragleft = self->dragright = self->dragtop = self->dragbottom = TRUE;
	}
    }

    /* continue creating or dragging */

    else if ((action == view_RightUp || action == view_RightMovement
	       || action == view_LeftUp || action == view_LeftMovement) &&
	      (layoutview_Hitmode(self) == HIT_CREATING || layoutview_Hitmode(self) == HIT_DRAGGING
	       || layoutview_Hitmode(self) == HIT_AUTHORING
	       && (x < self->dragx - MOTIONTHRESHHOLD
		|| x > self->dragx + MOTIONTHRESHHOLD
		|| y < self->dragy - MOTIONTHRESHHOLD
		|| y > self->dragy + MOTIONTHRESHHOLD))) {

	c = layoutview_Selection(self);

	/* do not drag background, but OK to resize it */
	if (layoutview_Hitmode(self) == HIT_AUTHORING
	    && self->dragleft && self->dragright && self->dragtop && self->dragbottom
	    && cLeft(c) <= 0 && cTop(c) <= 0 && cWidth(c) <= 0 && cHeight(c) <= 0) {
	    c = NULL;
	    layoutview_SetSelection(self, c);
	    self->dragleft = self->dragtop = FALSE;
	    self->dragright = self->dragbottom = TRUE;
	    self->rubberleft = layoutview_ApplyGranularity(self, self->dragx);
	    self->rubbertop = layoutview_ApplyGranularity(self, self->dragy);
	    self->rubberwidth = self->rubberheight = 0;
	}

	if (c == NULL ) {

	    /* actually begin creating */

	    if (layoutview_Hitmode(self) == HIT_AUTHORING)
		self->hitmode = HIT_CREATING;

	    /* continue creating */

	    self->rubberwidth = layoutview_ApplyGranularity(self, x - self->dragx);
	    self->rubberheight = layoutview_ApplyGranularity(self, y - self->dragy);
	    if (action == view_LeftUp || action == view_RightUp) {

		/* finish creating */

		message_DisplayString(&getView(self), 0, "");
		if (layoutview_SetSelectionSize(self, self->rubberleft, self->rubbertop, self->rubberwidth, self->rubberheight)) {
		    layoutview_SetInitMode(self);
		    if (cData(layoutview_Selection(self)) == NULL) {
			if (layoutview_Createmode(self) == CREATE_FILLER)
			    layoutview_SetChildByName(self, "filler");
			else if (layoutview_Createmode(self) == CREATE_PASTE)
			    layoutview_Paste(self);
		    }
		}
		else {
		    layoutview_SetSelection(self, NULL);
		    layoutview_SetAuthoringMode(self);
		}
	    }
	}
	else {
	    if (layoutview_Hitmode(self) == HIT_AUTHORING) {

		/* actually begin dragging */

		self->hitmode = HIT_DRAGGING;
		SetRubberBox(self, self->dragx, self->dragy);
		DrawRubberBox(self);
	    }

	    /* continue dragging */

	    SetRubberBox(self, x, y);
	    if (action == view_LeftUp || action == view_RightUp) {

		/* finish dragging */

		DrawRubberBox(self);
		message_DisplayString(&getView(self), 0, "");
		layoutview_SetSelectionSize(self, self->rubberleft, self->rubbertop, self->rubberwidth, self->rubberheight);
		self->hitmode = HIT_AUTHORING;
		if (layout_debug)
		    printf("Done dragging, back to authoring\n");
	    }
	}
    }

    if (!self->hasInputFocus && layoutview_Hitmode(self) != HIT_INITIALIZING)
	layoutview_WantInputFocus(self, &getView(self));

    if (layoutview_Hitmode(self) == HIT_DRAGGING || layoutview_Hitmode(self) == HIT_CREATING) {
	DrawRubberBox(self);
	*whereline = '\0';
	if (self->dragleft)
	    sprintf(whereline + strlen(whereline), "left =%4d ", self->rubberleft);
	if (self->dragright)
	    sprintf(whereline + strlen(whereline), "right =%4d ", self->rubberleft + self->rubberwidth);
	if (self->dragtop)
	    sprintf(whereline + strlen(whereline), "top =%4d ", self->rubbertop);
	if (self->dragbottom)
	    sprintf(whereline + strlen(whereline), "bottom =%4d ", self->rubbertop + self->rubberheight);
	if (self->dragleft != self->dragright)
	    sprintf(whereline + strlen(whereline), "width =%4d ", self->rubberwidth);
	if (self->dragtop != self->dragbottom)
	    sprintf(whereline + strlen(whereline), "height =%4d ", self->rubberheight);
	message_DisplayString(&getView(self), 0, whereline);
    }

    return &getView(self);
}

/* input focus obtained; highlight something and post menus */

void
layoutview__ReceiveInputFocus(self)
struct layoutview *self;
{
    if (layout_debug)
	printf("layoutview_ReceiveInputFocus\n");

    if (!(self->hasInputFocus)) {
	self->hasInputFocus = TRUE;
	self->keystate->next = NULL;
	layoutview_PostKeyState(self, self->keystate);
	layoutview_PostMenus(self, NULL);
    }
}

/* input focus lost; remove highlighting */

void
layoutview__LoseInputFocus(self)
struct layoutview *self;
{
    if (layout_debug)
	printf("layoutview_LoseInputFocus\n");

    if (self->hasInputFocus) {
	self->hasInputFocus = FALSE;
    }
}

/* handle request to post menus */

void
layoutview__PostMenus(self, ml)
struct layoutview *self;
struct menulist	*ml;			/* list of menus to post */
{
    if (layout_debug) {
	if (ml == NULL)
	    printf("layoutview_PostMenus NULL\n");
	else
	    printf("layoutview_PostMenus %x %s\n", ml, viewname((struct view *)ml->object));
    }

    menulist_UnchainML(self->menulist, CHILD_MENULIST_KEY);
    if (ml != NULL)
	menulist_ChainAfterML(self->menulist, ml, CHILD_MENULIST_KEY);

    super_PostMenus(self, self->menulist);
}

/* handle child's request for a new size */

void
layoutview__WantNewSize(self, requestor)
struct layoutview *self;
struct view *requestor;			/* view requesting a new size */
{
    struct viewlist *vl;
    struct component *c;
    long dWidth, dHeight;

    if (layout_debug)
	printf("layoutview_WantNewSize(%s)\n", viewname(requestor));

    forallsubviews(self, vl) {
	if (vChild(vl) == requestor) {
	    c = vComponent(vl);
	    view_DesiredSize(requestor, vWidth(self, c), vHeight(self, c), view_NoSet, &dWidth, &dHeight);
	    if (layout_debug)
		printf(" .. ignored %d %d\n", dWidth, dHeight);
	}
    }
}

/* destroy selected component including its view */

void
layoutview__DestroyComponent(self)
struct layoutview *self;
{
    struct viewlist *vl;
    struct view *child;
    FILE *cutFile;
    char buffer[81];

    if (layout_debug)
	printf("layoutview_DestroyComponent(%s)\n", GetClassName(self, cData(layoutview_Selection(self))));

    if (layoutview_Selection(self) == NULL)
	return;
    child = NULL;
    forallsubviews(self, vl)
      if (vComponent(vl) == layoutview_Selection(self)) {
	  child = vChild(vl);
	  break;
      }
    if (child != NULL) {
	layoutview_RemoveSubview(self, child); /* prevents removing component in view_Destroy */
	view_UnlinkTree(child);
	view_Destroy(child);
    }
    if (cData(layoutview_Selection(self)) != NULL ) {
	cutFile = im_ToCutBuffer(layoutview_GetIM(self));
	dataobject_Write(cData(layoutview_Selection(self)), cutFile, im_GetWriteID(), 0);
	im_CloseToCutBuffer(layoutview_GetIM(self), cutFile);
	sprintf(buffer, "Poof!  Your %s vanishes into the cutbuffer.", GetClassName(self, cData(layoutview_Selection(self))));
	message_DisplayString(self, 0, buffer);
    }
    else
	message_DisplayString(self, 0, "Poof!");
    layout_RemoveComponent(getLayout(self), layoutview_Selection(self));
    layoutview_SetSelection(self, NULL);
}

/* destroy all subviews of a layout */

static void
DestroySubviews(self)
struct layoutview *self;
{
    struct viewlist *vl;
    struct view *child;

    while (self->subviews != NULL) {
	vl = self->subviews;
	child = vChild(vl);
	view_UnlinkTree(child);
	view_Destroy(child);
    }
}

/* tear down a layoutview */

void
layoutview__FinalizeObject(classID, self)
struct classheader *classID;
struct layoutview *self;
{
    if (layout_debug)
	printf("layoutview_FinalizeObject\n");

    DestroySubviews(self);
    menulist_Destroy(self->menulist);
    keystate_Destroy(self->keystate);
}

/* build tree of views */

void
layoutview__LinkTree(self, parent)
struct layoutview *self;
struct view *parent;		/* parent into which to link self */
{
    struct viewlist *vl;

    if (layout_debug)
	printf("layoutview_LinkTree to %s\n", viewname(parent));

    forallsubviews(self, vl)
      view_LinkTree(vChild(vl), self);

    super_LinkTree(self, parent);
}

/* notification that observed object changed */

void
layoutview__ObservedChanged(self, changed, status)
struct layoutview *self;
struct observable *changed;		/* that which changed */
long status;				/* OBJECTDESTROYED is used to signal deletion */
{
    struct component *c;

    if (layout_debug)
	printf("layoutview_ObservedChanged(%ld)\n", status);

    if (changed	== &getLayout(self)->header.observable) {	/* if it is my dataobject */

	if (status == observable_OBJECTDESTROYED)
	    DestroySubviews(self);
	layoutview_WantUpdate(self, &getView(self));
    }

    else if (status == observable_OBJECTDESTROYED) {
	c = layoutview_RemoveSubview(self, (struct view *)changed);
	if (c != NULL) {
	    layout_RemoveComponent(getLayout(self), c);
	    if (layout_debug)
		printf("removing subview %s\n", GetClassName(self, cData(c)));
	}
    }

    else { /* not my dataobject */
	super_ObservedChanged(self, changed, status);

    }
}

/* toggle layout_debug */

void
layoutview__ToggleDebug(self)
struct layoutview *self;
{

/*
    the rather peculiar if statement below is designed to work properly
    in either the case that layout and layoutview are loaded separately or
    loaded together (ie two or one copies of 'debug'
*/
    if (layout_debug) {
	layout_ToggleDebug(getLayout(self));
	layout_debug = 0;
    } else {
	layout_ToggleDebug(getLayout(self));
	layout_debug = 1;
    }
}

/* enter execution mode */

void
layoutview__SetRunMode(self)
struct layoutview *self;
{
    if (layout_debug)
	printf("layoutview_SetRunMode() hitmode=%d\n", layoutview_Hitmode(self));

    if (layoutview_Hitmode(self) != HIT_EXEC) {
	if (menulist_SetMask(self->menulist, layoutview_EXEC_MASK))
	    layoutview_PostMenus(self, NULL);
	layoutview_WantUpdate(self, &getView(self));
	self->lastUpdate = -1;
	self->hitmode = HIT_EXEC;
    }
}

/* enter initialization mode */

void
layoutview__SetInitMode(self)
struct layoutview *self;
{
    if (layout_debug)
	printf("layoutview_SetInitMode() hitmode=%d\n", layoutview_Hitmode(self));

    if (layoutview_Hitmode(self) != HIT_INITIALIZING) {
	if (menulist_SetMask(self->menulist, layoutview_EXEC_MASK))
	    layoutview_PostMenus(self, NULL);
	if (layoutview_Hitmode(self) != HIT_AUTHORING) {
	    layoutview_WantUpdate(self, &getView(self));
	    self->lastUpdate = -1;
	}
	self->hitmode = HIT_INITIALIZING;
    }
}

/* enter authoring mode */

void
layoutview__SetAuthoringMode(self)
struct layoutview *self;
{
    if (layout_debug)
	printf("layoutview_SetAuthoringMode() hitmode=%d\n", layoutview_Hitmode(self));

    if (layoutview_Hitmode(self) != HIT_AUTHORING) {
	SetAuthoringMask(self);
	if (layoutview_Hitmode(self) != HIT_INITIALIZING) {
	    layoutview_WantUpdate(self, &getView(self));
	    self->lastUpdate = -1;
	}
	self->hitmode = HIT_AUTHORING;
    }
}

/* past object into selected component */

void
layoutview__Paste(self)
struct layoutview *self;
{
    FILE *pasteFile;
    int objectID;
    char *objectName;

    if (layout_debug)
	printf("layoutview_Paste()\n");

    if(layoutview_Selection(self) == NULL)
	return;
    pasteFile = im_FromCutBuffer(layoutview_GetIM(self));
    objectName = filetype_Lookup(pasteFile, NULL, &objectID, NULL); /* For now, ignore attributes. */
    if(objectName == NULL)
	objectName = "text";
    layoutview_ReplaceComponent(self, layoutview_Selection(self), objectName);
    if (cData(layoutview_Selection(self)) != NULL)
	dataobject_Read(cData(layoutview_Selection(self)), pasteFile, objectID);
    im_CloseFromCutBuffer(layoutview_GetIM(self), pasteFile);
    InitComponent(self);
}

/* insert dataobject by name */

void
layoutview__SetChildByName(self, dataname)
struct layoutview *self;
char *dataname;				/* dataobject name */
{
    if (dataname == NULL || *dataname == '\0')
	dataname = "filler";

    if (layout_debug)
	printf("layoutview_SetChildByName(%s)\n", dataname);

    layoutview_ReplaceComponent(self, layoutview_Selection(self), dataname);
    InitComponent(self);
}

/* prompt for name and insert data object */

void
layoutview__SetChild(self)
struct layoutview *self;
{
    char dataname[100];
    
    dataname[0] = '\0';
    if (message_AskForString(&getView(self), 0, "Data object to enter here (filler): ", "", dataname, sizeof dataname))
	return;
    layoutview_SetChildByName(self, dataname);
}

/* promote selection to front */

void
layoutview__Promote(self)
struct layoutview *self;
{
    if (layout_debug)
	printf("layoutview_Promote()\n");

    if (layoutview_Selection(self) != NULL)
	layout_Promote(getLayout(self), layoutview_Selection(self));
}


/* demote selection to back of list */

void
layoutview__Demote(self)
struct layoutview *self;
{
    if (layout_debug)
	printf("layoutview_Demote()\n");

    if (layoutview_Selection(self) != NULL)
	layout_Demote(getLayout(self), layoutview_Selection(self));
}

/* fill background of layout with selection */

void
layoutview__MakeBackground(self)
struct layoutview *self;
{
    if (layout_debug)
	printf("layoutview_MakeBackground()\n");

    if (layoutview_Selection(self) != NULL) {
	layout_Demote(getLayout(self), layoutview_Selection(self));
	layout_SetComponentSize(getLayout(self), layoutview_Selection(self), 0, 0, 0, 0);
	SetAuthoringMask(self);
    }
}

/* Allow selected component to vary */

void
layoutview__MakeVariable(self)
struct layoutview *self;
{
    if (layout_debug)
	printf("layoutview_MakeVariable()\n");

    if (layoutview_Selection(self) != NULL) {
	layout_MakeVariable(getLayout(self), layoutview_Selection(self));
	SetAuthoringMask(self);
    }
}

/* Require selected component to vary only when authoring */

void
layoutview__MakeFixed(self)
struct layoutview *self;
{
    if (layout_debug)
	printf("layoutview_MakeFixed()\n");

    if (layoutview_Selection(self) != NULL) {
	layout_MakeFixed(getLayout(self), layoutview_Selection(self));
	SetAuthoringMask(self);
    }
}

/* Do not fill newly created data objects */

void
layoutview__SetCreateMode(self, createmode)
struct layoutview *self;
enum createmode_enum createmode;
{
    if (layout_debug)
	printf("layoutview_SetCreateMode(%d)\n", createmode);

    self->createmode = createmode;
}

/* Set granularity */

void
layoutview__SetGranularity(self, granularity)
struct layoutview *self;
int granularity;
{
    if (layout_debug)
	printf("layoutview_SetGranularity(%d)\n", granularity);

    self->granularity = granularity;
}

/* print components back to front */

static void
PrintComponents(self, f, processor, finalFormat, c, saveno)
struct layoutview *self;
FILE *f;
char *processor;			/* processor */
char *finalFormat;			/* final format */
struct component *c;			/* current component to be printed */
int saveno;				/* number of state-restoring macro */
{
    struct view *child;
    struct rectangle childRect;

    /* use recursion to print from back to front */

    if (c == NULL)
	return;
    PrintComponents(self, f, processor, finalFormat, c->nextcomponent, saveno);

    /* OK with those behind, now print this individual component */

    child = layoutview_FindSubview(self, c);
    fprintf(f, "\\\"component: %s\n", viewname(child));

    fprintf(f, ".rs\n");			/* be sure we are really spacing */
    fprintf(f, ".sp %dp\n", vTop(self, c));		/* space to top of component */
    fprintf(f, ".in \\n(.iu+%dp\n", vLeft(self, c));	/* indent to left of component */
    if (cWidth(c) > 0)
	fprintf(f, ".ll \\n(.iu+%dp\n", cWidth(c));	/* set width for component */

    if (child) {
	rectangle_SetRectSize(&childRect, vLeft(self, c), vTop(self, c), vWidth(self, c), vHeight(self, c));
	view_InsertView(child, self, &childRect);
	view_Print(child, f, processor, finalFormat, FALSE);
    }

    fprintf(f, ".br\n");			/* flush line buffer */
    fprintf(f, ".%d\n", saveno);		/* restore state */
    fprintf(f, ".rt\n");			/* return to top of layout */
}

/* print as part of larger document */

void
layoutview__Print(self, f, processor, finalFormat, toplevel)
struct layoutview *self;
FILE *f;					/* output file */
char *processor;				/* processor */
char *finalFormat;				/* final format */
boolean	toplevel;				/* am I the top level view? */
{
    long height;
    struct component *c;
    static int saveno = 89;

    if (layout_debug)
	printf("layoutview_Print(%x, %x, %s, %s, %d)\n", self, f, processor, finalFormat, toplevel);

    /* set up  top-level stuff */

    if (class_Load("texttroff") == NULL)
	printf("Can't load texttroff - document initializtion missing.\n");
    if (toplevel && class_IsLoaded("texttroff")) {
	texttroff_BeginDoc(f);
	fprintf(f, ".nr IN 0\n");	/* suppress indent at outer level */
	fprintf(f, ".nr PO 0\n");	/* ditto left margin */
	fprintf(f, ".nr HM 0\n");	/* ditto header margin */
	fprintf(f, ".nr FM 0\n");	/* ditto footer margin */
	fprintf(f, ".RS\n");
    }

    fprintf(f, "\\\"layout begins\n");

    /* save state in macro 90, 91, ... */

    saveno += 1;
    fprintf(f, ".de %d\n", saveno);	/* macro to restore state */
    fprintf(f, ".if \\n(.u .fi\n");	/* filling */
    fprintf(f, ".if \\n(.j .ad\n");	/* adjusting */
    fprintf(f, ".if \\n(.j=0 .na\n");
    fprintf(f, ".ft \\n(.f\n");		/* font */
    fprintf(f, ".ps \\n(.s\n");		/* point size */
    fprintf(f, ".vs \\n(.vu\n");	/* vertical spacing */
    fprintf(f, ".ll \\n(.lu\n");	/* line length */
    fprintf(f, ".in \\n(.iu\n");	/* indentation */
    fprintf(f, "..\n");

    /* ask for necessary space on page and remember starting point */

    height = 0;
    forallcomponents(getLayout(self), c) {
	if (cHeight(c) > 0 && height < cBottom(c))
	    height = cBottom(c);
    }

    fprintf(f, ".rs\n");		/* be sure we are really spacing */
    fprintf(f, ".sp -\\n(VSu\n");	/* compensate for baseline offset */
    fprintf(f, ".ne %dp\n", height);	/* insure enough space to next trap */
    fprintf(f, ".mk\n");		/* mark this starting position */

    /* print all components back to front */

    PrintComponents(self, f, processor, finalFormat, layout_GetFirstComponent(getLayout(self)), saveno);

    fprintf(f, ".sp %dp\n", height);	/* space to bottom of layout */
    saveno -= 1;
    fprintf(f, "\\\"layout ends\n");

    if (toplevel && class_IsLoaded("texttroff"))
	texttroff_EndDoc(f);

    layoutview_WantUpdate(self, &getView(self));		/* redraw to fix up visual regions */
    self->lastUpdate = -1;
}

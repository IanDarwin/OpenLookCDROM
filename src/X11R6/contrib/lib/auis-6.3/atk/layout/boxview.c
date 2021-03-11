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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/layout/RCS/boxview.c,v 1.9 1994/04/17 19:33:20 rr2b Exp $";
#endif

/* $ACIS$ */

 

#define viewname(v) ((v) == NULL ? "<NO VIEW>" : atom_Name(atomlist_First(view_GetName(v))))
#define classname(do) ((do) == NULL ? "<NO OBJECT>" : class_GetTypeName(do))

#ifndef _IBMR2
extern char *malloc();
#endif /* _IBMR2 */

#include <class.h>
#include <assert.h>
#include <rect.h>

#include <atomlist.ih>
#include <dataobj.ih>
#include <filetype.ih>
#include <im.ih>
#include <message.ih>
#include <region.ih>
#include <txttroff.ih>
#include <view.ih>

#include <box.ih>
#include <boxview.eh>

static boolean boxview_debug=0;
/* replace child view with another object */

void InitChild();			/* forward reference */

void
ReplaceChild(self, child, dataname)
struct boxview *self;
struct view *child;			/* child to be replaced */
char *dataname;				/* name of replacement dataobject */
{
    char foo[81];

    if (boxview_debug)
	printf("ReplaceChild(%s, %s)\n", viewname(child), dataname);

    assert(child == self->child || self->child == NULL);

    if(! class_IsTypeByName(dataname, "dataobject")){
        sprintf(foo, "%s is not a dataobject", dataname);
	message_DisplayString(&getView(self), 0, foo);
        return;
    }

    if (child != NULL) {
	view_UnlinkTree(child);
	view_Destroy(child);
	self->child = NULL;
    }

    box_FillInContents(getBox(self), dataname);
    InitChild(self);
    if (self->child != NULL)
	view_WantInputFocus(self->child, self->child);
}

/* initialize child view corresponding to box contents */

void
InitChild(self)
struct boxview *self;
{
    char *subviewname;			/* name for new view */

    if (getBox(self)->contents == NULL) {
	ReplaceChild(self, self->child, "filler");
    }

    if (self->child != NULL)
	return;

    subviewname = dataobject_ViewName(getBox(self)->contents);
    self->child = (struct view *) class_NewObject(subviewname);
    if (self->child == NULL) {
	printf("unable to create child view\n");
	return;
    }
    view_SetDataObject(self->child, getBox(self)->contents);
    view_LinkTree(self->child, &getView(self));

    if (boxview_debug)
	printf("InitChild created %s\n", subviewname); 
}

boolean					/* always returns TRUE */
boxview__InitializeClass(classID)
struct classheader *classID;		/* unused */
{

    if (boxview_debug)
	printf("boxview_InitializeClass(%x)\n", classID);

    return TRUE;
}

/* initialize box view */

boolean					/* always returns TRUE */
boxview__InitializeObject(classID, self)
struct classheader *classID;		/* unused */
struct boxview *self;
{
    if (boxview_debug)
	printf("boxview_InitializeObject(%x)\n", classID);

    self->updateNeeded = FALSE;
    self->lastUpdate = 0;
    self->child = NULL;
    /* InitChild(self); */
    return TRUE;
}

/* get width of box */

int					/* returns width of box */
boxview__BoxWidth(self)
struct boxview *self;
{
    return 3;
}

/* negotiate size of view */

enum view_DSattributes			/* returns indication of what it wants */
boxview__DesiredSize(self, width, height, pass, dWidth, dHeight)
struct boxview *self;
long width;				/* width being offered by parent */
long height;				/* height being offered */
enum view_DSpass pass;			/* what parent is willing to give */
long *dWidth;				/* set to desired width */
long *dHeight;				/* set to desired height */

/*  boxview asks for space for its child plus the box proper */

{
    long desiredWidth;
    long desiredHeight;
    enum view_DSattributes rc;
    int tw = boxview_BoxWidth(self);

    if (boxview_debug)
	printf("boxview_DesiredSize(, %d, %d, %d, .. )\n", width, height, (int)pass);

    InitChild(self);
    if (self->child == NULL) {
	desiredWidth = desiredHeight = 0;
	rc = view_WidthFlexible | view_HeightFlexible;
    }
    else 
	rc = view_DesiredSize(self->child, width - 2*tw, height - 2*tw, pass, &desiredWidth, &desiredHeight);

    *dWidth = desiredWidth + 2*tw;
    *dHeight = desiredHeight + 2*tw;

    return rc;
}

/* draw box proper */

void
boxview__DrawBox(self)
struct boxview *self;
{
    boxview_DrawRectSize(self, boxview_GetLogicalLeft(self), boxview_GetLogicalTop(self), boxview_GetLogicalWidth(self) - 1, boxview_GetLogicalHeight(self) - 1);
}

/* update image */

void
Update(self, how, updateRect, contentsChanged)
struct boxview *self;
enum view_UpdateType how;		/* kind of update */
struct rectangle *updateRect;		/* rectangle affected; or NULL for update */
boolean contentsChanged;		/* contents changed since last update */
{
    struct region *updateRegion;	/* region for this update */
    struct region *remainingRegion;	/* region to be updated */
    struct rectangle r;			/* child rectangle */
    struct region *childRegion;		/* visual region available to child */
    int tw = boxview_BoxWidth(self);

    /* make sure child is filled in*/

    InitChild(self);

    /* initialize region being updated */

    remainingRegion = boxview_GetVisualRegion(self, region_CreateEmptyRegion());
    if (updateRect != NULL) {
	updateRegion = region_CreateRectRegion(updateRect);
	region_IntersectRegion(remainingRegion, updateRegion, remainingRegion);
	region_Destroy(updateRegion);
    }
    boxview_SetClippingRegion(self, remainingRegion);
    if (contentsChanged)
	boxview_EraseVisualRect(self);

    /* redraw box */

    boxview_DrawBox(self);

    /* update child */

    rectangle_SetRectSize(&r, boxview_GetLogicalLeft(self) + tw, boxview_GetLogicalTop(self) + tw, boxview_GetLogicalWidth(self) - 2*tw, boxview_GetLogicalHeight(self) - 2*tw);
    childRegion = region_CreateRectRegion(&r);
    region_IntersectRegion(childRegion, remainingRegion, childRegion);
    region_Destroy(remainingRegion);
    boxview_SetClippingRegion(self, childRegion);

    if (!contentsChanged)
	/* do nothing */ ;
    else if (self->child == NULL) {
	boxview_FillRect(self, &r, boxview_BlackPattern(self));
    }
    else if (updateRect != NULL) {
	if (boxview_debug)
	    printf("FullUpdating %s\n", dataobject_ViewName(getBox(self)->contents));
	view_InsertView(self->child, self, &r);
	region_OffsetRegion(childRegion, -tw, -tw);
	view_SetVisualRegion(self->child, childRegion);
	view_FullUpdate(self->child, how, rectangle_Left(updateRect), rectangle_Top(updateRect), rectangle_Width(updateRect), rectangle_Height(updateRect));
    }
    else {
	if (boxview_debug)
	    printf("Redrawing %s\n", dataobject_ViewName(getBox(self)->contents));
	view_InsertView(self->child, self, &r);
	region_OffsetRegion(childRegion, -tw, -tw);
	view_SetVisualRegion(self->child, childRegion);
	view_FullUpdate(self->child, view_FullRedraw, 0, 0, rectangle_Width(&r), rectangle_Height(&r));
    }
    region_Destroy(childRegion);
}

/* full update when window changes */

void
boxview__FullUpdate(self, how, left, top, width, height)
struct boxview *self;
enum view_UpdateType how;		/* kind of update */
long left, top, width, height;		/* updated rectangle (for certain kinds; */
{
    struct rectangle cliprect;		/* actual updated rectangle */

    if (boxview_debug)
	printf("boxview_FullUpdate(%d, %d, %d, %d, %d)\n", (int)how, left, top, width, height);

    /* define rectangle actually being updated */

    if (how == view_PartialRedraw || how == view_LastPartialRedraw)
	rectangle_SetRectSize(&cliprect, left, top, width, height);
    else {
	rectangle_SetRectSize(&cliprect, boxview_GetVisualLeft(self), boxview_GetVisualTop(self), boxview_GetVisualWidth(self), boxview_GetVisualHeight(self));
    }
    /* perform the update */

    boxview_SetTransferMode(self, graphic_COPY);
    Update(self, how, &cliprect, TRUE);
    if (how == view_FullRedraw) {
	self->updateNeeded = FALSE;
	self->lastUpdate = box_GetModified(getBox(self));
    }
}

/* partial update in response to WantUpdate request */

void
boxview__Update(self)
struct boxview *self;
{
    if (boxview_debug)
	printf("boxview_Update needed=%d\n", self->updateNeeded);

    Update(self, -1, NULL, (self->lastUpdate < box_GetModified(getBox(self))));
    self->updateNeeded = FALSE;
    self->lastUpdate = box_GetModified(getBox(self));
}

/* process mouse hit */

struct view *				/* returns view which should get follow-up events*/
boxview__Hit(self, action, x, y, numberOfClicks)
struct boxview *self;
enum view_MouseAction action;		/* which button; what it did */
long x, y;				/* where the mouse points */
long numberOfClicks;			/* number of hits at same place */
{
    int tw = boxview_BoxWidth(self);

    if (boxview_debug)
	printf("boxview_Hit(%d, %ld, %ld, %ld)\n", (int) action, x, y, numberOfClicks);

    if (self->child == NULL)
	return &getView(self);
    else 
	return view_Hit(self->child, action, x - tw, y - tw, numberOfClicks);
}

/* update request */

void
RequestUpdate(self)
struct boxview *self;
{
    if (boxview_debug)
	printf("RequestUpdate() already=%d\n", self->updateNeeded);

    if (!self->updateNeeded) {
	self->updateNeeded = TRUE;
	boxview_WantUpdate(self, &getView(self));
    }
}

/* handle child's request for a new size */

void
boxview__WantNewSize(self, requestor)
struct boxview *self;
struct view	*requestor;		/* view requesting a new size */
{
    if (boxview_debug)
	printf("boxview_WantNewSize(%s)\n", viewname(requestor));

    if (self->child == requestor && getView(self).parent != NULL) {
	view_WantNewSize(getView(self).parent, self);
    }
}

/* unlink a view for a component */

void
boxview__UnlinkNotification(self, unlinkedview)
struct boxview *self;
struct view	*unlinkedview;		/* view being unlinked */
{
    if (boxview_debug)
	printf("boxview_UnlinkNotification %s\n", viewname(unlinkedview));

    if (unlinkedview == self->child)
	self->child = NULL;

    super_UnlinkNotification(self, unlinkedview);
}

/* tear down a boxview */

void
boxview__FinalizeObject(classID, self)
struct classheader *classID;		/* unused */
struct boxview *self;
{
    if (boxview_debug)
	printf("boxview_FinalizeObject\n");

}

/* build tree of views */

void
boxview__LinkTree(self, parent)
struct boxview *self;
struct view *parent;			/* parent into which to link self */
{

    if (boxview_debug)
	printf("boxview_LinkTree to %s\n", viewname(parent));

    if (self->child != NULL)
      view_LinkTree(self->child, self);

    super_LinkTree(self, parent);
}

/* notification that observed object changed */

void
boxview__ObservedChanged(self, changed, status)
struct boxview *self;
struct observable *changed;		/* that which changed */
long status;				/* OBJECTDESTROYED is used to signal deletion */
{
    if (boxview_debug)
	printf("boxview_ObservedChanged(%ld)\n", status);

    if (changed	!= &getBox(self)->header.observable)	/* if it is not my dataobject */
	super_ObservedChanged(self, changed, status);

    else { /* my dataobject has changed */

	if (status != observable_OBJECTDESTROYED)
	    RequestUpdate(self);
    }
}

/* toggle boxview_debug */

void
boxview__ToggleDebug(self)
struct boxview *self;
{

/*
    the rather peculiar if statement below is designed to work properly
    in either the case that box and boxview are loaded separately or
    loaded together (ie two or one copies of 'debug')
*/
    if (boxview_debug) {
	box_ToggleDebug(getBox(self));
	boxview_debug = 0;
    } else {
	box_ToggleDebug(getBox(self));
	boxview_debug = 1;
    }
}

/* past object into selected component */

void
boxview__Paste(self)
struct boxview *self;
{
    FILE *pasteFile;
    int objectID;
    char *objectName;

    if (boxview_debug)
	printf("boxview_Paste()\n");

    pasteFile = im_FromCutBuffer(boxview_GetIM(self));
    objectName = filetype_Lookup(pasteFile, NULL, &objectID, NULL); /* For now, ignore attributes. */
    if(objectName == NULL)
	objectName = "text";
    ReplaceChild(self, self->child, objectName);
    if (getBox(self)->contents != NULL)
	dataobject_Read(getBox(self)->contents, pasteFile, objectID);
    im_CloseFromCutBuffer(boxview_GetIM(self), pasteFile);

}

/* print as part of larger document */

void
boxview__Print(self, f, processor, finalFormat, toplevel)
struct boxview *self;
FILE *f;			/* output file */
char *processor;		/* processor */
char *finalFormat;		/* final format */
boolean	toplevel;		/* am I the top level view? */
{
    int tw = boxview_BoxWidth(self);

    if (boxview_debug)
	printf("boxview_Print(%x, %x, %s, %s, %d)\n", self, f, processor, finalFormat, toplevel);

    /* set up  top-level stuff */

    if (class_Load("texttroff") == NULL)
	printf("Can't load texttroff - document initializtion missing.\n");
    if (toplevel && class_IsLoaded("texttroff"))
	texttroff_BeginDoc(f);

    InitChild(self);

    fprintf(f, ".sp -\\n(VSu\n");	/* move up one line */
    fprintf(f, "\\D'l %dp 0'", boxview_GetLogicalWidth(self));  /* draw box */
    fprintf(f, "\\D'l 0 %dp'", boxview_GetLogicalHeight(self));
    fprintf(f, "\\D'l %dp 0'", -boxview_GetLogicalWidth(self));
    fprintf(f, "\\D'l 0 %dp'", -boxview_GetLogicalHeight(self));
    fprintf(f, "\n");			/* I think this moves back down a line */

    fprintf(f, ".sp %dp\n", tw);	/* vertical down to enclosed object */
    fprintf(f, ".in +%dp\n", tw);	/* indent for box */
    fprintf(f, ".ll -%dp\n", 2 * tw);

    if (self->child) {
	view_InsertViewSize(self->child, self, boxview_GetLogicalLeft(self) + tw, boxview_GetLogicalTop(self) + tw, boxview_GetLogicalWidth(self) - 2*tw, boxview_GetLogicalHeight(self) - 2*tw);
	view_Print(self->child, f, processor, finalFormat, FALSE);
    }

    fprintf(f, ".in -%dp\n", tw);	/* remove box indentation */
    fprintf(f, ".ll +%dp\n", 2 * tw);

    if (toplevel && class_IsLoaded("texttroff"))
	texttroff_EndDoc(f);
}

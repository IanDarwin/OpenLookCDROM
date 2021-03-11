/* figv.c - drawing object view */
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
char *figv_c_rcsid = "$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/figure/RCS/figv.c,v 1.28 1994/04/17 19:45:16 rr2b Exp $";
#endif 

#include <math.h>
#include <figv.eh>

#include <figure.ih>
#include <figtoolv.ih>
#include <figobj.ih>
#include <figogrp.ih>
#include <figoins.ih>

#include <figio.ih>

#include <region.ih>
#include <graphic.ih>
#include <view.ih>
#include <dataobj.ih>
#include <fontdesc.ih>
#include <menulist.ih>
#include <keymap.ih>
#include <scroll.ih>
#include <keystate.ih>
#include <proctbl.ih>
#include <im.ih>
#include <buffer.ih>
#include <frame.ih>
#include <txttroff.ih>
#include <complete.ih>
#include <environ.ih>

#include <rect.h>

#define figview_InitNumHighlights (2)
#define SCROLL_EXTRA_SPACE (256)
#define recttopix_Exact (-9999)

static char debug=0;     
#define DEBUG(s) {if (debug) {printf s ; fflush(stdout);}}
#define ENTER(r) DEBUG(("Enter %s(0x%lx)\n", "r", self))
#define LEAVE(r) DEBUG(("Leave %s(0x%lx)\n", "r", self))

struct printlump {
    FILE *file;
    char *prefix;
    struct figview *figview;
    char *processor, *format;
    long width, height;
};

static void ToolsetCreateProc(), ToolsetKillProc(), ChangeZoomProc();
static void FocusUpProc(), FocusDownProc(), FocusLeftProc(), FocusRightProc(), SetExpertModeProc(), AbortObjectProc(), CutSelProc(), CopySelInsetProc(), CopySelProc(), PasteSelProc(), RotatePasteProc(), ToggleReadOnlyProc(), PanToOriginProc(), WritePSProc(), ReadZipProc(), SetPrintScaleProc(), ShowPrintAreaProc();
static void FlattenRefList();
static void IncreaseClipRegProc();
static void RedrawGroup(), RepostMenus();

static void ToggleDebugProc(self, rock)
struct figview *self;
long rock;
{
    debug = ! debug;
    printf("debug is now %d\n", debug);  fflush (stdout);
}

static struct menulist *EmbeddedMenus;
static struct keymap *EmbeddedKeymap;
static boolean DefaultExpertMode = FALSE;
static char *FigureBackgroundColor = NULL;

#define	ML_expertmode	    (1)
#define	ML_nonexpertmode    (2)
#define	ML_toolset	    (4)
#define	ML_nontoolset	    (8)
#define	ML_focuschange	    (16)
#define	ML_selected	    (32)
#define ML_oneinsetselected (64)
#define	ML_noshowprintarea  (128)
#define	ML_showprintarea    (256)

boolean figview__InitializeClass(ClassID)
struct classheader *ClassID;
{
    struct proctable_Entry *proc = NULL;

    EmbeddedMenus = menulist_New();
    EmbeddedKeymap = keymap_New();

    proc = proctable_DefineProc("figview-toggle-debug", ToggleDebugProc, &figview_classinfo, NULL, "Turn debugging on or off.");

    proc = proctable_DefineProc("figview-toggle-readonly", ToggleReadOnlyProc, &figview_classinfo, NULL, "Turn read-only flag on or off.");

    proc = proctable_DefineProc("figview-toolset-create", ToolsetCreateProc, &figview_classinfo, NULL, "Creates toolset window.");
    menulist_AddToML(EmbeddedMenus, "Figure~5,Toolset~30", proc, NULL, ML_nontoolset);

    proc = proctable_DefineProc("figview-toolset-destroy", ToolsetKillProc, &figview_classinfo, NULL, "Deletes toolset window.");
    menulist_AddToML(EmbeddedMenus, "Figure~5,Remove Toolset~31", proc, NULL, ML_toolset);

    proc = proctable_DefineProc("figview-set-expert-mode", SetExpertModeProc, &figview_classinfo, NULL, "Turns on expert mode for self and toolset.");
    menulist_AddToML(EmbeddedMenus, "Figure~5,Expert Mode~40", proc, 1, ML_nonexpertmode);

    proc = proctable_DefineProc("figview-zoom", ChangeZoomProc, &figview_classinfo, NULL, "Change scale of view.");
    menulist_AddToML(EmbeddedMenus, "Figure~5,Zoom In~10", proc, 1, 0);
    keymap_BindToKey(EmbeddedKeymap, "\033Z", proc, 1); /* esc-Z */
    menulist_AddToML(EmbeddedMenus, "Figure~5,Zoom Out~11", proc, -1, 0);
    keymap_BindToKey(EmbeddedKeymap, "\033z", proc, -1); /* esc-z */
    menulist_AddToML(EmbeddedMenus, "Figure~5,Normal Size~12", proc, 0, 0);

    proc = proctable_DefineProc("figview-pan-to-origin", PanToOriginProc, &figview_classinfo, NULL, "Pan to (0,0).");
    menulist_AddToML(EmbeddedMenus, "Figure~5,Pan to Origin~19", proc, 0, 0);

    proc = proctable_DefineProc("figview-zap-selection", CutSelProc, &figview_classinfo, NULL, "Remove selected objects and put them in cut buffer.");
    menulist_AddToML(EmbeddedMenus, "Cut~1", proc, 0, ML_selected);
    keymap_BindToKey(EmbeddedKeymap, "\027", proc, 0); /* ^W */
    proc = proctable_DefineProc("figview-copy-selection", CopySelProc, &figview_classinfo, NULL, "Put selected objects in cut buffer.");
    menulist_AddToML(EmbeddedMenus, "Copy~2", proc, 0, ML_selected);
    keymap_BindToKey(EmbeddedKeymap, "\033w", proc, 0); /* esc-w */
    proc = proctable_DefineProc("figview-copy-selected-inset", CopySelInsetProc, &figview_classinfo, NULL, "Put dataobject of selected inset in cut buffer.");
    menulist_AddToML(EmbeddedMenus, "Copy Inset Contents~3", proc, 0, ML_oneinsetselected);

    proc = proctable_DefineProc("figview-yank-selection", PasteSelProc, &figview_classinfo, NULL, "Paste an object from the cut buffer.");
    menulist_AddToML(EmbeddedMenus, "Paste~4", proc, 0, 0);
    keymap_BindToKey(EmbeddedKeymap, "\031", proc, 0); /* ^Y */
    proc = proctable_DefineProc("figview-rotate-yank-selection", RotatePasteProc, &figview_classinfo, NULL, "Paste an object from the cut buffer and rotate the buffer.");
    keymap_BindToKey(EmbeddedKeymap, "\033y", proc, 0); /* esc-y */

    proc = proctable_DefineProc("figview-write-PostScript", WritePSProc, &figview_classinfo, NULL, "Write out the fig in PostScript format.");
    menulist_AddToML(EmbeddedMenus, "File,Write As PostScript~19", proc, 0, 0);
    keymap_BindToKey(EmbeddedKeymap, "\030\020", proc, 0); /* ^X^P */

    proc = proctable_DefineProc("figview-read-zip-file", ReadZipProc, &figview_classinfo, NULL, "Read a zip datastream into the figure.");
    /*menulist_AddToML(EmbeddedMenus, "File,Read Zip File~18", proc, 0, 0);*/

    proc = proctable_DefineProc("figview-set-print-scale", SetPrintScaleProc, &figview_classinfo, NULL, "Set print scale.");
    menulist_AddToML(EmbeddedMenus, "File,Set Print Scale~21", proc, 0, 0);
    keymap_BindToKey(EmbeddedKeymap, "\033P", proc, 0); /* esc-P */

    proc = proctable_DefineProc("figview-show-print-area", ShowPrintAreaProc, &figview_classinfo, NULL, "Show page boundaries.");
    menulist_AddToML(EmbeddedMenus, "File,Show Print Area~26", proc, 1, ML_noshowprintarea);
    menulist_AddToML(EmbeddedMenus, "File,Hide Print Area~26", proc, 0, ML_showprintarea);

    proc = proctable_DefineProc("figview-focus-down", FocusDownProc, &figview_classinfo, NULL, "move focus down to subgroup.");
    keymap_BindToKey(EmbeddedKeymap, "\033B", proc, 0); /* esc-B */
    proc = proctable_DefineProc("figview-focus-up", FocusUpProc, &figview_classinfo, NULL, "move focus up to parent group.");
    keymap_BindToKey(EmbeddedKeymap, "\033A", proc, 0); /* esc-A */
    proc = proctable_DefineProc("figview-focus-left", FocusLeftProc, &figview_classinfo, NULL, "move focus left to sibling group.");
    keymap_BindToKey(EmbeddedKeymap, "\033D", proc, 0); /* esc-D */
    proc = proctable_DefineProc("figview-focus-right", FocusRightProc, &figview_classinfo, NULL, "move focus left to sibling group.");
    keymap_BindToKey(EmbeddedKeymap, "\033C", proc, 0); /* esc-C */

    proc = proctable_DefineProc("figview-abort-object", AbortObjectProc, &figview_classinfo, NULL, "abort object being created.");
    keymap_BindToKey(EmbeddedKeymap, "\007", proc, 0); /* ^G */

    DefaultExpertMode = environ_GetProfileSwitch("FigureExpertMode", FALSE);
    FigureBackgroundColor = environ_GetProfile("FigureMatteColor");
    if (!FigureBackgroundColor)
	FigureBackgroundColor = "white";

    return TRUE;
}

boolean figview__InitializeObject(ClassID, self)
struct classheader *ClassID;
struct figview *self;
{
    int hx;

    self->Menus = menulist_DuplicateML(EmbeddedMenus, self);
    self->Keystate = keystate_Create(self, EmbeddedKeymap);
    self->BuildKeystate = NULL;
    menulist_SetMask(self->Menus, ML_nontoolset | ML_nonexpertmode | ML_noshowprintarea);

    self->toolset = NULL;
    self->expertmode = FALSE;
    self->focuschange = FALSE;

    self->objs_size = 8;
    self->objs = (struct figv_oref *)malloc(self->objs_size * sizeof(struct figv_oref));
    FlattenRefList(self, 0);

    self->tmp_size = 0;
    self->tmplist = NULL;

    self->redraw_size = 0;
    self->redrawlist = NULL;

    self->clipreg_size = 0;
    self->clipreglist = NULL;
    IncreaseClipRegProc(self, 1);
    self->tmpregion = region_CreateEmptyRegion();
    self->currentclipreg = NULL;
    self->figureclip = region_CreateEmptyRegion();

    rectangle_EmptyRect(&self->UpdateRect);
    rectangle_EmptyRect(&self->MustEraseRect);

    self->numhighlights = 0;
    self->highlights = NULL;
    figview_SetNumHighlights(self, figview_InitNumHighlights);

    self->focusgroup = figure_NULLREF;
    self->numselected = 0;

    self->originx = 0;
    self->originy = 0;
    self->ppanx = self->panx = 0;
    self->ppany = self->pany = 0;
    self->scale = figview_NormScale;
    self->lastwidth = 0;
    self->lastheight = 0;

    self->lastupdater = figure_NULLREF;

    self->OnScreen = FALSE;
    self->HasInputFocus = FALSE;
    self->embedded = TRUE;
    self->ignoreUp = FALSE;
    self->NeedFullUpdate = FALSE;
    self->DoingFullUpdate = FALSE;
    self->UpdateCached = FALSE;
    self->UpdatesBlocked = FALSE;
    self->InputFocusClick = FALSE;
    self->ShowPrintArea = FALSE;
    self->PSFileName = NULL;
    self->PrintRect = NULL;
    self->ShowFocusAttachments = FALSE; 

    if (DefaultExpertMode)
	figview_SetExpertMode(self, TRUE);

    graphic_GetDefaultColors(&self->ForegroundColor, &self->BackgroundColor);
    if (!self->ForegroundColor)
	self->ForegroundColor = "black";
    if (!self->BackgroundColor)
	self->BackgroundColor = "white";
    self->FigBackColor = FigureBackgroundColor;

    return TRUE;
}

void figview__FinalizeObject(ClassID, self)
struct classheader *ClassID;
struct figview *self;
{
    int ix;

    if (self->PSFileName)
	free(self->PSFileName);

    if (self->tmplist)
	free(self->tmplist);

    if (self->highlights)
	free(self->highlights);

    if (self->redrawlist)
	free(self->redrawlist);

    if (self->clipreglist) {
	for (ix=0; ix<self->clipreg_size; ix++) 
	    if (self->clipreglist[ix])
		region_Destroy(self->clipreglist[ix]);
	free(self->clipreglist);
    }

    if (self->tmpregion)
	region_Destroy(self->tmpregion);
    if (self->figureclip)
	region_Destroy(self->figureclip);

    if (self->toolset) 
	figview_DestroyToolset(self);

    for (ix=0; ix<self->objs_size; ix++) {
	if (self->objs[ix].insetv) 
	    view_Destroy(self->objs[ix].insetv);
    }
    free(self->objs);

    menulist_Destroy(self->Menus);
    keystate_Destroy(self->Keystate);
}

void figview__SetDataObject(self, fig)
struct figview *self;
struct figure *fig;
{
    super_SetDataObject(self, fig);
    self->focusgroup = figure_RootObjRef(fig);
    self->originx = figure_GetOriginX(fig);
    self->originy = figure_GetOriginY(fig);
    self->panx = self->originx;
    self->pany = self->originy;
}

void figview__SetExpertMode(self, val)
struct figview *self;
boolean val;
{
    val = (val) ? TRUE : FALSE;

    if (val==self->expertmode)
	return;

    self->ShowFocusAttachments = val;
    self->expertmode = val;
    self->focuschange = val;
    if (self->toolset)
	figtoolview_SetExpertMode(self->toolset, val);
    figview_ClearSelection(self);
    figview_WantUpdate(self, self);
    RepostMenus(self);
}

static void SetExpertModeProc(self, val)
struct figview *self;
long val;
{
    figview_SetExpertMode(self, val);
}

static void ToggleReadOnlyProc(self, val)
struct figview *self;
long val;
{
    struct figure *fig = (struct figure *)figview_GetDataObject(self);
    if (!fig) return;

    fig->ReadOnly = !fig->ReadOnly;

    if (fig->ReadOnly) 
	message_DisplayString(self, 10, "Document is now read-only.");
    else
	message_DisplayString(self, 10, "Document is now writable.");
}

void figview__ObservedChanged(self, obs, status)
struct figview *self;
struct observable *obs;
long status;
{
    if (obs == (struct observable *)self->toolset) {
	if (status==observable_OBJECTDESTROYED) {
	    fprintf(stderr, "figview: observed toolset destroyed!\n");
	}
	else {
	}
    }
    else {
	if (status == observable_OBJECTDESTROYED) {
	}
	else if (status == figure_DATACHANGED) 
	    figview_WantUpdate(self, self);	
    }
}

struct view *figview__GetApplicationLayer(self)
struct figview *self;
{
    struct scroll *view;

    view = scroll_CreateScroller(self, scroll_LEFT | scroll_BOTTOM, environ_GetProfile("FigureScrollClass"));

    self->embedded = FALSE;
    return (struct view *) view;
}

static void y_getinfo(), y_setframe(), x_getinfo(), x_setframe();
static long y_whatisat(), x_whatisat();
static struct scrollfns	vertical_scroll_interface =
{y_getinfo, y_setframe, NULL, y_whatisat};
static struct scrollfns	horizontal_scroll_interface =
{x_getinfo, x_setframe, NULL, x_whatisat};

struct scrollfns *figview__GetInterface(self, interface_name)
struct figview *self;
char *interface_name;
{
    struct scrollfns *interface;
    
    if (strcmp(interface_name, "scroll,vertical") == 0)
	interface = &vertical_scroll_interface;
    else if (strcmp(interface_name, "scroll,horizontal") == 0)
	interface = &horizontal_scroll_interface;
    else
	interface = NULL;
    return interface;
}

static void x_getinfo(self, total, seen, dot)
struct figview *self;
struct range *total, *seen, *dot;
{
    struct figure *fig = (struct figure *)figview_GetDataObject(self);
    struct rectangle *figrect = figure_GetOverallBounds(fig);
    struct rectangle visrect;
    struct rectangle *focrect;

    if (rectangle_IsEmptyRect(figrect)) {
	total->beg = 0;
	total->end = 1;
	seen->beg = -1;
	seen->end = -1;
	dot->beg = -1;
	dot->end = -1;
    }
    else {
	total->beg = 0;
	total->end = figrect->width + SCROLL_EXTRA_SPACE;

	figview_GetVisualBounds(self, &visrect);
	seen->beg = figview_ToFigX(self, visrect.left) - (figrect->left - SCROLL_EXTRA_SPACE);
	seen->end = figview_ToFigX(self, visrect.left+visrect.width) - (figrect->left - SCROLL_EXTRA_SPACE);

	if (seen->beg < total->beg)
	    seen->beg = total->beg;
	if (seen->end > total->end)
	    seen->end = total->end;

	if (self->focusgroup == figure_RootObjRef(fig))
	    focrect = NULL;
	else
	    focrect = figobj_GetBounds(self->objs[self->focusgroup].o, self);

	if (focrect && !rectangle_IsEmptyRect(focrect)) {
	    dot->beg = focrect->left - (figrect->left - SCROLL_EXTRA_SPACE);
	    dot->end = focrect->left+focrect->width - (figrect->left - SCROLL_EXTRA_SPACE);

	    if (dot->beg < total->beg)
		dot->beg = total->beg;
	    if (dot->end > total->end)
		dot->end = total->end;
	}
	else {
	    dot->beg = -1;
	    dot->end = -1;
	}
    }
}

static long x_whatisat(self, coordinate, outof)
struct figview *self;
long coordinate, outof;
{
    struct figure *fig = (struct figure *)figview_GetDataObject(self);
    struct rectangle *figrect = figure_GetOverallBounds(fig);

    return figview_ToFigX(self, coordinate) - (figrect->left - SCROLL_EXTRA_SPACE);
}

static void x_setframe(self, position, coordinate, outof) 
struct figview *self;
int position;
long coordinate, outof;
{
    long diffpos;
    struct figure *fig = (struct figure *)figview_GetDataObject(self);
    struct rectangle *figrect = figure_GetOverallBounds(fig);

    diffpos = (figview_ToFigX(self, coordinate) - (figrect->left - SCROLL_EXTRA_SPACE)) - position;

    if (diffpos) {
	self->panx -= diffpos;
	/* self->NeedFullUpdate = TRUE; */
	figview_WantUpdate(self, self);
    }
}

static void y_getinfo(self, total, seen, dot)
struct figview *self;
struct range *total, *seen, *dot;
{
    struct figure *fig = (struct figure *)figview_GetDataObject(self);
    struct rectangle *figrect = figure_GetOverallBounds(fig);
    struct rectangle visrect;
    struct rectangle *focrect;

    if (rectangle_IsEmptyRect(figrect)) {
	total->beg = 0;
	total->end = 1;
	seen->beg = -1;
	seen->end = -1;
	dot->beg = -1;
	dot->end = -1;
    }
    else {
	total->beg = 0;
	total->end = figrect->height + SCROLL_EXTRA_SPACE;

	figview_GetVisualBounds(self, &visrect);
	seen->beg = figview_ToFigY(self, visrect.top) - (figrect->top - SCROLL_EXTRA_SPACE);
	seen->end = figview_ToFigY(self, visrect.top+visrect.height) - (figrect->top - SCROLL_EXTRA_SPACE);

	if (seen->beg < total->beg)
	    seen->beg = total->beg;
	if (seen->end > total->end)
	    seen->end = total->end;

	if (self->focusgroup == figure_RootObjRef(fig))
	    focrect = NULL;
	else
	    focrect = figobj_GetBounds(self->objs[self->focusgroup].o, self);

	if (focrect && !rectangle_IsEmptyRect(focrect)) {
	    dot->beg = focrect->top - (figrect->top - SCROLL_EXTRA_SPACE);
	    dot->end = focrect->top+focrect->height - (figrect->top - SCROLL_EXTRA_SPACE);

	    if (dot->beg < total->beg)
		dot->beg = total->beg;
	    if (dot->end > total->end)
		dot->end = total->end;
	}
	else {
	    dot->beg = -1;
	    dot->end = -1;
	}
    }
}

static long y_whatisat(self, coordinate, outof)
struct figview *self;
long coordinate, outof;
{
    struct figure *fig = (struct figure *)figview_GetDataObject(self);
    struct rectangle *figrect = figure_GetOverallBounds(fig);

    return figview_ToFigY(self, coordinate) - (figrect->top - SCROLL_EXTRA_SPACE);
}

static void y_setframe(self, position, coordinate, outof) 
struct figview *self;
int position;
long coordinate, outof;
{
    long diffpos;
    struct figure *fig = (struct figure *)figview_GetDataObject(self);
    struct rectangle *figrect = figure_GetOverallBounds(fig);

    diffpos = (figview_ToFigY(self, coordinate) - (figrect->top - SCROLL_EXTRA_SPACE)) - position;

    if (diffpos) {
	self->pany -= diffpos;
	/* self->NeedFullUpdate = TRUE; */
	figview_WantUpdate(self, self);
    }
}

/* assumes self is input focus */
static void RepostMenus(self)
struct figview *self;
{
    long menumask = 0;
    long val;

    if (self->expertmode)
	menumask |= ML_expertmode;
    else
	menumask |= ML_nonexpertmode;

    if (self->toolset)
	menumask |= ML_toolset;
    else
	menumask |= ML_nontoolset;

    if (self->focuschange)
	menumask |= ML_focuschange;

    if (self->numselected)
	menumask |= ML_selected;

    if (self->ShowPrintArea)
	menumask |= ML_showprintarea;
    else
	menumask |= ML_noshowprintarea;

    val = figview_GetOneSelected(self);
    if (val != figure_NULLREF && figobj_IsInset(self->objs[val].o))
	menumask |= ML_oneinsetselected;

    if (menulist_SetMask(self->Menus, menumask)) {
	figview_PostMenus(self, NULL);
    }
}

void figview__PostMenus(self, ml)
struct figview *self;
struct menulist *ml;
{
    /* Enable the menus for this object. */
    menulist_UnchainML(self->Menus, self);
    if (ml)
	menulist_ChainBeforeML(self->Menus, ml, self);

    super_PostMenus(self, self->Menus);
}

void figview__PostKeyState(self, ks)
struct figview *self;
struct keystate *ks;
{
    /* Enable the keys for this object. */
    struct keystate *newch;

    if (self->BuildKeystate) {
	newch = keystate_AddBefore(self->BuildKeystate, ks);
	newch = keystate_AddAfter(self->Keystate, newch);
	super_PostKeyState(self, newch);
    }
    else {
	newch = keystate_AddAfter(self->Keystate, ks);
	super_PostKeyState(self, newch);
    }
}

void figview__SetBuildKeystate(self, ks)
struct figview *self;
struct keystate *ks;
{
    self->BuildKeystate = ks;
    figview_PostKeyState(self, NULL);
}

void figview__ReceiveInputFocus(self)
struct figview *self;
{
    self->HasInputFocus = TRUE;
    figview_PostKeyState(self, NULL);
    figview_PostMenus(self, NULL);
    /*figview_WantUpdate(self, self);  if there's any visual change */
}

void figview__LoseInputFocus(self)
struct figview *self;
{
    self->HasInputFocus = FALSE;
    /*figview_WantUpdate(self, self);    if there's any visual change */
}

/* kind of dull right now, but we may want to change it later */
enum view_DSattributes figview__DesiredSize(self, width, height, pass, dwidth, dheight)
struct figview *self;
long width;
long height;
enum view_DSpass pass;
long *dwidth;
long *dheight;
{
    return super_DesiredSize(self, width, height, pass, dwidth, dheight);
}

static void IncreaseTmpProc(self, num)
struct figview *self;
long num;
{
    if (self->tmplist == NULL) {
	self->tmp_size = num+8;
	self->tmplist = (long *)malloc(sizeof(long) * self->tmp_size);
    }
    else {
	if (self->tmp_size >= num)
	    return;

	while (self->tmp_size < num)
	    self->tmp_size *= 2;
	self->tmplist = (long *)realloc(self->tmplist, sizeof(long) * self->tmp_size);
    }
}

static void IncreaseRedrawProc(self, num)
struct figview *self;
long num;
{
    if (self->redrawlist == NULL) {
	self->redraw_size = num+8;
	self->redrawlist = (struct figv_redraw_item *)malloc(sizeof(struct figv_redraw_item) * self->redraw_size);
    }
    else {
	if (self->redraw_size >= num)
	    return;

	while (self->redraw_size < num)
	    self->redraw_size *= 2;
	self->redrawlist = (struct figv_redraw_item *)realloc(self->redrawlist, sizeof(struct figv_redraw_item) * self->redraw_size);
    }
}

static void IncreaseClipRegProc(self, num)
struct figview *self;
long num;
{
    int ix;

    if (self->clipreglist == NULL) {
	self->clipreg_size = num+8;
	self->clipreglist = (struct region **)malloc(sizeof(struct region *) * self->clipreg_size);
	for (ix=0; ix<self->clipreg_size; ix++)
	    self->clipreglist[ix] = NULL;
    }
    else {
	if (self->clipreg_size >= num)
	    return;

	ix = self->clipreg_size;
	while (self->clipreg_size < num)
	    self->clipreg_size *= 2;
	self->clipreglist = (struct region **)realloc(self->clipreglist, sizeof(struct region *) * self->clipreg_size);

	for (; ix<self->clipreg_size; ix++)
	    self->clipreglist[ix] = NULL;
    }
}

void figview__SetNumHighlights(self, num)
struct figview *self;
int num;
{
    int oldnum = self->numhighlights;
    int hx;

    if (num <= oldnum)
	return;

    self->numhighlights = num;

    if (self->highlights) {
	self->highlights = (struct figv_highlight *)realloc(self->highlights, self->numhighlights * sizeof(struct figv_highlight));
    }
    else {
	self->highlights = (struct figv_highlight *)malloc(self->numhighlights * sizeof(struct figv_highlight));
    }

    for (hx=oldnum; hx<self->numhighlights; hx++) {
	rectangle_EmptyRect(&self->highlights[hx].r);
	rectangle_EmptyRect(&self->highlights[hx].old);
	self->highlights[hx].oldon = FALSE;
	self->highlights[hx].changed = FALSE;
    }
}

static void FlattenRefList(self, ix)
struct figview *self;
long ix;
{
    for (; ix<self->objs_size; ix++) {
	self->objs[ix].o = NULL;
	self->objs[ix].selected = FALSE;
	self->objs[ix].knownselected = FALSE;
	self->objs[ix].drawnselected = 0;
	self->objs[ix].selectdamaged = FALSE;
	self->objs[ix].insetv = NULL;
	self->objs[ix].inseto = NULL;
	self->objs[ix].insetbmoved = FALSE;
	self->objs[ix].wantupdate = FALSE;
	rectangle_EmptyRect(&(self->objs[ix].insetb));
	rectangle_EmptyRect(&(self->objs[ix].vbox));
	rectangle_EmptyRect(&(self->objs[ix].vselbox));
    }
}

/* convert a rectangle in fig coords to pix coords, expanding it by delta pixels. If delta = recttopix_Exact, do not expand it at all. */
static void RectToPix(self, dest, src, delta)
struct figview *self;
struct rectangle *dest, *src;
long delta;
{
    if (rectangle_IsEmptyRect(src)) {
	rectangle_EmptyRect(dest);
	return;
    }

    if (delta == recttopix_Exact) 
	rectangle_SetRectSize(dest,
			      figview_ToPixX(self, rectangle_Left(src)),
			      figview_ToPixY(self, rectangle_Top(src)), 
			      figview_ToPixW(self, rectangle_Width(src)), 
			      figview_ToPixH(self, rectangle_Height(src)));
    else
	rectangle_SetRectSize(dest,
			      -delta + figview_ToPixX(self, rectangle_Left(src)),
			      -delta + figview_ToPixY(self, rectangle_Top(src)), 
			      2*delta + 2 + figview_ToPixW(self, rectangle_Width(src)), 
			      2*delta + 2 + figview_ToPixH(self, rectangle_Height(src)));
}

/* Let B = UpdateRect clipped to visual bounds. Clip drawing to B, erase B, and redraw all elements (in order) that intersect B. 
If recterased is TRUE, we can assume that the drawing area has been erased already. */
static void RedrawView(self, recterased)
struct figview *self;
boolean recterased;
{
    struct rectangle B, foc;
    struct rectangle inrec;
    struct figure *fig 
      = (struct figure *)figview_GetDataObject(self);
    long hx, ix, jx, numclips, clipnum;
    struct figobj *o;

    /* Check the focus group. Yeah, look, I know it's a cheesy place to do it. */
    {
	struct rectangle newgrprec;
	long ref = figview_GetFocusRef(self);
	if (ref!=figure_NULLREF && ref != figure_RootObjRef(fig)) {
	    struct figogrp *o = (struct figogrp *)fig->objs[ref].o;
	    newgrprec = *figogrp_GetBounds(o, self);
	}
	else
	    rectangle_EmptyRect(&newgrprec);
	if (!rectangle_IsEqualRect(&newgrprec, &self->highlights[0].r))
	    figview_SetHighlight(self, 0, &newgrprec);
    }

    /* the plan: r is in fig coords, old is in pixel coords */

    figview_SetTransferMode(self, graphic_INVERT);
    for (hx=0; hx<self->numhighlights; hx++) {
	self->highlights[hx].focgone = FALSE;

	if (recterased) {
	    /* old one already erased */
	    self->highlights[hx].focgone = TRUE;
	}
	else if (self->DoingFullUpdate || self->highlights[hx].changed) {
	    /* erase old one */
	    self->highlights[hx].focgone = TRUE;
	    if (self->highlights[hx].oldon)
		figview_DrawRect(self, &self->highlights[hx].old);
	}
	if (self->highlights[hx].focgone) {
	    /* defigop new one */
	    if (rectangle_IsEmptyRect(&self->highlights[hx].r))
		self->highlights[hx].oldon = FALSE;
	    else {
		self->highlights[hx].oldon = TRUE;
		RectToPix(self, &self->highlights[hx].old, &self->highlights[hx].r, figview_SpotRad);
	    }
	}
    }

    figview_GetVisualBounds(self, &B);
    rectangle_IntersectRect(&B, &B, &self->UpdateRect);
    self->ClippedUpdateRect=B;
    
    if (!rectangle_IsEmptyRect(&B)) {
	/* the guts of the redraw loop */
	int panpixx, panpixy;
	long patorgx, patorgy;

	/* leetle hack to make sure that grey stipples line up right after bit-blitting. This assumes that selection / attachment / highlighting code (above) does not make use of any such stipples. If that assumption becomes false, this will have to be moved to before that code. */
	/* this also assumes that the patterns used by the grey-drawing functions have a periodicity of 8 pixels. */
	panpixx = 7 & figview_ToPixW(self, self->panx);
	panpixy = 7 & figview_ToPixH(self, self->pany);
	figview_GetPatternOrigin(self, &patorgx, &patorgy);
	figview_SetPatternOrigin(self, patorgx+8-panpixx, patorgy+8-panpixy);

	self->redrawnum = 0;
	DEBUG(("Adding: "));
	RedrawGroup(self, fig, figure_RootObjRef(fig), &B);
	DEBUG(("end.\n"));

	/* IncreaseClipRegProc(1) is already ensured in InitObject(). */
	if (!self->clipreglist[0])
	    self->clipreglist[0] = region_CreateEmptyRegion();
	region_RectRegion(self->clipreglist[0], &B);
	if (self->embedded)
	    region_IntersectRegion(self->clipreglist[0], self->figureclip, self->clipreglist[0]);
	numclips = 1;

	
	/*printf("B is (%d..%d) (%d..%d)\n", B.left, B.left+B.width, B.top, B.top+B.height);*/

	DEBUG(("Clipping:\n"));	
	for (jx=self->redrawnum-1; jx>=0; jx--) {
	    self->redrawlist[jx].clip = numclips-1;
	    ix = self->redrawlist[jx].oref;
	    o = self->objs[ix].o;
	    if (figobj_IsInset(o)) {
		IncreaseClipRegProc(self, numclips+1);
		if (!self->clipreglist[numclips])
		    self->clipreglist[numclips] = region_CreateEmptyRegion();
		inrec = self->objs[ix].insetb;
		rectangle_InsetRect(&inrec, -1, -1);
		region_RectRegion(self->tmpregion, &inrec);
		region_SubtractRegion(self->clipreglist[numclips-1], self->tmpregion, self->clipreglist[numclips]);
		numclips++;
	    }
	}

	figview_SetTransferMode(self, graphic_COPY);
	
	clipnum = numclips-1;
	self->currentclipreg = self->clipreglist[clipnum];
	figview_SetClippingRegion(self, self->currentclipreg);

	if (!recterased) {
	    figview_SetBackgroundColor(self, self->FigBackColor, 65535, 65535, 65535);
	    figview_EraseRect(self, &B); 
	    /*figview_FillRect(self, &B, figview_WhitePattern(self)); */
	}

	DEBUG(("Drawing: %d...", self->redrawnum));
	DEBUG(("[clip %d] ", clipnum));
	for (jx=0; jx<self->redrawnum; jx++) {
	    ix = self->redrawlist[jx].oref;
	    if (clipnum != self->redrawlist[jx].clip) {
		clipnum = self->redrawlist[jx].clip;
		self->currentclipreg = self->clipreglist[clipnum];
		figview_SetClippingRegion(self, self->currentclipreg);
		DEBUG(("[clip %d] ", clipnum));
	    }
	    DEBUG(("%d ", ix));
	    o = self->objs[ix].o;
	    if (figobj_IsGroup(o)) {
	    }
	    else if (figobj_IsInset(o)) {
		figobj_Draw(o, self);
		if (self->objs[ix].insetv) {
		    struct rectangle *currec, tmprec;
		    boolean drawfull = recterased;

		    DEBUG(("%s ", class_GetTypeName(self->objs[ix].insetv)));
		    currec = &(self->objs[ix].insetb);
		    if (self->DoingFullUpdate || self->objs[ix].insetbmoved) {
			view_LinkTree(self->objs[ix].insetv, self);
			view_InsertView(self->objs[ix].insetv, self, currec);
			drawfull = TRUE;
		    }

		    if (!drawfull) {
			rectangle_IntersectRect(&tmprec, &self->MustEraseRect, currec);
			if (!rectangle_IsEmptyRect(&tmprec))
			    drawfull = TRUE;
		    }

		    region_CopyRegion(self->tmpregion, self->clipreglist[clipnum]);
		    region_OffsetRegion(self->tmpregion, -currec->left, -currec->top);
		    view_SetClippingRegion(self->objs[ix].insetv, self->tmpregion);

		    /*printf("ins %d: drawfull = %s\n", ix, (drawfull ? "TRUE" : "FALSE"));*/
		    if (drawfull) {
			figview_SetBackgroundColor(self, self->BackgroundColor, 65535, 65535, 65535);
			figview_EraseRect(self,  &(self->objs[ix].insetb));
			view_FullUpdate(self->objs[ix].insetv, view_FullRedraw, 0, 0, -1, -1);
		    }
		    else
			view_Update(self->objs[ix].insetv);

		}
		else {
		    DEBUG(("NULL "));
		}
	    }
	    else { /* neither inset nor group */
		figobj_Draw(o, self);
	    }
	    if (self->objs[ix].drawnselected)
		self->objs[ix].selectdamaged = TRUE;
	    self->objs[ix].insetbmoved = FALSE;
	}
	DEBUG(("end.\n"));

	figview_SetForegroundColor(self, self->ForegroundColor, 0, 0, 0); 

	/* draw selection bits that were erased by the rectangle wipe */
	for (ix=0; ix<self->objs_size; ix++) {
	    if (self->objs[ix].o && self->objs[ix].selectdamaged) {
		self->objs[ix].selectdamaged = FALSE;
		figobj_Select(self->objs[ix].o, self);
		if (self->objs[ix].drawnselected==2) {
		    figobj_DrawAttachments(self->objs[ix].o, self);
		}
	    }
	}
	figview_SetTransferMode(self, graphic_INVERT);
	for (hx=0; hx<self->numhighlights; hx++) {
	    if (!self->highlights[hx].focgone) {
		if (self->highlights[hx].oldon)
		    figview_DrawRect(self, &self->highlights[hx].old);
	    }
	}

	self->currentclipreg = NULL;
	if (self->embedded)
	    figview_SetClippingRegion(self, self->figureclip);
	else
	    figview_ClearClippingRect(self);

	/* unshift the stipple origin thingie. */
	figview_SetPatternOrigin(self, patorgx, patorgy);

	/* end of the guts of the redraw loop */
    }

    /* redraw remaining selection and attachment bits */
    for (ix=0; ix<self->objs_size; ix++) {
	if (self->objs[ix].o) {
	    struct figobj *o = self->objs[ix].o;
	    /*boolean looksselected = 
	      (self->objs[ix].selected 
	       || (self->ShowFocusAttachments 
		   && figview_GetFocusRef(self)==ix 
		   && ((struct figogrp *)o)->doconstraints));*/

	    /* draw selection bits */
	    if (self->objs[ix].knownselected && self->objs[ix].drawnselected==0) {
		figobj_Select(o, self);
		if (self->ShowFocusAttachments && figobj_GetParentRef(self->objs[ix].o) == figview_GetFocusRef(self)) {
		    figobj_DrawAttachments(self->objs[ix].o, self);
		    self->objs[ix].drawnselected = 2;
		}
		else {
		    self->objs[ix].drawnselected = 1;
		}
	    }
	    /* undraw selection bits */
	    else if (!self->objs[ix].knownselected && self->objs[ix].drawnselected) {
		figobj_Select(o, self);
		if (self->objs[ix].drawnselected == 2) {
		    figobj_DrawAttachments(self->objs[ix].o, self);
		}
		self->objs[ix].drawnselected = 0;
	    }
	}
    }

    /* redraw remaining highlights */
    figview_SetTransferMode(self, graphic_INVERT);
    for (hx=0; hx<self->numhighlights; hx++) {
	if (self->highlights[hx].focgone) {
	    if (self->highlights[hx].oldon)
		figview_DrawRect(self, &self->highlights[hx].old);
	}
    }

    /* erase rectangles in preparation for another go-round */
    rectangle_EmptyRect(&self->UpdateRect);
    rectangle_EmptyRect(&self->MustEraseRect);
    self->DoingFullUpdate = FALSE;
}

static void RedrawGroup(self, fig, gref, B)
struct figview *self;
struct figure *fig;
long gref;
struct rectangle *B;
{
    int ix;
    struct rectangle tmp;
    struct figobj *o;

    /*printf("clipped to %d, %d, %d, %d\n", B->left, B->top, B->width, B->height);*/

    for (ix=gref; ix!=figure_NULLREF; ix=fig->objs[ix].next) {
	/* this is the one usage of vbox that should not be ignored if self->objs[ix].ignorevbox is TRUE. If we clobbered it somehow, we could get rid of ignorevbox. Unfortunately, we can't. */
	rectangle_IntersectRect(&tmp, B, &(self->objs[ix].vbox));
	if (!rectangle_IsEmptyRect(&tmp)) {
	    o = self->objs[ix].o;
	    DEBUG(("%d ", ix));
	    IncreaseRedrawProc(self, self->redrawnum+1);
	    self->redrawlist[self->redrawnum].oref = ix;
	    self->redrawnum++;
	    if (figobj_IsGroup(o)) {
		DEBUG(("("));
		RedrawGroup(self, fig, figogrp_GetRoot((struct figogrp *)o), B);
		DEBUG((") "));
	    }
	}
    }
}

/* update cached object / vbox lists; store old, changed, new rectangles in UpdateRect. */
static void UpdateCache(self, needfull)
struct figview *self;
 /* this indicates a need to update all bounds, but not
  necessarily redraw everything. */
boolean needfull; 
{
    long ix;
    long tstamp, tstampi;
    struct dataobject *dobj;
    short needup;
    struct figure *fig = (struct figure *)figview_GetDataObject(self);
    struct rectangle inrec, *tmprec;
    struct figobj *o;

    if (fig->objs_size > self->objs_size) {
	ix = self->objs_size;
	while (fig->objs_size > self->objs_size)
	    self->objs_size *= 2;
	self->objs = (struct figv_oref *)realloc(self->objs, self->objs_size * sizeof(struct figv_oref));
	FlattenRefList(self, ix);
    }

   /* this section moved to OldUpdateCache
    needfull = self->NeedFullUpdate;
    if (needfull)
	self->DoingFullUpdate = TRUE;
    self->NeedFullUpdate = FALSE;
*/
    for (ix=0; ix<fig->objs_size; ix++) {
	tstamp = 0;
	tstampi = 0;
	needup = 0;
	if (self->objs[ix].o != fig->objs[ix].o) {

	    /* handle old one */
	    if (self->objs[ix].o) {
		DEBUG(("Cache: old %d\n", ix));
		if (self->objs[ix].selected)
		    self->numselected--;
		if (self->objs[ix].drawnselected == 0) {
		    if (!self->objs[ix].ignorevbox) {
			rectangle_UnionRect(&self->MustEraseRect, &self->MustEraseRect, &(self->objs[ix].vbox));
		    }
		}
		else {
		    rectangle_UnionRect(&self->MustEraseRect, &self->MustEraseRect, &(self->objs[ix].vselbox));
		}
		if (ix == self->focusgroup)
		    self->focusgroup = figure_RootObjRef(fig);
	    }

	    /* handle new one */
	    if (fig->objs[ix].o) {
		struct figobj *o = fig->objs[ix].o;
		DEBUG(("Cache: new %d\n", ix));
		self->objs[ix].o = o;
		self->objs[ix].selected = FALSE;
		self->objs[ix].drawnselected = FALSE;

		if (self->objs[ix].selected
		    || (self->ShowFocusAttachments
			&& figobj_IsGroup(o)
			&& figview_GetFocusRef(self)==ix 
			&& ((struct figogrp *)o)->doconstraints))
		    self->objs[ix].knownselected = TRUE;
		else
		    self->objs[ix].knownselected = FALSE;

		RectToPix(self, &self->objs[ix].vbox, figobj_GetBounds(o, self), figview_SpotRad);
		self->objs[ix].ignorevbox = (figobj_IsGroup(o));
		/* if (self->objs[ix].knownselected) then */
		RectToPix(self, &self->objs[ix].vselbox, figobj_GetSelectedBounds(o, self), figview_AnchRad+1);

		if (figobj_IsInset(o)) {
		    RectToPix(self, &inrec, figobj_GetBounds(o, self), recttopix_Exact);
		    self->objs[ix].insetb = inrec;
		    self->objs[ix].insetbmoved = TRUE;
		}

		if (self->objs[ix].knownselected) {
		    rectangle_UnionRect(&self->UpdateRect, &self->UpdateRect, &(self->objs[ix].vselbox));
		}
		else {
		    if (!self->objs[ix].ignorevbox) {
			rectangle_UnionRect(&self->UpdateRect, &self->UpdateRect, &(self->objs[ix].vbox));
		    }
		}

		self->objs[ix].timestamp = figobj_GetModified(o);
	    }
	    else {
		DEBUG(("Cache: new NULL %d\n", ix));
		self->objs[ix].o = NULL;
	    }
	} /* end case self->objs[ix].o != fig->objs[ix].o */

	else if (self->objs[ix].o != NULL) { 

	    /* figure out if old object needs to be changed */
	    if (needfull 
		|| (tstamp=figobj_GetModified(self->objs[ix].o)) > self->objs[ix].timestamp) {
		needup = 1;
	    }
	    if (!needup && (
			    self->objs[ix].wantupdate
			    || (figobj_IsInset(self->objs[ix].o) 
				&& (dobj=figoins_GetDataObject((struct figoins *)self->objs[ix].o))
				&& (tstampi=dataobject_GetModified(dobj)) > self->objs[ix].timestamp))) {
		needup = 2;
		if (self->objs[ix].wantupdate) {
		    figobj_SetModified(self->objs[ix].o);
		    tstamp=figobj_GetModified(self->objs[ix].o);
		    self->objs[ix].wantupdate = FALSE;
		}
	    }
	    if (needup) {
		struct figobj *o = self->objs[ix].o;
		DEBUG(("Cache: changed %d\n", ix));

		if (self->objs[ix].selected
		    || (self->ShowFocusAttachments
			&& figobj_IsGroup(o)
			&& figview_GetFocusRef(self)==ix 
			&& ((struct figogrp *)o)->doconstraints))
		    self->objs[ix].knownselected = TRUE;
		else
		    self->objs[ix].knownselected = FALSE;

		/* add bbox of object and of old image */
		if (needup != 2)
		    if (self->objs[ix].drawnselected == 0) {
			if (!self->objs[ix].ignorevbox) {
			    rectangle_UnionRect(&self->MustEraseRect, &self->MustEraseRect, &(self->objs[ix].vbox));
			}
		    }
		    else {
			rectangle_UnionRect(&self->MustEraseRect, &self->MustEraseRect, &(self->objs[ix].vselbox));
		    }

		RectToPix(self, &self->objs[ix].vbox, figobj_GetBounds(o, self), figview_SpotRad);
		self->objs[ix].ignorevbox = (figobj_IsGroup(o));
		/* if [old] (self->objs[ix].knownselected) then */
		RectToPix(self, &self->objs[ix].vselbox, figobj_GetSelectedBounds(o, self), figview_AnchRad+1);

		if (figobj_IsInset(o)) {
		    RectToPix(self, &inrec, figobj_GetBounds(o, self), recttopix_Exact);
		    if (!rectangle_IsEqualRect(&inrec, &(self->objs[ix].insetb))) {
			self->objs[ix].insetb = inrec;
			self->objs[ix].insetbmoved = TRUE;
		    }
		}

		if (self->objs[ix].knownselected) {
		    rectangle_UnionRect(&self->UpdateRect, &self->UpdateRect, &(self->objs[ix].vselbox));
		}
		else {
		    if (!self->objs[ix].ignorevbox) /* an unselected group has empty bbox */ {
			rectangle_UnionRect(&self->UpdateRect, &self->UpdateRect, &(self->objs[ix].vbox));
		    }
		}

		if (tstampi > tstamp) {
		    tstamp = tstampi;
		}
		if (tstamp > self->objs[ix].timestamp)
		    self->objs[ix].timestamp = tstamp;
	    }
	    else { /* !needup */
		/* we now check for changes in the selection value. If it changes, we may have to update the vselbox size (but not the update rectangles) */
		o = self->objs[ix].o;

		if (self->objs[ix].selected
		    || (self->ShowFocusAttachments
			&& figobj_IsGroup(o)
			&& figview_GetFocusRef(self)==ix 
			&& ((struct figogrp *)o)->doconstraints)) {
		    if (!self->objs[ix].knownselected) 
			self->objs[ix].knownselected = TRUE;
		}
		else {
		    if (self->objs[ix].knownselected) 
			self->objs[ix].knownselected = FALSE;
		}
	    }
	}

	/* entirely separate things to deal with inset bits. Note that o may be NULL in these blocks. */
	o = self->objs[ix].o;
	{
	    char *name;

	    if (o && figobj_IsInset(o))
		dobj = figoins_GetDataObject((struct figoins *)o);
	    else
		dobj = NULL;

	    if (dobj != self->objs[ix].inseto) {
		if (self->objs[ix].insetv) {
		    DEBUG(("Inset: destroying: %s\n", class_GetTypeName(self->objs[ix].insetv))); 
		    view_Destroy(self->objs[ix].insetv);
		    self->objs[ix].insetv = NULL;
		}
		self->objs[ix].inseto = dobj;
		if (dobj) {
		    name = dataobject_ViewName(dobj);
		    DEBUG(("Inset: creating: %s\n", name));
		    if (name)
			self->objs[ix].insetv = (struct view *)class_NewObject(name);
		    if (!self->objs[ix].insetv) {
			fprintf(stderr, "figview: unable to get view!\n");
		    }
		    else {
			view_SetDataObject(self->objs[ix].insetv, dobj);
		    }
		}
	    }
	}
    }
    rectangle_UnionRect(&self->UpdateRect, &self->UpdateRect, &self->MustEraseRect);
}

static void OldUpdateCache(self)
struct figview *self;
{
    boolean needfull = self->NeedFullUpdate;
    if (needfull)
	self->DoingFullUpdate = TRUE;
    self->NeedFullUpdate = FALSE;
    UpdateCache(self, needfull);
}

void figview__FlushDataChanges(self)
struct figview *self;
{
    OldUpdateCache(self);
}

static void UpdateWindowSize(self)
struct figview *self;
{
    struct figobj *foc;
    struct rectangle logrec;
    struct figure *fig = (struct figure *)figview_GetDataObject(self);

    figview_GetLogicalBounds(self, &logrec);
    self->lastwidth = logrec.width;
    self->lastheight = logrec.height;
    if (self->embedded) {
	/* self->figureclip is used if and only if self->embedded. */
	rectangle_InsetRect(&logrec, 1, 1);
	region_RectRegion(self->figureclip, &logrec);
    }

    foc = figure_FindObjectByRef(fig, figure_RootObjRef(fig));
    if (foc 
	 && figobj_IsGroup(foc) 
	 && ((struct figogrp *)foc)->doconstraints) {
	long x1 = figview_ToDefFigX(self, 0);
	long y1 = figview_ToDefFigY(self, 0);
	long x2 = figview_ToDefFigX(self, 0+self->lastwidth);
	long y2 = figview_ToDefFigY(self, 0+self->lastheight);

	figogrp_MoveHandle((struct figogrp *)foc, x1, y1, 3);
	figogrp_MoveHandle((struct figogrp *)foc, x2, y2, 7);
    }

}

static void FixPixelPanning(self)
struct figview *self;
{
    self->ppanx=figview_ToPixW(self, (self)->panx);
    self->ppany=figview_ToPixH(self, (self)->pany);
}

void figview__FullUpdate(self, type, left, top, width, height)
struct figview *self;
enum view_UpdateType type;
long left, top, width, height;
{
    self->UpdateCached = FALSE;
  
    if (figview_GetIM(self)) FixPixelPanning(self);
    
    if (type == view_Remove  
	 ||  figview_GetLogicalWidth(self) == 0 
	 || figview_GetLogicalHeight(self) == 0) {
	/* view_Remove means the view has left the screen.
	 A zero dimension means the view is not visible */
	self->OnScreen = FALSE;
	return;
    }
    
    if (self->lastwidth != figview_GetLogicalWidth(self) 
	 || self->lastheight != figview_GetLogicalHeight(self)) {
	UpdateWindowSize(self);
    }

    figview_SetTransferMode(self, graphic_COPY);
    /* draw inset border */
    if (self->embedded) {
	figview_ClearClippingRect(self);
	figview_DrawRectSize(self, 0, 0, self->lastwidth-1, self->lastheight-1);

	figview_SetClippingRegion(self, self->figureclip);
    }

    switch (type) {
	case view_FullRedraw:
	    self->OnScreen = TRUE;
	    self->DoingFullUpdate = TRUE;
	    OldUpdateCache(self);
	    figview_GetLogicalBounds(self, &self->UpdateRect);
	    figview_SetBackgroundColor(self, self->FigBackColor, 65535, 65535, 65535);
	    figview_EraseRect(self, &self->UpdateRect);
	    RedrawView(self, TRUE); /* area is already erased */
	    break;
	case view_PartialRedraw:
	case view_LastPartialRedraw:
	    self->OnScreen = TRUE;
	    rectangle_SetRectSize(&self->UpdateRect, left, top, width, height);
	    figview_SetBackgroundColor(self, self->FigBackColor, 65535, 65535, 65535);
	    figview_EraseRect(self, &self->UpdateRect); 
	    RedrawView(self, TRUE); /* area is already erased */
	    break;
	default:
	    break;
    };
}

#ifndef ABS
#define ABS(x) ((x<0)?-(x):x)
#endif

static void DoRedraws(self, ux, uy, dr, diffx, diffy)
struct figview *self;
struct rectangle *ux;
struct rectangle *uy;
struct rectangle *dr;
long diffx, diffy;
{
    struct rectangle vb;
    struct region *vr=region_CreateEmptyRegion();
    struct region *vbr;
    struct region *tmp;
    
    if (vr==NULL) return;
    
    figview_GetVisualRegion(self, vr);
    figview_GetVisualBounds(self, &vb);

    vbr=region_CreateRectRegion(&vb);

    if (vbr==NULL) return;

   
    UpdateCache(self, TRUE);
    if (uy->height>0) {
	figview_EraseRect(self, uy);
	figview_FullUpdate(self, view_LastPartialRedraw, uy->left, uy->top, uy->width, uy->height);
	figview_SetTransferMode(self, graphic_COPY);
    }
    if (ux->width>0) {
	figview_EraseRect(self, ux);
	figview_FullUpdate(self, view_LastPartialRedraw, ux->left, ux->top, ux->width, ux->height);
	figview_SetTransferMode(self, graphic_COPY);
    }

    /* is there a better way to detect the case where
     the entire view is visible? */
    if (region_AreRegionsEqual(vr, vbr)) {
	region_Destroy(vr);
	region_Destroy(vbr);
	return;
    }

    tmp=region_CreateEmptyRegion();
    if (tmp==NULL) return;
    
    region_SubtractRegion(vbr, vr, tmp);
    region_OffsetRegion(tmp, diffx, diffy);
    
    region_RectRegion(vbr, dr);

    region_IntersectRegion(vbr, tmp, vr);

    if (!region_IsRegionEmpty(vr)) {
	struct rectangle rr;
	region_GetBoundingBox(vr, &rr);
	if (rr.width>0 && rr.height>0) {
	    figview_EraseRect(self, &rr);

	    figview_FullUpdate(self, view_LastPartialRedraw, rr.left, rr.top, rr.width, rr.height);
	    figview_SetTransferMode(self, graphic_COPY);
	}	 
    }
    region_Destroy(vr);
    region_Destroy(vbr);
    region_Destroy(tmp);	
}

    
static void DoBlit(self, diffx, diffy)
struct figview *self;
long diffx, diffy;
{
    struct rectangle s, ux, uy, vr;
    struct point d;
    struct region *vb = self->tmpregion;
    int border;

    border = (self->embedded ? 1 : 0);
    if (vb) {
	figview_GetVisualRegion(self, vb);
	region_OffsetRegion(vb, diffx, diffy);
    }

    figview_GetVisualBounds(self, &ux);
    figview_GetVisualBounds(self, &uy);
    
    if (diffx<0) {
	s.left=ABS(diffx)+border;
	d.x=0+border;
	s.width = (figview_GetVisualWidth( self ) - s.left) - border;
	ux.left=s.width+border;
	ux.width=s.left-border;
    } else {
	d.x = ABS(diffx)+border;
	s.left=0+border;
	s.width = (figview_GetVisualWidth( self ) - d.x) - border;
	ux.left=0+border;
	ux.width=d.x-border;
    }
    if (diffy<0) {
	s.top=ABS(diffy)+border;
	d.y=0+border;
	s.height = (figview_GetVisualHeight( self ) - s.top) - border;
	uy.top=s.height+border;
	uy.height=s.top-border;
    } else {
	d.y = ABS(diffy)+border;
	s.top=0+border;
	s.height = (figview_GetVisualHeight( self ) - d.y) - border;
	uy.top=0+border;
	uy.height=d.y-border;
    }
    
    figview_SetTransferMode(self, graphic_COPY);
    if (vb) figview_SetClippingRegion(self, vb);
    figview_BitBlt(self, &s, self, &d, NULL);
    if (vb) {
	figview_GetVisualRegion(self, vb);
	if (self->embedded)
	    region_IntersectRegion(vb, self->figureclip, vb);
	figview_SetClippingRegion(self, vb);
    }

    s.left=d.x;
    s.top=d.y;
    DoRedraws(self, &ux, &uy, &s, diffx, diffy);
}

void figview__Update(self)
struct figview *self;
{
    struct figure *fig = (struct figure *)figview_GetDataObject(self);
    long lppanx=self->ppanx;
    long lppany=self->ppany;
    long diffx;
    long diffy;

    self->UpdateCached = FALSE;
  
    if (!fig) return;
    if (!self->OnScreen) return;

    
    if (figview_GetIM(self)) FixPixelPanning(self);
    diffx=lppanx-self->ppanx;
    diffy=lppany-self->ppany;

    if (self->lastwidth != figview_GetLogicalWidth(self) 
	 || self->lastheight != figview_GetLogicalHeight(self)) {
	UpdateWindowSize(self);
    }

    if (diffx || diffy) {
	if (!self->NeedFullUpdate) {
	    if (ABS(diffx)<figview_GetVisualWidth(self) && ABS(diffy)<figview_GetVisualHeight(self)) {
		DoBlit(self, diffx, diffy);
		return;
	    } else self->NeedFullUpdate=TRUE;
	}
    }
    OldUpdateCache(self);
    RedrawView(self, FALSE);
}

/* if the requestor is an inset, we have to find the objref corresponding to it. This is a linear search. self->lastupdated is a cheap hack to speed this up. */
void figview__WantUpdate(self, requestor)
struct figview *self;
struct view *requestor;
{
    if ((struct view *)self != requestor) {
	int ix;

	ix = self->lastupdater;
	if (ix>=0 && ix<self->objs_size && self->objs[ix].o && self->objs[ix].insetv==requestor) {
	    /*figobj_SetModified(self->objs[ix].o);*/
	    self->objs[ix].wantupdate = TRUE;
	}
	else for (ix=0; ix<self->objs_size; ix++) {
	    if (self->objs[ix].o && self->objs[ix].insetv==requestor) {
		self->lastupdater = ix;
		/*figobj_SetModified(self->objs[ix].o);*/
		self->objs[ix].wantupdate = TRUE;
		break;
	    }
	};
	super_WantUpdate(self, self);
    }
    else {
	if (!self->UpdateCached) {
	    self->UpdateCached = TRUE;
	    /* if blocking is on, we leave UpdateCached on but don't pass the update request; it'll be passed on when the blockage is turned off. */
	    if (!self->UpdatesBlocked)
		super_WantUpdate(self, requestor);
	}
    }
}

void figview__BlockUpdates(self, val)
struct figview *self;
boolean val;
{
    if (val) {
	/* turn blocking on */
	if (self->UpdatesBlocked)
	    return;
	self->UpdatesBlocked = TRUE;
    }
    else {
	/* turn blocking off */
	 if (!self->UpdatesBlocked)
	     return;
	 self->UpdatesBlocked = FALSE;
	 if (self->UpdateCached) {
	     super_WantUpdate(self, self); /* super_ to avoid the mess above; it might be more correct to turn off UpdatesCached and then call figview_WantUpdate(), but this will work */
	 }
    }
}

static boolean TEI_Splot(o, ref, self, vv)
struct figobj *o;
long ref;
struct figure *self;
long *vv;
{
    if (figobj_IsInset(o) 
	 && (int)(figobj_HitMe(o, vv[1], vv[2], 0, NULL)) >= (int)(figobj_HitInside))
	vv[0] = ref;

    return FALSE;
}

struct view *figview__Hit(self, action, x, y, num_clicks)
struct figview *self;
enum view_MouseAction action;
long x, y, num_clicks;
{
    if (! self->OnScreen) return NULL;

    /*if (action == view_NoMouseEvent)
	return (struct view *)self;*/
    if (!self->toolset || self->toolset->toolnum==1) /* 1==pan ### */ {
	long px, py, vwid, vhgt, oref[3];
	long vx, vy;
	struct figure *fig;

	vwid = figview_GetLogicalWidth(self);
	vhgt = figview_GetLogicalHeight(self);
	vx = figview_ToFigX(self, x+figview_ToPixW(self,1)/2);
	vy = figview_ToFigY(self, y+figview_ToPixH(self,1)/2);

	if (action==view_LeftDown || action==view_RightDown) {
	    fig = (struct figure *)figtoolview_GetDataObject(self);

	    oref[0] = figure_NULLREF;
	    oref[1] = vx;
	    oref[2] = vy;
	    figure_EnumerateObjectTree(fig, figure_NULLREF, NULL, FALSE, TEI_Splot, oref);
	    if (oref[0] != figure_NULLREF) {
		/*struct figoins *o = (struct figoins *)figure_FindObjectByRef(fig, oref);*/
		struct view *vo = self->objs[oref[0]].insetv;
		if (!vo) {
		    message_DisplayString(self, 10, "This inset object has no inset.");
		    return (struct view *)self;
		}
		x = view_EnclosedXToLocalX(vo, x);
		y = view_EnclosedYToLocalY(vo, y);

		return view_Hit(vo, action, x, y, num_clicks);
	    }
	}

	if (!self->HasInputFocus) {
	    self->InputFocusClick = TRUE;
	    figview_WantInputFocus(self, self);
	    return (struct view *)self;
	}
	if (self->InputFocusClick) {
	    if (action==view_LeftUp || action==view_RightUp)
		self->InputFocusClick = FALSE;
	    return (struct view *)self;
	}

	switch (action) {
	    case view_LeftDown:
	    case view_RightDown:
		self->rockx = vx;
		self->rocky = vy;
		px = figview_ToPixX(self, vx);
		py = figview_ToPixY(self, vy);
		figview_SetTransferMode(self, graphic_INVERT);
		figview_MoveTo(self, 0, py);
		figview_DrawLineTo(self, vwid-1, py);
		figview_MoveTo(self, px, 0);
		figview_DrawLineTo(self, px, vhgt-1);
		self->lastx = px;
		self->lasty = py;
		break;
	    case view_LeftMovement:
	    case view_RightMovement:
		px = figview_ToPixX(self, vx);
		py = figview_ToPixY(self, vy);
		if (px!=self->lastx || py!=self->lasty) {
		    figview_SetTransferMode(self, graphic_INVERT);
		    figview_MoveTo(self, 0, self->lasty);
		    figview_DrawLineTo(self, vwid-1, self->lasty);
		    figview_MoveTo(self, self->lastx, 0);
		    figview_DrawLineTo(self, self->lastx, vhgt-1);
		    figview_MoveTo(self, 0, py);
		    figview_DrawLineTo(self, vwid-1, py);
		    figview_MoveTo(self, px, 0);
		    figview_DrawLineTo(self, px, vhgt-1);
		    self->lastx = px;
		    self->lasty = py;
		}
		break;
	    case view_LeftUp:
	    case view_RightUp:
		figview_SetTransferMode(self, graphic_INVERT);
		figview_MoveTo(self, 0, self->lasty);
		figview_DrawLineTo(self, vwid-1, self->lasty);
		figview_MoveTo(self, self->lastx, 0);
		figview_DrawLineTo(self, self->lastx, vhgt-1);
		self->panx += (self->rockx - vx);
		self->pany += (self->rocky - vy);
		/* self->NeedFullUpdate = TRUE; */
		figview_WantUpdate(self, self);
		break;
	}
    }
    else {
	struct view *(*proc)();

	if (!self->HasInputFocus) {
	    self->InputFocusClick = TRUE;
	    figview_WantInputFocus(self, self);
	    return (struct view *)self;
	}
	if (self->InputFocusClick) {
	    if (action==view_LeftUp || action==view_RightUp)
		self->InputFocusClick = FALSE;
	    return (struct view *)self;
	}

	proc = figtoolview_GetToolProc(self->toolset);
	x = figview_ToFigX(self, x+figview_ToPixW(self,1)/2);
	y = figview_ToFigY(self, y+figview_ToPixH(self,1)/2);
	switch (action) {
	    case view_LeftDown:
	    case view_RightDown:
		self->lastx = x;
		self->lasty = y;
		break;
	    case view_LeftMovement:
	    case view_RightMovement:
		if (self->lastx == x && self->lasty == y)
		    proc = NULL;
		else {
		    self->lastx = x;
		    self->lasty = y;
		}
		break;
	    default:
		break;
	};
	if (proc) {
	    return (*proc)(self->toolset, action, x, y, num_clicks);
	}
    }

    return (struct view *)self;		/* where to send subsequent hits */
}

boolean figview__IsSelected(self, ref)
struct figview *self;
long ref;
{
    if (ref<0 || ref>=self->objs_size) 
	return FALSE;
    return (self->objs[ref].o && self->objs[ref].selected);
}

void figview__ClearSelection(self)
struct figview *self;
{
    int ix;

    for (ix=0; ix<self->objs_size; ix++) {
	self->objs[ix].selected = FALSE;
    }
    self->numselected = 0;
    RepostMenus(self);
}

void figview__Select(self, o)
struct figview *self;
struct figobj *o;
{
    struct figure *fig = (struct figure *)figview_GetDataObject(self);
    long ref = figure_FindRefByObject(fig, o);

    if (ref != figure_NULLREF)
	figview_SelectByRef(self, ref);
}

void figview__SelectByRef(self, ref)
struct figview *self;
long ref;
{
    if (ref<0 || ref>=self->objs_size) 
	return;

    if (!self->objs[ref].selected) {
	self->objs[ref].selected = TRUE;
	self->numselected++;
	RepostMenus(self);
    }
}

void figview__ToggleSelect(self, o)
struct figview *self;
struct figobj *o;
{
    struct figure *fig = (struct figure *)figview_GetDataObject(self);
    long ref = figure_FindRefByObject(fig, o);

    if (ref != figure_NULLREF)
	figview_ToggleSelectByRef(self, ref);
}

void figview__ToggleSelectByRef(self, ref)
struct figview *self;
long ref;
{
    if (ref<0 || ref>=self->objs_size) 
	return;

    if (!self->objs[ref].selected) {
	self->objs[ref].selected = TRUE;
	self->numselected++;
    }
    else {
	self->objs[ref].selected = FALSE;
	self->numselected--;
    }
    RepostMenus(self);
}

void figview__Unselect(self, o)
struct figview *self;
struct figobj *o;
{
    struct figure *fig = (struct figure *)figview_GetDataObject(self);
    long ref = figure_FindRefByObject(fig, o);

    if (ref != figure_NULLREF) 
	figview_UnselectByRef(self, ref);
}

void figview__UnselectByRef(self, ref)
struct figview *self;
long ref;
{
    if (ref<0 || ref>=self->objs_size) 
	return;

    if (self->objs[ref].selected) {
	self->objs[ref].selected = FALSE;
	self->numselected--;
	RepostMenus(self);
    }
}

/* if there is exactly one object selected, return its ref. Otherwise, return figure_NULLREF */
long figview__GetOneSelected(self)
struct figview *self;
{
    /* could be more efficient */
    int ix;

    if (self->numselected != 1)
	return figure_NULLREF;

    for (ix=0; ix<self->objs_size; ix++)
	if (self->objs[ix].o && self->objs[ix].selected) 
	    return ix;

    return figure_NULLREF; /* should never happen, but what the hell */
}

static void EnumSelSplot(self, fig, grp, func, rock)
struct figview *self;
struct figure *fig;
long grp;
void (*func)();
long rock;
{
    long ix;
    struct figogrp *gr = (struct figogrp *)fig->objs[grp].o;
    struct figobj *this;

    for (ix=figogrp_GetRoot(gr); ix!=figure_NULLREF; ix=fig->objs[ix].next) {

	this=fig->objs[ix].o;
	if (self->objs[ix].selected) {
	    (*func)(this, ix, self, rock);
	}

	if (figobj_IsGroup(this)) {
	    EnumSelSplot(self, fig, ix, func, rock);
	}
    }
}

/* enumerate in order from back to front
func should be of the form
void func(struct figobj *o, long ref, struct figview *self, rock)
*/
void figview__EnumerateSelection(self, func, rock)
struct figview *self;
void (*func)();
long rock;
{
    long grp;
    struct figure *fig = (struct figure *)figview_GetDataObject(self);
    if (!fig) return;

    grp = figure_RootObjRef(fig);
    if (grp==figure_NULLREF) return;

    if (self->objs[grp].selected) {
	(*func)(fig->objs[grp].o, grp, self, rock);
    }
    EnumSelSplot(self, fig, grp, func, rock);
}

/* if ref==figure_NULLREF, set to root group */
void figview__SetFocusByRef(self, ref)
struct figview *self;
long ref;
{
    struct figure *fig = (struct figure *)figview_GetDataObject(self);
    if (!fig) return;

    if (ref==figure_NULLREF)
	ref = figure_RootObjRef(fig);
    else if (!figobj_IsGroup(fig->objs[ref].o)) {
	/* not a group */
	return;
    }

    if (ref==figure_NULLREF || !figobj_IsGroup(fig->objs[ref].o)) {
	/* root object is not a group */
	ref = figure_NULLREF;
    }

    self->focusgroup = ref;
    if (ref!=figure_NULLREF) {
	struct figogrp *o = (struct figogrp *)fig->objs[ref].o;
	if (figogrp_GetRoot(o) == figure_NULLREF) 
	    message_DisplayString(self, 0, "The focus is on an empty group.");
	else
	    message_DisplayString(self, 0, "");
	if (ref == figure_RootObjRef(fig))
	    figview_SetHighlightSize(self, 0, 0, 0, -1, -1);
	else
	    figview_SetHighlight(self, 0, figogrp_GetBounds(o, self));
    }
}

static void AbortObjectProc(self, rock)
struct figview *self;
long rock;
{
    if (self->toolset)
	figtoolview_AbortObjectBuilding(self->toolset);
    else
	message_DisplayString(self, 10, "No object is being created.");
}

static void FocusUpProc(self, rock)
struct figview *self;
long rock;
{
    long old, ref;

    if (!self->focuschange) return;

    old = self->focusgroup;
    if (old==figure_NULLREF) {
	figview_SetFocusByRef(self, figure_NULLREF);
	figview_SelectByRef(self, self->focusgroup);
	figview_WantUpdate(self, self);
	return;
    }

    self->focussib = 0; /* ### not right */
    figview_ClearSelection(self);

    ref = figobj_GetParentRef(self->objs[old].o);
    if (ref==figure_NULLREF) {
	message_DisplayString(self, 10, "Cannot go up; the focus is on the root group.");
    }
    else {
	figview_SetFocusByRef(self, ref);
	/*figview_SelectByRef(self, old);*/
    }
    figview_WantUpdate(self, self);
}

static void FocusDownProc(self, rock)
struct figview *self;
long rock;
{
    long old, ref;
    struct figogrp *foc;
    struct figure *fig = (struct figure *)figview_GetDataObject(self);
    if (!fig) return;

    if (!self->focuschange) return;

    old = self->focusgroup;
    foc = (struct figogrp *)fig->objs[old].o;
    
    for (ref=figogrp_GetRoot(foc); ref!=figure_NULLREF; ref=fig->objs[ref].next)
	if (figobj_IsGroup(fig->objs[ref].o))
	    break;

    if (ref==figure_NULLREF) {
	message_DisplayString(self, 10, "Cannot go down; this group contains no groups.");
	return;
    }

    figview_SetFocusByRef(self, ref);
    self->focussib = 0;
    figview_ClearSelection(self);
    figview_WantUpdate(self, self);
}

static void FocusLeftProc(self, rock)
struct figview *self;
long rock;
{
    long old, ref, fref;
    long count;
    struct figogrp *paro;
    struct figure *fig = (struct figure *)figview_GetDataObject(self);
    if (!fig) return;

    if (!self->focuschange) return;

    old = self->focusgroup;

    ref = figobj_GetParentRef(self->objs[old].o);
    if (ref==figure_NULLREF) {
	return;
    }

    paro = (struct figogrp *)fig->objs[ref].o;

    if (self->focussib==0) {
	count = 0;
	for (ref=figogrp_GetRoot(paro); ref!=figure_NULLREF; ref=fig->objs[ref].next)
	    if (figobj_IsGroup(fig->objs[ref].o)) {
		fref = ref;
		count++;
	    }
	self->focussib = count-1;
	ref = fref;
    }
    else {
	self->focussib--;
	count = 0;
	for (ref=figogrp_GetRoot(paro); ref!=figure_NULLREF; ref=fig->objs[ref].next)
	    if (figobj_IsGroup(fig->objs[ref].o)) 
		if (count++ == self->focussib) break;
    }

    figview_SetFocusByRef(self, ref);
    figview_ClearSelection(self);
    figview_WantUpdate(self, self);
}

static void FocusRightProc(self, rock)
struct figview *self;
long rock;
{
    long old, ref;
    long count;
    struct figogrp *paro;
    struct figure *fig = (struct figure *)figview_GetDataObject(self);
    if (!fig) return;

    if (!self->focuschange) return;

    old = self->focusgroup;

    ref = figobj_GetParentRef(self->objs[old].o);
    if (ref==figure_NULLREF) {
	return;
    }

    paro = (struct figogrp *)fig->objs[ref].o;

    count = 0;
    for (ref=figogrp_GetRoot(paro); ref!=figure_NULLREF; ref=fig->objs[ref].next)
	if (figobj_IsGroup(fig->objs[ref].o))
	    if (count++ > self->focussib) break;

    if (ref==figure_NULLREF) {
	for (ref=figogrp_GetRoot(paro); ref!=figure_NULLREF; ref=fig->objs[ref].next)
	    if (figobj_IsGroup(fig->objs[ref].o))
		break;
	self->focussib = 0;
    }
    else {
	self->focussib++;
    }

    figview_SetFocusByRef(self, ref);
    figview_ClearSelection(self);
    figview_WantUpdate(self, self);
}

void figview__CutNPaste(self, operation, rock)
struct figview *self;
short operation;
long rock; /* currently unused */
{
    switch (operation) {
	case figview_OpCopy:
	    CopySelProc(self, rock);
	    break;
	case figview_OpCopyInset:
	    CopySelInsetProc(self, rock);
	    break;
	case figview_OpCut:
	    CutSelProc(self, rock);
	    break;
	case figview_OpPaste:
	    PasteSelProc(self, rock);
	    break;
	case figview_OpPasteRotate:
	    RotatePasteProc(self, rock);
	    break;
	default:
	    break;
    }
}

static void CutSelSplot(self, fig, gref, fp)
struct figview *self;
struct figure *fig;
long gref;
FILE *fp;
{
    long ix;
    struct figogrp *gr = (struct figogrp *)fig->objs[gref].o;
    struct figobj *this;

    for (ix=figogrp_GetRoot(gr); ix!=figure_NULLREF; ix=fig->objs[ix].next) {
	this=fig->objs[ix].o;
	if (self->objs[ix].selected) {
	    IncreaseTmpProc(self, self->tmpnum+1);
	    self->tmplist[self->tmpnum] = ix;
	    self->tmpnum++;
	}
	else if (figobj_IsGroup(this)) {
	    CutSelSplot(self, fig, ix, fp);
	}
    }
}

static void CutSelProc(self, rock)
struct figview *self;
long rock;
{
    struct figure *fig = (struct figure *)figview_GetDataObject(self);
    long ref;
    int ix;
    FILE *fp;
    struct figobj *this;
    struct dataobject *dobj;
    struct point tmppt;

    long cutbuf_writeid = im_GetWriteID();

    if (!fig) return;
    if (figure_GetReadOnly(fig)) {
	CopySelProc(self, rock);
	return;
    }
    figview_FlushDataChanges(self);
    if (self->numselected==0)
	return;
    
    self->tmpnum = 0;
    ref = figure_RootObjRef(fig);
    if (ref==figure_NULLREF)
	return;
    CutSelSplot(self, fig, ref, NULL);

    fp = im_ToCutBuffer(figview_GetIM(self)); 
    figure_WritePartial(fig, fp, cutbuf_writeid, 0, self->tmplist, self->tmpnum, &tmppt);
    self->lastpaste[0] = tmppt.x;
    self->lastpaste[1] = tmppt.y;
    self->lastpasteoffset = (-1);
    im_CloseToCutBuffer(figview_GetIM(self), fp); 

    for (ix=0; ix<self->tmpnum; ix++) {
	this=fig->objs[self->tmplist[ix]].o;
	figure_DeleteObject(fig, this);
	figobj_Destroy(this);
    }

    figview_ClearSelection(self);
    figure_NotifyObservers(fig, figure_DATACHANGED);
    RepostMenus(self);
}

static void CopySelProc(self, rock)
struct figview *self;
long rock;
{
    struct figure *fig = (struct figure *)figview_GetDataObject(self);
    long ref;
    FILE *fp;
    struct figobj *this;
    struct dataobject *dobj;
    struct point tmppt;

    long cutbuf_writeid = im_GetWriteID();

    if (!fig) return;
    figview_FlushDataChanges(self);
    if (self->numselected==0)
	return;
    
    self->tmpnum = 0;
    ref = figure_RootObjRef(fig);
    if (ref==figure_NULLREF)
	return;
    CutSelSplot(self, fig, ref, NULL);

    fp = im_ToCutBuffer(figview_GetIM(self)); 
    figure_WritePartial(fig, fp, cutbuf_writeid, 0, self->tmplist, self->tmpnum, &tmppt);
    self->lastpaste[0] = tmppt.x;
    self->lastpaste[1] = tmppt.y;
    self->lastpasteoffset = 0;

    im_CloseToCutBuffer(figview_GetIM(self), fp); 
}

static void CopySelInsetProc(self, rock)
struct figview *self;
long rock;
{
    struct figure *fig = (struct figure *)figview_GetDataObject(self);
    long ref;
    FILE *fp;
    struct dataobject *dobj;

    long cutbuf_writeid = im_GetWriteID();

    if (!fig) return;
    figview_FlushDataChanges(self);

    ref = figview_GetOneSelected(self);
    if (ref == figure_NULLREF || !figobj_IsInset(self->objs[ref].o)) {
	message_DisplayString((struct view *)self, 10, "There must be exactly one inset selected.");
	return;
    }
    dobj = figoins_GetDataObject((struct figoins *)self->objs[ref].o);
    if (!dobj) 
	return;

    fp = im_ToCutBuffer(figview_GetIM(self)); 
    dataobject_Write(dobj, fp, cutbuf_writeid, 0);
    im_CloseToCutBuffer(figview_GetIM(self), fp); 
}

static void RotatePasteProc(self, rock)
struct figview *self;
long rock;
{
    struct figure *fig = (struct figure *)figview_GetDataObject(self);
    long ref;
    int ix;
    struct figobj *this;
    struct dataobject *dobj;
    struct point tmppt;

    if (!fig) return;
    if (figure_GetReadOnly(fig)) {
	message_DisplayString((struct view *)self, 10, "Document is read-only.");
	return;
    }
    figview_FlushDataChanges(self);
    
    self->tmpnum = 0;
    ref = figure_RootObjRef(fig);
    if (ref==figure_NULLREF)
	return;
    CutSelSplot(self, fig, ref, NULL);

    for (ix=0; ix<self->tmpnum; ix++) {
	this=fig->objs[self->tmplist[ix]].o;
	figure_DeleteObject(fig, this);
	figobj_Destroy(this);
    }

    PasteSelProc(self, rock);
    im_RotateCutBuffers(figview_GetIM(self), 1);
}

static void PasteSelProc(self, rock)
struct figview *self;
long rock;
{
    struct figure *fig = (struct figure *)figview_GetDataObject(self);
    FILE *fp;
    static char hdr[] = "\\begindata{figure,";
    char *hx = hdr;
    int c, ix;
    long foc, count1, count2, offx, offy, snap;
    struct figogrp *o;
    struct point tmppt;

    if (!fig) return;
    if (figure_GetReadOnly(fig)) {
	message_DisplayString((struct view *)self, 10, "Document is read-only.");
	return;
    }
    fp = im_FromCutBuffer(figview_GetIM(self));

    /* if the file does not begin with a fig, we may as well scan until we find one. Well, actually that's not so cool. */

    while ((c = getc(fp)) != EOF && *hx)
	if (c == *hx) hx++;
	else hx = (c == *hdr) ? hdr+1 : hdr;

    if (*hx) {
	message_DisplayString((struct view *)self, 0, "No fig found in cut buffer.");
	im_CloseFromCutBuffer(figview_GetIM(self), fp);
	return;
    }

    /* skip to end of header */
    ix = 0;
    while ((c=getc(fp)) != '\n' && c != EOF);

    foc = figview_GetFocusRef(self);
    count1 = figure_GetObjectCounter(fig);
    ix = figure_ReadPartial(fig, fp, 0, foc, &tmppt);
    if (self->lastpaste[0] == tmppt.x && self->lastpaste[1] == tmppt.y) {
	self->lastpasteoffset++;
    }
    else {
	self->lastpaste[0] = tmppt.x;
	self->lastpaste[1] = tmppt.y;
	self->lastpasteoffset = 0;
    }

    count2 = figure_GetObjectCounter(fig);
    if (ix!=dataobject_NOREADERROR) {
	message_DisplayString(self, 0, "Unable to read fig from cut buffer.");
	im_CloseFromCutBuffer(figview_GetIM(self), fp);
	return;
    }
    im_CloseFromCutBuffer(figview_GetIM(self), fp);

    figview_FlushDataChanges(self);
    figview_ClearSelection(self);

    if (self->toolset)
	snap = figtoolview_GetSnapGrid(self->toolset);
    else
	snap = 0;
    if (snap) {
	offx = 128 + snap - (128%snap);
	offy = 64 + snap - (64%snap);
    }
    else {
	offx = 128;
	offy = 64;
    }
    o = (struct figogrp *)figure_FindObjectByRef(fig, foc);
    for (ix=figogrp_GetRoot(o); ix!=figure_NULLREF; ix=fig->objs[ix].next) {
	if (fig->objs[ix].counter >= count1 && fig->objs[ix].counter < count2) {
	    figobj_Reposition(figure_FindObjectByRef(fig, ix), self->lastpasteoffset*offx, self->lastpasteoffset*offy);   
	    figview_SelectByRef(self, ix);
	}
    }

    figure_NotifyObservers(fig, figure_DATACHANGED);
}

static void ShowPrintAreaProc(self, rock)
struct figview *self;
long rock; /* 0 for off, 1 for on, 2 for recalc */
{
    long wpts, hpts;
    struct figure *fig = (struct figure *)figview_GetDataObject(self);
    struct rectangle *rtmp;

    if (rock) {
	if (rock==2 && !self->ShowPrintArea)
	    return;

	wpts = 540 * figview_FigUPerPix;
	hpts = 648 * figview_FigUPerPix;
	wpts = (double)wpts / figure_GetPrintScaleX(fig);
	hpts = (double)hpts / figure_GetPrintScaleY(fig);
	figview_SetHighlightSize(self, 1, self->originx, self->originy, wpts, hpts);
	self->ShowPrintArea = TRUE;
	RepostMenus(self);
	figview_WantUpdate(self, self);
    }
    else {
	if (self->ShowPrintArea) {
	    figview_SetHighlightSize(self, 1, 0, 0, -1, -1);
	    self->ShowPrintArea = FALSE;
	    RepostMenus(self);
	    figview_WantUpdate(self, self);
	}
    }
}

static void SetPrintScaleProc(self, rock)
struct figview *self;
long rock;
{
    char buffer[64];
    char obuffer[256];
    int res;
    struct rectangle *rtmp;
    double newx, newy;
    long wpts, hpts;
    struct figure *fig = (struct figure *)figview_GetDataObject(self);
    if (!fig) return;

    if (figure_GetPrintScaleX(fig) == figure_GetPrintScaleY(fig)) 
	sprintf(obuffer, "Print scale is %.2f.  New width scale [%.2f]: ", figure_GetPrintScaleX(fig), figure_GetPrintScaleX(fig));
    else
	sprintf(obuffer, "Print scale is %.2f by %.2f.  New width scale [%.2f]: ", figure_GetPrintScaleX(fig), figure_GetPrintScaleY(fig), figure_GetPrintScaleX(fig));

    res = message_AskForString (self, 40, obuffer, NULL, buffer, 30); 
    if (res<0) {
	message_DisplayString(self, 10, "Cancelled.");
	return;
    }
    if (strlen(buffer)==0)
	newx = figure_GetPrintScaleX(fig);
    else {
	newx = atof(buffer);
	if (newx <= 0.0) {
	    message_DisplayString(self, 10, "Scale must be a positive number.");
	    return;
	}
    }

    sprintf(obuffer, "Width scale is %.2f.  New height scale [same]: ",	newx);
    res = message_AskForString (self, 40, obuffer, NULL, buffer, 30); 
    if (res<0) {
	message_DisplayString(self, 10, "Cancelled.");
	return;
    }
    if (strlen(buffer)==0)
	newy = newx;
    else {
	newy = atof(buffer);
	if (newy <= 0.0) {
	    message_DisplayString(self, 10, "Scale must be a positive number.");
	    return;
	}
    }

    if (newx != figure_GetPrintScaleX(fig) || newy != figure_GetPrintScaleY(fig)) {
	figure_SetPrintScale(fig, newx, newy);
	ShowPrintAreaProc(self, 2);
	figure_SetModified(fig);
	figure_NotifyObservers(fig, figure_DATACHANGED);
    }

    rtmp = figure_GetOverallBounds(fig);
    wpts = figview_ToPrintPixX(self, rtmp->left+rtmp->width)+12;
    hpts = figview_ToPrintPixY(self, rtmp->top+rtmp->height)+12;
    if (wpts<1) wpts = 1;
    if (hpts<1) hpts = 1;
    newx = (double)wpts * figure_GetPrintScaleX(fig) / 72.0;
    newy = (double)hpts * figure_GetPrintScaleY(fig) / 72.0;

    if (figure_GetPrintScaleX(fig) == figure_GetPrintScaleY(fig)) 
	sprintf(obuffer, "Print scaling is now %.2f  (document will print %.2f by %.2f in.)\n", figure_GetPrintScaleX(fig), newx, newy);
    else
	sprintf(obuffer, "Print scaling is now %.2f by %.2f  (document will print %.2f by %.2f in.)\n", figure_GetPrintScaleX(fig), figure_GetPrintScaleY(fig), newx, newy);
    message_DisplayString(self, 10, obuffer);
}

static void ReadZipProc(self, rock)
struct figview *self;
long rock;
{
    int res;
    long count1, count2, ix;
    double ratio;
    char buffer[296], obuffer[296], scbuffer[32];
    char *ctmp;
    boolean eofl;
    FILE *fl;
    struct figogrp *o;
    struct figure *fig = (struct figure *)figview_GetDataObject(self);
    if (!fig) return;

    res = completion_GetFilename(self, "Read Zip file: ", NULL, buffer, 255, FALSE, FALSE);
    if (res==(-1) || strlen(buffer)==0) {
	message_DisplayString(self, 10, "Cancelled.");
	return;
    }

    res = message_AskForString(self, 40, "Scale Zip document by [1.00]: ", NULL, scbuffer, 30); 
    if (res<0) {
	message_DisplayString(self, 10, "Cancelled.");
	return;
    }
    if (strlen(scbuffer)==0)
	ratio = 1.0;
    else {
	ratio = atof(scbuffer);
	if (ratio <= 0.0) {
	    message_DisplayString(self, 10, "Scale must be a positive number.");
	    return;
	}
    }

    fl = fopen(buffer, "r");
    if (!fl) {
	sprintf(obuffer, "Unable to open file '%s'.", buffer);
	message_DisplayString(self, 10, obuffer);
	return;
    }

    /* snarf off \begindata */ 
    strcpy(buffer, "");
    eofl = FALSE;
    ctmp = fgets(buffer, 294, fl);
    res = sscanf(buffer, "\\begindata{%[^,],%ld}", obuffer, &ix);
    if (res!=2) {
	/* treat as raw Zip data */
	rewind(fl);
    }
    else {
	/* ATK datastream */
	while (!eofl && strcmp(obuffer, "zip")) {
	    do {
		ctmp = fgets(buffer, 294, fl);
		if (!ctmp) eofl=TRUE;
		res = sscanf(buffer, "\\begindata{%[^,],%ld}", obuffer, &ix);
	    } while (!eofl && res!=2);
	}
    }

    if (eofl)
	res = dataobject_NOTATKDATASTREAM;
    else {
	count1 = figure_GetObjectCounter(fig);
	res = figio_ReadZipFile(fl, fig, figview_GetFocusRef(self), ratio);
	count2 = figure_GetObjectCounter(fig);
    }

    switch (res) {
	case dataobject_NOREADERROR:
	    message_DisplayString(self, 10, "Zip data read in.");
	    break;
	case dataobject_PREMATUREEOF:
	    message_DisplayString(self, 10, "Error: unexpected end of file.");
	    break;
	case dataobject_NOTATKDATASTREAM:
	    message_DisplayString(self, 10, "Error: apparently not Zip file.");
	    break;
	case dataobject_MISSINGENDDATAMARKER:
	    message_DisplayString(self, 10, "Error: missing enddata line.");
	    break;
	case dataobject_OBJECTCREATIONFAILED:
	    message_DisplayString(self, 10, "Error: unable to create an object.");
	    break;
	case dataobject_BADFORMAT:
	    message_DisplayString(self, 10, "Error in Zip data file.");
	    break;
	default:
	    message_DisplayString(self, 10, "Error in reading Zip data file.");
	    break;
    }

    figview_FlushDataChanges(self);
    figview_ClearSelection(self);

    o = (struct figogrp *)figure_FindObjectByRef(fig, figview_GetFocusRef(self));
    for (ix=figogrp_GetRoot(o); ix!=figure_NULLREF; ix=fig->objs[ix].next) {
	if (fig->objs[ix].counter >= count1 && fig->objs[ix].counter < count2) {
	    figview_SelectByRef(self, ix);
	}
    }

    figure_SetModified(fig);
    figure_NotifyObservers(fig, figure_DATACHANGED);
}

static void WritePSProc(self, rock)
struct figview *self;
long rock;
{
    int res;
    char buffer[296];
    FILE *fl;

    res = completion_GetFilename(self, "Write PostScript to file: ", self->PSFileName, buffer, 255, FALSE, FALSE);
    if (res==(-1) || strlen(buffer)==0) {
	message_DisplayString(self, 10, "Cancelled.");
	return;
    }
    
    if (self->PSFileName)
	free(self->PSFileName);
    self->PSFileName = malloc(strlen(buffer)+1);
    strcpy(self->PSFileName, buffer);

    fl = fopen(self->PSFileName, "w");
    if (!fl) {
	sprintf(buffer, "Unable to open file '%s'.", self->PSFileName);
	message_DisplayString(self, 10, buffer);
	return;
    }

    fprintf(fl, "%%!PS-Adobe\n%% PostScript file generated by ATK fig inset\n");  /* ### exceedingly the wrong place */
    fprintf(fl, "36 36 translate  %% allow half-inch margins\n");  /* ### exceedingly the wrong place */
    figview_Print(self, fl, "PostScript", "PostScript", TRUE);
    fprintf(fl, "showpage\n");  /* ### exceedingly the wrong place */
    fclose(fl);

    sprintf(buffer, "Wrote to file '%s'.", self->PSFileName);
    message_DisplayString(self, 10, buffer);
}

static boolean PrintSplot(o, ref, fig, lump)
struct figobj *o;
long ref;
struct figure *fig;
struct printlump *lump;
{
    struct view *vtmp;
    struct rectangle insetb, *bbox;

    fprintf(lump->file, "%s  gsave\n", lump->prefix);	
    figobj_PrintObject(o, lump->figview, lump->file, lump->prefix);
    fprintf(lump->file, "%s  grestore\n", lump->prefix);
    if (figobj_IsInset(o) && (vtmp=lump->figview->objs[ref].insetv)) {

	bbox = figobj_GetBounds(lump->figview->objs[ref].o, lump->figview);
	insetb.left = figview_ToPrintPixX(lump->figview, bbox->left);
	insetb.top  = figview_ToPrintPixY(lump->figview, bbox->top);
	insetb.width  = figview_ToPrintPixW(lump->figview, bbox->width);
	insetb.height = figview_ToPrintPixH(lump->figview, bbox->height);

	if (class_IsTypeByName(class_GetTypeName(vtmp), "figview")) {
	    figview_SetPrintRect((struct figview *)vtmp, &insetb);
	}

	if (strcmp(lump->processor, "troff") == 0) {
	    fprintf(lump->file, "%s  gsave\n", lump->prefix);	
	    fprintf(lump->file, "%s  %d %d translate\n", lump->prefix, insetb.left, insetb.top);
	    fprintf(lump->file, "%s  1 -1 scale %d %d translate\n", lump->prefix, 0, -insetb.height);
	    view_Print(vtmp, lump->file, "PostScript", "troff", FALSE);
	    fprintf(lump->file, "%s  grestore\n", lump->prefix);
	}
	else {
	    fprintf(lump->file, "%s  gsave\n", lump->prefix);	
	    fprintf(lump->file, "%s  %d %d translate\n", lump->prefix, insetb.left, insetb.top);
	    fprintf(lump->file, "%s  1 -1 scale %d %d translate\n", lump->prefix, 0, -insetb.height);
	    view_Print(vtmp, lump->file, lump->processor, lump->format, FALSE);
	    fprintf(lump->file, "%s  grestore\n", lump->prefix);
	}

	if (class_IsTypeByName(class_GetTypeName(vtmp), "figview")) {
	    figview_SetPrintRect((struct figview *)vtmp, NULL);
	}

    }
    return FALSE;
}

void figview__Print(self, file, processor, format, toplevel)
struct figview *self;
FILE *file;
char *processor;
char *format;
boolean toplevel;
{
    struct figure *fig = (struct figure *)self->header.view.dataobject;
    long wpts, hpts;  /* image dimensions in points */
    char *prefix;
    struct printlump lump;

    figview_FlushDataChanges(self); /* make sure all objects are cached, especially if this is called from ezprint (ie, the view has never been drawn) */

    if (toplevel) {
	struct rectangle *rtmp = figure_GetOverallBounds(fig);
	wpts = figure_GetPrintScaleX(fig) * (double)figview_ToPrintPixX(self, rtmp->left+rtmp->width);
	hpts = figure_GetPrintScaleY(fig) * (double)figview_ToPrintPixY(self, rtmp->top+rtmp->height);
	wpts += 12;
	hpts += 12;
	if (wpts<1) wpts = 1;
	if (hpts<1) hpts = 1;
    }
    else {
	if (self->PrintRect) {
	    wpts = self->PrintRect->width;
	    hpts = self->PrintRect->height;
	}
	else {
	    wpts = figview_GetLogicalWidth(self);
	    hpts = figview_GetLogicalHeight(self);
	    if (wpts == 0  ||  hpts == 0) {
		/* the parent has GOOFED and has not
		 supplied a logical rectangle for printing */
		struct rectangle *RR;
		RR = figure_GetOverallBounds(fig);
		wpts = figview_ToPrintPixX(self, RR->left+RR->width);
		hpts = figview_ToPrintPixY(self, RR->top+RR->height);
	    }
	}
    }
    if (wpts < 0) wpts = 0;
    if (hpts < 0) hpts = 0;
    /* trim to 7.5 by 9 inches */
    if (wpts > 540) wpts = 540;
    if (hpts > 648) hpts = 648;

    /*printf("figview_Print: proc=%s, form=%s: %d, %d\n", processor, format, wpts, hpts);*/

    if (strcmp(processor, "troff") == 0) {
	/* output to troff */
	if (toplevel) {
	    /* take care of initial troff stream */
	    texttroff_BeginDoc(file);
	}
	/*  Put macro to interface to postscript */
	texttroff_BeginPS(file, wpts, hpts);
	prefix = "\\!  ";
    }
    else if (strcmp(format, "troff") == 0)
	prefix = "\\!  ";
    else prefix = "";

    /* generate PostScript  */
    fprintf(file, "%s  %% ATK fig inset beginning\n", prefix);
    fprintf(file, "%s  /width %d def  /height %d def\n", prefix, wpts, hpts);
    fprintf(file, "%s  1 -1 scale  0 %d translate\n", prefix, -hpts);
    fprintf(file, "%s  newpath 0 0 moveto 0 height lineto width height lineto\n", prefix);
    fprintf(file, "%s  width 0 lineto clip newpath   %% clip to assigned area\n", prefix);	
    fprintf(file, "%s  %f %f scale\n", prefix, figure_GetPrintScaleX(fig), figure_GetPrintScaleY(fig));	

    lump.file = file;
    lump.prefix = prefix;
    lump.figview = self;
    lump.processor = processor;
    lump.format = format;
    lump.width = wpts;
    lump.height = hpts;
    figure_EnumerateObjectTree(fig, figure_NULLREF, NULL, FALSE, PrintSplot, &lump);

    if (strcmp(processor, "troff") == 0) {
	texttroff_EndPS(file, wpts, hpts);
	if (toplevel)
	    texttroff_EndDoc(file);
    }
}

static void ToolsetCreateProc(self, rock)
struct figview *self;
char *rock;
{
    struct figure *fig = (struct figure *)figview_GetDataObject(self);
    struct im *im;
    struct frame *fr;
    struct figtoolview *tv;
    boolean res;

    if (self->toolset)  {
	message_DisplayString(self, 40, "There is already a toolset window.");
	return;
    }

    if (!fig) {
	message_DisplayString(self, 40, "This view has no data!");
	return;
    }

    im = im_Create(NULL);
    if (!im) {
	message_DisplayString(self, 40, "Unable to create new window.");
	return;
    }

    fr = frame_New();
    if (!fr) {
	message_DisplayString(self, 40, "Unable to create window frame.");
	return;
    }

    /*frame_SetCommandEnable(fr, TRUE);*/
    frame_PostDefaultHandler(fr, "message", frame_WantHandler(fr, "message"));

    tv = figtoolview_New();
    if (!tv) {
	message_DisplayString(self, 40, "Unable to create toolset.");
	return;
    }

    res = figtoolview_SetPrimaryView(tv, self);
    if (!res) {
	message_DisplayString(self, 40, "Unable to initialize toolset (this shouldn't happen).");
	return;
    }

    self->toolset = tv;
    frame_SetView(fr, tv);
    frame_SetQuitWindowFlag(fr, TRUE);
    im_SetView(im, fr);
    {   
	struct buffer *buf = buffer_FindBufferByData(fig); 
	char *nm;
	if (buf) {
	    nm = buffer_GetFilename(buf);
	    if (nm)
		im_SetTitle(im, nm);
	}
    }
    figtoolview_WantInputFocus(tv, tv);
    figtoolview_SetExpertMode(tv, self->expertmode);
    RepostMenus(self);
}

void figview__DestroyToolset(self)
struct figview *self;
{
    ToolsetKillProc(self, 0);
}

static void ToolsetKillProc(self, rock)
struct figview *self;
char *rock;
{
    struct im *toolim = NULL;
    struct frame *toolfr = NULL;

    if (!self->toolset) {
	message_DisplayString(self, 40, "There is no toolset window.");
	return;
    }

    if (self->toolset->creating)
	figtoolview_AbortObjectBuilding(self->toolset);

    toolim = figtoolview_GetIM(self->toolset);
    if (toolim)
	toolfr = (struct frame *)toolim->topLevel;

    figtoolview_RemoveObserver(self->toolset, self);
    figview_RemoveObserver(self, self->toolset);

    figtoolview_SetMoribund(self->toolset, 1); /* ensure that toolset doesn't try to kill itself when it sees it's being unlinked */

    if (toolim) {
	im_SetView(toolim, NULL);
    }

    if (toolfr) {
	frame_SetView(toolfr, NULL);
    }

    figtoolview_Destroy(self->toolset);
    self->toolset = NULL;

    if (toolim) {
	im_Destroy(toolim); 
    }
    if (toolfr) {
	frame_Destroy(toolfr);
    }

    figview_ClearSelection(self);
    figview_WantUpdate(self, self);
    RepostMenus(self); 
}

static void ChangeZoomProc(self, rock)
struct figview *self;
long rock;
{
    long newscale;
    long midx, midy, offx, offy;
    struct rectangle logrec;
    boolean atorigin = FALSE;

    if (self->panx == self->originx && self->pany == self->originy) 
	atorigin = TRUE;

    figview_GetLogicalBounds(self, &logrec);
    midx = figview_ToFigW(self, logrec.left+logrec.width/2) - self->panx;
    midy = figview_ToFigH(self, logrec.top+logrec.height/2) - self->pany;
    if (rock<0) 
	newscale = self->scale / 2;
    else if (rock>0)
	newscale = self->scale * 2;
    else newscale = figview_NormScale;

    if (newscale<1) newscale=1;
    else if (newscale>1024) newscale=1024;

    if (newscale != self->scale) {
	self->scale = newscale;

	if (newscale == figview_NormScale && atorigin) {
	    /* stay at origin */
	}
	else {
	    offx = figview_ToFigW(self, logrec.left+logrec.width/2) - self->panx;
	    offy = figview_ToFigH(self, logrec.top+logrec.height/2) - self->pany;
	    self->panx -= (offx - midx);
	    self->pany -= (offy - midy);
	}
	self->NeedFullUpdate = TRUE;
	figview_WantUpdate(self, self);
    }
}

/* 1 to zoom in, -1 to zoom out, 0 to zoom norm */
void figview__ChangeZoom(self, val)
struct figview *self;
long val;
{
    ChangeZoomProc(self, val);
}

static void PanToOriginProc(self, rock)
struct figview *self;
long rock;
{
    if (self->panx==self->originx && self->pany==self->originy)
	return;

    self->panx = self->originx;
    self->pany = self->originy;
    /* self->NeedFullUpdate = TRUE; */
    figview_WantUpdate(self, self);
}

void figview__LinkTree( self, parent )
struct figview *self;
struct view *parent;
{
    super_LinkTree(self, parent);
    if (parent && figview_GetIM(self)) {
	register int ix;
	struct figobj *o;
	for (ix = 0; ix < self->objs_size; ix++ ) {
	    o = self->objs[ix].o;
	    if (o && figobj_IsInset(o) && self->objs[ix].insetv)
		view_LinkTree(self->objs[ix].insetv, self);
	}
    }
}

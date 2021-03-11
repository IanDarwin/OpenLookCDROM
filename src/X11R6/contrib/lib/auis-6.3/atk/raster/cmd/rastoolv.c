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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/raster/cmd/RCS/rastoolv.c,v 1.16 1993/05/04 01:28:26 susan Exp $";
#endif


 
#include <andrewos.h>

#include <stdio.h>
#include <math.h>

#include <rastoolv.eh>

#include <menulist.ih>
#include <keymap.ih>
#include <keystate.ih>
#include <cursor.ih>
#include <proctbl.ih>

#include <raster.ih>
#include <rasterv.ih>
#include <observe.ih>
#include <fontdesc.ih>
#include <graphic.ih>
#include <frame.ih>
#include <rastimg.ih>
#include <strtbl.ih>
#include <strtblv.ih>
#include <lpair.ih>
#include <text.ih>
#include <textv.ih>
#include <view.ih>
#include <event.ih>
#include <im.ih>

#include <pattern.h>
#include <rect.h>

#define ZRPATTERN_INVERT ((unsigned char *)NULL)
#define ZRBRUSH_PIXEL ((unsigned char *)NULL)

#ifndef _IBMR2
extern char *malloc();
#endif

static void SetToolProc(), SetToolNum(), SetBrushProc(), SetPatternProc(), CallCommandProc(), CallPasteModeProc(), PasteDownProc();
static void Tool_Paint(), Tool_Paste(), Tool_Text(), Tool_Line(), Tool_Circle(), Tool_Ellipse(), Tool_SolidEllipse(), Tool_FloodFill(), Tool_SprayPaint(), Tool_Rectangle(), Tool_SolidCircle(), Tool_SolidRect(), Tool_CurvePaint();
static void Toolmod_Paste(), Toolmod_Text(), Toolmod_FloodFill(), Toolmod_SprayPaint(), Toolmod_CurvePaint();
static void Command_ZoomIn(), Command_ZoomOut(), Command_ZoomNorm(), Command_Copy(), Command_Refresh(), Command_Quit();
static void SpraySplot(), FloodSplot(), CurveSplot(), PasteResplot();
static void RemoveInsetProc(), ResizeInsetProc();

struct layout_t {
    void (*proc)(); /* tool procedure */
    void (*procmod)(); /* procedure to call when tool is double-clicked */
    boolean writes; /* does the tool change the raster data? */
    char name[16]; /* name in the toolset window */
};

struct layout_c {
    void (*proc)(); /* command procedure */
    char *rock;	    /* a rock */
    char name[16]; /* name in the toolset window */
};

#define RASTOOL_PAINT (6)
static struct layout_t toollayout[ZRTOOLS_NUM] = {
    {NULL, NULL, 0, "TOOLS:"},
    {NULL, NULL, 0, "Pan"},
    {NULL, NULL, 0, "Select"},
    {NULL, NULL, 1, "Touch up"},
    {Tool_Paste, Toolmod_Paste, 1, "Paste"},
    {Tool_Text, Toolmod_Text, 1, "Text"},
    {Tool_Paint, NULL, 1, "Paint"},
    {Tool_Line, NULL, 1, "Line"},
    {Tool_Rectangle, NULL, 1, "Rectangle"},
    {Tool_Circle, NULL, 1, "Circle"},
    {Tool_Ellipse, NULL, 1, "Ellipse"},
    {Tool_SolidRect, NULL, 1, "Solid rectangle"},
    {Tool_SolidCircle, NULL, 1, "Solid circle"},
    {Tool_SolidEllipse, NULL, 1, "Solid ellipse"},
    {Tool_FloodFill, Toolmod_FloodFill, 1, "Flood fill"},
    {Tool_SprayPaint, Toolmod_SprayPaint, 1, "Spraypaint"},
    {Tool_CurvePaint, Toolmod_CurvePaint, 1, "Curvy paint"}
};

static struct layout_c commandlayout[ZRCMDS_NUM] = {
    {NULL,		NULL,	"COMMANDS:"},
    {Command_Copy,	NULL,	"Copy"},
    {Command_ZoomIn,	NULL,	"Zoom in"},
    {Command_ZoomOut,	NULL,	"Zoom out"},
    {Command_ZoomNorm,	NULL,	"Normal size"},
    {Command_Refresh,	NULL,	"Refresh"}
};

static char pastemodelayout[ZRPASTEMODES_NUM][24] = {
    "PASTE MODES:",
    "Replace",
    "Combine",
    "Invert"
};

#define	ML_unpaste	    (1)
#define	ML_inset	    (2)

static struct menulist *Menus;
static struct keymap *Keymap;

boolean rastoolview__InitializeClass(ClassID)
struct classhdr *ClassID;
{
    struct proctable_Entry *proc = NULL;
    
    Menus = menulist_New();
    Keymap = keymap_New();

    proc = proctable_DefineProc("rastoolv-toolset-destroy", Command_Quit, &rastoolview_classinfo, NULL, "Deletes toolset window.");
    keymap_BindToKey(Keymap, "\030\004", proc, 0);	/* ^X^D */
    keymap_BindToKey(Keymap, "\030\003", proc, 0);	/* ^X^C */
    menulist_AddToML(Menus, "Quit~99", proc, NULL, 0);

    proc = proctable_DefineProc("rastoolv-pastedown", PasteDownProc, &rastoolview_classinfo, NULL, "Paste down paste region onto raster.");
    menulist_AddToML(Menus, "Paste Down Region~11", proc, NULL, ML_unpaste);
    keymap_BindToKey(Keymap, "\031", proc, 0);	/* ^Y */

    proc = proctable_DefineProc("rastoolv-unpaste", Toolmod_Paste, &rastoolview_classinfo, NULL, "Remove paste region from raster.");
    menulist_AddToML(Menus, "Remove Paste Region~12", proc, NULL, ML_unpaste);

    proc = proctable_DefineProc("rastoolv-remove-inset", RemoveInsetProc, &rastoolview_classinfo, NULL, "Remove the overlaid inset.");
    menulist_AddToML(Menus, "Remove Inset~22", proc, NULL, ML_inset);

    proc = proctable_DefineProc("rastoolv-resize-inset", ResizeInsetProc, &rastoolview_classinfo, NULL, "Resize the overlaid inset.");
    menulist_AddToML(Menus, "Resize Inset~23", proc, NULL, ML_inset);

    proc = proctable_DefineProc("rastoolv-imprint-inset", Toolmod_Text, &rastoolview_classinfo, NULL, "Imprint the overlaid inset.");
    menulist_AddToML(Menus, "Paste Down Inset~21", proc, NULL, ML_inset);

    proc = proctable_DefineProc("rastoolv-set-spray-size", Toolmod_SprayPaint, &rastoolview_classinfo, NULL, "Set radius of spraypaint tool.");
    menulist_AddToML(Menus, "Set Spray Radius~31", proc, NULL, 0);

    proc = proctable_DefineProc("rastoolv-set-curve-spring", Toolmod_CurvePaint, &rastoolview_classinfo, NULL, "Set spring stiffness in curvepaint tool.");
    menulist_AddToML(Menus, "Set Curvypaint Spring~32", proc, NULL, 0);

    proc = proctable_DefineProc("rastoolv-floodfill-abort", Toolmod_FloodFill, &rastoolview_classinfo, NULL, "Abort flood fill operation.");
    menulist_AddToML(Menus, "Abort Flood Fill~33", proc, NULL, 0);
    keymap_BindToKey(Keymap, "\003", proc, 0);	/* ^C */

    return TRUE;
}

boolean rastoolview__InitializeObject(c, self)
struct classheader *c;
struct rastoolview *self;
{
    int ix;

    struct lpair *lp1 = lpair_New();
    struct lpair *lp2 = lpair_New();
    struct lpair *lp3 = lpair_New();

    struct stringtbl *tl = stringtbl_New();
    struct strtblview *tlv = strtblview_New();

    struct stringtbl *tlc = stringtbl_New();
    struct strtblview *tlcv = strtblview_New();

    struct stringtbl *tlm = stringtbl_New();
    struct strtblview *tlmv = strtblview_New();

    struct stringtbl *tlp = stringtbl_New();
    struct strtblview *tlpv = strtblview_New();

    struct stringtbl *tlb = stringtbl_New();
    struct strtblview *tlbv = strtblview_New();

    if (!lp1 || !lp2 || !tl || !tlv || !tlm || !tlmv || !tlc || !tlcv || !tlp || !tlpv  || !tlb || !tlbv) {
	return FALSE;
    }

    stringtbl_Clear(tl);
    for (ix=0; ix<ZRTOOLS_NUM; ix++) {
	self->toolacc[ix] = stringtbl_AddString(tl, toollayout[ix].name);
	if (self->toolacc[ix] == (-1))
	    return FALSE;
    }
    strtblview_SetItemHitProc(tlv, SetToolProc, self);
    strtblview_SetDataObject(tlv, tl);

    stringtbl_Clear(tlc);
    for (ix=0; ix<ZRCMDS_NUM; ix++) {
	self->commandacc[ix] = stringtbl_AddString(tlc, commandlayout[ix].name);
	if (self->commandacc[ix] == (-1))
	    return FALSE;
    }
    strtblview_SetItemHitProc(tlcv, CallCommandProc, self);
    strtblview_SetDataObject(tlcv, tlc);

    stringtbl_Clear(tlm);
    for (ix=0; ix<ZRPASTEMODES_NUM; ix++) {
	self->pastemodeacc[ix] = stringtbl_AddString(tlm, pastemodelayout[ix]);
	if (self->pastemodeacc[ix] == (-1))
	    return FALSE;
    }
    strtblview_SetItemHitProc(tlmv, CallPasteModeProc, self);
    strtblview_SetDataObject(tlmv, tlm);

    stringtbl_Clear(tlp);
    for (ix=0; ix<ZRPATTERNS_NUM; ix++) {
	self->patternacc[ix] = stringtbl_AddString(tlp, patternnames[ix]);
	if (self->patternacc[ix] == (-1))
	    return FALSE;
    }
    strtblview_SetItemHitProc(tlpv, SetPatternProc, self);
    strtblview_SetDataObject(tlpv, tlp);

    stringtbl_Clear(tlb);
    for (ix=0; ix<ZRBRUSHES_NUM; ix++) {
	self->brushacc[ix] = stringtbl_AddString(tlb, brushnames[ix]);
	if (self->brushacc[ix] == (-1))
	    return FALSE;
    }
    strtblview_SetItemHitProc(tlbv, SetBrushProc, self);
    strtblview_SetDataObject(tlbv, tlb);

    self->lpair1 = lp1;
    self->lpair2 = lp2;
    self->lpair3 = lp3;
    self->tooltbl = tl;
    self->vtooltbl = tlv;
    self->commandtbl = tlc;
    self->vcommandtbl = tlcv;
    self->pastemodetbl = tlm;
    self->vpastemodetbl = tlmv;
    self->patterntbl = tlp;
    self->vpatterntbl = tlpv;
    self->brushtbl = tlb;
    self->vbrushtbl = tlbv;
    self->pastemode = pixelimage_COPY;
    self->pastemodenum = PASTEMODE_COPY;
    self->toolnum = RASTOOL_PAN;
    self->toolproc = NULL;
    self->patternnum = 2;
    self->pattern = patternlist[self->patternnum];
    self->brushnum = 1;
    self->brush = NULL;
    self->fillstack = NULL;
    self->fillpix = NULL;
    self->springconst = 0.2;
    self->sprayradius = 10;
    stringtbl_ClearBits(tl);
    stringtbl_ClearBits(tlc);
    stringtbl_ClearBits(tlm);
    stringtbl_ClearBits(tlp);
    stringtbl_ClearBits(tlb);
    stringtbl_SetBitOfEntry(tl, self->toolacc[self->toolnum], TRUE);
    stringtbl_SetBitOfEntry(tlm, self->pastemodeacc[self->pastemodenum], TRUE);
    stringtbl_SetBitOfEntry(tlp, self->patternacc[self->patternnum], TRUE);
    stringtbl_SetBitOfEntry(tlb, self->brushacc[self->brushnum], TRUE);

    self->primaryview = NULL;
    self->moribund = 0;

    lpair_SetUp(lp3, tlcv, tlmv, 30, lpair_PERCENTAGE, lpair_HORIZONTAL, TRUE);
    lpair_SetUp(lp1, tlv, lp3, 50, lpair_PERCENTAGE, lpair_VERTICAL, TRUE);
    lpair_SetUp(lp2, tlpv, tlbv, 50, lpair_PERCENTAGE, lpair_VERTICAL, TRUE);
    rastoolview_SetUp(self, lp1, lp2, 50, lpair_PERCENTAGE, lpair_VERTICAL, TRUE);

    self->pasteraster = raster_New();
    self->unpasteraster = raster_New();
    self->unpaste = FALSE;

    self->Menus = menulist_DuplicateML(Menus, self);
    self->Keystate = keystate_Create(self, Keymap);

    return TRUE;
}

void rastoolview__FinalizeObject(c, self)
struct classheader *c;
struct rastoolview *self;
{

    if (self->fillpix) {
	rasterimage_Destroy(self->fillpix);
    }

    raster_Destroy(self->pasteraster);
    raster_Destroy(self->unpasteraster);
    menulist_ClearChain(self->Menus);
    menulist_Destroy(self->Menus);
    keystate_Destroy(self->Keystate);

    self->primaryobj = NULL;

    rastoolview_SetNth(self, 0, NULL);
    rastoolview_SetNth(self, 1, NULL);
    lpair_Destroy(self->lpair2);
    lpair_Destroy(self->lpair1);
    lpair_Destroy(self->lpair3);

    strtblview_Destroy(self->vpatterntbl);
    strtblview_Destroy(self->vbrushtbl);
    strtblview_Destroy(self->vcommandtbl);
    strtblview_Destroy(self->vpastemodetbl);
    strtblview_Destroy(self->vtooltbl);

    stringtbl_Destroy(self->patterntbl);
    stringtbl_Destroy(self->brushtbl);
    stringtbl_Destroy(self->commandtbl);
    stringtbl_Destroy(self->pastemodetbl);
    stringtbl_Destroy(self->tooltbl);
}

static void RepostMenus(self)
struct rastoolview *self;
{
    long menumask = 0;

    if (self->unpaste)
	menumask |= ML_unpaste;

    if (rasterview_GetOverlaidInset(self->primaryview))
	menumask |= ML_inset;

    if (menulist_SetMask(self->Menus, menumask)) {
	rastoolview_PostMenus(self, NULL);
    }
}

void rastoolview__PostMenus(self, ml)
struct rastoolview *self;
struct menulist *ml;
{
/* Enable the menus for this object. */

    menulist_ClearChain(self->Menus);
    if (ml) menulist_ChainBeforeML(self->Menus, ml, ml);
    super_PostMenus(self, self->Menus);
}

void rastoolview__PostKeyState(self, ks)
struct rastoolview *self;
struct keystate *ks;
{
/* Enable the keys for this object. */

    self->Keystate->next = NULL;
    keystate_AddBefore(self->Keystate, ks);
    super_PostKeyState(self, self->Keystate);
}

boolean rastoolview__WantSelectionHighlighted(self)
struct rastoolview *self;
{
    void (*tpr)();

    tpr = toollayout[self->toolnum].proc;

    return (tpr==Tool_Paste || tpr==Tool_Text);
}

boolean rastoolview__SetPrimaryView(self, zrview)
struct rastoolview *self;
struct rasterview *zrview;
{
    if (self->primaryview) {
	rastoolview_RemoveObserver(self, self->primaryview);
	rasterview_RemoveObserver(self->primaryview, self);
    }

    if (!zrview) { /* odd, but somebody might do it */
	self->primaryview = NULL;
	self->primaryobj = NULL;
	return TRUE;
    }

    self->primaryview = zrview;
    self->primaryobj = rasterview_GetDataObject(zrview);
    rastoolview_AddObserver(self, zrview);
    rasterview_AddObserver(zrview, self);

    /* ### figure out tool to highlight */
    {
	int modenum = rasterview_GetMode(zrview);
	switch (modenum) {
	    case RegionSelectMode:
		SetToolNum(self, RASTOOL_SELECT);
		break;
	    case TouchUpMode:
		SetToolNum(self, RASTOOL_TOUCHUP);
		break;
	    case PanMode:
		SetToolNum(self, RASTOOL_PAN);
		break;
	    default:
		SetToolNum(self, RASTOOL_PAN);
		break;
	}
    }

    return TRUE;
}

static void SetToolNum(self, toolnum)
struct rastoolview *self;
int toolnum;
{
    if (toolnum==0)
	toolnum = RASTOOL_PAN;
    if (self->toolnum != toolnum) {
	self->unpaste = FALSE;
	RepostMenus(self);
    }
    self->toolnum = toolnum;
    self->toolproc = toollayout[toolnum].proc;
    stringtbl_ClearBits(self->tooltbl);
    stringtbl_SetBitOfEntry(self->tooltbl, self->toolacc[self->toolnum], TRUE);
}

void rastoolview__ObservedChanged(self, observed, status)
struct rastoolview *self;
struct observable *observed;
long status;
{
    if (observed == (struct observable *)self->primaryview) {
	if (status==observable_OBJECTDESTROYED) {
	    self->primaryview = NULL;
	    fprintf(stderr, "rastoolview: primary rasterview destroyed; suiciding...\n");
	    rastoolview_Destroy(self);
	}
	else {
	    int modenum = rasterview_GetMode(self->primaryview);
	    int newtnum;
	    switch (modenum) {
		case RegionSelectMode:
		    newtnum = (RASTOOL_SELECT);
		    break;
		case TouchUpMode:
		    newtnum = (RASTOOL_TOUCHUP);
		    break;
		case PanMode:
		    newtnum = (RASTOOL_PAN);
		    break;
		default:
		    newtnum = self->toolnum;
		    if (newtnum==RASTOOL_SELECT || newtnum==RASTOOL_TOUCHUP || newtnum==RASTOOL_PAN)
			newtnum = RASTOOL_PAINT;
		    break;
	    }
	    if (newtnum != self->toolnum)
		SetToolNum(self, newtnum);
	    RepostMenus(self);
	}
    }
    else {
	if (status==observable_OBJECTDESTROYED) {
	    fprintf(stderr, "rastoolview: Primary raster dataobject destroyed.\n");
	}
    }
}

void rastoolview__UnlinkTree(self)
struct rastoolview *self;
{
    super_UnlinkTree(self);
}

static void Command_Refresh(self, rock)
struct rastoolview *self;
char *rock;
{
    struct rasterview *rself = self->primaryview;

    if (!rself) return;

    rself->needsFullUpdate = TRUE;
    rasterview_WantUpdate(rself, rself);
}

static void Command_Copy(self, rock)
struct rastoolview *self;
char *rock;
{
    struct rasterview *rself = self->primaryview;

    if (!rself) return;

    rasterview_CopySelection(rself);
    /* only display message if it won't overwrite the rasterv's message */
    if (!rself->ShowCoords)
	message_DisplayString(rself, 10, "Selection copied.");
}

static void Command_ZoomIn(self, rock)
struct rastoolview *self;
char *rock;
{
    struct rasterview *rself = self->primaryview;
    int newscale;

    if (!rself) return;

    if (Cropped(((struct rasterview *)rself))) {
	message_DisplayString(self, 10, "You cannot zoom in a cropped raster.");
	return;
    }
    newscale = rasterview_GetScale(rself);
    newscale *= 2;
    rasterview_SetScale(rself, newscale);
}

static void Command_ZoomOut(self, rock)
struct rastoolview *self;
char *rock;
{
    struct rasterview *rself = self->primaryview;
    int newscale;

    if (!rself) return;

    if (Cropped(((struct rasterview *)rself))) {
	message_DisplayString(self, 10, "You cannot zoom in a cropped raster.");
	return;
    }
    newscale = rasterview_GetScale(rself);
    newscale /= 2;
    if (newscale < 1)
	newscale = 1;
    rasterview_SetScale(rself, newscale);
}

static void Command_ZoomNorm(self, rock)
struct rastoolview *self;
char *rock;
{
    struct rasterview *rself = self->primaryview;
    int newscale;

    if (!rself) return;

    newscale = 1;
    rasterview_SetScale(rself, newscale);
}

static void Command_Quit(self, rock)
struct rastoolview *self;
char *rock;
{
    if (self->primaryview) {
	rasterview_DestroyToolset(self->primaryview);
    }
    else {
	fprintf(stderr, "rastoolview: warning: no primary rasterview.\n");
	rastoolview_Destroy(self);
    }
}


static void CallCommandProc(st, self, accnum)
struct stringtbl *st;
struct rastoolview *self;
short accnum;
{
    int cmdnum;
    void (*comproc)();

    for (cmdnum=0; cmdnum<ZRCMDS_NUM; cmdnum++) {
	if (self->commandacc[cmdnum] == accnum) break;
    }
    if (cmdnum==ZRCMDS_NUM) {
	return;
    }
    if (cmdnum==0) {
	message_DisplayString(self, 10, "Click on any of these commands.");
	return;
    }

    comproc = commandlayout[cmdnum].proc;
    if (comproc)
	(*comproc)(self, commandlayout[cmdnum].rock);
}

static void CallPasteModeProc(st, self, accnum)
struct stringtbl *st;
struct rastoolview *self;
short accnum;
{
    int pmdnum;

    for (pmdnum=0; pmdnum<ZRPASTEMODES_NUM; pmdnum++) {
	if (self->pastemodeacc[pmdnum] == accnum) break;
    }
    if (pmdnum==ZRPASTEMODES_NUM) {
	return;
    }
    if (pmdnum==0) {
	message_DisplayString(self, 10, "Click on a mode to select it.");
	return;
    }

    self->pastemodenum = pmdnum;
    switch (pmdnum) {
	case PASTEMODE_COPY:
	    self->pastemode = pixelimage_COPY;
	    break;
	case PASTEMODE_OR:
	    self->pastemode = pixelimage_OR;
	    break;
	case PASTEMODE_XOR:
	    self->pastemode = pixelimage_XOR;
	    break;
    }

    if (!stringtbl_GetBitOfEntry(st, accnum)) {
	stringtbl_ClearBits(st);
	stringtbl_SetBitOfEntry(st, accnum, TRUE);
    }

    PasteResplot(self, 0);
}

static void SetPatternProc(st, self, accnum)
struct stringtbl *st;
struct rastoolview *self;
short accnum;
{
    int patternnum;

    for (patternnum=0; patternnum<ZRPATTERNS_NUM; patternnum++) {
	if (self->patternacc[patternnum] == accnum) break;
    }
    if (patternnum==ZRPATTERNS_NUM) {
	return;
    }
    if (patternnum==0) {
	message_DisplayString(self, 10, "Click on a pattern to select it.");
	return;
    }

    self->patternnum = patternnum;
    if (patternnum==1)
	self->pattern = ZRPATTERN_INVERT;
    else
	self->pattern = patternlist[patternnum];

    if (!stringtbl_GetBitOfEntry(st, accnum)) {
	stringtbl_ClearBits(st);
	stringtbl_SetBitOfEntry(st, accnum, TRUE);
    }
}

static void SetBrushProc(st, self, accnum)
struct stringtbl *st;
struct rastoolview *self;
short accnum;
{
    int brushnum;

    for (brushnum=0; brushnum<ZRBRUSHES_NUM; brushnum++) {
	if (self->brushacc[brushnum] == accnum) break;
    }
    if (brushnum==ZRBRUSHES_NUM) {
	return;
    }
    if (brushnum==0) {
	message_DisplayString(self, 10, "Click on a brush to select it.");
	return;
    }

    self->brushnum = brushnum;
    if (brushnum==1)
	self->brush = ZRBRUSH_PIXEL;
    else
	self->brush = brushlist[brushnum];

    if (!stringtbl_GetBitOfEntry(st, accnum)) {
	stringtbl_ClearBits(st);
	stringtbl_SetBitOfEntry(st, accnum, TRUE);
    }
}

static void SetToolProc(st, self, accnum)
struct stringtbl *st;
struct rastoolview *self;
short accnum;
{
    int toolnum;
    struct raster *ras = (struct raster *)self->primaryobj;

    for (toolnum=0; toolnum<ZRTOOLS_NUM; toolnum++) {
	if (self->toolacc[toolnum] == accnum) break;
    }
    if (toolnum==ZRTOOLS_NUM) {
	return;
    }
    if (toolnum==0) {
	message_DisplayString(self, 10, "Click on a tool to select it; click again to set tool parameters.");
	return;
    }

    if (ras->readOnly && toollayout[toolnum].writes) {
	message_DisplayString(self, 10, "Raster is read-only.");
	return;
    }

    if (self->toolnum==toolnum) {
	void (*prmod)();
	prmod = toollayout[toolnum].procmod;
	if (prmod) 
	    (*prmod)(self, NULL);
	return;
    }
    else {
	self->unpaste = FALSE;
	RepostMenus(self);
    }

    self->toolnum = toolnum;
    self->toolproc = toollayout[toolnum].proc;

    if (!stringtbl_GetBitOfEntry(st, accnum)) {
	stringtbl_ClearBits(st);
	stringtbl_SetBitOfEntry(st, accnum, TRUE);
    }
    rastoolview_NotifyObservers(self, NULL); 
}

static void DrawLine(rself, x0, y0, x1, y1, pattern, brush)
struct rasterview *rself;
long x0, y0, x1, y1;
unsigned char *pattern;
unsigned char *brush;
{
    int dx, dy, x, y, d, incr_str, incr_diag;

    dx = x1-x0;
    dy = y1-y0;
    x = x0;
    y = y0;
    /* draw initial point */
    rasterview_BrushSetPixel(rself, x, y, pattern, brush);

    /* standard midpoint line-drawing algorithm */
    /* split into 8 cases, depending on which way the line goes */
    if (dx>=0) {
	if (dy>=0) {
	    if (dy <= dx) {
		d = 2*dy-dx;
		incr_str = 2*dy;
		incr_diag = 2*(dy-dx);
		while (x<x1) {
		    if (d<=0) {
			d += incr_str;
			x++;
		    }
		    else {
			d += incr_diag;
			x++;
			y++;
		    }
		    rasterview_BrushSetPixel(rself, x, y, pattern, brush);
		}
	    }
	    else {
		d = 2*dx-dy;
		incr_str = 2*dx;
		incr_diag = 2*(dx-dy);
		while (y<y1) {
		    if (d<=0) {
			d += incr_str;
			y++;
		    }
		    else {
			d += incr_diag;
			y++;
			x++;
		    }
		    rasterview_BrushSetPixel(rself, x, y, pattern, brush);
		}
	    }
	}
	else {
	    if (-dy <= dx) {
		d = -2*dy-dx;
		incr_str = -2*dy;
		incr_diag = 2*(-dy-dx);
		while (x<x1) {
		    if (d<=0) {
			d += incr_str;
			x++;
		    }
		    else {
			d += incr_diag;
			x++;
			y--;
		    }
		    rasterview_BrushSetPixel(rself, x, y, pattern, brush);
		}
	    }
	    else {
		d = 2*dx+dy;
		incr_str = 2*dx;
		incr_diag = 2*(dx+dy);
		while (y>y1) {
		    if (d<=0) {
			d += incr_str;
			y--;
		    }
		    else {
			d += incr_diag;
			y--;
			x++;
		    }
		    rasterview_BrushSetPixel(rself, x, y, pattern, brush);
		}
	    }
	}
    }
    else {
	if (dy>=0) {
	    if (dy <= -dx) {
		d = 2*dy+dx;
		incr_str = 2*dy;
		incr_diag = 2*(dy+dx);
		while (x>x1) {
		    if (d<=0) {
			d += incr_str;
			x--;
		    }
		    else {
			d += incr_diag;
			x--;
			y++;
		    }
		    rasterview_BrushSetPixel(rself, x, y, pattern, brush);
		}
	    }
	    else {
		d = -2*dx-dy;
		incr_str = -2*dx;
		incr_diag = 2*(-dx-dy);
		while (y<y1) {
		    if (d<=0) {
			d += incr_str;
			y++;
		    }
		    else {
			d += incr_diag;
			y++;
			x--;
		    }
		    rasterview_BrushSetPixel(rself, x, y, pattern, brush);
		}
	    }
	}
	else {
	    if (-dy <= -dx) {
		d = -2*dy+dx;
		incr_str = -2*dy;
		incr_diag = 2*(-dy+dx);
		while (x>x1) {
		    if (d<=0) {
			d += incr_str;
			x--;
		    }
		    else {
			d += incr_diag;
			x--;
			y--;
		    }
		    rasterview_BrushSetPixel(rself, x, y, pattern, brush);
		}
	    }
	    else {
		d = -2*dx+dy;
		incr_str = -2*dx;
		incr_diag = 2*(-dx+dy);
		while (y>y1) {
		    if (d<=0) {
			d += incr_str;
			y--;
		    }
		    else {
			d += incr_diag;
			y--;
			x--;
		    }
		    rasterview_BrushSetPixel(rself, x, y, pattern, brush);
		}
	    }
	}
    }
}

/* draw a circle with midpoint algorithm */
static void DrawCircle(rself, x0, y0, rad, pattern, brush)
struct rasterview *rself;
long x0, y0, rad;
unsigned char *pattern, *brush;
{
    int d, x, y;

    x = 0;
    y = rad;
    d = 1-rad;
    rasterview_BrushSetPixel(rself, x0, y0-y, pattern, brush);
    rasterview_BrushSetPixel(rself, x0, y0+y, pattern, brush);
    rasterview_BrushSetPixel(rself, x0-y, y0, pattern, brush);
    rasterview_BrushSetPixel(rself, x0+y, y0, pattern, brush);
    if (d<0) {
	d += 2*x+3;
	x++;
    }
    else {
	d += 2*(x-y)+5;
	x++;
	y--;
    }
    while (y>x) {
	rasterview_BrushSetPixel(rself, x0-x, y0+y, pattern, brush);
	rasterview_BrushSetPixel(rself, x0-x, y0-y, pattern, brush);
	rasterview_BrushSetPixel(rself, x0+x, y0-y, pattern, brush);
	rasterview_BrushSetPixel(rself, x0+x, y0+y, pattern, brush);
	rasterview_BrushSetPixel(rself, x0-y, y0+x, pattern, brush);
	rasterview_BrushSetPixel(rself, x0-y, y0-x, pattern, brush);
	rasterview_BrushSetPixel(rself, x0+y, y0-x, pattern, brush);
	rasterview_BrushSetPixel(rself, x0+y, y0+x, pattern, brush);
	if (d<0) {
	    d += 2*x+3;
	    x++;
	}
	else {
	    d += 2*(x-y)+5;
	    x++;
	    y--;
	}
    }
    if (x==y) {
	rasterview_BrushSetPixel(rself, x0-x, y0+y, pattern, brush);
	rasterview_BrushSetPixel(rself, x0-x, y0-y, pattern, brush);
	rasterview_BrushSetPixel(rself, x0+x, y0-y, pattern, brush);
	rasterview_BrushSetPixel(rself, x0+x, y0+y, pattern, brush);
    }
}

/* draw an ellipse with midpoint algorithm */
static void DrawEllipse(rself, x0, y0, xrad, yrad, pattern, brush)
struct rasterview *rself;
long x0, y0;
long xrad, yrad; /* both must be >= 0 */
unsigned char *pattern, *brush;
{
    int d, x, y;

    x = 0;
    y = yrad;
    d = 4*yrad*yrad - xrad*xrad*yrad + xrad*xrad;
    rasterview_BrushSetPixel(rself, x0, y0-y, pattern, brush);
    rasterview_BrushSetPixel(rself, x0, y0+y, pattern, brush);

    while (xrad*xrad*(2*y-1) > 2*yrad*yrad*(x+1)) {
	if (d<0) {
	    d += 4*yrad*yrad*(2*x+3);
	    x++;
	}
	else {
	    d += 4*(yrad*yrad*(2*x+3) - xrad*xrad*(2*y-2));
	    x++;
	    y--;
	}
	rasterview_BrushSetPixel(rself, x0-x, y0+y, pattern, brush);
	rasterview_BrushSetPixel(rself, x0-x, y0-y, pattern, brush);
	rasterview_BrushSetPixel(rself, x0+x, y0-y, pattern, brush);
	rasterview_BrushSetPixel(rself, x0+x, y0+y, pattern, brush);
    }

    d = yrad*yrad*(2*x+1)*(2*x+1) + 4*xrad*xrad*(y-1)*(y-1) - 4*xrad*xrad*yrad*yrad;
    while (y > 1) {
	if (d>=0) {
	    d -= 4*xrad*xrad*(2*y-3);
	    y--;
	}
	else {
	    d += 4*(yrad*yrad*(2*x+2) - xrad*xrad*(2*y-3));
	    x++;
	    y--;
	}
	rasterview_BrushSetPixel(rself, x0-x, y0+y, pattern, brush);
	rasterview_BrushSetPixel(rself, x0-x, y0-y, pattern, brush);
	rasterview_BrushSetPixel(rself, x0+x, y0-y, pattern, brush);
	rasterview_BrushSetPixel(rself, x0+x, y0+y, pattern, brush);
    }
    if (d>=0) {
    }
    else {
	x++;
    }
    rasterview_BrushSetPixel(rself, x0-x, y0, pattern, brush);
    rasterview_BrushSetPixel(rself, x0+x, y0, pattern, brush);
}

static void FillRectangle(self, x0, y0, wid, hgt, pattern)
struct rastoolview *self;
long x0, y0;
long wid, hgt;
unsigned char *pattern;
{
    struct rasterimage *pix = raster_GetPix((struct raster *)self->primaryobj);
    int ix, iy;
    long tbit;

    if (wid<0) {
	x0 += wid;
	wid = (-wid);
    }

    if (hgt<0) {
	y0 += hgt;
	hgt = (-hgt);
    }

    for (iy=y0; iy<=y0+hgt; iy++) 
	for (ix=x0; ix<=x0+wid; ix++) {
	    if (self->pattern==ZRPATTERN_INVERT)
		tbit = !rasterimage_GetPixel(pix, ix, iy);
	    else
		tbit = (self->pattern[iy&7]>>(ix&7)) & 1;
	    rasterimage_SetPixel(pix, ix, iy, tbit);
	}
}

static void FillCircle(self, x0, y0, rad, pattern)
struct rastoolview *self;
long x0, y0;
long rad;
unsigned char *pattern;
{
    struct rasterimage *pix = raster_GetPix((struct raster *)self->primaryobj);
    int ix, iy;
    long tbit;
    int cosv, sinv;

    for (sinv=(-rad); sinv<=rad; sinv++) {
	cosv = (int)sqrt((double)(rad*rad-sinv*sinv));
	iy = y0+sinv;
	for (ix=x0-cosv; ix<=x0+cosv; ix++) {
	    if (self->pattern==ZRPATTERN_INVERT)
		tbit = !rasterimage_GetPixel(pix, ix, iy);
	    else
		tbit = (self->pattern[iy&7]>>(ix&7)) & 1;
	    rasterimage_SetPixel(pix, ix, iy, tbit);
	}
    }
}

static void FillEllipse(self, x0, y0, xrad, yrad, pattern)
struct rastoolview *self;
long x0, y0;
long xrad, yrad; /* both must be >= 0 */
unsigned char *pattern;
{
    struct rasterimage *pix = raster_GetPix((struct raster *)self->primaryobj);
    int ix, iy;
    long tbit;
    double eccen;
    int cosv, sinv;

    if (yrad==0) yrad=1;
    eccen = (double)xrad / (double)yrad;

    for (sinv=(-yrad); sinv<=yrad; sinv++) {
	cosv = (int)(0.5 + eccen * sqrt((double)(yrad*yrad-sinv*sinv)));
	iy = y0+sinv;
	for (ix=x0-cosv; ix<=x0+cosv; ix++) {
	    if (self->pattern==ZRPATTERN_INVERT)
		tbit = !rasterimage_GetPixel(pix, ix, iy);
	    else
		tbit = (self->pattern[iy&7]>>(ix&7)) & 1;
	    rasterimage_SetPixel(pix, ix, iy, tbit);
	}
    }
}

static void DrawRectangle(rself, x0, y0, wid, hgt, pattern, brush)
struct rasterview *rself;
long x0, y0;
long wid, hgt;
unsigned char *pattern, *brush;
{
    if (wid<0) {
	x0 += wid;
	wid = (-wid);
    }

    if (hgt<0) {
	y0 += hgt;
	hgt = (-hgt);
    }

    if (wid)
	DrawLine(rself, x0, y0, x0+wid-1, y0, pattern, brush);
    if (hgt)
	DrawLine(rself, x0+wid, y0, x0+wid, y0+hgt-1, pattern, brush);
    if (wid)
	DrawLine(rself, x0+wid, y0+hgt, x0+1, y0+hgt, pattern, brush);
    if (hgt)
	DrawLine(rself, x0, y0+hgt, x0, y0+1, pattern, brush);
}

static void Tool_SolidRect(self, action, x, y, numclicks)
struct rastoolview *self;
enum view_MouseAction action;
long x, y, numclicks;
{
    struct rasterimage *pix = raster_GetPix((struct raster *)self->primaryobj);

    switch (action) {
	case view_LeftDown:
	    self->rockx = x;
	    self->rocky = y;
	    self->lastx = 0;
	    self->lasty = 0;
	    DrawRectangle(self->primaryview, self->rockx, self->rocky, 0, 0, ZRPATTERN_INVERT, ZRBRUSH_PIXEL);
	    break;
	case view_LeftMovement:
	    DrawRectangle(self->primaryview, self->rockx, self->rocky, self->lastx, self->lasty, ZRPATTERN_INVERT, ZRBRUSH_PIXEL);
	    self->lastx = x - self->rockx;
	    self->lasty = y - self->rocky;
	    DrawRectangle(self->primaryview, self->rockx, self->rocky, self->lastx, self->lasty, ZRPATTERN_INVERT, ZRBRUSH_PIXEL);
	    break;
	case view_LeftUp: 
	    DrawRectangle(self->primaryview, self->rockx, self->rocky, self->lastx, self->lasty, ZRPATTERN_INVERT, ZRBRUSH_PIXEL);
	    self->lastx = x - self->rockx;
	    self->lasty = y - self->rocky;
	    FillRectangle(self, self->rockx, self->rocky, self->lastx, self->lasty, self->pattern);
	    rasterimage_NotifyObservers(pix, raster_BITSCHANGED);
	    break;
    }
}

static void Tool_SolidCircle(self, action, x, y, numclicks)
struct rastoolview *self;
enum view_MouseAction action;
long x, y, numclicks;
{
    int rad;
    struct rasterimage *pix = raster_GetPix((struct raster *)self->primaryobj);

    switch (action) {
	case view_LeftDown:
	    self->rockx = x;
	    self->rocky = y;
	    self->lastx = 0; /* 0 radius */
	    DrawCircle(self->primaryview, self->rockx, self->rocky, 0, ZRPATTERN_INVERT, ZRBRUSH_PIXEL);
	    break;
	case view_LeftMovement:
	    rad = ((y-self->rocky)*(y-self->rocky)+(x-self->rockx)*(x-self->rockx));
	    rad = (int)(sqrt((double)rad));
	    if (rad == self->lastx)
		break;
	    DrawCircle(self->primaryview, self->rockx, self->rocky, self->lastx, ZRPATTERN_INVERT, ZRBRUSH_PIXEL);
	    DrawCircle(self->primaryview, self->rockx, self->rocky, rad, ZRPATTERN_INVERT, ZRBRUSH_PIXEL);
	    self->lastx = rad;
	    break;
	case view_LeftUp: 
	    rad = ((y-self->rocky)*(y-self->rocky)+(x-self->rockx)*(x-self->rockx));
	    rad = (int)(sqrt((double)rad));
	    DrawCircle(self->primaryview, self->rockx, self->rocky, self->lastx, ZRPATTERN_INVERT, ZRBRUSH_PIXEL);
	    FillCircle(self, self->rockx, self->rocky, rad, self->pattern);
	    rasterimage_NotifyObservers(pix, raster_BITSCHANGED);
	    break;
    }
}

static void Tool_SolidEllipse(self, action, x, y, numclicks)
struct rastoolview *self;
enum view_MouseAction action;
long x, y, numclicks;
{
    int xrad, yrad;
    struct rasterimage *pix = raster_GetPix((struct raster *)self->primaryobj);

    switch (action) {
	case view_LeftDown:
	    self->rockx = x;
	    self->rocky = y;
	    self->lastx = 0; /* 0 radius */
	    self->lasty = 0; /* 0 radius */
	    DrawEllipse(self->primaryview, self->rockx, self->rocky, 0, 0, ZRPATTERN_INVERT, ZRBRUSH_PIXEL);
	    break;
	case view_LeftMovement:
	    xrad = x-self->rockx;
	    yrad = y-self->rocky;
	    if (xrad<0) xrad = (-xrad);
	    if (yrad<0) yrad = (-yrad);
	    DrawEllipse(self->primaryview, self->rockx, self->rocky, self->lastx, self->lasty, ZRPATTERN_INVERT, ZRBRUSH_PIXEL);
	    DrawEllipse(self->primaryview, self->rockx, self->rocky, xrad, yrad, ZRPATTERN_INVERT, ZRBRUSH_PIXEL);
	    self->lastx = xrad;
	    self->lasty = yrad;
	    break;
	case view_LeftUp: 
	    xrad = x-self->rockx;
	    yrad = y-self->rocky;
	    if (xrad<0) xrad = (-xrad);
	    if (yrad<0) yrad = (-yrad);
	    DrawEllipse(self->primaryview, self->rockx, self->rocky, self->lastx, self->lasty, ZRPATTERN_INVERT, ZRBRUSH_PIXEL);
	    FillEllipse(self, self->rockx, self->rocky, xrad, yrad, self->pattern);
	    rasterimage_NotifyObservers(pix, raster_BITSCHANGED);
	    break;
    }
}

static void Tool_Paint(self, action, x, y, numclicks)
struct rastoolview *self;
enum view_MouseAction action;
long x, y, numclicks;
{
    struct rasterimage *pix = raster_GetPix((struct raster *)self->primaryobj);

    switch (action) {
	case view_LeftDown:
	    self->rockx = x;
	    self->rocky = y;
	    DrawLine(self->primaryview, self->rockx, self->rocky, x, y, self->pattern, self->brush);
	    rasterimage_NotifyObservers(pix, raster_BITSCHANGED);
	    break;
	case view_LeftMovement:
	    DrawLine(self->primaryview, self->rockx, self->rocky, x, y, self->pattern, self->brush);
	    self->rockx = x;
	    self->rocky = y;
	    rasterimage_NotifyObservers(pix, raster_BITSCHANGED);
	    break;
	case view_LeftUp: 
	    break;
    }
}

static void Tool_Line(self, action, x, y, numclicks)
struct rastoolview *self;
enum view_MouseAction action;
long x, y, numclicks;
{
    struct rasterimage *pix;

    switch (action) {
	case view_LeftDown:
	    self->rockx = x;
	    self->rocky = y;
	    DrawLine(self->primaryview, self->rockx, self->rocky, x, y, ZRPATTERN_INVERT, ZRBRUSH_PIXEL);
	    self->lastx = x;
	    self->lasty = y;
	    break;
	case view_LeftMovement:
	    DrawLine(self->primaryview, self->rockx, self->rocky, self->lastx, self->lasty, ZRPATTERN_INVERT, ZRBRUSH_PIXEL);
	    DrawLine(self->primaryview, self->rockx, self->rocky, x, y, ZRPATTERN_INVERT, ZRBRUSH_PIXEL);
	    self->lastx = x;
	    self->lasty = y;
	    break;
	case view_LeftUp: 
	    DrawLine(self->primaryview, self->rockx, self->rocky, self->lastx, self->lasty, ZRPATTERN_INVERT, ZRBRUSH_PIXEL);
	    DrawLine(self->primaryview, self->rockx, self->rocky, x, y, self->pattern, self->brush);
	    pix = raster_GetPix((struct raster *)self->primaryobj);
	    rasterimage_NotifyObservers(pix, raster_BITSCHANGED);
	    break;
    }
}

static void Tool_Circle(self, action, x, y, numclicks)
struct rastoolview *self;
enum view_MouseAction action;
long x, y, numclicks;
{
    struct rasterimage *pix;
    int rad;

    switch (action) {
	case view_LeftDown:
	    self->rockx = x;
	    self->rocky = y;
	    self->lastx = 0; /* 0 radius */
	    DrawCircle(self->primaryview, self->rockx, self->rocky, 0, ZRPATTERN_INVERT, ZRBRUSH_PIXEL);
	    break;
	case view_LeftMovement:
	    rad = ((y-self->rocky)*(y-self->rocky)+(x-self->rockx)*(x-self->rockx));
	    rad = (int)(sqrt((double)rad));
	    if (rad == self->lastx)
		break;
	    DrawCircle(self->primaryview, self->rockx, self->rocky, self->lastx, ZRPATTERN_INVERT, ZRBRUSH_PIXEL);
	    DrawCircle(self->primaryview, self->rockx, self->rocky, rad, ZRPATTERN_INVERT, ZRBRUSH_PIXEL);
	    self->lastx = rad;
	    break;
	case view_LeftUp: 
	    rad = ((y-self->rocky)*(y-self->rocky)+(x-self->rockx)*(x-self->rockx));
	    rad = (int)(sqrt((double)rad));
	    DrawCircle(self->primaryview, self->rockx, self->rocky, self->lastx, ZRPATTERN_INVERT, ZRBRUSH_PIXEL);
	    DrawCircle(self->primaryview, self->rockx, self->rocky, rad, self->pattern, self->brush);
	    pix = raster_GetPix((struct raster *)self->primaryobj);
	    rasterimage_NotifyObservers(pix, raster_BITSCHANGED);
	    break;
    }
}

static void Tool_Ellipse(self, action, x, y, numclicks)
struct rastoolview *self;
enum view_MouseAction action;
long x, y, numclicks;
{
    struct rasterimage *pix;
    long xrad, yrad;

    switch (action) {
	case view_LeftDown:
	    self->rockx = x;
	    self->rocky = y;
	    self->lastx = 0;
	    self->lasty = 0;
	    DrawEllipse(self->primaryview, self->rockx, self->rocky, 0, 0, ZRPATTERN_INVERT, ZRBRUSH_PIXEL);
	    break;
	case view_LeftMovement:
	    xrad = x-self->rockx;
	    yrad = y-self->rocky;
	    if (xrad<0) xrad = (-xrad);
	    if (yrad<0) yrad = (-yrad);
	    if (xrad == self->lastx && yrad == self->lasty)
		break;
	    DrawEllipse(self->primaryview, self->rockx, self->rocky, self->lastx, self->lasty, ZRPATTERN_INVERT, ZRBRUSH_PIXEL);
	    DrawEllipse(self->primaryview, self->rockx, self->rocky, xrad, yrad, ZRPATTERN_INVERT, ZRBRUSH_PIXEL);
	    self->lastx = xrad;
	    self->lasty = yrad;
	    break;
	case view_LeftUp: 
	    xrad = x-self->rockx;
	    yrad = y-self->rocky;
	    if (xrad<0) xrad = (-xrad);
	    if (yrad<0) yrad = (-yrad);
	    DrawEllipse(self->primaryview, self->rockx, self->rocky, self->lastx, self->lasty, ZRPATTERN_INVERT, ZRBRUSH_PIXEL);
	    DrawEllipse(self->primaryview, self->rockx, self->rocky, xrad, yrad, self->pattern, self->brush);
	    pix = raster_GetPix((struct raster *)self->primaryobj);
	    rasterimage_NotifyObservers(pix, raster_BITSCHANGED);
	    break;
    }
}

static void Tool_Rectangle(self, action, x, y, numclicks)
struct rastoolview *self;
enum view_MouseAction action;
long x, y, numclicks;
{
    struct rasterimage *pix;

    switch (action) {
	case view_LeftDown:
	    self->rockx = x;
	    self->rocky = y;
	    self->lastx = 0;
	    self->lasty = 0;
	    DrawRectangle(self->primaryview, self->rockx, self->rocky, 0, 0, ZRPATTERN_INVERT, ZRBRUSH_PIXEL);
	    break;
	case view_LeftMovement:
	    DrawRectangle(self->primaryview, self->rockx, self->rocky, self->lastx, self->lasty, ZRPATTERN_INVERT, ZRBRUSH_PIXEL);
	    self->lastx = x - self->rockx;
	    self->lasty = y - self->rocky;
	    DrawRectangle(self->primaryview, self->rockx, self->rocky, self->lastx, self->lasty, ZRPATTERN_INVERT, ZRBRUSH_PIXEL);
	    break;
	case view_LeftUp: 
	    DrawRectangle(self->primaryview, self->rockx, self->rocky, self->lastx, self->lasty, ZRPATTERN_INVERT, ZRBRUSH_PIXEL);
	    self->lastx = x - self->rockx;
	    self->lasty = y - self->rocky;
	    DrawRectangle(self->primaryview, self->rockx, self->rocky, self->lastx, self->lasty, self->pattern, self->brush);
	    pix = raster_GetPix((struct raster *)self->primaryobj);
	    rasterimage_NotifyObservers(pix, raster_BITSCHANGED);
	    break;
    }
}

static void Tool_Text(self, action, x, y, numclicks)
struct rastoolview *self;
enum view_MouseAction action;
long x, y, numclicks;
{
    struct rasterimage *pix;
    struct rectangle *VS, *sel;
    long w, h;

    VS = &self->primaryview->ViewSelection;
    sel = &(self->primaryview->DesiredSelection);

    if (x < VS->left) x = VS->left;
    if (y < VS->top) y = VS->top;
    if (x >= VS->left+VS->width) x = VS->left+VS->width;
    if (y >= VS->top+VS->height) y = VS->top+VS->height;

    switch (action) {
	case view_LeftDown:
	    self->rockx = x;
	    self->rocky = y;
	    self->lastx = 1;
	    self->lasty = 1;
	    rectangle_SetRectSize(sel, x, y, 1, 1);
	    rasterview_SetDesiredSelection(self->primaryview, sel);
	    break;
	case view_LeftMovement:
	    if (x >= self->rockx) {
		w = x-self->rockx;
		x = self->rockx;
	    }
	    else {
		w = self->rockx-x;
	    }
	    if (y >= self->rocky) {
		h = y-self->rocky;
		y = self->rocky;
	    }
	    else {
		h = self->rocky-y;
	    }
	    self->lastx = x;
	    self->lasty = y;
	    rectangle_SetRectSize(sel, x, y, w, h);
	    rasterview_SetDesiredSelection(self->primaryview, sel);
	    break;
	case view_LeftUp: 
	    if (x >= self->rockx) {
		w = x-self->rockx;
		x = self->rockx;
	    }
	    else {
		w = self->rockx-x;
	    }
	    if (y >= self->rocky) {
		h = y-self->rocky;
		y = self->rocky;
	    }
	    else {
		h = self->rocky-y;
	    }
	    if (w<8 && h<8) {
		h = 40;
		w = VS->width - x;
	    }
	    rectangle_SetRectSize(sel, x, y, w, h);
	    rasterview_SetDesiredSelection(self->primaryview, sel);
	    if (rasterview_GetOverlaidInset(self->primaryview)) {
		/* resize inset if there is one */
		rasterview_ResizeInset(self->primaryview);
		self->primaryview->needsFullUpdate=FALSE;
		rasterview_WantUpdate(self->primaryview, self); 
	    }
	    else {
		/* otherwise create inset */
		rasterview_OverlayInset(self->primaryview, "text");
	    }
	    RepostMenus(self);
	    break;
    }
}

static struct raster *GetPasted(self)
struct rastoolview *self;
{
    struct raster *ras = self->pasteraster;
    FILE *pasteFile;
    static char hdr[] = "\\begindata{raster,";
    char *hx = hdr;
    int c;

    /* it would be nice if there was some way to determine whether the cut buffer contents hadn't changed since the last paste operation; if so, we could just return self->pasteraster immediately. */

    /* we snarf the rasterview's IM rather than our own; it might be more efficient. */
    pasteFile = im_FromCutBuffer(rasterview_GetIM(self->primaryview));
    if (!pasteFile)
	return NULL;

    /* if the file does not begin with a raster, 
	we may as well scan until we find one */

    while ((c = getc(pasteFile)) != EOF && *hx) {
	if (c == *hx) hx++;
	else hx = (c == *hdr) ? hdr+1 : hdr;
    }

    if (*hx) {
	ras = NULL;
    }
    else {
	while ((c=getc(pasteFile)) != '\n' && c != EOF) ;
	/* skip to end of header */

	raster_Read(ras, pasteFile, 1);
    }
    im_CloseFromCutBuffer(rasterview_GetIM(self), pasteFile);

    return ras;
}

static void Tool_Paste(self, action, x, y, numclicks)
struct rastoolview *self;
enum view_MouseAction action;
long x, y, numclicks;
{
    struct rasterimage *pix, *pix2, *pix3;
    struct point pt;
    struct rectangle *sel, *VS, R, CR;
    struct raster *pras, *ras;
    long vx, vy, vwid, vhgt;

    sel = &(self->primaryview->DesiredSelection);
    ras = (struct raster *)self->primaryobj;
    pix = raster_GetPix(ras);

    switch (action) {
	case view_LeftDown:
	case view_RightDown:
	    pras = GetPasted(self);
	    if (!pras) {
		message_DisplayString(self->primaryview, 0, "No raster found in cut buffer.");
		self->rock = 0;
		return;
	    }
	    VS = &self->primaryview->ViewSelection;
	    vx = rectangle_Left(VS);
	    vy = rectangle_Top(VS);
	    vwid = rectangle_Width(VS);
	    vhgt = rectangle_Height(VS);
	    self->rockw = raster_GetWidth(self->pasteraster);
	    self->rockh = raster_GetHeight(self->pasteraster);

	    if (self->rockw > vwid || self->rockh > vhgt) {
		message_DisplayString(self->primaryview, 0, "The selection is too large to paste into this raster.");
		self->rock = 0;
		return;
	    }

	    self->rock = 1;

	    /* we now want to find out if the paste raster is the same as the rasterview's selected region. Since there's no good way to do this, we just check if they're the same size. */
	    if (self->rockw == rectangle_Width(sel) && self->rockh == rectangle_Height(sel)) {
		/* selection is probably the paste raster */
		/* leave selection alone */
	    }
	    else {
		rectangle_EmptyRect(sel);
		self->unpaste = FALSE;
		/* clear selection, don't allow unpaste dragging */
	    }

	    point_SetPt(&pt, x, y);
	    if (rectangle_IsPtInRect(&pt, sel)) {
		self->rockx = x - rectangle_Left(sel);
		self->rocky = y - rectangle_Top(sel);
		x = rectangle_Left(sel);
		y = rectangle_Top(sel);
	    }
	    else {
		self->rockx = 0;
		self->rocky = 0;
		if (x<vx) x = vx;
		if (y<vy) y = vy;
		if (x > vx+vwid-self->rockw) x = vx+vwid-self->rockw;
		if (y > vy+vhgt-self->rockh) y = vy+vhgt-self->rockh;
	    }

	    if (!self->unpaste) {
		raster_Resize(self->unpasteraster, self->rockw, self->rockh); 
		rectangle_EmptyRect(&CR);
	    }
	    else {
		/* slap unpasteraster on screen */
		pix3 = raster_GetPix(self->unpasteraster);
		rectangle_SetRectSize(&R, 0, 0, self->rockw, self->rockh);
		rasterimage_BlitSubraster(pix, self->lastx, self->lasty, pix3, &R, pixelimage_COPY);
		rectangle_SetRectSize(&CR, self->lastx, self->lasty, self->rockw, self->rockh);
	    }

	    /* yank current screen contents to unpasteraster */
	    pix3 = raster_GetPix(self->unpasteraster);
	    rectangle_SetRectSize(&R, x, y, self->rockw, self->rockh);
	    rasterimage_BlitSubraster(pix3, 0, 0, pix, &R, pixelimage_COPY);

	    /* slap pasteraster onto screen */
	    pix2 = raster_GetPix(self->pasteraster);
	    rectangle_SetRectSize(&R, 0, 0, self->rockw, self->rockh);
	    rasterimage_BlitSubraster(pix, x, y, pix2, &R, self->pastemode);

	    rasterimage_NotifyObservers(pix, raster_BITSCHANGED);

	    self->lastx = x;
	    self->lasty = y;

	    rectangle_EmptyRect(sel);
	    rasterview_SetDesiredSelection(self->primaryview, sel);
	    /* maybe ought to call RepostMenus(self) here, but the user isn't too likely to call for a menu while dragging the mouse in another window */
	    break;

	case view_LeftMovement:
	case view_RightMovement:
	    if (!self->rock) return;
	    x -= self->rockx;
	    y -= self->rocky;
	    VS = &self->primaryview->ViewSelection;
	    vx = rectangle_Left(VS);
	    vy = rectangle_Top(VS);
	    vwid = rectangle_Width(VS);
	    vhgt = rectangle_Height(VS);
	    if (x<vx) x = vx;
	    if (y<vy) y = vy;
	    if (x > vx+vwid-self->rockw) x = vx+vwid-self->rockw;
	    if (y > vy+vhgt-self->rockh) y = vy+vhgt-self->rockh;
	    if (x != self->lastx || y != self->lasty) {

		/* slap unpasteraster on screen */
		pix3 = raster_GetPix(self->unpasteraster);
		rectangle_SetRectSize(&R, 0, 0, self->rockw, self->rockh);
		rasterimage_BlitSubraster(pix, self->lastx, self->lasty, pix3, &R, pixelimage_COPY);
		rectangle_SetRectSize(&CR, self->lastx, self->lasty, self->rockw, self->rockh);

		/* yank current screen contents to unpasteraster */
		rectangle_SetRectSize(&R, x, y, self->rockw, self->rockh);
		rasterimage_BlitSubraster(pix3, 0, 0, pix, &R, pixelimage_COPY);

		/* slap pasteraster onto screen */
		pix2 = raster_GetPix(self->pasteraster);
		rectangle_SetRectSize(&R, 0, 0, self->rockw, self->rockh);
		rasterimage_BlitSubraster(pix, x, y, pix2, &R, self->pastemode);

		rasterimage_NotifyObservers(pix, raster_BITSCHANGED);

		self->lastx = x;
		self->lasty = y;
	    }
	    break;
	case view_LeftUp: 
	case view_RightUp: 
	    if (!self->rock) return;
	    x -= self->rockx;
	    y -= self->rocky;
	    VS = &self->primaryview->ViewSelection;
	    vx = rectangle_Left(VS);
	    vy = rectangle_Top(VS);
	    vwid = rectangle_Width(VS);
	    vhgt = rectangle_Height(VS);
	    if (x<vx) x = vx;
	    if (y<vy) y = vy;
	    if (x > vx+vwid-self->rockw) x = vx+vwid-self->rockw;
	    if (y > vy+vhgt-self->rockh) y = vy+vhgt-self->rockh;
	    if (x != self->lastx || y != self->lasty) {

		/* slap unpasteraster on screen */
		pix3 = raster_GetPix(self->unpasteraster);
		rectangle_SetRectSize(&R, 0, 0, self->rockw, self->rockh);
		rasterimage_BlitSubraster(pix, self->lastx, self->lasty, pix3, &R, pixelimage_COPY);
		rectangle_SetRectSize(&CR, self->lastx, self->lasty, self->rockw, self->rockh);
		
		/* yank current screen contents to unpasteraster */
		rectangle_SetRectSize(&R, x, y, self->rockw, self->rockh);
		rasterimage_BlitSubraster(pix3, 0, 0, pix, &R, pixelimage_COPY);

		/* slap pasteraster onto screen */
		pix2 = raster_GetPix(self->pasteraster);
		rectangle_SetRectSize(&R, 0, 0, self->rockw, self->rockh);
		rasterimage_BlitSubraster(pix, x, y, pix2, &R, self->pastemode);

		rasterimage_NotifyObservers(pix, raster_BITSCHANGED);
	    }

	    if (action==view_LeftUp)
		self->unpaste = TRUE;
	    else
		self->unpaste = FALSE;

	    /* replace highlight and redo scrollbars */
	    rectangle_SetRectSize(sel, x, y, self->rockw, self->rockh);
	    rasterview_SetDesiredSelection(self->primaryview, sel);
	    self->primaryview->needsFullUpdate=FALSE;
	    rasterview_WantUpdate(self->primaryview, self); 

	    RepostMenus(self);
	    break;
    }
}

static void Tool_FloodFill(self, action, x, y, numclicks)
struct rastoolview *self;
enum view_MouseAction action;
long x, y, numclicks;
{
    int bit;
    struct rasterimage *pix = raster_GetPix((struct raster *)self->primaryobj);
    long wid = rasterimage_GetWidth(pix);
    long hgt = rasterimage_GetHeight(pix);
    long xp, yp;
    struct span *tmp;
    struct rectangle R;

    if (action!=view_LeftDown && action!=view_RightDown)
	return;

    if (x<0 || y<0 || x>=wid || y>=hgt) {
	return;
    }

    if (self->fillstack) {
	message_DisplayString(self->primaryview, 10, "There is already a fill operation running.");
	return;
    }

    self->fillpix = rasterimage_Create(wid, hgt);
    rectangle_SetRectSize(&R, 0, 0, wid, hgt);
    rasterimage_BlitSubraster(self->fillpix, 0, 0, pix, &R, pixelimage_COPY); 
    bit = rasterimage_GetPixel(pix, x, y); 
    self->color = (bit ? 0 : 1);

    yp = y;
    for (xp=x; xp>=0 && rasterimage_GetPixel(pix, xp, yp)==bit; xp--);
    xp++;

    tmp = (struct span *)malloc(sizeof(struct span));
    tmp->x = xp;
    tmp->y = yp;
    tmp->next = NULL;
    self->fillstack = tmp;
    self->fillbit = bit;

    message_DisplayString(self->primaryview, 10, "Filling....");
    im_EnqueueEvent(FloodSplot, self, (event_MSECtoTU(10)));
}  

static void FloodSplot(self)
struct rastoolview *self;
{
    struct rasterimage *pix = raster_GetPix((struct raster *)self->primaryobj);
    struct rasterimage *fillpix = self->fillpix;
    long wid = rasterimage_GetWidth(pix);
    long hgt = rasterimage_GetHeight(pix);
    long xp, yp, startu, startd;
    long xmin, xmax, ymin, ymax;
    int wallu, walld;
    struct span *tmp;
    int bit;
    int numpoints = 0;

    xmin = wid;
    ymin = hgt;
    xmax = -1;
    ymax = -1;

    bit = self->fillbit;

    if (!self->fillstack) {
	rasterimage_NotifyObservers(pix, raster_BITSCHANGED);
	message_DisplayString(self->primaryview, 10, "Filled.");
	if (fillpix) {
	    rasterimage_Destroy(fillpix);
	    self->fillpix = NULL;
	}
	return;
    }

    while (numpoints<100) {
	if (self->fillstack == NULL)
	    break;

	xp = self->fillstack->x;
	yp = self->fillstack->y;
	tmp = self->fillstack;
	self->fillstack = self->fillstack->next;
	free(tmp);

	if (yp>0) {
	    if (bit!=rasterimage_GetPixel(fillpix, xp, yp-1))
		wallu = 1;
	    else {
		for (startu=xp; startu>=0 && rasterimage_GetPixel(fillpix, startu, yp-1)==bit; startu--);
		startu++;
		wallu = 0;
	    }
	}
	if (yp<hgt-1) {
	    if (bit!=rasterimage_GetPixel(fillpix, xp, yp+1))
		walld = 1;
	    else {
		for (startd=xp; startd>=0 && rasterimage_GetPixel(fillpix, startd, yp+1)==bit; startd--);
		startd++;
		walld = 0;
	    }
	}
	if (yp>ymax) ymax = yp;
	if (yp<ymin) ymin = yp;
	if (xp<xmax) xmin = xp;
	for (; xp<wid && rasterimage_GetPixel(fillpix, xp, yp)==bit; xp++) {
	    int tbit;
	    if (self->pattern==ZRPATTERN_INVERT)
		tbit = self->color;
	    else
		tbit = (self->pattern[yp&7]>>(xp&7)) & 1;
	    rasterimage_SetPixel(fillpix, xp, yp, self->color);
	    rasterimage_SetPixel(pix, xp, yp, tbit);
	    numpoints++;
	    if (yp>0) {
		if (wallu==0) {
		    if (bit!=rasterimage_GetPixel(fillpix, xp, yp-1)) {
			tmp = (struct span *)malloc(sizeof(struct span));
			tmp->x = startu;
			tmp->y = yp-1;
			tmp->next = self->fillstack;
			self->fillstack = tmp;
			wallu = 1;
		    }
		}
		else {
		    if (bit==rasterimage_GetPixel(fillpix, xp, yp-1)) {
			startu = xp;
			wallu = 0;
		    }
		}
	    }
	    if (yp<hgt-1) {
		if (walld==0) {
		    if (bit!=rasterimage_GetPixel(fillpix, xp, yp+1)) {
			tmp = (struct span *)malloc(sizeof(struct span));
			tmp->x = startd;
			tmp->y = yp+1;
			tmp->next = self->fillstack;
			self->fillstack = tmp;
			walld = 1;
		    }
		}
		else {
		    if (bit==rasterimage_GetPixel(fillpix, xp, yp+1)) {
			startd = xp;
			walld = 0;
		    }
		}
	    }
	}
	if (xp>xmax) xmax = xp;
	if (wallu==0 && yp>0) {
	    tmp = (struct span *)malloc(sizeof(struct span));
	    tmp->x = startu;
	    tmp->y = yp-1;
	    tmp->next = self->fillstack;
	    self->fillstack = tmp;
	    wallu = 1;
	}
	if (walld==0 && yp<hgt-1) {
	    tmp = (struct span *)malloc(sizeof(struct span));
	    tmp->x = startd;
	    tmp->y = yp+1;
	    tmp->next = self->fillstack;
	    self->fillstack = tmp;
	    walld = 1;
	}
    }

    im_EnqueueEvent(FloodSplot, self, (event_MSECtoTU(10)));
    /*rasterimage_NotifyObservers(pix, raster_BITSCHANGED);*/
}

static void Tool_SprayPaint(self, action, x, y, numclicks)
struct rastoolview *self;
enum view_MouseAction action;
long x, y, numclicks;
{
    switch (action) {
	case view_LeftDown:
	    self->rockx = x;
	    self->rocky = y;
	    self->rock = 1;
	    im_EnqueueEvent(SpraySplot, self, (event_MSECtoTU(20)));

	case view_LeftMovement:
	    self->rockx = x;
	    self->rocky = y;
	    break;

	case view_LeftUp:
	    self->rock = 0;
	    break;
    }
}

static void SpraySplot(self)
struct rastoolview *self;
{
    int rad = self->sprayradius;
    int sqrad = rad*rad;
    int ix, xoff, yoff;
    int limit = sqrad / 10 + 1;

    for (ix=0; ix<limit; ix++) {
	xoff = random()%(2*rad+1) - rad;
	yoff = random()%(2*rad+1) - rad;

	if (xoff*xoff+yoff*yoff <= sqrad)
	    rasterview_BrushSetPixel(self->primaryview, self->rockx+xoff, self->rocky+yoff, self->pattern, self->brush);
    }

    if (self->rock)
	im_EnqueueEvent(SpraySplot, self, (event_MSECtoTU(20)));
    else {
	struct rasterimage *pix = raster_GetPix((struct raster *)self->primaryobj);
	rasterimage_NotifyObservers(pix, raster_BITSCHANGED);
    }
}

static void Tool_CurvePaint(self, action, x, y, numclicks)
struct rastoolview *self;
enum view_MouseAction action;
long x, y, numclicks;
{
    switch (action) {
	case view_LeftDown:
	    self->rockx = x;
	    self->rocky = y;
	    self->lastx = x;
	    self->lasty = y;
	    self->lastvx = 0.0;
	    self->lastvy = 0.0;
	    self->rock = 1;
	    im_EnqueueEvent(CurveSplot, self, (event_MSECtoTU(20)));

	case view_LeftMovement:
	    self->rockx = x;
	    self->rocky = y;
	    break;

	case view_LeftUp:
	    self->rock = 0;
	    break;
    }
}

static void CurveSplot(self)
struct rastoolview *self;
{
    int x, y;
    
    self->lastvx += (double)(self->rockx - self->lastx) * self->springconst;
    self->lastvy += (double)(self->rocky - self->lasty) * self->springconst;
    self->lastvx *= 0.8;
    self->lastvy *= 0.8;
    x = self->lastx + self->lastvx;
    y = self->lasty + self->lastvy;

    DrawLine(self->primaryview, self->lastx, self->lasty, x, y, self->pattern, self->brush);    
    self->lastx = x;
    self->lasty = y;

    if (self->rock)
	im_EnqueueEvent(CurveSplot, self, (event_MSECtoTU(40)));
    else {
	struct rasterimage *pix = raster_GetPix((struct raster *)self->primaryobj);
	rasterimage_NotifyObservers(pix, raster_BITSCHANGED);
    }
}

static void PasteDownProc(self, rock)
struct rastoolview *self;
char *rock;
{
    if (self->unpaste) {
	self->unpaste = FALSE;
	RepostMenus(self);
    }
}

static void Toolmod_Paste(self, rock)
struct rastoolview *self;
char *rock;
{
    struct rasterimage *pix, *pix3;
    struct rectangle R;
    struct raster *ras;

    /* slap unpasteraster on screen */
    if (self->unpaste) {
	ras = (struct raster *)self->primaryobj;
	pix = raster_GetPix(ras);
	pix3 = raster_GetPix(self->unpasteraster);
	rectangle_SetRectSize(&R, 0, 0, self->rockw, self->rockh);
	rasterimage_BlitSubraster(pix, self->lastx, self->lasty, pix3, &R, pixelimage_COPY);
	rasterimage_NotifyObservers(pix, raster_BITSCHANGED);
	self->unpaste = FALSE;
	message_DisplayString(self, 40, "Removed paste selection.");
	RepostMenus(self);
    }
}

static void PasteResplot(self, rock)
struct rastoolview *self;
char *rock;
{
    struct rasterimage *pix, *pix2, *pix3;
    struct rectangle CR, R;
    struct raster *ras;

    if (self->unpaste) {
	ras = (struct raster *)self->primaryobj;
	pix = raster_GetPix(ras);

	/* slap unpasteraster on screen */
	pix3 = raster_GetPix(self->unpasteraster);
	rectangle_SetRectSize(&R, 0, 0, self->rockw, self->rockh);
	rasterimage_BlitSubraster(pix, self->lastx, self->lasty, pix3, &R, pixelimage_COPY);
	rectangle_SetRectSize(&CR, self->lastx, self->lasty, self->rockw, self->rockh);

	/* slap pasteraster onto screen */
	pix2 = raster_GetPix(self->pasteraster);
	rectangle_SetRectSize(&R, 0, 0, self->rockw, self->rockh);
	rasterimage_BlitSubraster(pix, self->lastx, self->lasty, pix2, &R, self->pastemode);

	rasterimage_NotifyObservers(pix, raster_BITSCHANGED);
    }
}

static void Toolmod_FloodFill(self, rock)
struct rastoolview *self;
char *rock;
{
    struct span *tmp;
    if (self->fillstack==NULL) {
	message_DisplayString(self, 10, "Cancel the current flood-fill operation: none running.");
    }
    else {
	while (self->fillstack) {
	    tmp = self->fillstack;
	    self->fillstack = tmp->next;
	    free(tmp);
	}
	message_DisplayString(self, 10, "Cancelled the current flood-fill operation.");
    }
}

static void Toolmod_SprayPaint(self, rock)
struct rastoolview *self;    
char *rock;
{
    char buffer[32], buf2[32];
    int val, res;
    sprintf(buf2, "%d", self->sprayradius);
    res = message_AskForString (self, 40, "How large should the spraypaint circle be in pixels?  ", buf2, buffer, 30); 
    if (res<0 || strlen(buffer)==0) {
	message_DisplayString(self, 10, "Cancelled.");
	return;
    }
    val = atoi(buffer);
    if (val<=0) {
	message_DisplayString(self, 10, "Value must be a positive integer.");
	return;
    }
    if (val>100) {
	message_DisplayString(self, 10, "That value is too large.");
	return;
    }
    self->sprayradius = val;
    sprintf(buf2, "Spray area set to %d", self->sprayradius);
    message_DisplayString(self, 10, buf2);
}
		
static void Toolmod_CurvePaint(self, rock)
struct rastoolview *self;    
char *rock;
{
    char buffer[32], buf2[32];
    int res;
    double val;
    sprintf(buf2, "0.5");
    res = message_AskForString (self, 40, "How stiff should the pen spring be? [0.0 -- 1.0]  ", buf2, buffer, 30); 
    if (res<0 || strlen(buffer)==0) {
	message_DisplayString(self, 10, "Cancelled.");
	return;
    }
    val = atof(buffer);
    if (val > 1.0 || val < -1.0) {
	message_DisplayString(self, 10, "That value is out of range.");
	return;
    }
    if (val == 0.0) {
	message_DisplayString(self, 10, "You cannot set the stiffness to zero.");
	return;
    }
    self->springconst = val / 10.0;
    sprintf(buf2, "Spring stiffness set to %g", val);
    message_DisplayString(self, 10, buf2);
}

static void Toolmod_Text(self, rock)
struct rastoolview *self;    
char *rock;
{
    if (!rasterview_GetOverlaidInset(self->primaryview)) {
	message_DisplayString(self, 10, "There is no overlay inset in the raster window.");
	return;
    }
    rasterview_ImprintInset(self->primaryview, self->pastemode);
    RepostMenus(self);
}

static void ResizeInsetProc(self, rock)
struct rastoolview *self;    
char *rock;
{
    if (!rasterview_GetOverlaidInset(self->primaryview)) {
	message_DisplayString(self, 10, "There is no overlay inset in the raster window.");
	return;
    }
    rasterview_ResizeInset(self->primaryview);
    RepostMenus(self);
}

static void RemoveInsetProc(self, rock)
struct rastoolview *self;    
char *rock;
{
    if (!rasterview_GetOverlaidInset(self->primaryview)) {
	message_DisplayString(self, 10, "There is no overlay inset in the raster window.");
	return;
    }
    rasterview_RemoveInset(self->primaryview);
    RepostMenus(self);
}

/* figtoolv.c - drawing editor toolset */
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
char *figotoolv_c_rcsid = "$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/figure/RCS/figtoolv.c,v 1.7 1993/05/04 01:18:42 susan Exp $";
#endif

#include <figtoolv.eh>

#include <ctype.h>
#include <class.h>
#include <math.h>

#include <menulist.ih>
#include <message.ih>
#include <keymap.ih>
#include <keystate.ih>
#include <cursor.ih>
#include <proctbl.ih>
#include <strtbl.ih>
#include <strtblv.ih>
#include <lpair.ih>
#include <view.ih>

#include <fontsel.ih>
#include <fontselv.ih>

#include <figv.ih>
#include <figure.ih>
#include <figobj.ih>
#include <figogrp.ih>
#include <figattr.ih>
#include <figoplin.ih>
#include <figospli.ih>
#include <figoins.ih>

#include <point.h>
#include <rect.h>

#define figtoolview_SelectClickDistance (6)

static struct menulist *Menus;
static struct keymap *Keymap;

static void SetToolProc(), SetShadeProc(), SetTextPosProc(), SetColorProc(), SetLineWidthProc(), SetRRectCornerProc(), CallCmdProc(), SetSnapGridProc(), SetExpertModeProc(), ApplyToSelProc(), RepostMenus();
static void Toolsub_Add(), Toolsub_Del();
static struct view *Tool_CreateProc(), *Tool_Select(), *Tool_AddPoints(), *Tool_DelPoints(), *Tool_AddAnchor(), *Tool_DelAnchor();
static void AbortObjectProc(), ToggleClosedProc(), ToggleSmoothProc();
static void Toolmod_Select();
static void AdjustToSelection(), AdjustToMenus();
static void Command_Quit(), Command_SelectAll(), Command_CutNPaste(), Command_Zoom(), Command_Refresh(), Command_GroupSel(), Command_UngroupSel(), Command_MoveToExtreme(), Command_PanToOrigin(), Command_LockCreate();
static void Command_SetDoConstraint(), Command_ClearAnchors(), Command_DefaultAnchors(), Command_ProportAnchors();
static char *CopyString(), *WhiteKillString();
static void LowerString();

#ifndef ABS
#define ABS(x) (((x)<0)?(-(x)):(x))
#endif

struct layout_t {
    struct view *(*proc)(); /* tool procedure */
    void (*procmod)(); /* procedure to call when tool is double-clicked */
    boolean writes; /* does the tool change the raster data? */
    char *name; /* name in the toolset window */
};

struct comlayout_t {
    void (*proc)(); /* command procedure */
    char *rock;	    /* a rock */
    char *name; /* name in the toolset window */
};

#define FIGTOOLS_NUM (5)
#define FIGTOOLSEX_NUM (2)
#define NumFigTools(self)  ((self)->toolextras ? (FIGTOOLS_NUM+FIGTOOLSEX_NUM) : FIGTOOLS_NUM)

static struct layout_t toollayout[FIGTOOLS_NUM+FIGTOOLSEX_NUM] = {
    {NULL, NULL, 0, "TOOLS:"},
    {NULL, NULL, 0, "Browse"},
    {Tool_Select, Toolmod_Select, 0, "Reshape"},
    {Tool_AddPoints, NULL, 1, "Add Points"},
    {Tool_DelPoints, NULL, 1, "Del Points"},

    {Tool_AddAnchor, NULL, 1, "Set Anchors"},
    {Tool_DelAnchor, NULL, 1, "Del Anchors"}
};

struct objlayout_t {
    char *name;	    /* name in the toolset window */
    long rock;	    /* rock passed to Instantiate() */
};

#define FIGOBJS_NUM (11)
static struct objlayout_t objectlayout[FIGOBJS_NUM] = {
    {"figotext", 0},
    {"figoplin", 1},
    {"figoplin", 0},
    {"figoplin", 2},
    {"figoplin", 3},
    {"figospli", 0},
    {"figospli", 2},
    {"figorect", 0},
    {"figoell",  0},
    {"figorrec", 0},
    {"figoins",  0}
};

#define FIGCMDS_NUM (15)
#define FIGCMDSEX_NUM (5)
#define FIGCMD_LOCKCREATE (14)
static struct comlayout_t cmdlayout[FIGCMDS_NUM+FIGCMDSEX_NUM] = {
    {NULL,		    NULL,	"COMMANDS:"},
    {Command_SelectAll,	    NULL,	"Select all"},
    {Command_CutNPaste,	    (char *)figview_OpCut, "Cut"},
    {Command_CutNPaste,	    (char *)figview_OpCopy, "Copy"},
    {Command_CutNPaste,	    (char *)figview_OpPaste, "Paste"},
    {Command_Zoom,	    (char *)1,	"Zoom in"},
    {Command_Zoom,	    (char *)-1,	"Zoom out"},
    {Command_Zoom,	    (char *)0,	"Normal size"},
    {Command_PanToOrigin,   NULL,	"Pan to origin"},
    {Command_Refresh,	    NULL,	"Refresh"},
    {Command_GroupSel,	    NULL,	"Group selec."},
    {Command_UngroupSel,    NULL,	"Ungroup selec."},
    {Command_MoveToExtreme, (char *)1,	"Move to front"},
    {Command_MoveToExtreme, (char *)0,	"Move to back"},
    {Command_LockCreate,    NULL,	"Keep creating"},

    {Command_SetDoConstraint,	(char *)1,  "Resizing on"},
    {Command_SetDoConstraint,	(char *)0,  "Resizing off"},
    {Command_ClearAnchors,	NULL,	    "Clear anchors"},
    {Command_DefaultAnchors,	NULL,	    "Standard anchors"},
    {Command_ProportAnchors,	NULL,	    "Propor. anchors"}
};

#define FIGSHADES_NUM (12)
#define figtoolv_shade_Inherit (1)
#define figtoolv_shade_Clear (2)
#define figtoolv_shade_Zero (3)

static char *shadelayout[FIGSHADES_NUM] = {
    "SHADES:",
    "<default>",
    "clear",
    "white",
    "grey 1/8", 
    "grey 1/4", 
    "grey 3/8", 
    "grey 1/2", 
    "grey 5/8", 
    "grey 3/4", 
    "grey 7/8", 
    "black"
};

#define FIGTEXTPOSS_NUM (5)
#define figtoolv_textpos_Inherit (1)
#define figtoolv_textpos_Zero (2)

static char *textposlayout[FIGTEXTPOSS_NUM] = {
    "TEXT POS:",
    "<default>",
    "Center",
    "Left", 
    "Right"
};

#define	FIGLINEWIDTHS_NUM_INIT (9)
#define figtoolv_linewidth_Inherit (1)
#define figtoolv_linewidth_Other (2)
#define figtoolv_linewidth_Zero (3)

static char *linewidthlayout[FIGLINEWIDTHS_NUM_INIT] = {
    "LINEWIDTHS:",
    "<default>",
    "<other>",
    "0",
    "1", 
    "2", 
    "4", 
    "6", 
    "8"
};

#define	FIGRRECTCORNERS_NUM_INIT (6)
#define figtoolv_rrectcorner_Inherit (1)
#define figtoolv_rrectcorner_Other (2)
#define figtoolv_rrectcorner_Zero (3)

static char *rrectcornerlayout[FIGRRECTCORNERS_NUM_INIT] = {
    "ROUND CORNER:",
    "<default>",
    "<other>",
    "10",
    "20", 
    "30"
};

#define	FIGCOLORS_NUM_INIT (10)
#define figtoolv_color_Inherit (1)
#define figtoolv_color_Other (2)
#define figtoolv_color_Zero (3)

static char *colorlayout[FIGCOLORS_NUM_INIT] = {
    "COLORS:",
    "<default>",
    "<other>",
    "black",
    "red", 
    "blue",
    "green",
    "cyan",
    "magenta",
    "yellow"
};

struct snapgridlayout_t {
    char *name;  /* name in the toolset window */
    long size;	    /* snap distance in figs */
};

#define figview_UnitConvert(unt)  (((unt)==figtoolv_snapgrid_inches) ? (double)(figview_FigUPerPix * 72) : (((unt)==figtoolv_snapgrid_cms) ? ((double)figview_FigUPerPix) * 28.25 : (double)figview_FigUPerPix))
/* 28.25 should be 28.346456692913, but let's not worry too much */

#define	FIGSNAPGRIDS_NUM_INIT	 (9)
#define figtoolv_snapgrid_points (1)
#define figtoolv_snapgrid_inches (2)
#define	figtoolv_snapgrid_cms	 (3)
#define figtoolv_snapgrid_Other  (4)
#define	figtoolv_snapgrid_Zero	 (5)

static struct snapgridlayout_t snapgridlayout[FIGSNAPGRIDS_NUM_INIT] = {
    {"GRID SNAP:",  0},
    {"points",	    0},
    {"inches", 	    0},
    {"cms",	    0},
    {"<other>",	    0},
    {"snap off",    0},
    {"0.125",	    9*figview_FigUPerPix},
    {"0.25",	    18*figview_FigUPerPix},
    {"0.5",	    36*figview_FigUPerPix}
};

#define ML_expertmode (1)
#define ML_objectcreating (2)

boolean figtoolview__InitializeClass(ClassID)
struct classhdr *ClassID;
{
    struct proctable_Entry *proc = NULL;
    
    Menus = menulist_New();
    Keymap = keymap_New();

    proc = proctable_DefineProc("figtoolv-toolset-destroy", Command_Quit, &figtoolview_classinfo, NULL, "Deletes toolset window.");
    keymap_BindToKey(Keymap, "\030\004", proc, 0);	/* ^X^D */
    keymap_BindToKey(Keymap, "\030\003", proc, 0);	/* ^X^C */
    menulist_AddToML(Menus, "Quit~99", proc, NULL, 0);
    
    proc = proctable_DefineProc("figtoolv-apply-to-selection", ApplyToSelProc, &figtoolview_classinfo, NULL, "Apply attributes to selected objects.");
    menulist_AddToML(Menus, "Apply to Selection~33", proc, NULL, 0);

    proc = proctable_DefineProc("figtoolv-toggle-smooth", ToggleSmoothProc, &figtoolview_classinfo, NULL, "Turn selected polylines into splines and back.");
    menulist_AddToML(Menus, "Smooth / Unsmooth~31", proc, NULL, 0);

    proc = proctable_DefineProc("figtoolv-toggle-closed", ToggleClosedProc, &figtoolview_classinfo, NULL, "Open or close selected polylines and splines.");
    menulist_AddToML(Menus, "Close / Open~32", proc, NULL, 0);

    proc = proctable_DefineProc("figtoolv-abort-object", AbortObjectProc, &figtoolview_classinfo, NULL, "Abort the object being built in the figview.");
    menulist_AddToML(Menus, "Cancel Object~11", proc, NULL, ML_objectcreating);

    return TRUE;
}

boolean figtoolview__InitializeObject(c, self)
struct classheader *c;
struct figtoolview *self;
{
    int ix;
    struct stringtbl *tl;
    struct strtblview *tlv;

    self->Menus = menulist_DuplicateML(Menus, self);
    self->Keystate = keystate_Create(self, Keymap);
    self->expertmode = FALSE;
    self->moribund = FALSE;
    self->primaryview = NULL;
    self->selectonup = FALSE;
    self->LockCreateMode = FALSE;

    self->menuatt = figattr_New();
    self->toolnum = 1; /* ### pan */

    self->shadenum = figtoolv_shade_Clear; 
    figattr_SetShade(self->menuatt, figattr_ShadeClear);
    self->textposnum = figtoolv_textpos_Zero;
    figattr_SetTextPos(self->menuatt, figattr_PosCenter);
    self->linewidthnum = figtoolv_linewidth_Zero+1; 
    figattr_SetLineWidth(self->menuatt, atoi(linewidthlayout[self->linewidthnum]));
    self->rrectcornernum = figtoolv_rrectcorner_Zero; 
    figattr_SetRRectCorner(self->menuatt, atoi(rrectcornerlayout[self->rrectcornernum]));
    self->colornum = figtoolv_color_Zero; 
    figattr_SetColor(self->menuatt, colorlayout[self->colornum]);
    figattr_SetFontSize(self->menuatt, fontsel_default_Size);
    figattr_SetFontStyle(self->menuatt, fontsel_default_Style);
    figattr_SetFontFamily(self->menuatt, fontsel_default_Family);
    /* ##new */

    self->toolproc = NULL;
    self->dummylist = (struct figobj **)malloc(FIGOBJS_NUM * sizeof(struct figobj *));
    self->creating = NULL;
    self->selectdeep = FALSE;
    self->rect_size = 8;
    self->rectlist = (struct rectangle *)malloc(self->rect_size * sizeof(struct rectangle));
    self->tmp_size = 0;
    self->tmplist = NULL;

    self->lp1 = lpair_New();
    self->lp2 = lpair_New();
    self->lp3 = lpair_New();
    self->lp4 = lpair_New();
    self->lp5 = lpair_New();
    self->lp6 = lpair_New();
    self->lp7 = lpair_New();

    if (!self->lp1 || !self->lp2 || !self->lp3 || !self->lp4 || !self->lp5 || !self->lp6 || !self->lp7 || !self->rectlist || !self->dummylist || !self->Menus || !self->Keystate)
	return FALSE;

    self->vfontsel = fontselview_New();
    self->fontsel = fontsel_New();
    if (!self->vfontsel || !self->fontsel)
	return FALSE;
    fontselview_SetDataObject(self->vfontsel, self->fontsel);
    fontsel_AddObserver(self->fontsel, self);
    fontselview_ShowExtraOption(self->vfontsel);

    /* tool table */
    tl = stringtbl_New();
    tlv = strtblview_New();
    if (!tl || !tlv) return FALSE;
    stringtbl_Clear(tl);
    self->toolacc = (short *)malloc(sizeof(short) * (FIGOBJS_NUM + FIGTOOLS_NUM));
    self->toolextras = FALSE;
    for (ix=0; ix<FIGTOOLS_NUM; ix++) {
	self->toolacc[ix] = stringtbl_AddString(tl, toollayout[ix].name);
	if (self->toolacc[ix] == (-1))
	    return FALSE;
    }
    for (ix=0; ix<FIGOBJS_NUM; ix++) {
	self->dummylist[ix] = (struct figobj *)class_NewObject(objectlayout[ix].name);
	if (!self->dummylist[ix]) 
	    return FALSE;
	self->toolacc[ix+FIGTOOLS_NUM] = stringtbl_AddString(tl, figobj_ToolName(self->dummylist[ix], self, objectlayout[ix].rock));
	if (self->toolacc[ix+FIGTOOLS_NUM] == (-1))
	    return FALSE;
    }
    strtblview_SetItemHitProc(tlv, SetToolProc, self);
    stringtbl_ClearBits(tl);
    stringtbl_SetBitOfEntry(tl, self->toolacc[self->toolnum], TRUE);
    strtblview_SetDataObject(tlv, tl);
    self->tooltbl = tl;
    self->vtooltbl = tlv;

    /* shade table */
    tl = stringtbl_New();
    tlv = strtblview_New();
    if (!tl || !tlv) return FALSE;
    stringtbl_Clear(tl);
    self->shadeacc = (short *)malloc(sizeof(short) * FIGSHADES_NUM);
    for (ix=0; ix<FIGSHADES_NUM; ix++) {
	self->shadeacc[ix] = stringtbl_AddString(tl, shadelayout[ix]);
	if (self->shadeacc[ix] == (-1))
	    return FALSE;
    }
    strtblview_SetItemHitProc(tlv, SetShadeProc, self);
    stringtbl_ClearBits(tl);
    stringtbl_SetBitOfEntry(tl, self->shadeacc[self->shadenum], TRUE);
    strtblview_SetDataObject(tlv, tl);
    self->shadetbl = tl;
    self->vshadetbl = tlv;

    /* textpos table */
    tl = stringtbl_New();
    tlv = strtblview_New();
    if (!tl || !tlv) return FALSE;
    stringtbl_Clear(tl);
    self->textposacc = (short *)malloc(sizeof(short) * FIGTEXTPOSS_NUM);
    for (ix=0; ix<FIGTEXTPOSS_NUM; ix++) {
	self->textposacc[ix] = stringtbl_AddString(tl, textposlayout[ix]);
	if (self->textposacc[ix] == (-1))
	    return FALSE;
    }
    strtblview_SetItemHitProc(tlv, SetTextPosProc, self);
    stringtbl_ClearBits(tl);
    stringtbl_SetBitOfEntry(tl, self->textposacc[self->textposnum], TRUE);
    strtblview_SetDataObject(tlv, tl);
    self->textpostbl = tl;
    self->vtextpostbl = tlv;

    /* linewidth table */
    tl = stringtbl_New();
    tlv = strtblview_New();
    if (!tl || !tlv) return FALSE;
    stringtbl_Clear(tl);
    self->linewidths_num = FIGLINEWIDTHS_NUM_INIT;
    self->linewidthacc = (short *)malloc(sizeof(short) * self->linewidths_num);
    self->linewidthlist = (short *)malloc(sizeof(short) * self->linewidths_num);
    for (ix=0; ix<self->linewidths_num; ix++) {
	self->linewidthacc[ix] = stringtbl_AddString(tl, linewidthlayout[ix]);
	if (self->linewidthacc[ix] == (-1))
	    return FALSE;
	if (ix>figtoolv_linewidth_Other)
	    self->linewidthlist[ix] = atoi(linewidthlayout[ix]);
    }
    strtblview_SetItemHitProc(tlv, SetLineWidthProc, self);
    stringtbl_ClearBits(tl);
    stringtbl_SetBitOfEntry(tl, self->linewidthacc[self->linewidthnum], TRUE);
    strtblview_SetDataObject(tlv, tl);
    self->linewidthtbl = tl;
    self->vlinewidthtbl = tlv;

    /* rrectcorner table */
    tl = stringtbl_New();
    tlv = strtblview_New();
    if (!tl || !tlv) return FALSE;
    stringtbl_Clear(tl);
    self->rrectcorners_num = FIGRRECTCORNERS_NUM_INIT;
    self->rrectcorneracc = (short *)malloc(sizeof(short) * self->rrectcorners_num);
    self->rrectcornerlist = (short *)malloc(sizeof(short) * self->rrectcorners_num);
    for (ix=0; ix<self->rrectcorners_num; ix++) {
	self->rrectcorneracc[ix] = stringtbl_AddString(tl, rrectcornerlayout[ix]);
	if (self->rrectcorneracc[ix] == (-1))
	    return FALSE;
	if (ix>figtoolv_rrectcorner_Other)
	    self->rrectcornerlist[ix] = atoi(rrectcornerlayout[ix]);
    }
    strtblview_SetItemHitProc(tlv, SetRRectCornerProc, self);
    stringtbl_ClearBits(tl);
    stringtbl_SetBitOfEntry(tl, self->rrectcorneracc[self->rrectcornernum], TRUE);
    strtblview_SetDataObject(tlv, tl);
    self->rrectcornertbl = tl;
    self->vrrectcornertbl = tlv;

    /* color table */
    tl = stringtbl_New();
    tlv = strtblview_New();
    if (!tl || !tlv) return FALSE;
    stringtbl_Clear(tl);
    self->colors_num = FIGCOLORS_NUM_INIT;
    self->coloracc = (short *)malloc(sizeof(short) * self->colors_num);
    self->colorlist = (char **)malloc(sizeof(char *) * self->colors_num);
    for (ix=0; ix<self->colors_num; ix++) {
	self->coloracc[ix] = stringtbl_AddString(tl, colorlayout[ix]);
	if (self->coloracc[ix] == (-1))
	    return FALSE;
	if (ix>figtoolv_color_Other)
	    self->colorlist[ix] = CopyString(colorlayout[ix]);
	else
	    self->colorlist[ix] = NULL;
    }
    strtblview_SetItemHitProc(tlv, SetColorProc, self);
    stringtbl_ClearBits(tl);
    stringtbl_SetBitOfEntry(tl, self->coloracc[self->colornum], TRUE);
    strtblview_SetDataObject(tlv, tl);
    self->colortbl = tl;
    self->vcolortbl = tlv;

    /* ##new */

    /* snapgrid table */
    self->snapgrid = 0;
    self->snapgridnum = figtoolv_snapgrid_Zero;
    self->snapgridunit = figtoolv_snapgrid_inches;
    tl = stringtbl_New();
    tlv = strtblview_New();
    if (!tl || !tlv) return FALSE;
    stringtbl_Clear(tl);
    self->snapgrids_num = FIGSNAPGRIDS_NUM_INIT;
    self->snapgridacc = (short *)malloc(sizeof(short) * self->snapgrids_num);
    self->snapgridlist = (short *)malloc(sizeof(short) * self->snapgrids_num);
    for (ix=0; ix<self->snapgrids_num; ix++) {
	self->snapgridacc[ix] = stringtbl_AddString(tl, snapgridlayout[ix].name);
	if (self->snapgridacc[ix] == (-1))
	    return FALSE;
	if (ix>figtoolv_snapgrid_Other)
	    self->snapgridlist[ix] = (snapgridlayout[ix].size);
    }
    strtblview_SetItemHitProc(tlv, SetSnapGridProc, self);
    stringtbl_ClearBits(tl);
    stringtbl_SetBitOfEntry(tl, self->snapgridacc[self->snapgridnum], TRUE);
    stringtbl_SetBitOfEntry(tl, self->snapgridacc[self->snapgridunit], TRUE);
    strtblview_SetDataObject(tlv, tl);
    self->snapgridtbl = tl;
    self->vsnapgridtbl = tlv;

    /* cmd table */
    tl = stringtbl_New();
    tlv = strtblview_New();
    if (!tl || !tlv) return FALSE;
    stringtbl_Clear(tl);
    self->cmdacc = (short *)malloc(sizeof(short) * FIGCMDS_NUM);
    for (ix=0; ix<FIGCMDS_NUM; ix++) {
	self->cmdacc[ix] = stringtbl_AddString(tl, cmdlayout[ix].name);
	if (self->cmdacc[ix] == (-1))
	    return FALSE;
    }
    strtblview_SetItemHitProc(tlv, CallCmdProc, self);
    stringtbl_ClearBits(tl);
    strtblview_SetDataObject(tlv, tl);
    self->cmdtbl = tl;
    self->vcmdtbl = tlv;

    lpair_SetUp(self->lp4, self->vtooltbl, self->vcmdtbl, 50, lpair_PERCENTAGE, lpair_VERTICAL, TRUE);
    lpair_SetUp(self->lp6, self->vlinewidthtbl, self->vrrectcornertbl, 50, lpair_PERCENTAGE, lpair_HORIZONTAL, TRUE);
    lpair_SetUp(self->lp3, self->lp6, self->vsnapgridtbl, 33, lpair_PERCENTAGE, lpair_HORIZONTAL, TRUE);
    lpair_SetUp(self->lp2, self->vshadetbl, self->vcolortbl, 50, lpair_PERCENTAGE, lpair_HORIZONTAL, TRUE);
    lpair_SetUp(self->lp7, self->vfontsel, self->vtextpostbl, 25, lpair_PERCENTAGE, lpair_HORIZONTAL, TRUE);
    lpair_SetUp(self->lp1, self->lp4, self->lp7, 33, lpair_PERCENTAGE, lpair_HORIZONTAL, TRUE);
    lpair_SetUp(self->lp5, self->lp2, self->lp3, 50, lpair_PERCENTAGE, lpair_VERTICAL, TRUE);
    figtoolview_SetUp(self, self->lp1, self->lp5, 50, lpair_PERCENTAGE, lpair_VERTICAL, TRUE);

    return TRUE;
}

void figtoolview__FinalizeObject(c, self)
struct classheader *c;
struct figtoolview *self;
{
    int ix;

    menulist_ClearChain(self->Menus);
    menulist_Destroy(self->Menus);
    keystate_Destroy(self->Keystate);

    fontsel_RemoveObserver(self->fontsel, self);

    figtoolview_SetNth(self, 0, NULL);
    figtoolview_SetNth(self, 1, NULL);

    lpair_Destroy(self->lp5);
    lpair_Destroy(self->lp1);
    lpair_Destroy(self->lp7);
    lpair_Destroy(self->lp2);
    lpair_Destroy(self->lp3);
    lpair_Destroy(self->lp6);
    lpair_Destroy(self->lp4);

    strtblview_Destroy(self->vshadetbl);
    strtblview_Destroy(self->vcolortbl);
    strtblview_Destroy(self->vlinewidthtbl);
    strtblview_Destroy(self->vrrectcornertbl);
    strtblview_Destroy(self->vtooltbl);
    strtblview_Destroy(self->vcmdtbl);
    strtblview_Destroy(self->vsnapgridtbl);
    fontselview_Destroy(self->vfontsel);

    stringtbl_Destroy(self->shadetbl);
    stringtbl_Destroy(self->colortbl);
    stringtbl_Destroy(self->linewidthtbl);
    stringtbl_Destroy(self->rrectcornertbl);
    stringtbl_Destroy(self->tooltbl);
    stringtbl_Destroy(self->cmdtbl);
    stringtbl_Destroy(self->snapgridtbl);
    fontsel_Destroy(self->fontsel);

    free(self->shadeacc);
    free(self->coloracc);
    free(self->linewidthacc);
    free(self->rrectcorneracc);
    free(self->toolacc);
    free(self->cmdacc);
    free(self->snapgridacc);

    for (ix=0; ix<self->colors_num; ix++) 
	if (self->colorlist[ix])
	    free(self->colorlist[ix]);

    free(self->colorlist);
    free(self->linewidthlist);
    free(self->rrectcornerlist);
    free(self->snapgridlist);

    for (ix=0; ix<FIGOBJS_NUM; ix++) {
	if (self->dummylist[ix])
	    figobj_Destroy(self->dummylist[ix]);
    }
    free(self->dummylist);
    free(self->rectlist);
    figattr_Destroy(self->menuatt);
    if (self->tmplist)
	free(self->tmplist);
}

void figtoolview__SetExpertMode(self, val)
struct figtoolview *self;
boolean val;
{
    int ix;

    val = (val) ? TRUE : FALSE;

    if (val==self->expertmode)
	return;

    self->expertmode = val;
    if (self->primaryview)
	figview_SetExpertMode(self->primaryview, val);
    fontselview_ShowExtraOption(self->vfontsel);

    if (stringtbl_NStrings(self->cmdtbl) == FIGCMDS_NUM) {
	self->cmdacc = (short *)realloc(self->cmdacc, sizeof(short) * (FIGCMDS_NUM+FIGCMDSEX_NUM));
	for (ix=FIGCMDS_NUM; ix<FIGCMDS_NUM+FIGCMDSEX_NUM; ix++) {
	    self->cmdacc[ix] = stringtbl_AddString(self->cmdtbl, cmdlayout[ix].name);
	}
	stringtbl_ClearBits(self->cmdtbl);
    }

    self->toolextras = TRUE;
    stringtbl_Clear(self->tooltbl);
    self->toolacc = (short *)realloc(self->toolacc, sizeof(short) * (FIGOBJS_NUM + FIGTOOLS_NUM + FIGTOOLSEX_NUM));
    for (ix=0; ix<FIGTOOLS_NUM+FIGTOOLSEX_NUM; ix++) {
	self->toolacc[ix] = stringtbl_AddString(self->tooltbl, toollayout[ix].name);
    }
    for (ix=0; ix<FIGOBJS_NUM; ix++) {
	self->toolacc[ix+FIGTOOLS_NUM+FIGTOOLSEX_NUM] = stringtbl_AddString(self->tooltbl, figobj_ToolName(self->dummylist[ix], self, objectlayout[ix].rock));
    }
    stringtbl_ClearBits(self->tooltbl);
    if (self->toolnum >= FIGTOOLS_NUM)
	self->toolnum += FIGTOOLSEX_NUM;
    stringtbl_SetBitOfEntry(self->tooltbl, self->toolacc[self->toolnum], TRUE);

    RepostMenus(self);
}

static void SetExpertModeProc(self, val)
struct figtoolview *self;
long val;
{
    figtoolview_SetExpertMode(self, val);
}

/* assumes self is input focus */
static void RepostMenus(self)
struct figtoolview *self;
{
    long menumask = 0;

    if (self->expertmode)
	menumask |= ML_expertmode;

    if (self->creating)
	menumask |= ML_objectcreating;

    if (menulist_SetMask(self->Menus, menumask)) {
	figtoolview_PostMenus(self, NULL);
    }
}

void figtoolview__PostMenus(self, ml)
struct figtoolview *self;
struct menulist *ml;
{
/* Enable the menus for this object. */

    menulist_ClearChain(self->Menus);
    if (ml) menulist_ChainBeforeML(self->Menus, ml, ml);
    super_PostMenus(self, self->Menus);
}

void figtoolview__PostKeyState(self, ks)
struct figtoolview *self;
struct keystate *ks;
{
/* Enable the keys for this object. */

    self->Keystate->next = NULL;
    keystate_AddBefore(self->Keystate, ks);
    super_PostKeyState(self, self->Keystate);
}

boolean figtoolview__SetPrimaryView(self, view)
struct figtoolview *self;
struct figview *view;
{
    if (self->primaryview) {
	figtoolview_RemoveObserver(self, self->primaryview);
	figview_RemoveObserver(self->primaryview, self);
    }

    if (!view) { /* odd, but somebody might do it */
	self->primaryview = NULL;
	figtoolview_SetDataObject(self, NULL);
	return TRUE;
    }

    self->primaryview = view;
    figtoolview_SetDataObject(self, figview_GetDataObject(view));
    figtoolview_AddObserver(self, view);
    figview_AddObserver(view, self);

    return TRUE;
}

void figtoolview__ObservedChanged(self, observed, status)
struct figtoolview *self;
struct observable *observed;
long status;
{
    if (observed == (struct observable *)self->primaryview) {
	if (status==observable_OBJECTDESTROYED) {
	    self->primaryview = NULL;
	    fprintf(stderr, "figtoolview: primary figview destroyed; suiciding...\n");
	    figtoolview_Destroy(self);
	}
	else {
	}
    }
    else if (observed == (struct observable *)self->fontsel) {
	if (status!=observable_OBJECTDESTROYED) {
	    long changemask = 0;
	    /*printf("figtool: observing: %c%s %c%d (%c%d)\n", 
		   (fontsel_IsActive(self->fontsel, fontsel_Family) ? ' ' : '!'), fontsel_GetFamily(self->fontsel), 
		   (fontsel_IsActive(self->fontsel, fontsel_Size) ? ' ' : '!'), fontsel_GetSize(self->fontsel), 
		   (fontsel_IsActive(self->fontsel, fontsel_Style) ? ' ' : '!'), fontsel_GetStyle(self->fontsel));
	    printf("figtool: menuatt was: %c%s %c%d (%c%d)\n", 
		   (figattr_IsActive(self->menuatt, figattr_FontFamily) ? ' ' : '!'), figattr_GetFontFamilyVal(self->menuatt), 
		   (figattr_IsActive(self->menuatt, figattr_FontSize) ? ' ' : '!'), figattr_GetFontSizeVal(self->menuatt), 
		   (figattr_IsActive(self->menuatt, figattr_FontStyle) ? ' ' : '!'), figattr_GetFontStyleVal(self->menuatt));*/

	    if (!fontsel_IsActive(self->fontsel, fontsel_Size)) {
		if (figattr_IsActive(self->menuatt, figattr_FontSize)) {
		    figattr_SetActive(self->menuatt, figattr_FontSize, FALSE);
		    changemask |= (1<<figattr_FontSize);
		}
	    }
	    else {
		if (!figattr_IsActive(self->menuatt, figattr_FontSize) || figattr_GetFontSizeVal(self->menuatt) != fontsel_GetSize(self->fontsel)) {
		    figattr_SetFontSize(self->menuatt, fontsel_GetSize(self->fontsel));
		    changemask |= (1<<figattr_FontSize);
		}
	    }

	    if (!fontsel_IsActive(self->fontsel, fontsel_Style)) {
		if (figattr_IsActive(self->menuatt, figattr_FontStyle)) {
		    figattr_SetActive(self->menuatt, figattr_FontStyle, FALSE);
		    changemask |= (1<<figattr_FontStyle);
		}
	    }
	    else {
		if (!figattr_IsActive(self->menuatt, figattr_FontStyle) || figattr_GetFontStyleVal(self->menuatt) != fontsel_GetStyle(self->fontsel)) {
		    figattr_SetFontStyle(self->menuatt, fontsel_GetStyle(self->fontsel));
		    changemask |= (1<<figattr_FontStyle);
		}
	    }

	    if (!fontsel_IsActive(self->fontsel, fontsel_Family)) {
		if (figattr_IsActive(self->menuatt, figattr_FontFamily)) {
		    figattr_SetActive(self->menuatt, figattr_FontFamily, FALSE);
		    changemask |= (1<<figattr_FontFamily);
		}
	    }
	    else {
		if (!figattr_IsActive(self->menuatt, figattr_FontFamily) || strcmp(figattr_GetFontFamilyVal(self->menuatt), fontsel_GetFamily(self->fontsel))) {
		    figattr_SetFontFamily(self->menuatt, fontsel_GetFamily(self->fontsel));
		    changemask |= (1<<figattr_FontFamily);
		}
	    }

	    if (changemask) {
		AdjustToMenus(self, changemask);
	    }
	}
    }
    else { /* observed = GetDataObject(self), we hope */
	if (status==observable_OBJECTDESTROYED) {
	    fprintf(stderr, "figtoolview: Primary fig dataobject destroyed.\n");
	}
	else {
	    AdjustToSelection(self);
	}
    }
}

void figtoolview__UnlinkTree(self)
struct figtoolview *self;
{
    super_UnlinkTree(self);

  /* This is inappropriate, being unlinked doesn't necessarily imply the view should be destroyed.
   if (!self->moribund) {
	Command_Quit(self, 0);
   }
 */
}

static void SetToolProc(st, self, accnum)
struct stringtbl *st;
struct figtoolview *self;
short accnum;
{
    int toolnum, objnum;
    struct figure *fig = (struct figure *)figtoolview_GetDataObject(self);

    for (toolnum=0; toolnum<NumFigTools(self)+FIGOBJS_NUM; toolnum++) {
	if (self->toolacc[toolnum] == accnum) break;
    }
    if (toolnum==NumFigTools(self)+FIGOBJS_NUM) {
	return;
    }
    if (toolnum==0) {
	message_DisplayString(self, 10, "Click on a tool to select it; click again to set tool parameters.");
	return;
    }

    if (toolnum >= NumFigTools(self)) 
	objnum = toolnum - NumFigTools(self); 
    else
	objnum = (-1);

    if (fig->ReadOnly && (objnum >= 0 || toollayout[toolnum].writes)) {
	message_DisplayString(self, 10, "Document is read-only.");
	return;
    }

    if (self->toolnum==toolnum) {
	if (objnum == -1)
	    self->toolproc = toollayout[toolnum].proc;
	else
	    self->toolproc = Tool_CreateProc;

	if (objnum >= 0) {
	    figobj_ToolModify(self->dummylist[objnum], self, objectlayout[objnum].rock);
	    return;
	}
	else {
	    void (*prmod)();
	    prmod = toollayout[toolnum].procmod;
	    if (prmod) 
		(*prmod)(self, NULL);
	    return;
	}
    }
    else {
	/* changed tools */
	self->toolnum = toolnum;
	if (objnum == -1)
	    self->toolproc = toollayout[toolnum].proc;
	else
	    self->toolproc = Tool_CreateProc;

	self->selectdeep = FALSE;
	if (self->toolproc != Tool_CreateProc) {
	    self->LockCreateMode = FALSE;
	    stringtbl_SetBitOfEntry(self->cmdtbl, self->cmdacc[FIGCMD_LOCKCREATE], FALSE);
	}

	figview_WantInputFocus(self->primaryview, self->primaryview);

	if (self->creating)
	    figtoolview_AbortObjectBuilding(self);

	if (self->toolproc==Tool_CreateProc || self->toolproc==NULL) {
	    figview_ClearSelection(self->primaryview);
	    figview_WantUpdate(self->primaryview, self->primaryview);
	}
    }

    if (!stringtbl_GetBitOfEntry(st, accnum)) {
	stringtbl_ClearBits(st);
	stringtbl_SetBitOfEntry(st, accnum, TRUE);
    }
}

static void SetShadeProc(st, self, accnum)
struct stringtbl *st;
struct figtoolview *self;
short accnum;
{
    int shadenum;

    for (shadenum=0; shadenum<FIGSHADES_NUM; shadenum++) {
	if (self->shadeacc[shadenum] == accnum) break;
    }
    if (shadenum==FIGSHADES_NUM) {
	return;
    }
    if (shadenum==0) {
	message_DisplayString(self, 10, "Click on a shade to select it.");
	return;
    }

    if (!stringtbl_GetBitOfEntry(st, accnum)) {
	stringtbl_ClearBits(st);
	stringtbl_SetBitOfEntry(st, accnum, TRUE);
    }

    /* regardless of whether it's a change */
    self->shadenum = shadenum;
    if (shadenum == figtoolv_shade_Inherit) {
	figattr_SetActive(self->menuatt, figattr_Shade, FALSE);
    }
    else {
	int val;
	if (shadenum == figtoolv_shade_Clear)
	    val = figattr_ShadeClear;
	else
	    val = shadenum - figtoolv_shade_Zero;
	figattr_SetShade(self->menuatt, val);
    }

    AdjustToMenus(self, (1<<figattr_Shade));
}

static void SetTextPosProc(st, self, accnum)
struct stringtbl *st;
struct figtoolview *self;
short accnum;
{
    int textposnum;

    for (textposnum=0; textposnum<FIGTEXTPOSS_NUM; textposnum++) {
	if (self->textposacc[textposnum] == accnum) break;
    }
    if (textposnum==FIGTEXTPOSS_NUM) {
	return;
    }
    if (textposnum==0) {
	message_DisplayString(self, 10, "Click on a text position to select it.");
	return;
    }

    if (!stringtbl_GetBitOfEntry(st, accnum)) {
	stringtbl_ClearBits(st);
	stringtbl_SetBitOfEntry(st, accnum, TRUE);
    }

    /* regardless of whether it's a change */
    self->textposnum = textposnum;
    if (textposnum == figtoolv_textpos_Inherit) {
	figattr_SetActive(self->menuatt, figattr_TextPos, FALSE);
    }
    else {
	int val;
	val = textposnum - figtoolv_textpos_Zero;
	figattr_SetTextPos(self->menuatt, val);
    }

    AdjustToMenus(self, (1<<figattr_TextPos));
}

static void InsertLineWidth(self, val)
struct figtoolview *self;
short val;
{
    struct stringtbl *st = self->linewidthtbl;   
    char name[24];
    int ix;

    ix = self->linewidths_num;
    self->linewidthacc = (short *)realloc(self->linewidthacc, sizeof(short) * (1+self->linewidths_num));
    self->linewidthlist = (short *)realloc(self->linewidthlist, sizeof(short) * (1+self->linewidths_num));

    sprintf(name, "%d", val);
    self->linewidthacc[ix] = stringtbl_AddString(st, name);
    if (self->linewidthacc[ix] == (-1))
	return; 
    self->linewidthlist[ix] = val;

    self->linewidths_num++;
}

static void SetLineWidthProc(st, self, accnum)
struct stringtbl *st;
struct figtoolview *self;
short accnum;
{
    int linewidthnum;

    for (linewidthnum=0; linewidthnum<self->linewidths_num; linewidthnum++) {
	if (self->linewidthacc[linewidthnum] == accnum) break;
    }
    if (linewidthnum==self->linewidths_num) {
	return;
    }
    if (linewidthnum==0) {
	message_DisplayString(self, 10, "Click on a line width to select it.");
	return;
    }

    if (linewidthnum == figtoolv_linewidth_Other) {
	int ix;
	short val;
	char buffer[32];
	int res;

	res = message_AskForString (self, 40, "Enter a new line width:  ", NULL, buffer, 30); 
	if (res<0 || strlen(buffer)==0) {
	    message_DisplayString(self, 10, "Cancelled.");
	    return;
	}
	val = atoi(buffer);

	if (val<0) {
	    message_DisplayString(self, 10, "Value must be a positive integer.");
	    return;
	}
	if (val>64) {
	    message_DisplayString(self, 10, "That value is too large.");
	    return;
	}

	for (ix=figtoolv_linewidth_Zero; ix<self->linewidths_num; ix++)
	    if (self->linewidthlist[ix] == val) {
		message_DisplayString(self, 10, "That value is already available.");
		linewidthnum = ix;
		break;
	    }
	if (linewidthnum == figtoolv_linewidth_Other) {
	    InsertLineWidth(self, val);

	    for (linewidthnum=figtoolv_linewidth_Zero; linewidthnum<self->linewidths_num; linewidthnum++) {
		if (self->linewidthlist[linewidthnum] == val) break;
	    }
	    if (linewidthnum==self->linewidths_num) {
		return;
	    }
	}
	accnum = self->linewidthacc[linewidthnum];
    }

    if (!stringtbl_GetBitOfEntry(st, accnum)) {
	stringtbl_ClearBits(st);
	stringtbl_SetBitOfEntry(st, accnum, TRUE);
    }

    /* regardless of whether it's a change */
    self->linewidthnum = linewidthnum;
    if (linewidthnum == figtoolv_linewidth_Inherit) {
	figattr_SetActive(self->menuatt, figattr_LineWidth, FALSE);
    }
    else {
	figattr_SetLineWidth(self->menuatt, self->linewidthlist[linewidthnum]);
    }

    AdjustToMenus(self, (1<<figattr_LineWidth));
}

static void InsertRRectCorner(self, val)
struct figtoolview *self;
short val;
{
    struct stringtbl *st = self->rrectcornertbl;   
    char name[24];
    int ix;

    ix = self->rrectcorners_num;
    self->rrectcorneracc = (short *)realloc(self->rrectcorneracc, sizeof(short) * (1+self->rrectcorners_num));
    self->rrectcornerlist = (short *)realloc(self->rrectcornerlist, sizeof(short) * (1+self->rrectcorners_num));

    sprintf(name, "%d", val);
    self->rrectcorneracc[ix] = stringtbl_AddString(st, name);
    if (self->rrectcorneracc[ix] == (-1))
	return; 
    self->rrectcornerlist[ix] = val;

    self->rrectcorners_num++;
}

static void SetRRectCornerProc(st, self, accnum)
struct stringtbl *st;
struct figtoolview *self;
short accnum;
{
    int rrectcornernum;

    for (rrectcornernum=0; rrectcornernum<self->rrectcorners_num; rrectcornernum++) {
	if (self->rrectcorneracc[rrectcornernum] == accnum) break;
    }
    if (rrectcornernum==self->rrectcorners_num) {
	return;
    }
    if (rrectcornernum==0) {
	message_DisplayString(self, 10, "Click on a corner width to select it.");
	return;
    }

    if (rrectcornernum == figtoolv_rrectcorner_Other) {
	int ix;
	short val;
	char buffer[32];
	int res;

	res = message_AskForString (self, 40, "Enter a new corner width:  ", NULL, buffer, 30); 
	if (res<0 || strlen(buffer)==0) {
	    message_DisplayString(self, 10, "Cancelled.");
	    return;
	}
	val = atoi(buffer);

	if (val<0) {
	    message_DisplayString(self, 10, "Value must be a non-negative integer.");
	    return;
	}
	if (val>100) {
	    message_DisplayString(self, 10, "That value is too large.");
	    return;
	}

	for (ix=figtoolv_rrectcorner_Zero; ix<self->rrectcorners_num; ix++)
	    if (self->rrectcornerlist[ix] == val) {
		message_DisplayString(self, 10, "That value is already available.");
		rrectcornernum = ix;
		break;
	    }
	if (rrectcornernum == figtoolv_rrectcorner_Other) {
	    InsertRRectCorner(self, val);

	    for (rrectcornernum=figtoolv_rrectcorner_Zero; rrectcornernum<self->rrectcorners_num; rrectcornernum++) {
		if (self->rrectcornerlist[rrectcornernum] == val) break;
	    }
	    if (rrectcornernum==self->rrectcorners_num) {
		return;
	    }
	}
	accnum = self->rrectcorneracc[rrectcornernum];
    }

    if (!stringtbl_GetBitOfEntry(st, accnum)) {
	stringtbl_ClearBits(st);
	stringtbl_SetBitOfEntry(st, accnum, TRUE);
    }

    /* regardless of whether it's a change */
    self->rrectcornernum = rrectcornernum;
    if (rrectcornernum == figtoolv_rrectcorner_Inherit) {
	figattr_SetActive(self->menuatt, figattr_RRectCorner, FALSE);
    }
    else {
	figattr_SetRRectCorner(self->menuatt, self->rrectcornerlist[rrectcornernum]);
    }

    AdjustToMenus(self, (1<<figattr_RRectCorner));
}

static void InsertColor(self, val)
struct figtoolview *self;
char *val;
{
    struct stringtbl *st = self->colortbl;   
    int ix;

    ix = self->colors_num;
    self->coloracc = (short *)realloc(self->coloracc, sizeof(short) * (1+self->colors_num));
    self->colorlist = (char **)realloc(self->colorlist, sizeof(char *) * (1+self->colors_num));

    self->coloracc[ix] = stringtbl_AddString(st, val);
    if (self->coloracc[ix] == (-1))
	return; 
    self->colorlist[ix] = CopyString(val);

    self->colors_num++;
}

static void SetColorProc(st, self, accnum)
struct stringtbl *st;
struct figtoolview *self;
short accnum;
{
    int colornum;

    for (colornum=0; colornum<self->colors_num; colornum++) {
	if (self->coloracc[colornum] == accnum) break;
    }
    if (colornum==self->colors_num) {
	return;
    }
    if (colornum==0) {
	message_DisplayString(self, 10, "Click on a color to select it.");
	return;
    }

    if (colornum == figtoolv_color_Other) {
	int ix;
	char buffer[64];
	char *val;
	int res;

	res = message_AskForString (self, 40, "Enter a new color:  ", NULL, buffer, 62); 
	if (res<0 || strlen(buffer)==0) {
	    message_DisplayString(self, 10, "Cancelled.");
	    return;
	}
	LowerString(buffer);
	val = WhiteKillString(buffer);
	if (strlen(val)==0) {
	    message_DisplayString(self, 10, "Cancelled.");
	    return;
	}

	for (ix=figtoolv_color_Zero; ix<self->colors_num; ix++)
	    if (!strcmp(self->colorlist[ix], val)) {
		message_DisplayString(self, 10, "That value is already available.");
		colornum = ix;
		break;
	    }
	if (colornum == figtoolv_color_Other) {
	    InsertColor(self, val);

	    for (colornum=figtoolv_color_Zero; colornum<self->colors_num; colornum++) {
		if (!strcmp(self->colorlist[colornum], val)) break;
	    }
	    if (colornum==self->colors_num) {
		return;
	    }
	}
	accnum = self->coloracc[colornum];
    }

    if (!stringtbl_GetBitOfEntry(st, accnum)) {
	stringtbl_ClearBits(st);
	stringtbl_SetBitOfEntry(st, accnum, TRUE);
    }

    /* regardless of whether it's a change */
    self->colornum = colornum;
    if (colornum == figtoolv_color_Inherit) {
	figattr_SetActive(self->menuatt, figattr_Color, FALSE);
    }
    else {
	figattr_SetColor(self->menuatt, self->colorlist[colornum]);
    }

    AdjustToMenus(self, (1<<figattr_Color));
}

static void InsertSnapGrid(self, val)
struct figtoolview *self;
short val;
{
    struct stringtbl *st = self->snapgridtbl;   
    char name[24];
    int ix;
    double convert;

    convert = figview_UnitConvert(self->snapgridunit);

    ix = self->snapgrids_num;
    self->snapgridacc = (short *)realloc(self->snapgridacc, sizeof(short) * (1+self->snapgrids_num));
    self->snapgridlist = (short *)realloc(self->snapgridlist, sizeof(short) * (1+self->snapgrids_num));

    sprintf(name, "%.3g", (double)val / convert);
    self->snapgridacc[ix] = stringtbl_AddString(st, name);
    if (self->snapgridacc[ix] == (-1))
	return; 
    self->snapgridlist[ix] = val;

    self->snapgrids_num++;
}

static void SetSnapGridProc(st, self, accnum)
struct stringtbl *st;
struct figtoolview *self;
short accnum;
{
    int snapgridnum;
    int ix;
    char buffer[16];
    double convert;

    for (snapgridnum=0; snapgridnum<self->snapgrids_num; snapgridnum++) {
	if (self->snapgridacc[snapgridnum] == accnum) break;
    }
    if (snapgridnum==self->snapgrids_num) {
	return;
    }
    if (snapgridnum==0) {
	message_DisplayString(self, 10, "Click on a size to select it.");
	return;
    }
    if (snapgridnum < figtoolv_snapgrid_Other) {
	if (snapgridnum != self->snapgridunit) {
	    self->snapgridunit = snapgridnum;
	    convert = figview_UnitConvert(self->snapgridunit);
	    for (ix=figtoolv_snapgrid_Zero; ix<self->snapgrids_num; ix++)
		stringtbl_RemoveEntry(st, self->snapgridacc[ix]); 
	    for (ix=figtoolv_snapgrid_Zero; ix<self->snapgrids_num; ix++) {
		if (self->snapgridlist[ix]==0)
		    strcpy(buffer, "snap off");
		else
		    sprintf(buffer, "%.3g", (double)self->snapgridlist[ix] / convert);
		self->snapgridacc[ix] = stringtbl_AddString(st, buffer);
	    }

	    stringtbl_ClearBits(st);
	    stringtbl_SetBitOfEntry(st, self->snapgridacc[self->snapgridnum], TRUE);
	    stringtbl_SetBitOfEntry(st, accnum, TRUE);
	}
	return;
    }

    if (snapgridnum == figtoolv_snapgrid_Other) {
	double val;
	long ival;
	char buffer[32];
	char obuffer[256];
	int res;

	convert = figview_UnitConvert(self->snapgridunit);

	sprintf(obuffer, "Enter a new grid size (in %s):  ", snapgridlayout[self->snapgridunit].name);

	res = message_AskForString (self, 40, obuffer, NULL, buffer, 30); 
	if (res<0 || strlen(buffer)==0) {
	    message_DisplayString(self, 10, "Cancelled.");
	    return;
	}
	val = atof(buffer);
	
	if (val<=0.0) {
	    message_DisplayString(self, 10, "Value must be a positive number.");
	    return;
	}

	ival = (long)(val * convert);

	for (ix=figtoolv_snapgrid_Zero; ix<self->snapgrids_num; ix++)
	    if (self->snapgridlist[ix] == ival) {
		message_DisplayString(self, 10, "That value is already available.");
		snapgridnum = ix;
		break;
	    }
	if (snapgridnum == figtoolv_snapgrid_Other) {
	    InsertSnapGrid(self, ival);

	    for (snapgridnum=figtoolv_snapgrid_Zero; snapgridnum<self->snapgrids_num; snapgridnum++) {
		if (self->snapgridlist[snapgridnum] == ival) break;
	    }
	    if (snapgridnum==self->snapgrids_num) {
		return;
	    }
	}
	accnum = self->snapgridacc[snapgridnum];
    }

    if (!stringtbl_GetBitOfEntry(st, accnum)) {
	stringtbl_ClearBits(st);
	stringtbl_SetBitOfEntry(st, accnum, TRUE);
	stringtbl_SetBitOfEntry(st, self->snapgridacc[self->snapgridunit], TRUE);
    }

    /* regardless of whether it's a change */
    self->snapgridnum = snapgridnum;
    self->snapgrid = self->snapgridlist[self->snapgridnum];
}

static void CallCmdProc(st, self, accnum)
struct stringtbl *st;
struct figtoolview *self;
short accnum;
{
    int cmdnum;
    void (*comproc)();

    for (cmdnum=0; cmdnum<stringtbl_NStrings(self->cmdtbl); cmdnum++) {
	if (self->cmdacc[cmdnum] == accnum) break;
    }
    if (cmdnum==stringtbl_NStrings(self->cmdtbl)) {
	return;
    }
    if (cmdnum==0) {
	message_DisplayString(self, 10, "Click on any of these commands.");
	return;
    }

    comproc = cmdlayout[cmdnum].proc;
    if (comproc)
	(*comproc)(self, cmdlayout[cmdnum].rock);
}

static void Command_Quit(self, rock)
struct figtoolview *self;
char *rock;
{
    if (self->primaryview) {
	figview_DestroyToolset(self->primaryview);
    }
    else {
	fprintf(stderr, "figtoolview: warning: no primary figview.\n");
	figtoolview_Destroy(self);
    }
}

static boolean CSA_Splot(o, ref, fig, figv)
struct figobj *o;
long ref;
struct figure *fig;
struct figview *figv;
{
    figview_SelectByRef(figv, ref);
    return FALSE;
}

static void Command_LockCreate(self, rock)
struct figtoolview *self;
char *rock;
{
    if (!self->LockCreateMode) {
	if (self->toolproc != Tool_CreateProc) {
	    message_DisplayString(self, 10, "Not in an object-creation mode.");
	}
	else {
	    self->LockCreateMode = TRUE;
	    stringtbl_SetBitOfEntry(self->cmdtbl, self->cmdacc[FIGCMD_LOCKCREATE], TRUE);
	    message_DisplayString(self, 10, "Will stay in object-creation mode until told otherwise.");
	}
    }
    else {
	self->LockCreateMode = FALSE;
	stringtbl_SetBitOfEntry(self->cmdtbl, self->cmdacc[FIGCMD_LOCKCREATE], FALSE);
	if (self->toolproc == Tool_CreateProc) {
	    message_DisplayString(self, 10, "Will return to reshape mode after next object.");
	}
    }
}

static void Command_CutNPaste(self, rock)
struct figtoolview *self;
long rock;
{
    struct figview *figv = self->primaryview;
    if (!figv) return;

    figview_CutNPaste(figv, rock, 0);
}

static void Command_SelectAll(self, rock)
struct figtoolview *self;
char *rock;
{
    struct figview *figv = self->primaryview;
    struct figure *fig;
    if (!figv) return;
    fig = (struct figure *)figview_GetDataObject(figv);
    if (!fig) return;

    figview_ClearSelection(figv);
    figure_EnumerateObjectGroup(fig, figview_GetFocusRef(figv), NULL, FALSE, CSA_Splot, figv);
    figview_WantUpdate(figv, figv);

    AdjustToSelection(self);
}

static void Command_Refresh(self, rock)
struct figtoolview *self;
char *rock;
{
    struct figview *figv = self->primaryview;

    if (!figv) return;

    figv->NeedFullUpdate = TRUE;
    figview_WantUpdate(figv, figv);
}

static void Command_Zoom(self, rock)
struct figtoolview *self;
long rock;
{
    struct figview *figv = self->primaryview;

    if (!figv) return;
    figview_ChangeZoom(figv, rock);
}

static void Command_PanToOrigin(self, rock)
struct figtoolview *self;
long rock;
{
    struct figview *figv = self->primaryview;

    if (!figv) return;
    if (figv->panx==0 && figv->pany==0)
	return;

    figv->panx = 0;
    figv->pany = 0;
    figv->NeedFullUpdate = TRUE;
    figview_WantUpdate(figv, figv);
}

static void ATSPSplot(o, ref, vv, attr)
struct figobj *o;
long ref;
struct figview *vv;
struct menuatt *attr;
{
    figobj_UpdateVAttributes(o, attr, figattr_MaskAll);
}

static void ApplyToSelProc(self, rock)
struct figtoolview *self;
long rock;
{
    struct figure *fig = (struct figure *)figview_GetDataObject(self->primaryview);
    if (!fig) return;

    if (figview_GetNumSelected(self->primaryview) == 0) {
	message_DisplayString(self, 10, "No objects are selected in the document.");
	return;
    }

    figview_EnumerateSelection(self->primaryview, ATSPSplot, self->menuatt);
    figure_NotifyObservers(fig, figure_DATACHANGED);
}

static void AdjustToMenus(self, mask)
struct figtoolview *self;
long mask;
{
    struct figure *fig = (struct figure *)figtoolview_GetDataObject(self);
    boolean changed = FALSE;
    long ref;
    struct figobj *o;
    if (!fig) return;

    ref = figview_GetOneSelected(self->primaryview); 
    if (ref != figure_NULLREF) {
	o = figure_FindObjectByRef(fig, ref);
	figobj_UpdateVAttributes(o, self->menuatt, mask);
	changed = TRUE;
    }
    if (self->creating) {
	figobj_UpdateVAttributes(self->creating, self->menuatt, mask);
	changed = TRUE;
    }

    if (changed)
	figure_NotifyObservers(fig, figure_DATACHANGED);
}

static void AdjustToSelection(self)
struct figtoolview *self;
{
    long ref = figview_GetOneSelected(self->primaryview);
    long ix, vnum, accnum;
    char *cx;
    struct figobj *o;
    boolean fselchanged = FALSE;

    if (ref == figure_NULLREF)
	return; /* more or less than 1 selected */

    o = self->primaryview->objs[ref].o;

    figattr_CopyData(self->menuatt, figobj_GetVAttributes(o), figattr_MaskAll);

    if (!figattr_IsActive(self->menuatt, figattr_Shade))
	vnum = figtoolv_shade_Inherit;
    else {
	ix = figattr_GetShadeVal(self->menuatt);
	if (ix==figattr_ShadeClear)
	    vnum = figtoolv_shade_Clear;
	else
	    vnum = ix + figtoolv_shade_Zero;
    }
    accnum = self->shadeacc[vnum];
    if (!stringtbl_GetBitOfEntry(self->shadetbl, accnum)) {
	stringtbl_ClearBits(self->shadetbl);
	stringtbl_SetBitOfEntry(self->shadetbl, accnum, TRUE);
    }

    if (!figattr_IsActive(self->menuatt, figattr_TextPos))
	vnum = figtoolv_textpos_Inherit;
    else {
	ix = figattr_GetTextPosVal(self->menuatt);
	vnum = ix + figtoolv_textpos_Zero;
    }
    accnum = self->textposacc[vnum];
    if (!stringtbl_GetBitOfEntry(self->textpostbl, accnum)) {
	stringtbl_ClearBits(self->textpostbl);
	stringtbl_SetBitOfEntry(self->textpostbl, accnum, TRUE);
    }

    if (!figattr_IsActive(self->menuatt, figattr_LineWidth))
	vnum = figtoolv_linewidth_Inherit;
    else {
	ix = figattr_GetLineWidthVal(self->menuatt);
	for (vnum=figtoolv_linewidth_Zero; vnum<self->linewidths_num; vnum++)
	    if (self->linewidthlist[vnum] == ix) break;
	if (vnum==self->linewidths_num) {
	    InsertLineWidth(self, ix);
	    vnum = self->linewidths_num - 1;
	}
    }
    accnum = self->linewidthacc[vnum];
    if (!stringtbl_GetBitOfEntry(self->linewidthtbl, accnum)) {
	stringtbl_ClearBits(self->linewidthtbl);
	stringtbl_SetBitOfEntry(self->linewidthtbl, accnum, TRUE);
    }

    if (!figattr_IsActive(self->menuatt, figattr_RRectCorner))
	vnum = figtoolv_rrectcorner_Inherit;
    else {
	ix = figattr_GetRRectCornerVal(self->menuatt);
	for (vnum=figtoolv_rrectcorner_Zero; vnum<self->rrectcorners_num; vnum++)
	    if (self->rrectcornerlist[vnum] == ix) break;
	if (vnum==self->rrectcorners_num) {
	    InsertRRectCorner(self, ix);
	    vnum = self->rrectcorners_num - 1;
	}
    }
    accnum = self->rrectcorneracc[vnum];
    if (!stringtbl_GetBitOfEntry(self->rrectcornertbl, accnum)) {
	stringtbl_ClearBits(self->rrectcornertbl);
	stringtbl_SetBitOfEntry(self->rrectcornertbl, accnum, TRUE);
    }

    if (!figattr_IsActive(self->menuatt, figattr_Color))
	vnum = figtoolv_color_Inherit;
    else {
	cx = figattr_GetColorVal(self->menuatt);
	for (vnum=figtoolv_color_Zero; vnum<self->colors_num; vnum++)
	    if (!strcmp(self->colorlist[vnum], cx)) break;
	if (vnum==self->colors_num) {
	    InsertColor(self, cx);
	    vnum = self->colors_num - 1;
	}
    }
    accnum = self->coloracc[vnum];
    if (!stringtbl_GetBitOfEntry(self->colortbl, accnum)) {
	stringtbl_ClearBits(self->colortbl);
	stringtbl_SetBitOfEntry(self->colortbl, accnum, TRUE);
    }

    if (!figattr_IsActive(self->menuatt, figattr_FontFamily)) {
	if (fontsel_IsActive(self->fontsel, fontsel_Family)) {
	    fontsel_UnsetFamily(self->fontsel);
	    fselchanged = TRUE;
	}
    }
    else {
	if (!fontsel_IsActive(self->fontsel, fontsel_Family)
	    || strcmp(figattr_GetFontFamilyVal(self->menuatt), fontsel_GetFamily(self->fontsel))) {
	    fontsel_SetFamily(self->fontsel, figattr_GetFontFamilyVal(self->menuatt));
	    fselchanged = TRUE;
	}
    }

    if (!figattr_IsActive(self->menuatt, figattr_FontSize)) {
	if (fontsel_IsActive(self->fontsel, fontsel_Size)) {
	    fontsel_UnsetSize(self->fontsel);
	    fselchanged = TRUE;
	}
    }
    else {
	if (!fontsel_IsActive(self->fontsel, fontsel_Size)
	    || (figattr_GetFontSizeVal(self->menuatt) != fontsel_GetSize(self->fontsel))) {
	    fontsel_SetSize(self->fontsel, figattr_GetFontSizeVal(self->menuatt));
	    fselchanged = TRUE;
	}
    }

    if (!figattr_IsActive(self->menuatt, figattr_FontStyle)) {
	if (fontsel_IsActive(self->fontsel, fontsel_Style)) {
	    fontsel_UnsetStyle(self->fontsel);
	    fselchanged = TRUE;
	}
    }
    else {
	if (!fontsel_IsActive(self->fontsel, fontsel_Style)
	    || (figattr_GetFontStyleVal(self->menuatt) != fontsel_GetStyle(self->fontsel))) {
	    fontsel_SetStyle(self->fontsel, figattr_GetFontStyleVal(self->menuatt));
	    fselchanged = TRUE;
	}
    }

    if (fselchanged) {
	fontsel_NotifyObservers(self->fontsel, fontsel_DATACHANGED);
    }
    /* ##new */
}

static void IncreaseTmpProc(self, num)
struct figtoolview *self;
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

static void CacheSelectProc(o, ref, vv, figt)
struct figobj *o;
long ref;
struct figview *vv;
struct figtoolview *figt;
{
    int ix;
    struct figure *fig = (struct figure *)figview_GetDataObject(vv);

    if (ref==vv->focusgroup || ref==figure_RootObjRef(fig))
	return;

    ix = figt->tmpnum;
    IncreaseTmpProc(figt, ix+1);
    figt->tmplist[ix] = ref;
    figt->tmpnum = ix+1;
}

static void Command_GroupSel(self, rock)
struct figtoolview *self;
char *rock;
{
    struct figview *figv = self->primaryview;
    struct figure *fig;
    int ix;
    long foc, grpr;
    struct figogrp *grp;

    if (!figv) return;
    fig = (struct figure *)figview_GetDataObject(figv);
    if (!fig) return;

    foc = figview_GetFocusRef(figv);

    self->tmpnum = 0;
    figview_EnumerateSelection(figv, CacheSelectProc, self);

    if (self->tmpnum==0) {
	message_DisplayString(figv, 10, "No objects selected.");
	return;
    }

    grp = figogrp_New();
    figattr_CopyData(figogrp_GetVAttributes(grp), self->menuatt, figattr_MaskAll);
    grpr = figure_InsertObject(fig, grp, foc, -1);

    for (ix=0; ix<self->tmpnum; ix++)
	figure_UnlinkObjectByRef(fig, self->tmplist[ix]);
    for (ix=0; ix<self->tmpnum; ix++)
	figure_LinkObjectByRef(fig, self->tmplist[ix], grpr, -1);

    figview_FlushDataChanges(figv);
    figview_ClearSelection(figv);
    figview_SelectByRef(figv, grpr); 
    AdjustToSelection(self);
    figure_NotifyObservers(fig, figure_DATACHANGED);
}

static boolean CacheContentsProc(o, ref, fig, figt)
struct figobj *o;
long ref;
struct figure *fig;
struct figtoolview *figt;
{
    int ix;

    ix = figt->tmpnum;
    IncreaseTmpProc(figt, ix+1);
    figt->tmplist[ix] = ref;
    figt->tmpnum = ix+1;
    return FALSE;
}

static void Command_UngroupSel(self, rock)
struct figtoolview *self;
char *rock;
{
    struct figview *figv = self->primaryview;
    struct figure *fig;
    int ix;
    long grpr, foc;
    struct figogrp *grp;

    if (!figv) return;
    fig = (struct figure *)figview_GetDataObject(figv);
    if (!fig) return;

    grpr = figview_GetOneSelected(figv);
    if (grpr == figure_NULLREF || !figobj_IsGroup(figure_FindObjectByRef(fig, grpr))) {
	message_DisplayString(figv, 10, "You must select exactly one group to disassemble.");
	return;
    }
    grp = (struct figogrp *)figure_FindObjectByRef(fig, grpr);
    foc = figogrp_GetParentRef(grp);

    self->tmpnum = 0;
    figure_EnumerateObjectGroup(fig, grpr, NULL, FALSE, CacheContentsProc, self);

    for (ix=0; ix<self->tmpnum; ix++)
	figure_UnlinkObjectByRef(fig, self->tmplist[ix]);
    for (ix=0; ix<self->tmpnum; ix++)
	figure_LinkObjectByRef(fig, self->tmplist[ix], foc, -1);
    figure_DeleteObject(fig, grp);

    figview_FlushDataChanges(figv);
    figview_ClearSelection(figv);
    for (ix=0; ix<self->tmpnum; ix++)
	figview_SelectByRef(figv, self->tmplist[ix]); 
    AdjustToSelection(self);
    figure_NotifyObservers(fig, figure_DATACHANGED);
}

static void Command_MoveToExtreme(self, infront)
struct figtoolview *self;
long infront;
{
    struct figview *figv = self->primaryview;
    struct figure *fig;
    int ix;
    long foc;

    if (!figv) return;
    fig = (struct figure *)figview_GetDataObject(figv);
    if (!fig) return;

    foc = figview_GetFocusRef(figv);

    self->tmpnum = 0;
    figview_EnumerateSelection(figv, CacheSelectProc, self);

    if (self->tmpnum==0) {
	message_DisplayString(figv, 10, "No objects selected.");
	return;
    }

    for (ix=0; ix<self->tmpnum; ix++)
	figure_UnlinkObjectByRef(fig, self->tmplist[ix]);
    if (infront) {
	for (ix=0; ix<self->tmpnum; ix++)
	    figure_LinkObjectByRef(fig, self->tmplist[ix], foc, -1);
    }
    else {
	for (ix=self->tmpnum-1; ix>=0; ix--)
	    figure_LinkObjectByRef(fig, self->tmplist[ix], foc, 0);
    }

    figure_NotifyObservers(fig, figure_DATACHANGED);
}

static void ToggleSmoothProc(self, rock)
struct figtoolview *self;
long rock;
{
    struct figview *figv = self->primaryview;
    struct figure *fig;
    int ix;
    long ref, parent, depth;
    long didsmooth = 0, didunsmooth = 0;
    struct figobj *o;
    struct figoplin *newo;
    char *cname;

    if (!figv) return;
    fig = (struct figure *)figview_GetDataObject(figv);
    if (!fig) return;

    if (figure_GetReadOnly(fig)) {
	message_DisplayString(self->primaryview, 10, "Document is read-only.");
	return;
    }

    self->tmpnum = 0;
    figview_EnumerateSelection(figv, CacheSelectProc, self);

    for (ix=0; ix<self->tmpnum; ix++) {
	ref = self->tmplist[ix];
	o = figure_FindObjectByRef(fig, ref);
	cname = class_GetTypeName(o);
	if (class_IsTypeByName(cname, "figoplin")) {
	    if (class_IsTypeByName(cname, "figospli")) {
		didunsmooth++;
		newo = (struct figoplin *)class_NewObject("figoplin");
	    }
	    else {
		didsmooth++;
		newo = (struct figoplin *)class_NewObject("figospli");
	    }
	    figoplin_CopyData(newo, o);
	    parent = figobj_GetParentRef(o);
	    depth = figure_FindDepthByRef(fig, ref);
	    figure_DeleteObject(fig, o);
	    self->tmplist[ix] = figure_InsertObject(fig, newo, parent, depth);
	}
    }

    if (!didunsmooth && !didsmooth) 
	message_DisplayString(figv, 10, "No polylines, polygons or splines are selected.");
    else {
	if (didunsmooth && didsmooth)
	    message_DisplayString(figv, 10, "Selected objects smoothed / unsmoothed.");
	else if (didunsmooth) {
	    if (didunsmooth>1)
		message_DisplayString(figv, 10, "Selected objects unsmoothed.");
	    else
		message_DisplayString(figv, 10, "Object unsmoothed.");
	}
	else if (didsmooth) {
	    if (didsmooth>1)
		message_DisplayString(figv, 10, "Selected objects smoothed.");
	    else
		message_DisplayString(figv, 10, "Object smoothed.");
	}
	figview_FlushDataChanges(figv);
	for (ix=0; ix<self->tmpnum; ix++) {
	    figview_SelectByRef(figv, self->tmplist[ix]);
	}
	figure_NotifyObservers(fig, figure_DATACHANGED);
    }
}

static void ToggleClosedProc(self, rock)
struct figtoolview *self;
long rock;
{
    struct figview *figv = self->primaryview;
    struct figure *fig;
    int ix;
    long ref, parent, depth;
    long didclose = 0, didopen = 0;
    struct figobj *o;
    struct figoplin *plo;
    char *cname;

    if (!figv) return;
    fig = (struct figure *)figview_GetDataObject(figv);
    if (!fig) return;

    if (figure_GetReadOnly(fig)) {
	message_DisplayString(self->primaryview, 10, "Document is read-only.");
	return;
    }

    self->tmpnum = 0;
    figview_EnumerateSelection(figv, CacheSelectProc, self);

    for (ix=0; ix<self->tmpnum; ix++) {
	ref = self->tmplist[ix];
	o = figure_FindObjectByRef(fig, ref);
	cname = class_GetTypeName(o);
	if (class_IsTypeByName(cname, "figoplin")) {
	    plo = (struct figoplin *)o;
	    if (figoplin_Closed(plo)) {
		didopen++;
		figoplin_Closed(plo) = FALSE;
	    }
	    else {
		didclose++;
		figoplin_Closed(plo) = TRUE;
	    }
	    figoplin_RecomputeBounds(plo);
	    figoplin_SetModified(plo);
	}
    }

    if (!didopen && !didclose) 
	message_DisplayString(figv, 10, "No polylines, polygons or splines are selected.");
    else {
	if (didopen && didclose)
	    message_DisplayString(figv, 10, "Selected objects closed / opened.");
	else if (didopen) {
	    if (didopen>1)
		message_DisplayString(figv, 10, "Selected objects opened.");
	    else
		message_DisplayString(figv, 10, "Object opened.");
	}
	else if (didclose) {
	    if (didclose>1)
		message_DisplayString(figv, 10, "Selected objects closed.");
	    else
		message_DisplayString(figv, 10, "Object closed.");
	}
	figure_NotifyObservers(fig, figure_DATACHANGED);
    }
}

static void Command_SetDoConstraint(self, rock)
struct figtoolview *self;
boolean rock;
{
    struct figview *vv = self->primaryview;
    struct figure *fig = (struct figure *)figview_GetDataObject(vv);
    long ref = figview_GetFocusRef(vv);

    struct figogrp *focus = (struct figogrp *)figure_FindObjectByRef(fig, ref);
    if (focus==NULL) { 
	message_DisplayString(self, 0, "No focus group to clear attachments for!");
	return;
    }

    if (rock) {
	if (!focus->doconstraints) {
	    figogrp_SetConstraintsActive(focus, TRUE);
	    message_DisplayString(vv, 0, "Resizing is now active for this group.");
	    if (ref == figure_RootObjRef(fig)) {
		vv->lastheight--; /* tell view that it should resize the root group, by convincing it that it has changed size. */
	    }
	}
	else
	    message_DisplayString(vv, 0, "Resizing is already active for this group.");
    }
    else {
	if (focus->doconstraints) {
	    figogrp_SetConstraintsActive(focus, FALSE);
	    message_DisplayString(vv, 0, "Resizing is now inactive for this group.");
	}
	else
	    message_DisplayString(vv, 0, "Resizing is already inactive for this group.");
    }
}

static void DefaultAnchorSplot(o, ref, self, rock)
struct figobj *o;
long ref;
struct figview *self;
long rock;
{
    long diff1, diff2;
    struct figogrp *vg = figobj_GetParent(o);
    struct rectangle *R = (&(vg)->handlebox);
    long *canon, href;

    figobj_ClearAttachments(o);

    for (canon = figobj_GetCanonicalHandles(o); (href = (*canon)) != figobj_NULLREF; canon++) {

	switch (figobj_GetHandleType(o, href)) {

	    case figobj_MiddleTop:
	    case figobj_MiddleBottom:
		diff1=figobj_GetHandleY(o, href)-R->top;
		diff2=figobj_GetHandleY(o, href)-(R->top+R->height);
		if (ABS(diff1)<ABS(diff2)) {
		    figobj_SetAttachmentPosY(o, href, 0);
		    figobj_SetAttachmentOffY(o, href, diff1);
		} 
		else {
		    figobj_SetAttachmentPosY(o, href, figogrp_Range);
		    figobj_SetAttachmentOffY(o, href, diff2);
		}
		figobj_SetAttachmentPosX(o, href, ((figobj_GetHandleX(o, href)-R->left)*figogrp_Range)/R->width);
		figobj_SetAttachmentOffX(o, href, 0);
		break;

	    case figobj_MiddleLeft:
	    case figobj_MiddleRight:
		diff1=figobj_GetHandleX(o, href)-R->left;
		diff2=figobj_GetHandleX(o, href)-(R->left+R->width);
		if (ABS(diff1)<ABS(diff2)) {
		    figobj_SetAttachmentPosX(o, href, 0);
		    figobj_SetAttachmentOffX(o, href, diff1);
		}
		else {
		    figobj_SetAttachmentPosX(o, href, figogrp_Range);
		    figobj_SetAttachmentOffX(o, href, diff2);
		}
		figobj_SetAttachmentPosY(o, href, ((figobj_GetHandleY(o, href)-R->top)*figogrp_Range)/R->height);
		figobj_SetAttachmentOffY(o, href, 0);
		break;

	    default:
		diff1=figobj_GetHandleX(o, href)-R->left;
		diff2=figobj_GetHandleX(o, href)-(R->left+R->width);
		if (ABS(diff1)<ABS(diff2)) {
		    figobj_SetAttachmentPosX(o, href, 0);
		    figobj_SetAttachmentOffX(o, href, diff1);
		}
		else {
		    figobj_SetAttachmentPosX(o, href, figogrp_Range);
		    figobj_SetAttachmentOffX(o, href, diff2);
		}
		diff1=figobj_GetHandleY(o, href)-R->top;
		diff2=figobj_GetHandleY(o, href)-(R->top+R->height);
		if (ABS(diff1)<ABS(diff2)) {
		    figobj_SetAttachmentPosY(o, href, 0);
		    figobj_SetAttachmentOffY(o, href, diff1);
		} 
		else {
		    figobj_SetAttachmentPosY(o, href, figogrp_Range);
		    figobj_SetAttachmentOffY(o, href, diff2);
		}
		break;

	}
	figobj_SetAttachmentActive(o, href, TRUE);
    }

    figobj_ComputeSelectedBounds(o);
    figobj_SetModified(o);
}

static void ProportAnchorSplot(o, ref, self, rock)
struct figobj *o;
long ref;
struct figview *self;
long rock;
{
    long diff1, diff2;
    struct figogrp *vg = figobj_GetParent(o);
    struct rectangle *R = (&(vg)->handlebox);
    long *canon, href;

    figobj_ClearAttachments(o);

    for (canon = figobj_GetCanonicalHandles(o); (href = (*canon)) != figobj_NULLREF; canon++) {

	figobj_SetAttachmentPosY(o, href, ((figobj_GetHandleY(o, href)-R->top)*figogrp_Range)/R->height);
	figobj_SetAttachmentOffY(o, href, 0);
	figobj_SetAttachmentPosX(o, href, ((figobj_GetHandleX(o, href)-R->left)*figogrp_Range)/R->width);
	figobj_SetAttachmentOffX(o, href, 0);
	figobj_SetAttachmentActive(o, href, TRUE);
    }

    figobj_ComputeSelectedBounds(o);
    figobj_SetModified(o);
}

static void ClearAnchorSplot(o, ref, self, rock)
struct figobj *o;
long ref;
struct figview *self;
long rock;
{
    figobj_ClearAttachments(o);
    figobj_ComputeSelectedBounds(o);
    figobj_SetModified(o);
}

static void Command_ClearAnchors(self, rock) 
struct figtoolview *self;
char *rock;
{
    struct figure *fig = (struct figure *)figview_GetDataObject(self->primaryview);
    int numsel = figview_GetNumSelected(self->primaryview);

    if (numsel==0) {
	message_DisplayString(self->primaryview, 0, "No objects are selected.");
	return;
    }

    figview_EnumerateSelection(self->primaryview, ClearAnchorSplot, 0);
    figure_SetModified(fig);
    figure_NotifyObservers(fig, figure_DATACHANGED);

    if (numsel==1)
	message_DisplayString(self->primaryview, 0, "Anchors cleared for selected object.");
    else
	message_DisplayString(self->primaryview, 0, "Anchors cleared for selected objects.");
}

static void Command_DefaultAnchors(self, rock)
struct figtoolview *self;
char *rock;
{
    struct figure *fig = (struct figure *)figview_GetDataObject(self->primaryview);
    int numsel = figview_GetNumSelected(self->primaryview);

    if (numsel==0) {
	message_DisplayString(self->primaryview, 0, "No objects are selected.");
	return;
    }

    figview_EnumerateSelection(self->primaryview, DefaultAnchorSplot, 0);
    figure_SetModified(fig);
    figure_NotifyObservers(fig, figure_DATACHANGED);

    if (numsel==1)
	message_DisplayString(self->primaryview, 0, "Standard anchors created for selected object.");
    else
	message_DisplayString(self->primaryview, 0, "Standard anchors created for selected objects.");
}

static void Command_ProportAnchors(self, rock)
struct figtoolview *self;
char *rock;
{
    struct figure *fig = (struct figure *)figview_GetDataObject(self->primaryview);
    int numsel = figview_GetNumSelected(self->primaryview);

    if (numsel==0) {
	message_DisplayString(self->primaryview, 0, "No objects are selected.");
	return;
    }

    figview_EnumerateSelection(self->primaryview, ProportAnchorSplot, 0);
    figure_SetModified(fig);
    figure_NotifyObservers(fig, figure_DATACHANGED);

    if (numsel==1)
	message_DisplayString(self->primaryview, 0, "Proportional anchors created for selected object.");
    else
	message_DisplayString(self->primaryview, 0, "Proportional anchors created for selected objects.");
}

static boolean SelAddProc(o, ref, fig, vv)
struct figobj *o;
long ref;
struct figure *fig;
struct figview *vv;
{
    figview_SelectByRef(vv, ref);
    return FALSE;
}

static boolean SelTogProc(o, ref, fig, vv)
struct figobj *o;
long ref;
struct figure *fig;
struct figview *vv;
{
    figview_ToggleSelectByRef(vv, ref);
    return FALSE;
}

static void MakeBoxListProc(o, ref, vv, rec)
struct figobj *o;
long ref;
struct figview *vv;
struct rectangle **rec;
{
    struct rectangle *src = figobj_GetBounds(o, vv);
    if (rectangle_Width(src)<=0 || rectangle_Height(src)<=0)
	return;

    rectangle_SetRectSize(*rec,
			   figview_ToPixX(vv, rectangle_Left(src)),
			   figview_ToPixY(vv, rectangle_Top(src)), 
			   figview_ToPixW(vv, rectangle_Width(src)), 
			   figview_ToPixH(vv, rectangle_Height(src)));

    (*rec)++;
}

static void MoveObjsProc(o, ref, vv, pt)
struct figobj *o;
long ref;
struct figview *vv;
struct point *pt;
{
    figobj_Reposition(o, point_X(pt), point_Y(pt));
    figobj_StabilizeAttachments(o, FALSE);
}

static void Toolsub_Reshape(self, action, x, y, oref, ptref)
struct figtoolview *self;
enum view_MouseAction action;
long x, y;
long oref, ptref;
{
    struct figobj *o = self->primaryview->objs[oref].o;
    struct figure *fig;

    switch (action) {
	case view_LeftDown:
	case view_RightDown:
	    self->rock = figobj_Reshape(o, action, self->primaryview, x, y, TRUE, ptref);
	    break;
	case view_LeftMovement:
	case view_RightMovement:
	    figtoolview_SnapToGrid(self, x, y);
	    if (self->rock)
		self->rock = figobj_Reshape(o, action, self->primaryview, x, y, TRUE, ptref);
	    break;
	case view_LeftUp:
	case view_RightUp:
	    figtoolview_SnapToGrid(self, x, y);
	    if (self->rock) {
		self->rock = figobj_Reshape(o, action, self->primaryview, x, y, TRUE, ptref);
		if (self->rock) {
		    figobj_StabilizeAttachments(o, FALSE);
		    fig = (struct figure *)figtoolview_GetDataObject(self);
		    figure_SetModified(fig);
		    figure_NotifyObservers(fig, figure_DATACHANGED);
		}
	    }
	    break;
    }
}

static void Toolsub_AddAnch(self, action, x, y, onhandle, oref, ptref)
struct figtoolview *self;
enum view_MouseAction action;
long x, y;
boolean onhandle;
long oref, ptref;
{
    struct figobj *o = self->primaryview->objs[oref].o;
    struct figure *fig;
    long atx, aty;
    struct rectangle *R;
    struct figogrp *vg;

    if (!onhandle || ptref==figobj_NULLREF) return;

    switch (action) {
	case view_LeftDown:
	case view_RightDown:
	    self->rockx = figview_ToPixX(self->primaryview, figobj_GetHandleX(o, ptref));
	    self->rocky = figview_ToPixY(self->primaryview, figobj_GetHandleY(o, ptref));
	    self->lastx = figview_ToPixX(self->primaryview, x);
	    self->lasty = figview_ToPixY(self->primaryview, y);
	    figview_SetTransferMode(self->primaryview, graphic_INVERT);
	    figview_MoveTo(self->primaryview, self->rockx, self->rocky);
	    figview_DrawLineTo(self->primaryview, self->lastx, self->lasty);
	    break;
	case view_LeftMovement:
	case view_RightMovement:
	    figview_SetTransferMode(self->primaryview, graphic_INVERT);
	    figview_MoveTo(self->primaryview, self->rockx, self->rocky);
	    figview_DrawLineTo(self->primaryview, self->lastx, self->lasty);
	    self->lastx = figview_ToPixX(self->primaryview, x);
	    self->lasty = figview_ToPixY(self->primaryview, y);
	    figview_MoveTo(self->primaryview, self->rockx, self->rocky);
	    figview_DrawLineTo(self->primaryview, self->lastx, self->lasty);
	    break;
	case view_LeftUp:
	case view_RightUp:
	    figview_SetTransferMode(self->primaryview, graphic_INVERT);
	    figview_MoveTo(self->primaryview, self->rockx, self->rocky);
	    figview_DrawLineTo(self->primaryview, self->lastx, self->lasty);

	    vg = figobj_GetParent(o);
	    R = (&(vg)->handlebox);
	    fig = (struct figure *)figtoolview_GetDataObject(self);

	    atx = figobj_GetHandleX(o, ptref);
	    aty = figobj_GetHandleY(o, ptref);
	    figobj_SetAttachmentPosX(o, ptref, ((x-R->left)*figogrp_Range)/R->width);
	    figobj_SetAttachmentPosY(o, ptref, ((y-R->top)*figogrp_Range)/R->height);
	    figobj_SetAttachmentOffX(o, ptref, atx-x);
	    figobj_SetAttachmentOffY(o, ptref, aty-y);
	    figobj_SetAttachmentActive(o, ptref, TRUE);

	    figobj_ComputeSelectedBounds(o);
	    figobj_SetModified(o);
	    figure_SetModified(fig);
	    figure_NotifyObservers(fig, figure_DATACHANGED);

	    break;
    }
}

static void Toolsub_DelAnch(self, action, x, y, onhandle, oref, ptref)
struct figtoolview *self;
enum view_MouseAction action;
long x, y;
boolean onhandle;
long oref, ptref;
{
    struct figobj *o = self->primaryview->objs[oref].o;
    struct figure *fig;
    long atx, aty;
    struct rectangle *R;
    struct figogrp *vg;

    if (!onhandle || ptref==figobj_NULLREF) return;

    switch (action) {
	case view_LeftDown:
	case view_RightDown:
	    if (figobj_IsAttachmentActive(o, ptref)) {
		fig = (struct figure *)figtoolview_GetDataObject(self);
		figobj_SetAttachmentActive(o, ptref, FALSE);
		figobj_ComputeSelectedBounds(o);
		figobj_SetModified(o);
		figure_SetModified(fig);
		figure_NotifyObservers(fig, figure_DATACHANGED);
	    }

	    break;
	case view_LeftMovement:
	case view_RightMovement:
	    break;
	case view_LeftUp:
	case view_RightUp:
	    break;
    }
}

static void Toolsub_Add(self, action, x, y, onhandle, oref, ptref)
struct figtoolview *self;
enum view_MouseAction action;
long x, y;
boolean onhandle;
long oref, ptref;
{
    struct figobj *o = self->primaryview->objs[oref].o;
    struct figure *fig;

    switch (action) {
	case view_LeftDown:
	case view_RightDown:
	    self->rock = figobj_AddParts(o, action, self->primaryview, x, y, onhandle, ptref);
	    break;
	case view_LeftMovement:
	case view_RightMovement:
	    figtoolview_SnapToGrid(self, x, y);
	    if (self->rock)
		self->rock = figobj_AddParts(o, action, self->primaryview, x, y, onhandle, ptref);
	    break;
	case view_LeftUp:
	case view_RightUp:
	    figtoolview_SnapToGrid(self, x, y);
	    if (self->rock) {
		self->rock = figobj_AddParts(o, action, self->primaryview, x, y, onhandle, ptref);
		if (self->rock) {
		    fig = (struct figure *)figtoolview_GetDataObject(self);
		    figure_SetModified(fig);
		    figure_NotifyObservers(fig, figure_DATACHANGED);
		}
	    }
	    break;
    }
}

static void Toolsub_Del(self, action, x, y, onhandle, oref, ptref)
struct figtoolview *self;
enum view_MouseAction action;
long x, y;
boolean onhandle;
long oref, ptref;
{
    struct figobj *o = self->primaryview->objs[oref].o;
    struct figure *fig;

    switch (action) {
	case view_LeftDown:
	case view_RightDown:
	    self->rock = figobj_DeleteParts(o, action, self->primaryview, x, y, onhandle, ptref);
	    break;
	case view_LeftMovement:
	case view_RightMovement:
	    figtoolview_SnapToGrid(self, x, y);
	    if (self->rock)
		self->rock = figobj_DeleteParts(o, action, self->primaryview, x, y, onhandle, ptref);
	    break;
	case view_LeftUp:
	case view_RightUp:
	    figtoolview_SnapToGrid(self, x, y);
	    if (self->rock) {
		self->rock = figobj_DeleteParts(o, action, self->primaryview, x, y, onhandle, ptref);
		if (self->rock) {
		    fig = (struct figure *)figtoolview_GetDataObject(self);
		    figure_SetModified(fig);
		    figure_NotifyObservers(fig, figure_DATACHANGED);
		}
	    }
	    break;
    }
}

/* at this point, we know objects are selected */
static void Toolsub_Drag(self, action, x, y, numclicks)
struct figtoolview *self;
enum view_MouseAction action;
long x, y, numclicks;
{
    long ix;
    struct rectangle *tmp;
    struct point pt;
    struct figure *fig;

    figtoolview_SnapToGrid(self, x, y);
    switch (action) {
	case view_LeftDown:
	case view_RightDown:
	    ix = figview_GetNumSelected(self->primaryview);
	    if (ix > self->rect_size) {
		while (ix > self->rect_size)
		    self->rect_size *= 2;
		self->rectlist = (struct rectangle *)realloc(self->rectlist, self->rect_size * sizeof(struct rectangle));
	    }
	    tmp = self->rectlist;
	    figview_EnumerateSelection(self->primaryview, MakeBoxListProc, &tmp);
	    self->rock = ix;
	    self->rockx = x;
	    self->rocky = y;
	    self->lastx = 0;
	    self->lasty = 0;
	    figview_SetTransferMode(self->primaryview, graphic_INVERT);
	    for (ix=0; ix<self->rock; ix++) {
		tmp = &(self->rectlist[ix]);
		figview_DrawRectSize(self->primaryview, self->lastx+tmp->left, self->lasty+tmp->top, tmp->width, tmp->height);
	    }
	    break;
	case view_LeftMovement:
	case view_RightMovement:
	    figview_SetTransferMode(self->primaryview, graphic_INVERT);
	    for (ix=0; ix<self->rock; ix++) {
		tmp = &(self->rectlist[ix]);
		figview_DrawRectSize(self->primaryview, self->lastx+tmp->left, self->lasty+tmp->top, tmp->width, tmp->height);
	    }
	    self->lastx = figview_ToPixW(self->primaryview, x - self->rockx);
	    self->lasty = figview_ToPixH(self->primaryview, y - self->rocky);
	    for (ix=0; ix<self->rock; ix++) {
		tmp = &(self->rectlist[ix]);
		figview_DrawRectSize(self->primaryview, self->lastx+tmp->left, self->lasty+tmp->top, tmp->width, tmp->height);
	    }
	    break;
	case view_LeftUp:
	case view_RightUp:
	    figview_SetTransferMode(self->primaryview, graphic_INVERT);
	    for (ix=0; ix<self->rock; ix++) {
		tmp = &(self->rectlist[ix]);
		figview_DrawRectSize(self->primaryview, self->lastx+tmp->left, self->lasty+tmp->top, tmp->width, tmp->height);
	    }
	    point_SetPt(&pt, x - self->rockx, y - self->rocky);
	    figview_EnumerateSelection(self->primaryview, MoveObjsProc, &pt);
	    fig = (struct figure *)figtoolview_GetDataObject(self);
	    figure_SetModified(fig);
	    figure_NotifyObservers(fig, figure_DATACHANGED);
	    break;
    }
}

static void Toolsub_Select(self, action, x, y, numclicks)
struct figtoolview *self;
enum view_MouseAction action;
long x, y, numclicks;
{
    long w, h;
    struct rectangle area;
    struct figure *fig = (struct figure *)figtoolview_GetDataObject(self);
    if (!fig) return;

    switch (action) {
	case view_LeftDown:
	case view_RightDown:
	    self->rockx = x;
	    self->rocky = y;
	    self->lastx = figview_ToPixX(self->primaryview, x);
	    self->lasty = figview_ToPixY(self->primaryview, y);
	    self->lastw = 0;
	    self->lasth = 0;
	    figview_SetTransferMode(self->primaryview, graphic_INVERT);
	    figview_DrawRectSize(self->primaryview, self->lastx, self->lasty, self->lastw, self->lasth);
	    break;
	case view_LeftMovement:
	case view_RightMovement:
	    figview_SetTransferMode(self->primaryview, graphic_INVERT);
	    figview_DrawRectSize(self->primaryview, self->lastx, self->lasty, self->lastw, self->lasth);
	    /* x, y, w, h in fig coords; self->last* in pix coords */
	    w = x - self->rockx;
	    if (w<0) {
		self->lastw = figview_ToPixW(self->primaryview, -w);
		self->lastx = figview_ToPixX(self->primaryview, x);
	    }
	    else {
		self->lastw = figview_ToPixW(self->primaryview, w);
		self->lastx = figview_ToPixX(self->primaryview, self->rockx);
	    }
	    h = y - self->rocky;
	    if (h<0) {
		self->lasth = figview_ToPixH(self->primaryview, -h);
		self->lasty = figview_ToPixY(self->primaryview, y);
	    }
	    else {
		self->lasth = figview_ToPixH(self->primaryview, h);
		self->lasty = figview_ToPixY(self->primaryview, self->rocky);
	    }
	    figview_DrawRectSize(self->primaryview, self->lastx, self->lasty, self->lastw, self->lasth);
	    break;
	case view_LeftUp:
	case view_RightUp:
	    figview_SetTransferMode(self->primaryview, graphic_INVERT);
	    figview_DrawRectSize(self->primaryview, self->lastx, self->lasty, self->lastw, self->lasth);
	    /* x, y, w, h in fig coords */
	    w = x - self->rockx;
	    if (w<0) 
		w = -w;
	    else 
		x = self->rockx;
	    h = y - self->rocky;
	    if (h<0) 
		h = -h;
	    else 
		y = self->rocky;
	    if (figview_ToPixW(self->primaryview, w) < figview_SelectDelta
		&& figview_ToPixH(self->primaryview, h) < figview_SelectDelta) {
		/* cursor didn't move far enough; we have a point click. */
		long ptref;
		long ref, tmpref;
		long delta = figview_ToFigW(self->primaryview, figtoolview_SelectClickDistance);
		ref = figure_FindRefByPos(fig, figview_GetFocusRef(self->primaryview), TRUE, figobj_HitBody, delta, x, y, &ptref);
		if (!self->selectdeep) {
		    while (ref != figure_NULLREF && (tmpref=figobj_GetParentRef(figure_FindObjectByRef(fig, ref))) != figview_GetFocusRef(self->primaryview))
			ref = tmpref;
		}
		if (ref != figure_NULLREF) {
		    if (action==view_LeftUp)
			figview_SelectByRef(self->primaryview, ref);
		    else
			figview_ToggleSelectByRef(self->primaryview, ref);
		    AdjustToSelection(self);
		    figview_WantUpdate(self->primaryview, self->primaryview);
		}
	    }
	    else {
		rectangle_SetRectSize(&area, x, y, w, h);
		if (self->selectdeep)
		    figure_EnumerateObjectTree(fig, figview_GetFocusRef(self->primaryview), &area, FALSE, (action==view_LeftUp ? SelAddProc : SelTogProc), self->primaryview);
		else
		    figure_EnumerateObjectGroup(fig, figview_GetFocusRef(self->primaryview), &area, FALSE, (action==view_LeftUp ? SelAddProc : SelTogProc), self->primaryview);
		AdjustToSelection(self);
		figview_WantUpdate(self->primaryview, self->primaryview);
	    }
	    break;
    }
}

struct FHOP_lump {
    enum figobj_HitVal result;
    long oref, ptref;
    long x, y;
    long delta;
};

static void FindHitObjProc(o, ref, vv, val)
struct figobj *o;
long ref;
struct figview *vv;
struct FHOP_lump *val;
{
    enum figobj_HitVal res;
    long tmp;

    res = figobj_HitMe(o, val->x, val->y, val->delta, &tmp);

    if ((int)(res) < (int)(val->result))
	return;

    val->result = res;
    val->oref = ref;
    val->ptref = tmp;
}

static void FindHitObjAnchProc(o, ref, vv, val) /* about the most unpleasant function name I've ever come up with. This counts a hit on an anchor as a hit on the handle. */
struct figobj *o;
long ref;
struct figview *vv;
struct FHOP_lump *val;
{
    enum figobj_HitVal res;
    long tmp;
    int ix;
    long dx, dy;

    res = figobj_HitMe(o, val->x, val->y, val->delta, &tmp);

    if (res==figobj_Miss || res==figobj_HitInside) {
	for (ix=0; ix<figobj_GetNumHandles(o); ix++) {
	    dx = (figobj_GetHandleX(o, ix)-figobj_GetAttachmentOffX(o, ix)) - val->x;
	    dy = (figobj_GetHandleY(o, ix)-figobj_GetAttachmentOffY(o, ix)) - val->y;
	    if (dx<=val->delta && dx>=(-val->delta) && dy<=val->delta && dy>=(-val->delta)) {
		tmp = ix;
		res = figobj_HitHandle;
		break;
	    }
	}
    }

    if ((int)(res) < (int)(val->result))
	return;

    val->result = res;
    val->oref = ref;
    val->ptref = tmp;
}

/* the grotesque semantics:
Right-down is always select mode.
Left-down in a selected object:
  reshape mode if it's on a selected object's handle;
  drag mode otherwise;
Left-down in no selected object is select mode.
If the action is a mouse movement or mouse-up, continue in the mode determined by the mouse-down.
*/
static struct view *Tool_Select(self, action, x, y, numclicks)
struct figtoolview *self;
enum view_MouseAction action;
long x, y, numclicks;
{
    struct FHOP_lump val;
    struct figure *fig;

    if (action==view_LeftDown || action==view_RightDown) {
	if (action==view_RightDown) {
	    self->submode = 0;
	    Toolsub_Select(self, action, x, y, numclicks);
	}
	else {
	    val.result = figobj_Miss;
	    val.x = x;
	    val.y = y;
	    val.delta = figview_ToFigW(self->primaryview, figtoolview_SelectClickDistance);
	    figview_EnumerateSelection(self->primaryview, FindHitObjProc, &val);	 
	    fig = (struct figure *)figtoolview_GetDataObject(self);
	    if (!fig)
		self->submode = 3;
	    else if (val.result==figobj_Miss) {
		figview_ClearSelection(self->primaryview);
		figview_WantUpdate(self->primaryview, self->primaryview);
		self->submode = 0;
		Toolsub_Select(self, action, x, y, numclicks);
	    }
	    else if (!figure_GetReadOnly(fig) && (val.result==figobj_HitInside || val.result==figobj_HitBody)) {
		self->submode = 1;
		Toolsub_Drag(self, action, x, y, numclicks);
	    }
	    else if (!figure_GetReadOnly(fig)) {
		self->submode = 2;
		self->lastw = val.oref;
		self->lasth = val.ptref;
		/* this assumes, unfairly, that no object is resized by a HitBody click. */
		Toolsub_Reshape(self, action, x, y, self->lastw, self->lasth);
	    }
	    else
		self->submode = 3;
	}
    }
    else {
	/* mouse movement or up */
	switch (self->submode) {
	    case 0:
		Toolsub_Select(self, action, x, y, numclicks);
		break;
	    case 1:
		Toolsub_Drag(self, action, x, y, numclicks);
		break;
	    case 2:
		Toolsub_Reshape(self, action, x, y, self->lastw, self->lasth);
		break;
	    default:
		break;
	}
    }
    return (struct view *)(self->primaryview);
}

static struct view *Tool_AddPoints(self, action, x, y, numclicks)
struct figtoolview *self;
enum view_MouseAction action;
long x, y, numclicks;
{
    struct FHOP_lump val;

    if (action==view_LeftDown || action==view_RightDown) {
	if (action==view_RightDown) {
	    self->submode = 0;
	    Toolsub_Select(self, action, x, y, numclicks);
	}
	else {
	    val.result = figobj_Miss;
	    val.x = x;
	    val.y = y;
	    val.delta = figview_ToFigW(self->primaryview, figtoolview_SelectClickDistance);
	    figview_EnumerateSelection(self->primaryview, FindHitObjProc, &val);	    
	    if (val.result==figobj_Miss || val.result==figobj_HitInside) {
		figview_ClearSelection(self->primaryview);
		figview_WantUpdate(self->primaryview, self->primaryview);
		self->submode = 0;
		Toolsub_Select(self, action, x, y, numclicks);
	    }
	    else {
		self->submode = 2;
		self->lastw = val.oref;
		self->lasth = val.ptref;
		self->lastb = (val.result == figobj_HitHandle);
		Toolsub_Add(self, action, x, y, self->lastb, self->lastw, self->lasth);
	    }
	}
    }
    else {
	/* mouse movement or up */
	switch (self->submode) {
	    case 0:
		Toolsub_Select(self, action, x, y, numclicks);
		break;
	    case 2:
		Toolsub_Add(self, action, x, y, self->lastb, self->lastw, self->lasth);
		break;
	    default:
		break;
	}
    }
    return (struct view *)(self->primaryview);
}

static struct view *Tool_DelPoints(self, action, x, y, numclicks)
struct figtoolview *self;
enum view_MouseAction action;
long x, y, numclicks;
{
    struct FHOP_lump val;

    if (action==view_LeftDown || action==view_RightDown) {
	if (action==view_RightDown) {
	    self->submode = 0;
	    Toolsub_Select(self, action, x, y, numclicks);
	}
	else {
	    val.result = figobj_Miss;
	    val.x = x;
	    val.y = y;
	    val.delta = figview_ToFigW(self->primaryview, figtoolview_SelectClickDistance);
	    figview_EnumerateSelection(self->primaryview, FindHitObjProc, &val);	    
	    if (val.result==figobj_Miss || val.result==figobj_HitInside) {
		figview_ClearSelection(self->primaryview);
		figview_WantUpdate(self->primaryview, self->primaryview);
		self->submode = 0;
		Toolsub_Select(self, action, x, y, numclicks);
	    }
	    else {
		self->submode = 2;
		self->lastw = val.oref;
		self->lasth = val.ptref;
		self->lastb = (val.result == figobj_HitHandle);
		Toolsub_Del(self, action, x, y, self->lastb, self->lastw, self->lasth);
	    }
	}
    }
    else {
	/* mouse movement or up */
	switch (self->submode) {
	    case 0:
		Toolsub_Select(self, action, x, y, numclicks);
		break;
	    case 2:
		Toolsub_Del(self, action, x, y, self->lastb, self->lastw, self->lasth);
		break;
	    default:
		break;
	}
    }
    return (struct view *)(self->primaryview);
}

static struct view *Tool_AddAnchor(self, action, x, y, numclicks)
struct figtoolview *self;
enum view_MouseAction action;
long x, y, numclicks;
{
    struct FHOP_lump val;

    if (action==view_LeftDown || action==view_RightDown) {
	if (action==view_RightDown) {
	    self->submode = 0;
	    Toolsub_Select(self, action, x, y, numclicks);
	}
	else {
	    val.result = figobj_Miss;
	    val.x = x;
	    val.y = y;
	    val.delta = figview_ToFigW(self->primaryview, figtoolview_SelectClickDistance);
	    figview_EnumerateSelection(self->primaryview, FindHitObjAnchProc, &val);	    
	    if (val.result==figobj_Miss || val.result==figobj_HitInside) {
		struct figure *fig;
		long focref;
		struct figobj *vg;
		enum figobj_HitVal res;

		fig = (struct figure *)figtoolview_GetDataObject(self);
		if ((focref = figview_GetFocusRef(self->primaryview)) != figure_NULLREF
		    && (vg = figure_FindObjectByRef(fig, focref))
		    && (res = figobj_HitMe(vg, val.x, val.y, val.delta, &val.ptref) == figobj_HitHandle)) {
		    if (focref == figure_RootObjRef(fig)) {
			message_DisplayString(self->primaryview, 10, "Resize the window to resize the root group.");
			self->submode = 3;
		    }
		    else {
			self->submode = 1;
			self->lastw = focref;
			self->lasth = val.ptref;
			Toolsub_Reshape(self, action, x, y, self->lastw, self->lasth);
		    }
		}
		else {
		    figview_ClearSelection(self->primaryview);
		    figview_WantUpdate(self->primaryview, self->primaryview);
		    self->submode = 0;
		    Toolsub_Select(self, action, x, y, numclicks);
		}
	    }
	    else {
		self->submode = 2;
		self->lastw = val.oref;
		self->lasth = val.ptref;
		self->lastb = (val.result == figobj_HitHandle);
		Toolsub_AddAnch(self, action, x, y, self->lastb, self->lastw, self->lasth);
	    }
	}
    }
    else {
	/* mouse movement or up */
	switch (self->submode) {
	    case 0:
		Toolsub_Select(self, action, x, y, numclicks);
		break;
	    case 1:
		Toolsub_Reshape(self, action, x, y, self->lastw, self->lasth);
		break;
	    case 2:
		Toolsub_AddAnch(self, action, x, y, self->lastb, self->lastw, self->lasth);
		break;
	    default:
		break;
	}
    }
    return (struct view *)(self->primaryview);
}

static struct view *Tool_DelAnchor(self, action, x, y, numclicks)
struct figtoolview *self;
enum view_MouseAction action;
long x, y, numclicks;
{
    struct FHOP_lump val;

    if (action==view_LeftDown || action==view_RightDown) {
	if (action==view_RightDown) {
	    self->submode = 0;
	    Toolsub_Select(self, action, x, y, numclicks);
	}
	else {
	    val.result = figobj_Miss;
	    val.x = x;
	    val.y = y;
	    val.delta = figview_ToFigW(self->primaryview, figtoolview_SelectClickDistance);
	    figview_EnumerateSelection(self->primaryview, FindHitObjAnchProc, &val);	    
	    if (val.result==figobj_Miss || val.result==figobj_HitInside) {
		struct figure *fig;
		long focref;
		struct figobj *vg;
		enum figobj_HitVal res;

		fig = (struct figure *)figtoolview_GetDataObject(self);
		if ((focref = figview_GetFocusRef(self->primaryview)) != figure_NULLREF
		    && (vg = figure_FindObjectByRef(fig, focref))
		    && (res = figobj_HitMe(vg, val.x, val.y, val.delta, &val.ptref) == figobj_HitHandle)) {
		    if (focref == figure_RootObjRef(fig)) {
			message_DisplayString(self->primaryview, 10, "Resize the window to resize the root group.");
			self->submode = 3;
		    }
		    else {
			self->submode = 1;
			self->lastw = focref;
			self->lasth = val.ptref;
			Toolsub_Reshape(self, action, x, y, self->lastw, self->lasth);
		    }
		}
		else {
		    figview_ClearSelection(self->primaryview);
		    figview_WantUpdate(self->primaryview, self->primaryview);
		    self->submode = 0;
		    Toolsub_Select(self, action, x, y, numclicks);
		}
	    }
	    else {
		self->submode = 2;
		self->lastw = val.oref;
		self->lasth = val.ptref;
		self->lastb = (val.result == figobj_HitHandle);
		Toolsub_DelAnch(self, action, x, y, self->lastb, self->lastw, self->lasth);
	    }
	}
    }
    else {
	/* mouse movement or up */
	switch (self->submode) {
	    case 0:
		Toolsub_Select(self, action, x, y, numclicks);
		break;
	    case 1:
		Toolsub_Reshape(self, action, x, y, self->lastw, self->lasth);
		break;
	    case 2:
		Toolsub_DelAnch(self, action, x, y, self->lastb, self->lastw, self->lasth);
		break;
	    default:
		break;
	}
    }
    return (struct view *)(self->primaryview);
}

static void Toolmod_Select(self, rock)
struct figtoolview *self;
long rock;
{
    if (self->expertmode) {
	if (self->selectdeep == FALSE) {
	    message_DisplayString(self, 10, "Deep selection mode on.");
	    self->selectdeep = TRUE;
	}
	else {
	    message_DisplayString(self, 10, "Deep selection mode off.");
	    self->selectdeep = FALSE;
	}
    }
}

static struct view *Tool_CreateProc(self, action, x, y, numclicks)
struct figtoolview *self;
enum view_MouseAction action;
long x, y, numclicks;
{
    enum figobj_Status result;
    struct figure *fig = (struct figure *)figtoolview_GetDataObject(self);

    figtoolview_SnapToGrid(self, x, y);

    if (!self->creating) {
	struct figobj *dummy;
	struct figobj *vob;
	if (action != view_LeftDown) {
	    if (self->selectonup && (action==view_LeftUp || action==view_RightUp))
		SetToolProc(self->tooltbl, self, self->toolacc[2]); /* ### */
	    self->selectonup = FALSE;
	    return (struct view *)(self->primaryview); 
	}

	self->selectonup = FALSE;

	dummy = self->dummylist[self->toolnum - NumFigTools(self)];
	vob = figobj_Instantiate(dummy, self, objectlayout[self->toolnum - NumFigTools(self)].rock);

	if (!vob) 
	    return (struct view *)(self->primaryview);

	self->creating = vob;
	figattr_CopyData(figobj_GetVAttributes(vob), self->menuatt, figattr_MaskAll/* & figobj_AttributesUsed(vob)*/); /* the commented-out bit would prevent new objects from taking on attributes they don't use. This makes for shorter datastreams, but makes the toolset menus jump to <default> all the time, which is confusing. So we leave it commented. */
	/* we don't call figobj_UpdateVAttributes(), 'cause that would do a little too much bouncing around. */
	figure_InsertObject(fig, vob, figview_GetFocusRef(self->primaryview), -1);
	result = figobj_Build(vob, self->primaryview, action, x, y, numclicks);
	figview_FlushDataChanges(self->primaryview);
	figview_ClearSelection(self->primaryview);
	figview_WantUpdate(self->primaryview, self->primaryview);
	RepostMenus(self);
    }
    else {
	result = figobj_Build(self->creating, self->primaryview, action, x, y, numclicks);
    }

    if (result == figobj_Done) {
	message_DisplayString(self->primaryview, 10, "Completed.");
	figview_Select(self->primaryview, self->creating);
	figure_NotifyObservers(fig, figure_DATACHANGED);
	self->creating = NULL;
	self->selectonup = !self->LockCreateMode;
	figview_SetBuildKeystate(self->primaryview, NULL);
	if (!self->LockCreateMode && (action==view_LeftUp || action==view_RightUp))
	    SetToolProc(self->tooltbl, self, self->toolacc[2]); /* ### */
	RepostMenus(self);
    }
    else if (result == figobj_Failed) {
	/*message_DisplayString(self->primaryview, 10, "Object aborted.");*/
	figview_FlushDataChanges(self->primaryview);
	figure_DeleteObject(fig, self->creating);
	figure_NotifyObservers(fig, figure_DATACHANGED); 
	figobj_Destroy(self->creating);
	self->creating = NULL;
	figview_SetBuildKeystate(self->primaryview, NULL);
	RepostMenus(self);
    }

    return (struct view *)(self->primaryview);
}

/* either abort or complete the object being built. */
void figtoolview__AbortObjectBuilding(self)
struct figtoolview *self;
{
    enum figobj_Status result;
    struct figure *fig = (struct figure *)figtoolview_GetDataObject(self);

    if (!self->creating) {
	message_DisplayString(self->primaryview, 10, "No object is being created.");
	return;
    }

    result = figobj_Build(self->creating, self->primaryview, view_LeftDown, 0, 0, 0);

    if (result == figobj_Done) {
	message_DisplayString(self->primaryview, 10, "Object completed.");
	figview_FlushDataChanges(self->primaryview);
	figview_Select(self->primaryview, self->creating);
	figure_NotifyObservers(fig, figure_DATACHANGED);
	self->creating = NULL;
	if (!self->LockCreateMode)
	    SetToolProc(self->tooltbl, self, self->toolacc[2]); /* ### */
    }
    else {
	/*message_DisplayString(self->primaryview, 10, "Object aborted.");*/
	figview_FlushDataChanges(self->primaryview);
	figure_DeleteObject(fig, self->creating);
	figure_NotifyObservers(fig, figure_DATACHANGED); 
	figobj_Destroy(self->creating);
	self->creating = NULL;
    }
    figview_SetBuildKeystate(self->primaryview, NULL);
    RepostMenus(self);
}

static void AbortObjectProc(self, rock)
struct figtoolview *self;
long rock;
{
    figtoolview_AbortObjectBuilding(self);
}

static char *CopyString(str)
char *str;
{
    char *tmp;

    if (str==NULL)
	return NULL;
    tmp = malloc(strlen(str)+1);
    if (!tmp)
	return NULL;
    strcpy(tmp, str);
    return tmp;
}

static void LowerString(str)
char *str;
{
    char *cx;

    for (cx=str; *cx; cx++) 
	if (isupper(*cx))
	    *cx = tolower(*cx);
}

/* trim whitespace off the front and back of a string. This modifies the buffer (trimming the back) and returns an updated pointer */
static char *WhiteKillString(buf)
char *buf;
{
    char *cx;
    for (cx = buf; *cx && !isgraph(*cx); cx++);
    buf = cx;
    for (cx = buf + (strlen(buf)-1); cx>=buf && !isgraph(*cx); cx--);
    cx++;
    *cx = '\0';
    return buf;
}

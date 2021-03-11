/* fontselv.c - font selection inset view */
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
char *fontselv_c_rcsid = "$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/figure/RCS/fontselv.c,v 1.2 1992/12/14 20:45:16 rr2b R6tape $";
#endif

#include <fontselv.eh>

#include <dataobj.ih>
#include <message.ih>
#include <proctbl.ih>
#include <strtbl.ih>
#include <strtblv.ih>
#include <lpair.ih>
#include <fontdesc.ih>

#include <fontsamp.ih>
#include <fontsel.ih>

#define DEFAULTDEFAULT "<default>"

struct stylelayout_t {
    char name[16]; /* name in the toolset window */
    long style;	    /* fontdesc_Whatever */
};

#define	FAMILIES_NUM_INIT (3)
#define fontselv_family_Other (-1)
#define fontselv_family_Zero (0)
static char familylayout[FAMILIES_NUM_INIT][16] = {
    "andy",
    "andysans", 
    "andytype"
};

#define	STYLE_NUM_INIT (2)
#define fontselv_style_Zero (0)
static struct stylelayout_t stylelayout[STYLE_NUM_INIT] = {
    {"bold",	fontdesc_Bold},
    {"italic",	fontdesc_Italic}
};

#define	SIZES_NUM_INIT (5)
#define fontselv_size_Other (0)
#define fontselv_size_Zero (1)

static char sizelayout[SIZES_NUM_INIT][16] = {
    "<other>",
    "8",
    "10", 
    "12", 
    "14"
};

static void InsertSize(), ShowExtraProc(), SetSizeProc(), SetFamilyProc(), SetStyleProc();
static char *CopyString();

boolean fontselview__InitializeClass(ClassID)
struct classhdr *ClassID;
{
    return TRUE;
}

boolean fontselview__InitializeObject(ClassID, self)
struct classhdr *ClassID;
struct fontselview *self;
{
    int ix;
    struct stringtbl *tl;
    struct strtblview *tlv;

    self->showdefault = FALSE;
    self->defaultstring = malloc(strlen(DEFAULTDEFAULT)+1);
    strcpy(self->defaultstring, DEFAULTDEFAULT);

    self->familynum = fontselv_family_Zero;
    self->sizenum = fontselv_size_Zero+2; /* 12 points */
    self->style_mask = fontdesc_Plain;

    self->sample = fontsample_New();
    self->lp1 = lpair_New();
    self->lp2 = lpair_New();

    if (!self->lp1 || !self->lp2 || !self->sample)
	return FALSE;

    /* family table */
    tl = stringtbl_New();
    tlv = strtblview_New();
    if (!tl || !tlv) return FALSE;
    stringtbl_Clear(tl);
    self->families_num = FAMILIES_NUM_INIT;
    self->familyextra = -1;
    self->familyacc = (short *)malloc(sizeof(short) * self->families_num);
    self->familylist = (char **)malloc(sizeof(char *) * self->families_num);
    for (ix=0; ix<self->families_num; ix++) {
	self->familyacc[ix] = stringtbl_AddString(tl, familylayout[ix]);
	if (self->familyacc[ix] == (-1))
	    return FALSE;
	if (ix>fontselv_family_Other)
	    self->familylist[ix] = CopyString(familylayout[ix]);
    }
    strtblview_SetItemHitProc(tlv, SetFamilyProc, self);
    stringtbl_ClearBits(tl);
    stringtbl_SetBitOfEntry(tl, self->familyacc[self->familynum], TRUE);
    strtblview_SetDataObject(tlv, tl);
    self->familytbl = tl;
    self->vfamilytbl = tlv;

    /* style table */
    tl = stringtbl_New();
    tlv = strtblview_New();
    if (!tl || !tlv) return FALSE;
    stringtbl_Clear(tl);
    self->styles_num = STYLE_NUM_INIT;
    self->styleextra = -1;
    self->styleacc = (short *)malloc(sizeof(short) * self->styles_num);
    self->stylelist = (long *)malloc(sizeof(long) * self->styles_num);
    for (ix=0; ix<self->styles_num; ix++) {
	self->styleacc[ix] = stringtbl_AddString(tl, stylelayout[ix].name);
	if (self->styleacc[ix] == (-1))
	    return FALSE;
	self->stylelist[ix] = stylelayout[ix].style;
    }
    strtblview_SetItemHitProc(tlv, SetStyleProc, self);
    stringtbl_ClearBits(tl);
    strtblview_SetDataObject(tlv, tl);
    self->styletbl = tl;
    self->vstyletbl = tlv;

    /* size table */
    tl = stringtbl_New();
    tlv = strtblview_New();
    if (!tl || !tlv) return FALSE;
    stringtbl_Clear(tl);
    self->sizes_num = SIZES_NUM_INIT;
    self->sizeextra = -1;
    self->sizeacc = (short *)malloc(sizeof(short) * self->sizes_num);
    self->sizelist = (short *)malloc(sizeof(short) * self->sizes_num);
    for (ix=0; ix<self->sizes_num; ix++) {
	self->sizeacc[ix] = stringtbl_AddString(tl, sizelayout[ix]);
	if (self->sizeacc[ix] == (-1))
	    return FALSE;
	if (ix>fontselv_size_Other)
	    self->sizelist[ix] = atoi(sizelayout[ix]);
    }
    strtblview_SetItemHitProc(tlv, SetSizeProc, self);
    stringtbl_ClearBits(tl);
    stringtbl_SetBitOfEntry(tl, self->sizeacc[self->sizenum], TRUE);
    strtblview_SetDataObject(tlv, tl);
    self->sizetbl = tl;
    self->vsizetbl = tlv;

    lpair_SetUp(self->lp1, self->vsizetbl, self->vfamilytbl, 50, lpair_PERCENTAGE, lpair_VERTICAL, TRUE);
    lpair_SetUp(self->lp2, self->vstyletbl, self->sample, 50, lpair_PERCENTAGE, lpair_VERTICAL, TRUE);
    fontselview_SetUp(self, self->lp1, self->lp2, 30, lpair_PERCENTAGE, lpair_HORIZONTAL, TRUE);

    return TRUE;
}

void fontselview__FinalizeObject(ClassID, self)
struct classhdr *ClassID;
struct fontselview *self;
{
    int ix;

    fontselview_SetNth(self, 0, NULL);
    fontselview_SetNth(self, 1, NULL);

    lpair_Destroy(self->lp1);
    lpair_Destroy(self->lp2);

    strtblview_SetDataObject(self->vsizetbl, NULL);
    strtblview_SetDataObject(self->vstyletbl, NULL);
    strtblview_SetDataObject(self->vfamilytbl, NULL);

    fontsample_Destroy(self->sample);
    strtblview_Destroy(self->vsizetbl);
    strtblview_Destroy(self->vstyletbl);
    strtblview_Destroy(self->vfamilytbl);

    stringtbl_Destroy(self->sizetbl);
    stringtbl_Destroy(self->styletbl);
    stringtbl_Destroy(self->familytbl);

    for (ix=0; ix<self->families_num; ix++) {
	if (ix>fontselv_family_Other)
	    free(self->familylist[ix]);
    }
    free(self->sizeacc);
    free(self->sizelist);
    free(self->styleacc);
    free(self->stylelist);
    free(self->familyacc);
    free(self->familylist);
}

void fontselview__SetDataObject(self, dobj)
struct fontselview *self;
struct dataobject *dobj;
{
    super_SetDataObject(self, dobj);
    fontsample_SetDataObject(self->sample, dobj);
}

void fontselview__ObservedChanged(self, dobj, status)
struct fontselview *self;
struct fontsel *dobj;
long status;
{
    long ix, vnum, accnum;
    char *cx;

    if (status == observable_OBJECTDESTROYED) {
    }
    else if (status == fontsel_DATACHANGED) {

	if (fontsel_IsActive(dobj, fontsel_Family)) {
	    cx = fontsel_GetFamily(dobj);
	    for (vnum=fontselv_family_Zero; vnum<self->families_num; vnum++)
		if (!strcmp(self->familylist[vnum], cx)) break;
	    if (vnum==self->families_num) {
		vnum = self->families_num - 1;
	    }
	    self->familynum = vnum;
	    accnum = self->familyacc[vnum];
	    if (!stringtbl_GetBitOfEntry(self->familytbl, accnum)) {
		stringtbl_ClearBits(self->familytbl);
		stringtbl_SetBitOfEntry(self->familytbl, accnum, TRUE);
	    }
	}
	else {
	    fontselview_ShowExtraOption(self);
	    self->familynum = (-1);
	    accnum = self->familyextra;
	    if (!stringtbl_GetBitOfEntry(self->familytbl, accnum)) {
		stringtbl_ClearBits(self->familytbl);
		stringtbl_SetBitOfEntry(self->familytbl, accnum, TRUE);
	    }
	}

	if (fontsel_IsActive(dobj, fontsel_Size)) {
	    ix = fontsel_GetSize(dobj);
	    for (vnum=fontselv_size_Zero; vnum<self->sizes_num; vnum++)
		if (self->sizelist[vnum] == ix) break;
	    if (vnum==self->sizes_num) {
		InsertSize(self, ix);
		vnum = self->sizes_num - 1;
	    }
	    self->sizenum = vnum;
	    accnum = self->sizeacc[vnum];
	    if (!stringtbl_GetBitOfEntry(self->sizetbl, accnum)) {
		stringtbl_ClearBits(self->sizetbl);
		stringtbl_SetBitOfEntry(self->sizetbl, accnum, TRUE);
	    }
	}
	else {
	    fontselview_ShowExtraOption(self);
	    self->sizenum = (-1);
	    accnum = self->sizeextra;
	    if (!stringtbl_GetBitOfEntry(self->sizetbl, accnum)) {
		stringtbl_ClearBits(self->sizetbl);
		stringtbl_SetBitOfEntry(self->sizetbl, accnum, TRUE);
	    }
	}

	if (fontsel_IsActive(dobj, fontsel_Style)) {
	    ix = fontsel_GetStyle(dobj);
	    stringtbl_ClearBits(self->styletbl);
	    for (vnum=fontselv_style_Zero; vnum<self->styles_num; vnum++) {
		if (self->stylelist[vnum] & ix) {
		    accnum = self->styleacc[vnum];
		    stringtbl_SetBitOfEntry(self->styletbl, accnum, TRUE);
		}
	    }
	    self->style_mask = ix;
	}
	else {
	    fontselview_ShowExtraOption(self);
	    accnum = self->styleextra;
	    if (!stringtbl_GetBitOfEntry(self->styletbl, accnum)) {
		stringtbl_ClearBits(self->styletbl);
		stringtbl_SetBitOfEntry(self->styletbl, accnum, TRUE);
	    }
	}

	fontselview_WantUpdate(self, self);
    }
}

static void InsertSize(self, val)
struct fontselview *self;
short val;
{
    struct stringtbl *st = self->sizetbl;   
    char name[16];
    int ix;

    ix = self->sizes_num;
    self->sizeacc = (short *)realloc(self->sizeacc, sizeof(short) * (1+self->sizes_num));
    self->sizelist = (short *)realloc(self->sizelist, sizeof(short) * (1+self->sizes_num));

    sprintf(name, "%d", val);
    
    if (self->sizeextra != (-1)) {
	stringtbl_RemoveEntry(st, self->sizeextra);
	self->sizeacc[ix] = stringtbl_AddString(st, name);
	self->sizeextra = stringtbl_AddString(st, self->defaultstring);
    }
    else {
	self->sizeacc[ix] = stringtbl_AddString(st, name);
    }

    if (self->sizeacc[ix] == (-1))
	return; 
    self->sizelist[ix] = val;

    self->sizes_num++;
}

static void SetSizeProc(st, self, accnum)
struct stringtbl *st;
struct fontselview *self;
short accnum;
{
    int sizenum;
    struct fontsel *fontsel = (struct fontsel *)fontselview_GetDataObject(self);

    if (accnum==self->sizeextra) {
	sizenum = (-1);
    }
    else {
	for (sizenum=0; sizenum<self->sizes_num; sizenum++) {
	    if (self->sizeacc[sizenum] == accnum) break;
	}
	if (sizenum==self->sizes_num) {
	    return;
	}
    }

    if (sizenum == fontselv_size_Other) {
	int ix;
	short val;
	char buffer[32];
	int res;

	res = message_AskForString (self, 40, "Enter a new font size:  ", NULL, buffer, 30); 
	if (res<0 || strlen(buffer)==0) {
	    message_DisplayString(self, 10, "Cancelled.");
	    return;
	}
	val = atoi(buffer);

	if (val<1) {
	    message_DisplayString(self, 10, "Value must be a positive integer.");
	    return;
	}
	if (val>144) {
	    message_DisplayString(self, 10, "That value is too large.");
	    return;
	}

	for (ix=fontselv_size_Zero; ix<self->sizes_num; ix++)
	    if (self->sizelist[ix] == val) {
		message_DisplayString(self, 10, "That value is already available.");
		sizenum = ix;
		break;
	    }
	if (sizenum == fontselv_size_Other) {
	    InsertSize(self, val);

	    for (sizenum=fontselv_size_Zero; sizenum<self->sizes_num; sizenum++) {
		if (self->sizelist[sizenum] == val) break;
	    }
	    if (sizenum==self->sizes_num) {
		return;
	    }
	}
	accnum = self->sizeacc[sizenum];
    }

    if (!stringtbl_GetBitOfEntry(st, accnum)) {
	stringtbl_ClearBits(st);
	stringtbl_SetBitOfEntry(st, accnum, TRUE);
    }

    if (self->sizenum != sizenum) {
	self->sizenum = sizenum;
	if (sizenum == (-1))
	    fontsel_UnsetSize(fontsel);
	else
	    fontsel_SetSize(fontsel, self->sizelist[sizenum]);
	fontsel_NotifyObservers(fontsel, fontsel_DATACHANGED);
    }
}

static void SetStyleProc(st, self, accnum)
struct stringtbl *st;
struct fontselview *self;
short accnum;
{
    int stylenum;
    struct fontsel *fontsel = (struct fontsel *)fontselview_GetDataObject(self);

    if (accnum==self->styleextra) {
	stylenum = (-1);
    }
    else {
	for (stylenum=0; stylenum<self->styles_num; stylenum++) {
	    if (self->styleacc[stylenum] == accnum) break;
	}
	if (stylenum==self->styles_num) {
	    return;
	}
    }

    if (stylenum == (-1)) {
	if (!stringtbl_GetBitOfEntry(st, accnum)) {
	    self->style_mask = fontsel_default_Style;
	    fontsel_SetStyle(fontsel, self->style_mask);
	    fontsel_UnsetStyle(fontsel);
	    stringtbl_ClearBits(st);
	    stringtbl_SetBitOfEntry(st, accnum, TRUE);
	}
	else {
	    self->style_mask = fontsel_default_Style;
	    fontsel_SetStyle(fontsel, self->style_mask);
	    stringtbl_ClearBits(st);
	}
    }
    else {
	if (!stringtbl_GetBitOfEntry(st, accnum)) {
	    stringtbl_SetBitOfEntry(st, accnum, TRUE);
	    self->style_mask |= self->stylelist[stylenum];
	}
	else {
	    stringtbl_SetBitOfEntry(st, accnum, FALSE);
	    self->style_mask &= ~(self->stylelist[stylenum]);
	}
	if (self->styleextra != (-1))
	    stringtbl_SetBitOfEntry(st, self->styleextra, FALSE);
	fontsel_SetStyle(fontsel, self->style_mask);
    }

    fontsel_NotifyObservers(fontsel, fontsel_DATACHANGED);
}

static void SetFamilyProc(st, self, accnum)
struct stringtbl *st;
struct fontselview *self;
short accnum;
{
    int familynum;
    struct fontsel *fontsel = (struct fontsel *)fontselview_GetDataObject(self);

    if (accnum==self->familyextra) {
	familynum = (-1);
    }
    else {
	for (familynum=0; familynum<self->families_num; familynum++) {
	    if (self->familyacc[familynum] == accnum) break;
	}
	if (familynum==self->families_num) {
	    return;
	}
    }

    if (!stringtbl_GetBitOfEntry(st, accnum)) {
	stringtbl_ClearBits(st);
	stringtbl_SetBitOfEntry(st, accnum, TRUE);
    }

    if (self->familynum != familynum) {
	self->familynum = familynum;
	if (familynum == (-1)) {
	    fontsel_UnsetFamily(fontsel);
	}
	else {
	    fontsel_SetFamily(fontsel, self->familylist[familynum]);
	}
	fontsel_NotifyObservers(fontsel, fontsel_DATACHANGED);
    }
}

void fontselview__SetExtraOptionString(self, val)
struct fontselview *self;
char *val;
{
    if (!val)
	return;

    if (self->defaultstring)
	free(self->defaultstring);
    self->defaultstring = CopyString(val);
}

void fontselview__ShowExtraOption(self)
struct fontselview *self;
{
    if (self->showdefault)
	return;

    self->showdefault = TRUE;

    self->sizeextra = stringtbl_AddString(self->sizetbl, self->defaultstring);
    self->styleextra = stringtbl_AddString(self->styletbl, self->defaultstring);
    self->familyextra = stringtbl_AddString(self->familytbl, self->defaultstring);
}

static void ShowExtraProc(self, rock)
struct fontselview *self;
long rock;
{
    fontselview_ShowExtraOption(self);
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


/* 
 *	Copyright IBM Corporation 1988,1991 - All Rights Reserved  
 *	Copyright Carnegie Mellon University 1992 - All Rights Reserved
 *	For full copyright information see:'andrew/config/COPYRITE'  
 */

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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/lookz/RCS/lookzv.c,v 2.30 1993/12/08 01:19:14 rr2b Exp $";
#endif

 

/* lookzv.c	

	The view module for the lookz dataobject

	When initiated as an inset in a document, the FullUpdate routine 
	attempts to find a stylesheet associated with the parent text.  
	Otherwise a stylesheet can be specified by a client calling 
	SetStyleSheet.  Either way, lookzview displays some of the 
	components of the style via stringtbls.  Clicks on the 
	tables cause the associated attributes to be changed in the stylesht.

Deferred:
	Want to avoid direct modification of the style in the surrounding text.
		Instead, make a copy of the style, modify the copy, and
		call style_Copy to replace the original style.
	Menu options: Add Font, Rename Style, Cancel
	Baseline separation and minimum and maximum shim size.
*/
/* sys/types.h in AIX PS2 defines "struct label", causing a type name clash.
   Avoid this by temporarily redefining "label" to be something else in the 
preprocessor. */
#define label gezornenplatz
#include <andrewos.h> /* strings.h */
#include <stdio.h>
#undef label
#include <util.h>
#include <ctype.h>
#include <class.h>
#include <message.ih>
#include <rect.h>
#include <environ.ih>
#include <envrment.ih>
#include <graphic.ih>
#include <dataobj.ih>
#include <view.ih>
#include <text.ih>
#include <lpair.ih>
#include <stylesht.ih>
#include <style.ih>
#include <tabs.ih>

#include <bpair.ih>
#include <strtbl.ih>
#include <strtblv.ih>
#include <lprruler.ih>
#include <lprrulv.ih>
#include <tabrulv.ih>
#include <label.ih>
#include <labelv.ih>

#include <fontdesc.ih>

#include <lookz.ih>
#include <lookzv.eh>

#include <menulist.ih>
#include <keymap.ih>
#include <keystate.ih>
#include <proctbl.ih>
#include <bind.ih>

#include <buffer.ih>
#include <frame.ih>
#include <im.ih>

#if 0
#define DEBUG(s) (printf s, fflush(stdout))
#define ENTER(r) DEBUG(("Enter %s(0x%lx)\n", "r", self))
#define LEAVE(r) DEBUG(("Leave %s(0x%lx)\n", "r", self))
#else
#define DEBUG(s)
#define ENTER(r)
#define LEAVE(r)
#endif

extern int ULstrcmp();	/* case insensitive compare from libutil */

#define NOMENUSTRING "<No Menu>"

static void UpdateDocument(), AddStyle(), DeleteStyle();

static struct bind_Description MenuOptions[] = {
        /* Update Document must be the first entry.  See InitializeClass. */
	{"lookzview-update-document", "\033U", 0, "Update Document", 0, 0,
		UpdateDocument, "Cause document to redisplay itself", NULL},
	{"lookzview-delete-style", "\033D", 0, "Delete Style", 0, 0,
		DeleteStyle, "Delete a style from the stylesheet", NULL},
	{"lookzview-add-style", "\033A", 0, "Add Style", 0, 0,
		AddStyle, "Add a new style to stylesheet", NULL},
	NULL
};

static struct menulist  *MenuList;
static struct keymap  *Keymap;

static struct fontdesc *StrtblFont;
static boolean AutoUpdate;

struct styleeditorlist {
    struct text *text;
    struct lookz *lookz;
};

static struct styleeditorlist *styleEditors = NULL;
static long numStyleEditors = 0;
static long maxStyleEditors = 0;

/* indices into arrays st[] and stv[] */
enum TableNum {
	TNmenucard, TNstylename, TNfont, TNsize, 
	TNmode, TNsubscr, TNenable, TNdisable, TNspacing, TNspread, TNcolor
};

/* set up initial strings for stringtbls.  
		Where a string in this table is supposed to be the 
		same as a string in one of the XxxxNames tables, 
		both must be spelled identically */
	static char *Imenucard[] = {"", NULL};
	static char *Istylename[] = {"", NULL};
	static char *Ifont[] = {"Andy", "AndySans", "AndyType", "<other>", 
				NULL};
	static char *Isize[] = {"-2", "+2", "7", "8", "9", "10", "12", "14",
				"16", "18", "24", "30", "36", "<other>", NULL};
	static char *Imode[] = {"Center", "Left flush", "Right flush", 
			"Justified", "Left-right", NULL};
	static char *Isubscr[] = {"super (-6)", 
			"sub (+2)", "<other>", NULL};
	static char *Ienable[] = {"Bold", "Italic", "Fixed width", "Underline",
					"Dotted Box", "Tab by spaces", 
					"<other>", NULL};
	static char *Idisable[] ={"Bold", "Italic", "Fixed width", "Underline",
					"Dotted Box", "Tab by spaces", 
					"<other>", NULL};
	static char *Ispacing[] = {"0", "2", "4", "6", "8", "10", "12", 
					"<other>", NULL};
	static char *Ispread[] = {"0", "2", "4", "6", "8", "10", "12", "14", 
					"16", "18", "20", "<other>", NULL};
	static char *Icolor[] = {"Black", "Red", "Green", "Blue", "Magenta", 
					"Cyan", "Yellow", "<other>", NULL};

static char **InitialStrings[]=
		{Imenucard, Istylename, Ifont, Isize, Imode, Isubscr, Ienable,
				Idisable, Ispacing, Ispread, Icolor, NULL};

struct strTbl {char *str; long val;};

/* map justification mode names to and from justification codes*/
static 
struct strTbl ModeNames[] = {
		{"Center", (long)style_Centered},
		{"Left flush", (long)style_LeftJustified},
		{"Right flush", (long)style_RightJustified},
		{"Justified", (long)style_LeftAndRightJustified},
		{"Left-right", (long)style_LeftThenRightJustified},
		{NULL, 0L},
};
/* map flag names to and from flag codes*/
static 
struct strTbl FlagNames[] = {
		{"Underline", style_Underline},
		{"Hidden", style_Hidden},
		{"Read Only", style_ReadOnly},
		{"Format Note", style_PassThru},
		{"Icon", style_Icon},
		{"Continue Indent", style_ContinueIndent},
		{"Hinge", style_Hinge},
		{"New Page", style_NewPage},
		{"Change Bar", style_ChangeBar},
		{"Over Bar", style_OverBar},
		{"No Wrap", style_NoWrap},
		{"No Fill", style_NoFill},
		{"Keep Prior NL", style_KeepPriorNL},
		{"Keep Next NL", style_KeepNextNL},
		{"Tab by spaces", style_TabsCharacters},
		{"Dotted Box", style_DottedBox},
		{"Strike Through", style_StrikeThrough},
		{NULL, 0L},
};
/* map face names to and from face codes*/
static 
struct strTbl FaceNames[] = {
		{"Bold", fontdesc_Bold},
		{"Italic", fontdesc_Italic},
		{"Shadow", fontdesc_Shadow},
		{"Fixed width", fontdesc_Fixed},
		{"Outline", fontdesc_Outline},
		{"Thin", fontdesc_Thin},
		{"Black", fontdesc_Black},
		{"Medium", fontdesc_Medium},
		{"Heavy", fontdesc_Heavy},
		{"Condense", fontdesc_Condense},
		{NULL, 0L},
};

/* map baseline codes to distance */
static 
struct strTbl SubScrNames[] = {
		{"super (-6)", -6},
		{"super (-2)", -2},
		{"up 15", -15},
		{"up 6", -6},
		{"up 4", -4},
		{"up 2", -2},
		{"sub (+2)", 2},
		{"down 2", 2},
		{"down 4", 4},
		{"down 6", 6},
		{NULL, 0L},
};

/* long CVDots(amt, unit)
	Convert the distance 'amt' measured in the given 'units' 
	to a distance measured in pixels (points)
	(should be a class procedure in style.c) 
*/

static
long CVDots(amt, unit)
	register long amt;
	enum style_Unit unit;
{
	switch (unit) {
            case style_RawDots:
                return amt;
            case style_Inches:
                return (amt * 72) >> 16;
            case style_CM:
                return (amt * 225 / 127) >> 12;
            case style_Points:
                return amt;
            default: 		/* style_Ems,  style_Lines */
                return 0;
        }
}

/* CVFractionalPoints(amt, unit)
	convert a value to a fixed-point floating value in points
              (long with binary point in the middle) 
*/

static long 
CVFractionalPoints(amt, unit)
	register long amt;
	enum style_Unit unit;
{
	switch (unit) {
            case style_RawDots:
                return amt << 16;
            case style_Inches:
                return amt * 72;
            case style_CM:
                return (amt<<4)*225/127;
            case style_Points:
                return amt << 16;
            default:
                return 0;		/* style_Ems,  style_Lines */
        }
}

static long
MapStringToVal(tbl, s)
	struct strTbl tbl[];
	char *s;
{
	long i;
	if(s==NULL) return -999L;
	for (i = 0; tbl[i].str; i++)
		if (FOLDEDEQ(tbl[i].str, s)) 
			return (tbl[i].val);
	return -999L;
}

static char *
MapValToString(tbl, val)
	struct strTbl tbl[];
	long val;
{
	long i;
	for (i = 0; tbl[i].str; i++)
		if (tbl[i].val == val)
			return (tbl[i].str);
	return NULL;
}

/* MapDeltaToString(dots, basis)
	convert dots value dd to "dd" if ConstantSize or "+dd" or "-dd" for 
PreviousSize 
		result is in a static buffer 
		(NOTE: used for both FontSize and ScriptMovement: 
		ASSUMES parallel definition of style_FontSize 
			and style_ScriptMovement) */
	static char *
MapDeltaToString(dots, basis)
	long dots;
	enum style_FontSize basis;
{
	static char buf[15];
	if (dots == 0) 
		*buf = '\0';
	else 
		sprintf(buf, "%s%ld", 
			(basis == style_ConstantFontSize) ? "" 
				:  ((dots >= 0) ? "+" : ""), 
			dots);
	return buf;
}

/* MapStringToDelta(str, dots, basis)
	convert string generated by MapDeltaToString back to SizeBasis and dots value 
*/
	static
MapStringToDelta(str, dots, basis)
	char *str;
	long *dots;
	enum style_FontSize *basis;
{
	*basis = (*str == '+' || *str == '-') ? style_PreviousFontSize 
				: style_ConstantFontSize;
	sscanf(str, "%ld", dots);
}

	static void 
ChangeStyle(self)
	register struct lookzview *self;
{
	self->curstyle->template = 0;
	stylesheet_NotifyObservers(self->curss, (long) self);	/*BOGUS XXX
			The protocol for stylesheet observed_changed status is
			to send the address of the view that caused the change;
			Thus its own ObservedChanged routine can reject it.  */
	if (AutoUpdate)
	    UpdateDocument(self);
}

/* RulerValueChanged(rv, self, icon, newvalue)
	update the style parameter given by setProc 
		relative to basis to the given value 
	set values in centimeters  {2.54/72 ==  127/(225*16)}
	use relative zero to mean no value */

	static void
RulerValueChanged(rv, self, icon, newvalue)
	struct lprrulerview *rv;
	register struct lookzview *self;
	enum iconcode icon;
	long newvalue;
{
	if (self->curstyle == NULL) return;
	switch (icon) {
		case leftIcon:
			if (newvalue <= lprrulerview_NoValue) newvalue = 0;
			style_SetNewLeftMargin(self->curstyle, style_LeftMargin,
					(newvalue>>4)*127/225, style_CM);
			break;
		case rightIcon:
			if (newvalue <= lprrulerview_NoValue) newvalue = 0;
			style_SetNewRightMargin(self->curstyle, style_RightMargin,
					(newvalue>>4)*127/225, style_CM);
			break;
		case paraIcon:
			if (newvalue <= lprrulerview_NoValue) newvalue = 0;
			style_SetNewIndentation(self->curstyle, style_LeftMargin,
					(newvalue>>4)*127/225, style_CM);
			break;
	}
	ChangeStyle(self);
}

	static void
TabsValueChanged(rv, self, pos, op, unit)
	struct lprrulerview *rv;
	register struct lookzview *self;
	long pos;
	enum style_TabAlignment op;
	enum style_Unit unit;
{
	if (self->curstyle == NULL) return;

	if (pos == -1) /* Cannot have negative tabstops - this is cancel code*/
		style_ClearTabChanges(self->curstyle);
	else
		style_AddTabChange(self->curstyle, op, pos, unit);
	ChangeStyle(self);
}

/* SetBitsForCode(code, names, tbl, parity)
	set bits in stringtbl -tbl- for the names given in -names- 
		for the bits in -code- 
	parity adjusts for the nonsense of the OutXxxx stuff 
*/
	static
SetBitsForCode(code, names, tbl, parity)
	unsigned long code;
	struct strTbl names[];
	struct stringtbl *tbl;
	long parity;
{
	long i;
	short accnum;
	for (i = 1;   code;   i <<= 1, code >>= 1) 
		if ((1 & code) ^ parity) {
			accnum = stringtbl_AddString(tbl, 
				(unsigned char *)MapValToString(names, i)); 
			stringtbl_SetBitOfEntry(tbl, accnum, 1);
		}
}

static void 
UnpackStyle(self)
	struct lookzview *self;
{
	register struct style *style = self->curstyle;
	register struct stringtbl **st = self->st;

	if (style == NULL) {
		return;
	}
	{	/* justification mode */
		stringtbl_ClearBits(st[(long)TNmode]);
		stringtbl_SetBit(st[(long)TNmode], 
			(unsigned char *)MapValToString(ModeNames,
					style_GetJustification(style)),
			1);
	}
	{	/* font family */
		char font[50];
		stringtbl_ClearBits(st[(long)TNfont]);
		style_GetFontFamily(style, font, sizeof (font));
		stringtbl_SetBitOfEntry(st[(long)TNfont],
				stringtbl_AddString(st[(long)TNfont], font), 1);
	}
	{	/* font size */
		enum style_FontSize basis;
		long points;
		stringtbl_ClearBits(st[(long)TNsize]);
		style_GetFontSize(style, &basis, &points);
		stringtbl_SetBitOfEntry(st[(long)TNsize],
			stringtbl_AddString(st[(long)TNsize], 
				(unsigned char *)MapDeltaToString(points, basis)), 
			1);
	}
	{	/* font script movement  (baseline adjustment) */
		enum style_ScriptMovement basis;
		long amt;
		enum style_Unit unit;
		char *str;
		stringtbl_ClearBits(st[(long)TNsubscr]);
		style_GetFontScript(style, &basis, &amt, &unit);
		str = MapValToString(SubScrNames, CVDots(amt, unit));
		stringtbl_SetBitOfEntry(st[(long)TNsubscr],
			stringtbl_AddString(st[(long)TNsubscr], 
				(unsigned char *)((str) ? str : 
				MapDeltaToString(CVDots(amt, unit), basis))), 
			1);
	}

	stringtbl_ClearBits(st[(long)TNenable]);
	stringtbl_ClearBits(st[(long)TNdisable]);
	stringtbl_RemoveString(st[(long)TNenable], "<other>");
	stringtbl_RemoveString(st[(long)TNdisable], "<other>");
	{	/* font face codes */
		SetBitsForCode(style_GetAddedFontFaces(style), 
				FaceNames, st[(long)TNenable], 0);
		SetBitsForCode(style_GetRemovedFontFaces(style), 
				FaceNames, st[(long)TNdisable], 1);
	}
	{	/* flags */
			/* KLUDGE:  missing macro in style.ch 
				access fields of style directly */
		SetBitsForCode(style->AddMiscFlags, 
				FlagNames, st[(long)TNenable], 0);
		SetBitsForCode(style->OutMiscFlags, 
				FlagNames, st[(long)TNdisable], 1);
	}
	stringtbl_AddString(st[(long)TNenable], "<other>");
	stringtbl_AddString(st[(long)TNdisable], "<other>");

	{	/* leftmargin, rightmargin, paraindent */
		long leftmargin, rightmargin, paraindent;
		enum style_MarginValue Basis;
		long Operand;
		enum style_Unit Unit;
		leftmargin = rightmargin = paraindent 
				= lprrulerview_NoValue;	/* initially no icons */
		style_GetNewLeftMargin(style, &Basis, &Operand, &Unit);
		if (Basis == style_LeftMargin && Operand != 0)
			leftmargin = CVFractionalPoints(Operand, Unit);
		style_GetNewRightMargin(style, &Basis, &Operand, &Unit);
		if (Basis == style_RightMargin && Operand != 0)
			rightmargin = CVFractionalPoints(Operand, Unit);
		style_GetNewIndentation(style, &Basis, &Operand, &Unit);
		if (Basis == style_LeftMargin && Operand != 0)
			paraindent = CVFractionalPoints(Operand, Unit);
		lprrulerview_SetValues(self->rulerview, leftmargin, rightmargin, 
				paraindent);
	}
	{ /* Interline/Interparagraph Spacing */
		enum style_SpacingValue Basis;
		long Operand;
		enum style_Unit Unit;
		unsigned char value[100];

		stringtbl_ClearBits(st[(long)TNspacing]);
		style_GetNewInterlineSpacing(style, &Basis, &Operand, &Unit);
		if (Basis == style_ConstantSpacing && Unit == style_Points) {
			sprintf(value, "%d", Operand);
			stringtbl_SetBitOfEntry(st[(long)TNspacing],
					stringtbl_AddString(st[(long)TNspacing], 
					value), 1); 
		}
		stringtbl_ClearBits(st[(long)TNspread]);
		style_GetNewInterparagraphSpacing(style, &Basis, &Operand, &Unit);
		if (Basis == style_ConstantSpacing && Unit == style_Points) {
			sprintf(value, "%d", Operand);
			stringtbl_SetBitOfEntry(st[(long)TNspread],
					stringtbl_AddString(st[(long)TNspread], 
					value), 1); 
		}
	}
	{ /* Color */
		char *color;
		stringtbl_ClearBits(st[(long) TNcolor]);
		color = style_GetAttribute(self->curstyle,"color");
		if(color != NULL) 
			stringtbl_SetBitOfEntry(st[(long)TNcolor], 
				stringtbl_AddString(st[(long)TNcolor], color), 1);
	}
	{ /* Tabs */
		long numTabChanges;
		struct tabentry **TabChangeArray;
		long i;
		struct tabs *tabs;
		tabs = tabs_Create();
		style_GetTabChangeList(style, &numTabChanges, &TabChangeArray);
		for (i = 0; i < numTabChanges; i++)
			tabs = tabs_ApplyChange(tabs, TabChangeArray[i]);
		if (TabChangeArray)
			free(TabChangeArray);
		tabrulerview_SetValues(self->tabrulerv, tabs);
	}
	self->NeedsUnpack = FALSE;
}

static char *
InitialWord(s)
	char *s;
{
	static char buf[50];
	register char *bx, *sx;
	long i;
	if(s==NULL) {
	    buf[0]='\0';
	    return buf;
	}
	for (bx=buf, sx=s, i=48;   i && *sx && *sx != '~' && *sx != ',';   i--)
		*bx++ = *sx++;
	*bx++ = '\0';
	return buf;
}

/* clean(s) 
	removes spaces and non-alphabetics.  It changes uppercase to lower 
	result must be less than 50 bytes 
	returns a pointer to an internal buffer
*/
	static char *
clean(s)
	char *s;
{
	static char buf[50];
	register char *bx, *sx;
	long i;
	if(s==NULL) {
	    buf[0]='\0';
	    return buf;
	}
	for (bx=buf, sx=s, i=48;   i && *sx;   i--) {
		register char c = *sx++;
		if ( ! isalnum(c)) {}
		else if (isupper(c)) 
			*bx++ = tolower(c);
		else
			*bx++ = c;
	}
	*bx++ = '\0';
	return buf;
}

/* SetStyleDisplay(self, st)
	initiates editing the style 'st'
*/
	static void
SetStyleDisplay(self, st)
	register struct lookzview *self;
	register struct style *st;
{
	self->curstyle = st;
	if (st) 
		UnpackStyle(self);
}

/* ClearStyleDisplay(self)
	removes highlights from all attributes in the tableux
*/

	static void
ClearStyleDisplay(self)
	register struct lookzview *self;
{
	long i;
	for (i=(long)TNfont; i<=(long)TNcolor; i++)
		stringtbl_ClearBits(self->st[i]);

	lprrulerview_SetValues(self->rulerview, lprrulerview_NoValue,
			 lprrulerview_NoValue, lprrulerview_NoValue);
	tabrulerview_SetValues(self->tabrulerv, NULL);
}

/* CloseStyleSheet(self)
	terminates editing the current style
*/
	static void
CloseStyleSheet(self)
	register struct lookzview *self;
{
	/* The document is updated for all changes, so there is no need
	to do anything about the style currently displayed */
	
	if ( ! self->foundstylesheet) return;
	ClearStyleDisplay(self);
	stringtbl_Clear(self->st[(long)TNmenucard]);
	stringtbl_Clear(self->st[(long)TNstylename]);
	self->foundstylesheet = FALSE;
	stylesheet_RemoveObserver(self->curss, self);
	self->curss = NULL;
	self->curstyle = NULL;
}

/* called by EnumerateStyles in OpenStyleSheet */
	static boolean
AddCardName(s, st)
	struct style *s;
	register struct stringtbl *st;
{
	char *mnnm = style_GetMenuName(s);
	if (mnnm != NULL)    stringtbl_AddString(st, (unsigned char 
*)InitialWord(mnnm));
	return FALSE;
}

	static
OpenStyleSheet(self, ss)
	register struct lookzview *self;
	register struct stylesheet *ss;
{
	self->curss = ss;
	if (ss != NULL) {
		stylesheet_AddObserver(ss, self);
		/* set up the cardmenu and stylename data objects */
		stylesheet_EnumerateStyles(self->curss, AddCardName,
					   (long) self->st[(long)TNmenucard]);
		stringtbl_AddString(self->st[(long)TNmenucard], NOMENUSTRING);
	}
	self->foundstylesheet = TRUE;
}


static char *TempStyleName;  /* holds the stylename during 
		EnumerateStyles (...FindAndUnpack...) */
/* FindAndUnPack is called by Enumerate styles in HitStyleName
		it finds the style named by lv->curcard and TempStyleName
		then it unpacks that style */
	static boolean
FindAndUnpack(s, lv)
	register struct style *s;
	register struct lookzview *lv;
{
	register char *mnnm = style_GetMenuName(s);
	if (mnnm != NULL && ULstrcmp(lv->curcard, InitialWord(mnnm)) == 0) {
		register char *ThisStylename = index(mnnm, ',');
		if (ThisStylename
				&& ULstrcmp(InitialWord(ThisStylename+1), 
							TempStyleName) == 0) {
			SetStyleDisplay(lv, s);
			return TRUE;  /* finished */
		}
	}
	return FALSE;
}

	static boolean
FindAndUnpackNoMenus(s, lv)
	register struct style *s;
	register struct lookzview *lv;
{
	register char *mnnm = style_GetMenuName(s);
	if (mnnm == NULL) {
		register char *ThisStylename = style_GetName(s);
		if (ThisStylename
				&& ULstrcmp(InitialWord(ThisStylename), 
							TempStyleName) == 0) {
			SetStyleDisplay(lv, s);
			return TRUE;  /* finished */
		}
	}
	return FALSE;
}

/* HitStyleName is called as an ItemHitProc for the stylename stringtbl */
	static
HitStyleName(st1, lv, accnum)
	register struct stringtbl *st1;
	register struct lookzview *lv;
	long accnum;
{
	strtblview_OneOnly(st1, lv, accnum);
	ClearStyleDisplay(lv);
	TempStyleName = (char *) 
			stringtbl_GetStringOfEntry(lv->st[(long)TNstylename],
			accnum);	/* side arg to FindAndUnpack KLUDGE */
	stylesheet_EnumerateStyles(lv->curss, FindAndUnpack, (long) lv);
}

	static
HitStyleNameNoMenus(st1, lv, accnum)
	register struct stringtbl *st1;
	register struct lookzview *lv;
	long accnum;
{
	strtblview_OneOnly(st1, lv, accnum);
	ClearStyleDisplay(lv);
	TempStyleName = (char *) 
			stringtbl_GetStringOfEntry(lv->st[(long)TNstylename],
			accnum);	/* side arg to FindAndUnpack KLUDGE */
	stylesheet_EnumerateStyles(lv->curss, FindAndUnpackNoMenus, (long) lv);
}

/* called by EnumerateStyles in HitMenuCard */
	static boolean
AddStyleName(s, lv)
	register struct style *s;
	register struct lookzview *lv;
{
	register char *mnnm = style_GetMenuName(s), *ThisStylename;
	if (mnnm != NULL && ULstrcmp(lv->curcard, InitialWord(mnnm)) == 0) {
		ThisStylename = index(mnnm, ',');
		if (ThisStylename) 
			stringtbl_AddString(lv->st[(long)TNstylename], 
				(unsigned char *)InitialWord(ThisStylename+1));
	}
	return FALSE;
}

	static boolean
AddStyleNameNoMenus(s, lv)
	register struct style *s;
	register struct lookzview *lv;
{
	register char *mnnm = style_GetMenuName(s), *ThisStylename;
	if (mnnm == NULL) {
		ThisStylename = style_GetName(s);
		if (ThisStylename) {
			stringtbl_AddString(lv->st[(long)TNstylename], 
				(unsigned char *)InitialWord(ThisStylename));
		}
	}
	return FALSE;
}

/* called as an ItemHitProc for stringtbl[TNmenucard] 
		set up the stylename stringtbl */
	static
HitMenuCard(st0, lv, accnum)
	struct stringtbl *st0;
	register struct lookzview *lv;
	long accnum;
{
	strtblview_OneOnly(st0, lv, accnum);
	stringtbl_Clear(lv->st[(long)TNstylename]);
	ClearStyleDisplay(lv);
	lv->curcard = (char *)stringtbl_GetStringOfEntry(st0, accnum);
	lv->curstyle = NULL;
	if (ULstrcmp(lv->curcard, NOMENUSTRING) != 0) {
		stylesheet_EnumerateStyles(lv->curss, AddStyleName, (long) lv);
		strtblview_SetItemHitProc(lv->stv[(long)TNstylename], 
					HitStyleName, lv);
	}
	else {
		stylesheet_EnumerateStyles(lv->curss, AddStyleNameNoMenus, 
					(long) lv);
		strtblview_SetItemHitProc(lv->stv[(long)TNstylename], 
					HitStyleNameNoMenus, lv);
	}
}

static char newString[300];

	static char *
GetStringValue(st, self, accnum, prompt)
	struct stringtbl *st;
	struct lookzview *self;
	short accnum;
	char *prompt;
{
	char *str;
	str = (char *) stringtbl_GetStringOfEntry(st, accnum);
	if (str && ULstrcmp(str, "<other>") == 0) {
		long ans = message_AskForString(self, 0, prompt, NULL, 
				newString, sizeof(newString));
		stringtbl_SetBitOfEntry(st, accnum, 0);

		if (ans == -1) {
			UnpackStyle(self);
			return NULL;
		}
		str = newString;
		accnum = stringtbl_GetEntryOfString(st, str, 0);
		if (accnum < 0) {
			stringtbl_RemoveString(st, "<other>");
			accnum = stringtbl_AddString(st, str);
			stringtbl_AddString(st, "<other>");
		}
		stringtbl_SetBitOfEntry(st, accnum, 1);
	}
	return str;
}

/* called as an ItemHitProc for stringtbl[TNfont] 
		modify font  in the stylesheet */
	static
HitFont(st, self, accnum)
	struct stringtbl *st;
	register struct lookzview *self;
	long accnum;
{
	if (self->curstyle == NULL) return;
	strtblview_ZeroOrOne(st, self, accnum);
	if (stringtbl_GetBitOfEntry(st, accnum)) {
		char *str =  GetStringValue(st, self, accnum, "New font: ");
		if (str != NULL) 
			style_SetFontFamily(self->curstyle, str);
	}
	else {
		/* delete font reference from style.   
			KLUDGE KLUDGE: style_SetFontFamily 
			is BOGUS: we do it directly here */
		if (self->curstyle->FontFamily != NULL) {
			free(self->curstyle->FontFamily);
			self->curstyle->FontFamily = NULL;
		}
	}
	ChangeStyle(self);
}

/* WARNING:  KLUDGE KLUDGE
	style.c, style.ch, and default.tpl treat the value of FontSize as Points,
		but do not shift the value left 16 bits.
	the same modules treat the FontScriptMovement value as points, 
		but DO shift the values left 16 bits.
	The code below reflects this "convention".
*/

/* called as an ItemHitProc for stringtbl[TNsize] 
		modify size  in the stylesheet */
	static
HitSize(st, self, accnum)
	struct stringtbl *st;
	register struct lookzview *self;
	long accnum;
{
	enum style_FontSize basis;
	long points;
	if (self->curstyle == NULL) return;
	strtblview_ZeroOrOne(st, self, accnum);
	if (stringtbl_GetBitOfEntry(st, accnum)) {
		char *str =  GetStringValue(st, self, accnum, 
			"New font size: ");
		if (str != NULL) {
			MapStringToDelta(str, &points, &basis);
			style_SetFontSize(self->curstyle, basis, points);
		}
	}
	else {
		points = 0, basis = style_PreviousFontSize;
		style_SetFontSize(self->curstyle, basis, points);
	}
	ChangeStyle(self);
}

/* called as an ItemHitProc for stringtbl[TNmode] 
		modify JustificationMode  in the stylesheet */
	static
HitMode(st, self, accnum)
	struct stringtbl *st;
	register struct lookzview *self;
	long accnum;
{
	if (self->curstyle == NULL) return;
	strtblview_ZeroOrOne(st, self, accnum);
	if (stringtbl_GetBitOfEntry(st, accnum)) {
		/* (The braces around this clause are NECESSARY because
			style_SetJustification is surrounded by braces) */
		style_SetJustification(self->curstyle, 
			(enum style_Justification)MapStringToVal(
					ModeNames, 
					((char *)stringtbl_GetStringOfEntry(st,accnum))));
	}
	else 
		style_SetJustification(self->curstyle, style_PreviousJustification);
	ChangeStyle(self);
}

/* called as an ItemHitProc for stringtbl[TNsubscr] 
		modify subscription  in the stylesheet */
	static
HitSubscr(st, self, accnum)
	struct stringtbl *st;
	register struct lookzview *self;
	long accnum;
{
	enum style_ScriptMovement  basis = style_PreviousScriptMovement;
	long points = 0;
	if (self->curstyle == NULL) return;
	strtblview_ZeroOrOne(st, self, accnum);
	if (stringtbl_GetBitOfEntry(st, accnum)) {
		char *str =  GetStringValue(st, self, accnum, "New script position: ");

		if (str != NULL) {
			points = MapStringToVal(SubScrNames, str);
			if (points == -999L) 
				MapStringToDelta(str, &points, 
						(enum style_FontSize *)&basis);
			style_SetFontScript(self->curstyle, basis, points, 
						style_Points);
		}
	}
	else 
		style_SetFontScript(self->curstyle, basis, points, style_Points);
	ChangeStyle(self);
}

/* FinagleStyleBits is called by HitEnable and HitDisable to modify the
	XxxFontFaces or XxxMiscFlags fields of the style.
	KLUDGE KLUDGE  This routine does not use the
	access procedures defined in style.ch;  that module 
	DOES NOT DEFINE six of the eight functions that are required 
*/
	static
FinagleStyleBits(self, st, accnum, parity, faces, flags)
	struct lookzview *self;
	struct stringtbl *st;
	long accnum;
	long parity;
	unsigned long *faces, *flags;
{
	char *str;
	unsigned long bit;
	unsigned long *which;
	unsigned long val;

	str =  GetStringValue(st, self, accnum, "New attribute: ");
	if(str==NULL) return;
	bit = stringtbl_GetBit(st, str);
	val = MapStringToVal(FaceNames, str);
	if (val != -999L) 
		which = faces;
	else {
		val = MapStringToVal(FlagNames, str);
		if (val != -999L) 
			which = flags;
		else {
			/* unknown str value */
			stringtbl_RemoveString(st, str);
			return;	
		}
	}
	if (bit ^ parity)
		*which |= val;		/* OR in a bit */
	else
		*which &= ~val;	/* remove a bit */
}

/* called as an ItemHitProc for stringtbl[TNenable] 
		modify enable flags  in the stylesheet */
	static
HitEnable(st, self, accnum)
	struct stringtbl *st;
	register struct lookzview *self;
	long accnum;
{
	if (self->curstyle == NULL) return;
	strtblview_ZeroOrMany(st, self, accnum);
	FinagleStyleBits(self, st, accnum, 0,
			((unsigned long *)&self->curstyle->AddFontFaces), 
			((unsigned long *)&self->curstyle->AddMiscFlags));
	ChangeStyle(self);
}
/* called as an ItemHitProc for stringtbl[TNdisable] 
		modify disable flags  in the stylesheet */
	static
HitDisable(st, self, accnum)
	struct stringtbl *st;
	register struct lookzview *self;
	long accnum;
{
	if (self->curstyle == NULL) return;
	strtblview_ZeroOrMany(st, self, accnum);
	FinagleStyleBits(self, st, accnum, 1,
			((unsigned long *)&self->curstyle->OutFontFaces), 
			((unsigned long *)&self->curstyle->OutMiscFlags));
	ChangeStyle(self);
}

	static
HitSpacing(st, self, accnum)
	struct stringtbl *st;
	register struct lookzview *self;
	long accnum;
{
	enum style_SpacingValue basis;
	long points;
	if (self->curstyle == NULL) return;
	strtblview_ZeroOrOne(st, self, accnum);
	if (stringtbl_GetBitOfEntry(st, accnum))  {
		char *str =  GetStringValue(st, self, accnum, 
					"New interline spacing: ");
		if (str != NULL) {
			if (*str == '+' || *str == '-') 
				basis = style_InterlineSpacing;
			else 
				basis = style_ConstantSpacing;
			points = atoi(str);
			style_SetNewInterlineSpacing(self->curstyle, basis, 
					points, style_Points);
		}
	}
	else {
		points = 0;
		basis = style_ConstantSpacing;
		style_SetNewInterlineSpacing(self->curstyle, basis, 
				points, style_Points);
	}
	ChangeStyle(self);
}

	static
HitSpread(st, self, accnum)
	struct stringtbl *st;
	register struct lookzview *self;
	long accnum;
{
	enum style_SpacingValue basis;
	long points;
	if (self->curstyle == NULL) return;
	strtblview_ZeroOrOne(st, self, accnum);
	if (stringtbl_GetBitOfEntry(st, accnum))  {
		char *str =  GetStringValue(st, self, accnum, 
					"New interparagraph spacing: ");

		if (str != NULL) {
			if (*str == '+' || *str == '-') 
				basis = style_InterparagraphSpacing;
			else 
				basis = style_ConstantSpacing;
			points = atoi(str);
			style_SetNewInterparagraphSpacing(self->curstyle, 
					basis, points, style_Points);
		}
	}
	else {
		points = 0;
		basis = style_ConstantSpacing;
		style_SetNewInterparagraphSpacing(self->curstyle, basis, 
				points, style_Points);
	}
	ChangeStyle(self);
}

	static void 
HitColor(st, self, accnum)
	struct stringtbl *st;
	register struct lookzview *self;
	long accnum;
{
	if (self->curstyle == NULL) return;
	strtblview_ZeroOrOne(st, self, accnum);
	if (stringtbl_GetBitOfEntry(st, accnum))  {
		char *str =  GetStringValue(st, self, accnum, "New color: ");
		style_AddAttribute(self->curstyle, "color", str);
	}
	else 
		style_RemoveAttribute(self->curstyle,"color");
	ChangeStyle(self);
}

	static
HitShrink(shrink, action, self)
	struct labelview *shrink;
	register enum view_MouseAction  action;
	register struct lookzview *self;
{
	struct lookz *lookz;
	if (action != view_LeftDown && action != view_RightDown)
		return;
	lookz = (struct lookz *) lookzview_GetDataObject(self);
	if (! lookzview_GetVisibility(self) ||  lookz_GetCanClose(lookz)) {
		lookzview_SetVisibility(self, ! lookzview_GetVisibility(self));
		lookzview_WantNewSize(self, self);
		lookzview_WantInputFocus(self, self);
	}
}
	

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
 * 
 *	Now the routines for the view 
 *		(Above were the routines for the application
 *	
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

	void
ChooseShrinkIcon(self)
	register struct lookzview *self;
{
ENTER(ChooseShrinkIcon);
	label_SetText((struct label *)labelview_GetDataObject(self->shrinkicon), 
			(self->HasInputFocus) ? "-" : ",");
LEAVE(ChooseShrinkIcon);
}

/* LL - generate a horizontal or vertical pair of views 
	first arg is H or V to choose direction, second is percent of split,
	third and fourth args are the views 
	all boundaries are movable */

enum dir {H, V};

	static struct lpair *
LL(dir, pct, top, bot)
	enum dir dir;
	long pct;
	struct lpair *top, *bot;
{
	/* XXX (sigh) would really like lines to be movable, 
			but the wm has a terrible time leak in that area */
	register struct lpair *ret = lpair_New();
	if (dir == H) 
		lpair_HSplit(ret, top, bot, pct, FALSE);
	else
		lpair_VSplit(ret, top, bot, pct, FALSE);
	return ret;
}

/* LBL - generate an lpair with a fixed size top having a label */
	static struct lpair *
LBL (label, view)
	char *label;
	struct view *view;
{
	static struct fontdesc *asb8 = NULL;
	static struct im *icache = NULL;
	static long hcache = 13;

	struct im *im;	
	long w;
	register struct label *l = label_New();
	register struct labelview *lv = labelview_New();

	label_SetFont(l, "andysans", fontdesc_Bold, 8);
	label_SetText(l, label);
	label_SetFlags(l, label_VCENTERJUST);
	labelview_SetDataObject(lv, l);
	im = (struct im *)view_GetIM(view);
	if (im == NULL) 
		im = (struct im *)im_GetLastUsed();
	if (im == NULL)
		hcache = 13;
	else if (icache != im) {
		icache = im;
		if (asb8 == NULL)
			asb8 = fontdesc_Create("andysans", fontdesc_Bold, 8);
		fontdesc_StringBoundingBox(asb8, 
				(struct graphic *)im_GetDrawable(icache),
				"Xy", &w, &hcache);
		DEBUG(("LBL top height - im: 0x%lx    asb8: 0x%lx    h: %d\n", 
				im, asb8, hcache+5)); 
	}
	return  (struct lpair *)(bpair_VTFixed(bpair_New(), lv, view, 
					hcache+5, FALSE));
}

#define Text(v)	(struct text *) ((v)->header.view.dataobject)

	static 
EditStylesInWindow(textv)
	struct view *textv;
{
	register struct text *d;
	struct lookz *newobject;
	int i;
	int openSlot = -1;
	struct buffer *buffer = NULL;
DEBUG(("Enter EditStylesInWindow\n"));
	d = (struct text *) view_GetDataObject(textv);

	for (i = 0; i < numStyleEditors; i++) {
		if (styleEditors[i].text == d) {
			buffer = buffer_FindBufferByData(styleEditors[i].lookz);
			break;
		}
		else if (styleEditors[i].text == NULL) 
			openSlot = i;
	}

	if (buffer == NULL) {
		if ((newobject = (struct lookz *) class_NewObject("lookz")))  {
			char bufferName[50];

			lookz_SetID(newobject, lookz_UniqueID(newobject));
			lookz_SetTextObject(newobject, d);
			lookz_SetCanClose(newobject, FALSE);
			buffer_GetUniqueBufferName("Style-Editor", bufferName,
					sizeof(bufferName));
			buffer = buffer_Create(bufferName, NULL, "lookzview",
					newobject);
			buffer_SetDestroyData(buffer, TRUE);

			if (openSlot == -1) {
				if (numStyleEditors == maxStyleEditors) {
					if (maxStyleEditors == 0) {
						maxStyleEditors = 4;
						styleEditors = (struct styleeditorlist *)
							 malloc(sizeof(
									struct styleeditorlist) 
								* maxStyleEditors);
					}
					else {
						maxStyleEditors += 4;
						styleEditors = (struct styleeditorlist *)
							realloc(styleEditors, 
								sizeof(struct styleeditorlist) 
								* maxStyleEditors);
					}
				}
				openSlot = numStyleEditors++;
			}
			styleEditors[openSlot].text = d;
			styleEditors[openSlot].lookz = newobject;
		}
	}
	if (buffer != NULL) {
		/* get a view on the buffer */
		struct frame *frame = frame_GetFrameInWindowForBuffer(buffer);
		struct view *view;
		struct im *im;
		if (frame == NULL || (im = frame_GetIM(frame)) == NULL) {
			message_DisplayString(textv, 50, "Couldn't find window.");
			return;
		}
		view = frame_GetView(frame);
		view_WantInputFocus(view,view);
		/* pop to top window */
		im_ExposeWindow(im);
	}
DEBUG(("Leave EditStylesInWindow\n"));
}

	static  void
NewTextObject(self, text)
	struct lookzview *self;
	struct text *text;
{
	CloseStyleSheet(self);
	if (text != NULL) 
		lookzview_SetStyleSheet(self, text_GetStyleSheet(text));
	lookzview_WantUpdate(self, self);
}

/* we defer building the image until LinkTree so LBL can compute label size */
	static void
BuildImage(self)
	register struct lookzview  *self;
{
	struct lpair *R[7];
	struct labelview *emptylabelview;
	struct label *emptylabel;

	emptylabel = label_New();
	emptylabelview = labelview_New();
	label_SetFont(emptylabel, "icon", fontdesc_Plain, 12);
	label_SetText(emptylabel, "");
	labelview_SetDataObject(emptylabelview, emptylabel);

	/* build image 
		there are ten apparent rectangles arranged like this:
	
	icon		MENUCARD		MENUNAME

	FONT  	ENABLE	DISABLE	JUSTIFI-	BASE-
							CATION	LINE
	FONTSIZE		 		COLOR

	LINE SPACING			PARAGRAPH SPACING
		
	MARGINS AND PARAGRAPH INDENT RULER

	TABS RULER

	All but the icon rectangle have a small upper rectangle for the label.
	There is a small and empty label icon between the top row and the rest.
	There are seven groups of rectangles:
		R[0]	(icon, MENUCARD, MENUNAME)
		R[1]	(FONT, ENABLE, DISABLE)
		R[2]	(JUSTIFICATION, BASELINE)
		R[3]	(FONTSIZE, COLOR)
		R[4]	(MARGIN/PARA RULER)
		R[5]	(VERTICAL SPACING)
		R[6]	(TABRULER)
	We first create each group and then arrange them as a whole.
*/

	R[0] = LL(V, 4, 
		self->shrinkparent = LL(H, 94,
			(struct lpair *)self->shrinkicon,
			LL (H, 69,
				LBL("menu card", 
					(struct view *)self->stv[(long)TNmenucard]),
				LBL("name on menu card", 
					(struct view *)self->stv[(long)TNstylename])
			)
		),
		(struct lpair *)emptylabelview  /* dummy for thin line below menu stuff */
	);
	R[1] = LL(H, 70,
		LBL("font", (struct view *)self->stv[(long)TNfont]),
		LL(H, 50,
			LBL("enable", (struct view *)self->stv[(long)TNenable]),
			LBL("disable", (struct view *)self->stv[(long)TNdisable])
		)
	);
	R[2] = LL(H, 50,
		LBL("justify", (struct view *)self->stv[(long)TNmode]),
		LBL("baseline", (struct view *)self->stv[(long)TNsubscr])
	);
	R[3] = LL(H, 50,
		  LBL("font size", (struct view *)self->stv[(long)TNsize]),
		  LBL("color", (struct view *) self->stv[(long)TNcolor]));
	R[4] = LBL("relative margins and paragraph indent", 
				(struct view *)self->rulerview);
	R[5] = LL(H, 50,
		  LBL("line spacing", (struct view *) self->stv[(long) TNspacing]),
		  LBL("paragraph spacing", (struct view *) self->stv[(long) TNspread]));
	R[6] = LBL("tab stops - relative to left margin", 
				(struct view *)self->tabrulerv);

/* cut the following formulas and paste them in a 6x6 table inset to adjust the percentages
and see the resulting vertical sizes.  For the percentages 30, 39, 60, 50, 50, 
the resulting split of 700 vertical pixels is: 119.6 179.3 95.6 95.6 105.0 105.0

{F} =.30	{F1} 700	{F1} =[r,c-1]*(1-[r,c-2])	{F1} =[r,c-2]*[r,c-3]	{F1} =[r+2,c-2]	menus
{F} =.39	{F1} =[r-1,c+1]	{F1} =[r,c-1]*(1-[r,c-2])	{F1} =[r,c-2]*[r,c-3]	{F1} =[r+1,c-1]	font...
{F} =.6	{F1} =[r-1,c+1]	{F1} =[r,c-1]*(1-[r,c-2])	{F1} =[r,c-2]*[r,c-3]	{F1} =[r+1,c-2]	fontsize...
{F} =.5	{F1} =[r-2,c+2]	{F1} =[r,c-1]*(1-[r,c-2])	{F1} =[r,c-2]*[r,c-3]	{F1} =[r,c-1]	spacing
{F} =.5	{F1} =+[r-4,c+2]	{F1} =[r,c-1]*(1-[r,c-2])	{F1} =[r,c-2]*[r,c-3]	{F1} =[r,c-2]	margins
 	{F1}  	{F1}  	{F1}  	{F1} =[r-1,c-1]	tabs
*/

	self->image = 
		LL(V, 30,
			LL(V, 39,
			    LL(V, 60,
				R[0],		/* menucard and menuname */
				LL(H, 33,
				    R[1],		/* font, enable, disable */
				    R[2]				/* justification and baseline */
				)
			    ),
			    LL(V, 50,
				R[3],		/* fontsize   color*/
				R[5]		/* spacing */
			    )
			),
			LL(V, 50,
			    R[4],			/* ruler */
			    R[6]			/* tabs */
			)
		);

	ChooseShrinkIcon(self);
}

	boolean
lookzview__InitializeClass(ClassID)
	struct classhdr *ClassID;
{
	struct classinfo *textviewClassinfo;

	textviewClassinfo = class_Load("textview");
	proctable_DefineProc("lookzview-edit-styles", EditStylesInWindow, 
			textviewClassinfo, NULL,
			"Bring up style editor in a seprate window");

	MenuList = menulist_New();
	Keymap = keymap_New();
	AutoUpdate = environ_GetProfileSwitch("lookzautoupdate", FALSE);
	if (AutoUpdate) {
	    /* Skip first menu entry (Update Document) */
	    bind_BindList(MenuOptions+1, Keymap, MenuList,
		   &lookzview_classinfo);
	} else {
	    bind_BindList(MenuOptions, Keymap, MenuList,
		   &lookzview_classinfo);
	}
	/* We *really* want the default font to be smaller - it just looks
		Sooo gross if you have the default 12pt thing... 
		so while we're at it, let the user have a choice */
	StrtblFont = fontdesc_Create("andysans", fontdesc_Plain,
			environ_GetProfileInt("lookzfontsize", 10));

	return TRUE;
}

	boolean
lookzview__InitializeObject(ClassID, self)
	struct classhdr *ClassID;
	register struct lookzview  *self;
{
	long i, j;
	struct label *shrinklabel;

ENTER(lookzview__InitializeObject);
	self->MyMenus = menulist_DuplicateML(MenuList, self);
	self->Keystate = keystate_Create(self, Keymap);
	self->embedded = TRUE;
	self->foundstylesheet = FALSE;
	self->curcard = NULL;
	self->curss = NULL;
	self->curstyle = NULL;
	self->HasInputFocus = FALSE;
	self->NeedsUnpack = FALSE;
	self->OnScreen = FALSE;
	self->Linked = FALSE;
	self->OnceOnlyInUpdate = FALSE;	/* We haven't been thru yet */

	shrinklabel = label_New();
	label_SetFont(shrinklabel, "icon", fontdesc_Plain, 12);
	self->shrinkicon = labelview_New();
	labelview_SetHitProc(self->shrinkicon, (void (*)()) HitShrink, 
			(char *) self);
	labelview_SetDataObject(self->shrinkicon, shrinklabel);
	/* defer initial ChooseShrinkIcon because it will call SetText,
		which calls NotifyObservers, 
		which calls ObservedChanged in labelview, 
		which calls WantNewSize, 
		which requires having a parent 
		(The error is in having SetText call NotifyObservers
		rather than have that done by the caller of SetText.)
	*/

	for (i = 0; InitialStrings[i]; i++) {
		self->st[i] = stringtbl_New();
		for (j = 0; InitialStrings[i][j]; j++)
			stringtbl_AddString(self->st[i], 
				(unsigned char *)InitialStrings[i][j]);
		self->stv[i] = strtblview_New();
		strtblview_SetDataObject(self->stv[i], self->st[i]);
	}
	strtblview_SetItemHitProc(self->stv[(long)TNmenucard], HitMenuCard, self);
	strtblview_SetItemHitProc(self->stv[(long)TNstylename], HitStyleName, self);
	strtblview_SetItemHitProc(self->stv[(long)TNfont], HitFont, self);
	strtblview_SetItemHitProc(self->stv[(long)TNsize], HitSize, self);
	strtblview_SetItemHitProc(self->stv[(long)TNmode], HitMode, self);
	strtblview_SetItemHitProc(self->stv[(long)TNsubscr], HitSubscr, self);
	strtblview_SetItemHitProc(self->stv[(long)TNenable], HitEnable, self);
	strtblview_SetItemHitProc(self->stv[(long)TNdisable], HitDisable, self);
	strtblview_SetItemHitProc(self->stv[(long)TNspacing], HitSpacing, self);
	strtblview_SetItemHitProc(self->stv[(long)TNspread], HitSpread, self);
	strtblview_SetItemHitProc(self->stv[(long)TNcolor], HitColor, self);

	self->ruler = lprruler_New();
	self->tabruler = lprruler_New();
	self->tabrulerv = tabrulerview_New();
	self->rulerview = lprrulerview_New();
	tabrulerview_SetDataObject(self->tabrulerv, self->tabruler);
	lprrulerview_SetDataObject(self->rulerview, self->ruler);
	lprrulerview_SetValues(self->rulerview, lprrulerview_NoValue,
			 lprrulerview_NoValue, lprrulerview_NoValue);
	lprrulerview_SetValueChangeProc(self->rulerview, RulerValueChanged, self);
	tabrulerview_SetValues(self->tabrulerv, NULL);
	tabrulerview_SetValueChangeProc(self->tabrulerv, TabsValueChanged, self);
	
	self->image = NULL;
	self->shrinkparent = NULL;
	/* image is assembled in BuildImage(), called from LinkTree  */

LEAVE(lookzview__InitializeObject);

	return TRUE;
}

static
ChopTree(branch, lpairInfo)
	struct lpair *branch;
	struct classinfo *lpairInfo;
{
	struct classinfo *branchInfo = class_GetType(branch);
	/* destroy both children if the branch is either an lpair or
		a sub class of lpair (such as bpair) 
		XXX use knowledge that class_GetType returns ptr to classinfo */
	if (branch == NULL) {}
	else if (branchInfo == lpairInfo || branchInfo->superclass == lpairInfo) {
		struct view* o0 = lpair_GetNth(branch, 0);
		struct view* o1 = lpair_GetNth(branch, 1);
		lpair_SetNth(branch, 0, NULL);
		lpair_SetNth(branch, 1, NULL);
		lpair_UnlinkTree(branch);
		ChopTree((struct lpair *)o0, lpairInfo);
		ChopTree((struct lpair *)o1, lpairInfo);
		lpair_Destroy(branch);
	}
	else {
		struct view *v = (struct view *)branch;
		/* destroy a childview and its data object */
		struct dataobject *dobj = ((struct view *)branch)->dataobject;
		view_UnlinkTree(v);
		view_Destroy(v);
		dataobject_Destroy(dobj);
	}
}

void 
lookzview__FinalizeObject(ClassID, self)
	struct classhdr *ClassID;
	register struct lookzview  *self;
{
ENTER(lookzview__FinalizeObject);
	if (self->foundstylesheet)
		stylesheet_RemoveObserver(self->curss, self);

	lookzview_SetVisibility(self, TRUE);
	lookzview_LinkTree(self, self->header.view.parent);

	ChopTree(self->image, class_GetType(self->image));

	menulist_Destroy(self->MyMenus);
	keystate_Destroy(self->Keystate);
LEAVE(lookzview__FinalizeObject);
}

struct lookzview *
lookzview__GetApplicationLayer(self)
	struct lookzview *self;
{
	self->embedded = FALSE;
	/* XXX ought to delete the shrink-icon from the view tree */
	lookzview_WantInputFocus(self, self);
	return self;
}

	void
lookzview__LinkTree(self, parent)
	struct lookzview *self;
	struct view *parent;
{
ENTER(lookzview__LinkTree);
	super_LinkTree(self, parent);
	if (self->image == NULL)
		BuildImage(self);
	if (lookzview_GetVisibility(self)) {
		labelview_LinkTree(self->shrinkicon, NULL);
		lpair_SetNth(self->shrinkparent, 0, self->shrinkicon);
		lpair_LinkTree(self->image, self);
	}
	else {
		lpair_LinkTree(self->image, NULL);
		lpair_SetNth(self->shrinkparent, 0, NULL);
		labelview_LinkTree(self->shrinkicon, self);
	}
	self->Linked = TRUE;
LEAVE(lookzview__LinkTree);
}

/* intercept all requests from children for inputfocus 
		and cause the lookz itself to request inputfocus */
	void 
lookzview__WantInputFocus(self, child)
	register struct lookzview  *self;
	register struct view *child;
{
ENTER(lookzview__WantInputFocus);
	if ( ! self->HasInputFocus)
		super_WantInputFocus(self, self);
LEAVE(lookzview__WantInputFocus);
}

	void 
lookzview__ReceiveInputFocus(self)
	register struct lookzview  *self;
{
ENTER(lookzview__ReceiveInputFocus);
	lookzview_PostMenus(self, self->MyMenus);

	self->Keystate->next = NULL;
	lookzview_PostKeyState(self, self->Keystate);
	self->HasInputFocus = TRUE;
	if (self->Linked) {
		ChooseShrinkIcon(self);
		lookzview_WantUpdate( self, self );
	}
LEAVE(lookzview__ReceiveInputFocus);
}

	void 
lookzview__LoseInputFocus(self)
	register struct lookzview  *self;
{
ENTER(lookzview__LoseInputFocus);
	self->HasInputFocus = FALSE;
	ChooseShrinkIcon(self);
	lookzview_WantUpdate(self, self);
LEAVE(lookzview__LoseInputFocus);
}

	void 
lookzview__FullUpdate(self, type, left, top, width, height)
	struct lookzview  *self;
	enum view_UpdateType  type;
	long left, top, width, height;
{
	struct rectangle r;
	int i;
	enum view_UpdateType lpairtype;

	self->OnScreen = (type != view_Remove);
	if (! self->Linked)
		lookzview_LinkTree(self, self->header.view.parent);
	if (type == view_FullRedraw)
		lookzview_GetLogicalBounds(self, &r);
	else 
		rectangle_SetRectSize(&r, left, top, width, height);

DEBUG(("FullUpdate type %d  redraw (%d,%d,%d,%d) within (%d,%d,%d,%d)\n", 
			type, left, top, width, height, r.left, r.top, r.width, r.height));
DEBUG(("	Drawable at 0x%lx   Visibile: %s \n", lookzview_GetDrawable(self),
			(lookzview_GetVisibility(self)) ? "yes" : "no"));

	if ((type == view_FullRedraw || type == view_LastPartialRedraw)
			 && ! self->foundstylesheet) {
		/* BOGOSITY ALERT:  we grub around in the parent to
		  find its stylesheet */
		register struct view *v;
		struct basicobject *texttype 
			= (struct basicobject *)class_Load("textview");
		for (v = (struct view *)self; v != NULL; v = v->parent) 
			if (class_IsType(v, texttype)) {
				register struct text *t = (struct text *)v->dataobject;
				if (t && t->styleSheet) 
					OpenStyleSheet(self, t->styleSheet);
				break;
			}
	}
	if (type == view_FullRedraw && self->embedded && self->OnScreen) {
		lookzview_SetTransferMode(self, graphic_COPY);
		lookzview_MoveTo(self, 0, 0);
		lookzview_DrawLineTo(self, r.width-1, 0);
		lookzview_DrawLineTo(self, r.width-1, r.height-1);
		lookzview_DrawLineTo(self, 0, r.height-1);
		lookzview_DrawLineTo(self, 0, 0);

		r.top++, r.left++, r.height-=2, r.width-=2;
	}

	if (lookzview_GetVisibility(self)) {
		if (type != view_PartialRedraw 
			/* && type != view_LastPartialRedraw */) {
			lpair_InsertView(self->image, self, &r);
		}
		/* Now that we are here, the strtbl's have been instantiated,
		   so we can set their font's to something reasonable! */
		/* This should only be done once... */
		if (!self->OnceOnlyInUpdate) {
			for (i = 0; InitialStrings[i]; i++)
				strtblview_SetFont(self->stv[i], StrtblFont);
			self->OnceOnlyInUpdate = TRUE;
		}

		lpairtype = (type == view_LastPartialRedraw) ? view_FullRedraw : type;
		DEBUG(("	FullUpdate lpair with type %d\n", lpairtype));
		lpair_FullUpdate(self->image, lpairtype, left, top, width, height);
	} else {
		if (type != view_PartialRedraw 
			 && type != view_LastPartialRedraw) 
			labelview_InsertView(self->shrinkicon, self, &r);
		labelview_FullUpdate(self->shrinkicon,  type, left, top, width, height);
	}
	LEAVE(lookzview__FullUpdate);
}

	void 
lookzview__Update( self )
	register struct lookzview *self;
{
ENTER(lookzview__Update);
	if (! self->OnScreen) return;
	if ( ! self->Linked)
		lookzview_FullUpdate(self, view_FullRedraw, 0, 0,
				lookzview_GetLogicalWidth(self),
				lookzview_GetLogicalHeight(self));
	else if (lookzview_GetVisibility(self)) {
		if (self->NeedsUnpack) 
			UnpackStyle(self);
		lpair_Update(self->image);
	}
	else 
		labelview_Update(self->shrinkicon);
LEAVE(lookzview__Update);
}

	struct view *
lookzview__Hit(self, action, x, y, num_clicks)
	register struct lookzview  *self;
	register enum view_MouseAction  action;
	register long  x, y, num_clicks;
{
	struct view *ret;
	boolean oldvis=lookzview_GetVisibility(self);
ENTER(lookzview__Hit);
	if (action == view_NoMouseEvent)
		return (struct view *)self;
	if ( ! self->OnScreen) return NULL;
DEBUG(("	OldVis %d\n", oldvis));
	if (oldvis)
		ret = (struct view *)lpair_Hit(self->image, action, x, y, num_clicks);
	else
		ret = (struct view *)labelview_Hit(self->shrinkicon, action, x, y, 
				num_clicks);
LEAVE(lookzview__Hit);
	if (oldvis == lookzview_GetVisibility(self))
		return ret;
	else return (struct view *)self;	/* visibility changed */
}

	enum view_DSattributes
lookzview__DesiredSize( self, width, height, pass, 
				desiredWidth, desiredHeight ) 
	register struct lookzview *self;
	long width;
	long height;
	enum view_DSpass pass;
	long *desiredWidth;
	long *desiredHeight;
{
	if (lookzview_GetVisibility(self)) 
		*desiredWidth = 550,  *desiredHeight = 400;
	else
		*desiredWidth = 26,  *desiredHeight = 20;
DEBUG(("Desired Size %d x %d\n", *desiredWidth, *desiredHeight));
	return view_Fixed;
}

	void
lookzview__Print( self, file, processor, format, level )
	register struct lookzview 	 *self;
	register FILE   file;
	register char  	 *processor;
	register char  	 *format;
	register boolean  	level;
{
	/* never print anything */
}

	void
lookzview__ObservedChanged(self, dobj, status)
	register struct lookzview  *self;
	struct dataobject  *dobj;
	long  status;
{
	if (dobj == lookzview_GetDataObject(self)) {
	    struct lookz *lookz;
	    struct text *text;

	    lookz = (struct lookz *) lookzview_GetDataObject(self);
	    text = lookz_GetTextObject(lookz);

	    if (status == lookz_TEXTOBJECTCHANGED) {
		NewTextObject(self, text);
	    }
	    if (status == observable_OBJECTDESTROYED) {
		long i;

		for (i = 0; i < numStyleEditors; i++) {
		    if (((struct dataobject *) (styleEditors[i].lookz)) == dobj) {
			styleEditors[i].text = NULL;
			styleEditors[i].lookz = NULL;
			break;
		    }
		}
	    }
	}
	else if (dobj == (struct dataobject *) self->curss) {
	    if (status == observable_OBJECTDESTROYED) {
		CloseStyleSheet(self);
	    }
	    else if (status == (long)self || self->curstyle == NULL) {
		return;	/* changed by self */
	    }
	    else {
		self->NeedsUnpack = TRUE;
	    }
	}

	lookzview_WantUpdate(self, self);
}

	void
lookzview__SetVisibility(self, visibility)
	struct lookzview *self;
	boolean visibility;
{
	if (visibility != lookzview_GetVisibility(self)) {
DEBUG(("Change Visibility to %d\n", visibility));
		/* I am surprised I need the cast in the next line */
		lookz_SetVisibility((struct lookz *)labelview_GetDataObject(self), 
				visibility);
		self->Linked = FALSE;
	}
}

	boolean
lookzview__GetVisibility(self)
	struct lookzview *self;
{
	return lookz_GetVisibility(lookzview_GetDataObject(self));
}

	void
lookzview__SetStyleSheet(self, ss)
	struct lookzview *self;
	struct stylesheet *ss;
{
	CloseStyleSheet(self);
	OpenStyleSheet(self, ss);
}

	struct stylesheet *
lookzview__GetStyleSheet(self)
	struct lookzview *self;
{
	return self->curss;
}

	void 
lookzview__SetEmbedded(self, isEmbedded)
	struct lookzview *self;
	boolean isEmbedded;
{
	self->embedded = isEmbedded;
}

	static void 
UpdateDocument(self)
	register struct lookzview *self;
{
	struct lookz *lookz;
	struct text *text;

	lookz = (struct lookz *) lookzview_GetDataObject(self);
	text = lookz_GetTextObject(lookz);

	if (text != NULL) {
	        text_SetModified(text);
		text_RegionModified(text, 0, text_GetLength(text));
		text_NotifyObservers(text, observable_OBJECTCHANGED);
	}
	if (self->embedded) {
		/* BOGOSITY ALERT:  we call Full_Update of parent */
		register struct view *v;
		for (v = (struct view *)self; v != NULL; v = v->parent) 
			if (strcmp("textview", class_GetTypeName(v)) == 0) {
				/* this is exceedingly dangerous XXX XXX
					 We call a FullUpdate 
					 without establishing any environment */
				view_FullUpdate(v, view_FullRedraw, 0, 0, 0, 0);
				break;
			}
	}
}

/* * * * * * * * * * * * * /
/*   A D D   S T Y L E   */
/* * * * * * * * * * * * */

static char CardFound[50];   /* result from FindCardName */

/* FindCardName(s, nm)
	compare nm to the menu card name in s
	when match, set CardFound and exit
*/
	static boolean
FindCardName(s, nm)
	register struct style *s;
	register char *nm;
{
	register char *mnnm = style_GetMenuName(s);
	register char *iw;
	if (mnnm != NULL  &&  (iw=InitialWord(mnnm)) != NULL
			&& FOLDEDEQ(nm, iw)) {
		/* found a match */
		long len;
		char *comma;
		comma = index(mnnm, ',');
		if (comma == NULL)
			len = strlen(mnnm);
		else
			len = comma - mnnm;
		if (len > 49)  len = 49;
		strncpy(CardFound, mnnm, len);
		CardFound[len] = '\0';
		return TRUE;
	}
	return FALSE;
}

/* NewMenuName(name)
	compute a MenuName string for name
	return pointer to static storage containing the name
*/
	static char *
NewMenuName(lv, name)
	struct lookzview *lv;
	char *name;
{
	static char buf[100];
	char *tilde, *comma;
	char *eltloc;

	tilde = index(name, '~');
	comma = index(name, ',');

	*CardFound = '\0';
	if (comma != NULL && (tilde == NULL || tilde > comma)) {
		/* There is no tilde for the card name,
			see if there is an existing menu card */
		*comma = '\0';
		stylesheet_EnumerateStyles(lv->curss, FindCardName, (long) name);
		*comma = ',';
	}
	if (*CardFound != '\0') {
		/* use existing card name and the remainder of 'name' */
		register long len = strlen(CardFound);
		strcpy(buf, CardFound);
		eltloc = buf + len;
		strncpy(eltloc++, comma, 99 - len);
	}
	else {
		/* new card and new style.  just do capitalization */
		strncpy(buf, name, 99);
		if (islower(*buf))  *buf = toupper(*buf);
		eltloc = (comma == NULL) ? NULL 
			: buf + (comma-name) + 1;
	}
	buf[99] = '\0';
	if (eltloc != NULL && islower(*eltloc)) 
		*eltloc = toupper(*eltloc);
	return buf;
}

	static void 
AddStyle(self)
	register struct lookzview *self;
{
	register struct style *newsty;
	char *stylename;
	long accnum;
	char name[75], temp[75];

	if ( ! self->foundstylesheet) return;	/* can't add if have no style sheeet */
	ClearStyleDisplay(self);
	stringtbl_ClearBits(self->st[(long)TNstylename]);

	sprintf(temp, "%s,", self->curcard);
	if (message_AskForString(self, 0, "Name for new style: ", temp, name, 75) < 0 
			|| *temp == '\0') {
		message_DisplayString(self, 0, "Cancelled.");
		return;
	}

	stringtbl_ClearBits(self->st[(long)TNmenucard]);
	accnum = stringtbl_AddString(self->st[(long)TNmenucard], 
			(unsigned char *)InitialWord(name));
	stringtbl_SetBitOfEntry(self->st[(long)TNmenucard], accnum, 1);
	self->curcard = (char *)stringtbl_GetStringOfEntry(self->st[(long)TNmenucard], 
				accnum);

	stylename = index(name, ',');
	if (stylename) stylename = InitialWord(stylename+1);

	if ( stylename == NULL || *stylename == '\0') {
		message_DisplayString(self, 0, 
			"Missing comma or stylename.  Enter name as:  <MenuCard>,<StyleName>");
		return;
	}

	stylename = clean(stylename);
	/* by this time 'stylename' contains an all lowercase version of the 
		name after the comma without any ~00 */
	newsty=stylesheet_Find(self->curss, stylename);
	if (newsty == NULL) {
		/* create the style */
		newsty = style_New();
		style_SetName(newsty, stylename);
		stylesheet_Add(self->curss, newsty);
		if (strncmp(name, NOMENUSTRING, strlen(NOMENUSTRING)) != 0) {
			style_SetMenuName(newsty, NewMenuName(self, name));
		}
	}
	else if (index(name, '~') != NULL) {
		/* existing style and the input contains a ~, 
				change the menu name */
		if (strncmp(name, NOMENUSTRING, strlen(NOMENUSTRING)) != 0) {
			style_SetMenuName(newsty, name);
		}
	}

	/* Update the display so the new style menu appears. */
	stringtbl_Clear(self->st[(long)TNstylename]);
	if (self->curcard && (ULstrcmp(self->curcard, NOMENUSTRING) != 0)) {
	    stylesheet_EnumerateStyles(self->curss, AddStyleName, (long) self);
	    strtblview_SetItemHitProc(self->stv[(long)TNstylename], HitStyleName, self);
	}
	else {
	    stylesheet_EnumerateStyles(self->curss, AddStyleNameNoMenus, (long) self);
	    strtblview_SetItemHitProc(self->stv[(long)TNstylename], HitStyleNameNoMenus, self);
	}

	SetStyleDisplay(self, newsty);
}

/*
 * This is the callback when enumerating the text environments.
 * We return TRUE when the given environment uses the style we are
 * deleting.
 */
boolean CheckDelStyle(sty, txt, level, env)
struct style *sty;
struct text *txt;
int level;
struct environment *env;
{
    return (env != NULL && env->type == environment_Style && env->data.style == sty);
}

static void DeleteStyle(self)
	register struct lookzview *self;
{
	struct style *delsty;
	struct lookz *lz;
	struct text *txt;
	struct environment *env;
	long pos, length;
	boolean askedfordelete;
	long result;
	char *stylename, *curstylename;
	char name[75], temp[75];
	static char *choices[] = { "Yes", "No", NULL };

	if ( ! self->foundstylesheet) return;	/* can't delete if have no style sheeet */

	/* Ask for the style.  Use the currently selected style as the default. */
	curstylename = NULL;
	if (self->curstyle)
	    curstylename = style_GetName(self->curstyle);

	sprintf(temp, "%s,%s", self->curcard, curstylename ? curstylename : "");
	if (message_AskForString(self, 0, "Style name to delete: ", temp, name, 75) < 0) {
		message_DisplayString(self, 0, "Cancelled.");
		return;
	}
	stylename = index(name, ',');
	if (stylename) stylename = InitialWord(stylename+1);

	if ( stylename == NULL || *stylename == '\0') {
		message_DisplayString(self, 0, 
			"Missing comma or stylename.  Enter name as:  <MenuCard>,<StyleName>");
		return;
	}
	stylename = clean(stylename);
	/* by this time 'stylename' contains an all lowercase version of the 
		name after the comma without any ~00 */
	delsty=stylesheet_Find(self->curss, stylename);
	if (delsty == NULL) {
		message_DisplayString(self, 0, "Unknown style name.");
		return;
	}

	/* Delete the style from the template's initial text. */
	askedfordelete = FALSE;
	lz = (struct lookz *)lookzview_GetDataObject(self);
	if (lz) {
	    txt = lookz_GetTextObject(lz);
	    if (txt) {
		/* This is a bit inefficient, but it doesn't happen much. */
		do {
		    env = text_EnumerateEnvironments(txt, 0, text_GetLength(txt), CheckDelStyle, delsty);
		    if (env) {
			if (!askedfordelete) {
			    if (message_MultipleChoiceQuestion(self, 51,
				"The templates uses this style in its initial text.\nIs it ok to delete the style from the text?",
				1, &result, choices, NULL) < 0) {
				message_DisplayString(self, 0, "Cancelled.");
				return;
			    }
			    if (result == 1) {
				message_DisplayString(self, 0, "Style not deleted.");
				return;
			    }
			    askedfordelete = TRUE;
			}
			/* Delete this environment. */
			pos = environment_Eval(env);
			length = environment_GetLength(env);
			environment_Remove(txt->rootEnvironment, pos, length, environment_Style, FALSE);
		    }
		} while (env != NULL);
	    }
	}

	if (self->curstyle == delsty)
	    self->curstyle = NULL;

	/* Now delete the style itself. */
	stylesheet_Delete(self->curss, delsty);

	/* Update the display so the deleted menu disappears. */
	stringtbl_Clear(self->st[(long)TNstylename]);
	ClearStyleDisplay(self);
	if (self->curcard && (ULstrcmp(self->curcard, NOMENUSTRING) != 0)) {
	    stylesheet_EnumerateStyles(self->curss, AddStyleName, (long) self);
	    strtblview_SetItemHitProc(self->stv[(long)TNstylename], HitStyleName, self);
	}
	else {
	    stylesheet_EnumerateStyles(self->curss, AddStyleNameNoMenus, (long) self);
	    strtblview_SetItemHitProc(self->stv[(long)TNstylename], HitStyleNameNoMenus, self);
	}
	stylesheet_NotifyObservers(self->curss, (long) self);
	UpdateDocument(self);
	sprintf(temp, "Style \"%s\" deleted%s.", name,
		askedfordelete ? " and removed from the initial text" : "");
	message_DisplayString(self, 0, temp);
  }
	void 
lookzview__SetDataObject(self, dobj)
	struct lookzview *self;
	struct dataobject *dobj;
{
	super_SetDataObject(self, dobj);
	if (class_IsTypeByName(class_GetTypeName(dobj), "lookz")) {
		struct lookz *lookz = (struct lookz *) dobj;
		NewTextObject(self, lookz_GetTextObject(lookz));
	}
}

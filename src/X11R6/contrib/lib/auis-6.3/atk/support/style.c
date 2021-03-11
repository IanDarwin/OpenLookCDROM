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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/support/RCS/style.c,v 2.23 1993/09/17 21:21:25 rr2b Exp $";
#endif


 

#include <class.h>
#include <style.eh>

#include <namespc.ih>
#include <fontdesc.ih>

#define iswhite(c) ((c) == ' ' || (c) == '\t' || (c) == '\n')

/*
 * Utility routines
 */

/* Convert any unit to unit style_RawDots */

static long CVDots(amt, unit)
register long amt;
enum style_Unit unit;
{
    switch (unit) {
        case style_RawDots:
            return amt;
        case style_Inches:
            return (amt * 72) >> 16;
        case style_CM:
            return (amt * 28) >> 16;
        case style_Points:
            return amt;
        case style_Ems:     /* amt * 10? */
            return 0;
        case style_Lines:   /* amt * 10? */
            return 0;
    }

    return 0;
}

static int style_freeattributes(procdata, curnamespace, indexvalue)
long procdata;
struct namespace * curnamespace;
int indexvalue; {
    char * tmpValue;
    if (namespace_BoundpAt(curnamespace,indexvalue)) {
	tmpValue = (char *) namespace_ValueAt(curnamespace,indexvalue);
	if (tmpValue) free(tmpValue);
    }
    return TRUE;
}


/*
 * Datastream alias table for attributes
 */

static char *AttributeAlias[] = {
    "LeftMargin",
    "RightMargin",
    "Indent",
    "Script",
    "Justification",
    "Spacing",
    "Spread",
    "Tabs",
    "Flags",
    "Function",
    "",             /* unused */
    "FontFamily",
    "FontFace",
    "ExternalSize",
    "FontSize",	    /* hack to avoid splitting FontMod yet */
    "TopMargin",
    "BottomMargin",
    "Above",
    "Below",
    "CounterName",
    "CounterParent",
    "CounterScope",
    "CounterPosition",
    "CounterInitialValue",
    NULL
};

static char *OptypeAlias[] = {
    "Inch",
    "Cm",
    "Point",
    "Em",
    "Line",
    "String",
    "Int",
    NULL
};

static char *OpcodeAlias[] = {
    "Noop",
    "Copy",
    "Add",
    "Set",
    "Clear",
    NULL
};

static char *TabAlignmentAlias[] = {
    "",
    "LeftAligned",
    "RightAligned",
    "CenteredOnTab",
    "CenteredBetweenTab",
    "TabDivide",
    "CharAligned",
    "TabClear",
    "AllClear",
    NULL
};

static char *JustificationAlias[] = {
    "PreviousJustification",
    "LeftJustified",
    "RightJustified",
    "Centered",
    "LeftAndRightJustified",
    "LeftThenRightJustified",
    NULL
};

static char *ScopeAlias[] = {
    "GlobalScope",
    "LocalScope",
    NULL
};

static char *PositionAlias[] = {
    "EnvironmentCtr",
    "ParagraphCtr",
    "LineCtr",
    NULL
};

static char *MarginValueAlias[] = {
    "ConstantMargin",
    "LeftMargin",
    "LeftEdge",
    "RightMargin",
    "RightEdge",
    "TopMargin",
    "TopEdge",
    "BottomMargin",
    "BottomEdge",
    "PreviousIndentation",
    NULL
};

static char *SpacingValueAlias[] = {
    "ConstantSpacing",
    "InterlineSpacing",
    "InterparagraphSpacing",
    "AboveSpacing",
    "BelowSpacing",
    NULL
};

static char *FontSizeAlias[] = {
    "PreviousFontSize",
    "ConstantFontSize",
    NULL
};

static char *ScriptMovementAlias[] = {
    "PreviousScriptMovement",
    "ConstantScriptMovement",
    NULL
};

static char *FlagAlias[] = {
    "Underline",
    "Hidden",
    "ReadOnly",
    "PassThru",
    "Icon",		/* unused */
    "ContinueIndent",
    "Hinge",
    "NewPage",
    "ChangeBar",
    "OverBar",
    "NoWrap",
    "NoFill",
    "KeepPriorNL",
    "KeepNextNL",
    "TabsCharacters",
    "DottedBox",
    "StrikeThrough",
    NULL
};

static char *FaceStyleAlias[] = {
    "Bold",
    "Italic",
    "Shadow",
    "FixedFace",
    "Outline",
    "Thin",
    "Black",
    "Medium",
    "Heavy",
    "Condense",
    NULL
};

/* The following must be kept in sync with the flag defs in style.ch */

enum style_FlagIndex {
    style_UnderlineIndex=0,
    style_HiddenIndex=1,
    style_ReadOnlyIndex=2,
    style_PassThruIndex=3,
    style_IconIndex=4,
    style_ContinueIndentIndex=5,
    style_HingeIndex=6,
    style_NewPageIndex=7,
    style_ChangeBarIndex=8,
    style_OverBarIndex=9,
    style_NoWrapIndex=10,
    style_NoFillIndex=11,
    style_KeepPriorNLIndex=12,
    style_KeepNextNLIndex=13,
    style_TabsCharactersIndex=14,
    style_DottedBoxIndex=15,
    style_StrikeThroughIndex=16
};

/* The following must be kept in sync with the flag defs in style.ch */

enum style_FontFaceIndex {
    style_BoldIndex=0,
    style_ItalicIndex=1,
    style_ShadowIndex=2,
    style_FixedFaceIndex=3,
    style_OutlineIndex=4,
    style_ThinIndex=5,
    style_BlackIndex=6,
    style_MediumIndex=7,
    style_HeavyIndex=8,
    style_CondenseIndex=9
};

/*
 * Class procedures
 */

static char unknownstyle[]="unknown";

boolean style__InitializeObject(classID, self)
struct classheader *classID;
struct style *self;
{
    self->name = malloc(sizeof(unknownstyle));
    if(self->name==NULL) return FALSE;
    strcpy(self->name, unknownstyle);
    self->menuName = NULL;
    self->template = FALSE;

    self->FontFamily = NULL;
    self->TabChangeList = NULL;
    self->NumTabChanges = 0;
    self->CounterName = NULL;
    self->CounterParent = NULL;
    self->CounterStyles = NULL;

    self->AdditionalAttributes = NULL;
    style_Reset(self);

    return TRUE;
}

void style__FinalizeObject(classID, self)
struct classheader *classID;
struct style *self;
{
    if (self->name != NULL)
        free(self->name);
    if (self->menuName != NULL)
        free(self->menuName);

    if (self->FontFamily != NULL)
        free(self->FontFamily);
    if (self->CounterName != NULL)
        free(self->CounterName);
    if (self->CounterParent != NULL)
        free(self->CounterParent);

    style_ClearCounterStyles(self);
    style_ClearTabChanges(self);

    if (self->AdditionalAttributes) {
	/* First get rid of any strings left as values */
	(void) namespace_Enumerate(self->AdditionalAttributes, style_freeattributes, NULL);
	namespace_Destroy(self->AdditionalAttributes);
    }
}

/*
 * Methods
 */

void style__Reset(self)
struct style *self;
{
    style_SetNewLeftMargin(self, style_LeftMargin, 0, style_Points);
    style_SetNewRightMargin(self, style_RightMargin, 0, style_Points);
    style_SetNewTopMargin(self, style_TopMargin, 0, style_Points);
    style_SetNewBottomMargin(self, style_BottomMargin, 0, style_Points);

    style_SetNewIndentation(self, style_PreviousIndentation, 0, style_Points);
    style_SetJustification(self, style_PreviousJustification);

    style_SetNewInterparagraphSpacing(self, style_InterparagraphSpacing, 0, style_Points);
    style_SetNewAbove(self, style_AboveSpacing, 0, style_Points);
    style_SetNewBelow(self, style_BelowSpacing, 0, style_Points);
    style_SetNewInterlineSpacing(self, style_InterlineSpacing, 0, style_Points);

    if (self->FontFamily != NULL) {
	free(self->FontFamily);
        self->FontFamily = NULL;
    }

    style_SetFontSize(self, style_PreviousFontSize, 0);
    style_SetFontScript(self, style_PreviousScriptMovement, 0, style_Points);
    self->AddFontFaces= (long) fontdesc_Plain;
    self->OutFontFaces= ~ (long) fontdesc_Plain;

    style_ClearTabChanges(self);

    if (self->CounterName != NULL) {
	free(self->CounterName);
	self->CounterName = NULL;
    }

    self->CounterScope = style_GlobalScope;
    self->CounterPosition=style_EnvironmentCtr;

    if (self->CounterParent != NULL) {
	free(self->CounterParent);
	self->CounterParent = NULL;
    }

    self->CounterInitialValue = 0;
    style_ClearCounterStyles(self);

    self->AddMiscFlags = style_NoFlags;
    self->OutMiscFlags = ~style_NoFlags;

    if (self->AdditionalAttributes) {
	/* First get rid of any strings left as values */
	(void) namespace_Enumerate(self->AdditionalAttributes, style_freeattributes, NULL);
	namespace_Destroy(self->AdditionalAttributes);
    }
}

static int style_copyattributes(procdata, curnamespace, indexvalue)
long procdata;
struct namespace * curnamespace;
int indexvalue; {
    char * tmpValue;
    char * tmpAttributeName;
    struct atom * tmpAttributeAtom;
    if (namespace_BoundpAt(curnamespace,indexvalue)) {
	tmpValue = (char *) namespace_ValueAt(curnamespace,indexvalue);
	tmpAttributeAtom = namespace_NameAt(curnamespace, indexvalue);
	tmpAttributeName = atom_Name(tmpAttributeAtom);
	style_AddAttribute((struct style*) procdata, tmpAttributeName, tmpValue);
    }
    return TRUE;
}

void style__Copy(self, dest)
struct style *self, *dest;
{
    register int i;
    register char **counterstyle;
    register struct tabentry *tabchange;

    style_SetName(dest, self->name);
    style_SetMenuName(dest, self->menuName);
    dest->template = self->template;

    style_SetNewLeftMargin(dest, self->NewLeftMargin.MarginBasis, self->NewLeftMargin.Operand, self->NewLeftMargin.MarginUnit);
    style_SetNewRightMargin(dest, self->NewRightMargin.MarginBasis, self->NewRightMargin.Operand, self->NewRightMargin.MarginUnit);
    style_SetNewTopMargin(dest, self->NewTopMargin.MarginBasis, self->NewTopMargin.Operand, self->NewTopMargin.MarginUnit);
    style_SetNewBottomMargin(dest, self->NewBottomMargin.MarginBasis, self->NewBottomMargin.Operand, self->NewBottomMargin.MarginUnit);

    style_SetNewIndentation(dest, self->NewIndentation.MarginBasis, self->NewIndentation.Operand, self->NewIndentation.MarginUnit);
    style_SetJustification(dest,self->NewJustification);

    style_SetNewInterparagraphSpacing(dest, self->NewInterparagraphSpacing.SpacingBasis, self->NewInterparagraphSpacing.Operand, self->NewInterparagraphSpacing.SpacingUnit);
    style_SetNewAbove(dest, self->NewAbove.SpacingBasis, self->NewAbove.Operand, self->NewAbove.SpacingUnit);
    style_SetNewBelow(dest, self->NewBelow.SpacingBasis, self->NewBelow.Operand, self->NewBelow.SpacingUnit);
    style_SetNewInterlineSpacing(dest, self->NewInterlineSpacing.SpacingBasis, self->NewInterlineSpacing.Operand, self->NewInterlineSpacing.SpacingUnit);

    style_SetFontFamily(dest, self->FontFamily);
    style_SetFontSize(dest, self->FontSize.SizeBasis, self->FontSize.Operand);
    style_SetFontScript(dest, self->FontScript.ScriptBasis, self->FontScript.Operand, self->FontScript.SizeUnit);
    dest->AddFontFaces= (long) self->AddFontFaces;
    dest->OutFontFaces= (long) self->OutFontFaces;

    style_ClearTabChanges(dest);
    dest->NumTabChanges= 0;
    dest->TabChangeList=NULL;
    for (i = 0, tabchange = self->TabChangeList; i < self->NumTabChanges; i++, tabchange++)
	style_AddTabChange(dest, tabchange->TabOpcode, tabchange->Location, tabchange->LocationUnit);

    style_ClearCounterStyles(dest);
    style_SetCounterName(dest, self->CounterName);
    dest->CounterScope = self->CounterScope;
    dest->CounterPosition = self->CounterPosition;
    style_SetCounterParent(dest, self->CounterParent);
    dest->CounterInitialValue = self->CounterInitialValue;
    dest->NumCounterStyles = self->NumCounterStyles;
    for (i = 0, counterstyle = self->CounterStyles; i < self->NumCounterStyles; i++, counterstyle++)
	style_AddCounterStyle(dest, *counterstyle);

    dest->AddMiscFlags = self->AddMiscFlags;
    dest->OutMiscFlags = self->OutMiscFlags;

    if (self->AdditionalAttributes)
	(void) namespace_Enumerate(self->AdditionalAttributes, style_copyattributes, dest);
}

/* Attribute Setting and Clearing Functions */

void style__SetName(self, name)
struct style *self;
char *name;
{
    if (self->name != NULL)
        free(self->name);
    if (name == NULL)
        name = unknownstyle;
        self->name = malloc(strlen(name) + 1),
        strcpy(self->name, name);
}

char *style__GetName(self)
struct style *self;
{
    return self->name;
}

void style__SetMenuName(self, menuName)
struct style *self;
char *menuName;
{
    if (self->menuName != NULL)
        free(self->menuName);
    if (menuName == NULL)
        self->menuName = NULL;
    else {
        self->menuName = malloc(strlen(menuName)+ 1),
        strcpy(self->menuName, menuName);
    }
}

char *style__GetMenuName(self)
struct style *self;
{
    return self->menuName;
}

void style__SetNewLeftMargin(self, Basis, Operand, Unit)
struct style *self;
enum style_MarginValue Basis;
long Operand;
enum style_Unit Unit;
{
    self->NewLeftMargin.MarginBasis = Basis;
    self->NewLeftMargin.Operand = Operand;
    self->NewLeftMargin.MarginUnit = Unit;
    self->NewLeftMargin.DotCvtOperand = CVDots(Operand,Unit);
}

void style__GetNewLeftMargin(self, RefBasis, RefOperand, RefUnit)
struct style *self;
enum style_MarginValue *RefBasis;
long *RefOperand;
enum style_Unit *RefUnit;
{
    *RefBasis =  self->NewLeftMargin.MarginBasis;
    *RefOperand = self->NewLeftMargin.Operand;
    *RefUnit = self->NewLeftMargin.MarginUnit;
}

void style__SetNewRightMargin(self, Basis, Operand, Unit)
struct style *self;
enum style_MarginValue Basis;
long Operand;
enum style_Unit Unit;
{
    self->NewRightMargin.MarginBasis = Basis;
    self->NewRightMargin.Operand = Operand;
    self->NewRightMargin.MarginUnit = Unit;
    self->NewRightMargin.DotCvtOperand = CVDots(Operand,Unit);
}

void style__GetNewRightMargin(self, RefBasis, RefOperand, RefUnit)
struct style *self;
enum style_MarginValue *RefBasis;
long *RefOperand;
enum style_Unit *RefUnit;
{
    *RefBasis =  self->NewRightMargin.MarginBasis;
    *RefOperand = self->NewRightMargin.Operand;
    *RefUnit = self->NewRightMargin.MarginUnit;
}

void style__SetNewTopMargin(self, Basis, Operand, Unit)
struct style *self;
enum style_MarginValue Basis;
long Operand;
enum style_Unit Unit;
{
    self->NewTopMargin.MarginBasis = Basis;
    self->NewTopMargin.Operand = Operand;
    self->NewTopMargin.MarginUnit = Unit;
    self->NewTopMargin.DotCvtOperand = CVDots(Operand, Unit);
}

void style__GetNewTopMargin(self, RefBasis, RefOperand, RefUnit)
struct style *self;
enum style_MarginValue *RefBasis;
long *RefOperand;
enum style_Unit *RefUnit;{
    *RefBasis =  self->NewTopMargin.MarginBasis;
    *RefOperand = self->NewTopMargin.Operand;
    *RefUnit = self->NewTopMargin.MarginUnit;
}

void style__SetNewBottomMargin(self, Basis, Operand, Unit)
struct style *self;
enum style_MarginValue Basis;
long Operand;
enum style_Unit Unit;
{
    self->NewBottomMargin.MarginBasis = Basis;
    self->NewBottomMargin.Operand = Operand;
    self->NewBottomMargin.MarginUnit = Unit;
    self->NewBottomMargin.DotCvtOperand = CVDots(Operand, Unit);
}

void style__GetNewBottomMargin(self, RefBasis, RefOperand, RefUnit)
struct style *self;
enum style_MarginValue *RefBasis;
long *RefOperand;
enum style_Unit *RefUnit;
{
    *RefBasis =  self->NewBottomMargin.MarginBasis;
    *RefOperand = self->NewBottomMargin.Operand;
    *RefUnit = self->NewBottomMargin.MarginUnit;
}

void style__SetNewIndentation(self, Basis, Operand, Unit)
struct style *self;
enum style_MarginValue Basis;
long Operand;
enum style_Unit Unit;
{
    self->NewIndentation.MarginBasis = Basis;
    self->NewIndentation.Operand = Operand;
    self->NewIndentation.MarginUnit = Unit;
    self->NewIndentation.DotCvtOperand = CVDots(Operand, Unit);
}

void style__GetNewIndentation(self, RefBasis, RefOperand, RefUnit)
struct style *self;
enum style_MarginValue *RefBasis;
long *RefOperand;
enum style_Unit *RefUnit;
{
    *RefBasis =  self->NewIndentation.MarginBasis;
    *RefOperand = self->NewIndentation.Operand;
    *RefUnit = self->NewIndentation.MarginUnit;
}

void style__SetNewInterparagraphSpacing(self, Basis, Operand, Unit)
struct style *self;
enum style_SpacingValue Basis;
long Operand;
enum style_Unit Unit;
{
    self->NewInterparagraphSpacing.SpacingBasis = Basis;
    self->NewInterparagraphSpacing.Operand = Operand;
    self->NewInterparagraphSpacing.SpacingUnit = Unit;
    self->NewInterparagraphSpacing.DotCvtOperand = CVDots(Operand, Unit);
}

void style__GetNewInterparagraphSpacing(self, RefBasis,RefOperand, RefUnit)
struct style *self;
enum style_SpacingValue *RefBasis;
long *RefOperand;
enum style_Unit *RefUnit;
{
    *RefBasis = self->NewInterparagraphSpacing.SpacingBasis;
    *RefOperand = self->NewInterparagraphSpacing.Operand;
    *RefUnit = self->NewInterparagraphSpacing.SpacingUnit;
}

void style__SetNewAbove(self, Basis, Operand, Unit)
struct style *self;
enum style_SpacingValue Basis;
long Operand;
enum style_Unit Unit;
{
    self->NewAbove.SpacingBasis = Basis;
    self->NewAbove.Operand = Operand;
    self->NewAbove.SpacingUnit = Unit;
    self->NewAbove.DotCvtOperand = CVDots(Operand, Unit);
}

void style__GetNewAbove(self, RefBasis, RefOperand, RefUnit)
struct style *self;
enum style_SpacingValue *RefBasis;
long *RefOperand;
enum style_Unit *RefUnit;
{
    *RefBasis = self->NewAbove.SpacingBasis;
    *RefOperand = self->NewAbove.Operand;
    *RefUnit = self->NewAbove.SpacingUnit;
}

void style__SetNewBelow(self, Basis, Operand, Unit)
struct style *self;
enum style_SpacingValue Basis;
long Operand;
enum style_Unit Unit;
{

    self->NewBelow.SpacingBasis = Basis;
    self->NewBelow.Operand = Operand;
    self->NewBelow.SpacingUnit = Unit;
    self->NewBelow.DotCvtOperand = CVDots(Operand,Unit);
}

void style__GetNewBelow(self, RefBasis, RefOperand, RefUnit)
struct style *self;
enum style_SpacingValue *RefBasis;
long *RefOperand;
enum style_Unit *RefUnit;
{
    *RefBasis = self->NewBelow.SpacingBasis;
    *RefOperand = self->NewBelow.Operand;
    *RefUnit = self->NewBelow.SpacingUnit;
}

void style__SetNewInterlineSpacing(self, Basis, Operand, Unit)
struct style *self;
enum style_SpacingValue Basis;
long Operand;
enum style_Unit Unit;
{
    self->NewInterlineSpacing.SpacingBasis = Basis;
    self->NewInterlineSpacing.Operand = Operand;
    self->NewInterlineSpacing.SpacingUnit = Unit;
    self->NewInterlineSpacing.DotCvtOperand = CVDots(Operand, Unit);
}

void style__GetNewInterlineSpacing(self, RefBasis, RefOperand, RefUnit)
struct style *self;
enum style_SpacingValue *RefBasis;
long *RefOperand;
enum style_Unit *RefUnit;
{
    *RefBasis = self->NewInterlineSpacing.SpacingBasis;
    *RefOperand = self->NewInterlineSpacing.Operand;
    *RefUnit = self->NewInterlineSpacing.SpacingUnit;
}

void style__SetFontFamily(self, NewFont)
struct style *self;
char *NewFont;
{
    if (self->FontFamily != NULL)
        free(self->FontFamily);

    if (NewFont == NULL || NewFont[0] == '\0')
        self->FontFamily = NULL;
    else 
        self->FontFamily = malloc(strlen(NewFont) + 1),
        strcpy(self->FontFamily, NewFont);
}

void style__GetFontFamily(self, FontName, bufsize)
struct style *self;
char *FontName;
int bufsize;
{
    register char *s;

    if (self->FontFamily == NULL) {
	FontName[0] = '\0';
        return;
    }

    s = self->FontFamily;
    while (--bufsize && *s != '\0')
        *FontName++ = *s++;
    *FontName = '\0';
}

void style__SetFontSize(self, Basis, Operand)
struct style *self;
enum style_FontSize Basis;
long Operand;
{

    self->FontSize.SizeBasis = Basis;
    self->FontSize.Operand = Operand;
}

void style__GetFontSize(self, RefBasis, RefOperand)
struct style *self;
enum style_FontSize *RefBasis;
long *RefOperand;
{
    *RefBasis = self->FontSize.SizeBasis;
    *RefOperand = self->FontSize.Operand;
}

void style__SetFontScript(self, Basis, Operand, Unit)
struct style *self;
enum style_ScriptMovement Basis;
long Operand;
enum style_Unit Unit;
{
    self->FontScript.ScriptBasis = Basis;
    self->FontScript.Operand = Operand;
    self->FontScript.SizeUnit = Unit;
    self->FontScript.DotCvtOperand = CVDots(Operand, Unit);
}

void style__GetFontScript(self,RefBasis, RefOperand, RefUnit)
struct style *self;
enum style_ScriptMovement *RefBasis;
long *RefOperand;
enum style_Unit *RefUnit;
{
    *RefBasis = self->FontScript.ScriptBasis;
    *RefOperand = self->FontScript.Operand;
    *RefUnit = self->FontScript.SizeUnit;
}

void style__SetCounterName(self, NewName)
struct style *self;
char *NewName;
{
    if (self->CounterName != NULL)
	free(self->CounterName);

    if (NewName == NULL || NewName[0] == '\0')
	self->CounterName = NULL;
    else
        self->CounterName = malloc(strlen(NewName) + 1),
	strcpy(self->CounterName, NewName);
}

void style__GetCounterName(self, RetrievedName)
struct style *self;
char *RetrievedName;
{
    if (self->CounterName == NULL)
	RetrievedName[0] = '\0';
    else
	strcpy(RetrievedName, self->CounterName);
}

void style__SetCounterParent(self, NewParent)
struct style *self;
char *NewParent;
{
    if (self->CounterParent != NULL)
	free(self->CounterParent);

    if (NewParent == NULL || NewParent[0] == '\0')
	self->CounterParent = NULL;
    else
	self->CounterParent = malloc(strlen(NewParent) + 1),
	strcpy(self->CounterParent, NewParent);
}

void style__GetCounterParent(self, RetrievedParent)
struct style *self;
char *RetrievedParent;
{
    if (self->CounterParent == NULL)
	RetrievedParent[0] ='\0';
    else
	strcpy(RetrievedParent, self->CounterParent);
}

void style__ClearCounterStyles(self)
struct style *self;
{
    register int i;

    if (self->CounterStyles != NULL) {
	for (i = 0; i < self->NumCounterStyles; i++) free(self->CounterStyles[i]);
	free(self->CounterStyles);
    }
    self->NumCounterStyles = 0;
    self->CounterStyles = NULL;
}

void style__AddCounterStyle(self, NewStyle)
struct style *self;
char *NewStyle;
{
    if (self->NumCounterStyles == 0)
	self->CounterStyles = (char **) malloc(sizeof(char *));
    else
	self->CounterStyles = (char **)
          realloc((char *) self->CounterStyles,
            (self->NumCounterStyles + 1) * sizeof(char *));

    self->CounterStyles[self->NumCounterStyles] = (char *) malloc(strlen(NewStyle) + 1);
    strcpy(self->CounterStyles[self->NumCounterStyles], NewStyle);

    self->NumCounterStyles++;
}

void style__GetCounterStyles(self,RefNumStyles, RefStyleStrings)
struct style *self;
long *RefNumStyles;
char ***RefStyleStrings;
{
    register int i;

    *RefNumStyles = self->NumCounterStyles;
    *RefStyleStrings = (char **) malloc(self->NumCounterStyles * sizeof(char *));

    for (i = 0;i < self->NumCounterStyles; i++) {
        (*RefStyleStrings)[i] = (char *)    malloc(strlen(self->CounterStyles[i]) + 1);
        strcpy((*RefStyleStrings)[i], self->CounterStyles[i]);
    }
}

void style__ClearTabChanges(self)
struct style *self;
{  
    if (self->TabChangeList != NULL)
	free(self->TabChangeList);
    self->NumTabChanges = 0;
    self->TabChangeList = NULL;
}

void style__AddTabChange(self, TabOp, Where, Unit)
struct style *self;
enum style_TabAlignment TabOp;
long Where;
enum style_Unit Unit;
{
    if (TabOp == style_AllClear) {
	self->NumTabChanges = 0;
	if (self->TabChangeList != NULL) {
	    free(self->TabChangeList);
	}
    }
    else {
	long loc = CVDots(Where, Unit);
	long i;

	for (i = 0; i < self->NumTabChanges; i++) {
	    if (self->TabChangeList[i].DotLocation == loc &&
		self->TabChangeList[i].TabOpcode != style_AllClear) {
		self->TabChangeList[i].TabOpcode = TabOp;
		self->TabChangeList[i].Location = Where;
		self->TabChangeList[i].LocationUnit = Unit;
		return;
	    }
	}
    }

    if (self->NumTabChanges == 0)
	self->TabChangeList = (struct tabentry *) malloc(sizeof(struct tabentry));
    else
	self->TabChangeList = (struct tabentry *)
          realloc((char *) self->TabChangeList,
            (self->NumTabChanges + 1) * sizeof(struct tabentry));

    self->TabChangeList[self->NumTabChanges].TabOpcode = TabOp;
    self->TabChangeList[self->NumTabChanges].Location = Where;
    self->TabChangeList[self->NumTabChanges].LocationUnit = Unit;
    self->TabChangeList[self->NumTabChanges].DotLocation = CVDots(Where, Unit);

    self->NumTabChanges++;
}

void style__GetTabChangeList(self, RefNumTabChanges, RefTabChanges)
struct style *self;
long *RefNumTabChanges;
struct tabentry ***RefTabChanges;
{
    register int i;
    struct tabentry **Newptr;

    if ((*RefNumTabChanges = self->NumTabChanges) == 0)
	*RefTabChanges = NULL;
    else {
	Newptr = (struct tabentry **) malloc(self->NumTabChanges * sizeof(struct tabentry *));
	*RefTabChanges = Newptr;

	for (i = 0; i < self->NumTabChanges; i++)
	    *Newptr++ = &(self->TabChangeList[i]);
    }
}

void style__AddAttribute(self,NewAttributeName, NewAttributeValue)
struct style * self;
char * NewAttributeName;
char * NewAttributeValue; {
    struct atom *tmpAtom;
    char * tmpValue;

    tmpAtom = atom_Intern(NewAttributeName);
    if(NewAttributeValue) {
        tmpValue = (char *) malloc(strlen(NewAttributeValue)+1);
	(void) strcpy(tmpValue,NewAttributeValue);
    }
    else tmpValue = NULL;
    if (!self->AdditionalAttributes) self->AdditionalAttributes = namespace_New();
    if(self->AdditionalAttributes)    namespace_SetValue(self->AdditionalAttributes,tmpAtom, (long) tmpValue);
}

void style__RemoveAttribute(self,OldAttributeName)
struct style * self;
char * OldAttributeName;{
    struct atom * tmpAtom;
    long tmpValue;

    if (!self->AdditionalAttributes) return;
    tmpAtom = atom_Intern(OldAttributeName);
    tmpValue = namespace_GetValue(self->AdditionalAttributes, tmpAtom);
    namespace_Unbind(self->AdditionalAttributes, tmpAtom);
    if (tmpValue > 0) free( (char *)tmpValue);

}

char * style__GetAttribute(self,OldAttributeName)
struct style * self;
char * OldAttributeName; {
    struct atom * tmpAtom;
    long tmpValue;

    if (!self->AdditionalAttributes) return NULL;
    tmpAtom = atom_Intern(OldAttributeName);
    tmpValue =  namespace_GetValue(self->AdditionalAttributes, tmpAtom);
     if (tmpValue > 0) return (char *)tmpValue;
     else return NULL;
    
}

boolean style__IsAttribute(self,TestAttributeName)
struct style * self;
char * TestAttributeName;{
    struct atom * tmpAtom;

    if (!self->AdditionalAttributes) return FALSE;
    tmpAtom = atom_Intern(TestAttributeName);
    return namespace_Boundp(self->AdditionalAttributes, tmpAtom, NULL);
}

/* Datastream I/O: Menu fields */

void style__WriteMenu(self, fp)
struct style *self;
FILE *fp;
{
    if (self->menuName != NULL && self->menuName[0] != '\0')
        fprintf(fp, "menu:[%s]", self->menuName);
}

long style__ReadMenu(self, fp)
struct style *self;
FILE *fp;
{
    int c, pos;
    char name[128];

    pos = 0;
    while (1) {
        if ((c = getc(fp)) == EOF)
            return -1;
        if (c == ']')
            break;
        if (pos == sizeof (name) - 1)   /* Truncates */
            continue;
        name[pos++] = c;
    };

    name[pos] = '\0';

    style_SetMenuName(self, name);

    return 0;
}

/* Datastream I/O: attribute fields */

static int style_writeAdditionalAttribute(fileptr, curnamespace, indexvalue)
FILE *fileptr;
struct namespace * curnamespace;
int indexvalue; {
    char * tmpValue;
    char * tmpAttributeName;
    struct atom * tmpAttributeAtom;

    if (namespace_BoundpAt(curnamespace,indexvalue)) {
	tmpAttributeAtom = namespace_NameAt(curnamespace, indexvalue);
	tmpAttributeName = atom_Name(tmpAttributeAtom);
	tmpValue = (char *) namespace_ValueAt(curnamespace,indexvalue);
	fprintf(fileptr, "\nattr:['%s' ", tmpAttributeName);
	if (tmpValue)
	    fprintf(fileptr, "'%s']",tmpValue);
	else fprintf(fileptr, "'']");
    }
    return TRUE;
}

void style__WriteAttr(self, fp)
struct style *self;
FILE *fp;
{
    register int i;
    long numtabchanges, operand;

    enum style_Unit unit;
    enum style_Justification newjust;
    enum style_Scope newscope;
    enum style_Position newposition;
    enum style_MarginValue newmargin;
    enum style_SpacingValue newspacing;
    enum style_FontSize newfsize;
    enum style_ScriptMovement newscript;

    char newfontfamily[200], newName[200], newcparent[200];
    struct tabentry **tabchanges;

    static char *AttrD = "\nattr:[%s %s %s %d]";
    static char *AttrS = "\nattr:[%s %s %s %s]";

    style_GetNewLeftMargin(self, &newmargin, &operand, &unit);
    if (newmargin != style_LeftMargin || operand != 0 || unit != style_Points)
        fprintf(fp, AttrD, AttributeAlias[(int) style_LeftMarginAttr], MarginValueAlias[(int) newmargin], OptypeAlias[(int) unit], operand);

    style_GetNewRightMargin(self, &newmargin, &operand, &unit);
    if (newmargin != style_RightMargin || operand != 0 || unit != style_Points)
        fprintf(fp, AttrD, AttributeAlias[(int) style_RightMarginAttr], MarginValueAlias[(int) newmargin], OptypeAlias[(int) unit], operand);

    style_GetNewTopMargin(self, &newmargin, &operand, &unit);
    if (newmargin != style_TopMargin || operand != 0 || unit != style_Points)
	fprintf(fp, AttrD, AttributeAlias[(int) style_TopMarginAttr], MarginValueAlias[(int) newmargin], OptypeAlias[(int) unit], operand);

    style_GetNewBottomMargin(self, &newmargin, &operand, &unit);
    if (newmargin != style_BottomMargin || operand != 0 || unit != style_Points)
	fprintf(fp, AttrD, AttributeAlias[(int) style_BottomMarginAttr], MarginValueAlias[(int) newmargin], OptypeAlias[(int) unit], operand);

    style_GetNewIndentation(self, &newmargin, &operand, &unit);
    if (newmargin != style_PreviousIndentation || operand != 0 || unit != style_Points)
	fprintf(fp, AttrD, AttributeAlias[(int) style_IndentAttr], MarginValueAlias[(int) newmargin], OptypeAlias[(int) unit], operand);

    style_GetFontScript(self, &newscript, &operand, &unit);
    if (newscript != style_PreviousScriptMovement || operand != 0 || unit != style_Points)
	fprintf(fp, AttrD, AttributeAlias[(int) style_ScriptAttr], ScriptMovementAlias[(int) newscript], OptypeAlias[(int) unit], operand);

    newjust = (enum style_Justification) style_GetJustification(self);
    if (newjust != style_PreviousJustification)
	fprintf(fp, AttrD, AttributeAlias[(int) style_JustificationAttr], JustificationAlias[(int) newjust], OptypeAlias[(int) style_Points], 0);

    style_GetNewInterlineSpacing(self, &newspacing, &operand, &unit);
    if (newspacing != style_InterlineSpacing || operand != 0 || unit != style_Points)
	fprintf(fp, AttrD, AttributeAlias[(int) style_SpacingAttr], SpacingValueAlias[(int) newspacing], OptypeAlias[(int) unit], operand);

    style_GetNewAbove(self, &newspacing, &operand, &unit);
    if (newspacing != style_AboveSpacing || operand != 0 || unit != style_Points)
	fprintf(fp, AttrD, AttributeAlias[(int) style_AboveAttr], SpacingValueAlias[(int) newspacing], OptypeAlias[(int) unit], operand);

    style_GetNewBelow(self, &newspacing, &operand, &unit);
    if (newspacing != style_BelowSpacing || operand != 0 || unit != style_Points)
	fprintf(fp, AttrD, AttributeAlias[(int) style_BelowAttr], SpacingValueAlias[(int) newspacing], OptypeAlias[(int) unit], operand);

    style_GetNewInterparagraphSpacing(self, &newspacing, &operand, &unit);
    if (newspacing != style_InterparagraphSpacing || operand != 0 || unit != style_Points)
	fprintf(fp, AttrD, AttributeAlias[(int) style_SpreadAttr], SpacingValueAlias[(int) newspacing], OptypeAlias[(int) unit], operand);

    style_GetTabChangeList(self, &numtabchanges, &tabchanges);
    if (numtabchanges != 0) {
	struct tabentry **tc;

	for (i = 0, tc = tabchanges; i < numtabchanges; i++, tc++)
	    fprintf(fp, AttrD, AttributeAlias[(int) style_TabsAttr], TabAlignmentAlias[(int) (*tc)->TabOpcode], OptypeAlias[(int) (*tc)->LocationUnit], (*tc)->Location);
	free (tabchanges);   /* Correct a core leak with GetTabChangeList */
    }

    if (self->AddMiscFlags != style_NoFlags) {
	if (style_IsUnderlineAdded(self))
	    fprintf(fp, AttrS, AttributeAlias[(int) style_FlagsAttr], FlagAlias[(int) style_UnderlineIndex], OptypeAlias[(int) style_RawDots], OpcodeAlias[style_OPSET]);

	if (style_IsHiddenAdded(self))
	    fprintf(fp, AttrS, AttributeAlias[(int) style_FlagsAttr], FlagAlias[(int) style_HiddenIndex], OptypeAlias[(int) style_RawDots], OpcodeAlias[style_OPSET]);

	if (style_IsReadOnlyAdded(self))
	    fprintf(fp, AttrS, AttributeAlias[(int) style_FlagsAttr], FlagAlias[(int) style_ReadOnlyIndex], OptypeAlias[(int) style_RawDots], OpcodeAlias[style_OPSET]);

	if (style_IsPassThruAdded(self))
	    fprintf(fp, AttrS, AttributeAlias[(int) style_FlagsAttr], FlagAlias[(int) style_PassThruIndex], OptypeAlias[(int) style_RawDots], OpcodeAlias[style_OPSET]);

	if (style_IsContinueIndentAdded(self))
	    fprintf(fp, AttrS, AttributeAlias[(int) style_FlagsAttr], FlagAlias[(int) style_ContinueIndentIndex], OptypeAlias[(int) style_RawDots], OpcodeAlias[style_OPSET]);

	if (style_IsHingeAdded(self))
	    fprintf(fp, AttrS, AttributeAlias[(int) style_FlagsAttr], FlagAlias[(int) style_HingeIndex], OptypeAlias[(int) style_RawDots], OpcodeAlias[style_OPSET]);

	if (style_IsNewPageAdded(self))
	    fprintf(fp, AttrS, AttributeAlias[(int) style_FlagsAttr], FlagAlias[(int) style_NewPageIndex], OptypeAlias[(int) style_RawDots], OpcodeAlias[style_OPSET]);

	if (style_IsChangeBarAdded(self))
	    fprintf(fp, AttrS, AttributeAlias[(int) style_FlagsAttr], FlagAlias[(int) style_ChangeBarIndex], OptypeAlias[(int) style_RawDots], OpcodeAlias[style_OPSET]);

	if (style_IsOverBarAdded(self))
	    fprintf(fp, AttrS, AttributeAlias[(int) style_FlagsAttr], FlagAlias[(int) style_OverBarIndex], OptypeAlias[(int) style_RawDots], OpcodeAlias[style_OPSET]);

	if (style_IsDottedBoxAdded(self))
	    fprintf(fp, AttrS, AttributeAlias[(int) style_FlagsAttr], FlagAlias[(int) style_DottedBoxIndex], OptypeAlias[(int) style_RawDots], OpcodeAlias[style_OPSET]);

	if (style_IsStrikeThroughAdded(self))
	    fprintf(fp, AttrS, AttributeAlias[(int) style_FlagsAttr], FlagAlias[(int) style_StrikeThroughIndex], OptypeAlias[(int) style_RawDots], OpcodeAlias[style_OPSET]);

	if (style_IsNoWrapAdded(self))
	    fprintf(fp, AttrS, AttributeAlias[(int) style_FlagsAttr], FlagAlias[(int) style_NoWrapIndex], OptypeAlias[(int) style_RawDots], OpcodeAlias[style_OPSET]);

	if (style_TestAddFlag(self, style_NoFill))
	    fprintf(fp, AttrS, AttributeAlias[(int) style_FlagsAttr], FlagAlias[(int) style_NoFillIndex], OptypeAlias[(int) style_RawDots], OpcodeAlias[style_OPSET]);

	if (style_TestAddFlag(self, style_KeepPriorNL))
	    fprintf(fp, AttrS, AttributeAlias[(int) style_FlagsAttr], FlagAlias[(int) style_KeepPriorNLIndex], OptypeAlias[(int) style_RawDots], OpcodeAlias[style_OPSET]);

	if (style_TestAddFlag(self, style_KeepNextNL))
	    fprintf(fp, AttrS, AttributeAlias[(int) style_FlagsAttr], FlagAlias[(int) style_KeepNextNLIndex], OptypeAlias[(int) style_RawDots], OpcodeAlias[style_OPSET]);

	if (style_TestAddFlag(self, style_TabsCharacters))
	    fprintf(fp, AttrS, AttributeAlias[(int) style_FlagsAttr], FlagAlias[(int) style_TabsCharactersIndex], OptypeAlias[(int) style_RawDots], OpcodeAlias[style_OPSET]);

    }

    if (self->OutMiscFlags != ~ style_NoFlags) {
	if (style_IsUnderlineRemoved(self))
	    fprintf(fp, AttrS, AttributeAlias[(int) style_FlagsAttr], FlagAlias[(int) style_UnderlineIndex], OptypeAlias[(int) style_RawDots], OpcodeAlias[style_OPCLEAR]);

	if (style_IsHiddenRemoved(self))
	    fprintf(fp, AttrS, AttributeAlias[(int) style_FlagsAttr], FlagAlias[(int) style_HiddenIndex], OptypeAlias[(int) style_RawDots], OpcodeAlias[style_OPCLEAR]);

	if (style_IsReadOnlyRemoved(self))
	    fprintf(fp, AttrS, AttributeAlias[(int) style_FlagsAttr], FlagAlias[(int) style_ReadOnlyIndex], OptypeAlias[(int) style_RawDots], OpcodeAlias[style_OPCLEAR]);

	if (style_IsPassThruRemoved(self))
	    fprintf(fp, AttrS, AttributeAlias[(int) style_FlagsAttr], FlagAlias[(int) style_PassThruIndex], OptypeAlias[(int) style_RawDots], OpcodeAlias[style_OPCLEAR]);

	if (style_IsContinueIndentRemoved(self))
	    fprintf(fp, AttrS, AttributeAlias[(int) style_FlagsAttr], FlagAlias[(int) style_ContinueIndentIndex], OptypeAlias[(int) style_RawDots], OpcodeAlias[style_OPCLEAR]);

	if (style_IsHingeRemoved(self))
	    fprintf(fp, AttrS, AttributeAlias[(int) style_FlagsAttr], FlagAlias[(int) style_HingeIndex], OptypeAlias[(int) style_RawDots], OpcodeAlias[style_OPCLEAR]);

	if (style_IsNewPageRemoved(self))
	    fprintf(fp, AttrS, AttributeAlias[(int) style_FlagsAttr], FlagAlias[(int) style_NewPageIndex], OptypeAlias[(int) style_RawDots], OpcodeAlias[style_OPCLEAR]);

	if (style_IsUnderlineRemoved(self))
	    fprintf(fp, AttrS, AttributeAlias[(int) style_FlagsAttr], FlagAlias[(int) style_UnderlineIndex], OptypeAlias[(int) style_RawDots], OpcodeAlias[style_OPCLEAR]);

	if (style_IsChangeBarRemoved(self))
	    fprintf(fp, AttrS, AttributeAlias[(int) style_FlagsAttr], FlagAlias[(int) style_ChangeBarIndex], OptypeAlias[(int) style_RawDots], OpcodeAlias[style_OPCLEAR]);

	if (style_IsOverBarRemoved(self))
	    fprintf(fp, AttrS, AttributeAlias[(int) style_FlagsAttr], FlagAlias[(int) style_OverBarIndex], OptypeAlias[(int) style_RawDots], OpcodeAlias[style_OPCLEAR]);

	if (style_IsDottedBoxRemoved(self))
	    fprintf(fp, AttrS, AttributeAlias[(int) style_FlagsAttr], FlagAlias[(int) style_DottedBoxIndex], OptypeAlias[(int) style_RawDots], OpcodeAlias[style_OPCLEAR]);

	if (style_IsStrikeThroughRemoved(self))
	    fprintf(fp, AttrS, AttributeAlias[(int) style_FlagsAttr], FlagAlias[(int) style_StrikeThroughIndex], OptypeAlias[(int) style_RawDots], OpcodeAlias[style_OPCLEAR]);

	if (style_IsNoWrapRemoved(self))
	    fprintf(fp, AttrS, AttributeAlias[(int) style_FlagsAttr], FlagAlias[(int) style_NoWrapIndex], OptypeAlias[(int) style_RawDots], OpcodeAlias[style_OPCLEAR]);

	if (style_TestRemoveFlag(self, style_NoFill))
	    fprintf(fp, AttrS, AttributeAlias[(int) style_FlagsAttr], FlagAlias[(int) style_NoFillIndex], OptypeAlias[(int) style_RawDots], OpcodeAlias[style_OPCLEAR]);

	if (style_TestRemoveFlag(self, style_KeepPriorNL))
	    fprintf(fp, AttrS, AttributeAlias[(int) style_FlagsAttr], FlagAlias[(int) style_KeepPriorNLIndex], OptypeAlias[(int) style_RawDots], OpcodeAlias[style_OPCLEAR]);

	if (style_TestRemoveFlag(self, style_KeepNextNL))
	    fprintf(fp, AttrS, AttributeAlias[(int) style_FlagsAttr], FlagAlias[(int) style_KeepNextNLIndex], OptypeAlias[(int) style_RawDots], OpcodeAlias[style_OPCLEAR]);

	if (style_TestRemoveFlag(self, style_TabsCharacters))
	    fprintf(fp, AttrS, AttributeAlias[(int) style_FlagsAttr], FlagAlias[(int) style_TabsCharactersIndex], OptypeAlias[(int) style_RawDots], OpcodeAlias[style_OPCLEAR]);
    }
    if (style_GetAddedFontFaces(self)) {
	if (style_IsAnyAddedFontFace(self, fontdesc_Bold))
	    fprintf(fp, AttrS, AttributeAlias[(int) style_FontFaceAttr], FaceStyleAlias[(int) style_BoldIndex], OptypeAlias[(int) style_RawDots], OpcodeAlias[style_OPSET]);

	if (style_IsAnyAddedFontFace(self, fontdesc_Italic))
	    fprintf(fp, AttrS, AttributeAlias[(int) style_FontFaceAttr], FaceStyleAlias[(int) style_ItalicIndex], OptypeAlias[(int) style_RawDots], OpcodeAlias[style_OPSET]);

	if (style_IsAnyAddedFontFace(self, fontdesc_Shadow))
	    fprintf(fp, AttrS, AttributeAlias[(int) style_FontFaceAttr], FaceStyleAlias[(int) style_ShadowIndex], OptypeAlias[(int) style_RawDots], OpcodeAlias[style_OPSET]);

	if (style_IsAnyAddedFontFace(self, fontdesc_Fixed))
	    fprintf(fp, AttrS, AttributeAlias[(int) style_FontFaceAttr], FaceStyleAlias[(int) style_FixedFaceIndex], OptypeAlias[(int) style_RawDots], OpcodeAlias[style_OPSET]);

	if (style_IsAnyAddedFontFace(self, fontdesc_Outline))
	    fprintf(fp, AttrS, AttributeAlias[(int) style_FontFaceAttr], FaceStyleAlias[(int) style_OutlineIndex], OptypeAlias[(int) style_RawDots], OpcodeAlias[style_OPSET]);

	if (style_IsAnyAddedFontFace(self, fontdesc_Thin))
	    fprintf(fp, AttrS, AttributeAlias[(int) style_FontFaceAttr], FaceStyleAlias[(int) style_ThinIndex], OptypeAlias[(int) style_RawDots], OpcodeAlias[style_OPSET]);

	if (style_IsAnyAddedFontFace(self, fontdesc_Black))
	    fprintf(fp, AttrS, AttributeAlias[(int) style_FontFaceAttr], FaceStyleAlias[(int) style_BlackIndex], OptypeAlias[(int) style_RawDots], OpcodeAlias[style_OPSET]);

	if (style_IsAnyAddedFontFace(self, fontdesc_Medium))
	    fprintf(fp, AttrS, AttributeAlias[(int) style_FontFaceAttr], FaceStyleAlias[(int) style_MediumIndex], OptypeAlias[(int) style_RawDots], OpcodeAlias[style_OPSET]);

	if (style_IsAnyAddedFontFace(self, fontdesc_Heavy))
	    fprintf(fp, AttrS, AttributeAlias[(int) style_FontFaceAttr], FaceStyleAlias[(int) style_HeavyIndex], OptypeAlias[(int) style_RawDots], OpcodeAlias[style_OPSET]);

	if (style_IsAnyAddedFontFace(self, fontdesc_Condense))
	    fprintf(fp, AttrS, AttributeAlias[(int) style_FontFaceAttr], FaceStyleAlias[(int) style_CondenseIndex], OptypeAlias[(int) style_RawDots], OpcodeAlias[style_OPSET]);
    }
    if (style_GetRemovedFontFaces(self)){
	if (style_IsAnyRemovedFontFace(self, fontdesc_Bold))
	    fprintf(fp, AttrS, AttributeAlias[(int) style_FontFaceAttr], FaceStyleAlias[(int) style_BoldIndex], OptypeAlias[(int) style_RawDots], OpcodeAlias[style_OPCLEAR]);

	if (style_IsAnyRemovedFontFace(self, fontdesc_Italic))
	    fprintf(fp, AttrS, AttributeAlias[(int) style_FontFaceAttr], FaceStyleAlias[(int) style_ItalicIndex], OptypeAlias[(int) style_RawDots], OpcodeAlias[style_OPCLEAR]);

	if (style_IsAnyRemovedFontFace(self, fontdesc_Shadow))
	    fprintf(fp, AttrS, AttributeAlias[(int) style_FontFaceAttr], FaceStyleAlias[(int) style_ShadowIndex], OptypeAlias[(int) style_RawDots], OpcodeAlias[style_OPCLEAR]);

	if (style_IsAnyRemovedFontFace(self, fontdesc_Fixed))
	    fprintf(fp, AttrS, AttributeAlias[(int) style_FontFaceAttr], FaceStyleAlias[(int) style_FixedFaceIndex], OptypeAlias[(int) style_RawDots], OpcodeAlias[style_OPCLEAR]);

	if (style_IsAnyRemovedFontFace(self, fontdesc_Outline))
	    fprintf(fp, AttrS, AttributeAlias[(int) style_FontFaceAttr], FaceStyleAlias[(int) style_OutlineIndex], OptypeAlias[(int) style_RawDots], OpcodeAlias[style_OPCLEAR]);

	if (style_IsAnyRemovedFontFace(self, fontdesc_Thin))
	    fprintf(fp, AttrS, AttributeAlias[(int) style_FontFaceAttr], FaceStyleAlias[(int) style_ThinIndex], OptypeAlias[(int) style_RawDots], OpcodeAlias[style_OPCLEAR]);

	if (style_IsAnyRemovedFontFace(self, fontdesc_Black))
	    fprintf(fp, AttrS, AttributeAlias[(int) style_FontFaceAttr], FaceStyleAlias[(int) style_BlackIndex], OptypeAlias[(int) style_RawDots], OpcodeAlias[style_OPCLEAR]);

	if (style_IsAnyRemovedFontFace(self, fontdesc_Medium))
	    fprintf(fp, AttrS, AttributeAlias[(int) style_FontFaceAttr], FaceStyleAlias[(int) style_MediumIndex], OptypeAlias[(int) style_RawDots], OpcodeAlias[style_OPCLEAR]);

	if (style_IsAnyRemovedFontFace(self, fontdesc_Heavy))
	    fprintf(fp, AttrS, AttributeAlias[(int) style_FontFaceAttr], FaceStyleAlias[(int) style_HeavyIndex], OptypeAlias[(int) style_RawDots], OpcodeAlias[style_OPCLEAR]);

	if (style_IsAnyRemovedFontFace(self, fontdesc_Condense))
	    fprintf(fp, AttrS, AttributeAlias[(int) style_FontFaceAttr], FaceStyleAlias[(int) style_CondenseIndex], OptypeAlias[(int) style_RawDots], OpcodeAlias[style_OPCLEAR]);
    }

    style_GetFontFamily(self,
      newfontfamily, sizeof (newfontfamily));
    if (newfontfamily[0] != '\0')
	fprintf(fp, AttrD, AttributeAlias[(int) style_FontFamilyAttr], newfontfamily, OptypeAlias[(int) style_RawDots], 0);

    style_GetFontSize(self, &newfsize, &operand);
    if (newfsize != style_PreviousFontSize || operand != 0)
	fprintf(fp, AttrD, AttributeAlias[(int) style_FontSizeAttr], FontSizeAlias[(int) newfsize], OptypeAlias[(int) style_Points], operand);

    style_GetCounterName(self, newName);
    if (newName[0] != '\0') {
	fprintf(fp, AttrD, AttributeAlias[(int) style_CounterNameAttr], newName, OptypeAlias[(int) style_RawDots], 0);

	style_GetCounterParent(self, newcparent);
	if (newcparent[0] != '\0')
	    fprintf(fp, AttrD, AttributeAlias[(int) style_CounterParentAttr], newcparent, OptypeAlias[(int) style_RawDots], 0);

	newscope = (enum style_Scope) style_GetCounterScope(self);
	if (newscope != style_GlobalScope)
	    fprintf(fp, AttrD, AttributeAlias[(int) style_CounterScopeAttr], ScopeAlias[(int) newscope], OptypeAlias[(int) style_Points], 0);

	newposition = (enum style_Position) style_GetCounterPosition(self);
	if (newposition != style_EnvironmentCtr)
	    fprintf(fp, AttrD, AttributeAlias[(int) style_CounterPositionAttr], PositionAlias[(int) newposition], OptypeAlias[(int) style_Points], 0);

	operand = style_GetCounterInitialValue(self);
	if (operand != 0)
	    fprintf(fp, AttrD, AttributeAlias[(int) style_CounterInitialValueAttr], "none", OptypeAlias[(int) style_Points], operand);
    }

    if (self->AdditionalAttributes) {
	(void) namespace_Enumerate(self->AdditionalAttributes, style_writeAdditionalAttribute, (long) fp);
    }
}

long style__ReadAttr(self, fp)
struct style *self;
FILE *fp;
{
    register int j;
    int c, operand, basis, unit, newface;
    unsigned int flag;

    enum style_Attribute attr;
    enum style_MarginValue newmargin;
    enum style_SpacingValue newspacing;
    enum style_TabAlignment newtabalign;
    enum style_Justification newjust;
    enum style_Scope newscope;
    enum style_Position newposition;
    enum style_FontSize newfsize;
    enum style_ScriptMovement newscript;

    char Attribute[50], Basis[256], Unit[25], Operand[400];

    do {
        if ((c = getc(fp)) == EOF)
            return -1;
    } while (iswhite(c));

    if (c == '\'') {
        /* Temporary hack for generic attributes. */
        j = 0;
        while (1) {
            if ((c = getc(fp)) == EOF)
                return -1;
            if (c == '\'')
                break;
            Attribute[j++] = c;
            if (j == sizeof (Attribute))
                goto bomb;
        }
        Attribute[j] = '\0';
        do {
            if ((c = getc(fp)) == EOF)
                return -1;
        } while (iswhite(c));
        if (c == ']') {
            style_AddAttribute(self, Attribute, NULL);
            return 0;
        }
        if (c != '\'')
            goto bomb;
        j = 0;
        while (1) {
            if ((c = getc(fp)) == EOF)
                return -1;
            if (c == '\'')
                break;
            Operand[j++] = c;
            if (j == sizeof (Operand))
                goto bomb;
        }
        Operand[j] = '\0';
        do {
            if ((c = getc(fp)) == EOF)
                return -1;
        } while (iswhite(c));
        if (c != ']')
            goto bomb;
        style_AddAttribute(self, Attribute, Operand);
        return 0;
    }

    j = 0;
    do {
        if (c == ']')
            return -1;
        Attribute[j++] = c;
        if (j == sizeof (Attribute))
            goto bomb;
        if ((c = getc(fp)) == EOF)
            return -1;
    } while (! iswhite(c));
    Attribute[j] = '\0';

    do {
        if ((c = getc(fp)) == EOF)
            return -1;
    } while (iswhite(c));

    j = 0;
    do {
        if (c == ']')
            return -1;
        Basis[j++] = c;
        if (j == sizeof (Basis))
            goto bomb;
        if ((c = getc(fp)) == EOF)
            return -1;
    } while (! iswhite(c));
    Basis[j] = '\0';

    do {
        if ((c = getc(fp)) == EOF)
            return -1;
    } while (iswhite(c));

    j = 0;
    do {
        if (c == ']')
            return -1;
        Unit[j++] = c;
        if (j == sizeof (Unit))
            goto bomb;
        if ((c = getc(fp)) == EOF)
            return -1;
    } while (! iswhite(c));
    Unit[j] = '\0';

    do {
        if ((c = getc(fp)) == EOF)
            return -1;
    } while(iswhite(c));

    if (c == ']')
        return -1;

    j = 0;
    do {
        if (c == ']')
            break;
        Operand[j++] = c;
        if (j == sizeof (Operand))
            goto bomb;
        if ((c = getc(fp)) == EOF)
            return -1;
    } while (! iswhite(c));
    Operand[j] = '\0';

    while (iswhite(c))
        if ((c = getc(fp)) == EOF)
            return -1;

    if (c != ']')
        goto bomb;

    for (j = 0; AttributeAlias[j] != NULL; j++)
        if (strcmp(Attribute, AttributeAlias[j]) == 0)
            break;
    if (AttributeAlias[j] == NULL)
        return -1;

    attr = (enum style_Attribute) j;

    switch (attr) {
	case style_LeftMarginAttr:
	case style_RightMarginAttr:
	case style_IndentAttr:
	case style_TopMarginAttr:
	case style_BottomMarginAttr:

            for (basis = 0; MarginValueAlias[basis] != NULL; basis++)
		if (strcmp(Basis, MarginValueAlias[basis]) == 0)
                    break;
            if (MarginValueAlias[basis] == NULL)
                return -1;

	    newmargin = (enum style_MarginValue) basis;

            for (unit = 0; OptypeAlias[unit] != NULL; unit++)
		if (strcmp(Unit, OptypeAlias[unit]) == 0)
                    break;
            if (OptypeAlias[unit] == NULL)
                return -1;

            operand = atoi(Operand);

            switch (attr) {
                case style_LeftMarginAttr:
                    style_SetNewLeftMargin(self, newmargin, operand, unit);
		    break;
		case style_RightMarginAttr:
		    style_SetNewRightMargin(self, newmargin, operand, unit);
		    break;
		case style_IndentAttr:
		    style_SetNewIndentation(self, newmargin, operand, unit);
		    break;
		case style_TopMarginAttr:
                    style_SetNewTopMargin(self, newmargin, operand, unit);
		    break;
		case style_BottomMarginAttr:
                    style_SetNewBottomMargin(self, newmargin, operand, unit);
                    break;
	    }
	    break;

	case style_ScriptAttr:

            for (basis = 0; ScriptMovementAlias[basis] != NULL; basis++)
		if (strcmp(Basis, ScriptMovementAlias[basis]) == 0)
                    break;
            if (ScriptMovementAlias[basis] == NULL)
                return -1;

	    newscript = (enum style_ScriptMovement) basis;

            for (unit = 0; OptypeAlias[unit] != NULL; unit++)
		if (strcmp(Unit, OptypeAlias[unit]) == 0)
                    break;
            if (OptypeAlias[unit] == NULL)
                return -1;

	    operand = atoi(Operand);

            /* BE1 had an inconsistent representation, giving point size */
            /* in fixed-point form for scripting.  The following warns */
            /* about and fixes outrageous scripting values (>1 page) */

            if (unit == (int) style_Points &&
                (operand > 11*72 || operand < -11*72)) {
                operand >>= 16;
                fprintf(stderr, "Found BE1-format super/subscripting error.\n");
                fprintf(stderr, "Please save the file to correct this problem.\n");
                fprintf(stderr, "If the error persists, send mail to Advisor.\n");
                fprintf(stderr, "Include the name of the bad file.\n");
            }

	    style_SetFontScript(self, newscript, operand, unit);
	    break;

	case style_JustificationAttr:

            for (basis = 0; JustificationAlias[basis] != NULL; basis++)
		if (strcmp(Basis, JustificationAlias[basis]) == 0)
                    break;
            if (JustificationAlias[basis] == NULL)
                return -1;

	    newjust = (enum style_Justification) basis;

	    style_SetJustification(self, newjust);
	    break;

	case style_SpacingAttr:
	case style_SpreadAttr:
	case style_AboveAttr:
	case style_BelowAttr:

            for (basis = 0; SpacingValueAlias[basis] != NULL; basis++)
		if (strcmp(Basis, SpacingValueAlias[basis]) == 0)
                    break;
            if (SpacingValueAlias[basis] == NULL)
                return -1;

	    newspacing = (enum style_SpacingValue) basis;

            for (unit = 0; OptypeAlias[unit] != NULL; unit++)
		if (strcmp(Unit, OptypeAlias[unit]) == 0)
                    break;
            if (OptypeAlias[unit] == NULL)
                return -1;

            operand = atoi(Operand);

	    switch (attr) {
		case style_SpacingAttr:
		    style_SetNewInterlineSpacing(self, newspacing, operand, unit);
		    break;
		case style_SpreadAttr:
		    style_SetNewInterparagraphSpacing(self, newspacing, operand, unit);
		    break;
		case style_AboveAttr:
		    style_SetNewAbove(self, newspacing, operand, unit);
		    break;
		case style_BelowAttr:
		    style_SetNewBelow(self, newspacing, operand, unit);
		    break;
		default:
		    break;
	    }
	    break;

	case style_TabsAttr:

            for (basis = 0; TabAlignmentAlias[basis] != NULL; basis++)
		if (strcmp(Basis, TabAlignmentAlias[basis]) == 0)
                    break;
            if (TabAlignmentAlias[basis] == NULL)
                return -1;

	    newtabalign = (enum style_TabAlignment) basis;

            for (unit = 0; OptypeAlias[unit] != NULL; unit++)
		if (strcmp(Unit, OptypeAlias[unit]) == 0)
                    break;
            if (OptypeAlias[unit] == NULL)
                return -1;

            operand = atoi(Operand);

	    style_AddTabChange(self, newtabalign, operand, unit);
	    break;

	case style_FlagsAttr:

            for (basis = 0; FlagAlias[basis] != NULL; basis++)
		if (strcmp(Basis, FlagAlias[basis]) == 0)
                    break;
            if (FlagAlias[basis] == NULL)
                return -1;

	    flag = 1 << basis;

            for (operand = 0; OpcodeAlias[operand] != NULL; operand++)
		if (strcmp(Operand, OpcodeAlias[operand]) == 0)
                    break;
            if (OpcodeAlias[operand] == NULL)
                return -1;

	    switch (flag) {
		case style_Underline:
		    if (operand == style_OPSET) style_AddUnderline(self);
		    if (operand == style_OPCLEAR) style_RemoveUnderline(self);
		    break;
		case style_Hidden:
		    if (operand == style_OPSET) style_AddHidden(self);
		    if (operand == style_OPCLEAR) style_RemoveHidden(self);
		    break;
		case style_ReadOnly:
		    if (operand == style_OPSET) style_AddReadOnly(self);
		    if (operand == style_OPCLEAR) style_RemoveReadOnly(self);
		    break;
		case style_PassThru:
		    if (operand == style_OPSET) style_AddPassThru(self);
		    if (operand == style_OPCLEAR) style_RemovePassThru(self);
		    break;
		case style_ContinueIndent:
		    if (operand == style_OPSET) self->AddMiscFlags |= style_ContinueIndent;
		    if (operand==style_OPCLEAR) self->OutMiscFlags |= style_ContinueIndent;
		    break;
		case style_Hinge:
		    if (operand == style_OPSET) style_AddHinge(self);
		    if (operand == style_OPCLEAR) style_RemoveHinge(self);
		    break;
		case style_NewPage:
		    if (operand == style_OPSET) style_AddNewPage(self);
		    if (operand == style_OPCLEAR) style_RemoveNewPage(self);
		    break;
		case style_ChangeBar:
		    if (operand == style_OPSET) style_AddChangeBar(self);
		    if (operand == style_OPCLEAR) style_RemoveChangeBar(self);
		    break;
		case style_OverBar:
		    if (operand == style_OPSET) style_AddOverBar(self);
		    if (operand == style_OPCLEAR) style_RemoveOverBar(self);
		    break;
		case style_DottedBox:
		    if (operand == style_OPSET) style_AddDottedBox(self);
		    if (operand == style_OPCLEAR) style_RemoveDottedBox(self);
		    break;
		case style_StrikeThrough:
		    if (operand == style_OPSET) style_AddStrikeThrough(self);
		    if (operand == style_OPCLEAR) style_RemoveStrikeThrough(self);
		    break;
		case style_NoWrap:
		    if (operand == style_OPSET) style_AddNoWrap(self);
		    if (operand == style_OPCLEAR) style_RemoveNoWrap(self);
		    break;
		case style_NoFill:
		    if (operand == style_OPSET) 
			style_AddFlag(self, style_NoFill);
		    if (operand == style_OPCLEAR) 
			style_RemoveFlag(self, style_NoFill);
		    break;
		case style_KeepPriorNL:
		    if (operand == style_OPSET) 
			style_AddFlag(self, style_KeepPriorNL);
		    if (operand == style_OPCLEAR) 
			style_RemoveFlag(self, style_KeepPriorNL);
		    break;
		case style_KeepNextNL:
		    if (operand == style_OPSET) 
			style_AddFlag(self, style_KeepNextNL);
		    if (operand == style_OPCLEAR) 
			style_RemoveFlag(self, style_KeepNextNL);
		    break;
		case style_TabsCharacters:
		    if (operand == style_OPSET) 
			style_AddFlag(self, style_TabsCharacters);
		    if (operand == style_OPCLEAR) 
			style_RemoveFlag(self, style_TabsCharacters);
		    break;
		default:
		    return -1;
	    }
	    break;

	case style_FontFaceAttr:

            for (basis = 0; FaceStyleAlias[basis] != NULL; basis++)
		if (strcmp(Basis, FaceStyleAlias[basis]) == 0)
                    break;
            if (FaceStyleAlias[basis] == NULL)
                return -1;

	    flag = 1 << basis;
	    newface = (int) flag;

            for (operand = 0; OpcodeAlias[operand] != NULL; operand++)
		if (strcmp(Operand, OpcodeAlias[operand]) == 0)
                    break;
            if (OpcodeAlias[operand] == NULL)
                return -1;

	    if (operand == style_OPSET) style_AddNewFontFace(self, newface);
	    if (operand == style_OPCLEAR) style_RemoveOldFontFace(self, newface);
	    break;

	case style_FontFamilyAttr:

	    style_SetFontFamily(self, Basis);
	    break;

	case style_FontSizeAttr:

            for (basis = 0; FontSizeAlias[basis] != NULL; basis++)
		if (strcmp(Basis, FontSizeAlias[basis]) == 0)
                    break;
            if (FontSizeAlias[basis] == NULL)
                return -1;

	    newfsize = (enum style_FontSize) basis;

            operand = atoi(Operand);

	    style_SetFontSize(self, newfsize, operand);
	    break;

	case style_CounterNameAttr:

	    style_SetCounterName(self, Basis);
	    break;

	case style_CounterParentAttr:

	    style_SetCounterParent(self, Basis);
	    break;

	case style_CounterScopeAttr:

            for (basis = 0; ScopeAlias[basis] != NULL; basis++)
		if (strcmp(Basis, ScopeAlias[basis]) == 0)
                    break;
            if (ScopeAlias[basis] == NULL)
                return -1;

	    newscope = (enum style_Scope) basis;

	    style_SetCounterScope(self, newscope);
	    break;

	case style_CounterPositionAttr:

            for (basis = 0; PositionAlias[basis] != NULL; basis++)
		if (strcmp(Basis, PositionAlias[basis]) == 0)
                    break;
            if (PositionAlias[basis] == NULL)
                return -1;

	    newposition = (enum style_Position) basis;

	    style_SetCounterPosition(self, newposition);
	    break;

	case style_CounterInitialValueAttr:

            operand = atoi(Operand);
	    style_SetCounterInitialValue(self, operand);
	    break;

	default:
            return -1;
    }

    return 0;

bomb:
    while ((c = getc(fp)) != EOF && c != ']')
        ;
    return -1;
}

/* Device fields, for backward compat, eventually will be removed. */
/* Not meant to be robust; seeks out attr:[] fields nested in device:[]. */

static long ReadDevice(self, fp)
struct style *self; FILE *fp; {
    int bra = 0;
    while (1) {
        switch (getc(fp)) {
        case '[': bra++; break;
        case ']': if (--bra < 0) return 0; break;
        case 'a':
            if (getc(fp) == 't' && getc(fp) == 't' && getc(fp) == 'r'
                 && getc(fp) == ':' && getc(fp) == '[')
                style_ReadAttr(self, fp);
        }
    }
}

/*
 * style_Write assumes that "\define{stylename\n" has
 * already been written.  Does not write trailing "}" and newline.
 */

void style__Write(self, fp)
struct style *self;
FILE *fp;
{
    style_WriteMenu(self, fp);
    style_WriteAttr(self, fp);
}

/*
 * style_Read assumes that the string "\define{name" has already been
 * read.  The '\n' after the "\define{name" may have or have not been read.
 * Handles menu:[] and attr:[] entries until it reaches the "}" that matches
 * the \define.  The brace, and the (mandatory) following newline, are also read.
 *
 * On a parsing error, the remainder of the style is read up to and
 * including the closing brace and newline, to facilitate the possibility
 * of ignoring the bad style definition.
 */

long style__Read(self, fp)
struct style *self;
FILE *fp;
{
    int c, pos;
    char token[17];

    style_Reset(self);

    while (1) {
        do {
            if ((c = getc(fp)) == EOF)
                return -1;
        } while (iswhite(c));

        if (c == '}') {
            if ((c = getc(fp)) == EOF || c != '\n')
                return -1;
            return 0;
        }

        pos = 0;

        if (c == ':')
            goto bomb;

        do {
            if (c == ':')
                break;
            token[pos++] = c;
            if (pos == sizeof (token))
                goto bomb;
            if ((c = getc(fp)) == EOF)
                return -1;
        } while (! iswhite(c));

        token[pos] = '\0';

        while (iswhite(c))
            if ((c = getc(fp)) == EOF)
                return -1;

        if (c != ':')
            goto bomb;

        do {
            if ((c = getc(fp)) == EOF)
                return -1;
        } while (iswhite(c));

        if (c != '[')
            goto bomb;

        if (strcmp(token, "menu") == 0) {
            if (style_ReadMenu(self, fp) < 0)
                goto bomb;
        } else if (strcmp(token, "attr") == 0) {
            if (style_ReadAttr(self, fp) < 0)
                goto bomb;
        } else if (strcmp(token, "device") == 0) {
            if (ReadDevice(self, fp) < 0)
                goto bomb;
        } else
            goto bomb;
    }

bomb:
    while ((c = getc(fp)) != EOF)
        if (c == '}') {
            c = getc(fp);   /* Eat the following newline */
            break;
        }

    return -1;
}

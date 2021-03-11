/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */

/*
	$Disclaimer: 
*Permission to use, copy, modify, and distribute this software and its 
*documentation for any purpose is hereby granted without fee, 
*provided that the above copyright notice appear in all copies and that 
*both that copyright notice, this permission notice, and the following 
*disclaimer appear in supporting documentation, and that the names of 
*IBM, Carnegie Mellon University, and other copyright holders, not be 
*used in advertising or publicity pertaining to distribution of the software 
*without specific, written prior permission.
*
*IBM, CARNEGIE MELLON UNIVERSITY, AND THE OTHER COPYRIGHT HOLDERS 
*DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
*ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.  IN NO EVENT 
*SHALL IBM, CARNEGIE MELLON UNIVERSITY, OR ANY OTHER COPYRIGHT HOLDER 
*BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
*DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, 
*WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS 
*ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
*OF THIS SOFTWARE.
* $
*/


 

#define style_VERSION   1

/**************************************************************
 * Style Definitions
 **************************************************************/

#define style_OPNOOP        0
#define style_OPCOPY        1
#define style_OPADD         2
#define style_OPSET         3
#define style_OPCLEAR       4

enum style_Attribute {
    style_LeftMarginAttr=0,
    style_RightMarginAttr=1,
    style_IndentAttr=2,
    style_ScriptAttr=3,
    style_JustificationAttr=4,
    style_SpacingAttr=5,
    style_SpreadAttr=6,
    style_TabsAttr=7,
    style_FlagsAttr=8,
    style_FunctionAttr=9,
    style_FontFamilyAttr=11,
    style_FontFaceAttr=12,
    style_FontSizeAttr=14,
    style_TopMarginAttr=15,
    style_BottomMarginAttr=16,
    style_AboveAttr=17,
    style_BelowAttr=18,
    style_CounterNameAttr=19,
    style_CounterParentAttr=20,
    style_CounterScopeAttr=21,
    style_CounterPositionAttr=22,
    style_CounterInitialValueAttr=23
};

enum style_Unit {
    style_Inches=0,
    style_CM=1,
    style_Points=2,
    style_Ems=3,
    style_Lines=4,
    style_RawDots=6
};

enum style_TabAlignment {
    style_LeftAligned=1,
    style_RightAligned=2,
    style_CenteredOnTab=3,
    style_CenteredBetweenTab=4,
    style_TabDivide=5,
    style_CharAligned=6,
    style_TabClear=7,
    style_AllClear=8
};

struct tabentry {
    enum style_TabAlignment TabOpcode;
    long Location;
    enum style_Unit LocationUnit;
    long DotLocation;
};

enum style_Justification {
    style_PreviousJustification,
    style_LeftJustified,
    style_RightJustified,
    style_Centered,
    style_LeftAndRightJustified,
    style_LeftThenRightJustified
};

enum style_Scope {
    style_GlobalScope,
    style_LocalScope
};

enum style_Position {
    style_EnvironmentCtr,
    style_ParagraphCtr,
    style_LineCtr
};

enum style_MarginValue {
    style_ConstantMargin,
    style_LeftMargin,
    style_LeftEdge,
    style_RightMargin,
    style_RightEdge,
    style_TopMargin,
    style_TopEdge,
    style_BottomMargin,
    style_BottomEdge,
    style_PreviousIndentation
};

struct marginstyle {
    enum style_MarginValue MarginBasis;
    long Operand;
    enum style_Unit MarginUnit;
    long DotCvtOperand;
};

enum style_SpacingValue {
    style_ConstantSpacing,
    style_InterlineSpacing,
    style_InterparagraphSpacing,
    style_AboveSpacing,
    style_BelowSpacing
};

struct spacingstyle {
    enum style_SpacingValue SpacingBasis;
    long Operand;
    enum style_Unit SpacingUnit;
    long DotCvtOperand;
};

enum style_FontSize {
    style_PreviousFontSize,
    style_ConstantFontSize
};

struct fontsizestyle {
    enum style_FontSize SizeBasis;
    long Operand;
};

enum style_ScriptMovement {
    style_PreviousScriptMovement,
    style_ConstantScriptMovement
};

struct fontscriptstyle {
    enum style_ScriptMovement ScriptBasis;
    long Operand;
    enum style_Unit SizeUnit;
    long DotCvtOperand;
};

/* Flags for styles controlling the appearance of text */

#define style_NoFlags		0
#define style_AllFlags          ~0

/* The following must be kept in sync with the enum defs in style.c */

#define style_Underline         (1<<0)
#define style_Hidden		(1<<1)
#define style_ReadOnly          (1<<2)
#define style_PassThru          (1<<3)
#define style_Icon              (1<<4)	/* unused */
#define style_ContinueIndent 	(1<<5)
#define style_Hinge		(1<<6)
#define style_NewPage		(1<<7)
#define style_ChangeBar         (1<<8)
#define style_OverBar		(1<<9)
#define	style_NoWrap		(1<<10)
	/* the next three are for translation from Scribe to ATK */
#define style_NoFill		(1<<11)
#define style_KeepPriorNL	(1<<12)
#define style_KeepNextNL	(1<<13)
#define	style_TabsCharacters	(1<<14)
#define	style_DottedBox		(1<<15)
#define	style_StrikeThrough	(1<<16)

/**************************************************************
 * Style Class
 **************************************************************/

class style {

classprocedures:

    InitializeObject(struct style *self) returns boolean;
    FinalizeObject(struct style *self);

methods:

    Reset();
    Copy(struct style *dest);

    SetName(char *name);
    GetName() returns char *;

    SetMenuName(char *name);
    GetMenuName() returns char *;

    SetNewLeftMargin(enum style_MarginValue Basis, long Operand, enum style_Unit Unit);
    GetNewLeftMargin(enum style_MarginValue *RefBasis, long *RefOperand, enum style_Unit *RefUnit);

    SetNewRightMargin(enum style_MarginValue Basis, long Operand, enum style_Unit Unit);
    GetNewRightMargin(enum style_MarginValue *RefBasis, long *RefOperand, enum style_Unit *RefUnit);

    SetNewTopMargin(enum style_MarginValue Basis, long Operand, enum style_Unit Unit);
    GetNewTopMargin(enum style_MarginValue *RefBasis, long *RefOperand, enum style_Unit *RefUnit);

    SetNewBottomMargin(enum style_MarginValue Basis, long Operand, enum style_Unit Unit);
    GetNewBottomMargin(enum style_MarginValue *RefBasis, long *RefOperand, enum style_Unit *RefUnit);

    SetNewIndentation(enum style_MarginValue Basis, long Operand, enum style_Unit Unit);
    GetNewIndentation(enum style_MarginValue *RefBasis, long *RefOperand, enum style_Unit *RefUnit);    

    GetNewInterparagraphSpacing(enum style_SpacingValue *RefBasis, long *RefOperand, enum style_Unit *RefUnit);
    SetNewInterparagraphSpacing(enum style_SpacingValue Basis, long Operand, enum style_Unit Unit);

    SetNewAbove(enum style_SpacingValue Basis, long Operand, enum style_Unit Unit);
    GetNewAbove(enum style_SpacingValue *RefBasis, long *RefOperand, enum style_Unit *RefUnit);

    SetNewBelow(enum style_SpacingValue Basis, long Operand, enum style_Unit Unit);
    GetNewBelow(enum style_SpacingValue *RefBasis, long *RefOperand, enum style_Unit *RefUnit);

    SetNewInterlineSpacing(enum style_SpacingValue Basis, long Operand, enum style_Unit Unit);
    GetNewInterlineSpacing(enum style_SpacingValue *RefBasis, long *RefOperand, enum style_Unit *RefUnit);

    SetFontFamily(char *NewFont);
    GetFontFamily(char *FontName, int bufsize);

    SetFontSize(enum style_FontSize Basis, long Operand);
    GetFontSize(enum style_FontSize *RefBasis, long *RefOperand);

    SetFontScript(enum style_ScriptMovement Basis, long Operand, enum style_Unit Unit);
    GetFontScript(enum style_ScriptMovement *RefBasis, long *RefOperand, enum style_Unit *RefUnit);

    SetCounterName(char *NewCName);
    GetCounterName(char *RetrievedCName);

    SetCounterParent(char *NewParent);
    GetCounterParent(char *RetrievedParent);

    ClearCounterStyles();
    AddCounterStyle(char *NewStyle);
    GetCounterStyles(long *RefNumStyles, char ***RefStyleStrings);

    ClearTabChanges();
    AddTabChange(enum style_TabAlignment TabOp, long Where, enum style_Unit Unit);
    GetTabChangeList(long *RefNumTabChanges, struct tabentry ***RefTabChanges);

    /* Provision for arbitrary attributes */

    AddAttribute(char *name, char *value);
    RemoveAttribute(char *name);
    GetAttribute(char *name) returns char *;
    IsAttribute(char *name) returns boolean;

    /* File I/O */

    WriteMenu(FILE *fp);
    ReadMenu(FILE *fp) returns long;

    WriteAttr(FILE *fp);
    ReadAttr(FILE *fp) returns long;

    Write(FILE *fp);
    Read(FILE *fp) returns long;

macromethods:

    /* Macros modifying font face flags; a style may add and/or */
    /* remove face flags from the surrounding envrment. */

    ClearNewFontFaces() ((self)->AddFontFaces = (long) fontdesc_Plain)
    AddNewFontFace(newface) ((self)->AddFontFaces |= (long) (newface))
    ClearOldFontFaces() ((self)->OutFontFaces = ~ (long) fontdesc_Plain)
    RemoveOldFontFace(oldface) ((self)->OutFontFaces &= ~ (long) (oldface))
    GetAddedFontFaces() ((self)->AddFontFaces)
    GetRemovedFontFaces() ((self)->OutFontFaces)
    IsAllAddedFontFace(testface) (((self)->AddFontFaces & (long) (testface)) == (long) (testface))
    IsAllRemovedFontFace(testface) ((~ (self)->OutFontFaces & (long) (testface)) == (long) (testface))
    IsAnyAddedFontFace(testface) ((self)->AddFontFaces & (long) (testface))
    IsAnyRemovedFontFace(testface) (~ (self)->OutFontFaces & (long) (testface))

    /* Macros for changing a style's associated counter data */

    SetCounterScope(newscope) ((self)->CounterScope = (newscope))
    GetCounterScope() ((self)->CounterScope)
    SetCounterPosition(newpos) ((self)->CounterPosition = (newpos))
    GetCounterPosition() ((self)->CounterPosition)
    SetCounterInitialValue(newval) ((self)->CounterInitialValue = (newval))
    GetCounterInitialValue() ((self)->CounterInitialValue)

    /* Macros for changing a style's justification mode; a style */
    /* may change the justification of the surrounding envrment. */

    SetJustification(just) ((self)->NewJustification = (just))
    GetJustification() ((self)->NewJustification)

    /* PRIVATE -- used below.  In the future, these may be */
    /* used publically and the repetitious stuff below removed. */
    /* In the AddMiscFlags, all bits are '0' except flags to be added. */
    /* In the OutMiscFlags, all bits are '1' except flags to be removed. */

    AddFlag(flag) ((self)->AddMiscFlags |= (flag), \
                   (self)->OutMiscFlags |= (flag))
    RemoveFlag(flag) ((self)->AddMiscFlags &= ~(flag), \
                   (self)->OutMiscFlags &= ~(flag))
    UseOldFlag(flag) ((self)->AddMiscFlags &= ~(flag), \
                   (self)->OutMiscFlags |= ~(flag))
    TestAddFlag(flag) ((self)->AddMiscFlags & (flag))
    TestRemoveFlag(flag) (~ (self)->OutMiscFlags & (flag))
    TestUseOldFlag(flag) (! style_TestAddFlag(self, flag) && \
                   ! style_TestRemoveFlag(self, flag))

    AddHinge() style_AddFlag((self), style_Hinge)
    RemoveHinge() style_RemoveFlag((self), style_Hinge)
    UseOldHinge() style_UseOldFlag((self), style_Hinge)
    IsHingeAdded() style_TestAddFlag((self), style_Hinge)
    IsHingeRemoved() style_TestRemoveFlag((self), style_Hinge)
    IsHingeUnchange() style_TestUseOldFlag((self), style_Hinge)

    AddNewPage() style_AddFlag((self), style_NewPage)
    RemoveNewPage() style_RemoveFlag((self), style_NewPage)
    UseOldNewPage() style_UseOldFlag((self), style_NewPage)
    IsNewPageAdded() style_TestAddFlag((self), style_NewPage)
    IsNewPageRemoved() style_TestRemoveFlag((self), style_NewPage)
    IsNewPageUnchange() style_TestUseOldFlag((self), style_NewPage)

    AddHidden() style_AddFlag((self), style_Hidden)
    RemoveHidden() style_RemoveFlag((self), style_Hidden)
    UseOldHidden() style_UseOldFlag((self), style_Hidden)
    IsHiddenAdded() style_TestAddFlag((self), style_Hidden)
    IsHiddenRemoved() style_TestRemoveFlag((self), style_Hidden)
    IsHiddenUnchange() style_TestUseOldFlag((self), style_Hidden)

    AddUnderline() style_AddFlag((self), style_Underline)
    RemoveUnderline() style_RemoveFlag((self), style_Underline)
    UseOldUnderline() style_UseOldFlag((self), style_Underline)
    IsUnderlineAdded() style_TestAddFlag((self), style_Underline)
    IsUnderlineRemoved() style_TestRemoveFlag((self), style_Underline)
    IsUnderlineUnchange() style_TestUseOldFlag((self), style_Underline)

    AddPassThru() style_AddFlag((self), style_PassThru)
    RemovePassThru() style_RemoveFlag((self), style_PassThru)
    UseOldPassThru() style_UseOldFlag((self), style_PassThru)
    IsPassThruAdded() style_TestAddFlag((self), style_PassThru)
    IsPassThruRemoved() style_TestRemoveFlag((self), style_PassThru)
    IsPassThruUnchange() style_TestUseOldFlag((self), style_PassThru)

    AddContinueIndent() style_AddFlag((self), style_ContinueIndent)
    RemoveContinueIndent() style_RemoveFlag((self), style_ContinueIndent)
    UseOldContinueIndent() style_UseOldFlag((self), style_ContinueIndent)
    IsContinueIndentAdded() style_TestAddFlag((self), style_ContinueIndent)
    IsContinueIndentRemoved() style_TestRemoveFlag((self), style_ContinueIndent)
    IsContinueIndentUnchange() style_TestUseOldFlag((self), style_ContinueIndent)

    AddChangeBar() style_AddFlag((self), style_ChangeBar)
    RemoveChangeBar() style_RemoveFlag((self), style_ChangeBar)
    UseOldChangeBar() style_UseOldFlag((self), style_ChangeBar)
    IsChangeBarAdded() style_TestAddFlag((self), style_ChangeBar)
    IsChangeBarRemoved() style_TestRemoveFlag((self), style_ChangeBar)
    IsChangeBarUnchange() style_TestUseOldFlag((self), style_ChangeBar)

    AddOverBar() style_AddFlag((self), style_OverBar)
    RemoveOverBar() style_RemoveFlag((self), style_OverBar)
    UseOldOverBar() style_UseOldFlag((self), style_OverBar)
    IsOverBarAdded() style_TestAddFlag((self), style_OverBar)
    IsOverBarRemoved() style_TestRemoveFlag((self), style_OverBar)
    IsOverBarUnchange() style_TestUseOldFlag((self), style_OverBar)

    AddDottedBox() style_AddFlag((self), style_DottedBox)
    RemoveDottedBox() style_RemoveFlag((self), style_DottedBox)
    UseOldDottedBox() style_UseOldFlag((self), style_DottedBox)
    IsDottedBoxAdded() style_TestAddFlag((self), style_DottedBox)
    IsDottedBoxRemoved() style_TestRemoveFlag((self), style_DottedBox)
    IsDottedBoxUnchange() style_TestUseOldFlag((self), style_DottedBox)

    AddStrikeThrough() style_AddFlag((self), style_StrikeThrough)
    RemoveStrikeThrough() style_RemoveFlag((self), style_StrikeThrough)
    UseOldStrikeThrough() style_UseOldFlag((self), style_StrikeThrough)
    IsStrikeThroughAdded() style_TestAddFlag((self), style_StrikeThrough)
    IsStrikeThroughRemoved() style_TestRemoveFlag((self), style_StrikeThrough)
    IsStrikeThroughUnchange() style_TestUseOldFlag((self), style_StrikeThrough)

    AddReadOnly() style_AddFlag((self), style_ReadOnly)
    RemoveReadOnly() style_RemoveFlag((self), style_ReadOnly)
    UseOldReadOnly() style_UseOldFlag((self), style_ReadOnly)
    IsReadOnlyAdded() style_TestAddFlag((self), style_ReadOnly)
    IsReadOnlyRemoved() style_TestRemoveFlag((self), style_ReadOnly)
    IsReadOnlyUnchange() style_TestUseOldFlag((self), style_ReadOnly)

    AddNoWrap() style_AddFlag((self), style_NoWrap)
    RemoveNoWrap() style_RemoveFlag((self), style_NoWrap)
    UseOldNoWrap() style_UseOldFlag((self), style_NoWrap)
    IsNoWrapAdded() style_TestAddFlag((self), style_NoWrap)
    IsNoWrapRemoved() style_TestRemoveFlag((self), style_NoWrap)
    IsNoWrapUnchange() style_TestUseOldFlag((self), style_NoWrap)

    AddNoFill() style_AddFlag((self), style_NoFill)
    RemoveNoFill() style_RemoveFlag((self), style_NoFill)
    UseOldNoFill() style_UseOldFlag((self), style_NoFill)
    IsNoFillAdded() style_TestAddFlag((self), style_NoFill)
    IsNoFillRemoved() style_TestRemoveFlag((self), style_NoFill)
    IsNoFillUnchange() style_TestUseOldFlag((self), style_NoFill)

    AddKeepPriorNL() style_AddFlag((self), style_KeepPriorNL)
    RemoveKeepPriorNL() style_RemoveFlag((self), style_KeepPriorNL)
    UseOldKeepPriorNL() style_UseOldFlag((self), style_KeepPriorNL)
    IsKeepPriorNLAdded() style_TestAddFlag((self), style_KeepPriorNL)
    IsKeepPriorNLRemoved() style_TestRemoveFlag((self), style_KeepPriorNL)
    IsKeepPriorNLUnchange() style_TestUseOldFlag((self), style_KeepPriorNL)

    AddKeepNextNL() style_AddFlag((self), style_KeepNextNL)
    RemoveKeepNextNL() style_RemoveFlag((self), style_KeepNextNL)
    UseOldKeepNextNL() style_UseOldFlag((self), style_KeepNextNL)
    IsKeepNextNLAdded() style_TestAddFlag((self), style_KeepNextNL)
    IsKeepNextNLRemoved() style_TestRemoveFlag((self), style_KeepNextNL)
    IsKeepNextNLUnchange() style_TestUseOldFlag((self), style_KeepNextNL)

    AddTabsCharacters() style_AddFlag((self), style_TabsCharacters)
    RemoveTabsCharacters() style_RemoveFlag((self), style_TabsCharacters)
    UseOldTabsCharacters() style_UseOldFlag((self), style_TabsCharacters)
    IsTabsCharactersAdded() style_TestAddFlag((self), style_TabsCharacters)
    IsTabsCharactersRemoved() style_TestRemoveFlag((self), style_TabsCharacters)
    IsTabsCharactersUnchange() style_TestUseOldFlag((self), style_TabsCharacters)
data:
    char *name;
    char *menuName;                     /* e.g. "Font~1,Italic~11" */
    boolean template;                   /* Style from current template */

    struct marginstyle NewLeftMargin;
    struct marginstyle NewRightMargin;
    struct marginstyle NewTopMargin;
    struct marginstyle NewBottomMargin;

    struct marginstyle NewIndentation;
    enum style_Justification NewJustification;

    struct spacingstyle NewInterparagraphSpacing;
    struct spacingstyle NewAbove;
    struct spacingstyle NewBelow;
    struct spacingstyle NewInterlineSpacing;

    char *FontFamily;
    struct fontsizestyle FontSize;
    struct fontscriptstyle FontScript;
    long AddFontFaces;      /* OR'ed FontFaces */
    long OutFontFaces;      /* NOT'ed OR'ed FontFaces */

    long NumTabChanges;
    struct tabentry *TabChangeList;	/* ptr to array */

    char *CounterName;
    enum style_Scope CounterScope;
    enum style_Position CounterPosition;
    char *CounterParent;
    long CounterInitialValue;
    long NumCounterStyles;
    char **CounterStyles;               /* Array of strings */

    long AddMiscFlags;      /* Hinge, Pagebreak, Hidden, ... */
    long OutMiscFlags;

    struct namespace *AdditionalAttributes;
};

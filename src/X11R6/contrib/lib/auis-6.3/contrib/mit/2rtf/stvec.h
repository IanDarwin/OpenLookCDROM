/*
 stvec.h of

 2rtf: a facility to convert files in the ATK file format to
 RTF manuscript files.

 Rtf2 is copyright (c) 1991 by the Massachusetts Institute of
 Technology.

 RTF is a product of the Microsoft Corporation.

 Permission to use, copy, modify, and distribute this software and
 its documentation for any purpose and without fee is hereby granted,
 provided that the above copyright notice and the name of the author(s)
 appear in all copies; that both that copyright notice, the name of
 the author(s) and this permission notice appear in supporting
 documentation; and that the name of the Massachusetts Institute of
 Technology not be used in advertising or publicity pertaining to
 distribution of the software without specific, written prior
 permission.  The Massachusetts Institute of Technology makes no
 representations about the suitability of this software for any purpose.
 It is provided "as is" without express or implied warranty.

 2rtf was written by Scott Rixner, rixner@ATHENA.MIT.EDU and Jeremy Paul Kirby,jpkirby@ATHENA.MIT.EDU

 $Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/2rtf/RCS/stvec.h,v 1.1 1994/02/02 19:05:55 susan Exp $

 This file was created from parts of 'atk/text/txtstvec.h' and
 'andrew/common/include/atk/style.ih'.  As this program is a 
 conversion utility involving ATK, any changes to either of those 
 files should probably be accompanied by a change to this one, 
 with the appropriate changes to the rest of 2rtf.
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
 *  $Disclaimer: This software is part of version 5.2.0 of the 
 * Andrew User Interface System and is the 
 * property of IBM, Carnegie Mellon University, 
 * and the other copyright holders.  The source 
 * code of this version is for the sole use of 
 * members of the Andrew Consortium with 
 * memberships extending into calendar year 
 * 1993.  This source code is not to be distributed 
 * to non-members of the consortium nor beyond 
 * a fifty-mile radius from the membership address.  
 * Binary object code compiled or derived from 
 * these sources is not to be distributed to non-
 * members.  Members may have additional 
 * distribution rights granted by prior written 
 * permission of Carnegie Mellon University.
 * 
 * IBM, CARNEGIE MELLON UNIVERSITY, 
 * AND THE OTHER COPYRIGHT HOLDERS
 *  DISCLAIM ALL WARRANTIES WITH 
 * REGARD TO THIS SOFTWARE, INCLUDING 
 * ALL IMPLIED WARRANTIES OF MERCHANT-
 * ABILITY AND FITNESS. IN 
 * NO EVENT SHALL  IBM, CARNEGIE 
 * MELLON UNIVERSITY, OR ANY OTHER 
 * COPYRIGHT HOLDER BE LIABLE FOR 
 * ANY SPECIAL, INDIRECT OR CONSE-
 * QUENTIAL DAMAGES OR ANY DAMAGES 
 * WHATSOEVER RESULTING FROM LOSS OF
 * USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR
 * OTHER TORTIOUS ACTION, ARISING OUT 
 * OF OR IN CONNECTION WITH THE USE OR
 * PERFORMANCE OF THIS SOFTWARE.
 *  $
 *  $
*/


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
/*
    style_CounterNameAttr=19,
    style_CounterParentAttr=20,
    style_CounterScopeAttr=21,
    style_CounterPositionAttr=22,
    style_CounterInitialValueAttr=23,
 */
    style_None=24
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

/*
enum style_Scope {
    style_GlobalScope,
    style_LocalScope
};
*/

/*
enum style_Position {
    style_EnvironmentCtr,
    style_ParagraphCtr,
    style_LineCtr
};
*/

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

struct text_statevector {
    long CurLeftMargin;
    long CurRightMargin;
    long CurRightEdge;
    long CurLeftEdge;
    long CurTopMargin;
    long CurBottomMargin;
/*
    long CurFontAttributes;
 */
    long CurScriptMovement;
    long CurFontSize;
    long CurLeftIndentation;
    long CurRightIndentation;
    enum style_Justification CurJustification;
/*
    long CurSpacing;
    long CurSpread;
    long SpecialFlags;
    struct fontdesc *CurCachedFont;
 */
    char *CurFontFamily;
/* 
    struct dfamily * CurFontFamily;
    struct dfont * CurCachedFont;
    struct tabs *tabs;
 */
};


struct text_statevector State;

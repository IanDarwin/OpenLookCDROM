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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/text/RCS/txtvcmds.c,v 2.73 1993/10/26 22:43:58 gk5g Exp $";
#endif

#include <andrewos.h>
#include <class.h>
#include <ctype.h>
#include <txtvinfo.h>
#include <keymap.ih>
#include <proctbl.ih>
#include <menulist.ih>
#include <environ.ih>
#include <im.ih>
#include <bind.ih>

#define AUXMODULE 1
#include <textv.eh>

long lcKill;
long lcYank;
long lcMove;
long lcDisplayEnvironment;
long lcInsertEnvironment;
long lcNewLine;

extern void textview_AppendNextCut();
extern void textview_BackwardCmd();
extern void textview_BackwardParaCmd();
extern void textview_BackwardWSWordCmd();
extern void textview_BackwardWordCmd();
extern void textview_BackwardsRotatePasteCmd();
extern void textview_BalanceCmd();
extern void textview_BeginningOfFirstWordCmd();
extern void textview_BeginningOfLineCmd();
extern void textview_BeginningOfNextLineCmd();
extern void textview_BeginningOfPreviousLineCmd();
extern void textview_BeginningOfTextCmd();
extern void textview_CapitalizeWord();
extern void textview_ChangeLineCmd();
extern void textview_ChangeRestOfLineCmd();
extern void textview_ChangeSelectionCmd();
extern void textview_ChangeTemplate();
extern void textview_ChangeWordCmd();
extern void textview_CheckSpelling();
extern void textview_CloseFootnotes();
extern void textview_CopyRegionCmd();
extern void textview_CtrlAtCmd();
extern void textview_CursorToBottom();
extern void textview_CursorToCenter();
extern void textview_CursorToTop();
extern void textview_DeleteBackwardWSWordCmd();
extern void textview_DeleteBackwardWordCmd();
extern void textview_DeleteCmd();
extern void textview_DeleteEndOfWSWordCmd();
extern void textview_DeleteEndOfWordCmd();
extern void textview_DeleteWSWordCmd();
extern void textview_DeleteWordCmd();
extern void textview_ForeKillWordCmd();
extern void textview_DigitCmd();
extern void textview_DisplayInsertEnvironment();
extern void textview_DownCmd();
extern void textview_DownInsertEnvironmentCmd();
extern void textview_EndOfLineCmd();
extern void textview_EndOfTextCmd();
extern void textview_EndOfWSWordCmd();
extern void textview_EndOfWordCmd();
extern void textview_ExchCmd();
extern void textview_ExposeStyleEditor();
extern void textview_ForwardCmd();
extern void textview_ForwardParaCmd();
extern void textview_ForwardWSWordCmd();
extern void textview_ForwardWordCmd();
extern void textview_GlitchDownCmd();
extern void textview_GlitchUpCmd();
extern void textview_GoToLineCmd();
extern void textview_GotoParagraphCmd();
extern void textview_GrabReference();
extern void textview_IndentCmd();
extern void textview_InsertAtBeginningCmd();
extern void textview_InsertAtEndCmd();
extern void textview_InsertEnvironment();
extern void textview_InsertFile();
extern void textview_InsertFootnote();
extern void textview_InsertInsetCmd();
extern void textview_InsertNLCmd();
extern void textview_InsertPageBreak();
extern void textview_InsertSoftNewLineCmd();
extern void textview_JoinCmd();
extern void textview_KillLineCmd();
extern void textview_KillWhiteSpaceCmd();
extern void textview_LastPage();
extern void textview_LeftInsertEnvironmentCmd();
extern void textview_LineToTopCmd();
extern void textview_LowercaseWord();
extern void textview_MITKillLineCmd();
extern void textview_MyLfCmd();
extern void textview_MySoftLfCmd();
extern void textview_NextLineCmd();
extern void textview_NextPage();
extern void textview_NextScreenCmd();
extern void textview_OpenFootnotes();
extern void textview_OpenLineAfterCmd();
extern void textview_OpenLineBeforeCmd();
extern void textview_OpenLineCmd();
extern void textview_PlaceReference();
extern void textview_PlainerCmd();
extern void textview_PlainestCmd();
extern void textview_PrevScreenCmd();
extern void textview_PreviousLineCmd();
extern void textview_PutAfterCmd();
extern void textview_PutBeforeCmd();
extern void textview_QueryReplaceCmd();
extern void textview_QuoteCmd();
extern void textview_RSearchCmd();
extern void textview_ReplaceCharCmd();
extern void textview_RightInsertEnvCmd();
extern void textview_RotatePasteCmd();
extern void textview_RuboutCmd();
extern void textview_RuboutWordCmd();
extern void textview_BackKillWordCmd();
extern void textview_SearchAgain();
extern void textview_SearchAgainOppositeCmd();
extern void textview_SearchCmd();
extern void textview_SelectRegionCmd();
extern void textview_SelfInsertCmd();
extern void textview_ShowStylesCmd();
extern void textview_SubstituteCharCmd();
extern void textview_ToggleCase();
extern void textview_ToggleEditorCmd();
extern void textview_ToggleExposeStyles();
extern void textview_ToggleColorStyles();
extern void textview_ToggleReadOnly();
extern void textview_ToggleViModeCmd();
extern void textview_TwiddleCmd();
extern void textview_UnindentCmd();
extern void textview_UpCmd();
extern void textview_UpInsertEnvironmentCmd();
extern void textview_UppercaseWord();
extern void textview_ViCommandCmd();
extern void textview_ViDeleteCmd();
extern void textview_ViDeleteLineCmd();
extern void textview_ViYankLineCmd();
extern void textview_WhatParagraphCmd();
extern void textview_WriteFootnotes();
extern void textview_YankBackwardWSWordCmd();
extern void textview_YankBackwardWordCmd();
extern void textview_YankCmd();
extern void textview_YankEndOfWSWordCmd();
extern void textview_YankEndOfWordCmd();
extern void textview_YankLineCmd();
extern void textview_YankWSWordCmd();
extern void textview_YankWordCmd();
extern void textview_ZapRegionCmd();

static int parseBackslashed();

void textview_NOOPCmd (self)
register struct textview *self;
{
    /* Do nothing.  Used to rebind keys for no operation */
}

/*
 * CharType determines what kind of character has been passed
 * and is used or forward/backward word operations
 */

charType(c)
	register char c;
{

	if (isspace(c))
		return (WHITESPACE);
	else if ( isalpha(c) || isdigit(c) || c == '_' )
		return (WORD);
	else
		return(SPECIAL);
}

char styleString[] = "textview-insert-environment";

/****** EMACS key bindings ********/

static struct bind_Description textviewEmacsBindings[]={

    {"textview-show-styles", "\033s",0,NULL,0,0,textview_ShowStylesCmd,"Show styles at dot.", NULL},
    {"textview-show-styles", "\033'\033s", 0, NULL, 0, 0, textview_ShowStylesCmd, "Show styles at dot.", NULL},

    {"textview-noop", NULL, 0, NULL, 0, 0, textview_NOOPCmd, "Do absolutely nothing.", NULL}, 
    {"textview-insert-environment", "\033'\033l", 0, NULL, 0, 0, textview_InsertEnvironment, "Prompt for a style to use for inserting characters.", NULL},
    {"textview-show-insert-environment", "\033'\033?", 0, NULL, 0, 0, textview_DisplayInsertEnvironment, "Show the environment that will be used for inserting characters.", NULL},
    {"textview-left-insert-environment", "\033'\033D", 0, NULL, 0, 0, textview_LeftInsertEnvironmentCmd, "Move to the left the environment that will be used for inserting characters.", NULL},
    {"textview-right-insert-environment", "\033'\033C", 0, NULL, 0, 0, textview_RightInsertEnvCmd, "Move to the right the environment that will be used for inserting characters.", NULL},
    {"textview-up-insert-environment", "\033'\033A", 0, NULL, 0, 0, textview_UpInsertEnvironmentCmd, "Move up environment that will be used for inserting characters.", NULL},
    {"textview-down-insert-environment", "\033'\033B", 0, NULL, 0, 0, textview_DownInsertEnvironmentCmd, "Move down environment that will be used for inserting characters.", NULL},
    {"textview-up-insert-environment", "\033'\033u", 0, NULL, 0, 0, NULL, NULL, NULL},
    {"textview-down-insert-environment", "\033'\033d", 0, NULL, 0, 0, NULL, NULL, NULL},

    {styleString, "\033'\033i", (long) "italic", NULL, 0, 0, NULL, NULL, NULL},
    {styleString, "\033'\033b", (long) "bold", NULL, 0, 0, NULL, NULL, NULL},
    {styleString, "\033'\033^", (long) "superscript", NULL, 0, 0, NULL, NULL, NULL},
    {styleString, "\033'\033!", (long) "subscript", NULL, 0, 0, NULL, NULL, NULL},
    {styleString, "\033'\033_", (long) "underline", NULL, 0, 0, NULL, NULL, NULL},
    {styleString, "\033'\033t", (long) "typewriter", NULL, 0, 0, NULL, NULL, NULL},
    {styleString, "\033'\033+", (long) "bigger", NULL, 0, 0, NULL, NULL, NULL},
    {styleString, "\033'\033-", (long) "smaller", NULL, 0, 0, NULL, NULL, NULL},
    {styleString, "\033'\033=", (long) "center", NULL, 0, 0, NULL, NULL, NULL},
    {styleString, "\033'\033<", (long) "flushleft", NULL, 0, 0, NULL, NULL, NULL},
    {styleString, "\033'\033>", (long) "flushright", NULL, 0, 0, NULL, NULL, NULL},
    {styleString, "\033'\033\011", (long) "leftindent", NULL, 0, 0, NULL, NULL, NULL},

    {"textview-plainer", "\033'\033p", (long) "new", "Plainer~40", (long) "new", textview_NotReadOnlyMenus, textview_PlainerCmd, "Remove style.", NULL},
    {"textview-plainer", "\030\020", (long) "new", NULL, 0, 0, NULL, NULL, NULL},
    {"textview-plainest","\033'\033P", 0,"Plainest~41", 0, textview_NotReadOnlyMenus, textview_PlainestCmd, "Remove all enclosing styles.", NULL},
    {"textview-beginning-of-line", "\001",0,NULL,0,0,textview_BeginningOfLineCmd,"Move to beginning of current line.", NULL},
    {"textview-beginning-of-line", "\033H",0, NULL, 0, 0, NULL, NULL, NULL},
    {"textview-backward-character", "\002",0,NULL,0,0,textview_BackwardCmd,"Move backward one character.", NULL},
    {"textview-backward-character", "\033D",0, NULL, 0, 0, NULL, NULL, NULL},
    {"textview-delete-next-character", "\004",0,NULL,0,0,textview_DeleteCmd,"Delete the next character.", NULL},
    {"textview-end-of-line", "\005",0,NULL,0,0,textview_EndOfLineCmd,"Move to end of the current line.", NULL},
    {"textview-end-of-line", "\033F",0, NULL, 0, 0, NULL, NULL, NULL},
    {"textview-forward-character", "\006",0,NULL,0,0,textview_ForwardCmd,"Move forward one character.", NULL},
    {"textview-forward-character", "\033C",0, NULL, 0, 0, NULL, NULL, NULL},
    {"textview-delete-previous-character", "\010",0,NULL,0,0,textview_RuboutCmd,"Delete the previous character.", NULL},
    {"textview-delete-previous-character", "\177",0, NULL, 0, 0, NULL, NULL, NULL},
    {"textview-insert-newline", "\015",0,NULL,0,0,textview_InsertNLCmd,"Insert a newline character.", NULL},
    {"textview-insert-soft-newline", "\033\015",0,NULL,0,0,textview_InsertSoftNewLineCmd,"Insert a soft newline character.", NULL},
    {"textview-next-line", "\016",0,NULL,0,0,textview_NextLineCmd,"Move to next line.", NULL},
    {"textview-next-line", "\033B",0, NULL, 0, 0, NULL, NULL, NULL},
    {"textview-previous-line", "\020",0,NULL,0,0,textview_PreviousLineCmd,"Move to previous line.", NULL},
    {"textview-previous-line", "\033A",0, NULL, 0, 0, NULL, NULL, NULL},

    {"textview-indent", "\033i",0,NULL,0,0,textview_IndentCmd,"Indent current line.", NULL},

    {"textview-GrabReference", "\030w",0,NULL,0,0,textview_GrabReference,"Grab the next viewref", NULL},

    {"textview-PlaceReference", "\030y",0,NULL,0,0,textview_PlaceReference,"Create a new viewref", NULL},

    {"textview-unindent", "\033u",0,NULL,0,0,textview_UnindentCmd,"Un-indent current line.", NULL},

    {"textview-exch", "\030\030",0,NULL,0,0,textview_ExchCmd,"Exchange dot and mark.", NULL},

    {"textview-select-region", "\033\200",0,NULL,0,0,textview_SelectRegionCmd,"Select between dot and mark.", NULL},

    {"textview-copy-region", "\033w",0,"Copy~11",0,textview_SelectionMenus,textview_CopyRegionCmd,"Copy region to kill-buffer.", NULL},

    {"textview-mylf", "\012",0,NULL,0,0,textview_MyLfCmd,"Skip to next line and indent.", NULL},

    {"textview-my-soft-lf", "\033j",0,NULL,0,0,textview_MySoftLfCmd,"Skip to next line/soft return and indent.", NULL},

    {"textview-zap-region", "\027",0,"Cut~10",0, textview_SelectionMenus | textview_NotReadOnlyMenus,textview_ZapRegionCmd,"Remove the text within the selection region and place it in a cutbuffer.", NULL},

    {"textview-append-next-cut", "\033\027",0,NULL,0,0,textview_AppendNextCut, "Make next cut command append to the kill-buffer as opposed to making a new buffer.", NULL},

    {"textview-yank", "\031",0,"Paste~10",0,textview_NotReadOnlyMenus | textview_NoSelectionMenus,textview_YankCmd,"Yank text back from kill-buffer.", NULL},

    {"textview-insert-file", "\030\t", 0,"File~10,Insert File~10",0,textview_NotReadOnlyMenus,textview_InsertFile,"Prompt for a filename and insert that file's contents into the document.", NULL},

    {"textview-rotate-backward-paste", "\033\031",0,NULL,0,0,textview_BackwardsRotatePasteCmd,"Rotate kill-buffer backwards.", NULL},

    {"textview-line-to-top", "\033!",0,NULL,0,0,textview_LineToTopCmd,"Move current line to top of screen.", NULL},

    {"textview-rotate-paste", "\033y",0,NULL,0,0,textview_RotatePasteCmd,"Rotate kill-buffer.", NULL},
    
    {"textview-forward-para", "\033]",0,NULL,0,0,textview_ForwardParaCmd,"Move to the next paragraph.", NULL},

    {"textview-backward-para", "\033[",0,NULL,0,0,textview_BackwardParaCmd,"Move to the previous paragraph.", NULL},

    {"textview-open-line", "\017",0,NULL,0,0,textview_OpenLineCmd,"Insert blank line at dot", NULL},

    {"textview-prev-screen", "\033v",0,NULL,0,0,textview_PrevScreenCmd,"Move back to previous screen", NULL},
    {"textview-prev-screen", "\033G",0, NULL, 0, 0, NULL, NULL, NULL},

    {"textview-next-screen", "\026",0,NULL,0,0,textview_NextScreenCmd,"Move forward to next screen", NULL},
    {"textview-next-screen", "\033E",0, NULL, 0, 0, NULL, NULL, NULL},

    {"textview-glitch-down", "\021",0,NULL,0,0,textview_GlitchDownCmd,"Glitch screen down one line.", NULL},
    {"textview-glitch-down", "\033z",0, NULL, 0, 0, NULL, NULL, NULL},

    {"textview-glitch-up", "\032",0,NULL,0,0,textview_GlitchUpCmd,"Glitch screen up one line.", NULL},

    {"textview-twiddle-chars", "\024",0,NULL,0,0,textview_TwiddleCmd,"Exchange previous two chars.", NULL},

    {"textview-kill-line", "\013",0,NULL,0,0,textview_KillLineCmd,"Kill rest of line.", NULL},

    {"textview-MIT-kill-line", NULL,0,NULL,0,0,textview_MITKillLineCmd,"Kill rest of line.", NULL},

    {"textview-search", "\023",0,"Search/Spell~1,Forward~10",0,0,(void (*)())textview_SearchCmd,"Search forward.", NULL},

    {"textview-reverse-search", "\022",0,"Search/Spell~1,Backward~11",0,0,(void (*)())textview_RSearchCmd,"Search backward.", NULL},

    {"textview-search-again",NULL,0,"Search/Spell~1,Search Again~12",0,0,textview_SearchAgain,"Repeat last search.", NULL},

    {"textview-insert-inset-here", "\033\t",0,NULL,0,0,textview_InsertInsetCmd,"Add inset at this location.", NULL},

    {"textview-what-paragraph", "\033N",0,NULL,0,0,textview_WhatParagraphCmd,"Print current paragraph number.", NULL},

    {"textview-goto-paragraph", "\033n",0,NULL,0,0,textview_GotoParagraphCmd,"Go to specific paragraph.", NULL},

    {"textview-query-replace", "\033q",0,"Search/Spell~1,Query Replace~20",0,textview_NotReadOnlyMenus,textview_QueryReplaceCmd,"Query replace.", NULL},
    
#ifdef IBM
    {"spell-check-document",NULL,0,"Search/Spell~1,Check Spelling~30", 0,textview_NotReadOnlyMenus,textview_CheckSpelling,"Checks spelling from the caret on.", "spell"},
#else /* IBM */
    {"spell-check-document",NULL,0,"Search/Spell~1,Check Spelling~30", 0,textview_NotReadOnlyMenus,NULL,"Checks spelling from dot to end.", "spell"},
#endif /* IBM */

    {"textview-quote", "\030\021",0,NULL,0,0,textview_QuoteCmd,"Uninterpreted insert of any char or octal code.", NULL},

    {"textview-kill-white-space", "\033k",0,NULL,0,0,textview_KillWhiteSpaceCmd,"Delete spaces and tabs around dot", NULL},

    {"textview-ctrl-at", "\200",0,NULL,0,0,textview_CtrlAtCmd,"Set a mark.", NULL},

    {"textview-backward-word", "\033b",0,NULL,0,0,textview_BackwardWordCmd,"Move backward to beginning of word.", NULL},
    {"textview-delete-next-word", "\033d",0,NULL,0,0,textview_DeleteWordCmd,"Delete  the next word.", NULL},
    {"textview-kill-next-word", NULL,0,NULL,0,0,textview_ForeKillWordCmd,"Kill  the next word, saving it on the kill ring.", NULL},
    {"textview-forward-word", "\033f",0,NULL,0,0,textview_ForwardWordCmd,"Move forward to end of word.", NULL},
    {"textview-delete-previous-word", "\033h",0,NULL,0,0,textview_RuboutWordCmd,"Delete the previous word.", NULL},
    {"textview-delete-previous-word", "\033\b",0,NULL,0,0,textview_RuboutWordCmd,NULL, NULL},
    {"textview-delete-previous-word", "\033\177",0,NULL,0,0,textview_RuboutWordCmd,NULL, NULL},
    {"textview-kill-previous-word", NULL,0,NULL,0,0,textview_BackKillWordCmd,"Kill the previous word, saving it on the kill ring.", NULL},
    {"textview-end-of-text", "\033>",0,NULL,0,0,textview_EndOfTextCmd,"Move to end of text.", NULL},
    {"textview-beginning-of-text", "\033<",0,NULL,0,0,textview_BeginningOfTextCmd,"Move to beginning of text.", NULL},
    {"textview-toggle-character-case", "\036",0,NULL,0,0,textview_ToggleCase,"Toggle the case of the character at the dot.", NULL},
    {"textview-lowercase-word", "\033l",0,NULL,0,0,textview_LowercaseWord,"Convert word (or region) to lower case.", NULL},
    {"textview-uppercase-word",NULL,0,NULL,0,0,textview_UppercaseWord,"Convert word (or region) to upper case.", NULL},
    {"textview-capitalize-word",NULL,0,NULL,0,0,textview_CapitalizeWord,"Capitalize word (or all words within a region).", NULL},
    {"textview-cursor-to-top", "\033,",0,NULL,0,0,textview_CursorToTop,"Moves cursor to the beginning of the line currently at the top of the screen.", NULL},
    {"textview-cursor-to-bottom", "\033.",0,NULL,0,0,textview_CursorToBottom,"Moves cursor to the beginning of the line currently at the bottom of the screen.", NULL},
    {"textview-cursor-to-center", "\033/",0,NULL,0,0,textview_CursorToCenter,"Moves cursor to the beginning of the line currently at the center of the screen.", NULL},

    {"textview-change-template",NULL,0,"File~10,Add Template~31",0,textview_NotReadOnlyMenus,textview_ChangeTemplate, "Change to named template.", NULL},

    {"textview-toggle-read-only", "\033~",0,NULL,0,0,textview_ToggleReadOnly,"Change read only status of text object.", NULL},
    {"textview-toggle-expose-styles", NULL,0,NULL,0,0,textview_ToggleExposeStyles,"Expose/hide style information", NULL},
    {"textview-toggle-color-styles", NULL,0,NULL,0,0,textview_ToggleColorStyles,"Show/don't show color styles", NULL},
    {"textview-edit-styles", NULL, 0, NULL, 0, textview_NotReadOnlyMenus, textview_ExposeStyleEditor,"Expose style editor", NULL},
    {"lookzview-edit-styles", NULL, 0, "File~10,Edit Styles~30", 0, textview_NotReadOnlyMenus, NULL,"Bring up style editor in separate window", "lookzview"},
    {"textview-insert-pagebreak", NULL,0,"Page~9,Insert Pagebreak~11",0,textview_NotReadOnlyMenus,textview_InsertPageBreak,"Add page break at this location.", NULL},
    {"textview-next-page", NULL,0,"Page~9,Next Page~12",0,0,textview_NextPage,"Frame text at next page break object", NULL},
    {"textview-last-page", NULL,0,"Page~9,Previous Page~13",0,0,textview_LastPage,"Frame text at last page break object", NULL},
    {"textview-insert-footnote", NULL,0,"Page~9,Insert Footnote~20",0,textview_NotReadOnlyMenus,textview_InsertFootnote,"Add footnote at this location.", NULL},
    {"contentv-make-window", NULL,0,"Page~9,Table of Contents~30",0,0,NULL,"Make a table of contents window","contentv"},
    {"textview-open-footnotes",NULL,0,"Page~9,Open Footnotes~22",0,0,textview_OpenFootnotes,"Open all footnotes", NULL},
    {"textview-close-footnotes",NULL,0,"Page~9,Close Footnotes~23",0,0,textview_CloseFootnotes,"Close all footnotes", NULL}, {"textview-write-footnotes",NULL,0,NULL,0,0,textview_WriteFootnotes,"Write all footnotes", NULL},
    {"textview-toggle-editor", "\033t",0,NULL,0,0,textview_ToggleEditorCmd, "Switch to vi editor.", NULL},
    {"textview-balance", "\033m",0,NULL,0,0,textview_BalanceCmd,"Balance parentheses, brackets, or braces.", NULL},
    {NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL}
};

static void CheckStylePreferences(newKeymap, normalMenus)
struct keymap *newKeymap;
struct menulist **normalMenus;
{
    struct proctable_Entry *pl;

    /*
      This code is here to handle allow users to select which style
      of plainer that they want to use: old or new
      */

    if (! environ_GetProfileSwitch("UseNewStylePlainer", TRUE)) {
	pl = proctable_Lookup("textview-plainer");
	keymap_BindToKey(newKeymap, "\030\020", pl, (long) "old");
	if (normalMenus != NULL && *normalMenus != NULL) {
	    menulist_AddToML(*normalMenus, "Plainer~40", pl, (long) "old", textview_NotReadOnlyMenus);
	}
	pl = proctable_Lookup("textview-plainest");
	if (normalMenus != NULL && *normalMenus != NULL) {
	    menulist_AddToML(*normalMenus, "Plainest~40", pl, 0, textview_SelectionMenus | textview_NotReadOnlyMenus);
	}
    }

    /* Override binding of our stylizing prefix char with noop */
    if (! environ_GetProfileSwitch("UseStyleKeybindings", TRUE)) {
	pl = proctable_Lookup("textview-noop");
	keymap_BindToKey(newKeymap, "\033'", pl, NULL);
    }
}

/* These two procs ripped out of basics/common/init.c */
/* Translate a key sequence that has ^A, \ddd, and \c conventions. */
static int TranslateKeySequence(from, to)
    char *from;
    char *to;
{
    while (*from != '\0') {
        if (*from == '\\') {

            int temp = parseBackslashed(&from);

            if (temp == -1)
                return -1;
            else
                *to++ = temp;
        }
        else if (*from == '^') {
            ++from;
            if (*from == 0)
                return -1;
            *to++ = (*from++) & 0x1f;
        }
        else
            *to++ = *from++;
    }
    *to++ = 0;
    return 0;
}

static int parseBackslashed(fromChars)
    char **fromChars;
{

    int returnChar;
    char *from = *fromChars;
    static char *bsSource = "ebrnt";
    static char *bsDest = "\033\b\r\n\t";

    if (*from == '\\') {
        ++from;
        if (*from == 0)
            return -1;
        if (isdigit(*from)) {

            int sum = 0, i;

            for (i = 0; i < 3; ++i) {
                if (!isdigit(*from))
                    break;
                if (*from == '8' || *from == '9')
                    return -1;
                sum = sum * 8 + *from - '0';
                ++from;
            }
            returnChar = sum;
        }
        else {

            char *p;

            p = index(bsSource, *from);
            if (p != NULL)
                returnChar = bsDest[p-bsSource];
            else
                returnChar = *from;
            ++from;
        }
    }
    else
        return -1;
    *fromChars = from;
    return returnChar;
}

static adjustBindings(bindings)
struct bind_Description *bindings;
{
    struct bind_Description *bd;
    char *stylePrefixPref;
    char stylePrefixStr[100];

    stylePrefixPref = environ_GetProfile("StyleCommandPrefix");
    if (stylePrefixPref != NULL) {
        if (TranslateKeySequence(stylePrefixPref, stylePrefixStr) >= 0) {
            long plen = strlen(stylePrefixStr);

            for (bd = bindings; bd != NULL && (bd->procName || bd->keyVector || bd->menuEntry); bd++) {
		if (bd->keyVector != NULL){
		    if (strcmp(bd->keyVector, stylePrefixStr) == 0) {
			bd->keyVector = NULL;
		    }
		    else if (bd->keyVector[0] == '\033' && bd->keyVector[1] == '\'') {
			long vlen = strlen(bd->keyVector);
			char *oldVector = bd->keyVector;

			if (bd->keyVector = (char *) malloc(plen + vlen - 1)) {
			strcpy(bd->keyVector, stylePrefixStr);
			strcat(bd->keyVector, &oldVector[2]);
			}
		    }
		}
            }
        }
    }
}

struct keymap *textview_InitEmacsKeyMap(classInfo, normalMenus)
struct textview_classinfo *classInfo;
struct menulist **normalMenus;
{
    struct keymap *newKeymap = keymap_New();
    register long i;
    char str[2];
    struct proctable_Entry *si;
    
    if(normalMenus!=NULL)
	*normalMenus=menulist_New();

    lcKill = im_AllocLastCmd();
    lcYank = im_AllocLastCmd();
    lcMove = im_AllocLastCmd();
    lcDisplayEnvironment = im_AllocLastCmd();
    lcInsertEnvironment = im_AllocLastCmd();
    lcNewLine = im_AllocLastCmd();

    adjustBindings(textviewEmacsBindings);
    bind_BindList(textviewEmacsBindings, newKeymap, *normalMenus, classInfo);

    si=proctable_DefineProc("textview-self-insert", (procedure) textview_SelfInsertCmd, classInfo, NULL, "Insert a character.");

    str[0] = ' ';
    str[1] = '\0';
    for (i = 32; i < 127; i++)  {
	keymap_BindToKey(newKeymap, str, si, i);
	str[0]++;
    }
    /* add bindings for iso keyboards */
    str[0] = (unsigned char ) 160;
    for (i = 160 ; i < 256; i++)  {
	keymap_BindToKey(newKeymap, str, si, i);
	str[0]++;
    }
    keymap_BindToKey(newKeymap, "\t", si, '\t');

    CheckStylePreferences(newKeymap, normalMenus);

    /* Disable keystroke conversion to VI commands unless preference specifically allows it. */
    if (! environ_GetProfileSwitch("AllowKeyToggleToVIMode", FALSE)) {
	si = proctable_Lookup("textview-noop");
	keymap_BindToKey(newKeymap, "\033t", si, NULL);
    }

    return newKeymap;
}

/*************** VI Command mode key bindings *********************/

static struct bind_Description textviewViCommandModeBindings[]={

    {"textview-noop", NULL, 0, NULL, 0, 0, textview_NOOPCmd, "Do absolutely nothing.", NULL}, 
    {"textview-vi-command", ":",0,NULL,0,0,textview_ViCommandCmd, "Execute a vi command via commmand line.", NULL},
    {"textview-input-mode", "a",0,NULL,0,0,textview_ToggleViModeCmd, "Switch to input mode.", NULL},
    {"textview-input-mode", "i",0, NULL, 0, 0, NULL, NULL, NULL},

    {"textview-toggle-editor", "\033t",0,NULL,0,0,textview_ToggleEditorCmd, "Switch to emacs editor.", NULL},
    
    {"textview-open-line-before", "O",0,NULL,0,0,textview_OpenLineBeforeCmd, "Insert a new line before the current line.", NULL},

    {"textview-open-line-after", "o",0,NULL,0,0,textview_OpenLineAfterCmd, "Insert a new line after the current line.", NULL},

    {"textview-insert-at-beginning", "I",0,NULL,0,0,textview_InsertAtBeginningCmd,"Insert at beginning of current line.", NULL},

    {"textview-insert-at-end", "A",0,NULL,0,0,textview_InsertAtEndCmd,"Insert at end of current line.", NULL},

    {"textview-change-rest-of-line", "C",0,NULL,0,0,textview_ChangeRestOfLineCmd,"Replace rest of current line.", NULL},

    {"textview-change-rest-of-line", "S",0, NULL, 0, 0, NULL, NULL, NULL},

    {"textview-change-line", "cc",0,NULL,0,0,textview_ChangeLineCmd,"Replace current line.", NULL},

    {"textview-change-word", "cw",0,NULL,0,0,textview_ChangeWordCmd,"Replace current word.", NULL},

    {"textview-change-selection", "cs",0,NULL,0,0,textview_ChangeSelectionCmd,"Replace current selection.", NULL},

    {"textview-replace-char", "r",0,NULL,0,0,textview_ReplaceCharCmd,"Replace character after cursor.", NULL},

    {"textview-substitute-char", "s",0,NULL,0,0,textview_SubstituteCharCmd,"Insert over character after cursor.", NULL},

    {"textview-beginning-of-previous-line", "-",0,NULL,0,0,textview_BeginningOfPreviousLineCmd,"Move to beginning of previous line.", NULL},

    {"textview-beginning-of-next-line", "+",0,NULL,0,0,textview_BeginningOfNextLineCmd,"Move to beginning of next line.", NULL},

    {"textview-beginning-of-next-line", "\015",0, NULL, 0, 0, NULL, NULL, NULL},

    {"textview-beginning-of-first-word", "^",0,NULL,0,0,textview_BeginningOfFirstWordCmd,"Move to beginning of first word on current line.", NULL},

    {"textview-backward-character", "\010",0,NULL,0,0,textview_BackwardCmd,"Move backward one character.", NULL},

    {"textview-backward-character", "h",0, NULL, 0, 0, NULL, NULL, NULL},

    {"textview-backward-character", "\177",0, NULL, 0, 0, NULL, NULL, NULL},

   {"textview-backward-character", "\033D",0, NULL, 0, 0, NULL, NULL, NULL},
 
    {"textview-vi-delete-next-character", "x",0,NULL,0,0,textview_ViDeleteCmd,"Delete the next character.", NULL},

    {"textview-end-of-line", "$",0,NULL,0,0,textview_EndOfLineCmd,"Move to end of the current line.", NULL},

    {"textview-end-of-line", "\033F",0, NULL, 0, 0, NULL, NULL, NULL},

    {"textview-beginning-of-line", "0",0,NULL,0,0,textview_BeginningOfLineCmd,"Move to beginning of current line.", NULL},

    {"textview-beginning-of-line", "\033H",0, NULL, 0, 0, NULL, NULL, NULL},

    {"textview-forward-character", "040",0,NULL,0,0,textview_ForwardCmd,"Move forward one character.", NULL},

    {"textview-forward-character", "l",0, NULL, 0, 0, NULL, NULL, NULL},

    {"textview-forward-character", "\011",0, NULL, 0, 0, NULL, NULL, NULL},

    {"textview-forward-character", " ",0, NULL, 0, 0, NULL, NULL, NULL},

    {"textview-forward-character", "\033C",0, NULL, 0, 0, NULL, NULL, NULL},

    {"textview-delete-previous-character", "X",0,NULL,0,0,textview_RuboutCmd,"Delete the previous character.", NULL},

    {"textview-next-line", "\016",0,NULL,0,0,textview_NextLineCmd,"Move to next line.", NULL},

    {"textview-next-line", "\012",0, NULL, 0, 0, NULL, NULL, NULL},

    {"textview-next-line", "j",0, NULL, 0, 0, NULL, NULL, NULL},

     {"textview-next-line", "\033B",0, NULL, 0, 0, NULL, NULL, NULL},

   {"textview-previous-line", "\020",0,NULL,0,0,textview_PreviousLineCmd,"Move to previous line.", NULL},

    {"textview-previous-line", "\013",0, NULL, 0, 0, NULL, NULL, NULL},

    {"textview-previous-line", "k",0, NULL, 0, 0, NULL, NULL, NULL},

    {"textview-previous-line", "\033A",0, NULL, 0, 0, NULL, NULL, NULL},

    {"textview-shift-up", "\005",0,NULL,0,0,textview_GlitchUpCmd,"Shift display up one line.", NULL},

    {"textview-shift-down", "\031",0,NULL,0,0,textview_GlitchDownCmd,"Shift display down one line.", NULL},

    {"textview-GrabReference", "\030w",0,NULL,0,0,textview_GrabReference,"Grab the next viewref", NULL},

    {"textview-PlaceReference", "\030y",0,NULL,0,0,textview_PlaceReference,"Create a new viewref", NULL},

    {"textview-exch", "\030\030",0,NULL,0,0,textview_ExchCmd,"Exchange dot and mark.", NULL},

    {"textview-copy-region", "ys",0,"Copy~11",0,textview_SelectionMenus,textview_CopyRegionCmd,"Yank region to kill-buffer.", NULL},

    {"textview-zap-region", "ds",0,"Cut~10",0, textview_SelectionMenus | textview_NotReadOnlyMenus,textview_ZapRegionCmd,"Remove the text within the selection region and place it in a cutbuffer.", NULL},

    {"textview-yank", NULL,0,"Paste~10",0,textview_NotReadOnlyMenus | textview_NoSelectionMenus,textview_YankCmd,"Yank text back from kill-buffer.", NULL},

    {"textview-put-before", "P",0,NULL,0,0,textview_PutBeforeCmd,"Put text back from kill-buffer, before current line.", NULL},

    {"textview-put-after", "p",0,NULL,0,0,textview_PutAfterCmd,"Put text back from kill-buffer, after current line.", NULL},

    {"textview-insert-file", ":r", 0,"File~10,Insert File~10",0,textview_NotReadOnlyMenus,textview_InsertFile,"Prompt for a filename and insert that file's contents into the document.", NULL},

    {"textview-forward-para", "}",0,NULL,0,0,textview_ForwardParaCmd,"Move to the next paragraph.", NULL},

    {"textview-backward-para", "{",0,NULL,0,0,textview_BackwardParaCmd,"Move to the next paragraph.", NULL},

    {"textview-prev-screen", "\002",0,NULL,0,0,textview_PrevScreenCmd,"Move forward to previous screen", NULL},

    {"textview-prev-screen", "\033G",0, NULL, 0, 0, NULL, NULL, NULL},

    {"textview-next-screen", "\006",0,NULL,0,0,textview_NextScreenCmd,"Move forward to next screen", NULL},

    {"textview-next-screen", "\033E",0, NULL, 0, 0, NULL, NULL, NULL},

    {"textview-down", "\004",0,NULL,0,0,textview_DownCmd,"Move down. ", NULL},

    {"textview-up", "\025",0,NULL,0,0,textview_UpCmd,"Move up. ", NULL},

    {"textview-go-to", "G",0,NULL,0,0,textview_GoToLineCmd,"Go to line number.", NULL},

    {"textview-go-to", "g",0, NULL, 0, 0, NULL, NULL, NULL},
    
    {"textview-balance", "%",0,NULL,0,0,textview_BalanceCmd,"Balnce parentheses, brackets, or braces.", NULL},

    {"textview-twiddle-chars", "\024",0,NULL,0,0,textview_TwiddleCmd,"Exchange previous two chars.", NULL},

    {"textview-kill-line", "D",0,NULL,0,0,textview_KillLineCmd,"Kill rest of line.", NULL},

    {"textview-kill-line", "d$",0, NULL, 0, 0, NULL, NULL, NULL},

    {"textview-yank-line", "y$",0,NULL,0,0,textview_YankLineCmd,"Yank rest of line.", NULL},

    {"textview-vi-delete-line", "dd",0,NULL,0,0,textview_ViDeleteLineCmd,"Delete line.", NULL},

    {"textview-vi-yank-line", "yy",0,NULL,0,0,textview_ViYankLineCmd,"Yank line.", NULL},

    {"textview-vi-yank-line", "Y",0, NULL, 0, 0, NULL, NULL, NULL},

    {"textview-join", "J",0,NULL,0,0,textview_JoinCmd,"Join current and next line.", NULL},

    {"textview-backward-word", "b",0,NULL,0,0,textview_BackwardWordCmd,"Move backward to beginning of word.", NULL},

    {"textview-backward-whitespace-word", "B",0,NULL,0,0,textview_BackwardWSWordCmd,"Move backward to beginning of word separateded by whitespace.", NULL},

    {"textview-end-of-word", "e",0,NULL,0,0,textview_EndOfWordCmd,"Move forward to end of word.", NULL},

    {"textview-end-of-whitespace-word", "E",0,NULL,0,0,textview_EndOfWSWordCmd,"Move forward to end of word separated by whitespace.", NULL},

    {"textview-forward-beginning-of-word", "w",0,NULL,0,0,textview_ForwardWordCmd,"Move forward to beginning of word.", NULL},

    {"textview-forward-whitespace-word", "W",0,NULL,0,0,textview_ForwardWSWordCmd,"Move forward to beginning of word separated space.", NULL},

    {"textview-delete-next-word", "dw",0,NULL,0,0,textview_DeleteWordCmd,"Delete the next word.", NULL},

    {"textview-yank-next-word", "yw",0,NULL,0,0,textview_YankWordCmd,"Yank the next word.", NULL},

    {"textview-delete-end-of-word", "de",0,NULL,0,0,textview_DeleteEndOfWordCmd,"Delete end of word.", NULL},

    {"textview-yank-end-of-word", "ye",0,NULL,0,0,textview_YankEndOfWordCmd,"Yank end of word.", NULL},

    {"textview-delete-backward-word", "db",0,NULL,0,0,textview_DeleteBackwardWordCmd,"Delete the previous word.", NULL},

    {"textview-yank-previous-word", "yb",0,NULL,0,0,textview_YankBackwardWordCmd,"Yank the previous word.", NULL},

    {"textview-delete-next-whitespace-word", "dW",0,NULL,0,0,textview_DeleteWSWordCmd,"Delete the next word separated by whitespace.", NULL},

    {"textview-yank-next-whitespace-word", "yW",0,NULL,0,0,textview_YankWSWordCmd,"Yank the next word separated by whitespace.", NULL},

    {"textview-delete-end-of-whitespace-word", "dE",0,NULL,0,0,textview_DeleteEndOfWSWordCmd,"Delete end of word separated by whitespace.", NULL},

    {"textview-yank-end-of-whitespace-word", "yE",0,NULL,0,0,textview_YankEndOfWSWordCmd,"Yank end of word separated by whitespace.", NULL},

    {"textview-delete-previous-whitespace-word", "dB",0,NULL,0,0,textview_DeleteBackwardWSWordCmd,"Delete the previous word separated by whitespace.", NULL},

    {"textview-yank-previous-whitespace-word", "yB",0,NULL,0,0,textview_YankBackwardWSWordCmd,"Yank the previous word separated by whitespace.", NULL},

    {"dynsearch-search-forward", "/",0,"Search/Spell~1,Forward~10",0,0,NULL,NULL, NULL},

    {"dynsearch-search-reverse", "?",0,"Search/Spell~1,Backward~11",0,0, NULL, NULL, NULL},

    {"dynsearch-search-again","n",0,"Search/Spell~1,Search Again~12",0,0, NULL, NULL, NULL},

    {"dynsearch-search-again-opposite", "N", 0, NULL, 0, 0, NULL, NULL, NULL},

    {"textview-query-replace", "\033q",0,"Search/Spell~1,Query Replace~20",0,textview_NotReadOnlyMenus,textview_QueryReplaceCmd,"Query replace.", NULL},

#ifdef IBM
    {"spell-check-document",NULL,0,"Search/Spell~1,Check Spelling~30", 0,textview_NotReadOnlyMenus,textview_CheckSpelling,"Checks spelling from the caret on.", "spell"},
#else
    {"spell-check-document",NULL,0,"Search/Spell~1,Check Spelling~30", 0,textview_NotReadOnlyMenus,NULL,"Checks spelling from the caret on.", "spell"},
#endif

    {"textview-ctrl-at", "\200",0,NULL,0,0,textview_CtrlAtCmd,"Set a mark.", NULL},

    {"textview-cursor-to-top", "H",0,NULL,0,0,textview_CursorToTop,"Moves cursor to the beggining of the line currently at the top of the screen.", NULL},

    {"textview-cursor-to-bottom", "L",0,NULL,0,0,textview_CursorToBottom,"Moves cursor to the beginning of the line currently at the bottom of the screen.", NULL},

    {"textview-cursor-to-center", "M",0,NULL,0,0,textview_CursorToCenter,"Moves cursor to the beginning of the line currently at the center of the screen.", NULL},

    {"textview-change-template",NULL,0,"File~10,Add Template~31",0,textview_NotReadOnlyMenus,textview_ChangeTemplate, "Change to named template.", NULL},


/*************** from original textview **************/
    {"textview-indent", "\033i",0,NULL,0,0,textview_IndentCmd,"Indent current line.", NULL},

    {"textview-unindent", "\033u",0,NULL,0,0,textview_UnindentCmd,"Un-indent current line.", NULL},

    {"textview-select-region", "\033\200",0,NULL,0,0,textview_SelectRegionCmd,"Select between dot and mark.", NULL},

    {"textview-append-next-cut", "\033\027",0,NULL,0,0,textview_AppendNextCut, "Make next cut command append to the cutbuffer as opposed to making a new buffer.", NULL},

    {"textview-rotate-backward-paste", "\033\031",0,NULL,0,0,textview_BackwardsRotatePasteCmd,"Rotate kill-buffer backwards.", NULL},

    {"textview-line-to-top", "\033!",0,NULL,0,0,textview_LineToTopCmd,"Move current line to top of screen.", NULL},

    {"textview-rotate-paste", "\033y",0,NULL,0,0,textview_RotatePasteCmd,"Rotate kill-buffer.", NULL},
    
    {"textview-insert-inset-here", "\033\t",0,NULL,0,0,textview_InsertInsetCmd,"Add inset at this location.", NULL},

    {"textview-what-paragraph", "\033N",0,NULL,0,0,textview_WhatParagraphCmd,"Print current paragraph number.", NULL},

    {"textview-goto-paragraph", "\033n",0,NULL,0,0,textview_GotoParagraphCmd,"Go to specific paragraph.", NULL},

#ifdef IBM
    {"spell-check-document",NULL,0,"Search/Spell~1,Check Spelling~30", 0,textview_NotReadOnlyMenus,textview_CheckSpelling,"Checks spelling from the caret on.", "spell"},
#else /* IBM */
    {"spell-check-document",NULL,0,"Search/Spell~1,Check Spelling~30", 0,textview_NotReadOnlyMenus,NULL,"Checks spelling from the caret on.", "spell"},
#endif /* IBM */

    {"textview-kill-white-space", "\033k",0,NULL,0,0,textview_KillWhiteSpaceCmd,"Delete spaces and tabs around the current pos.", NULL},

    {"textview-toggle-character-case", "\036",0,NULL,0,0,textview_ToggleCase,"Toggle the case of the character at the dot.", NULL},
    {"textview-lowercase-word", "\033l",0,NULL,0,0,textview_LowercaseWord,"Convert word (or region) to lower case.", NULL},
    {"textview-uppercase-word",NULL,0,NULL,0,0,textview_UppercaseWord,"Convert word (or region) to upper case.", NULL},
    {"textview-capitalize-word",NULL,0,NULL,0,0,textview_CapitalizeWord,"Capitalize word (or all words within a region).", NULL},

    {"textview-toggle-read-only", "\033~",0,NULL,0,0,textview_ToggleReadOnly,"Change read only status of text object.", NULL},
    {"textview-toggle-expose-styles", NULL,0,NULL,0,0,textview_ToggleExposeStyles,"Expose/hide style information", NULL},
    {"textview-toggle-color-styles", NULL,0,NULL,0,0,textview_ToggleColorStyles,"Show/don't show color styles", NULL},
    {"textview-edit-styles", NULL, 0, NULL, 0, textview_NotReadOnlyMenus, textview_ExposeStyleEditor,"Expose style editor", NULL},
    {"textview-insert-pagebreak", NULL,0,"Page~9,Insert Pagebreak~11",0,textview_NotReadOnlyMenus,textview_InsertPageBreak,"Add page break at this location.", NULL},
    {"textview-next-page", NULL,0,"Page~9,Next Page~12",0,0,textview_NextPage,"Frame text at next page break object", NULL},
    {"textview-last-page", NULL,0,"Page~9,Previous Page~13",0,0,textview_LastPage,"Frame text at last page break object", NULL},
    {"textview-insert-footnote", NULL,0,"Page~9,Insert Footnote~20",0,textview_NotReadOnlyMenus,textview_InsertFootnote,"Add footnote at this location.", NULL},
    {"contentv-make-window", NULL,0,"Page~9,Table of Contents~30",0,0,NULL,"Make a table of contents window","contentv"},
    {"textview-open-footnotes",NULL,0,"Page~9,Open Footnotes~22",0,0,textview_OpenFootnotes,"Open all footnotes", NULL},
    {"textview-close-footnotes",NULL,0,"Page~9,Close Footnotes~23",0,0, textview_CloseFootnotes,"Close all footnotes", NULL},
    {"textview-write-footnotes", NULL,0,NULL,0,0, textview_WriteFootnotes,"Write all footnotes", NULL},

    {"textview-show-styles", "\033s",0,NULL,0,0,textview_ShowStylesCmd,"Show styles at dot.", NULL},
    {"textview-show-styles", "\033'\033s", 0, NULL, 0, 0, textview_ShowStylesCmd, "Show styles at dot.", NULL},

    {"textview-insert-environment", "\033'\033l", 0, NULL, 0, 0, textview_InsertEnvironment, "Prompt for a style to use for inserting characters.", NULL},
    {"textview-show-insert-environment", "\033'\033?", 0, NULL, 0, 0, textview_DisplayInsertEnvironment, "Show the environment that will be used for inserting characters.", NULL},
    {"textview-left-insert-environment", "\033'\033D", 0, NULL, 0, 0, textview_LeftInsertEnvironmentCmd, "Move to the left the environment that will be used for inserting characters.", NULL},
    {"textview-right-insert-environment", "\033'\033C", 0, NULL, 0, 0, textview_RightInsertEnvCmd, "Move to the right the environment that will be used for inserting characters.", NULL},
    {"textview-up-insert-environment", "\033'\033A", 0, NULL, 0, 0, textview_UpInsertEnvironmentCmd, "Move up environment that will be used for inserting characters.", NULL},
    {"textview-down-insert-environment", "\033'\033B", 0, NULL, 0, 0, textview_DownInsertEnvironmentCmd, "Move down environment that will be used for inserting characters.", NULL},
    {"textview-up-insert-environment", "\033'\033u", 0, NULL, 0, 0, NULL, NULL, NULL},
    {"textview-down-insert-environment", "\033'\033d", 0, NULL, 0, 0, NULL, NULL, NULL},

    {styleString, "\033'\033i", (long) "italic", NULL, 0, 0, NULL, NULL, NULL},
    {styleString, "\033'\033b", (long) "bold", NULL, 0, 0, NULL, NULL, NULL},
    {styleString, "\033'\033^", (long) "superscript", NULL, 0, 0, NULL, NULL, NULL},
    {styleString, "\033'\033!", (long) "subscript", NULL, 0, 0, NULL, NULL, NULL},
    {styleString, "\033'\033_", (long) "underline", NULL, 0, 0, NULL, NULL, NULL},
    {styleString, "\033'\033t", (long) "typewriter", NULL, 0, 0, NULL, NULL, NULL},
    {styleString, "\033'\033+", (long) "bigger", NULL, 0, 0, NULL, NULL, NULL},
    {styleString, "\033'\033-", (long) "smaller", NULL, 0, 0, NULL, NULL, NULL},
    {styleString, "\033'\033=", (long) "center", NULL, 0, 0, NULL, NULL, NULL},
    {styleString, "\033'\033<", (long) "flushleft", NULL, 0, 0, NULL, NULL, NULL},
    {styleString, "\033'\033>", (long) "flushright", NULL, 0, 0, NULL, NULL, NULL},
    {styleString, "\033'\033\011", (long) "leftindent", NULL, 0, 0, NULL, NULL, NULL}, 

    {"textview-plainer", "\033'\033p", (long) "new", "Plainer~40", (long) "new", textview_NotReadOnlyMenus, textview_PlainerCmd, "Remove style.", NULL},
    {"textview-plainer", "\030\020", (long) "new", NULL, 0, 0, NULL, NULL, NULL},
    {"textview-plainest","\033'\033P", 0,"Plainest~41", 0, textview_NotReadOnlyMenus, textview_PlainestCmd, "Remove all enclosing styles.", NULL},
    {NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL, NULL}
};

struct keymap *textview_InitViCommandModeKeyMap(classInfo, Menus)
struct textview_classinfo *classInfo;
struct menulist **Menus;
{
    struct keymap *newKeymap = keymap_New();
    register long i;
    char str[2];
    struct proctable_Entry *dig;

    adjustBindings(textviewViCommandModeBindings);
    bind_BindList(textviewViCommandModeBindings, newKeymap, NULL, classInfo);

    dig=proctable_DefineProc("textview-digit", (procedure) textview_DigitCmd, classInfo, NULL, "Insert a character.");

    str[0] = ' ';
    str[1] = '\0';
    for (i = str[0] = '0'; i <= '9'; i = ++str[0])
	    keymap_BindToKey(newKeymap, str, dig, i);

    CheckStylePreferences(newKeymap, Menus);

    return newKeymap;
}

/*************** VI Input mode  key bindings *********************/
static struct bind_Description textviewViInputModeBindings[] = {

    {"textview-noop", NULL, 0, NULL, 0, 0, textview_NOOPCmd, "Do absolutely nothing.", NULL},

    {"textview-delete-previous-character", "\010",0,NULL,0,0,textview_RuboutCmd,"Delete the previous character.", NULL},

    {"textview-delete-previous-character", "\177",0, NULL},

    {"textview-insert-newline", "\015",0,NULL,0,0,textview_InsertNLCmd,"Insert a newline.", NULL},

    {"textview-toggle-mode", "\033\033",0,NULL,0,0,textview_ToggleViModeCmd,"Switch to command mode. ", NULL},

    {"textview-toggle-mode", "\005",0, NULL},

    {"textview-show-styles", "\033s",0,NULL,0,0,textview_ShowStylesCmd,"Show styles at dot.", NULL},
    {"textview-show-styles", "\033'\033s", 0, NULL, 0, 0, textview_ShowStylesCmd, "Show styles at dot.", NULL},

    {"textview-insert-environment", "\033'\033l", 0, NULL, 0, 0, textview_InsertEnvironment, "Prompt for a style to use for inserting characters.", NULL},
    {"textview-show-insert-environment", "\033'\033?", 0, NULL, 0, 0, textview_DisplayInsertEnvironment, "Show the environment that will be used for inserting characters.", NULL},
    {"textview-left-insert-environment", "\033'\033D", 0, NULL, 0, 0, textview_LeftInsertEnvironmentCmd, "Move to the left the environment that will be used for inserting characters.", NULL},
    {"textview-right-insert-environment", "\033'\033C", 0, NULL, 0, 0, textview_RightInsertEnvCmd, "Move to the right the environment that will be used for inserting characters.", NULL},
    {"textview-up-insert-environment", "\033'\033A", 0, NULL, 0, 0, textview_UpInsertEnvironmentCmd, "Move up environment that will be used for inserting characters.", NULL},
    {"textview-down-insert-environment", "\033'\033B", 0, NULL, 0, 0, textview_DownInsertEnvironmentCmd, "Move down environment that will be used for inserting characters.", NULL},
    {"textview-up-insert-environment", "\033'\033u", NULL},
    {"textview-down-insert-environment", "\033'\033d", NULL},

    {styleString, "\033'\033i", (long) "italic", NULL, 0, 0, NULL, NULL, NULL},
    {styleString, "\033'\033b", (long) "bold", NULL, 0, 0, NULL, NULL, NULL},
    {styleString, "\033'\033^", (long) "superscript", NULL, 0, 0, NULL, NULL, NULL},
    {styleString, "\033'\033!", (long) "subscript", NULL, 0, 0, NULL, NULL, NULL},
    {styleString, "\033'\033_", (long) "underline", NULL, 0, 0, NULL, NULL, NULL},
    {styleString, "\033'\033t", (long) "typewriter", NULL, 0, 0, NULL, NULL, NULL},
    {styleString, "\033'\033+", (long) "bigger", NULL, 0, 0, NULL, NULL, NULL},
    {styleString, "\033'\033-", (long) "smaller", NULL, 0, 0, NULL, NULL, NULL},
    {styleString, "\033'\033=", (long) "center", NULL, 0, 0, NULL, NULL, NULL},
    {styleString, "\033'\033<", (long) "flushleft", NULL, 0, 0, NULL, NULL, NULL},
    {styleString, "\033'\033>", (long) "flushright", NULL, 0, 0, NULL, NULL, NULL},
    {styleString, "\033'\033\011", (long) "leftindent", NULL, 0, 0, NULL, NULL, NULL},

    {"textview-plainer", "\033'\033p", (long) "new", "Plainer~40", (long) "new", textview_NotReadOnlyMenus, textview_PlainerCmd, "Remove style.", NULL},
    {"textview-plainer", "\030\020", (long) "new", NULL},
    {"textview-plainest","\033'\033P", 0,"Plainest~41", 0, textview_NotReadOnlyMenus, textview_PlainestCmd, "Remove all enclosing styles.", NULL},

    NULL
 };
 
struct keymap *textview_InitViInputModeKeyMap(classInfo, Menus)
struct textview_classinfo *classInfo;
struct menulist **Menus;
{
    struct keymap *newKeymap = keymap_New();
    register long i;
    unsigned char str[2];
    struct proctable_Entry *si, *proc;
    
    adjustBindings(textviewViInputModeBindings);
    bind_BindList(textviewViInputModeBindings, newKeymap, NULL, classInfo);

    si=proctable_DefineProc("textview-self-insert", (procedure) textview_SelfInsertCmd, classInfo, NULL, "Insert a character.");

    str[0] = ' ';
    str[1] = '\0';
    for (i = 32; i < 127; i++)  {
	keymap_BindToKey(newKeymap, str, si, i);
	str[0]++;
    }
    /* add bindings for iso keyboards */
    str[0] = (unsigned char ) 160;
    for (i = 160 ; i < 256; i++)  {
	keymap_BindToKey(newKeymap, str, si, i);
	str[0]++;
    }
    keymap_BindToKey(newKeymap, "\t", si, '\t');

    CheckStylePreferences(newKeymap, Menus);

    return newKeymap;
}


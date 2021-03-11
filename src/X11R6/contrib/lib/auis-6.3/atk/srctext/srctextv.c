/* File srctextv.c created by R L Quinn
 
   SrctextView, a Source Text mode for ATK. */
/* Copyright 1988,1994 Carnegie Mellon University and IBM. All rights reserved.
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
static char rcsid[] = "$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/srctext/RCS/srctextv.c,v 1.16 1994/02/28 21:02:02 rr2b Exp $";
#endif

#include <andrewos.h>
#include <ctype.h>
#include <class.h>

#include <im.ih>
#include <message.ih>
#include <keymap.ih>
#include <keystate.ih>
#include <menulist.ih>
#include <proctbl.ih>
#include <bind.ih>
#include <envrment.ih>

#include <frame.ih>
#include <framecmd.ih>
#include <rect.h>
#include <view.ih>
#include <cursor.ih>
#include <buffer.ih>
#include <content.ih>
#include <environ.ih>

#include "compress.ih"
#include "compressv.ih"
#include "srctext.ih"
#include "srctextv.eh"
#include "toolcnt.h"

/* AutoCut was not made externally visible by txtvcmod, so WE have to check the preference TOO */
static int autocut_mode = -1;	/* uninitialized */
#define IsAutoCutMode() ((autocut_mode == -1 && (autocut_mode = environ_GetProfileSwitch("autocut", FALSE))) || autocut_mode)

static struct keymap *src_Map;
static struct menulist *src_Menus;
static struct cursor *waitCursor;

void compress(),
     compressAll(),
     endComment(),
     forceupperon(),
     forceupperoff(),
     gotoColumn(),
     insertComment(),
     insertLineComment(),
     newline(),
     paren(),
     redo(),
     reformat(),
     reindent(),
     renameIdent(),
     retrn(),
     reindretrn(),
     selfinsert(),
     selfinsertreindent(),
     startComment(),
     startLineComment(),
     styleLabel(),
     styleString(),
     whatColumn();
void nextLongLine(),
     checkLineLengths();
void toggleOverstrike(), overstrikeOn(), overstrikeOff(); /*RSK91overstrike*/

static struct bind_Description srctextBindings[]={
    {"srctextview-self-insert"," ",' ', NULL,0,0, selfinsert, "Insert a character and check for a preceding keyword."},
    {"srctextview-self-insert","(",'('},
    {"srctextview-self-insert","[",'['},
    {"srctextview-self-insert","{",'{'},
    {"srctextview-self-insert",".",'.'},
    {"srctextview-self-insert",",",','},
    {"srctextview-self-insert",";",';'},
    {"srctextview-self-insert-reindent", NULL,0, NULL,0,0, selfinsertreindent, "Insert a character, check for preceding keyword, and reindent this line."},
    {"srctextview-redo-styles","\033r",0,"Source Text,Redo styles~10", 0,0, redo, "Wrap styles around comments, functions, keywords, etc in the source code."},
    {"srctextview-paren",")",')',NULL,0,0,paren,"Insert a paren, with balancing."},
    {"srctextview-paren","]",']'},
    {"srctextview-paren","}",'}'},
    {"srctextview-reformat", "\033\022",0, "Source Text,Format line/region~12",0,0, reformat, "Indent line or region and reflow comments."},
    {"srctextview-reindent", "\t", 0, "Source Text,Indent line/region~11", 0,0, reindent, "Indent the current line or selected region."},
    {"srctextview-tab", NULL,0, NULL,0,0, reindent, "(Obsolete, replaced by srctextview-reindent.)"},
    {"srctextview-newline", "\n", 0, NULL,0, 0, newline,"Insert a newline and indent properly."},
    {"srctextview-return", "\015", 0, NULL,0, 0, retrn,"Insert a newline."},
    {"srctextview-reindent-return", 0,0, NULL,0,0, reindretrn,"Indent current line properly and insert a newline."},
    {"srctextview-compress", NULL,0, "Source Text,Compress region~20",0,0, compress, "Compress a region of text for viewing general outline."},
    {"srctextview-compress-all", NULL,0, "Source Text,Compress all indents~21",0,0, compressAll, "Compress all lines indented (as far or) farther than the current line."},
    {"compressv-decompress-all", NULL,0, "Source Text,Decompress all~25",0,0},
    {"srctextview-rename-identifier","\033Q",0,"Source Text,Rename Identifier~80", 0,0,renameIdent,"Rename identifier in selection region."},
    {"srctextview-next-long-line", "\033\024",0, NULL,0,0, nextLongLine,"Find the next line exceeding max-length and highlight the offending characters."},
    {"srctextview-check-line-lengths", 0,0, NULL,0,0, checkLineLengths,"Display a warning box if any lines exceed max-length."},
    {"srctextview-toggle-overstrike-mode", "\033\034",0, NULL,0,0, toggleOverstrike, "Turn overstrike mode on or off."}, /*RSK91overstrike*/
    {"srctextview-overstrike-mode-on", NULL,0, NULL,0,0, overstrikeOn, "Turn overstrike mode on."}, /*RSK91overstrike*/
    {"srctextview-overstrike-mode-off", NULL,0, NULL,0,0, overstrikeOff, "Turn overstrike mode off."}, /*RSK91overstrike*/
    {"srctextview-what-column", 0,0, NULL,0,0, whatColumn,"Display in the message window what column the cursor is in."},
    {"srctextview-goto-column", 0,0, NULL,0,0, gotoColumn,"Prompt for a column number and move the cursor there, appending whitespace to if needed."},
    {"srctextview-insert-comment", "\0331", 0, NULL,0, 0, insertComment, "Inserts a comment at the end of the line."},
    {"srctextview-insert-linecomment", "\0332", 0, NULL,0, 0, insertLineComment, "Inserts a comment-to-end-of-line."},
    {"srctextview-start-comment", 0,0, NULL,0,0, startComment, "Begins comment style if part of a comment delimiter."},
    {"srctextview-end-comment", 0,0, NULL,0,0, endComment, "Ends a comment style if part of a comment delimiter."},
    {"srctextview-start-linecomment", 0,0, NULL,0,0, startLineComment, "Begins a comment to end of line."},
    {"srctextview-style-label", 0,0, NULL,0,0, styleLabel, "Puts a style on labels."},
    {"srctextview-style-string", 0,0, NULL,0,0, styleString, "Starts or ends a string style."},
    NULL
};

boolean srctextview__InitializeClass(classID)
struct classheader *classID;
{
    src_Menus = menulist_New();
    src_Map = keymap_New();
    bind_BindList(srctextBindings,src_Map,src_Menus,&srctextview_classinfo);
    waitCursor= cursor_Create(NULL);
    cursor_SetStandard(waitCursor,Cursor_Wait);
    return TRUE;
}

boolean srctextview__InitializeObject(classID, self)
struct classheader *classID;
struct srctextview *self;
{
    self->src_state = keystate_Create(self, src_Map);
    self->src_menus = menulist_DuplicateML(src_Menus, self);
    srctextview_SetBorder(self,5,5); 
    return TRUE;
}

void srctextview__FinalizeObject(classID, self)
struct classheader *classID;
struct srctextview *self;
{
    keystate_Destroy(self->src_state);
    menulist_Destroy(self->src_menus);
}

void srctextview__PostMenus(self, menulist)
struct srctextview *self;
struct menulist *menulist;
{
    menulist_ChainBeforeML(self->src_menus, menulist, self);
    super_PostMenus(self, self->src_menus);
}

/* override */
/* srctextview_ReceiveInputFocus does mostly the same thing as textview's, but it calls _PrependKeyState to allow srctextview and its subclasses to prepend their keystate overrides to its own, and does some funky stuff to add menus too. */ /*RSK92add*/
/*RSK92note: if you're a genius and want to clean this up, make sure your fix works in the following test case: edit a C program, embed a table inset in a comment, embed a ctext object INTO that table, and type slash-star and try the "Indent" menu for both the parent C program and the double-nested ctext object. If that still works OK, you're not getting paid enough; ask for a raise. */
void srctextview__ReceiveInputFocus(self)
struct srctextview *self;
{
    self->header.textview.hasInputFocus = TRUE;
    self->header.textview.keystate->next= NULL;
    if (self->header.textview.keystate == self->header.textview.viCommandModeKeystate)
	/* uh-oh, don't be messin' with vi-emulations's Command Mode! */
	srctextview_PostKeyState(self, self->header.textview.keystate);
    else
	srctextview_PostKeyState(self, srctextview_PrependKeyState(self));
    srctextview_PostMenus(self, self->header.textview.menus); /* I find this particularly nauseating, but it seems to do the trick */
    menulist_SetMask(self->src_menus, textview_NoMenus);
    if (srctextview_GetEditor(self) == VI)
	if (srctextview_GetVIMode(self) == COMMAND)
	    message_DisplayString(self, 0, "Command Mode");
        else
	    message_DisplayString(self, 0, "Input Mode");
    srctextview_WantUpdate(self, self);
}

/* srctextview_PrependKeyState is called by _ReceiveInputFocus, in order to set up all the subclasses' keybinding overrides. This is overridden by subclasses, who should call super_PrependKeyState and then return that, prepended with their OWN keystate. This is a pretty dorky system, but it succeeds in binding keys with the proper precedence. */ /*RSK92add*/
struct keystate *srctextview__PrependKeyState(self)
struct srctextview *self;
{
    self->src_state->next= NULL;
    return keystate_AddBefore(self->src_state, self->header.textview.keystate);
}

static void redo(self)
struct srctextview *self;
{
    srctextview_RedoStyles(self);
}

void srctextview__RedoStyles(self)
struct srctextview *self;
{
    struct srctext *c=(struct srctext *)srctextview_GetDataObject(self);
    srctextview_WaitCursorOn(self);
    srctext_RedoStyles(c);
    srctextview_WaitCursorOff(self);
    srctext_RegionModified(c, 0, srctext_GetLength(c));
    srctext_NotifyObservers(c, 0);
}

/* friendly read-only behavior stolen from txtvcmod.c */
boolean srctextview__ConfirmReadOnly(self)
struct srctextview *self;
{
    if (srctext_GetReadOnly((struct srctext *)srctextview_GetDataObject(self))) {
	message_DisplayString(self, 0, "Document is read only.");
	return TRUE;
    } else
	return FALSE;
}

/* override to disable "feature" of picking up the style at the front of a paragragh */
void srctextview__PrepareInsertion(self, insertingNewLine)
struct srctextview *self;
boolean insertingNewLine;
{
    return;
}

static void selfinsert(self, key)
struct srctextview *self;
char key;
{
    srctextview__SelfInsert(self,key);
}

void srctextview__SelfInsert(self, key)
struct srctextview *self;
char key; /* must be char for "&" to work. */
{
    struct srctext *ct=(struct srctext *)srctextview_GetDataObject(self);
    int count=im_Argument(srctextview_GetIM(self));
    long pos, oldpos;
    if (srctextview_ConfirmReadOnly(self))
	return;
    if (IsAutoCutMode() && srctextview_GetDotLength(self)>0)
	im_HandleMenu(srctextview_GetIM(self), proctable_Lookup("textview-zap-region"), self, 0); /* not a particularly efficient way to call textview_ZapRegionCmd, but what else ya gonna do? */
    pos= oldpos= srctextview_CollapseDot(self);
    while(count--)
	srctext_InsertCharacters(ct, pos++, &key, 1);
    if (oldpos)
	srctext_BackwardCheckWord(ct,oldpos-1,0);
    srctextview_SetDotPosition(self, pos);
    srctextview_FrameDot(self, pos);
    srctext_NotifyObservers(ct, 0);
}

static void selfinsertreindent(self, key)
struct srctextview *self;
char key;
{
    srctextview__SelfInsertReindent(self,key);
}

void srctextview__SelfInsertReindent(self, key)
struct srctextview *self;
char key; /* must be char for "&" to work. */
{
    struct srctext *ct=(struct srctext *)srctextview_GetDataObject(self);
    long pos;
    if (srctextview_ConfirmReadOnly(self))
	return;
    srctextview_SelfInsert(self,key);
    pos= srctextview_GetDotPosition(self);
    if (srctext_IndentingEnabled(ct) && !srctext_InCommentStart(ct,pos))
	srctext_ReindentLine(ct, pos);
}

static void reindent(self, key)
struct srctextview *self;
long key;
{
    srctextview_Reindent(self);
}

/* srctextview_Reindent is called when the Tab key is pressed.  If the cursor is in front of a line, it will reindent the line (if enable-indentation is on).  Otherwise, it'll jump to the next tab stop or multiple of tab size, depending on their settings.  If Overstrike mode is on, it will MOVE the cursor there instead of inserting whitespace to that point. */
void srctextview__Reindent(self)
struct srctextview *self;
{
    struct srctext *ct=(struct srctext *)srctextview_GetDataObject(self);
    long pos=srctextview_GetDotPosition(self), len=srctextview_GetDotLength(self);
    int c=0;
    struct mark *mark=srctext_CreateMark(ct,pos,len);

    if (len>0 && srctext_IndentingEnabled(ct)) {
	/* region selected; reindent it */
	srctextview_WaitCursorOn(self);
	srctext_Indent(ct,mark);
	srctextview_WaitCursorOff(self);
    }
    else {
	long oldPos=pos;
	if (len>0)
	    /* region selected; collapse it and add tabs at END of region */
	    pos= oldPos= srctextview_CollapseDot(self);
	do  {
	    pos--;
	} while (pos>=0 && (c=srctext_GetChar(ct,pos))!='\n' && isspace(c));

	if ((pos<0 || c=='\n') && srctext_IndentingEnabled(ct)) {
	    /* in front of the line; reindent it */
	    mark_SetPos(mark,pos+1);
	    srctextview_SetDotPosition(self,srctext_Indent(ct,mark));
	}
	else { /* in the middle/end of the line */
	    int tabstop=srctext_NextTabStop(ct, srctext_CurrentColumn(ct, oldPos));
	    if (oldPos)
		srctext_BackwardCheckWord(ct,oldPos-1,0);
	    if (tabstop==0) {
		/* past last tab stop; clean up whitespace and jump to next line */
		long eol=oldPos;
		if (srctext_IsInOverstrikeMode(ct))
		    /* in Insert mode, oldPos IS the (new) end of line. in Overstrike mode, it's NOT; find it */
		    eol= srctext_GetEndOfLine(ct,eol);
		if ((eol>=srctext_GetLength(ct) || !srctext_IsInOverstrikeMode(ct)) && !srctext_GetReadOnly(ct)) {
		    /* add a newline and check for bang-comment, because either we're in insert mode, or the end of the file */
		    srctext_InsertCharacters(ct,eol, "\n",1);
		    if (srctext_GetStyle(ct,eol) == ct->linecomment_style)
			environment_Remove(ct->header.text.rootEnvironment, eol,1, environment_Style,FALSE);
		}
		for (oldPos=eol; oldPos>0 && is_whitespace(srctext_GetChar(ct,oldPos-1)); oldPos--) ;
		if (oldPos<eol && !srctext_GetReadOnly(ct)) {
		    srctext_DeleteCharacters(ct, oldPos, eol-oldPos);
		    srctext_SetModified(ct);
		}
		srctextview_SetDotPosition(self,++oldPos);
	    }
	    else
		if (srctext_IsInOverstrikeMode(ct))
		    /* just MOVE the cursor, we're in Overstrike mode */
		    srctextview_GotoColumn(self,tabstop);
		else if (!srctext_GetReadOnly(ct))
		    /* insert whitespace to next tab stop, we're in Insert mode */
		    srctextview_SetDotPosition(self, srctext_TabAndOptimizeWS(ct, oldPos,tabstop));
	}
    }
    srctext_RemoveMark(ct,mark);
    mark_Destroy(mark);
    srctext_NotifyObservers(ct,0);
}

static void reformat(self, key)
struct srctextview *self;
long key;
{
    srctextview_Reformat(self);
}

/* Reformat calls the same ReindentLine routine as Ctrl-J, on the whole region. This will break up too-long lines, instead of just indenting. */
void srctextview__Reformat(self)
struct srctextview *self;
{
    struct srctext *ct=(struct srctext *)srctextview_GetDataObject(self);
    long pos=srctextview_GetDotPosition(self), len=srctextview_GetDotLength(self);
    struct mark *endregion, *endofline, *endofflow;
    boolean flowed, oldReindCmmntsValue=ct->reindentComments;
    if (srctextview_ConfirmReadOnly(self))
	return;
    endregion= srctext_CreateMark(ct,pos+len,0);
    endofline= srctext_CreateMark(ct,pos,0);
    endofflow= srctext_CreateMark(ct,0,0);
    ct->reindentComments= TRUE; /* formatter ignores value of "reindent-comments" */
    if (len>0) srctextview_WaitCursorOn(self);
    /* put pos at beginning of line so its value will still be valid after the ReindentLine call */
    pos= srctext_GetBeginningOfLine(ct,pos);
    do  {
	srctext_ReindentLine(ct,pos);
	/* WARNING: the call to ReindentLine invalidates the value of pos; (we assume it's still on the correct line, though) */
	pos= srctext_GetEndOfLine(ct,pos);
	mark_SetPos(endofline,pos);
	/* remove trailing whitespace */
	while (is_whitespace(srctext_GetChar(ct,pos-1))) --pos;
	if (pos<mark_GetPos(endofline))
	    srctext_DeleteCharacters(ct,pos,mark_GetPos(endofline)-pos);
	/* pull the following comment up with this one if this one isn't blank */
	flowed= srctext_GetChar(ct, pos-1)!='\n' && srctext_ReflowComment(ct,pos+1);
	if (flowed)
	    /* move mark to the new end of the line */
	    mark_SetPos(endofflow, srctext_GetEndOfLine(ct,mark_GetPos(endofline)));
	if (srctext_GetMaxLineLength(ct))
	    /* break the extras back off so it fits in max-length */
	    srctext_BreakLine(ct, flowed ? endofflow : endofline);
	if (flowed) /* reformat the last line of the flowed result */
	    pos= mark_GetPos(endofflow);
	else /* we're all done here; move to next line */
	    pos= mark_GetPos(endofline)+1;
    } while (pos<mark_GetPos(endregion));
    ct->reindentComments= oldReindCmmntsValue; /* restore old value of "reindent-comments" */
    srctext_RemoveMark(ct,endregion);
    srctext_RemoveMark(ct,endofline);
    srctext_RemoveMark(ct,endofflow);
    mark_Destroy(endregion);
    mark_Destroy(endofline);
    mark_Destroy(endofflow);
    if (len>0) srctextview_WaitCursorOff(self);
    srctext_NotifyObservers(ct,0);
}

static void retrn(self,key)   
struct srctextview *self;
long key;
{
    srctextview_HandleNewlineAndRetrn(self,key,FALSE,FALSE);
}

static void reindretrn(self,key)
struct srctextview *self;
long key;
{
    srctextview_HandleNewlineAndRetrn(self,key,TRUE,FALSE);
}

static void newline(self, key)
struct srctextview *self;
long key;
{
    srctextview_HandleNewlineAndRetrn(self,key,TRUE,TRUE);
}

/* HandleEndOfLineStyle will terminate linecomments when a newline is inserted. Override it if there are *other* such styles (such as preprocessor style in C) */
void srctextview__HandleEndOfLineStyle(self, pos)
struct srctextview *self;
long pos;
{
    struct srctext *ct = (struct srctext *)srctextview_GetDataObject(self);

    if (srctext_GetStyle(ct,pos)==ct->linecomment_style) {
	long start=pos;
	while (srctext_GetChar(ct,start-1)=='\n' && !srctext_Quoted(ct,start-1)) --start;
	environment_Remove(ct->header.text.rootEnvironment, start,pos-start+1, environment_Style, FALSE);
    }
}

void srctextview__HandleNewlineAndRetrn(self,key, reindentThisLine,preindentNewLine)
struct srctextview *self;
long key;
boolean reindentThisLine, preindentNewLine;
{
    int newlines = im_Argument(srctextview_GetIM(self));
    struct srctext *ct = (struct srctext *)srctextview_GetDataObject(self);
    struct mark *endofline, *startofnext;
    int c;
    long pos,end;

    if (srctextview_ConfirmReadOnly(self))
	return;
    if (IsAutoCutMode() && srctextview_GetDotLength(self)>0)
	im_HandleMenu(srctextview_GetIM(self), proctable_Lookup("textview-zap-region"), self, 0); /* not a particularly efficient way to call textview_ZapRegionCmd, but what else ya gonna do? */
    pos= srctextview_CollapseDot(self);
    if (pos)
	srctext_BackwardCheckWord(ct,pos-1,0);

    /* delete trailing whitespace from line key was pressed on */
    end= pos;
    while(pos>0 && ((c=srctext_GetChar(ct,pos-1))==' ' || c=='\t'))
	pos--;
    if (pos<end)
	srctext_DeleteCharacters(ct,pos,end-pos);

    /* insert newlines and remember significant positions */
    for (end=newlines; end>0; end--)
	srctext_InsertCharacters(ct,pos++,"\n",1);
    endofline= srctext_CreateMark(ct,pos-newlines,0);
    startofnext= srctext_CreateMark(ct, srctext_SkipWhitespace(ct,pos, srctext_GetLength(ct))+1,0);
    
    /* terminate any bang-comments that might be there */
    srctextview_HandleEndOfLineStyle(self, pos-1);

    /* avoid reindenting hassles at very beginning of file */
    if (mark_GetPos(endofline)<1)
	goto DoneReindenting;

    /* reindent the (non-blank) line the key was pressed on (Newline & ReindRetrn) */
    if (reindentThisLine && srctext_GetChar(ct,mark_GetPos(endofline)-1)!='\n')
	srctext_ReindentLine(ct,mark_GetPos(endofline));

    /* break up line to enforce max-length if need be */
    if (srctext_GetMaxLineLength(ct))
	srctext_BreakLine(ct,endofline);

    /* preindent the new line that was just inserted (Newline only) */
    if (preindentNewLine) {
	/* delete any existing leading whitespace in case ^J hit in middle of line */
	long strt=mark_GetPos(startofnext)-1;
	while(strt>0 && ((c=srctext_GetChar(ct,strt-1))==' ' || c=='\t'))
	    strt--;
	if (strt<mark_GetPos(startofnext)-1)
	    srctext_DeleteCharacters(ct,strt,mark_GetPos(startofnext)-1-strt);
	/* indent this new line the way it oughta be, even if it's in a comment */
	pos= srctext_TabAndOptimizeWS(ct,mark_GetPos(startofnext)-1,srctext_Indentation(ct,mark_GetPos(startofnext)-1));
    }
    else /* leave caret in left margin (Retrn or ReindRetrn) */
	pos= mark_GetPos(endofline)+newlines;

    DoneReindenting: /* jumped here if at very beginning of file */
    srctextview_SetDotPosition(self,pos);
    srctextview_FrameDot(self,pos);
    srctext_RemoveMark(ct,endofline);
    srctext_RemoveMark(ct,startofnext);
    mark_Destroy(endofline);
    mark_Destroy(startofnext);
    srctext_NotifyObservers(ct,0);
}

/* MatchParens will wrap the cursor around the matched parens, *even if* the starting one is off the screen. */
void srctextview__MatchParens(self, key)
struct srctextview *self;
char key;
{
    struct srctext *ct = (struct srctext *)srctextview_GetDataObject(self);
    long start = srctextview_GetDotPosition(self), openparen = srctext_ReverseBalance(ct, start), pos;
    char buffer[256], *ptr;
    int i;

    if (openparen != EOF) {
        if (!srctextview_Visible(self, openparen)) {
            for (pos = openparen; pos > 0; pos--) {
                if (srctext_GetChar(ct, pos) == '\n') {
                    pos++;
                    break;
                }
            }

            ptr = buffer;
            for (i = sizeof(buffer) - 1; i > 0; i--)
                if ((*ptr++ = srctext_GetChar(ct, pos++)) == '\n')
                    break;
            *ptr = NULL;

            message_DisplayString(self, 0, buffer);
	}
	if (!IsAutoCutMode()) {
	    /* wrap region whether openparen is visible or not */
	    srctextview_SetDotPosition(self, openparen);
	    srctextview_SetDotLength(self, start - openparen);
	}
    }
    else
        switch (key) {
            case '}':
                message_DisplayString(self, 0, "No matching open brace.");
                break;
            case ')':
                message_DisplayString(self, 0, "No matching open parenthesis");
                break;
            case ']':
                message_DisplayString(self, 0, "No matching open bracket.");
                break;
            default:
                message_DisplayString(self, 0, "Parenthesis mis-match.\n");
        }
}

static void paren(self,key)
struct srctextview *self;
char key;
{
    srctextview_Paren(self,key);
}

/* Paren is overridden by modtextview, because it must check for nested comments and such */
void srctextview__Paren(self, key)
struct srctextview *self;
char key; /* must be char for "&" to work. */
{
    struct srctext *ct = (struct srctext *)srctextview_GetDataObject(self);
    long pos=srctextview_GetDotPosition(self);
    srctextview_SelfInsert(self,key);
    if (!srctext_InCommentStart(ct,pos) && !srctext_InString(ct,pos))
	srctextview_MatchParens(self, key);
}

static void forceupperon(self, rock)
struct srctextview *self;
long rock; /* required by key bindings, but not used */
{
    srctextview_ForceUpperOn(self);
}

/* Set the data object's data item forceUpper to TRUE and */
/* change the menu list entry to Force Upper Off */    /*DHGadd*/
void srctextview__ForceUpperOn(self)
struct srctextview *self;
{
    struct proctable_Entry *proc;

    srctext_SetForceUpper((struct srctext *)srctextview_GetDataObject(self),TRUE);

    /* delete existing menu item */
    menulist_DeleteFromML(self->src_menus, "Source Text,Force Upper On~90");

    /* define the procedure for the new menu option */
    proc = proctable_DefineProc("srctextview-force-upper-off", forceupperoff, &srctextview_classinfo, NULL, "Turn off auto upper casing of keywords");

    /* add Force Upper Off option to Source Text menu card */
    menulist_AddToML(self->src_menus, "Source Text,Force Upper Off~90", proc, 0, 0);

    srctextview_WantInputFocus(self, self);
}

static void forceupperoff(self, rock)
struct srctextview *self;
long rock; /* required by key bindings, but not used */
{
    srctextview_ForceUpperOff(self);
}

/* Set the data object's data item forceUpper to FALSE and */
/* change the menu list entry to Force Upper On */    /*DHGadd*/
void srctextview__ForceUpperOff(self)
struct srctextview *self;
{
    struct proctable_Entry *proc;

    srctext_SetForceUpper((struct srctext *)srctextview_GetDataObject(self),FALSE);

    /* delete existing menu item */
    menulist_DeleteFromML(self->src_menus, "Source Text,Force Upper Off~90");

    /* define the procedure for the new menu option */
    proc = proctable_DefineProc("srctextview-force-upper-on", forceupperon, &srctextview_classinfo, NULL, "Turn on auto upper casing of keywords");

    /* add Force Upper On option to Source Text menu card */
    menulist_AddToML(self->src_menus, "Source Text,Force Upper On~90", proc, 0, 0);

    srctextview_WantInputFocus(self, self);
}

/*-- Override-- */
/* Associate the view with the data object, and */
/* initialize the force upper on/off menu option */
void srctextview__SetDataObject( self, dataobj )    /*DHGadd*/
struct srctextview *self;
struct srctext *dataobj;
{
    /*struct proctable_Entry *proc;*/

    /* call overriden SetDataObject */
    super_SetDataObject( self, dataobj );

    if ( srctext_GetForceUpper( dataobj ) )
	forceupperon(self,0);
    else
	forceupperoff(self,0);

    if (srctext_GetMaxLineLength(dataobj))
	menulist_AddToML(self->src_menus, "Source Text,Find Next too-long Line~60", proctable_Lookup("srctextview-next-long-line"), 0, 0);
    else
	menulist_DeleteFromML(self->src_menus, "Source Text,Find Next too-long Line~60");
}

void srctextview__DeleteMenuItem(self,menuitem)
struct srctextview *self;
char menuitem[];
{
    menulist_DeleteFromML(self->src_menus, menuitem);
    return;
}

static void compress(self, key)
struct srctextview *self;
long key;
{
    srctextview_Compress(self);
}

void srctextview__Compress(self)
struct srctextview *self;
{
    struct srctext *ct=(struct srctext *)srctextview_GetDataObject(self);
    long pos=srctextview_GetDotPosition(self), len=srctextview_GetDotLength(self);
    long modValue=srctext_GetModified(ct);

    if (len==0) {
	long ind;
	if (srctext_GetChar(ct,pos)=='\n') --pos;
	if ((ind=srctext_CurrentIndent(ct,pos)) < 2 || pos<1) {
	    /* don't bother trying to compress the WHOLE FILE! */
	    message_DisplayString(self,0, "Deliberately ignoring your request to compress the entire file into a box.");
	    return;
	}
	srctext_ExtendToOutdent(ct,ind, &pos,&len);
    }
    compress_Compress(ct,pos,len);

    srctextview_CollapseDot(self);
    srctext_RestoreModified(ct,modValue);
    srctext_NotifyObservers(ct,0);
    ToolCount("EditViews-compress",NULL);
}

static void compressAll(self, key)
struct srctextview *self;
long key;
{
    srctextview_CompressAll(self);
}

void srctextview__CompressAll(self)
struct srctextview *self;
{
    struct srctext *ct=(struct srctext *)srctextview_GetDataObject(self);
    long pos=1, oldpos=srctextview_CollapseDot(self), srclen;
    long count, nextcount=srctext_GetLength(ct)-4096;
    int curInd;
    long modValue=srctext_GetModified(ct);

    if (srctext_GetChar(ct,oldpos)=='\n') --oldpos;
    curInd= srctext_CurrentIndent(ct,oldpos);
    if (curInd<2 || oldpos<1) {
	/* don't bother trying to compress the WHOLE FILE! */
	message_DisplayString(self,0, "Deliberately ignoring your request to compress the entire file into a box.");
	return;
    }
    srctextview_WaitCursorOn(self);
    do	{
	long startcompress, len;
	srclen=srctext_GetLength(ct);
	/* find next line indented as far as, or farther than, curInd */
	while (pos<srclen && srctext_CurrentIndent(ct,pos)<curInd)
	    pos= srctext_GetEndOfLine(ct,pos) +1;
	startcompress= pos;
	if (pos<srclen)
	    if (srctext_ExtendToOutdent(ct,curInd, &startcompress,&len)) {
		compress_Compress(ct,startcompress,len);
		pos= startcompress+1; /* move past the new inset */
		count= srclen-pos;
		if (count < nextcount) {
		    /* only do this at minimal intervals of 4K chars, to avoid excessive message-flashing */
		    char countdown[256];
		    nextcount= count-4096;
		    /* shr 10 bits to produce a value more meaningful than a jumpy 6-digit number */
		    sprintf(countdown, "compressing (countdown: %ld)", count >> 12);
		    message_DisplayString(self,0, countdown);
		    im_ForceUpdate();
		}
	    } else {
		pos= startcompress+len+1+1; /* skip insignificant indented region */
	    }
    } while (pos<srclen); /* srclen may be invalid, but no matter. we'll figure that out on the next loop */
    message_DisplayString(self,0, "compressing completed.");
    srctextview_WaitCursorOff(self);
    srctext_RestoreModified(ct,modValue);
    srctext_NotifyObservers(ct,0);
    ToolCount("EditViews-compress",NULL);
}

/* This method is only around for backward compatibility with things like watson */
/* dummy is a dummy parameter left here for no reason other than to be consistent with previous versions. */
void srctextview__DecompressAll(self,dummy)
struct srctextview *self;
long dummy;
{
    struct srctext *ct=(struct srctext *)srctextview_GetDataObject(self);
    long modValue=srctext_GetModified(ct);
    compress_DecompressAll(ct);
    srctext_RestoreModified(ct,modValue);
    srctext_NotifyObservers(ct,0);
}

static void renameIdent(self, key)
struct srctextview *self;
long key;
{
    srctextview_RenameIdent(self);
}

void srctextview__RenameIdent(self)
struct srctextview *self;
{
    struct srctext *ct = (struct srctext *)srctextview_GetDataObject(self);
    long pos, len, newlen;
    char promptbuf[256], *prompt;
    static char defaultorig[40];
    static char defaultrep[40];
    char orig[40], rep[40];
    int origlen, replen;
    int occurrences=0;

    defaultorig[0]=defaultrep[0]='\0';
    
    if (srctextview_ConfirmReadOnly(self))
	return;
    pos = srctextview_GetDotPosition(self);
    newlen = len = srctextview_GetDotLength(self);

    if (len == 0) {
        message_DisplayString(self, 0, "No region selected\n");
        return;
    }

    /* prompt for original identifier name */
    if (defaultorig[0]) {
	sprintf(promptbuf, "Replace identifier [%s] : ", defaultorig);
	prompt= promptbuf;
    }
    else
	prompt= "Replace identifier : ";
    if (message_AskForString(self, 0, prompt, NULL, orig, sizeof(orig)) < 0) {
	return;
    }
    if (!orig[0] && defaultorig[0])
	/* nothing entered, use default string */
	sprintf(orig, "%s", defaultorig);
    if (!orig[0]) {
	message_DisplayString(self, 0, "Cancelled.");
	return;
    }

    /* prompt for what to rename it to */
    if (defaultrep[0]) {
	sprintf(promptbuf, "New identifier name [%s] : ", defaultrep);
	prompt= promptbuf;
    }
    else
	prompt= "New identifier name : ";
    if (message_AskForString(self, 0, prompt, NULL, rep, sizeof(rep)) < 0) {
	return;
    }
    if (!rep[0] && defaultrep[0])
	/* nothing entered, use default string */
	sprintf(rep, "%s", defaultrep);
    if (!rep[0]) {
	message_DisplayString(self, 0, "Cancelled.");
	return;
    }

    /* skip any identifier partially outside region */
    if (pos>0 && srctext_IsTokenChar(ct, srctext_GetChar(ct,pos-1)))
	while (len>0 && srctext_IsTokenChar(ct, srctext_GetChar(ct,pos)))
	    pos++, len--;
    origlen = strlen(orig);
    replen = strlen(rep);

    /* hunt down identifiers and replace them */
    for (; len >= origlen; pos++, len--)
	if (srctext_IsTokenChar(ct,srctext_GetChar(ct,pos)) &&
	    !srctext_Quoted(ct,pos) &&
	    srctext_Strncmp(ct,pos,orig,origlen)==0 &&
	    !srctext_IsTokenChar(ct,srctext_GetChar(ct,pos+origlen)) &&
	    !srctext_IsTokenChar(ct,srctext_GetChar(ct,pos-1)) &&
	    !srctext_InCommentStart(ct,pos) &&
	    !srctext_InString(ct,pos)) {
	    /* found a legit match. change it */
	    srctext_ReplaceCharacters(ct, pos,origlen, rep,replen);
	    newlen+= replen-origlen;
	    pos+= replen-origlen;
	    occurrences++;
	}
    srctextview_SetDotLength(self, newlen);
    strcpy(defaultorig, orig);
    strcpy(defaultrep, rep);
    if (occurrences)
	sprintf(promptbuf, "Replaced %d occurrences.", occurrences);
    else
	sprintf(promptbuf, "No occurrences of identifier '%s' found.", orig);
    message_DisplayString(self, 0, promptbuf);
    srctext_NotifyObservers(ct, 0);
}

/*-----------------RSKmod copied from 'compile.c'-------------------*/
struct finderInfo {
    struct frame *myFrame, *otherFrame, *bestFrame;
    struct buffer *myBuffer;
};
static boolean FrameFinder(frame, info)
struct frame *frame;
struct finderInfo *info;
{
    struct rectangle bogus;

    if (info->myFrame == frame)
        return FALSE;
    if (info->otherFrame != NULL && info->bestFrame != NULL)
        return TRUE;
    frame_GetVisualBounds(frame, &bogus);
    if (!rectangle_IsEmptyRect(&bogus))
        if (frame_GetBuffer(frame) == info->myBuffer) {
            info->bestFrame = frame;
            return FALSE;
        }
        else {
            if (frame_GetBuffer(frame) != NULL) {
                info->otherFrame = frame;
                return FALSE;
            }
        }
    return FALSE;
}
static ViewEqual(frame, view)
struct frame *frame;
struct view *view;
{
#if 1
    return (frame_GetView(frame) == view);
#else /* 1 */
    return (view_GetIM((struct view *) frame) == view_GetIM(view))
#endif /* 1 */
}
static struct frame *FindByView(view)
struct srctextview *view;
{
    return frame_Enumerate(ViewEqual, view);
}
/* Find a window other than the one that contains this inset.  Create one if we have to. */
static struct view *PutInAnotherWindow(view, buffer, forceWindow)
struct srctextview *view;
struct buffer *buffer;
int forceWindow;
{
    boolean FrameFinder();
    struct frame *frame;
    struct finderInfo myInfo;

    myInfo.myFrame = FindByView(view);
    myInfo.otherFrame = NULL;
    myInfo.bestFrame = NULL;
    myInfo.myBuffer = buffer;
    frame_Enumerate(FrameFinder, &myInfo);
#ifdef NEVER_WASTE_SCREEN_SPACE
    frame = (myInfo.bestFrame != NULL) ? myInfo.bestFrame : ((myInfo.otherFrame != NULL) ? myInfo.otherFrame : NULL);
#else /*NEVER_WASTE_SCREEN_SPACE*/
    frame= (myInfo.bestFrame != NULL) ? myInfo.bestFrame : NULL;
    if (!frame && environ_GetProfileSwitch("WasteScreenSpace",TRUE)==FALSE && myInfo.otherFrame!=NULL)
	frame= myInfo.otherFrame;
#endif /*NEVER_WASTE_SCREEN_SPACE*/
    if (frame == NULL) {
        struct im *newIM;

        if (!forceWindow)
            return NULL;
        newIM = im_Create(NULL);
        frame = frame_Create(buffer);
        im_SetView(newIM, frame);

        /* This is here because the frame_Create call can't set the input focus because the frame didn't have a parent when it called view_WantInputFocus.  This is bogus but hard to fix... */
        view_WantInputFocus(frame_GetView(frame), frame_GetView(frame));
    }
    else {
	struct im *im;
	if (frame_GetBuffer(frame) != buffer)
	    frame_SetBuffer(frame, buffer, TRUE);
	im= frame_GetIM(frame);
	if (im) im_ExposeWindow(im); /* make sure window gets uniconified */
    }
    return frame_GetView(frame);
}
/*------------------------------------------------------*/

static long tcpos(txt,strng)
struct content *txt;
char *strng;
{
    long pos=0, end=content_GetLength(txt);
    char buf[256];
    while (pos<end)
    {
        int i=0;
        while (i<256 && pos+i<end && (buf[i]=content_GetChar(txt,pos+i))!='\n')
            i++;
        buf[i]='\0';
        if (strcmp(strng,buf)==0)
            return pos;
        pos+=i+1;
    }
    return -1;
}

void srctextview__PutFileIntoNewWindow(self, bufname, proc, filename)
struct srctextview *self;
char *bufname,*proc,*filename;
{
    struct view *newView=0;
    struct view *view=(struct view *)(srctextview_GetIM(self)->topLevel);
    struct buffer *bf;
    static struct content *TofC=NULL;
    struct mark *loc;
    if ((bf=buffer_FindBufferByFile(filename))==NULL)
    {
        bf=buffer_GetBufferOnFile(filename,FALSE);
        if (bf)
        {
            buffer_ReadFile(bf,filename);
            buffer_SetName(bf,bufname);
        }
    }
    if (bf)
    {
        if (view->dataobject!=buffer_GetData(bf)) /*not viewing Int/Mod already*/
            newView=PutInAnotherWindow(self,bf,TRUE);
        if (proc && strlen(proc)>0)
        {
            if (!TofC)
                TofC=content_New();
            if (TofC)
            {
                long pos;
                content_SetSourceText(TofC,buffer_GetData(bf));
                pos=tcpos(TofC,proc);
                if (pos<0) return;
                loc=content_locate(TofC, pos);
                if (loc)
                {
                    struct srctextview *mnewv=(struct srctextview *)newView;
                    srctextview_SetDotPosition(mnewv,mark_GetPos(loc));
                    srctextview_SetDotLength(mnewv,mark_GetLength(loc));
                    srctextview_FrameDot(mnewv,mark_GetPos(loc));
                }
            }
        }
    }
}

/* fileExists(self,filename) returns true if "filename" exists */
static boolean fileExists(self, filename)
    struct srctextview *self;
    char *filename;
    {
    FILE *fp;
    if ((fp=fopen(filename,"r"))==NULL)
	return FALSE;
    else
	{
	fclose(fp);
	return TRUE;
        }
    }

/* FindSubFile() attempts to locate "filename" somewhere in the "searchpath", and then PutFileIntoNewWindow() */
boolean srctextview__FindSubFile(self, filename, bufname, procname, searchpath)
    struct srctextview *self;
    char *filename,*bufname,*procname,*searchpath;
    {
    char path[1024];
    long pos=0,oldpos=0;

    /*check directory of source file*/
    struct dataobject *dtobj= (struct dataobject *)srctextview_GetDataObject(self);
    struct buffer *buf= buffer_FindBufferByData(dtobj);
    strcpy(path,buffer_GetFilename(buf));
    *(rindex(path,'/')+1)='\0'; /*chop off filename to get source's path*/
    strcat(path,filename);
    if (fileExists(self,path))
	{
	srctextview_PutFileIntoNewWindow(self,bufname,procname,path);
	return TRUE;
        }
    
    /*check current working directory*/
    strcpy(path,"./\0"); strcat(path,filename);
    if (fileExists(self,path))
	{
	srctextview_PutFileIntoNewWindow(self,bufname,procname,path);
	return TRUE;
        }

    /*check Search Path*/
    if (searchpath!=NULL && strlen(searchpath)>0 && searchpath[0]!=' ')
        {
	while (pos<strlen(searchpath))
	    {
	    while (pos<strlen(searchpath) && searchpath[pos]!=':') pos++;
	    strncpy(path,searchpath+oldpos,pos-oldpos);
	    path[pos-oldpos]='\0';
	    oldpos=(++pos);
	    strcat(path,"/\0"); strcat(path,filename);
	    if (fileExists(self,path))
	        {
		srctextview_PutFileIntoNewWindow(self,bufname,procname,path);
		return TRUE;
	        }
	    }
        }
    return FALSE;
    }

static void insertComment(self,key)   
struct srctextview *self;
long key;
{
    srctextview_InsertComment(self);
}

/* InsertComment gets called when Esc-1 is pressed in the view, to insert a comment. If there's already a comment there, it will be reformatted */
void srctextview__InsertComment(self)
struct srctextview *self;
{
    struct srctext *ct=(struct srctext *)srctextview_GetDataObject(self);
    long endofline, pos;
    int desiredCol=ct->commentCol, linelength, spaceWanted;
    if (srctextview_ConfirmReadOnly(self))
	return;
    /* first make sure this language HAS comments */
    if (ct->commentString==NULL)
	return;
    pos= srctextview_CollapseDot(self);
    endofline= srctext_GetEndOfLine(ct,pos);
    if (srctext_GetStyle(ct,endofline-1)==ct->comment_style) {
	/* already a comment there! just reformat it */
	srctextview_Reformat(self);
	return;
    }
    /* remove trailing whitespace, but remember how much in case the user explicitly ADDED trailing whitespace */
    linelength= srctext_CurrentColumn(ct,endofline);
    while (is_whitespace(srctext_GetChar(ct,endofline-1)))
	srctext_DeleteCharacters(ct,--endofline,1);
    spaceWanted= linelength-srctext_CurrentColumn(ct,endofline);
    if (spaceWanted<ct->remarkPadding)
	spaceWanted= ct->remarkPadding;
    linelength= srctext_CurrentColumn(ct,endofline);
    if (linelength+spaceWanted>desiredCol)
	if (ct->commentFixed)
	    /* it's not going to fit and it MUST be in commentCol, so start a new line */
	    srctext_JustInsertCharacters(ct,endofline++,"\n",1);
	else
	    /* we just have to nudge it over a little */
	    desiredCol= linelength+spaceWanted;
    if (srctext_CurrentColumn(ct,endofline)<desiredCol)
	endofline= srctext_TabAndOptimizeWS(ct,endofline,desiredCol);
    /* insert the comment and style it */
    srctext_JustInsertCharacters(ct, endofline, ct->commentString, strlen(ct->commentString));
    srctext_WrapStyleNow(ct, endofline,strlen(ct->commentString), ct->comment_style, FALSE,FALSE);
    /* move cursor to middle of comment */
    srctextview_SetDotPosition(self, endofline+strlen(ct->commentString)/2);
    srctext_NotifyObservers(ct,0);
}

static void insertLineComment(self,key)  
struct srctextview *self;
long key;
{
    srctextview_InsertLineComment(self);
}

/* InsertLineComment gets called when Esc-2 is pressed in the view, to insert a line-comment. If there's already a comment there, it will be reformatted */
void srctextview__InsertLineComment(self)
struct srctextview *self;
{
    struct srctext *ct=(struct srctext *)srctextview_GetDataObject(self);
    long endofline, pos;
    int desiredCol=ct->linecommentCol, linelength, spaceWanted;
    if (srctextview_ConfirmReadOnly(self))
	return;
    /* first make sure this language HAS line-comments */
    if (ct->linecommentString==NULL)
	return;
    pos= srctextview_CollapseDot(self);
    endofline= srctext_GetEndOfLine(ct,pos);
    if (srctext_GetStyle(ct,endofline-1)==ct->linecomment_style) {
	/* already a line-comment there! just reformat it */
	srctextview_Reformat(self);
	return;
    }
    /* remove trailing whitespace, but remember how much in case the user explicitly ADDED trailing whitespace */
    linelength= srctext_CurrentColumn(ct,endofline);
    while (is_whitespace(srctext_GetChar(ct,endofline-1)))
	srctext_DeleteCharacters(ct,--endofline,1);
    spaceWanted= linelength-srctext_CurrentColumn(ct,endofline);
    if (spaceWanted<ct->remarkPadding)
	spaceWanted= ct->remarkPadding;
    linelength= srctext_CurrentColumn(ct,endofline);
    if (linelength+spaceWanted>desiredCol)
	if (ct->linecommentFixed)
	    /* it's not going to fit and it MUST be in linecommentCol, so start a new line */
	    srctext_JustInsertCharacters(ct,endofline++,"\n",1);
	else
	    /* we just have to nudge it over a little */
	    desiredCol= linelength+spaceWanted;
    if (srctext_CurrentColumn(ct,endofline)<desiredCol)
	endofline= srctext_TabAndOptimizeWS(ct,endofline,desiredCol);
    /* insert the line-comment and style it */
    srctext_JustInsertCharacters(ct, endofline, ct->linecommentString, strlen(ct->linecommentString));
    if (!srctext_InCommentStart(ct,endofline))
	srctext_WrapStyleNow(ct, endofline,strlen(ct->linecommentString), ct->linecomment_style, FALSE,TRUE);
    /* move cursor to end of line-comment */
    srctextview_SetDotPosition(self, endofline+strlen(ct->linecommentString));
    srctext_NotifyObservers(ct,0);
}

static void startComment(self,key)
struct srctextview *self;
char key; /* must be char for "&" to work. */
{
    srctextview_StartComment(self,key);
}

/* srctextview_StartComment is written to handle slash-star comments, but not mapped in the proctable. To enable them in ctextview or plxtextview, simply bind them. */
void srctextview__StartComment(self,key)
struct srctextview *self;
char key; /* must be char for "&" to work. */
{
    struct srctext *ct=(struct srctext *)srctextview_GetDataObject(self);
    int count=im_Argument(srctextview_GetIM(self));
    long pos, oldpos;
    if (srctextview_ConfirmReadOnly(self))
	return;
    if (IsAutoCutMode() && srctextview_GetDotLength(self)>0)
	im_HandleMenu(srctextview_GetIM(self), proctable_Lookup("textview-zap-region"), self, 0); /* not a particularly efficient way to call textview_ZapRegionCmd, but what else ya gonna do? */
    pos= oldpos= srctextview_CollapseDot(self);
    while (count--) srctext_InsertCharacters(ct, pos++, &key, 1);
    if (oldpos && srctext_GetChar(ct,oldpos-1)=='/' && !srctext_GetStyle(ct,oldpos-1) && !srctext_InString(ct,oldpos))
	srctext_WrapStyleNow(ct, oldpos-1,pos-oldpos+1, ct->comment_style, FALSE,TRUE);
    srctextview_SetDotPosition(self, pos);
    srctextview_FrameDot(self, pos);
    srctext_NotifyObservers(ct, 0);
}

static void endComment(self,key)
struct srctextview *self;
char key; /* must be char for "&" to work. */
{
    srctextview_EndComment(self,key);
}

/* srctextview_EndComment is written to handle slash-star comments, but not mapped in the proctable. To enable them in ctextview or plxtextview, simply bind them. */
void srctextview__EndComment(self,key)
struct srctextview *self;
char key; /* must be char for "&" to work. */
{
    struct srctext *ct=(struct srctext *)srctextview_GetDataObject(self);
    int count=im_Argument(srctextview_GetIM(self));
    long pos,oldpos;
    if (srctextview_ConfirmReadOnly(self))
	return;
    if (IsAutoCutMode() && srctextview_GetDotLength(self)>0)
	im_HandleMenu(srctextview_GetIM(self), proctable_Lookup("textview-zap-region"), self, 0); /* not a particularly efficient way to call textview_ZapRegionCmd, but what else ya gonna do? */
    oldpos= pos= srctextview_CollapseDot(self);
    while (count--) srctext_InsertCharacters(ct,pos++,&key,1);
    if (oldpos && srctext_GetChar(ct,oldpos-1)=='*')
	if (srctext_GetStyle(ct, oldpos+1)==ct->comment_style)
	    /* terminate existing style */
	    environment_SetStyle(environment_GetEnclosing( ct->header.text.rootEnvironment, oldpos+1),FALSE,FALSE);
	else {
	    /* wrap a new style */
	    long start=oldpos-1;
	    while (--start>0) {
		if (srctext_GetChar(ct,start)=='*')
		    if (srctext_GetChar(ct,start-1)=='/') {
			/* found start of comment, wrap style */
			if (srctext_GetStyle(ct,start) && srctext_GetStyle(ct,start)!=ct->comment_style)
			    /* must be inside a preprocessor directive or some other unknown style. abort */
			    break;
			start--;
			if (srctext_InString(ct,start))
			    break;
			srctext_WrapStyleNow(ct, start,oldpos-start+1, ct->comment_style, FALSE,FALSE);
			break;
		    }
		    else if (srctext_GetChar(ct,start+1)=='/')
			/* uh-oh, found another end of comment! */
			break;
	    }
	}
    srctextview_SetDotPosition(self,pos);
    srctextview_FrameDot(self,pos);
    srctext_NotifyObservers(ct,0); 
}

static void startLineComment(self,key)
struct srctextview *self;
char key; /* must be char for "&" to work. */
{
    srctextview_StartLineComment(self,key);
}

/* srctextview_StartLineComment is written to handle any bang-comment character, but not mapped in the proctable. To enable a bang comment, simply bind it in the subclass. */
void srctextview__StartLineComment(self,key)
struct srctextview *self;
char key; /* must be char for "&" to work. */
{
    struct srctext *ct=(struct srctext *)srctextview_GetDataObject(self);
    int count=im_Argument(srctextview_GetIM(self));
    long pos, oldpos;
    if (srctextview_ConfirmReadOnly(self))
	return;
    if (IsAutoCutMode() && srctextview_GetDotLength(self)>0)
	im_HandleMenu(srctextview_GetIM(self), proctable_Lookup("textview-zap-region"), self, 0); /* not a particularly efficient way to call textview_ZapRegionCmd, but what else ya gonna do? */
    pos= oldpos= srctextview_CollapseDot(self);
    while (count--) srctext_InsertCharacters(ct, pos++, &key, 1);
    if (!srctext_GetStyle(ct, oldpos) && !srctext_InString(ct,oldpos))
	srctext_WrapStyleNow(ct, oldpos,pos-oldpos, ct->linecomment_style, FALSE,TRUE);
    srctextview_SetDotPosition(self, pos);
    srctextview_FrameDot(self, pos);
    srctext_NotifyObservers(ct, 0);
}

static void styleLabel(self,key)
struct srctextview *self;
char key; /* must be char for "&" to work. */
{
    srctextview_StyleLabel(self,key);
}

/* StyleLabel JUST wraps the label style, if appropriate. It calls SelfInsert to do all the actual insertion/update work. */
void srctextview__StyleLabel(self, key)
struct srctextview *self;
char key;
{
    struct srctext *ct=(struct srctext *)srctextview_GetDataObject(self);
    long pos;
    if (srctextview_ConfirmReadOnly(self))
	return;
    if (IsAutoCutMode() && srctextview_GetDotLength(self)>0)
	im_HandleMenu(srctextview_GetIM(self), proctable_Lookup("textview-zap-region"), self, 0); /* not a particularly efficient way to call textview_ZapRegionCmd, but what else ya gonna do? */
    pos= srctextview_CollapseDot(self);
    if (pos && !srctext_InCommentStart(ct,pos) && !srctext_InString(ct,pos)) {
	struct mark *colonpos=srctext_CreateMark(ct,pos,0);
	srctextview_SelfInsertReindent(self,key); /* do this first, so the inserted char won't be part of the label style */
	srctext_BackwardCheckLabel(ct,mark_GetPos(colonpos));
	srctext_RemoveMark(ct,colonpos);
	mark_Destroy(colonpos);
    }
    else
	srctextview_SelfInsert(self,key);
}

static void styleString(self,key)
struct srctextview *self;
char key;
{
    srctextview_StyleString(self,key);
}

/* String starts or ends a string style, as appropriate. */
void srctextview__StyleString(self, key)
struct srctextview *self;
char key;
{
    struct srctext *ct=(struct srctext *)srctextview_GetDataObject(self);
    long pos;
    if (srctextview_ConfirmReadOnly(self))
	return;
    if (IsAutoCutMode() && srctextview_GetDotLength(self)>0)
	im_HandleMenu(srctextview_GetIM(self), proctable_Lookup("textview-zap-region"), self, 0); /* not a particularly efficient way to call textview_ZapRegionCmd, but what else ya gonna do? */
    pos= srctextview_CollapseDot(self);
    srctextview_SelfInsert(self,key);
    if (srctext_Quoted(ct,pos))
	return;
    if (srctext_InString(ct,pos)) {
	/* we're in a string now, let's see if it needs terminating */
	long start=srctext_BackwardSkipString(ct,pos-1,key)+1;
	struct environment *existingstyle= srctext_GetEnvironment(ct, pos);
	if (start<=0 || srctext_GetStyle(ct,start))
	    /* either we're NOT in a string, or the place we thought we started is part of some other style */
	    return;
	if (existingstyle == ct->header.text.rootEnvironment)
	    /* need to wrap whole string */
	    srctext_WrapStyleNow(ct, start,pos-start+1, ct->string_style, FALSE,FALSE);
	else if (environment_Eval(existingstyle) == start)
	    /* only need to terminate existing style */
	    environment_SetStyle(existingstyle, FALSE,FALSE);
    }
    else if (!srctext_GetStyle(ct,pos))
	/* we're not already IN a string; let's START one */
	srctext_WrapStyleNow(ct, pos,srctextview_GetDotPosition(self)-pos, ct->string_style, FALSE,TRUE);
}

/* nextLongLine finds the next line exceeding max-length and wraps the cursor around the offending characters */
static void nextLongLine(self,key)
struct srctextview *self;
long key;
{
    struct srctext *ct = (struct srctext *)srctextview_GetDataObject(self);
    long max=srctext_GetMaxLineLength(ct);
    if (max==0)
	message_DisplayString(self,0,"None found; No maximum line length set.");
    else {
	long pos=srctextview_GetDotPosition(self)+srctextview_GetDotLength(self);
	long samepos=pos, start=0;
	long len=srctext_GetLength(ct);
	int c, linelen=0;
	
	for (pos=srctext_GetBeginningOfLine(ct,pos+1); pos<len; pos++) {
	    c= srctext_GetChar(ct,pos);
	    if (c=='\n') {
		if (start) {
		    /* found the end of a line that is too long */
		    srctextview_SetDotPosition(self,start);
		    srctextview_SetDotLength(self,pos-start);
		    srctextview_FrameDot(self,pos);
		    return;
		}
		linelen= 0;
	    }
	    else if (c=='\t')
		linelen=(linelen+8)&~7;
	    else
		linelen++;
	    if (linelen>max && !start)
		/* this line is too long (and start wasn't set already) */
		start= pos;
	}
	/* display a reassuring message if end-of-file reached */
	message_DisplayString(self,0,"No more too-long lines found.");
    }
}

/* checkLineLengths calls srctext_CheckLineLengths to display a box if any lines exceed that length */
static void checkLineLengths(self,rock)
struct srctextview *self;
long rock;
{
    struct srctext *d = (struct srctext *)srctextview_GetDataObject(self);
    int numericalvalue= srctext_GetMaxLineLength(d); /* use max-length unless explicitly specified in exinit */
    if (rock) /* rock is actually a string, if from ezinit file */
	numericalvalue= atoi(rock);
    srctext_CheckLineLengths(d,numericalvalue,self);
}

static void toggleOverstrike(self,key) /*RSK91overstrike*/
struct srctextview *self;
long key;
{
    struct srctext *d = (struct srctext *)srctextview_GetDataObject(self);
    if (srctext_IsInOverstrikeMode(d)) {
	srctext_ChangeOverstrikeMode(d,FALSE);
	message_DisplayString(self,0,"Normal (insert) mode.");
    }
    else {
	srctext_ChangeOverstrikeMode(d,TRUE);
	message_DisplayString(self,0,"Overstrike mode.");
    }
    /*srctext_RegionModified(d,0,srctext_GetLength(d));*/ /*wimpy way to get all the views to refresh the cursor type*/ /* not needed anyway, unless drawtxtv got changed to display a different cursor in overstrike mode */
    srctext_NotifyObservers(d,observable_OBJECTCHANGED);
}

static void overstrikeOn(self,key) /*RSK91overstrike*/
struct srctextview *self;
long key;
{
    struct srctext *d = (struct srctext *)srctextview_GetDataObject(self);
    srctext_ChangeOverstrikeMode(d,TRUE);
    srctext_NotifyObservers(d,observable_OBJECTCHANGED);
}

static void overstrikeOff(self,key) /*RSK91overstrike*/
struct srctextview *self;
long key;
{
    struct srctext *d = (struct srctext *)srctextview_GetDataObject(self);
    srctext_ChangeOverstrikeMode(d,FALSE);
    srctext_NotifyObservers(d,observable_OBJECTCHANGED);
}

/* WaitCursorOn sets the process cursor to a Clock (wait) symbol */
void srctextview__WaitCursorOn(self)
struct srctextview *self;
{
    im_SetProcessCursor(waitCursor);
}

/* WaitCursorOff sets the process cursor back to normal */
void srctextview__WaitCursorOff(self)
struct srctextview *self;
{
    im_SetProcessCursor(NULL);
}

static void whatColumn(self)
struct srctextview *self;
{
    srctextview_WhatColumn(self);
}

/* WhatColumn displays the current column of the caret (or selected region) in the message window, taking tab characters into account */
void srctextview__WhatColumn(self)
struct srctextview *self;
{
    struct srctext *ct = (struct srctext *)srctextview_GetDataObject(self);
    long pos=srctextview_GetDotPosition(self), len=srctextview_GetDotLength(self);
    char s[128];
    /* note that 1 is added to all columns to translate to left-margin-is-column-one system */
    if (len>0)
	sprintf(s,"Selected region starts in column %d and ends in column %d.\0", srctext_CurrentColumn(ct,pos)+1, srctext_CurrentColumn(ct,pos+len)+1);
    else
	sprintf(s,"Column %d.\0", srctext_CurrentColumn(ct,pos)+1);
    message_DisplayString(self,0,s);
}

/* gotoColumn is called from a menu card. Its rock parameter, passed from the proctable entry, defaults to 0 but may contain a left-margin-is-column-one parameter from an ezinit binding. In either case, the number will be converted to left-margin-is-column-ZERO (negative means PROMPT for column) value, and passed on to the GotoColumn method. */
static void gotoColumn(self,rock)
struct srctextview *self;
long rock;
{
    int numericalvalue= 0;
    if (rock) /* rock is actually a string, if from ezinit file */
	numericalvalue= atoi(rock);
    srctextview_GotoColumn(self,numericalvalue-1);
}

/* GotoColumn method's rock parameter is a left-margin-is-column-zero value, which is the system used internally by all source view code. Users communicate with left-margin-is-column-ONE values, which get interpreted by gotoColumn. */
/* If rock parameter is negative, it means a proctable entry is calling this with no parameter and the user should be PROMPTED for a column number. Any other value of rock is considered a parameter and will forgo the prompting, using rock as the target column. */
void srctextview__GotoColumn(self,rock)
struct srctextview *self;
int rock;
{
    struct srctext *ct = (struct srctext *)srctextview_GetDataObject(self);
    long p;
    char s[10];
    int newcol;
    s[0]='\0';
    p= srctextview_CollapseDot(self);
    if (rock<0) {
	if (message_AskForString(self,0,"What column? ",NULL,s,sizeof(s))<0 || sscanf(s,"%d",&newcol)!=1) {
	    message_DisplayString(self, 0, "Cancelled.");
	    return;
	}
	newcol--; /* translate to left-margin-is-column-ZERO */
    }
    else
	newcol= rock; /* passed parameters are *already* in left-margin-is-column-zero format */
    if (srctext_CurrentColumn(ct,p)>newcol)
	/* move backward to desired column */
	while (p>0 && srctext_CurrentColumn(ct,p)>newcol) p--;
    else
	/* move forward to desired column */
	while (p<srctext_GetLength(ct) && srctext_GetChar(ct,p)!='\n' && srctext_CurrentColumn(ct,p+1)<=newcol) p++;
    if (srctext_CurrentColumn(ct,p)<newcol)
	/* past end of line (or possibly at a tab char); need to slap on some whitespace */
	if (srctext_GetReadOnly(ct)) {
	    char msg[128];
	    srctextview_SetDotPosition(self, p);
	    sprintf(msg, "Document is read only.  Moved to column %d.\0", srctext_CurrentColumn(ct,p)+1);
	    message_DisplayString(self, 0, msg);
	}
	else
	    srctextview_SetDotPosition(self,srctext_TabAndOptimizeWS(ct,p,newcol));
    else
	/* otherwise, just move the cursor over */
	srctextview_SetDotPosition(self,p);
}

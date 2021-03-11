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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/text/RCS/txtvcsty.c,v 1.14 1993/10/26 22:43:58 gk5g Exp $";
#endif

#include <andrewos.h>
#include <class.h>
#include <txtvcmds.h>
#include <text.ih>
#include <im.ih>
#include <message.ih>
#include <view.ih>
#include <mark.ih>
#include <envrment.ih>
#include <stylesht.ih>
#include <style.ih>
#include <environ.ih>
#include <complete.ih>
#include <txtvinfo.h>
#include <sys/param.h>
#include <sys/stat.h>
#include <ctype.h>

#define AUXMODULE 1
#include <textv.eh>

#define DELETE_NOSTYLES 0
#define DELETE_LEFT 1
#define DELETE_RIGHT 2
#define Text(v)	(struct text *) ((v)->header.view.dataobject)

void textview_InsertEnvironment();
static void DoDisplayInsertEnvironment();

extern long lcNewLine;
extern long lcInsertEnvironment;

static boolean useOldInsertionRules;
static long deletionDirection;
static long deletionPosition;
static long deletionCount;
static struct environment **flipBegEnvs;
static struct environment **flipEndEnvs;
static long flipBegCnt;
static long flipEndCnt;
static long flipBegMax = 0;
static long flipEndMax = 0;
struct mark *insertMark = NULL;


void textview__PrepareDeletion(self, pos, len)
struct textview *self;
long pos;
long len;
{
    struct environment *env;
    long envPos;
    long envLen;
    struct environment *newEnv;
    long newEnvPos;
    long newEnvLen;
    long newPos;
    long checkPos;
    struct text *d = Text(self);

    deletionCount = 0;
    deletionDirection = DELETE_NOSTYLES;

    if (useOldInsertionRules) {
	return;
    }

    env = textview_GetInsertEnvironment(self, pos);
    textview_ClearInsertStack(self);
    if (env != NULL) {
	if (len < 0) {
	    newPos = pos + len;
	    checkPos = newPos;
	    newEnv = environment_GetInnerMost(d->rootEnvironment, checkPos);
	    deletionDirection = DELETE_LEFT;
	}
	else {
	    newPos = pos;
	    checkPos = pos + len;
	    newEnv = environment_GetInnerMost(d->rootEnvironment, checkPos - 1);
	    deletionDirection = DELETE_RIGHT;
	}

	envPos = environment_Eval(env);
	envLen = environment_GetLength(env);
	newEnvPos = environment_Eval(newEnv);
	newEnvLen = environment_GetLength(newEnv);

	if (newEnv != env /* && newEnvPos + newEnvLen <= pos */) {
	    env = newEnv;
	    envPos = newEnvPos;
	    envLen = newEnvLen;
	}

	while ((len < 0 && (envPos == newPos && pos >= envPos + envLen))
	       || (len > 0 && (envPos + envLen == pos + len && pos <= envPos))) {
	    if (env->type == environment_Style) {
		struct style *style = env->data.style;
		char *styleName = style_GetName(style);

		if (styleName != NULL) {
		    struct InsertStack *p = (struct InsertStack *) malloc(sizeof(struct InsertStack));

		    p->name = (char *) malloc(strlen(styleName) + 1);
		    strcpy(p->name, styleName);
		    p->next = self->insertStack;
		    self->insertStack = p;
		}
	    }
	    env = (struct environment *) environment_GetParent(env);
	    envPos = environment_Eval(env);
	    envLen = environment_GetLength(env);
	}

	deletionPosition = newPos;

	if (len > 0 && checkPos == envPos + envLen) {
	    deletionDirection = DELETE_LEFT;
	}
	else if (len < 0 && checkPos == envPos) {
	    deletionDirection = DELETE_RIGHT;
	}

	for (deletionCount = 0; env != d->rootEnvironment; env = (struct environment *) environment_GetParent(env)) {
	    deletionCount++;
	}

    }
}

void textview__FinishDeletion(self)
struct textview *self;
{
    struct text *d = Text(self);
    struct environment *env;
    struct environment *newEnv;
    long pos;

    if (deletionDirection == DELETE_LEFT) {
	pos = deletionPosition - 1;
    }
    else {
	pos = deletionPosition;
    }

    env = d->rootEnvironment;

    while (deletionCount-- != 0) {
	newEnv = environment_GetChild(env, pos);
	if (newEnv != NULL) {
	    env = newEnv;
	}
    }
	
    self->insertEnvironment = env;
    mark_SetPos(self->insertEnvMark, deletionPosition);
    mark_SetModified(self->insertEnvMark, FALSE);
}

void textview__DeleteCharacters(self, pos, len)
struct textview *self;
long pos;
long len;
{
    if (len != 0) {
	textview_PrepareDeletion(self, pos, len);
	if (len < 0) {
	    text_DeleteCharacters(Text(self), pos + len, -len);
	}
	else {
	    text_DeleteCharacters(Text(self), pos, len);
	}
	textview_FinishDeletion(self);
	text_NotifyObservers(Text(self), observable_OBJECTCHANGED);
    }
}

static struct environment *AddNewEnvironment(self, ss, env, insert)
struct textview *self;
struct stylesheet *ss;
struct environment *env;
struct InsertStack *insert;
{
    struct style *style = NULL;
    struct environment *newEnv;

    if (insert-> next != NULL) {
	env = AddNewEnvironment(self, ss, env, insert->next);
    }

    if (ss != NULL) {
	style = stylesheet_Find(ss, insert->name);
	if (style == NULL) {
	    char message[100];

	    sprintf(message, "Could not find style %s", insert->name);
	    message_DisplayString(self, 0, message);
	}
    }

    if (style != NULL) {
	newEnv = environment_WrapStyle(env, mark_GetPos(insertMark), mark_GetLength(insertMark), style);
    }

    return newEnv;
}

void textview__FinishInsertion(self)
struct textview *self;
{
    long i;
    long pos;

    if (insertMark != NULL) {
	if (self->insertEnvironment != NULL) {
	    self->insertEnvironment = AddNewEnvironment(self, text_GetStyleSheet(Text(self)), self->insertEnvironment, self->insertStack);
	}
	textview_ClearInsertStack(self);

	text_RemoveMark(Text(self), insertMark);
	mark_Destroy(insertMark);
	insertMark = NULL;
    }

    if (flipBegCnt > 0 || flipEndCnt > 0) {
	for (i = flipBegCnt - 1; i >= 0; i--) {
	    flipBegEnvs[i]->header.nestedmark.includeBeginning = ! flipBegEnvs[i]->header.nestedmark.includeBeginning;
	}

	for (i = flipEndCnt - 1; i >= 0; i--) {
	    flipEndEnvs[i]->header.nestedmark.includeEnding = ! flipEndEnvs[i]->header.nestedmark.includeEnding;
	}

	pos = mark_GetEndPos(self->insertEnvMark);
	mark_SetPos(self->insertEnvMark, pos);
	mark_SetLength(self->insertEnvMark, 0);
	mark_SetModified(self->insertEnvMark, FALSE);
	mark_SetStyle(self->insertEnvMark, FALSE, FALSE);
    }
    
}

static void PushFlipBegEnv(te, value)
struct environment *te;
boolean value;
{
    if (flipBegCnt >= flipBegMax) {
	    flipBegMax = flipBegCnt * 2;
	    flipBegEnvs = (struct environment **) realloc(flipBegEnvs, sizeof(struct environment *) * flipBegMax);
    }
    flipBegEnvs[flipBegCnt++] = te;
    te->header.nestedmark.includeBeginning = value;
}

static void PushFlipEndEnv(te, value)
struct environment *te;
boolean value;
{
    if (flipEndCnt >= flipEndMax) {
	    flipEndMax = flipEndCnt * 2;
	    flipEndEnvs = (struct environment **) realloc(flipEndEnvs, sizeof(struct environment *) * flipEndMax);
    }
    flipEndEnvs[flipEndCnt++] = te;
    te->header.nestedmark.includeEnding = value;
}

static void SetBeginningDown(self, te, pos)
struct textview *self;
struct environment *te;
long pos;
{
    while ((te = environment_GetChild(te, pos)) != NULL && te->type == environment_Style) {
	if (te->header.nestedmark.includeBeginning && environment_Eval(te) == pos) {
	    PushFlipBegEnv(te, FALSE);
	}
    }
}

static void SetEndingDown(self, te, pos)
struct textview *self;
struct environment *te;
long pos;
{
    long endPos;

    if (pos <= 0) {
	return;
    }

    while ((te = environment_GetChild(te, pos - 1)) != NULL && te->type == environment_Style && (endPos = (environment_Eval(te) + environment_GetLength(te))) >= pos) {
	if (endPos == pos && te->header.nestedmark.includeEnding) {
	    PushFlipEndEnv(te, FALSE);
	}
    }
}

void textview__PrepareInsertion(self, insertingNewLine)
struct textview *self;
boolean insertingNewLine;
{
    long pos = textview_GetDotPosition(self) + textview_GetDotLength(self);
    long ePos;
    struct environment *te;
    struct text *d = Text(self);
    long lastCmd;

    flipBegCnt = 0;
    flipEndCnt = 0;

    if (useOldInsertionRules) {
	return;
    }

    if ((lastCmd = im_GetLastCmd(self->header.view.imPtr)) == lcInsertEnvironment && self->insertEnvironment != NULL && ! mark_GetModified(self->insertEnvMark)) {
	if (self->insertStack != NULL) {
	    insertMark = text_CreateMark(d, pos, 0);
	    mark_SetStyle(insertMark, FALSE, TRUE);
	}

	if ((ePos = environment_Eval(self->insertEnvironment)) == pos) {
	    SetBeginningDown(self, self->insertEnvironment, pos);
	    for (te = self->insertEnvironment;
		 te != d->rootEnvironment && environment_Eval(te) == pos;
		 te = (struct environment *) environment_GetParent(te)) {
		if (! te->header.nestedmark.includeBeginning) {
		    PushFlipBegEnv(te, TRUE);
		}
	    }
	    SetEndingDown(self, te, pos);
	}
	else if (ePos + environment_GetLength(self->insertEnvironment) == pos) {
	    SetEndingDown(self, self->insertEnvironment, pos);
	    for (te = self->insertEnvironment;
		 te != d->rootEnvironment && environment_Eval(te) + environment_GetLength(te) != pos;
		 te = (struct environment *) environment_GetParent(te)) {
		if (! te->header.nestedmark.includeEnding) {
		    PushFlipEndEnv(te, TRUE);
		}
	    }
	    SetBeginningDown(self, te, pos);
	}
	else {
	    te = environment_GetChild(self->insertEnvironment, pos);
	    if (te != NULL && (ePos = environment_Eval(te)) != pos) {
		environment_Split(te, pos - ePos);
	    }
	    SetEndingDown(self, self->insertEnvironment, pos);
	    SetBeginningDown(self, self->insertEnvironment, pos);
	}

	if (flipBegCnt > 0 || flipEndCnt > 0) {
	    mark_SetStyle(self->insertEnvMark, FALSE, TRUE);
	}
    }
    else if (lastCmd != lcNewLine) {
	if (! insertingNewLine && (pos == 0 || text_GetChar(d, pos - 1) == '\n')) {
	    /*
	     Need to turn off the endings of any environments that
	     stop at pos and turn on the beginnings of any environments
	     that estart at pos
	     */
	    struct environment *le;
	    struct environment *pe;

	    te = d->rootEnvironment;
	    le = te;
	    while ((pe = environment_GetChild(te, pos)) != NULL && pe->type == environment_Style) {
		if (environment_Eval(pe) == pos) {
		    if (! pe->header.nestedmark.includeBeginning) {
			PushFlipBegEnv(pe, TRUE);
		    }
		    le = te;
		}
		te = pe;
	    }
	    if (pos > 0) {
		SetEndingDown(self, le, pos);
	    }
	}
    }
}

struct environment *textview__GetEnclosingEnvironment(self, pos)
struct textview *self;
long pos;
{
    struct environment *te;
    long lastCmd = im_GetLastCmd(self->header.view.imPtr);
    struct text *d = Text(self);

    if (lastCmd != lcNewLine
	 && (pos == 0
	     || text_GetChar(d, pos - 1) == '\n')) {
	struct environment *pe;

	te = d->rootEnvironment;
	while ((pe = environment_GetChild(te, pos)) != NULL && pe->type == environment_Style) {
	    te = pe;
	}
    }
    else {
	te = textview_GetEnclosedStyleInformation(self, pos, NULL);
	while (te != NULL && te->type != environment_Style) {
	    te = (struct environment *) environment_GetParent(te);
	}
    }

    return te;
}

struct environment *textview__GetInsertEnvironment(self, pos)
struct textview *self;
long pos;
{
    struct environment *te;
    long lastCmd = im_GetLastCmd(self->header.view.imPtr);
    
    if (lastCmd != lcInsertEnvironment || mark_GetModified(self->insertEnvMark)) {
	te = textview_GetEnclosingEnvironment(self, pos);
	textview_ClearInsertStack(self);
	mark_SetPos(self->insertEnvMark, pos);
	mark_SetModified(self->insertEnvMark, FALSE);
    }
    else {
	te = self->insertEnvironment;
    }

    return te;
}

void textview__ClearInsertStack(self)
struct textview *self;
{
    while (self->insertStack != NULL) {
	struct InsertStack *p;

	p = self->insertStack;
	self->insertStack = p->next;

	free(p->name);
	free(p);
    }
}

void textview__PopInsertStack(self)
struct textview *self;
{
    if (self->insertStack != NULL) {
	struct InsertStack *p;

	p = self->insertStack;
	self->insertStack = p->next;

	free(p->name);
	free(p);
    }
}

void textview__AddStyleToInsertStack(self, styleName)
struct textview *self;
char *styleName;
{
    struct InsertStack *p;

    p = (struct InsertStack *) malloc(sizeof(struct InsertStack));
    if (p != NULL) {
	p->name = (char *) malloc(strlen(styleName) + 1);
	if (p->name == NULL) {
	    free(p);
	    return;
	}
	strcpy(p->name, styleName);
	p->next = self->insertStack;
	self->insertStack = p;
    }
}

void textview__PlainInsertEnvironment(self)
struct textview *self;
{
    struct text *d = Text(self);
    long pos;
    
    pos = textview_GetDotPosition(self);

    /* this call also resets insertStack if we have not previously been dealing with styles */

    self->insertEnvironment = textview_GetInsertEnvironment(self, pos);

    if (self->insertStack != NULL) {
	textview_ClearInsertStack(self);
    }

    self->insertEnvironment = d->rootEnvironment;

}


void textview__UpInsertEnvironment(self)
struct textview *self;
{
    struct text *d = Text(self);
    long pos;
    
    pos = textview_GetDotPosition(self);

    /* this call also resets insertStack if we have not previously been dealing with styles */

    self->insertEnvironment = textview_GetInsertEnvironment(self, pos);

    if (self->insertStack != NULL) {
	textview_PopInsertStack(self);
    }
    else {
	if (self->insertEnvironment != NULL && self->insertEnvironment !=d->rootEnvironment) {
	    self->insertEnvironment = (struct environment *) environment_GetParent(self->insertEnvironment);
	}
    }
}

void textview__DownInsertEnvironment(self)
struct textview *self;
{
    struct environment *pe;
    struct environment *te;
    struct environment *ce;
    long pos;

    pos = textview_GetDotPosition(self);

    self->insertEnvironment = textview_GetInsertEnvironment(self, pos);

    if (self->insertStack == NULL && self->insertEnvironment != NULL) {
	pe = textview_GetEnclosedStyleInformation(self, pos, NULL);
	ce = environment_GetCommonParent(self->insertEnvironment, pe);
	if (ce == self->insertEnvironment && pe != self->insertEnvironment) {
	    do {
		te = pe;
		pe = (struct environment *) environment_GetParent(te);
	    } while (pe != self->insertEnvironment);

	    self->insertEnvironment = te;
	}
	else {
	    if (pos > 0
		&& (te = environment_GetChild(self->insertEnvironment, pos - 1)) != NULL
		&& environment_Eval(te) + environment_GetLength(te) == pos) {
		self->insertEnvironment = te;
	    }
	    else if ((te = environment_GetChild(self->insertEnvironment, pos)) != NULL) {
		self->insertEnvironment = te;
	    }
	}
    }
}

void textview__LeftInsertEnvironment(self)
struct textview *self;
{
    struct environment *te;
    struct environment *pe;
    long ePos;
    struct text *d = Text(self);
    long pos;
    
    pos = textview_GetDotPosition(self);

    te = textview_GetInsertEnvironment(self, pos);

    if (te != NULL && self->insertStack == NULL) {
	if ((ePos = environment_Eval(te)) == pos) {
	    te = (struct environment *) environment_GetParent(te);
	}
	else if (ePos + environment_GetLength(te) == pos) {
	    if (pos != 0 && (pe = environment_GetChild(te, pos - 1)) != NULL && pe->type == environment_Style) {
		te = pe;
	    }
	}
	else {
	    if (pos != 0) {
		pe = environment_GetChild(te, pos - 1);
		if (pe != NULL && pe != d->rootEnvironment && environment_Eval(pe) + environment_GetLength(pe) == pos) {
		    te = pe;
		}
	    }
	}
    }

    self->insertEnvironment = te;
}

void textview__RightInsertEnvironment(self)
struct textview *self;
{
    struct environment *te;
    struct environment *pe;
    long ePos;
    long pos;
    
    pos = textview_GetDotPosition(self);

    te = textview_GetInsertEnvironment(self, pos);

    if (te != NULL && self->insertStack == NULL) {
	if ((ePos = environment_Eval(te)) == pos) {
	    pe = environment_GetChild(te, pos);
	    if (pe != NULL) {
		te = pe;
	    }
	}
	else if (ePos + environment_GetLength(te) == pos) {
	    te = (struct environment *) environment_GetParent(te);
	}
	else {
	    if (pos != 0) {
		pe = environment_GetChild(te, pos);
		if (pe != NULL && pe->type == environment_Style) {
		    te = pe;
		}
	    }
	}
    }

    self->insertEnvironment = te;
}

/* This is no longer a command you can type;
 * it is now called from a menu item */

void textview__LookCmd(self, look)
register struct textview *self;
int look;
{
    register struct text *d;
    struct stylesheet *ss;
    struct style *styleptr;
    char *name;	

    if (ConfirmReadOnly(self))
        return;

    d = Text(self);
    ss = text_GetStyleSheet(d);

    if (ss != NULL) {
	styleptr = ss->styles[look];
	if ((name = style_GetName(styleptr)) != NULL) {
	    textview_InsertEnvironment(self, name);
	    return;
	}
    }

    message_DisplayString(self, 0, "Sorry; can't add style.");
}

void textview_PlainerCmd(self, type)
register struct textview *self;
char *type;
{
    register struct text *d;
    register struct environment *env;
    int pos, len;

    if (ConfirmReadOnly(self))
        return;

    d = Text(self);
    pos = textview_GetDotPosition(self);
    len = textview_GetDotLength(self);
    if (im_GetLastCmd(textview_GetIM(self)) == lcDisplayEnvironment && (env = self->displayEnvironment) != NULL && env != d->rootEnvironment)  {
	pos = environment_Eval(env);
	len = env->header.nestedmark.length;
	environment_Delete(env);
	text_SetModified(d);
	im_SetLastCmd(textview_GetIM(self), lcDisplayEnvironment);
	self->displayEnvironment = NULL;
	message_DisplayString(self, 0, "");
    }
    else if (len == 0) {
	if (strcmp(type, "old") == 0) {
	    env = environment_GetInnerMost(d->rootEnvironment, pos);
	    if (env == NULL  || env == d->rootEnvironment) return;
	    pos = environment_Eval(env);
	    len = env->header.nestedmark.length;
	    environment_Delete(env);
	    text_SetModified(d);
	}
	else {
	    textview_UpInsertEnvironment(self);
	    im_SetLastCmd(textview_GetIM(self), lcInsertEnvironment);
	    DoDisplayInsertEnvironment(self);
	    return;
	}
    }
    else {
	if (environment_Remove(d->rootEnvironment, pos, len, environment_Style, FALSE))
	    text_SetModified(d);
    }
    text_RegionModified(d, pos, len);
    text_NotifyObservers(d, observable_OBJECTCHANGED);
}

void textview_PlainestCmd(self)
register struct textview *self;
{
    register struct text *d;
    int pos, len;

    if (ConfirmReadOnly(self))
        return;

    d = Text(self);
    pos = textview_GetDotPosition(self);
    len = textview_GetDotLength(self);
    if (len == 0) {
	textview_PlainInsertEnvironment(self);
	im_SetLastCmd(textview_GetIM(self), lcInsertEnvironment);
	return;
    }
    else if (environment_Remove(d->rootEnvironment,
       pos, len, environment_Style, TRUE))
	text_SetModified(d);
    text_RegionModified(d, pos, len);
    text_NotifyObservers(d, observable_OBJECTCHANGED);
}

/* Inserts a lookz view in front of the current paragraph. */

void textview_ExposeStyleEditor(self)
struct textview *self;
{
    register int pos;
    register struct text *d;

    if(textview_objecttest(self, "lookzview", "view") == FALSE)
        return;
    if(textview_objecttest(self, "lookz", "dataobject") == FALSE)
        return;
    if(text_GetObjectInsertionFlag(Text(self)) == FALSE){
	message_DisplayString(self, 0, "Object Insertion Not Allowed!");
	return;
    }

    d = Text(self);
    pos = text_GetBeginningOfLine(d, textview_GetDotPosition(self) + textview_GetDotLength(self));

    /* If we can insert it between two carriage returns (or at the beginning of the
         * document) do so, otherwise insert it at the start of this paragraph. */
    if (pos > 0 && text_GetChar(d, pos - 1) != '\n')
	pos += 1;

    textview_PrepareInsertion(self, FALSE);
    self->currentViewreference =
          text_InsertObject(Text(self), pos, "lookz", "lookzview");
    textview_FinishInsertion(self);
    textview_FrameDot(self, pos);
    text_NotifyObservers(d, observable_OBJECTCHANGED);
    if (im_GetLastCmd(textview_GetIM(self)) == lcInsertEnvironment) {
	im_SetLastCmd(textview_GetIM(self), lcInsertEnvironment);
    }
}

#define SHOWSIZE 250

void textview_ShowStylesCmd(self)
register struct textview *self;
{
    char tbuf[SHOWSIZE];
    register char *tp;
    struct environment *te;
    struct environment *de = NULL;
    struct text *d;
    int curLen, flag;
    long pos;

    curLen = 1;		/* the null at the end */
    if (im_GetLastCmd(textview_GetIM(self)) == lcDisplayEnvironment) {
	de = self->displayEnvironment;
	pos = self->displayEnvironmentPosition;
    }
    else {
	textview_ReleaseStyleInformation(self, self->displayEnvironment);
	self->displayEnvironment = NULL;
	pos = textview_CollapseDot(self);
    }

    strcpy(tbuf, "Styles: ");
    flag = 0;
    d = Text(self);
    te = textview_GetEnclosedStyleInformation(self, pos, NULL);
    if (te) {
	if (te == d->rootEnvironment) {
	    strcat(tbuf, "None");
	}
	else {
	    while (te != d->rootEnvironment) {
		tp = te->data.style->name;
		
		if(tp == NULL) tp = "(Unnamed Style)";
		if (flag)
		    strcat(tbuf, " <- ");

		if (de == NULL)
		    de = te;
		else if (de == te)
		    de = NULL;

		curLen += 4+strlen(tp);
		if (curLen >= SHOWSIZE) break;
		if (de == te)
		    strcat(tbuf, "**");
		strcat(tbuf,tp);
		if (de == te)  {
		    strcat(tbuf, "**");
		    textview_SetDotPosition(self, environment_Eval(te));
		    textview_SetDotLength(self, environment_GetLength(te));
		}
		te = (struct environment *) te->header.nestedmark.parent;
		flag = 1;
	    }
	}
	if (de == NULL)  {
	    textview_SetDotPosition(self, pos);
	    textview_SetDotLength(self, 0);
	}
	message_DisplayString(self, 0, tbuf);
	im_SetLastCmd(textview_GetIM(self), lcDisplayEnvironment);
	self->displayEnvironment = de;
	self->displayEnvironmentPosition = pos;
    }
    else message_DisplayString(self, 0, "Error");
    textview_WantUpdate(self, self);
}


void textview_ChangeTemplate(self)
struct textview *self;
{
    char tname[150];
    register struct text *d;
    int gf;

    if (ConfirmReadOnly(self))
        return;

    d = Text(self);
    gf = message_AskForString(self, 0,
      "Add styles from template [default]: ", "", tname, 100);
    if (gf < 0)
        return;
    if (tname[0] == '\0')  {   /* no name specified -- use default */
	strcpy(tname, "default");
    }
    if (text_ReadTemplate(d, tname, text_GetLength(d) == 0) < 0)
        message_DisplayString(self, 100, "Could not read template file.");
    else {
	text_RegionModified(d, 0, text_GetLength(d));
	text_NotifyObservers(d, observable_OBJECTCHANGED);
        message_DisplayString(self, 0, "Done.");
    }
}

#define BADCURPOS -1

void textview_ToggleExposeStyles(self)
struct textview *self;
{
    self->exposeStyles = ! self->exposeStyles;
    self->force = TRUE;
    self->csxPos = BADCURPOS;		/* Indication that cursor is not visible */
    self->cexPos = BADCURPOS;
    textview_WantUpdate(self, self);
}

void textview_ToggleColorStyles(self)
struct textview *self;
{
    self->showColorStyles = ! self->showColorStyles;
    self->force = TRUE;
    textview_WantUpdate(self, self);
}

static void DoDisplayInsertEnvironment(self)
struct textview *self;
{
    struct text *d = Text(self);
    char *p;
    char outstr[1000];

    if (self->insertEnvironment == d->rootEnvironment && self->insertStack == NULL) {
	p = "Current Insertion Style: default";
    }
    else {
	long count = 0;
	long len;
	struct InsertStack *st;
	struct environment *env;
	char *name;

	p = &outstr[999];
	*p = '\0';


	for (st = self->insertStack; st != NULL; st = st->next) {
	    name = st->name;
	    if (name == NULL) {
		name = "unknown";
	    }
	    p -= 6;
	    strncpy(p, " (new)", 6);
	    len = strlen(name);
	    p -= len;
	    strncpy(p, name, len);
	    p -= 2;
	    strncpy(p, ", ", 2);
	    count++;
	}
	for (env = self->insertEnvironment; env != d->rootEnvironment; env = (struct environment *) environment_GetParent(env)) {

	    if (env->type != environment_Style) {
		name = "inset";
	    }
	    else if ((name = env->data.style->name) == NULL) {
		name = "unknown";
	    }

	    len = strlen(name);
	    p -= len;
	    strncpy(p, name, len);
	    p -= 2;
	    strncpy(p, ", ", 2);
	    count++;
	}
	*p = ':';
	if (count > 1) {
	    *--p = 's';
	}
	p -= 23;
	strncpy(p, "Current Insertion Style", 23);
    }
    message_DisplayString(self, 0, p);

#if 0
    if (self->insertStack != NULL) {
	char outstr[1000];
	char *name;

	name = self->insertStack->name;
	if (name == NULL) {
	    name = "unknown";
	}

	sprintf(outstr, "Current Insertion Style: %s (new)", name);
	message_DisplayString(self, 0, outstr);
    }
    else if (self->insertEnvironment == NULL) {
	message_DisplayString(self, 0, "Current Insertion Style: none");
    }
    else if (self->insertEnvironment == d->rootEnvironment) {
	message_DisplayString(self, 0, "Current Insertion Style: default");
    }
    else {
	char outstr[1000];
	char *name;

	if (self->insertEnvironment->type != environment_Style) {
	    name = "inset - need to fix this";
	}
	else if ((name = self->insertEnvironment->data.style->name) == NULL) {
	    name = "unknown";
	}
	    
	sprintf(outstr, "Current Insertion Style: %s", name);
	message_DisplayString(self, 0, outstr);
    }
#endif
}

void textview_DisplayInsertEnvironment(self)
struct textview *self;
{
    long pos;

    if (ConfirmReadOnly(self))
        return;

    pos = textview_CollapseDot(self);

    self->insertEnvironment = textview_GetInsertEnvironment(self, pos);

    DoDisplayInsertEnvironment(self);
	
    im_SetLastCmd(textview_GetIM(self), lcInsertEnvironment);
}


void textview_PlainInsertEnvCmd(self)
struct textview *self;
{
    if (ConfirmReadOnly(self))
        return;

    textview_CollapseDot(self);

    textview_PlainInsertEnvironment(self);

    im_SetLastCmd(textview_GetIM(self), lcInsertEnvironment);

    DoDisplayInsertEnvironment(self);
}

void textview_UpInsertEnvironmentCmd(self)
struct textview *self;
{
    if (ConfirmReadOnly(self))
        return;

    textview_CollapseDot(self);

    textview_UpInsertEnvironment(self);

    im_SetLastCmd(textview_GetIM(self), lcInsertEnvironment);

    DoDisplayInsertEnvironment(self);
}

void textview_DownInsertEnvironmentCmd(self)
struct textview *self;
{
    if (ConfirmReadOnly(self))
        return;

    textview_CollapseDot(self);

    textview_DownInsertEnvironment(self);

    im_SetLastCmd(textview_GetIM(self), lcInsertEnvironment);

    DoDisplayInsertEnvironment(self);
}

void textview_LeftInsertEnvironmentCmd(self)
struct textview *self;
{
    if (ConfirmReadOnly(self))
        return;

    textview_CollapseDot(self);

    textview_LeftInsertEnvironment(self);

    im_SetLastCmd(textview_GetIM(self), lcInsertEnvironment);

    DoDisplayInsertEnvironment(self);
}

void textview_RightInsertEnvCmd(self)
struct textview *self;
{
    if (ConfirmReadOnly(self))
        return;

    textview_CollapseDot(self);

    textview_RightInsertEnvironment(self);
    
    im_SetLastCmd(textview_GetIM(self), lcInsertEnvironment);

    DoDisplayInsertEnvironment(self);
}

static boolean StyleCompletionWork(style, data)
struct style *style;
struct result *data;
{
    completion_CompletionWork(style_GetName(style), data);
    return FALSE;
}

static enum message_CompletionCode StyleComplete(partial, styleSheet, resultStr, resultSize)
char *partial;
struct stylesheet *styleSheet;
char *resultStr;
int resultSize;
{
    struct result result;
    char textBuffer[100];

    *textBuffer = '\0';
    result.partial = partial;
    result.partialLen = strlen(partial);
    result.bestLen = 0;
    result.code = message_Invalid;
    result.best = textBuffer;
    result.max = sizeof(textBuffer) - 1; /* Leave extra room for a NUL. */

    stylesheet_EnumerateStyles(styleSheet, (procedure) StyleCompletionWork, (long) &result);

    strncpy(resultStr, result.best, resultSize);
    if (result.bestLen == resultSize) /* Now make sure buffer ends in a NUL. */
        resultStr[result.bestLen] = '\0';

    return result.code;
}

struct helpData {
    char *partial;
    int (*textFunction)();
    long textRock;
};

static boolean StyleHelpWork(style, helpData)
struct style *style;
struct helpData *helpData;
{
    char infoBuffer[1024];
    char strippedMenuName[1000];
    char *p;
    char *q;
    char *r;

    if (completion_FindCommon(helpData->partial,
       style_GetName(style)) == strlen(helpData->partial)) {
        char *menuName;
	
	strippedMenuName[0] = '\0';
	r = strippedMenuName;
	if ((menuName = style_GetMenuName(style)) != NULL) {
	    q = menuName;
	    while ((p = index(q, '~')) != NULL) {
		while (q != p) {
		    *r++ = *q++;
		}
		while (*++p != ',' && *p != '\0') {
		}
		q = p;
	    }
	    *r = '\0';
	}

        sprintf(infoBuffer, "%-16s %s", style_GetName(style), strippedMenuName);

        (*helpData->textFunction)(helpData->textRock,
            message_HelpListItem, infoBuffer, NULL);
    }
    return FALSE; /* Keep on enumerating. */
}

static void StyleHelp(partial, styleSheet, helpTextFunction, helpTextRock)
char *partial;
struct stylesheet *styleSheet;
int (*helpTextFunction)();
long helpTextRock;
{
    struct helpData helpData;

    helpData.partial = partial;
    helpData.textFunction = helpTextFunction;
    helpData.textRock = helpTextRock;

    stylesheet_EnumerateStyles(styleSheet, (procedure) StyleHelpWork, (long) &helpData);
}

void textview_InsertEnvironment(self, sName)
struct textview *self;
char *sName;
{
    struct text *d = Text(self);
    struct stylesheet *ss = text_GetStyleSheet(d);
    char styleName[100];
    struct style *style;
    long pos = textview_GetDotPosition(self);
    long len = textview_GetDotLength(self);
    long lastCmd;

    if (ConfirmReadOnly(self))
        return;

    lastCmd = im_GetLastCmd(textview_GetIM(self));
    self->insertEnvironment = textview_GetInsertEnvironment(self, pos + len);

    if (sName == NULL || sName[0] == '\0') {
	if (message_AskForStringCompleted(self, 0, "Insert style: ",
					  NULL, styleName,
					  sizeof(styleName), NULL, (procedure) StyleComplete,
					  (procedure) StyleHelp, (long) ss,
					  message_MustMatch |
					  message_NoInitialString) == -1) {
	    return;
	}
    }
    else {
	strcpy(styleName, sName);
    }

    style = stylesheet_Find(ss, styleName);

    if (style != NULL) {

	if (lastCmd != lcInsertEnvironment && len != 0) {
	    /* We want to wrap the environment here and now */
	    if (text_AddStyle(d, pos, len, style) == NULL) {
		message_DisplayString(self, 0,
				      "Sorry; can't add style.");
	    }
	    else {
		char message[200];

		sprintf(message, "Added the style %s", styleName);
		message_DisplayString(self, 0, message);
		text_NotifyObservers(d, observable_OBJECTCHANGED);
	    }
	}
	else {
	    textview_AddStyleToInsertStack(self, styleName);
	    im_SetLastCmd(textview_GetIM(self), lcInsertEnvironment);

	    DoDisplayInsertEnvironment(self);
	}
    }
    else {
	char message[200];

	sprintf(message, "Could not find the style %s", styleName);
	message_DisplayString(self, 0, message);
    }
}
void InitializeMod()
{
    flipEndMax = 4;
    flipEndEnvs = (struct environment **) malloc(sizeof(struct environment *) * flipEndMax);

    flipBegMax = 4;
    flipBegEnvs = (struct environment **) malloc(sizeof(struct environment *) * flipBegMax);

    useOldInsertionRules = environ_GetProfileSwitch("UseOldInsertionRules", FALSE);

}



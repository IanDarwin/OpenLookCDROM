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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/typescript/RCS/fcomp.c,v 1.26 1993/10/06 22:26:32 gk5g Exp $";
#endif

/* fcomp.c -- typescript package to perform filename completion
  * David Anderson
  * August 1988
  */

#include <andrewos.h>
#include <ctype.h>
#include <class.h>
#include <text.ih>
#include <mark.ih>
#include <proctbl.ih>
#include <smpltext.ih>
#include <dataobj.ih>
#include <tscript.ih>
#include <filetype.ih>
#include <keystate.ih>
#include <keymap.ih>
#include <im.ih>
#include <environ.ih>
#include <envrment.ih>
#include <message.ih>
#include <cursor.ih>
#include <style.ih>
#include <stylesht.ih>
#include <fontdesc.ih>
#include <complete.ih>
#include <search.ih>

#include <fcomp.eh>

#include <sys/types.h>
#include <sys/param.h>
#include <sys/stat.h>

static struct cursor *waitCursor;
static long searchCmd;
static long tokenSearchCmd;

static int (*typescript_GrabLastCmd) ();

#define Text(self) ((struct text *) ((self)->header.view.dataobject))
#define MyEnvinfo(text,pos) environment_GetInnerMost(text->rootEnvironment, pos)
#define min(x, y) (((x) < (y)) ? (x) : (y))

#define DEFAULTCHARWIDTH 12

CompleteFname(ts, key)
register struct typescript *ts;
long key;
{
    register long pos;
    register long fname;
    register struct text *theText = Text(ts);
    char pathname[MAXPATHLEN];
    char buffer[MAXPATHLEN];
    char canonical[MAXPATHLEN];
    register char *insert;
    char *msg = NULL;
    long c;
    long tlen = text_GetLength(theText);
    long cStart = mark_GetPos(ts->cmdStart);
    long endpos;

    if (ts->pipescript) return;

    pos = typescript_GetDotPosition(ts);
    if (pos < cStart) {
	return;
    }
    for (endpos = pos; endpos < tlen && ! isspace(text_GetChar(theText, endpos)); endpos++) {
    }
    for (fname = pos - 1; fname >= cStart && (c = text_GetChar(theText, fname)) != '>'
	   && c != '<' && ! isspace(c); fname--) {
    }
    fname++;
    text_CopySubString(theText, fname, endpos-fname, pathname, FALSE);
    pathname[endpos-fname] = 0;
    switch (completion_FileComplete(pathname, FALSE, buffer, MAXPATHLEN)) {
	case message_Complete:
	    msg = "[Unique]";
	    break;
	case message_CompleteValid:
	    msg = "[Others]";
	    break;
	case message_Valid:
	    msg = "[Incomplete]";
	    break;
	case message_Invalid:
	    message_DisplayString(ts, 0, "[No Match]");
	    return;	/* Don't attempt to insert a name. */
   }
    message_DisplayString(ts, 0, msg);
    filetype_CanonicalizeFilename(canonical, pathname, MAXPATHLEN);
    insert = buffer + strlen(canonical);
    text_InsertCharacters(theText, endpos, insert, strlen(insert));
    typescript_SetDotPosition(ts, endpos+strlen(insert));
    typescript_SetDotLength(ts, 0);
    text_NotifyObservers(theText, 0);
}


struct helpstat {
    boolean refusedot;
    long itemcnt;
    long maxitemlen;
    long errorcnt;
};

#define acceptitem(rock, item, itemtype) \
  (itemtype == message_HelpListItem) && \
  strcmp(item, "./") && strcmp(item, "../") && \
  (item[0] != '.' || !rock->refusedot)

static GatherStats(rock, itemtype, item, dummy)
struct helpstat *rock;
enum message_HelpItem itemtype;
char *item;
long dummy; /* along for the ride */
{
    if (acceptitem(rock, item, itemtype)) {
        long itemlen = strlen(item);

        rock->itemcnt++;
        if (itemlen > rock->maxitemlen) {
            rock->maxitemlen = itemlen;
        }
    }
    else if (itemtype != message_HelpListItem) rock->errorcnt++;
}


struct repCookie {
    boolean refusedot;
    char **report;
    long columns;
    long colwidth;
    long rows;
    long rowlen;
    long repcolwidth;
    long reprowlen;
    long widthinchar;
    long maxwidthinpix;
    long count;
    struct graphic *ts;
    struct fontdesc *font;
};

#define COLSPACE    1

static MakeReport(cookie, itemtype, item, dummy)
struct repCookie *cookie;
enum message_HelpItem itemtype;
char *item;
long dummy; /* along for the ride */
{
    long widthinpix;

    if (acceptitem(cookie, item, itemtype)) {
	cookie->report[cookie->count] = (char*)malloc(strlen(item) + 1);
        strcpy(cookie->report[cookie->count++], item);
	widthinpix = fontdesc_StringSize(cookie->font, (struct graphic*) cookie->ts, item, NULL, NULL);
	if(widthinpix > cookie->maxwidthinpix) {
	    cookie->maxwidthinpix = widthinpix;
	}
    }
}

static char *
SaveLastCommand(td)
struct typescript *td;
{
    long len, spos;
    struct text *mydoc;
    char *textBuf;
    char *cmdStr, *p;
    long retLen;

    mydoc =  Text(td);
    spos = mark_GetPos(td->cmdStart);
    len = text_GetLength(mydoc);

    if (spos == len)
	return NULL;

    cmdStr = (char *) malloc(len - spos + 1);
    *cmdStr = '\0';

    p = cmdStr;
    while(spos < len) {
	textBuf = text_GetBuf(mydoc, spos, len - spos, &retLen);
	if(textBuf != NULL) {
	    strncpy(p,textBuf,retLen);
	    p[retLen] = '\0';
	    p += retLen;
	    spos += retLen;
	}
	else
	    break;
    }
    return cmdStr;
}

static int
mystrcmp(s1,s2)
register char **s1,**s2;
{
  if(s1 && *s1 && s2 && *s2)
      return(strcmp(*s1,*s2));
  else return(0);
}

PossibleCompletions(ts, key)
register struct typescript *ts;
long key;
{
    register long pos;
    register struct text *theText = Text(ts);
    long c;
    long tlen = text_GetLength(theText);
    long cStart = mark_GetPos(ts->cmdStart);
    long endpos;
    register long fname;
    char pathname[MAXPATHLEN];
    long pathlen;
    char *cmdStr;
    struct helpstat rock;
    long cmdOffset;


    if (ts->pipescript) return;

    pos = typescript_GetDotPosition(ts);
    if (pos < cStart) {
	return;
    }
    for (endpos = pos; endpos < tlen && ! isspace(text_GetChar(theText, endpos)); endpos++) {
    }
    cmdOffset = endpos - cStart;
    for (fname = pos - 1; fname >= cStart && (c = text_GetChar(theText, fname)) != '>'
      && c != '<' && ! isspace(c); fname--) {
    }
    fname++;
    cmdStr = SaveLastCommand(ts);
    text_CopySubString(theText, fname, endpos-fname, pathname, FALSE);
    pathname[endpos-fname] = 0;

    text_InsertCharacters(theText, tlen++, "\n", 1);

    rock.itemcnt = rock.maxitemlen = rock.errorcnt = 0;
    pathlen = strlen(pathname);
    rock.refusedot = FALSE;
    if(pathlen == 0) rock.refusedot = TRUE;
    else {
	if(pathname[pathlen-1] == '/') rock.refusedot = TRUE;
    }
    completion_FileHelp((*pathname) ? pathname : "./", 0, GatherStats, (long) &rock);

    if(rock.errorcnt || rock.itemcnt == 0) {
	message_DisplayString(ts, 0, "[No Match]");
    }
    else {
	static struct style *tsStyle = NULL;
	static struct fontdesc *tsfont;
	static long charwidth = DEFAULTCHARWIDTH;
	enum style_FontSize dummy;
	char ffamily[MAXPATHLEN];
	long tssize;
	char spaces[MAXPATHLEN];
	long windowwidth, windowheight;
	struct repCookie cookie;
	register long row, col;
	char **files, *thisfile;
	int len, count;


	if(!tsStyle && (tsStyle = typescript_GetDefaultStyle(ts))) {
	    style_GetFontSize(tsStyle, &dummy, &tssize);
	    style_GetFontFamily(tsStyle, ffamily, MAXPATHLEN);
	    tsfont = fontdesc_Create(ffamily, fontdesc_Fixed, tssize);
	    charwidth = fontdesc_StringSize(tsfont, typescript_GetDrawable(ts), " ", NULL, NULL);
	}

	typescript_GetTextSize(ts, &windowwidth, &windowheight);
	cookie.ts = typescript_GetDrawable(ts);
	cookie.font = tsfont;
	cookie.refusedot = rock.refusedot;
	cookie.count = 0;
	cookie.maxwidthinpix = 0;
	cookie.report = (char**) calloc(rock.itemcnt, sizeof(char*));

	completion_FileHelp((*pathname) ? pathname : "./", 0, MakeReport, (long)&cookie);

	cookie.colwidth = rock.maxitemlen + COLSPACE;
	cookie.columns = windowwidth / (cookie.maxwidthinpix + charwidth);
	if (cookie.columns == 0) cookie.columns = 1;
	cookie.rows = rock.itemcnt / cookie.columns;
	if(rock.itemcnt % cookie.columns) cookie.rows++;
	cookie.repcolwidth = rock.maxitemlen + 1;
	cookie.reprowlen = cookie.repcolwidth * cookie.columns;

	qsort(cookie.report, rock.itemcnt, sizeof(char*), mystrcmp);

	for(count = 0; count < cookie.colwidth; count++) 
	    spaces[count] = ' ';

	files = cookie.report;
	for(count = 0,row = 0; row < cookie.rows; row++) {
	    for(col = 0; col < cookie.columns; count++, col++)
		if (count < cookie.count) {
		    thisfile = *files++;
		    len = strlen(thisfile);
		    text_InsertCharacters(theText, tlen, thisfile, len);
		    tlen += len;
		    text_InsertCharacters(theText, tlen, spaces, cookie.colwidth - len);
		    tlen += cookie.colwidth - len;
		    free(thisfile);
		}
	    text_InsertCharacters(theText, tlen++, "\n", 1);
	}
	free(cookie.report);
    }

    ts->readOnlyLen = -1;
    ts->lastCmdPos = text_GetLength(ts->cmdText);
    mark_SetPos(ts->cmdStart, tlen);
    mark_SetLength(ts->cmdStart, 0);
    text_SetFence(theText, tlen);
    typescript_FrameDot(ts, tlen);

    write(ts->SubChannel, "\n", 1);
    if (cmdStr != NULL) {
	text_InsertCharacters(theText, tlen, cmdStr, strlen(cmdStr));
	free(cmdStr);
    }
    typescript_SetDotPosition(ts, tlen + cmdOffset);
    typescript_SetDotLength(ts, 0);
    typescript_FrameDot(ts, tlen + cmdOffset);

    text_NotifyObservers(theText, 0);
}

static struct SearchPattern *pattern = 0;
static char lastcmd[MAXPATHLEN];
static long lastmatch;
static long beginToken;
static long endToken;

CompleteTokenWork(ts, forward)
register struct typescript *ts;
boolean forward;
{
    register long pos;
    register struct text *theText = Text(ts);
    register long begincmd;
    long lastEventCmd;
    char cmd[MAXPATHLEN];
    register long match;
    char *patcode;
    long tlen;
    long clen;
    long c;
    long tokenLen;


    if (ts->pipescript) return;

    tlen = text_GetLength(theText);
    clen = text_GetLength(ts->cmdText);

    lastEventCmd = im_GetLastCmd(typescript_GetIM(ts));

    pos = typescript_GetDotPosition(ts);
    begincmd = mark_GetPos(ts->cmdStart);

    if (pos < begincmd) {
	pos = tlen;
    }

    if (lastEventCmd == tokenSearchCmd) {
	match = lastmatch;
	strcpy(cmd, lastcmd);
    }
    else {
	char unquotedCmd[MAXPATHLEN];
	long len = mark_GetLength(ts->cmdStart);

	for (beginToken = pos - 1; beginToken >= begincmd && (c = text_GetChar(theText, beginToken)) != '<' && c != '>' && ! isspace(c); beginToken--) {
	}
	beginToken++;

	for (endToken = pos; endToken < clen && (c = text_GetChar(theText, beginToken)) != '<' && c != '>' && ! isspace(c); endToken++) {
	}


	text_CopySubString(theText, beginToken, endToken - beginToken, unquotedCmd, FALSE);
	search_GetQuotedSearchString(unquotedCmd, cmd, sizeof(cmd));
	match = text_GetLength(ts->cmdText);
    }

    if (strlen(cmd) != 0) {
        if (patcode = search_CompilePattern(cmd, &pattern)) {
	    message_DisplayString(ts, 0, patcode);
	    return;
        }
	else {
	    while (TRUE) {
		if (forward) {
		    match = search_MatchPattern(ts->cmdText, match+1, pattern);
		}
		else {
		    match = search_MatchPatternReverse(ts->cmdText, match-1, pattern);
		}
		if (match <= 0 || (c = text_GetChar(ts->cmdText, match - 1)) == '\n' || c == '<' || c == '>' || isspace(c)) {
		    long end;

		    for (end = match; end < clen && (c = text_GetChar(ts->cmdText, end)) != '<' && c != '>' && c != '\n' && ! isspace(c); end++) {
		    }
		    
		    tokenLen = end - match;
		    break;
		}
	    }
	}
    }
    else {
	if (forward) {
	    long end;

	    while (match < clen && (c = text_GetChar(ts->cmdText, match)) != '<' && c != '>' && c != '\n' && ! isspace(c)) {
		match++;
	    }
	    while (match < clen && ((c = text_GetChar(ts->cmdText, match)) == '<' || c == '>' || c == '\n' || isspace(c))) {
		match++;
	    }
	    for (end = match + 1; end < clen && (c = text_GetChar(ts->cmdText, end)) != '<' && c != '>' && c != '\n' && ! isspace(c); end++) {
	    }
	    tokenLen = end - match;
	    
	    if (match >= tlen) {
		match = -1;
	    }
	}
	else {
	    long end;
	    for (end = match - 1; end >= 0 && ((c = text_GetChar(ts->cmdText, end)) == '<' || c == '>' || c == '\n' || isspace(c)); end--) {
	    }
	    if (end >= 0) {
		for (match = end - 1;  match >= 0 && ((c = text_GetChar(ts->cmdText, match)) != '<' && c != '>' && c != '\n' && ! isspace(c)); match--) {
		}
		tokenLen = end - match;
		match++;
	    }
	    
	    else {
		match = -1;
	    }
	}
    }

    if (match >= 0 && match != clen) {
	lastmatch = match;
	strcpy(lastcmd, cmd);
	text_DeleteCharacters(theText, beginToken, endToken - beginToken);
	pos = beginToken;
	text_CopySubString(ts->cmdText, match, tokenLen, cmd, FALSE);
	text_InsertCharacters(theText, beginToken, cmd, tokenLen);
	endToken = beginToken + tokenLen;
	typescript_SetDotPosition(ts, endToken);
	typescript_SetDotLength(ts, 0);
	typescript_FrameDot(ts, endToken);
	text_NotifyObservers(theText, 0);
    }
    else message_DisplayString(ts, 0, "[No matching command]");

    im_SetLastCmd(typescript_GetIM(ts), tokenSearchCmd);
}

CompleteTokenForward(ts, key)
register struct typescript *ts;
long key;
{
    CompleteTokenWork(ts, TRUE);
}

CompleteTokenBackward(ts, key)
register struct typescript *ts;
long key;
{
    CompleteTokenWork(ts, FALSE);
}


CompleteCmdWork(ts, forward)
register struct typescript *ts;
boolean forward;
{
    register long pos;
    register struct text *theText;
    register long begincmd;
    long lastEventCmd;
    char cmd[MAXPATHLEN];
    register long match;
    char *patcode;

    if (ts->pipescript) return;

    lastEventCmd = im_GetLastCmd(typescript_GetIM(ts));
    theText = Text(ts);

    pos = typescript_GetDotPosition(ts);
    begincmd = mark_GetPos(ts->cmdStart);

    if (lastEventCmd == searchCmd) {
	match = lastmatch;
	strcpy(cmd, lastcmd);
    }
    else {
	char unquotedCmd[MAXPATHLEN];
	long len = mark_GetLength(ts->cmdStart);

	if (len >= sizeof(unquotedCmd) - 1) {
	    len = sizeof(unquotedCmd) -1;
	}
	    
	text_CopySubString(theText, begincmd, len, unquotedCmd, FALSE);
	search_GetQuotedSearchString(unquotedCmd, cmd, sizeof(cmd));
	match = text_GetLength(ts->cmdText);
    }

    if (strlen(cmd) != 0) {
        if (patcode = search_CompilePattern(cmd, &pattern)) {
	    message_DisplayString(ts, 0, patcode);
	    return;
        }
	else {
	    while (TRUE) {
		if (forward) {
		    match = search_MatchPattern(ts->cmdText, match+1, pattern);
		}
		else {
		    match = search_MatchPatternReverse(ts->cmdText, match-1, pattern);
		}
		if (match <= 0 || text_GetChar(ts->cmdText, match - 1) == '\n') {
		    break;
		}
	    }
	}
    }
    else {
	if (forward) {
	    if (match < text_GetLength(ts->cmdText)) {
		match = text_GetEndOfLine(ts->cmdText, match) + 1;
	    }
	    else {
		match = -1;
	    }
	}
	else {
	    if (match > 0) {
		match = text_GetBeginningOfLine(ts->cmdText, match - 1);
	    }
	    else {
		match = -1;
	    }
	}
    }

    if (match >= 0 && match != text_GetLength(ts->cmdText)) {
	register long endmatch;

	lastmatch = match;
	strcpy(lastcmd, cmd);
	text_DeleteCharacters(theText, begincmd, mark_GetLength(ts->cmdStart));
	pos = begincmd;
	endmatch = text_GetEndOfLine(ts->cmdText, match);
	text_CopySubString(ts->cmdText, match, endmatch-match, cmd, FALSE);
	text_InsertCharacters(theText, pos, cmd, endmatch-match);
	pos += endmatch-match;
	typescript_SetDotPosition(ts, pos);
	typescript_SetDotLength(ts, 0);
	typescript_FrameDot(ts, pos);
	text_NotifyObservers(theText, 0);
    }
    else message_DisplayString(ts, 0, "[No matching command]");

    im_SetLastCmd(typescript_GetIM(ts), searchCmd);
}

CompleteCmdForward(ts, key)
register struct typescript *ts;
long key;
{
    CompleteCmdWork(ts, TRUE);
}

CompleteCmdBackward(ts, key)
register struct typescript *ts;
long key;
{
    CompleteCmdWork(ts, FALSE);
}

boolean fcomp__InitializeClass(classID)
struct classheader *classID;
{
    struct classinfo *imc;

    waitCursor = cursor_Create(NULL);
    cursor_SetStandard(waitCursor, Cursor_Wait);

    searchCmd = im_AllocLastCmd();
    tokenSearchCmd = im_AllocLastCmd();

    lastcmd[0] = 0;
    class_Load("search");

    class_Load("completion");

    imc = class_Load("typescript");

    proctable_DefineProc("fcomp-complete-filename", CompleteFname, imc, NULL, "Filename completion (typescript)");
    proctable_DefineProc("fcomp-possible-completions", PossibleCompletions, imc, NULL, "Display possible filename completions (typescript)");

    proctable_DefineProc("fcomp-complete-command-backward", CompleteCmdBackward, imc, NULL, "Complete partial command - searching backwards (typescript)");
    proctable_DefineProc("fcomp-complete-command-forward", CompleteCmdForward, imc, NULL, "Complete partial command - searching forwards (typescript)");
    proctable_DefineProc("fcomp-complete-token-backward", CompleteTokenBackward, imc, NULL, "Complete partial token - searching backwards (typescript)");
    proctable_DefineProc("fcomp-complete-token-forward", CompleteTokenForward, imc, NULL, "Complete partial token - searching forwards (typescript)");

    typescript_GrabLastCmd = proctable_GetFunction(proctable_Lookup("typescript-Grab-Last-Cmd"));

    return TRUE;
}

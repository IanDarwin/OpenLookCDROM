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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atkams/messages/lib/RCS/captions.c,v 2.48 1993/04/23 20:09:32 Zarf Exp $";
#endif


 

#include <andrewos.h> /* sys/file.h */
#include <cui.h>
#include <fdphack.h>
#include <sys/param.h>
#include <sys/stat.h>
#include <netinet/in.h>  /* for htonl, etc. */
#include <errprntf.h>
#include <ctype.h>
#include <rect.h>
#include <class.h>
#include <hdrparse.h>

#include <text822v.ih>
#include <message.ih>
#include <envrment.ih>
#include <scroll.ih>
#include <style.ih>
#include <fontdesc.ih>
#include <environ.ih>
#include <search.ih>
#include <text.ih>
#include <textv.ih>
#include <ams.ih>
#include <amsutil.ih>
#include <sendmsg.ih>
#include <folders.ih>
#include <keystate.ih>
#include <menulist.ih>
#include <cursor.ih>
#include <captions.eh>

extern void captions_SimulateLeftClick();
extern void captions_SimulateRightClick();
extern void captions_CaptionsCompound();
extern void captions_CaptionsTextviewCommand();
extern void captions_CaptionsFoldersCommand();
extern void captions_CaptionsBodiesCommand();
extern void captions_DownFocus(), captions_UpFocus();
extern void CapBeginText(), CapScrollBack();
extern void PreviousCaptionLine();
extern void CapGlitchDown();
extern void OneTimeInitKeyMenus();

extern int (*captextv_PreviousLineCmd)(),
    (*captextv_ReverseSearchCmd)(),
    (*captextv_ScrollScreenFwdCmd)(),
    (*captextv_ScrollScreenBackCmd)(),
    (*captextv_BeginningOfTextCmd)(),
    (*captextv_BeginningOfLineCmd)(),
    (*captextv_EndOfLineCmd)(),
    (*captextv_GlitchDownCmd)();

struct keymap *captions_privkeymap;
struct menulist *captions_privmenulist;

void captions__ShowHelp(self)
struct captions *self;
{
    int doprefix = 0;
    char *motdfile;
    FILE *fp;

    text_ClearCompletely(self->CaptText);
    motdfile = (char *) environ_GetProfile("messages.motdfile");
    if (!motdfile) {
	doprefix = 1;
	motdfile = "/etc/motd";
    }
    fp = fopen(motdfile, "r");
    if (!fp) {
	char ErrMsg[100];

	sprintf(ErrMsg, "Cannot open file %s (%d).", motdfile, errno);
	text_AlwaysInsertCharacters(self->CaptText, 0, ErrMsg, strlen(ErrMsg));
    } else {
	int inslen;
	struct environment *et;

	if (doprefix) {
	    unsigned long clock;
	    char *timestr, InsStr[500];
	    struct stat SBlock;

	    if (fstat(fileno(fp), &SBlock) == 0) {
		clock = (unsigned long) SBlock.st_mtime;
	    } else {
		clock = osi_GetSecs();
	    }
	    timestr = ctime(&clock);
	    inslen = strlen(timestr);
	    timestr[inslen-1] = '\0';
	    sprintf(InsStr, "Message of the day as of %s:\n\n", timestr);
	    inslen = strlen(InsStr);
	    text_AlwaysInsertCharacters(self->CaptText, 0, InsStr, inslen);
	    et = environment_InsertStyle(self->CaptText->rootEnvironment, 0, self->ActiveCaptionStyle, 1);
	    environment_SetLength(et, inslen - 1);
	} else {
	    inslen = 0;
	}
	text_SetReadOnly(self->CaptText, FALSE); /* BOGUS -- AlwaysInsertFile doesn't work for read-only text, 12/28/88 */
	text_AlwaysInsertFile(self->CaptText, fp, motdfile, inslen);
	text_SetReadOnly(self->CaptText, TRUE); /* BOGUS -- DITTO */
	fclose(fp);
    }
    captions_WantUpdate(self, self);
}    

void captions__SetBodies(captions, bv)
struct captions *captions;
struct t822view *bv;
{
    captions->BodView = bv;
    if (bv) t822view_SetCaptions(bv, captions);
}

struct view *
captions__Hit(captions, action, x, y, nclicks)
struct captions *captions;
int x, y, nclicks;  
enum view_MouseAction action;
{
    int thisdot, linelen, whichthis, whichdown;
    struct environment *env;

    if (action == view_LeftDown || action == view_RightDown) {
	super_Hit(captions, view_LeftDown, x, y, 1);
	captions->downdot = captions_GetDotPosition(captions);
    } else if (action == view_LeftUp) {
	super_Hit(captions, view_LeftUp, x, y, 1);
	captions_SimulateClick(captions, TRUE);
    } else if (action == view_RightUp)  {
	super_Hit(captions, view_LeftUp, x, y, 1);
	if (amsutil_GetOptBit(EXP_MARKING)) {
	    thisdot = captions_GetDotPosition(captions);
	    if (thisdot == captions->downdot) thisdot += captions_GetDotLength(captions);
	    if (thisdot != captions->downdot) {
		captions_FindCUIDByDocLocation(captions, &thisdot, &linelen, &env, &whichthis);
		captions_FindCUIDByDocLocation(captions, &captions->downdot, &linelen, &env, &whichdown);
		if (whichthis != whichdown) {
		    int i, low, high;

		    if (whichthis < whichdown) {
			low = whichthis;
			high = whichdown;
		    } else {
			low = whichdown;
			high = whichthis;
		    }
		    for (i=low; i<= high; ++i) {
			if (!captions->capcache[i].IsMarked) {
			    captions_ToggleMark(captions, &captions->capcache[i], captions->capcache[i].offset);
			}
		    }
		    captions_PostMenus(captions, NULL);
		    captions_ReportMarkedMessageCount(captions);
		    return((struct view *)captions);
		}
	    }
	}
	captions_SimulateClick(captions, FALSE);
    } else if (action == view_LeftMovement || action == view_RightMovement) {
	super_Hit(captions, view_LeftMovement, x, y, 1);
    }
    return((struct view *) captions);
}

void captions__SimulateClick(captions, IsLeftClick)
struct captions *captions;
boolean IsLeftClick;
{
    int linestart, linelen, thisCUID, whichcaption;
    struct environment *env;

    captions_WantInputFocus(captions, captions);
    if (captions->captioncachecount <= 0) {
	/* This code is kind of silly; it says if there are no captions on display, but there is some kind of text in the captions area, we will search through the bodies area for the contents of the current line and will scroll the body to start with that same line.  This is currently used only by the Set Options code, but it could in theory be used to make Messages able to view all sorts of things besides mail and bulletin boards... */
	struct textview *tv = (struct textview *)captions;
	struct text *t = (struct text *) captions_GetDataObject(captions);
	int dot, len, tmpdot, retlen;
	char *str, *tp, BodyBuf[1000];
	struct SearchPattern *Pattern = NULL;
	struct text *bod;

	ams_WaitCursor(TRUE);
	captextv_BeginningOfLineCmd(tv);
	dot = captions_GetDotPosition(captions);
	captextv_EndOfLineCmd(tv);
	tmpdot = captions_GetDotPosition(captions);
	len = tmpdot - dot;
	if (len > 0) {
	    str = text_GetBuf(t, dot, len, &retlen);
	    if (retlen >= sizeof(BodyBuf)) retlen = sizeof(BodyBuf) - 1;
	    strncpy(BodyBuf, str, retlen);
	    BodyBuf[retlen] = '\0';
	    tp = search_CompilePattern(BodyBuf, &Pattern);
	    if (tp) {
		message_DisplayString(NULL, 10, tp);
	    } else {
		bod = captions_GetBodDoc(captions);
		if ((tmpdot = search_MatchPattern(bod, dot, Pattern)) < 0) {
		    tmpdot = search_MatchPattern(bod, 0, Pattern);
		}
		if (tmpdot < 0) {
		    message_DisplayString(NULL, 10, "Option not found.");
		} else {
		    t822view_SetTopPosition(captions_GetBodView(captions), tmpdot);

		}
	    }
	}
	ams_WaitCursor(FALSE);
	return;
    }
    linestart = captions_GetDotPosition(captions);
    thisCUID =  captions_FindCUIDByDocLocation(captions, &linestart, &linelen, &env, &whichcaption);
    captions_SetDotPosition(captions, linestart);
    captions_SetDotLength(captions, 0);

    if (IsLeftClick || (!amsutil_GetOptBit(EXP_MARKING))) { 
	captions_DisplayNewBody(captions, thisCUID, linestart, linelen, env);
    } else {
	captions_ToggleMark(captions, &captions->capcache[whichcaption], linestart);
	captions_PostMenus(captions, NULL);
	captions_ReportMarkedMessageCount(captions);
    }
}





void GetInfo(ci, total, seen, dot)
struct captions *ci;
struct range *total, *seen, *dot;
{
    int pos, mylen, whichcaption, gap, mystart;
    struct environment *envptr;

    ci->textscrollinterface->GetInfo(ci, total, seen, dot);
    if (ci->captioncachecount <= 0) return;
    gap = ci->FolderSize - ci->FetchedFromEnd - ci->FetchedFromStart;
    total->beg = 0;
    total->end = captions_EncodePosition(ci, ci->FolderSize);
    pos = captions_DecodePosition(ci, seen->beg);
    captions_FindCUIDByDocLocation(ci, &pos, &mylen, &envptr, &whichcaption);
    if (pos < 0) {
	total->beg = total->end = 0;
	seen->beg = seen->end = 0;
	dot->beg = dot->end = 0;
	return;
    }
    mystart = (whichcaption >= ci->FetchedFromStart) ? (whichcaption + gap) : whichcaption;
    pos = captions_DecodePosition(ci, seen->end) - 1;
    captions_FindCUIDByDocLocation(ci, &pos, &mylen, &envptr, &whichcaption);
    seen->beg = captions_EncodePosition(ci, mystart);
    seen->end = ((whichcaption >= ci->FetchedFromStart) ? (whichcaption + gap) : whichcaption) + 1;
    seen->end = captions_EncodePosition(ci, seen->end);
    pos = ci->HighlightStartPos;
    captions_FindCUIDByDocLocation(ci, &pos, &mylen, &envptr, &whichcaption);
    dot->beg = (whichcaption >= ci->FetchedFromStart) ? (whichcaption + gap) : whichcaption;
    dot->beg = captions_EncodePosition(ci, dot->beg);
    dot->end = dot->beg;
}

static void SetFrame(ci, p, n, d)
struct captions *ci;
long p, n, d;
{
    int min, max, w, myp, outp;

    if (ci->captioncachecount <= 0) {
	ci->textscrollinterface->SetFrame(ci, p, n, d);
	return;
    }
    myp = captions_DecodePosition(ci, p);
    w = (n * captions_GetLogicalHeight(ci)) / d;
    /* The following conservatively assume a 6 pt font, 4 pts spacing */
    min = myp - (w / 10);
    max = myp + ((captions_GetLogicalHeight(ci) - w) / 10);
    if (min < 0) min = 0;
    if (max > ci->FolderSize) max = ci->FolderSize;
    if (captions_GuaranteeFetchedRange(ci, min, max)) return; /* error reported */

    if (myp >= ci->FolderSize) myp = ci->FolderSize - 1;
    if (myp >= ci->FetchedFromStart) {
	myp -= ci->FolderSize - ci->FetchedFromEnd - ci->FetchedFromStart;
    }
    if (myp >= 0) {
        outp = captions_EncodePosition(ci, ci->capcache[myp].offset);
    } else {
        outp = captions_EncodePosition(ci, 0);
    }
    ci->textscrollinterface->SetFrame(ci, outp, n, d);
}

static long WhatIsAt(ci, n, d)
struct captions *ci;
long n,d;
{
    int pos, len, whichcaption;
    struct environment *envptr;

    pos = ci->textscrollinterface->WhatIsAt(ci, n, d);
    if (ci->captioncachecount > 0) {
        pos = captions_DecodePosition(ci, pos);
	captions_FindCUIDByDocLocation(ci, &pos, &len, &envptr, &whichcaption);
        pos = (whichcaption >= ci->FetchedFromStart) ? (whichcaption + ci->FolderSize - ci->FetchedFromEnd - ci->FetchedFromStart) : whichcaption;
        pos = captions_EncodePosition(ci, pos);
    }
    return(pos);
}

static struct scrollfns scrollInterface = {GetInfo, SetFrame, NULL, WhatIsAt};

char * captions__GetInterface(self, interfaceName) 
struct captions *self;
char *interfaceName;
{
    if (strcmp(interfaceName, "scroll,vertical") == 0)
        return (char *) &scrollInterface;
    return NULL;
}


ResetCaptionNotBody(ci)
struct captions *ci;
{
    RemoveHighlighting(ci);
    ci->VisibleCUID = -1;
    captions_WantUpdate(ci, ci);
    captions_PostMenus(ci, NULL);
}

RemoveHighlighting(h) 
struct captions *h;
{
    if (h->HighlightEnv) {
	text_SetEnvironmentStyle(h->CaptText, h->HighlightEnv, AMS_GET_ATTRIBUTE(h->VisibleSnapshot, AMS_ATT_DELETED) ? h->DeletedStyle : h->NormalCaptionStyle);
	h->HighlightStartPos = 0;
	h->HighlightLen = 0;
	h->HighlightEnv = NULL;
    }
    return(0);
}

int captions__DeleteVisibleMessage(ci, Delete)
struct captions *ci;
Boolean Delete;
{
    int cuid;

    cuid = ci->VisibleCUID;    
    if (cuid < 1 || !ci->HighlightEnv) {
	message_DisplayString(NULL, 10, "There is no message on display.");
	return(0);
    }
    ams_WaitCursor(TRUE);
    if (Delete) {
	if (ams_CUI_DeleteMessage(ams_GetAMS(), cuid) == 0) { /* Everything worked Fine */
	    text_SetEnvironmentStyle(ci->CaptText, ci->HighlightEnv, ci->ActiveDeletedStyle);
	    AMS_SET_ATTRIBUTE(ci->VisibleSnapshot, AMS_ATT_DELETED);
	    captions_WantUpdate(ci, ci);
	    captions_AlterDeletedIcon(ci, ci->HighlightStartPos, TRUE);
	    message_DisplayString(NULL, 10, "Message deleted.");
	} /* The cui routine reported the errors itself */
    } else {
	if (ams_CUI_UndeleteMessage(ams_GetAMS(), cuid) == 0) { /* Everything worked Fine */
	    text_SetEnvironmentStyle(ci->CaptText, ci->HighlightEnv, ci->ActiveCaptionStyle);
	    AMS_UNSET_ATTRIBUTE(ci->VisibleSnapshot, AMS_ATT_DELETED);
	    captions_WantUpdate(ci, ci);
	    captions_AlterDeletedIcon(ci, ci->HighlightStartPos, FALSE);
	    message_DisplayString(NULL, 10, "Message undeleted.");
	} /* The cui routine reported the errors itself */
    }
    captions_PostMenus(ci, NULL);
    ams_WaitCursor(FALSE);
    return(0);
}

void captions__AlterDeletedIcon(ci, pos, Delete)
struct captions *ci;
int pos;
Boolean Delete;
{
    char c;
    char *newicon, ErrorText[256];

    c = text_GetChar(ci->CaptText, ++pos);
    switch(c) {
	case ICON_MAIL:
	case ICON_DELMAIL:
	    newicon = Delete ? SICON_DELMAIL : SICON_MAIL;
	    break;
	case ICON_READMAIL:
	case ICON_READDELMAIL:
	    newicon = Delete ? SICON_READDELMAIL : SICON_READMAIL;
	    break;
	case ICON_POST:
	case ICON_DELPOST:
	    newicon = Delete ? SICON_DELPOST : SICON_POST;
	    break;
	case ICON_READDELPOST:
	case ICON_READPOST:
	    newicon = Delete ? SICON_READDELPOST : SICON_READPOST;
	    break;
	default:
	    newicon = "X";
	    sprintf(ErrorText, "Found '%c' on '%sdelete' where a mail/post icon was expected", c, Delete ? "" : "un");
	    ams_ReportError(ams_GetAMS(), ErrorText, ERR_WARNING, FALSE, 0);
	    break;
    }
    text_AlwaysInsertCharacters(ci->CaptText, pos++, newicon, 1);
    text_AlwaysDeleteCharacters(ci->CaptText, pos, 1);
}

captions__FindCUIDByDocLocation(ci, position, len, envptr, whichcaption)
struct captions *ci;
int *position, *len;
struct environment **envptr;
int *whichcaption;
{
    int top, bottom, split;

    bottom = 0;
    top = ci->captioncachecount -1;
    if (top < 0) {
	*position = *len = *whichcaption = -1;
	*envptr = NULL;
	return(-1);
    }
    split = top/2;
    while (TRUE) {
	if (ci->capcache[split].offset > *position) {
	    top = split;
	} else {
	    bottom = split;
	}
	split = bottom + (top-bottom)/2;
	if (top == bottom) break;
	if (top == bottom + 1) {
	    if (*position < ci->capcache[top].offset) {
		split = bottom;
	    } else {
		split = top;
	    }
	    break;
	}
    }
    *position = ci->capcache[split].offset;
    if (split == ci->captioncachecount -1) {
	*len = text_GetLength(ci->CaptText) - *position;
    } else {
	*len = ci->capcache[split+1].offset - *position;
    }
    *envptr = ci->capcache[split].env;
    *whichcaption = split;
    return(ci->capcache[split].cuid);
}

void captions__ToggleMark(ci, hc,linestart)
struct captions *ci;
struct CaptionCache *hc;
int linestart;
{
    text_AlwaysInsertCharacters(ci->CaptText, linestart+1, hc->IsMarked ? " " : SICON_MARK, 1);
    text_AlwaysDeleteCharacters(ci->CaptText, linestart, 1);
    ci->MarkCount += hc->IsMarked ? -1 : 1;
    hc->IsMarked = hc->IsMarked ? 0 : 1;
    captions_WantUpdate(ci, ci);
}


AddCaptionToCacheEntry(ccache, ct, size, cuid, offset, env, iconenv, MayModify, snapshot, IsDup)
struct CaptionCache **ccache;
int *ct, *size;
int cuid, offset;
struct environment *env, *iconenv;
char *snapshot;
Boolean MayModify, IsDup;
{
    long dumint;
    struct CaptionCache *cc = &((*ccache)[*ct]);

    cc->offset = offset;
    cc->cuid = cuid;
    cc->env = env;
    cc->iconenv = iconenv;
    cc->IsMarked = 0;
    cc->MayModify = MayModify ? 1 : 0;
    cc->IsDup = IsDup ? 1 : 0;
    bcopy(AMS_CHAIN(snapshot), &dumint, sizeof(long));
    cc->Chain = ntohl(dumint);
    strcpy(cc->Date, AMS_DATE(snapshot));
    bcopy(AMS_ATTRIBUTES(snapshot), cc->Attributes, AMS_ATTRIBUTESIZE);
    if (++*ct >= *size) {
	int newsize;

	newsize = *size * 2;
	*ccache = (struct CaptionCache *) realloc(*ccache, newsize * sizeof(struct CaptionCache));
	*size = newsize;
    }
}

MergeTwoCacheEntries(ci, ccache, cct, csize, prefixend)
struct captions *ci;
struct CaptionCache *ccache;
int cct, csize, prefixend;
{
    int totalct, i;

    totalct = ci->captioncachecount + cct;
    if (totalct >= ci->captioncachesize) {
	ci->capcache = (struct CaptionCache *) realloc(ci->capcache, (1+totalct) * sizeof(struct CaptionCache));
	ci->captioncachesize = totalct+1;
    }
    bcopyfromback(ci->capcache, &ci->capcache[cct], ci->captioncachecount * sizeof(struct CaptionCache));
    bcopy(ccache, ci->capcache, cct * sizeof(struct CaptionCache));
    for (i = cct; i<totalct; ++i) {
	ci->capcache[i].offset += prefixend;
    }
    ci->captioncachecount = totalct;
}

int GetSouthernmostPoint(ci)
struct captions *ci;
{
    if (ci->SouthPoint) {
	return(environment_Eval(ci->SouthPoint));
    } else {
	return(0);
    }
}

SetSouthernmostPoint(ci, pos)
struct captions *ci;
int pos;
{
    int i;

    if (pos >= 0 && pos < GetSouthernmostPoint(ci)) return;
    if (ci->SouthPoint) {
	text_SetEnvironmentStyle(ci->CaptText, ci->SouthPoint, ci->IconicStyle);
	ci->SouthPoint = NULL;
    }
    if (pos < 0) return;
    for (i=0; i<ci->captioncachecount; ++i) {
	if (ci->capcache[i].offset > pos) break;
    }
    if (--i < 0) return;
    ci->SouthPoint = ci->capcache[i].iconenv;
    text_SetEnvironmentStyle(ci->CaptText, ci->SouthPoint, ci->UnderlinedIconicStyle);

    captions_WantUpdate(ci, ci);
}


MarkVisibleMessageSeen(ci)
struct captions *ci;
{
    if (AMS_GET_ATTRIBUTE(ci->VisibleSnapshot, AMS_ATT_MAYMODIFY) && AMS_GET_ATTRIBUTE(ci->VisibleSnapshot, AMS_ATT_UNSEEN) && ams_CUI_MarkAsRead(ams_GetAMS(), ci->VisibleCUID)) {
	return(-1); /* error was reported */
    }
    AMS_UNSET_ATTRIBUTE(ci->VisibleSnapshot, AMS_ATT_UNSEEN);
    captions_MarkVisibleMessageStateofSeeing(ci, TRUE);
}


void captions__MarkVisibleMessageStateofSeeing(ci, HasSeen)
struct captions *ci;
Boolean HasSeen;
{
    int c, pos;
    struct text *d;
    char *newicon, ErrorText[256];

    d = ci->CaptText;
    pos = ci->HighlightStartPos + 1;
    c = text_GetChar(d,  pos);
    switch(c) {
	case ICON_MAIL:
	case ICON_READMAIL:
	    newicon = HasSeen ? SICON_READMAIL : SICON_MAIL;
	    break;
	case ICON_READDELMAIL:
	case ICON_DELMAIL:
	    newicon = HasSeen ? SICON_READDELMAIL : SICON_DELMAIL;
	    break;
	case ICON_POST:
	case ICON_READPOST:
	    newicon = HasSeen ? SICON_READPOST : SICON_POST;
	    break;
	case ICON_READDELPOST:
	case ICON_DELPOST:
	    newicon = HasSeen ? SICON_READDELPOST : SICON_DELPOST;
	    break;
	default:
	    newicon = "X";
	    sprintf(ErrorText, "Found '%c' on 'mark %sseen' where a mail/post icon was expected", c, HasSeen ? "" : "un");
	    ams_ReportError(ams_GetAMS(), ErrorText, ERR_WARNING, FALSE, 0);
	    break;
    }
    text_AlwaysInsertCharacters(d, pos++, newicon, 1);
    text_AlwaysDeleteCharacters(d, pos, 1);
    captions_WantUpdate(ci, ci);
    captions_PostMenus(ci, NULL);
    if (!HasSeen) message_DisplayString(NULL, 10, "Marked message as unread");
}


int captions__ShowMore(ci, MayScroll, MayGoOn, InsistOnMark)
struct captions *ci;
Boolean MayScroll, MayGoOn, InsistOnMark;
{
    int whichcaption, pos, len, thisCUID;
    struct environment *env;

    if (captions_GetDotLength(ci) > 0) {
	pos = captions_GetDotPosition(ci);
	thisCUID = captions_FindCUIDByDocLocation(ci, &pos, &len, &env, &whichcaption);
	if (pos > 0) {
	    captions_SetDotPosition(ci, pos - 1);
	}
	captions_SetDotLength(ci, 0);
    }
    if (MayScroll && (len=text_GetLength(captions_GetBodDoc(ci))) > 0 && !t822view_Visible(captions_GetBodView(ci), len)  && (len != t822view_GetTopPosition(captions_GetBodView(ci)))) {
	/* Scroll it forward */
	NextTextviewScreen(captions_GetBodView(ci));
	return(0);
    } else {
	/* Go on to the next one */
	if (ci->HighlightLen > 0) {
	    pos = ci->HighlightStartPos;
	} else {
	    pos = captions_GetDotPosition(ci);
	}
	if (pos <= 0 && ci->HighlightLen <= 0) {
	    whichcaption = -1;
	} else {
	    thisCUID = captions_FindCUIDByDocLocation(ci, &pos, &len, &env, &whichcaption);
	}
	++whichcaption;
	while (whichcaption < ci->captioncachecount && ci->capcache[whichcaption].IsDup) {
	    ++whichcaption;
	}
	if (InsistOnMark) {
	    while (whichcaption < ci->captioncachecount && !ci->capcache[whichcaption].IsMarked) {
		++whichcaption;
	    }
	}
	if (whichcaption >= ci->captioncachecount) {
	    if (MayGoOn) {
		/* Have to set southernmost point in case skipped duplicates */
		if (ci->captioncachecount > 0) {
		    SetSouthernmostPoint(ci, ci->capcache[ci->captioncachecount-1].offset);
		}
		folders_NextFolder(captions_GetFolders(ci), TRUE);
	    } else {
		if (ci->captioncachecount > 0) {
		    message_DisplayString(NULL, 10, "You are at the end of the captions");
		}
		/* Else we assume another error is displayed already... */
	    }
	} else {
		pos = ci->capcache[whichcaption].offset;
		captions_SetDotPosition(ci, pos);
		thisCUID = captions_FindCUIDByDocLocation(ci, &pos, &len, &env, &whichcaption);
		captions_DisplayNewBody(ci, thisCUID, pos, len, env);
		return(0);
	}
    }
}



void captions__MakeCachedUpdates(ci) 
struct captions *ci;
{
    char ErrorText[256], UpdateDate[AMS_DATESIZE];
    int south, j;
    long mserrcode;

    ams_TryDelayedUpdates();
    if (!ci->FullName) return;
    south = GetSouthernmostPoint(ci);
    if (south <= 0 && (ci->FolderSize > (ci->FetchedFromEnd + ci->FetchedFromStart))) {
	captions_GuaranteeFetchedRange(ci, ci->FolderSize - ci->FetchedFromEnd - 1, ci->FolderSize);
	south = GetSouthernmostPoint(ci);
    }
    for(j=0; j<ci->captioncachecount; ++j) {
	if (ci->capcache[j].offset > south) break;
    }
    if (j<=0) {
	folders_SetSkip(captions_GetFolders(ci), ci->FullName, TRUE);
	return;
    }
    strcpy(UpdateDate, ci->capcache[--j].Date);
    mserrcode = ams_MS_SetAssociatedTime(ams_GetAMS(), ci->FullName, UpdateDate);
    if (mserrcode) {
	if (ams_AMS_ERRNO(ams_GetAMS()) == ENOENT) {
	    sprintf(ErrorText, "Folder %s has recently been deleted -- profile not set", ci->ShortName);
	    ams_ReportError(ams_GetAMS(), ErrorText, ERR_WARNING, FALSE, mserrcode);
	} else if (ams_vdown(ams_GetAMS(), ams_AMS_ERRNO(ams_GetAMS())) || (ams_AMS_ERRNO(ams_GetAMS()) == EWOULDBLOCK)) {
	    sprintf(ErrorText, "Could not set profile for %s; will try again later.", ci->ShortName);
	    ams_ReportSuccess(ams_GetAMS(), ErrorText);
	    ams_CacheDelayedUpdate(ci->FullName, UpdateDate);
	} else {
	    sprintf(ErrorText, "Could not set profile for %s (%s, %d, %d, %d)", ci->ShortName, ci->FullName, ams_AMS_ERRNO(ams_GetAMS()), AMS_ERRCAUSE, AMS_ERRVIA);
	    /* UGH!  Above line only works because we've defined mserrcode locally to be the right thing... */
	    ams_ReportError(ams_GetAMS(), ErrorText, ERR_WARNING, TRUE, mserrcode);
	}
	return;
    }
    if (j >= (ci->captioncachecount-1)) {
	folders_SetSkip(captions_GetFolders(ci), ci->FullName, TRUE);
    }
}


GetCUIDFromPosition(ci, pos)
struct captions *ci;
int pos;
{
    int j;

    for(j=0; j<ci->captioncachecount; ++j) {
	if (ci->capcache[j].offset > pos) break;
    }
    if (j<=0) {
	return(-1);
    } else {
	return(ci->capcache[j].cuid);
    }
}

GetPositionFromCUID(ci, cuid)
struct captions *ci;
int cuid;
{
    int j;

    for(j=0; j<ci->captioncachecount; ++j) {
	if (ci->capcache[j].cuid == cuid) return(ci->capcache[j].offset);
    }
    return(-1);
}


captions__GuaranteeFetchedRange(ci, min, max)
struct captions *ci;
int min, max;
{
    int frontgap, backgap, oldsize, olddot, added, oldtop, south, southcuid, dotcuid, topcuid, pos;
    char TimeBuf[AMS_DATESIZE+1], SBuf[AMS_SNAPSHOTSIZE], ErrorText[500];
    long errcode;
    int RetryCount = 0;
#define RETRY_MAX 5 /* Maximum number of times to believe the folder is changing underneath me in rapid sequence. */

    if (ci->FolderSize <= 0) return(0);
    if (min < 0) min = 0;
restart:
    frontgap = max - ci->FetchedFromStart;
    backgap = (ci->FolderSize - min) - ci->FetchedFromEnd;
    if (frontgap < 0 || backgap < 0) return(0); /* already there */
    oldtop = captions_GetTopPosition(ci);
    ams_WaitCursor(TRUE);
    if (errcode = ams_MS_GetNthSnapshot(ams_GetAMS(), ci->FullName, min, SBuf)) {
	if ((RetryCount < RETRY_MAX) && (ams_AMS_ERRNO(ams_GetAMS()) == EINVAL)) {
	    /* This is the case where you have had a folder purged underneath you! */
	    char nickname[1+MAXPATHLEN], fullname[1+MAXPATHLEN];

	    strcpy(nickname, ci->ShortName);
	    strcpy(fullname, ci->FullName);
	    southcuid = GetCUIDFromPosition(ci, GetSouthernmostPoint(ci));
	    topcuid = GetCUIDFromPosition(ci, captions_GetTopPosition(ci));
	    dotcuid = GetCUIDFromPosition(ci, captions_GetDotPosition(ci));
	    captions_ClearAndUpdate(ci, FALSE, FALSE); /* Both false to inhibit recursion into GuaranteeFetchedRange again */
	    captions_InsertUpdatesInDocument(ci, nickname, fullname, FALSE);
	    RetryCount++;
	    goto restart;
	} else {
	    sprintf(ErrorText, "Could not get %dth snapshot in folder %s (size %d)", min, ci->FullName, ci->FolderSize);
	    ams_ReportError(ams_GetAMS(), ErrorText, ERR_CRITICAL, TRUE, errcode);
	    ams_WaitCursor(FALSE);
	    return(-1);
	}
    }
    if (strcmp(AMS_DATE(SBuf), "zzzzzz")) {
	strcpy(TimeBuf, amsutil_convlongto64(amsutil_conv64tolong(AMS_DATE(SBuf)) -1, 0));
    } else {
	if (min <= 0) {
	    sprintf(ErrorText, "Bad first date for %s; probably needs reconstruction.", ci->ShortName);
	} else {
	    sprintf(ErrorText, "Bad %dth date for %s; probably needs reconstruction.", min, ci->ShortName);
	}
	ams_ReportError(ams_GetAMS(), ErrorText, ERR_WARNING, FALSE, 0);
	strcpy(TimeBuf, "000000");
    }
    oldsize = text_GetLength(ci->CaptText);
    olddot = captions_GetDotPosition(ci);
    south = GetSouthernmostPoint(ci);
    captions_InsertCaptions(ci, ci->ShortName, ci->FullName, TimeBuf, FALSE);
    added = text_GetLength(ci->CaptText) - oldsize;
    captions_SetDotPosition(ci, olddot + added);
    captions_SetTopPosition(ci, oldtop + added);
    if (ci->HighlightLen > 0) {
	ci->HighlightStartPos += added;
    }
    SetSouthernmostPoint(ci, south+added);
    if (RetryCount > 0) {
	pos = GetPositionFromCUID(ci, southcuid);
	if (pos >= 0) SetSouthernmostPoint(ci, pos);
	pos = GetPositionFromCUID(ci, dotcuid);
	if (pos >= 0) captions_SetDotPosition(ci, pos);
	pos = GetPositionFromCUID(ci, topcuid);
	if (pos >= 0) captions_SetTopPosition(ci, pos);
    }
    ams_WaitCursor(FALSE);
    return (0);
}

void captions__FileCurrent(ci, FullName, nickname)
struct captions *ci;
char *FullName, *nickname;
{
    int	    cuid, OpCode;
    Boolean DoAppend = FALSE;

    if (*FullName == '*') {
	++FullName;
	DoAppend = TRUE;
    }
    cuid = ci->VisibleCUID;
    if (cuid < 1) {
	message_DisplayString(NULL, 10, "There is nothing to classify.");
	return;
    }
    ams_WaitCursor(TRUE);
    if (DoAppend) {
	OpCode = AMS_GET_ATTRIBUTE(ci->VisibleSnapshot, AMS_ATT_MAYMODIFY) ? MS_CLONE_APPENDDEL : MS_CLONE_APPEND;
    } else {
	OpCode = AMS_GET_ATTRIBUTE(ci->VisibleSnapshot, AMS_ATT_MAYMODIFY) ? MS_CLONE_COPYDEL : MS_CLONE_COPY;
    }
    if (!ams_CUI_CloneMessage(ams_GetAMS(), cuid, FullName, OpCode)) {
	/* cuilib reports errors, here we deal with success */
	if (OpCode == MS_CLONE_COPYDEL || OpCode == MS_CLONE_APPENDDEL) {
	    if (ci->HighlightEnv) text_SetEnvironmentStyle(ci->CaptText, ci->HighlightEnv, ci->ActiveDeletedStyle);
	    AMS_SET_ATTRIBUTE(ci->VisibleSnapshot, AMS_ATT_DELETED);
	    captions_PostMenus(ci, NULL);
	}
	captions_WantUpdate(ci, ci);
    }
    ams_WaitCursor(FALSE);
    return;
}

void captions__FileMarked(ci, FullName, nickname)
struct captions *ci;
char *FullName, *nickname;
{
    ClassifyMarkedByName(ci, FullName);
    return;
}

void captions__AlterPrimaryFolderName(ci, addname, delname)
struct captions *ci;
char *addname, *delname;
{
    char Nick[1+MAXPATHLEN], *s;

    if (strcmp(delname, ci->FullName)) return;

    if (addname) {
	s = malloc(1+strlen(addname));
	if (!s) return;
	free (ci->FullName);
	ci->FullName = s; 
	strcpy(ci->FullName, addname);

	ams_CUI_BuildNickName(ams_GetAMS(), addname, Nick);
	s = malloc(1+strlen(Nick));
	if (!s) return;
	free (ci->ShortName);
	ci->ShortName = s;
	strcpy(ci->ShortName, Nick);
    } else {
	/* Just Deleting the sucker on display */
	captions_ClearAndUpdate(ci, FALSE, FALSE);
    }
}

CreateCaptionsCursor(self)
struct captions *self;
{
        struct fontdesc *fd;

	fd = fontdesc_Create("icon", 0, 12);
	self->mycursor = cursor_Create(self);
	cursor_SetGlyph(self->mycursor, fd, 'R');
}

void captions__FullUpdate(self, type, left, top, width, height)
struct captions *self;
enum view_UpdateType type;
long left, top, width, height;
{
    struct rectangle Rect;

    captions_GetLogicalBounds(self, &Rect);
    super_FullUpdate(self, type, left, top, width, height);
    if (!self->mycursor) {
	CreateCaptionsCursor(self);
    }
    captions_PostCursor(self, &Rect, self->mycursor);
}

void captions__ReportMarkedMessageCount(captions)
struct captions *captions;
{
    char ErrorText[100];
    if (captions->MarkCount <=0) {
	strcpy(ErrorText, "There are now no messages marked.");
    } else if (captions->MarkCount == 1) {
	strcpy(ErrorText, "There is now one message marked.");
    } else {
	sprintf(ErrorText, "There are now %s messages marked.", amsutil_cvEng(captions->MarkCount, 0, 1000));
    }
    message_DisplayString(NULL, 10, ErrorText);
}

NextTextviewScreen(tv)
struct textview *tv;
{
    captextv_ScrollScreenFwdCmd(tv);
}

RSearchTextview(tv)
struct textview *tv;
{
    captextv_ReverseSearchCmd(tv);
}

/* This is the same as bcopy, but copies from the back to be safe when you're 
    really just extending an array by pushing back elements */

bcopyfromback(from, to, length)
char *from, *to;
int length;
{
    register char *f, *t;

    f = from + length-1;
    t = to+length-1;
    while (f >=from) {
	*t-- = *f--;
    }
}


/* The next two used to be defines, but things are getting tight again... */
static int PADTOCOLUMNA = 11;
static int PADTOCOLUMNB = 45;
static char *LOTSASPACE="                                                                ";

/* The next call returns a pointer to a static area, overwritten
	on each call */
static char CaptionBuf[AMS_SNAPSHOTSIZE];

MakeCaptionLine(Buf, cuid, RawSnapshot, Fixed, HighStart, HighLen, IsMail, IsDup, IsRead)
char **Buf, *RawSnapshot;
int cuid, Fixed, *HighStart, *HighLen;
Boolean IsMail, IsDup, IsRead;
{
    char *s, *t, *RawCap;
    int len, len2, IconCode, IconCode2;

    *HighStart = *HighLen = 0;
    if (AMS_GET_ATTRIBUTE(RawSnapshot, AMS_ATT_DELETED)) {
	IconCode = IsMail ? (IsRead ? ICON_READDELMAIL : ICON_DELMAIL) : (IsRead ? ICON_READDELPOST : ICON_DELPOST);
    } else {
	IconCode = IsMail ? (IsRead ? ICON_READMAIL : ICON_MAIL) : (IsRead ? ICON_READPOST : ICON_POST);
    }
    IconCode2 = IsDup ? ICON_DUP : ' ';
    RawCap = AMS_CAPTION(RawSnapshot);
    *Buf = CaptionBuf;
    s = index(RawCap, '\t');
/* IF BOGUS ATK tabbing is fixed, the following line should be all we ever need. */
    if (!s || ((s-RawCap)>PADTOCOLUMNA)) {
	sprintf(CaptionBuf, " %c%c%s\n", IconCode, IconCode2, RawCap);
	return(0);
    }
    *s = '\0';
    sprintf(CaptionBuf, " %c%c%s ", IconCode, IconCode2, RawCap);
    if (Fixed) {
	len2 = strlen(CaptionBuf);
	len = PADTOCOLUMNA - len2;
	if (len > 0) {
	    strncat(CaptionBuf, LOTSASPACE, len);
	    CaptionBuf[len+len2] = '\0';
	}
    } else { /* This is bogus -- can NOT get tabbing right in ATK */
	strcat(CaptionBuf, " ");
    }
    t = index(++s, '\t');
    if (!t || ((t-s)>PADTOCOLUMNB)) {
	strcat(CaptionBuf, s);
	strcat(CaptionBuf, "\n");
	return(0);
    }
    *t = '\0';

    if (Fixed) {
	strcat(CaptionBuf, s);
	len2 = strlen(CaptionBuf);
	len = PADTOCOLUMNB - len2;
	if (len > 0) {
	    strncat(CaptionBuf, LOTSASPACE, len);
	    CaptionBuf[len+len2] = '\0';
	}
    } else { /* This is bogus -- can NOT get tabbing right in ATK */
	*HighStart = strlen(CaptionBuf);
	strcat(CaptionBuf, s);
	*HighLen = strlen(CaptionBuf) - *HighStart;
	strcat(CaptionBuf, " - ");
    }
    strcat(CaptionBuf, t+1);
    strcat(CaptionBuf, "\n");
    return(0);
}

void captions__SearchAll(ci)
struct captions *ci;
{
    char ShortName[1+MAXPATHLEN], *tp, ErrorText[256];
    struct SearchPattern *Pattern = NULL;
    int pos, numfound, len, orgpos, whichcaption, oldpos;
    struct environment *envdum;
    
    if (message_AskForString(NULL, 50, "Find all occurrences of: ", "", ShortName, sizeof(ShortName))< 0) return;
    captions_GuaranteeFetchedRange(ci, 0, ci->FolderSize);
    tp = search_CompilePattern(ShortName, &Pattern);
    if (tp) {
	message_DisplayString(NULL, 10, tp);
	return;
    }
    if (ci->MarkCount > 0) {
	captions_ClearMarks(ci);
    }
    numfound = 0;
    orgpos = captions_GetDotPosition(ci);
    pos = 0;
    captions_SetDotPosition(ci, 0);
    captions_SetDotLength(ci, 0);
    while ((pos = search_MatchPattern(ci->CaptText, pos, Pattern)) >= 0) {
	oldpos = pos;
	if (captions_FindCUIDByDocLocation(ci, &pos, &len, &envdum, &whichcaption)) {
	    if (numfound++ <= 0) {
		orgpos = pos;
	    }
	    if (!ci->capcache[whichcaption].IsMarked) {
		captions_ToggleMark(ci, &ci->capcache[whichcaption], pos);
	    }
	    pos += len;
	} else {
	    pos = oldpos + 1;
	}
    }
    captions_SetDotPosition(ci, orgpos);
    captions_FrameDot(ci, orgpos);
    captions_PostMenus(ci, NULL);
    sprintf(ErrorText, "Marked %s captions containing '%s'", amsutil_cvEng(numfound, 0, 1000), ShortName);
    message_DisplayString(NULL, 10, ErrorText);
    captions_WantInputFocus(ci, ci);
}

void captions__FindRelatedMessages(self)
struct captions *self;
{
    char ErrorText[256];
    long numfound, i, j, mainchain, orgpos;
    struct CaptionCache *cc;
    
    if (self->VisibleCUID <= 0) {
	message_DisplayString(NULL, 10, "There is nothing on display.");
	return;
    }
    captions_GuaranteeFetchedRange(self, 0, self->FolderSize);
    if (self->MarkCount > 0) {
	captions_ClearMarks(self);
    }
    orgpos = captions_GetDotPosition(self);
    numfound = 0;
    bcopy(AMS_CHAIN(self->VisibleSnapshot), &i, sizeof(long));
    mainchain = ntohl(i);
    if (mainchain) {
	for (j = 0; j < self->captioncachecount; ++j) {
	    cc = &self->capcache[j];
	    if (cc->Chain == mainchain) {
		if (numfound++ <= 0) {
		    orgpos = cc->offset;
		}
		captions_ToggleMark(self, cc, cc->offset);
	    }
	}
    }
    captions_SetDotPosition(self, orgpos);
    captions_FrameDot(self, orgpos);
    captions_PostMenus(self, NULL);
    if (numfound) {
	sprintf(ErrorText, "Marked %s related messages.", amsutil_cvEng(numfound, 0, 1000));
    } else {
	strcpy(ErrorText, "There are no related messages.");
    }
    message_DisplayString(NULL, 10, ErrorText);
    captions_WantInputFocus(self, self);
}

void captions__MarkRangeOfMessages(self)
struct captions *self;
{
    char ErrorText[256], Sdate[1+AMS_DATESIZE], Edate[1+AMS_DATESIZE], DBuf[400];
    long numfound, j, orgpos;
    struct CaptionCache *cc;
    long year, month, day, hour, min, sec, wday, gtm;

    captions_GuaranteeFetchedRange(self, 0, self->FolderSize);
    if (self->MarkCount > 0) {
	captions_ClearMarks(self);
    }
    if (message_AskForString(NULL, 50, "Mark messages since date [beginning of time]: ", "", DBuf, sizeof(DBuf)) < 0) {
	return;
    }
    if (DBuf[0]) {
	long code;
	code = ams_MS_ParseDate(ams_GetAMS(), DBuf, &year, &month, &day, &hour, &min, &sec, &wday, &gtm);
	if (code) {
	    message_DisplayString(NULL, 25, "Sorry; I don't understand the date you entered.");
	    return;
	}
	strcpy(Sdate, amsutil_convlongto64(gtm, 0));
    } else {
	strcpy(Sdate, "000000");
    }
    if (message_AskForString(NULL, 50, "Mark messages through date [end of time]: ", "", DBuf, sizeof(DBuf)) < 0) {
	return;
    }
    if (DBuf[0]) {
	long code;
	code = ams_MS_ParseDate(ams_GetAMS(), DBuf, &year, &month, &day, &hour, &min, &sec, &wday, &gtm);
	if (code) {
	    message_DisplayString(NULL, 25, "Sorry; I don't understand the date you entered.");
	    return;
	}
	strcpy(Edate, amsutil_convlongto64(gtm, 0));
    } else {
	strcpy(Edate, "zzzzzz");
    }
    orgpos = captions_GetDotPosition(self);
    numfound = 0;
    for (j = 0; j < self->captioncachecount; ++j) {
	cc = &self->capcache[j];
	if (strcmp(cc->Date, Sdate) > 0 && strcmp(cc->Date, Edate) <= 0) {
	    if (numfound++ <= 0) {
		orgpos = cc->offset;
	    }
	    captions_ToggleMark(self, cc, cc->offset);
	}
    }
    captions_SetDotPosition(self, orgpos);
    captions_FrameDot(self, orgpos);
    captions_PostMenus(self, NULL);
    if (numfound) {
	sprintf(ErrorText, "Marked %s messages in the date range requested.", amsutil_cvEng(numfound, 0, 1000));
    } else {
	strcpy(ErrorText, "There are no messages in that date range.");
    }
    message_DisplayString(NULL, 10, ErrorText);
    captions_WantInputFocus(self, self);
}

static char LastClassification[1+MAXPATHLEN] = AMS_DEFAULTMAILDIR;

char *captions__GetLastClassification(self)
struct captions *self;
{
    if ((self->ShortName) && !strcmp(LastClassification, self->ShortName)) {
	return("");
    }
    return(LastClassification);
}

SetLastClassification(self, lc)
struct captions *self;
char *lc;
{
    strncpy(LastClassification, lc, sizeof(LastClassification));
}


void captions__BackUpCheckingMarks(ci, InsistOnMark)
struct captions *ci;
Boolean InsistOnMark;
{
	int pos, len, thisCUID, whichcaption;
	struct environment *env;
	Boolean FirstPass = TRUE;

restart:
	pos = ci->HighlightEnv ? environment_Eval(ci->HighlightEnv) : -1;
        if (pos <= 0) pos = captions_GetDotPosition(ci);
	thisCUID = captions_FindCUIDByDocLocation(ci, &pos, &len, &env, &whichcaption);
	if (captions_GetDotLength(ci) > 0) {
	    if (pos > 0) {
		captions_SetDotPosition(ci, pos + len + 1);
	    }
	    captions_SetDotLength(ci, 0);
	}
	--whichcaption;
	if (InsistOnMark) {
	     while (whichcaption >= 0 && !ci->capcache[whichcaption].IsMarked) {
		--whichcaption;
	     }
	}
	if (whichcaption < 0) {
	    if (FirstPass && ((ci->FetchedFromStart + ci->FetchedFromEnd) < ci->FolderSize)) {
		captions_GuaranteeFetchedRange(ci, InsistOnMark ? 0 : (ci->FolderSize - ci->FetchedFromEnd-5), ci->FolderSize);
		FirstPass = FALSE;
		goto restart;
	    } else {
		message_DisplayString(NULL, 10, "You are at the beginning of the captions");
		return;
	    }
	}
	pos = ci->capcache[whichcaption].offset;
	captions_SetDotPosition(ci, pos);
	captions_FrameDot(ci, pos + 5);
	thisCUID = captions_FindCUIDByDocLocation(ci, &pos, &len, &env, &whichcaption);
	captions_DisplayNewBody(ci, thisCUID, pos, len, env);
	return;
}

void captions__AlterFileIntoMenus(self, Shrink)
struct captions *self;
boolean Shrink;
{
    if (Shrink) {
	if (self->MenusExpanded) {
	    self->MenusExpanded = FALSE;
	    captions_PostMenus(self, NULL);
	}
    } else {
	if (!self->MenusExpanded) {
	    self->MenusExpanded = TRUE;
	    captions_PostMenus(self, NULL);
	}
    }
}

void captions__MarkCurrent(ci)
struct captions *ci;
{
    int whichcaption, linestart, linelen;
    char ErrorText[256];
    struct environment *env;

    if (!ci->HighlightEnv) {
	message_DisplayString(NULL, 10, "There is nothing to mark.");
	return;
    }
    linestart = environment_Eval(ci->HighlightEnv);
    captions_FindCUIDByDocLocation(ci, &linestart, &linelen, &env, &whichcaption);
    captions_ToggleMark(ci, &ci->capcache[whichcaption], linestart);
    captions_PostMenus(ci, NULL);
    if (ci->MarkCount <=0) {
	strcpy(ErrorText, "There are now no messages marked.");
    } else if (ci->MarkCount == 1) {
	strcpy(ErrorText, "There is now one message marked.");
    } else {
	sprintf(ErrorText, "There are now %s messages marked.", amsutil_cvEng(ci->MarkCount, 0, 1000));
    }
    message_DisplayString(NULL, 10, ErrorText);
}

void captions__Redisplay(self, Mode, contenttype)
struct captions *self;
int Mode;
char *contenttype;
{
    int dot, top, len;
    struct t822view *bod;

    if (self->VisibleCUID < 0) return;
    ams_WaitCursor(TRUE);
    bod = captions_GetBodView(self);
    dot = t822view_GetDotPosition(bod);
    top = t822view_GetTopPosition(bod);
    len = t822view_GetDotLength(bod);
    captions_GetBodyFromCUID(self, self->VisibleCUID, Mode, contenttype);
    captions_PostMenus(self, NULL);
    t822view_SetDotPosition(bod, dot);
    t822view_SetTopPosition(bod, top);
    t822view_SetDotLength(bod, len);
    ams_WaitCursor(FALSE);
}

void captions__CloneMessage(self, Code)
struct captions *self;
int Code;
{
    int cuid, MayModify;
    char NewDirName[1+MAXPATHLEN], SaveDirName[1+MAXPATHLEN], ErrorText[256];

    cuid = self->VisibleCUID;    
    if (cuid < 1) {
	message_DisplayString(NULL, 10, "There is no message on display.");
	return;
    }
    MayModify = AMS_GET_ATTRIBUTE(self->VisibleSnapshot, AMS_ATT_MAYMODIFY) ? 1 : 0;
    switch(Code) {
	case MS_CLONE_APPEND:
	case MS_CLONE_APPENDDEL:
	    sprintf(ErrorText, "Append this message to what folder [%s]:  ", captions_GetLastClassification(self));
	    if (!MayModify) Code = MS_CLONE_APPEND;
	    break;
	case MS_CLONE_COPY:
	case MS_CLONE_COPYDEL:
	    sprintf(ErrorText, "File this message into which folder [%s]: ", captions_GetLastClassification(self));
	    if (!MayModify) Code = MS_CLONE_COPY;
	    break;
	default:
	    ams_ReportError(ams_GetAMS(), "Invalid internal parameter to CloneMessage.", ERR_WARNING, FALSE, 0);
	    return;
    }
    if (ams_GetFolderName(ErrorText, NewDirName, MAXPATHLEN, "", FALSE)) return;
    if (NewDirName[0] == '\0') {
	strcpy(NewDirName, captions_GetLastClassification(self));
	if (NewDirName[0] == '\0') return;
    }
    strcpy(SaveDirName, NewDirName); /* Cuilib fiddles with the string */
    ams_WaitCursor(TRUE);
    if (!ams_CUI_CloneMessage(ams_GetAMS(), cuid, NewDirName, Code)) {
	SetLastClassification(self, SaveDirName);
	/* cuilib reports errors, here we deal with success */
	if (Code == MS_CLONE_COPYDEL || Code == MS_CLONE_APPENDDEL) {
	    if (self->HighlightEnv) text_SetEnvironmentStyle(self->CaptText, self->HighlightEnv, self->ActiveDeletedStyle);
	    AMS_SET_ATTRIBUTE(self->VisibleSnapshot, AMS_ATT_DELETED);
	    captions_AlterDeletedIcon(self, self->HighlightStartPos, TRUE);
	    captions_PostMenus(self, NULL);
	}
	captions_WantUpdate(self, self);
    }
    ams_WaitCursor(FALSE);
    return;
}

void captions__SendMessage(ci, code)
struct captions *ci;
int code; 
{
    char FileName[MAXPATHLEN+1];
    struct sendmessage *sm;

    if (code == AMS_REPLY_FRESH) {
	ams_WaitCursor(TRUE);
	sm = folders_ExposeSend(captions_GetFolders(ci));
	sendmessage_Reset(sm);
	ams_WaitCursor(FALSE);
	return;
    }
    if (ci->VisibleCUID <1) {
	message_DisplayString(NULL, 10, "There is no message on display.");
	return;
    }
    ams_WaitCursor(TRUE);
    if (ams_CUI_NameReplyFile(ams_GetAMS(), ci->VisibleCUID, code, FileName) != 0) {
	ams_WaitCursor(FALSE);
	return;
    } 
    sm = folders_ExposeSend(captions_GetFolders(ci));
    if (sm) {
	sendmessage_ReadFromFile(sm, FileName, TRUE);
	/* Put in a newline so when we insert, it comes up 
	 in the plain style. */
#if 0	/* this results in bogus questions about deleting unsent mail, and extra newlines in messages if sending is aborted... users just have to remember to be careful and click on the TOP line... not the one below it, alternately we could try to move the style -rr2b 11/92*/
	text_InsertCharacters(sm->BodyText, 0, "\n", 1);
#endif	
    }
    if (code != AMS_REPLY_FRESH)
	sendmessage_SetCurrentState(sm, SM_STATE_INPROGRESS);
    ams_WaitCursor(FALSE);
    return;
}

void captions__SetFolders(self, f)
struct captions *self;
struct folders *f;
{
    self->myfold = f;
}

struct t822view *
captions__NewBodiesInNewWindow(self)
struct captions *self;
{
    struct t822view *tv = t822view_New();
    struct text *t = text_New();

    text_SetReadOnly(t, TRUE);	/* -wjh */
    captions_SetBodies(self, tv);
    t822view_SetCaptions(tv, self);
    t822view_SetDataObject(tv, t);
    ams_InstallInNewWindow(t822view_GetApplicationLayer(tv), "messages-bodies", "Message Bodies", environ_GetProfileInt("bodies.width", 600), environ_GetProfileInt("bodies.height", 250), tv);
    return(tv);
}

struct folders *
captions__NewFoldersInNewWindow(self)
struct captions *self;
{
    struct folders *f = folders_New();

    captions_SetFolders(self, f);
    folders_SetCaptions(f, self);
    ams_InstallInNewWindow(folders_GetApplicationLayer(f), "messages-folders", "Message Folders", environ_GetProfileInt("folders.width", 600), environ_GetProfileInt("folders.height", 120), f);
    return(f);
}

struct folders *captions__GetFolders(self)
struct captions *self;
{
    if (!self->myfold) {
	captions_NewFoldersInNewWindow(self);
    }
    return(self->myfold);
}

struct t822view *captions__GetBodView(self)
struct captions *self;
{
    if (!self->BodView) {
	captions_NewBodiesInNewWindow(self);
    }
    return(self->BodView);
}


InitKeysMenus(captions)
struct captions *captions;
{
    captions->privkeys = keystate_Create(captions, captions_privkeymap);
    captions->privmenus = menulist_DuplicateML(captions_privmenulist, captions);
}

boolean captions__InitializeClass(classID) 
struct classheader *classID;
{
    class_Load("textview"); /* make sure the textview is loaded first */
    OneTimeInitKeyMenus(&captions_classinfo);
    return(TRUE);
}

boolean captions__InitializeObject(c, captions)
struct classheader *c;
struct captions *captions;  
{
    char *fontname;
    int fontsize, mailfontbloat = (amsutil_GetOptBit(EXP_WHITESPACE)) ? 2 : 0;

    captions_SetWhatIAm(captions, WHATIAM_CAPTIONS);
    InitKeysMenus(captions);
    captions->CommentText = NULL;
    captions->myframe = NULL;
    captions->MenusExpanded = FALSE;
    captions->mycursor = NULL;
    captions->MarkCount = 0;
    captions->FolderSize = captions->FetchedFromStart = captions->FetchedFromEnd = 0;

    captions->CaptText = text_New();
    captions_SetDataObject(captions, captions->CaptText);
    captions->textscrollinterface = (struct scrollfns *) super_GetInterface(captions, "scroll,vertical");

    captions->ActiveCaptionStyle = style_New();
    captions->ActiveDeletedStyle = style_New();
    captions->DeletedStyle = style_New();
    captions->FixedStyle = style_New();
    captions->GlobalCapStyle = style_New();
    captions->HighlightStyle = style_New();
    captions->IconicStyle = style_New();
    captions->UnderlinedIconicStyle = style_New();
    captions->MailStyle = style_New();
    captions->NormalCaptionStyle = style_New();

    style_AddNewFontFace(captions->FixedStyle, (long) fontdesc_Fixed);
    style_AddNewFontFace(captions->HighlightStyle, (long) fontdesc_Italic);
    style_SetFontFamily(captions->FixedStyle, "andytype");
    style_SetFontFamily(captions->IconicStyle, "msgs");
    style_SetFontFamily(captions->UnderlinedIconicStyle, "msgs");
    style_SetFontSize(captions->IconicStyle, style_ConstantFontSize, 10);
    style_SetFontSize(captions->UnderlinedIconicStyle, style_ConstantFontSize, 10);
    style_SetFontSize(captions->MailStyle, style_PreviousFontSize, mailfontbloat);
    style_SetJustification(captions->GlobalCapStyle, style_LeftJustified);
    style_SetJustification(captions->HighlightStyle, style_LeftJustified);
    style_SetName(captions->FixedStyle, "Typewriter");
    style_SetNewIndentation(captions->ActiveCaptionStyle, style_ConstantMargin, -10, style_RawDots);
    style_SetNewIndentation(captions->ActiveDeletedStyle, style_ConstantMargin, -10, style_RawDots);
    style_SetNewIndentation(captions->GlobalCapStyle, style_ConstantMargin, -10, style_RawDots);
    style_SetNewLeftMargin(captions->ActiveCaptionStyle, style_ConstantMargin, 20, style_RawDots);
    style_SetNewLeftMargin(captions->ActiveDeletedStyle, style_ConstantMargin, 20, style_RawDots);
    style_SetNewLeftMargin(captions->GlobalCapStyle, style_ConstantMargin, 20, style_RawDots);
    style_AddFlag(captions->UnderlinedIconicStyle, style_Underline);
    if (amsutil_GetOptBit(EXP_FIXCAPTIONS)) {
	style_AddNewFontFace(captions->ActiveCaptionStyle, (long) fontdesc_Bold | fontdesc_Fixed);
	style_AddNewFontFace(captions->ActiveDeletedStyle, (long) fontdesc_Bold | fontdesc_Fixed);
	style_AddNewFontFace(captions->DeletedStyle, (long) fontdesc_Fixed);
	style_AddNewFontFace(captions->GlobalCapStyle, (long) fontdesc_Fixed);
	style_AddNewFontFace(captions->HighlightStyle, (long) fontdesc_Fixed);
	style_AddNewFontFace(captions->MailStyle, (long) fontdesc_Fixed);
	style_AddNewFontFace(captions->NormalCaptionStyle, (long) fontdesc_Fixed);
    } else {
	style_AddNewFontFace(captions->ActiveCaptionStyle, (long) fontdesc_Bold);
	style_AddNewFontFace(captions->ActiveDeletedStyle, (long) fontdesc_Bold);
    }
    text_SetGlobalStyle(captions->CaptText, captions->GlobalCapStyle);

    captions->SouthPoint = NULL;

    captions->VisibleCUID = -1;
    bzero(captions->VisibleSnapshot, AMS_SNAPSHOTSIZE);
    captions->HighlightStartPos = 0;
    captions->HighlightLen = 0;
    captions->HighlightEnv = NULL;
    captions->OldMarkCount = 0;
    captions->OldMarks = NULL;
    captions->capcache = (struct CaptionCache *) malloc(25 *sizeof(struct CaptionCache));
    captions->captioncachesize = 25;
    captions->captioncachecount = 0;
    captions->FullName = NULL;
    captions->ShortName = NULL;
    captions->firstcuid = -1;
    ams_AddCheckpointCaption(captions);

    captions->myfold = NULL;

    fontsize = environ_GetProfileInt("messages.fontsize", 12);
    style_SetFontSize(captions->DeletedStyle, style_ConstantFontSize, fontsize - 4);
    style_SetFontSize(captions->ActiveDeletedStyle, style_ConstantFontSize, fontsize - 4);
    style_SetFontSize(captions->GlobalCapStyle, style_ConstantFontSize, fontsize);

    fontname = amsutil_GetDefaultFontName();
    if (amsutil_GetOptBit(EXP_FIXCAPTIONS)) fontname = "andytype";
    style_SetFontFamily(captions->ActiveCaptionStyle, fontname);
    style_SetFontFamily(captions->ActiveDeletedStyle, fontname);
    style_SetFontFamily(captions->DeletedStyle, fontname);
    style_SetFontFamily(captions->GlobalCapStyle, fontname);
    style_SetFontFamily(captions->HighlightStyle, fontname);
    style_SetFontFamily(captions->MailStyle, fontname);
    style_SetFontFamily(captions->NormalCaptionStyle, fontname);

    captions->BodView = NULL;

    captions_ShowHelp(captions);
    text_SetReadOnly(captions->CaptText, TRUE);
    return(TRUE);
}

FinalizeProcStuff(self)
struct captions *self;
{
    keystate_Destroy(self->privkeys);
    menulist_Destroy(self->privmenus);
    cursor_Destroy(self->mycursor);
}

void captions__FinalizeObject(c, self)
struct classheader *c;
struct captions *self;
{
    ams_RemoveCheckpointCaption(self);
    text_Destroy(self->CaptText);
    if (self->CommentText) {
	free(self->CommentText);
	self->CommentText = NULL;
    }
    if (self->myfold) {
	folders_SetCaptions(self->myfold, NULL);
    }
    if (self->BodView) {
	t822view_SetCaptions(self->BodView, NULL);
    }
    style_Destroy(self->ActiveCaptionStyle);
    style_Destroy(self->NormalCaptionStyle);
    style_Destroy(self->HighlightStyle);
    style_Destroy(self->GlobalCapStyle);
    style_Destroy(self->DeletedStyle);
    style_Destroy(self->ActiveDeletedStyle);
    style_Destroy(self->IconicStyle);
    style_Destroy(self->UnderlinedIconicStyle);
    style_Destroy(self->MailStyle);
    style_Destroy(self->FixedStyle);
    FinalizeProcStuff(self);
    if (self->OldMarks) {
	free(self->OldMarks);
	self->OldMarks = NULL;
    }
    if (self->capcache) {
	free(self->capcache);
	self->capcache = NULL;
    }
    if (self->FullName) {
	free(self->FullName);
	self->FullName = NULL;
    }
    if (self->ShortName) {
	free(self->ShortName);
	self->ShortName = NULL;
    }
}


void captions__PostMenus(self, ml)
struct captions *self;
struct menulist *ml;
{
    menulist_ClearChain(self->privmenus);
    if (ml) menulist_ChainAfterML(self->privmenus, ml, ml);
    super_PostMenus(self, self->privmenus);
}

void captions__PostKeyState(self, ks)
struct captions *self;
struct keystate *ks;
{
    self->privkeys->next = NULL;
    if (amsutil_GetOptBit(EXP_KEYSTROKES)) {
	if (ks) keystate_AddAfter(ks, self->privkeys);
	super_PostKeyState(self, self->privkeys);
    } else {
	super_PostKeyState(self, ks);
    }
}

void
captions__ActOnMarkedMessages(ci, Code, GivenName)
struct captions *ci;
int Code;
char *GivenName; /* Not always supplied */
{
    char ErrorText[256];
    struct CaptionCache *hc;
    int j, k, resultcode, OpCode, goodct = 0, len;
    Boolean IsActiveCaption, HadDisaster, errct;
    char HeaderBuf[2000];
    char *HeadAccum = NULL;
    struct sendmessage *sm = NULL;

    if ((ci->MarkCount <= 0) && (Code != MARKACTION_RESTORE)) {
	message_DisplayString(NULL, 10, "There are no marked messages");
	return;
     }
    ams_WaitCursor(TRUE);
    HadDisaster = FALSE;
    errct = 0;
    for (j = 0; j < ci->captioncachecount && !HadDisaster; ++j) {
	hc = &ci->capcache[j];
	if (hc->IsMarked || (Code == MARKACTION_RESTORE)) {
	    if (hc->env->data.style == ci->ActiveDeletedStyle || hc->env->data.style == ci->ActiveCaptionStyle) {
		IsActiveCaption = TRUE;
	    } else {
		IsActiveCaption = FALSE;
	    }
	    switch(Code) {
		case MARKACTION_APPENDTOFILE:
		case MARKACTION_APPENDTOFILERAW:
		    if (captions_AppendOneMessageToFile(ci, hc->cuid, GivenName, (Code == MARKACTION_APPENDTOFILERAW) ? 1 : 0)) {
			errct++;
			HadDisaster = TRUE;
		    } else ++goodct;
		    break;
		case MARKACTION_CLASSIFYBYNAME:
		case MARKACTION_APPENDBYNAME:
		case MARKACTION_COPYBYNAME:
		    if (Code == MARKACTION_APPENDBYNAME) {
			OpCode = hc->MayModify ? MS_CLONE_APPENDDEL : MS_CLONE_APPEND;
		    } else if (Code == MARKACTION_COPYBYNAME) {
			OpCode = MS_CLONE_COPY;
		    } else {
			OpCode = hc->MayModify ? MS_CLONE_COPYDEL : MS_CLONE_COPY;
		    }
		    resultcode = ams_CUI_CloneMessage(ams_GetAMS(), hc->cuid, GivenName, OpCode);
		    if (resultcode) {
			errct++;
			HadDisaster = TRUE;
			break;
		    } 		    
		    if (OpCode == MS_CLONE_APPEND || OpCode == MS_CLONE_COPY) {
			++goodct;
			break;
		    }
		    /* FALL THROUGH to delete */
		case MARKACTION_DELETE:
		    if (ams_CUI_DeleteMessage(ams_GetAMS(), hc->cuid) == 0) {
			text_SetEnvironmentStyle(ci->CaptText, hc->env, IsActiveCaption ? ci->ActiveDeletedStyle : ci->DeletedStyle);
			if (IsActiveCaption) {
			    AMS_SET_ATTRIBUTE(ci->VisibleSnapshot, AMS_ATT_DELETED);
			}
			captions_AlterDeletedIcon(ci, hc->offset, TRUE);
			++goodct;
		    } else {
			++errct;
			HadDisaster = TRUE;
			/* Errors were reported by cui routine */
		    }
		    break;
		case MARKACTION_UNDELETE:
		    if (ams_CUI_UndeleteMessage(ams_GetAMS(), hc->cuid) == 0) {
			text_SetEnvironmentStyle(ci->CaptText, hc->env, IsActiveCaption ? ci->ActiveCaptionStyle : ci->NormalCaptionStyle);
			if (IsActiveCaption) {
			    AMS_UNSET_ATTRIBUTE(ci->VisibleSnapshot, AMS_ATT_DELETED);
			}
			captions_AlterDeletedIcon(ci, hc->offset, FALSE);
			++goodct;
		    } else {
			++errct;
			HadDisaster = TRUE;
			/* Errors were reported by cui routine */
		    }
		    break;
		case MARKACTION_PRINT:
		    if (ams_CUI_PrintBodyFromCUIDWithFlags(ams_GetAMS(), hc->cuid, 0, NULL)) {
			HadDisaster = TRUE;
			++errct;
		    } else {
			++goodct;
		    }
		    /* Errors were reported by cui routine */
		    break;
		case MARKACTION_RESTORE:
		    for (k=0; k<ci->OldMarkCount; ++k) {
			if (hc->cuid == ci->OldMarks[k] && (!hc->IsMarked)) {
			    captions_ToggleMark(ci, hc, hc->offset);
			    ++goodct;
			    break;
			}
		    }
		    break;
		case MARKACTION_EXCERPT:
		    if (!sm) {
			sm = folders_ExposeSend(captions_GetFolders(ci));
			if (!sm) return;
		    }
		    if (j == (ci->captioncachecount - 1)) {
			len = text_GetLength(ci->CaptText) - hc->offset;
		    } else {
			len = ci->capcache[j+1].offset - hc->offset;
		    }
		    captions_DisplayNewBody(ci, hc->cuid, hc->offset, len, hc->env);
		    textview_SetDotPosition(sm->BodyTextview, text_GetLength((struct text *) textview_GetDataObject(sm->BodyTextview)));
		    sendmessage_QuoteBody(sm);
		    ++goodct;
		    break;
		case MARKACTION_REPLYALL:
		case MARKACTION_REPLYSENDERS:
		    HeaderBuf[0] = '\0';
		    ams_CUI_GetHeaderContents(ams_GetAMS(), hc->cuid, NULL, (Code == MARKACTION_REPLYSENDERS) ? HP_REPLY_TO : HP_ALLREPLY, HeaderBuf, sizeof(HeaderBuf) - 2);
		    if (HeadAccum) {
			HeadAccum = realloc(HeadAccum, strlen(HeadAccum) + strlen(HeaderBuf) + 5);
			strcat(HeadAccum, ",\n\t");
			strcat(HeadAccum, HeaderBuf);
		    } else {
			HeadAccum = malloc(1+strlen(HeaderBuf));
			strcpy(HeadAccum, HeaderBuf);
		    }
		    ++goodct;
		    break;
		case MARKACTION_RESEND:
		    if (ams_CUI_ResendMessage(ams_GetAMS(), hc->cuid, GivenName)) {
			HadDisaster = TRUE;
		    } else {
			++goodct;
		    }
		    break;
	    }
	    if (HadDisaster) {
		if (ams_GetBooleanFromUser(ams_GetAMS(), "Do you want to continue with the other marked messages", FALSE)) {
		    HadDisaster = FALSE;
		}
	    }
	}
    }
    if (HeadAccum) {
	if (!sm) {
	    sm = folders_ExposeSend(captions_GetFolders(ci));
	    if (!sm) return;
	}
	sendmessage_AddToToHeader(sm, HeadAccum);
	free(HeadAccum);
	HeadAccum = NULL;
    }
    if (errct) {
	char Foobar[100];
	sprintf(ErrorText, "Errors were encountered on %s ", amsutil_cvEng(errct, 0, 1000));
	sprintf(Foobar, "of the %s marked messages.", amsutil_cvEng(ci->MarkCount, 0, 1000));
	strcat(ErrorText, Foobar);
	message_DisplayString(NULL, 50, ErrorText);
    } else {
	switch (Code) {
	    case MARKACTION_RESTORE:
		sprintf(ErrorText, "Restored %s old marks.", amsutil_cvEng(goodct, 0, 1000));
		break;
	    case MARKACTION_RESEND:
		sprintf(ErrorText, "Resent %s messages to %s.", amsutil_cvEng(goodct, 0, 1000), GivenName);
		break;
	    case MARKACTION_DELETE:
		sprintf(ErrorText, "Deleted %s messages.", amsutil_cvEng(goodct, 0, 1000));
		break;
	    case MARKACTION_UNDELETE:
		sprintf(ErrorText, "Undeleted %s messages.", amsutil_cvEng(goodct, 0, 1000));
		break;
	    case MARKACTION_CLASSIFYBYNAME:
		sprintf(ErrorText, "Classified %s messages into %s.", amsutil_cvEng(goodct, 0, 1000), GivenName);
		break;
	    case MARKACTION_PRINT:
		sprintf(ErrorText, "Printed %s messages.", amsutil_cvEng(goodct, 0, 1000));
		break;
	    case MARKACTION_APPENDBYNAME:
		sprintf(ErrorText, "Appended %s messages to folder %s.", amsutil_cvEng(goodct, 0, 1000), GivenName);
		break;
	    case MARKACTION_EXCERPT:
		sprintf(ErrorText, "Excerpted %s messages.", amsutil_cvEng(goodct, 0, 1000));
		break;
	    case MARKACTION_REPLYSENDERS:
		sprintf(ErrorText, "Replying to senders of %s messages.", amsutil_cvEng(goodct, 0, 1000));
		break;
	    case MARKACTION_REPLYALL:
		sprintf(ErrorText, "Replying widely to %s messages.", amsutil_cvEng(goodct, 0, 1000));
		break;
	    case MARKACTION_COPYBYNAME:
		sprintf(ErrorText, "Copied %s messages into folder %s.", amsutil_cvEng(goodct, 0, 1000), GivenName);
		break;
	    case MARKACTION_APPENDTOFILE:
	    case MARKACTION_APPENDTOFILERAW:
		sprintf(ErrorText, "Appended %s messages to file %s.", amsutil_cvEng(goodct, 0, 1000), GivenName);
		break;
	    default:
		sprintf(ErrorText, "Did something to %s messages.", amsutil_cvEng(goodct, 0, 1000));
		break;
	}
	message_DisplayString(NULL, 10, ErrorText);
	captions_PostMenus(ci, NULL);
	if (Code == MARKACTION_CLASSIFYBYNAME
		|| Code ==  MARKACTION_APPENDBYNAME
	    || Code ==  MARKACTION_COPYBYNAME) {
	    char Nick[1+MAXPATHLEN];
	    ams_CUI_BuildNickName(ams_GetAMS(), GivenName, Nick);
	    SetLastClassification(ci, Nick);
	}
    }
    captions_WantUpdate(ci, ci);
    ams_WaitCursor(FALSE);
    return;
}

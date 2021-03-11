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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/text/RCS/drawtxtv.c,v 2.61 1993/12/09 00:17:43 gk5g Exp $";
#endif

#include <andrewos.h>
#include <class.h>
#include <text.ih>
#include <mark.ih>
#include <envrment.ih>
#include <style.ih>
#include <fontdesc.ih>
#include <graphic.ih>
#include <view.ih>
#include <dict.ih>
#include <viewref.ih>
#include <txtvinfo.h>
#include <tabs.ih>
#include <matte.ih>
#include <dataobj.ih>
#define AUXMODULE 1
#include <textv.eh>

#define textview_MOVEVIEW 99999999
static struct graphic *pat;
#define FGC 1
#define Text(self) \
    ((struct text *) ((self)->header.view.dataobject))

#define FastJustify(INFO) \
    ((INFO->endOfPara && INFO->just == \
    style_LeftAndRightJustified) || INFO->just == \
    style_LeftJustified)

#define TABBASE ((int) 'n')
/* the max number of pixels a char can overhang its bounding box */
#define MAXOVERHANG 4

static long StringWidth(widths, s)
register short *widths;
register unsigned char *s;
{
    register short w = 0;
    while (*s)
        w += widths[*s++];
    return (long) w;
}

/* Puts a 5-char sequence in string */

static void CharToOctal(s, c)
register unsigned char *s;
register char c;
{
    *s++ = '\\';
    *s++ = '0' + ((c >> 6) & 3);
    *s++ = '0' + ((c >> 3) & 7);
    *s++ = '0' + (c & 7);
    *s = '\0';
}

int drawtxtv_tabscharspaces = 8;

static long GetNextTabPosition(v, width, sv, info)
struct textview *v;
int width;
struct text_statevector *sv;
struct formattinginfo *info;
{
    int tabWidth = drawtxtv_tabscharspaces * (v->tabWidth);
    if (sv->SpecialFlags & style_TabsCharacters)
	return ((width + tabWidth)/tabWidth) * tabWidth;
    else {
#ifdef MEASURE_TABS_FROM_LM
	long lm = (sv->CurLeftMargin * v->ScreenScaleMul + v->ScreenScaleDiv / 2) / v->ScreenScaleDiv;

	if (width < lm) {
	    return lm;
	}
	else
#endif
	{
	    long t;
	    int i;
	    int m = v->ScreenScaleMul;
	    long nwidth = width * v->ScreenScaleDiv;
#ifdef MEASURE_TABS_FROM_LM
	    long marginwidth = v->ScreenScaleDiv * info->bLM;
#else /* Measure Tabs from beginning of paragraph (or beginning of wrapped line, which happens to be the left margin) */
	    long marginwidth = 0;
#endif
	    if (sv->tabs->Positions) {
		for(i = 0; i < sv->tabs->number; i++) {
		    if ((t = (marginwidth + m * sv->tabs->Positions[i])) > nwidth) {
			return (t + v->ScreenScaleDiv / 2) / v->ScreenScaleDiv;
		    }
		}
	    }
	}
    }
    /* No more tabs defined - hence should *not* perform tab */
    /* Will change tab into the width of a space */

    return width + info->myWidths[(int) ' '];

}

static long ParagraphIndent(self, text, pos, info)
struct textview *self;
struct text *text;
long pos;
register struct formattinginfo *info;
{
    struct environment *myEnv;
    struct text_statevector mysv;
    int spaceWidth;
    register int width = 0;
    register int c;
    struct fontdesc *font;
    short *widthTable;

    pos = text_GetBeginningOfLine(text, pos);

    myEnv = textview_GetStyleInformation(self, &mysv, pos, NULL);

    font = fontdesc_Create(mysv.CurFontFamily,
					    mysv.CurFontAttributes, mysv.CurFontSize);

    widthTable = fontdesc_WidthTable(font, textview_GetDrawable(self));

    spaceWidth = widthTable[(int) ' '];

    while ((c = text_GetChar(text, pos)) == ' ' || c == '\t') {
	pos++;
	if (c == ' ')
	    width += spaceWidth;
	else
	    width = GetNextTabPosition(self, width, &mysv, info);
    }

    textview_ReleaseStyleInformation(self, myEnv);
    text_FinalizeStateVector(&mysv);
    
    return width;
}

static long MovePast(self, width, widths, info, string)
struct textview *self;
register long width;
register short *widths;
struct formattinginfo *info;
register unsigned char *string;
{
    register unsigned char tc;
    register long bump = info->spaceBump;

    while (1) {
	if ((tc = *string++) == '\0')
	    return width;
	if (tc == ' ')
            width += bump;
	if (tc == '\t')
	    width = GetNextTabPosition(self, width, &info->sv, info);
        else
	    width += widths[(int) tc];
    }
}

/*
 * Finds the screen X coordinate for a given document pos
 */

static void LocateCursor(self, startX, spaceShim, startPos, widths, linePos, searchPos, info)
struct textview *self;
long startX;
long spaceShim;
long startPos;
register short *widths;
long linePos;
long searchPos;
register struct formattinginfo *info;
{
    long bx = (self->hasApplicationLayer) ? self->bx : self->ebx;

    if (startPos > searchPos)
        return;

    info->locateX = startX;   /* Default x */

    while (1) {
        register unsigned char tc = (unsigned char ) info->lineBuffer[linePos++];
	if (startPos == searchPos) {
	    info->locateX = startX;
	    return;
	}
        if (tc == '\0') {   /* At last char and still not found */
	    info->locateX = -1;
	    return;
	}
	if (tc == ' ')
	    startX += spaceShim;
	if (tc == '\t')
	    startX = GetNextTabPosition(self, startX - bx, &info->sv, info) + bx;
        else
	    startX += widths[tc];
	startPos++;
    }
}

/*
 * Finds document pos for a given screen X coordinate
 */

static void LocateHit(self, startX, spaceShim, startPos, widths, linePos, searchX, info)
struct textview *self;
long startX;
long spaceShim;
long startPos;
register short *widths;
long linePos;
long searchX;
register struct formattinginfo *info;
{
    long bx = (self->hasApplicationLayer) ? self->bx : self->ebx;

    if (startX > searchX)
        return;

    info->locateX = startPos;	/* Default pos */

    while (1) {
        register unsigned char tc;
        register int endX;

	endX = startX;
	tc = (unsigned char) info->lineBuffer[linePos++];
	if (tc == ' ')
	    endX += spaceShim;
	if (tc == '\t')
	    endX = GetNextTabPosition(self, endX-bx, &info->sv, info) + bx;
        else
	    endX += widths[tc];

	/* Now we have the start (startX) and end (endX) of char */
        /* A position more than half way across a character goes to */
        /* the next character (most noticeable with tabs) */

	if ((searchX >= startX && searchX - startX <= endX - searchX) ||
          startX >= searchX || tc == '\0') {
	    info->locateX = startPos;
	    return;
	}

	startX = endX;
	startPos++;
    }
}

static void AllocateLineItem(self, text, pos, info)
struct textview *self;
struct text *text;
long pos;
register struct formattinginfo *info;
{
    register struct fontdesc *tf;
    register struct lineitem *tlp;
    struct text_statevector *tsv;
    register int th;
    struct FontSummary *fontInfo;

    /* Now allocate a lineitem for this call */

    info->clp = tlp = &(info->lineItems[info->lineIP]);
    tsv = &info->sv;
    
    tf = tsv->CurCachedFont;
    fontInfo = fontdesc_FontSummary(tf, textview_GetDrawable(self));

    th = fontInfo->maxHeight - fontInfo->maxBelow;

    if (tsv->CurScriptMovement < 0)
        th -= tsv->CurScriptMovement;

    if (th > info->lineAbove)
	info->lineAbove = th;
    
    th = fontInfo->maxHeight - th;

    if (tsv->CurScriptMovement > 0)
        th += tsv->CurScriptMovement;

    if (th > info->textBelow) {
	info->textBelow = th;
        if (th > info->lineBelow)
            info->lineBelow = th;
    }

    tlp->docPos = pos;
    tlp->scripting = tsv->CurScriptMovement;
    tlp->xPos = info->totalWidth;	/* where we are so far */

    if (tsv->CurView == NULL) {
        tlp->type = li_Plain;
        tlp->ti_lineBufPos = info->lineBP;
        tlp->ti_rSpaces = 0;		/* really set by adder */
        tlp->ti_font = tf;		/* current font structure */
        tlp->ti_fontWidths = fontdesc_WidthTable(tf,
          textview_GetDrawable(self));
        tlp->ti_styleFlags = tsv->SpecialFlags;
        tlp->ti_hasTab = 0;
	tlp->ti_color = tsv->CurColor;
    } else {
        register struct view *view = tsv->CurView;
        long desw, desh;

        tlp->type = li_View;
	tlp->vi_view = view;

	/* Offer it the whole screen.  We really shouldn't offer it
         * more than we can deliver, based on clp->height */

	view_DesiredSize(view, info->xDim - info->totalWidth,
          16384, view_NoSet, &desw, &desh);
	tlp->vi_width = (desw > info->xDim) ? info->xDim: desw;
	tlp->vi_height = desh;
	if (tlp->vi_height > info->lineBelow) {

            long xoffset, yoffset;

	    view_GetOrigin(view, tlp->vi_width, tlp->vi_height, &xoffset, &yoffset);
            if (((tlp->vi_height) - yoffset) > info->lineBelow)
                info->lineBelow = ((tlp->vi_height) - yoffset);
            if (yoffset > info->lineAbove)
		info->lineAbove = yoffset;
        }
    }
    tlp->lineAbove=info->lineAbove;
    tlp->textBelow=info->textBelow;
    tlp->lineBelow=info->lineBelow;
    info->lineIP++;
}

/*
 * Todo: move showstyle data into textview?
 *
 * Important rule: since we do not have a position model that
 * indicates where within the style tree we are when we stop,
 * we have no way of differentiating between
 *  @bold{word
 *    }junk
 *
 *  and
 *
 *   @bold{word}
 *  junk
 *
 * In the case where there is *no* newline between these lines,  a
 * normal line wrapping operation has occurred.  In both cases, line
 * 2 is drawn from the same character position in the text buffer,
 * yet in one case, we are drawing some of the environment info on
 * that line, while in the other we are not.
 *
 * Since we do not know which we should do, given a simple buffer
 * position, we make the *convention* that lines can start with
 * @environmentname or normal text, but can not end with
 * @environmentname.  Similarly, they can end with } chars,
 * but can not begin with these close brackets.
 *
 * This gives us an unambiguous rule for how to draw a line, given
 * only a buffer position.
 *
 * A better "fix" is to rewrite this code to have another parameter,
 * indicating where we stopped drawing on the previous line.  This
 * might even work, although it should require some pervasive changes
 * to the paragraph analyzer code as well as the main redisplay.  It
 * also restricts how LineRedraw can be used. This whole file should
 * be rewritten.
 *
 * Yet another alternative is to clean this up and use the model that
 * there are certain states that one can not leave the line in, and that
 * one must backup to a previously-seen safe state.  E.g. of a safe
 * state: has no style lineitem at its end.
 */

struct fontdesc *exposeStylesFont = NULL;
short *exposeStylesWidths = NULL;
/* This definitely should be in the view */
#define NCXS 50

/* Need to look here - ajp */

static void ComputeStyleItem(self, startPos, endPos, pixelAddr, charAddr, info)
long startPos, endPos;
struct textview *self;
long *pixelAddr, *charAddr;
register struct formattinginfo *info;
{
    struct environment *startEnv, *endEnv;
    register struct environment *cparent, *te;
    struct environment *envStack[NCXS];
    struct text *text;
    register unsigned char *tp;
    long newChars, newPixels, i, nextSlot;

    if (exposeStylesFont == NULL) {
	/* Try to fill it in */
	exposeStylesFont =
          fontdesc_Create("andysans", fontdesc_Plain, 10);
	exposeStylesWidths =
          fontdesc_WidthTable(exposeStylesFont,
	    textview_GetDrawable(self));
    }

    text = Text(self);

    /* If environment starts the line, don't do the trailing curly's, */
    /* as they're on the preceding line. */

    if (startPos == -1) {
	/* Hack: try to guess when to emit a '>' char */
	if (endPos == 0)
	    startEnv = text->rootEnvironment;
	else if (textview_PrevCharIsNewline(text, endPos))
	    startEnv = textview_GetStyleInformation(self, NULL, endPos - 1, NULL);
	else {
	    startEnv = textview_GetStyleInformation(self, NULL, endPos, NULL);

            i = endPos;     /* Where we should not move from */
	    while (1) {
		/* Keep going up tree until find root or new left edge */
		if (startEnv == text->rootEnvironment)
                    break;
		if (environment_Eval(startEnv) < i)
                    break;  /* Gone too far */
		startEnv = (struct environment *)
                  startEnv->header.nestedmark.parent;
	    }
	}
    }
    else {
	startEnv = textview_GetStyleInformation(self, NULL, startPos, NULL);
    }

    endEnv = textview_GetStyleInformation(self, NULL, endPos, NULL);

    tp = &info->lineBuffer[info->lineBP];
    cparent = environment_GetCommonParent(startEnv, endEnv);
    newChars = 0;
    newPixels = 0;
    /* start out by putting out required close curlys */
    for (te = startEnv; te && te != cparent;
	te = (struct environment *) te->header.nestedmark.parent) {
	*tp++ = '>';
	newChars++;
	newPixels += exposeStylesWidths['>'];
    }
    te = endEnv;
    for (nextSlot = 0; nextSlot < NCXS; nextSlot++) {
	if (te == cparent)
            break;
	envStack[nextSlot] = te;
	te = (struct environment *) te->header.nestedmark.parent;
    }
    for (i = nextSlot - 1; i >= 0; i--) {
	/* Put out the word into the buffer */
        /* Use @view<object-name,OBJECT> for view environments */
	te = envStack[i];
        switch (te->type) {
            case environment_Style:
                strcpy((char*)tp, "@");
                if (te->data.style->name == NULL)
                    strcat((char*)tp, "?");
                else
                    strcat((char*)tp, te->data.style->name);
                strcat((char*)tp, "<");
                break;
            case environment_View:
                strcpy((char*)tp, "@view<");
                if (te->data.viewref->dataObject == NULL)
                    strcat((char*)tp, "?");
                else
                    strcat((char*)tp, class_GetTypeName(te->data.viewref->dataObject));
                strcat((char*)tp, ",");
                break;
        }
        newPixels += StringWidth(exposeStylesWidths, tp);
        while (*tp) {
            newChars++;
            tp++;
        }
    }
    /* Finally, null terminate */
    *tp++ = 0;
    newChars++;
    /* Now handle returning stuff */
    *pixelAddr = newPixels;
    *charAddr = newChars;
    info->lineBP += newChars;

    textview_ReleaseStyleInformation(self, endEnv);
    textview_ReleaseStyleInformation(self, startEnv);
}


/* Note: When trying to speed up redisplay, GenerateLineItems is a */
/* very important place to begin. */

/* This routine breaks a line up into line items.  A line item is a */
/* section of text that has the same look, or a view, or an expose */
/* styles string, etc.  Also sets a number of global variables from */
/* the top of this file so that FullLineRedraw can know about this */
/* line. Have temporarily taken out caching of the state vector. */

/* NOTE: the info structure passed in is expected to be initialized, and will be left initialized upon exit.
  The caller (text__LinRedraw) is responsible for finalizing the statevector in the info struct. */
static void GenerateLineItems(self, text, currentLine, info)
struct textview *self;
struct text *text;
struct mark *currentLine;
register struct formattinginfo *info;
{
    struct environment *myEnv;
    long lastEnvPos = 0;
    register unsigned char *tp, tc;
    long nChars, pos, bufEnd;
    long pi;            /* Value from ParagraphIndent */
    long i, localWidth, initPos, localRubber;
    int lastWidth = 0;      /* Width just before the last rubber space */
    unsigned char *lastBP;       /* &linebuffer[linebp] just before last rubber space */
    long lastIP;        /* Line item index containing last space */
    long lastPos;       /* Text pos just before the last space */
    short passes;
    long dotPos = -1, markPos = -1;
    boolean FoundmarkPos = FALSE, FounddotPos = FALSE, oncePerLine;
    long bufLen;
    unsigned char *buf = NULL;
    char *color = NULL;

    if (self->predrawn && mark_GetLength(self->predrawn) > 0 &&
      mark_GetPos(self->predrawn) == mark_GetPos(currentLine))
	markPos = mark_GetPos(self->predrawn) + mark_GetLength(self->predrawn);

    bufEnd = text_GetLength(text);		/* The ending point */
    initPos = pos = mark_GetPos(currentLine);	/* Initialize it */
    if (bufEnd > pos)
        dotPos = mark_GetPos(self->dot);
    oncePerLine = TRUE;
    passes = 0;
    info->lineBP = 0;
    lastBP = 0;
    lastPos = -1;
    lastIP = -1;

    /* First compute the current state vector: */

    while (1) {
	int envStart = pos, envLength; /* Remember beginning pos */
	struct text_statevector *tsv = &info->sv;
	long lastLineSpacing;

        if (info->lineIP == NTITEMS) {
            /* Trouble:  out of line items.  Just wrap before EOL */
	    info->endOfPara = FALSE;
	    info->below = info->lineBelow;
            return;
        }

	/* finalize the statevector since GetStyleInformation will re-initialize it. */
	text_FinalizeStateVector(tsv);
	myEnv = textview_GetStyleInformation(self, tsv, pos, &nChars);

	color = tsv->CurColor;
	envLength = environment_GetLength(myEnv);

        /* The hidden flag handling probably does not work. */

	if ((tsv->SpecialFlags & style_Hidden) && envLength > 0) {
	    pos += envLength-(environment_Eval(myEnv)-pos);
	    info->lineLength += envLength - (environment_Eval(myEnv)-pos);
	    continue;
	}

	/* Now we have to generate the proper font structure in sv */

	tsv->CurCachedFont = fontdesc_Create(tsv->CurFontFamily,
          tsv->CurFontAttributes, tsv->CurFontSize);

	if (myEnv->type == environment_View) {
	    struct viewref *vr = myEnv->data.viewref;
	    if ((tsv->CurView = (struct view *)
              dictionary_LookUp(self, (char *) vr)) == NULL) {
		if ((tsv->CurView = (struct view *) matte_Create(vr,
                  (struct view *)self)) != NULL) {
		    dictionary_Insert(self, (char *) vr,
                      (char *) tsv->CurView);
		    viewref_AddObserver(vr, self);
                    dataobject_AddObserver(vr->dataObject, text);
		    if (vr == self->currentViewreference)
                      view_WantInputFocus(tsv->CurView, tsv->CurView);
		} else
		    dictionary_Insert(self, (char *) vr,
                      (char *) textview_UNKNOWNVIEW);
	    } else
                if (tsv->CurView == (struct view *) textview_UNKNOWNVIEW)
                    tsv->CurView = NULL;
        }

	textview_ReleaseStyleInformation(self, myEnv);

	/* Break up line items at dot so we can skimp on line redrawing */

	if (pos + 1 < markPos && pos + nChars > markPos ) {
	    FoundmarkPos = TRUE;
	    nChars = markPos - pos;
	}

	if (pos <= dotPos && pos + nChars >= dotPos)
		FounddotPos = TRUE;

	/* Find end of this */

	if (oncePerLine) {
	    /* This can't be done before calling GenerateLineItems since */
            /* env must be computed before we can do these calculations. */

	    info->inContinueMode =
              (tsv->SpecialFlags & style_ContinueIndent);
	    if (! info->pBreak &&
              (tsv->SpecialFlags & style_ContinueIndent))
		pi = ParagraphIndent(self, text, pos, info);
	    else
		pi = 0;
	    if (info->pBreak) {
		info->bIndent=(tsv->CurIndentation)*self->ScreenScaleMul / self->ScreenScaleDiv;
	    }
	    else {
		info->bIndent = 0;
	    }
	    info->bLM=(tsv->CurLeftMargin + pi) * self->ScreenScaleMul / self->ScreenScaleDiv;
	    info->just = tsv->CurJustification;
	    info->xDim = info->xDim - info->bLM - info->bIndent - tsv->CurRightMargin;
	   /* Removed since Cattey's fix seems to handle the problem, and it provides a visual cue that some text was left undrawn.   This doesn't....
	    if (info->xDim < 0) info->xDim= 0;
	    this prevents info->xDim from becoming a NEGATIVE value when the leading whitespace exceeds the width of the window (and ContinueIndent Flag was Set in template). Such a situation formerly created a blank void that could not even be scrolled through.  This is only a partial fix; the text still disappears off the right edge, but at least the lines following it are still visible & scrollable!  RSK91add */
	    lastEnvPos = -1;
	    oncePerLine = 0;
	    lastLineSpacing = tsv->CurSpacing;
	}

	info->myWidths = fontdesc_WidthTable(tsv->CurCachedFont,
	    textview_GetDrawable(self));

	if (nChars > bufEnd - pos)
	    nChars = bufEnd - pos;

	/* If we're in expose style mode, and haven't done styles */
        /* for this position yet, do so. */

	if (self->exposeStyles && pos != lastEnvPos) {
	    long width, chars, oldLineBP;
	    struct lineitem *tlp;
	    oldLineBP = info->lineBP;
	    ComputeStyleItem(self, lastEnvPos, pos, &width, &chars, info);
	    if (width > 0) {
		if (info->totalWidth + width > info->xDim) {
		    info->endOfPara = FALSE;	/* Wrapped the line */
		    info->lineBP = oldLineBP;
		    info->below = info->lineBelow + lastLineSpacing;
		    return;	/* That's it for now */
		}

		tlp = &info->lineItems[info->lineIP++];

		tlp->docPos = pos;
		tlp->scripting = 0;
		tlp->xPos = info->totalWidth;
                tlp->type = li_Expose;

		tlp->ti_lineBufPos = oldLineBP;
		tlp->ti_rSpaces = 0;
		tlp->ti_font = tsv->CurCachedFont;
		tlp->ti_fontWidths = info->myWidths;
                tlp->ti_styleFlags = tsv->SpecialFlags;
                tlp->ti_hasTab = 0;
		tlp->ti_color = color;

		/* lineBP advanced in ComputeStyleItem */

		info->totalWidth += width;
	    }
	    lastEnvPos = pos;
	}

	/* Check to see if we're really done. */
        /* Always go once around. */

	if (passes > 0) {
	    if (pos >= bufEnd) {
		info->below = info->lineBelow;
		return;
	    }
	} else
            passes = 1;

	AllocateLineItem(self, text, pos, info);

	if (FounddotPos) {
	    int ipos; 
	    struct fontdesc *cfont = info->clp->ti_font;
	    info->itemAtDot = info->clp;
	    /* Find how far the same font preceeds the dot */
	    info->lastFontPos = info->clp->docPos;
	    for (ipos = info->lineIP - 2; ipos >= 0; ipos--) {
		if (info->lineItems[ipos].ti_font != cfont)
                    break;
		info->lastFontPos = info->lineItems[ipos].docPos;
	    }
	    FounddotPos = FALSE;
	}

	/* Process all of the characters into line items */

	localWidth = info->totalWidth;
	localRubber = 0;
	tp = &(info->lineBuffer[info->lineBP]);

	bufLen = 0;  /* Refresh GetBuf since pos may have changed */

	for (i = 0; ; i++) {
	    if (info->xDim < localWidth) {
		int noWrap = tsv->SpecialFlags & style_NoWrap;
		/* Wrap line */

		info->endOfPara = FALSE;

		if (noWrap || (localRubber <= 0 &&
			       info->rubberSpaces <= 0)) {
		    /* Here on word too long to fit on line; */
		    /* should really push last char to next line */

		    if (noWrap || (pos - initPos == 0) ) {
			/* If we are not wrapping lines, or if
			    this line is so far to the right that
			    no chars were processed into a line
			    item: */
                       /* Skip to end of line (so it never gets */
                       /* displayed), or the end of the env. */

			int end = envStart + envLength; 
			int count = 0;

			info->endOfPara = TRUE;
			do {
			    if (pos >= bufEnd)
				break;
			    /*if (pos >= end) {
				info->endOfPara = FALSE;
				break;
			    } RSKmoved*/
			    if (bufLen == 0)    /* filbuf */
				buf = (unsigned char *) text_GetBuf(Text(self), pos, 1024, &bufLen);
			    if (pos >= end) { /* we have to check the end-of-style condition AFTER we've "filbuffed", so we can make sure the next character isn't a newline before we waste another line of screen space. */
				if (*buf=='\n') /*RSKadd*/
				    tc = *buf++, bufLen--, pos++;
				else
				    info->endOfPara = FALSE;
				break;
			    }
			    tc = *buf++, bufLen--, pos++;
			    if(tc != ' ') count++;
			} while (tc != '\n' && tc != '\r');

			/* only put up continued mark 
			 if real characters remain */
			if (count > 1)
			    info->continued = TRUE;
		    }

		    *tp++ = 0;
		    info->lineBP = (int)(tp - info->lineBuffer);
		    info->totalWidth = localWidth;
		    info->lineLength = pos - initPos;
		    /* No rubber spaces, otherwise we'd wrap the line there */
		    if (info->clp->type != li_View) 
			info->clp->ti_rSpaces = 0;
		    info->below = info->lineBelow + tsv->CurSpacing;
		    return;
		}

		if (lastBP == 0) {	/* Is this necessary? */
		    info->lineIP--;
		    info->endOfPara = FALSE;
		    info->lineLength = info->clp->docPos - initPos;
		    info->below = info->lineBelow + lastLineSpacing;
		    return;
		}

		info->totalWidth = lastWidth;

		/* How far we've gone, less the last word */

		info->markLength = pos - initPos;
		info->lineLength = lastPos - initPos;
		info->lineIP = lastIP;
		*lastBP++ = NULL;
		/* if we saw a tab, check if it is in the part of the line we kept. */
		if(info->sawTab && index(info->lineBuffer, '\t')==NULL) info->sawTab=FALSE;
		info->lineBP = (int)(lastBP - info->lineBuffer);
		localRubber--;  /* Include deleted one */
		info->rubberSpaces += localRubber;
		if (info->clp->type != li_View) 
		    info->clp->ti_rSpaces = localRubber;
		if(info->lineIP) {
		    info->lineBelow=info->lineItems[info->lineIP-1].lineBelow;
		    info->lineAbove=info->lineItems[info->lineIP-1].lineAbove;
		    info->textBelow=info->lineItems[info->lineIP-1].textBelow;
		}
		info->below=info->lineBelow+lastLineSpacing;
		return;
	    }

	    if (i >= nChars)
		break;	/* are we done yet? */

	    if (bufLen == 0)    /* filbuf */
		buf = (unsigned char *) text_GetBuf(Text(self), pos, 1024, &bufLen);
	    tc = *buf++, bufLen--, pos++;

	    if (info->clp->type == li_View) {
		localWidth += info->clp->vi_width;
	    } else if (tc == ' ') {
		localRubber++;
		lastWidth = localWidth;
		lastBP = tp;	/* Points at the space! */
		lastPos = pos - 1;
		lastIP = info->lineIP;
		*tp++ = tc;
		localWidth += info->myWidths[(int) tc];
	    } else if (tc == '\n' || tc == '\r') {
		*tp++ = '\0';
		info->lineBP = (int)(tp - info->lineBuffer);
		info->totalWidth = localWidth;
		info->lineLength = pos - initPos;
		info->markLength = info->lineLength;
		info->rubberSpaces += localRubber;
		info->clp->ti_rSpaces = localRubber;
		if (FoundmarkPos)
		    info->predrawnEnd = info->totalWidth;
		if (tc == '\r') {
		    info->endOfPara = FALSE;
		    info->below = info->lineBelow + tsv->CurSpacing;
		}
		else {
		    info->below = info->lineBelow + ((tsv->CurSpacing > tsv->CurSpread) ? tsv->CurSpacing : tsv->CurSpread);
		}
		return;
	    } else if (tc == '\t') {
#ifdef MEASURE_TABS_FROM_LM
		localWidth += (info->bLM + info->bIndent);
#endif
		localWidth = GetNextTabPosition(self, localWidth, tsv, info);
#ifdef MEASURE_TABS_FROM_LM
		localWidth -= (info->bLM + info->bIndent);
#endif
		*tp++ = tc;
		info->sawTab = TRUE;
		info->clp->ti_hasTab = 1;
/* Tabs end processing of a line item */
		break;
	    } else if (info->myWidths[(int) tc] == 0 ||
		       ! fontdesc_CharValid(tsv->CurCachedFont, tc)) {
		if (i == 0) {
		    long width;
		    /* Gets its own lineitem.  It's not very */
		    /* efficient when there is a whole screenful */
		    /* of octal codes in a row. */
		    CharToOctal(tp, tc);
		    width = StringWidth(info->myWidths, tp);
		    if (info->totalWidth + width > info->xDim) {
			info->endOfPara = FALSE;    /* Wrapped line */
			info->lineIP--;     /* Retract this item */
			info->below = info->lineBelow + lastLineSpacing;
			return;
		    }
		    /* AllocateLineItem has set most item params */
		    info->lineItems[info->lineIP - 1].type = li_Octal;
		    tp += 5;
		    localWidth += width;
		    break;
		}
		/* Unprintable char ends processing of line item; */
		/* Back up one.  Octal will be placed in its own line */
		/* item next time around outer loop. */
		pos--;
		break;
	    } else {    /* Plain char */
		*tp++ = tc;
		localWidth += info->myWidths[tc];
	    }

	    /* End of handling of the character */
	}

	info->rubberSpaces += localRubber;
	if (info->clp->type != li_View) {
	    info->clp->ti_rSpaces = localRubber;
	}
	info->lineLength = pos - initPos;
	info->totalWidth = localWidth;
	*tp++ = 0;
	info->lineBP = (int)(tp - info->lineBuffer);

	if (FoundmarkPos) {
	    info->predrawnEnd = info->totalWidth;
	    FoundmarkPos = FALSE;
	}

	lastLineSpacing = tsv->CurSpacing;
    }
}

static void DrawBar(self, tt, bx, by, width)
struct textview *self;
struct lineitem *tt;
long bx, by;
long width;
{
    struct FontSummary *fontInfo;
    long above, below;

    if (width == 0)
        return;

    fontInfo = fontdesc_FontSummary(tt->ti_font,
      textview_GetDrawable(self));

    below = fontInfo->maxBelow;
    above = fontInfo->maxHeight - below;

    if (below > 2)
        below = 2;

    if (tt->ti_styleFlags & style_Underline) {
        textview_MoveTo(self, bx, by + below);
        textview_DrawLineTo(self, bx + width, by + below);
    }

    if (tt->ti_styleFlags & style_StrikeThrough || (self->LineThruFormatNotes && (tt->ti_styleFlags & style_PassThru))) {
        textview_MoveTo(self, bx, by - above / 3);
        textview_DrawLineTo(self, bx + width, by - above / 3);
    }

    if (tt->ti_styleFlags & style_DottedBox) {
	char *oldpattern;
	int oldoffset;
	short oldtype;

	textview_GetLineDash(self, &oldpattern, &oldoffset, &oldtype);
	textview_SetLineDash(self, "\001\001", 0, graphic_LineDoubleDash);
        textview_MoveTo(self, bx - 1, by - above + 1);
        textview_DrawLineTo(self, bx + width, by - above + 1);
        textview_DrawLineTo(self, bx + width, by + below);
        textview_DrawLineTo(self, bx - 1, by + below);
        textview_DrawLineTo(self, bx - 1, by - above + 1);
	textview_SetLineDash(self, oldpattern, oldoffset, oldtype);
	if (oldpattern) free(oldpattern);
    }

    if (tt->ti_styleFlags & style_OverBar) {
        textview_MoveTo(self, bx, by - above);
        textview_DrawLineTo(self, bx + width, by - above);
    }
}

/*
 * Draw a change bar line at the right side of the view.
 */
static void DrawChangeBar(self,info,by)
struct textview *self;
register struct formattinginfo *info;
long by;
{
    long x = textview_GetLogicalWidth(self);
    if (x > 5) {
	textview_MoveTo(self, x - 5, by - info->lineAbove);
	textview_DrawLineTo(self, x - 5, by + info->textBelow);
    }
}

/* Elimination of tabs from displayed strings
 * This function should be either eliminated or fixed. As far as I can tell,
 * the tab can only occur at the beginning or end of the string passed to this
 * function. The 1024 character array is a quick fix (it used to be 90) as it
 * would to painful to do it right at this time.
 */

static void DrawStringNoTabs(self, s, ctrl)
struct textview *self;
unsigned char *s;
int ctrl;
{
    unsigned char *st, *dt, xbuf[1024];
    for (st = s, dt = xbuf; *st; st++)
        if (*st != '\t')
            *dt++ = *st;
    *dt = '\0';
    textview_DrawString(self, xbuf, graphic_ATBASELINE);
}

static void drawcontinued(self,info,by)
struct textview *self;
register struct formattinginfo *info;
long by;
{   /* draw a pointer at the end of the line 
      to indicate that unwrapped text is 
      beyond the end of the page */
    struct rectangle clearRect;
    struct point pt[3];
    long x,ht,twid,sx ;
    twid = (self->hasApplicationLayer) ? self->bx : self->ebx;
    if(twid < 2) return;
    x = textview_GetLogicalWidth(self) ;
    by += info->textBelow;
    ht = info->lineAbove + info->textBelow;
    sx = x - 15;
    rectangle_SetRectSize(&clearRect,sx, by - ht,15,ht);
    by += -6; ht += -6;
    /* height of the pointer is dependent on the height of the text */
    /* width of the pointer is fixed */
    pt[0].x= sx; pt[0].y = by;
    pt[1].x = sx + 10 ; pt[1].y = by - (ht / 2);
    pt[2].x = sx; pt[2].y = by - ht;
    textview_SetTransferMode(self, graphic_COPY);
    textview_FillRect(self, &clearRect, textview_WhitePattern(self));
    textview_FillPolygon(self,pt, 3,textview_GrayPattern(self,12,16));
    textview_SetTransferMode(self, graphic_BLACK);
    textview_DrawPolygon(self,pt, 3);
}

/* NOTE: the info structure passed in is expected to be UN-initialized. The info structure will be initialized upon exit, but the tabs field of the statevector will be NULL. */
long textview__LineRedraw(self, type, currentLine, x, y, xSize, ySize, search, cont, txheight, info)
struct textview *self;
enum textview_LineRedrawType type;
struct mark *currentLine;
long x, y;
long xSize, ySize;
long search;
boolean *cont;
long *txheight;
register struct formattinginfo *info;
{
    struct text *text = Text(self);
    long zapMe;
    int k, lli, delta, zapPos = 0, foundSpaces = 0, pos, by, bx = 0;
    unsigned char *sPtr;
    boolean isBlack = FALSE;
    struct fontdesc *fontID = textview_GetFont(self);
    register int i, j = 0;
    register struct lineitem *tt, *tlp;
    int leftmost, leftmostchar;
    int currentBump;
    char *color = NULL;
    char *rcolor = NULL;
#ifdef FGC
    long c1, c2, c3;
#else
    double c1,c2,c3;
#endif
    char cbuf[64];
    boolean colordefined = FALSE;

    int SkipPredrawn;
    int initialIndent;
    struct tabs *gclist=NULL;

    info->xDim = xSize;
    info->totalWidth = 0;
    info->rubberSpaces = 0;
    info->lineLength = 0;
    info->markLength = -1;
    info->lineBelow = 0;
    info->textBelow = 0;
    info->lineAbove = 0;
    info->continued = FALSE;
    info->foundView = NULL;
    zapMe = -1;
    info->cursorY = y;
    info->endOfPara = TRUE;
    info->lineIP = 0;
    pos = mark_GetPos(currentLine);     /* The starting position */
    info->sawTab = FALSE;
    info->predrawnEnd = 0;
    info->itemAtDot = NULL;
    info->clp = NULL;

    /* Initialize the statevector since GenerateLineItems will assume it is initialized */
    text_InitStateVector(&info->sv);

    /* First decide if we're a paragraph break (just after newline) */

    info->pBreak =
     (pos == 0 || textview_PrevCharIsNewline(text, pos));

    if (type == textview_FullLineRedraw &&
      mark_GetModified(self->predrawn) &&
      ! mark_GetModified(self->prepredrawn)) {
	mark_GetLength(self->predrawn) = mark_GetLength(self->prepredrawn);
	self->predrawn->modified = 0;
    }

    GenerateLineItems(self, text, currentLine, info);
    if (type == textview_FullLineRedraw && info->predrawnEnd &&
	self->predrawnY == y + info->lineAbove && self->predrawnX == x && 
	FastJustify(info) && ! mark_GetModified(self->predrawn) && 
	self->csxPos > 0 /* Indicates if we are in a full redraw */ )
	SkipPredrawn = info->predrawnEnd;
    else
        SkipPredrawn = 0;

    /* Advance the marker over any blanks absorbed by a line-wrap */

    if (! info->endOfPara && text_GetChar(text, info->lineLength + pos - 1) != '\r') {
	long endPos;

	for (endPos = info->lineLength + pos;
          endPos < text_GetLength(text) &&
          text_GetChar(text, endPos) == ' '; endPos++)
            ;

        info->lineLength = endPos - pos;
	if (info->markLength < info->lineLength)
	    info->markLength = info->lineLength;
    }

    /* Before drawing the lineitems, if this is justified text, */
    /* delete rubber spaces at the start and end of each line. */
   
    if (info->just == style_LeftAndRightJustified) {
	/* Do the end */

	if (! info->endOfPara) {
	    lli = info->lineIP-1;
	    tlp =  &(info->lineItems[lli]);
            j = tlp->ti_lineBufPos;    /* The start pos for the last item */
            i = info->lineBP - 2;   /* Before nul at end of last item */
	    while (i >= j) {
		if (info->lineBuffer[i] != ' ')
                    break;              /* No more spaces */
                info->lineBuffer[i]= '\0';  /* Zap the space */
                tlp->ti_rSpaces--;      /* One less rubber space */
		info->totalWidth -= tlp->ti_fontWidths[' '];
		info->rubberSpaces--;
                i--;                /* Back up one more char */
	    }
	}
    }

    /* Taking care of tabs... */
    /* We really are going to justify this line... BUT */
    /* We want to remove all rubber spaces to the left of the tab */
    /* Coz you don't justify left of a tab */
    /* So if all of the conditions for justification are satisfied */
    /* Scan backwards through the line items, until we hit a tab */
    /* Continue scanning backwards, but from then on, 'unmark' all */
    /*  of the rubber spaces.  -njw */
    leftmost = 0;
    leftmostchar = 0;
    if (info->sawTab && info->just == style_LeftAndRightJustified && info->rubberSpaces > 0 && !info->endOfPara) {
	/* burning has three states:
	 *  0 = not burning
	 *  1 = burning as soon as tab seen
	 * >1 = burning unconditionally
	 */
	register int burning = 0; /* Whether or not we are burning rubber */

	i = info->lineBP - 2;   /* Before nul at end of last item */
	for (lli = info->lineIP - 1; lli >= 0; lli--) {
	    tlp = &(info->lineItems[lli]);
	    if (tlp->type == li_View) continue;  /* don't touch rSpaces unless it exists */
	    if (tlp->ti_hasTab)
		burning++; /* Pick up the heat */
	    if (burning) {
		j = tlp->ti_lineBufPos;    /* The start pos for this item */
		/* Scan through the characters in the line item... */
		while (i >= j) {
		    if(info->lineBuffer[i] == '\t') {
			if (burning == 1) {
			    /* This is the rightmost tab! */
			    /* We mark where justifying activates */
			    leftmost = lli;
			    leftmostchar = i;
			}
			burning++;
		    } else if (info->lineBuffer[i] == ' ' && burning > 1) {
			tlp->ti_rSpaces--; /* one less rubber space */
			info->rubberSpaces--;
		    }
		    i--;
		}
	    }
	}
    }
    /* The computations for delta and spaceBump are unchanged w.r.t tabs */
    /* the line	length has not changed,	after all... Only the number of	*/
    /* rubberSpaces to bump has decreased */

    /* Now all of the rubber spaces at the start and end of */
    /* the line are gone.  Next compute the amount of space */
    /* per rubber space that has to be inserted.  Delta is the left */
    /* over, which adds up to be a significant amount of text if */
    /* one tries to insert a fixed amount per rubber space. */

    if (info->just == style_LeftAndRightJustified &&
      info->rubberSpaces > 0) {
	if (info->endOfPara || text_GetChar(text, info->lineLength + pos - 1) == '\r') {
	    info->spaceBump = 0;
	    delta = 0;
	} else {
	    info->spaceBump = (info->xDim -
              info->totalWidth) / info->rubberSpaces;
            delta = (info->xDim - info->totalWidth) -
              info->spaceBump * info->rubberSpaces;
	}
    } else {
	/* Don't even have one rubber space */
	info->spaceBump = 0;
	delta = 0;
    }

    /* If delta is non-zero, it is how much space to jam in */
    /* after seeing (rubberspaces - delta) rubberspaces.  Find */
    /* the lineitem containing this space, and set zapPos to point */
    /* at it.  Also set foundspaces to be the number of shorter */
    /* spaces to be printed in this lineitem.  Also set zapMe to */
    /* point at this item. */

    if (delta != 0) {
	foundSpaces = info->rubberSpaces - delta;
	for (i = leftmost; i < info->lineIP; i++) {
            tlp = &info->lineItems[i];
            if (tlp->type == li_Plain) {
                if (tlp->ti_rSpaces > foundSpaces) {
                    /* Found the guy to hit */
                    zapMe = i;
		    if (info->sawTab) /* Only look right of the tabs */
			j = (leftmostchar > tlp->ti_lineBufPos)	? leftmostchar : tlp->ti_lineBufPos;
		    else 
			j = tlp->ti_lineBufPos;
                    k = foundSpaces;    /* Skip k spaces */
                    while (1) {
                        if (info->lineBuffer[j] == ' ') {
                            if (k == 0)
                                break;
                            k--;
                        }
                        j++;
                    }
                    zapPos = j;

                    /* By breaking here, foundspaces is left at the # */
                    /* of spaces printed at the original space bump */
                    /* in this lineitem */

                    break;
                }

                foundSpaces -= tlp->ti_rSpaces;
            }
        }
    }

    /* Next draw all of the lineitems */

    i = 0;

    /* Most of the work for justification follows.  In addition, */
    /* there is some code in GenerateLineItems which computes */
    /* xDim (amount of space available on line) when the first env */
    /* is entered.  So, if you're adding new Modes, don't forget to */
    /* consider changing GenerateLineItems, too.   j is extra x bump */
    /* due to rubber space expansion.  We initialize it to the place */
    /* to start drawing the line. */

    if (info->just == style_LeftAndRightJustified ||
      info->just == style_LeftJustified) {
	j = info->bIndent + info->bLM;
    } else if (info->just == style_Centered)
	j = info->bLM + (info->xDim-info->totalWidth) / 2;
    else if (info->just == style_RightJustified)
	j = info->bLM + info->xDim - info->totalWidth;
    else if (info->just == style_LeftThenRightJustified) {
	if (info->pBreak) 
	    j = info->bLM + info->bIndent;
	else
	    j = info->bLM + info->xDim - info->totalWidth;
    }

    /* If we're looking, do initializations */

    j += x;
    initialIndent = j;
    if (SkipPredrawn)
        SkipPredrawn += j;
    if (type == textview_GetCoordinate || type == textview_GetHeight)
	info->locateX = j + info->lineItems[0].xPos;
    else if (type == textview_GetPosition) {
	info->locateX = info->lineItems[0].docPos;
	info->foundView = NULL;	/* Flag tells us if mouse hit an inset */
    }

    /* We add three below to take care of overhang from the last */
    /* char, rather than compute at the proper value.  We really should */
    /* compute it properly.  This sort of counts on a reasonable clip */
    /* rectangle being set on the view. */

    /* Unfortunately, the proper thing to do is to, in the text justifier, */
    /* add in the origin to east edge to see if a char fits.  Then, if we */
    /* put a new character in, we subtract this value out and insert its */
    /* spacing value.  This way, the last character's width also includes */
    /* the distance to the edge of its bounding box.  Computationally it */
    /* is cheaper to just add a little fudge here, however. */
   
    if (type == textview_FullLineRedraw && search == 0) {
	struct rectangle clearRect;
	textview_SetTransferMode(self, graphic_COPY);
	rectangle_SetRectSize(&clearRect,
          SkipPredrawn, info->cursorY,
          textview_GetLogicalWidth(self)- SkipPredrawn,
          MIN(ySize, info->lineAbove + info->below));
	pat = textview_WhitePattern(self);
	textview_FillRect(self, &clearRect, pat);
	if (SkipPredrawn != 0 && info->endOfPara && info->lineBelow != info->below) {
	    rectangle_SetRectSize(&clearRect, 0, info->cursorY + info->lineAbove + info->lineBelow, SkipPredrawn, info->below - info->lineBelow);
	    textview_FillRect(self, &clearRect, pat);
	}
	isBlack = FALSE;
    }

    if (! isBlack) {
	textview_SetTransferMode(self, graphic_BLACK);
	isBlack = TRUE;
    }

    while (i < info->lineIP) {		/* Print each line item */
	tt = &info->lineItems[i];
	if (info->sawTab && i <= leftmost) {
	    textview_SetSpaceShim(self, 0); /* No justification left of tab */
	    currentBump =0;
	} else {
	    textview_SetSpaceShim(self, info->spaceBump);
	    currentBump = info->spaceBump;
	}
	bx = j + tt->xPos;
	sPtr = &info->lineBuffer[tt->ti_lineBufPos];
        /* by = Superscript + y coord + line "above" */
	by = tt->scripting + info->cursorY + info->lineAbove;
	if (tt->type == li_View) {
	    if (type == textview_FullLineRedraw) {
		if (bx >= SkipPredrawn) {

		    struct rectangle enclosingRect;
                    long xoffset, yoffset;

		    /* The code to display an inset; size computed in */
                    /* AllocateLineItem during line layout */

		    if (tt->vi_width > xSize)
                        tt->vi_width = xSize;
		    view_GetOrigin(tt->vi_view, tt->vi_width, tt->vi_height, &xoffset, &yoffset);
		    rectangle_SetRectSize(&enclosingRect,
                        bx - xoffset, by - yoffset, tt->vi_width, tt->vi_height);
		    view_InsertView(tt->vi_view, self, &enclosingRect);
		    textview_RetractViewCursors(self, tt->vi_view);
		    view_FullUpdate(tt->vi_view,
                      view_FullRedraw, 0, 0, 0, 0);
		}		
		info->foundView = tt->vi_view;
	    } else if (type == textview_GetPosition) {
		if (search >= bx && search <= bx + tt->vi_width)
		    info->foundView = tt->vi_view;
	    } else if (type == textview_GetCoordinate ||
                       type == textview_GetHeight) {
		/* Check if in bounds; 1 should be length of fake env */
		if (search >= tt->docPos && search < tt->docPos + 1)
		    info->locateX = bx;
                else if (i + 1 == info->lineIP && tt->docPos + 1 == search)
                    info->locateX = bx + tt->vi_width;
	    }
        } else {    /* Textual item */
	    if (self->showColorStyles) {
		if(color != tt->ti_color){
		    if(!colordefined){
		        /* first time we need color, find out the current one */
			colordefined = TRUE;
#ifdef FGC
			textview_GetForegroundColor(self,&rcolor,&c1,&c2,&c3);
			if(rcolor != NULL){
			    if (strcmp(tt->ti_color,rcolor) == 0)
			        /* foreground  already set*/
				color = tt->ti_color;
			    strncpy(cbuf,rcolor,64);
			    cbuf[63] = '\0';
			    rcolor = cbuf;
			   }
#else
			textview_GetFGColor(self,&c1,&c2,&c3);
#endif
		       }
		    if(color != tt->ti_color){
			color = tt->ti_color;
			if(color == NULL){
			    /* go back to the original color */
#ifdef FGC
			    textview_SetForegroundColor(self,rcolor,c1,c2,c3);
#else
			    textview_SetFGColor(self,c1,c2,c3);
#endif
			   }
			else {
			    textview_SetForegroundColor(self,color,NULL,NULL,NULL);
			}
		    }
		}
	    }
	    if (tt->ti_font != fontID) {
		fontID = tt->ti_font;
		textview_SetFont(self, fontID);
	    }
	    if (i != zapMe) {
		if (type == textview_FullLineRedraw) {
		    /* Drawing a line */
		    if (bx >= SkipPredrawn) {
			if (tt->type == li_Expose) {
			    fontID = exposeStylesFont;
			    textview_SetFont(self, fontID);
			    textview_MoveTo(self, bx, by);
			    textview_DrawString(self,
                              sPtr, graphic_ATBASELINE);
			} else if (info->lineAbove +
                          info->textBelow <= ySize) {
                            textview_MoveTo(self, bx, by);
                            if (tt->ti_hasTab)
                                DrawStringNoTabs(self, sPtr, graphic_ATBASELINE);
                            else
                                textview_DrawString(self,
                                        sPtr, graphic_ATBASELINE);
                            if (tt->ti_styleFlags &
                              (style_Underline | style_StrikeThrough |
                               style_OverBar | style_DottedBox)) {
                                DrawBar(self, tt, bx, by,
                                  StringWidth(tt->ti_fontWidths, sPtr) +
                                  tt->ti_rSpaces * currentBump);
			    }
			    if((tt->ti_styleFlags & style_PassThru) && self->LineThruFormatNotes) {
				DrawBar(self, tt, bx, by,
					StringWidth(tt->ti_fontWidths, sPtr) +
					tt->ti_rSpaces * currentBump);
			    }

			    if (tt->ti_styleFlags & style_ChangeBar)
				DrawChangeBar(self, info, by);
			}
		    }
		} else if (type == textview_GetPosition) {
		    /* Looking for a cursor hit */
		    if (tt->type == li_Plain)
			LocateHit(self, bx, currentBump,
                          tt->docPos, tt->ti_fontWidths,
                          tt->ti_lineBufPos, search, info);
                    else if (tt->type == li_Octal) {
                        long ex = bx +
                          StringWidth(tt->ti_fontWidths, sPtr);
                        if (search - bx >= ex - search)
                            info->locateX = tt->docPos + 1;
                    }
                    /* li_Expose are ignored */
		} else {  /* textview_GetCoordinate */
                    if (tt->type == li_Plain)
                        LocateCursor(self, bx, currentBump,
                          tt->docPos, tt->ti_fontWidths,
                          tt->ti_lineBufPos, search, info);
                    else if (tt->type == li_Expose ||
                      tt->type == li_Octal) {
                        if (tt->docPos == search)
                            info->locateX = bx;
                        else if (tt->docPos + 1 == search)
                            /* Special case at end of line. */
                            info->locateX = bx +
                              StringWidth(tt->ti_fontWidths, sPtr);
                    }
                }
                j += tt->ti_rSpaces * currentBump;
            } else {   /* i == zapMe */
		if (type == textview_GetPosition) {
		    /* Looking for document pos */
		    info->lineBuffer[zapPos] = '\0';
		    LocateHit(self, bx, currentBump,
                      tt->docPos, tt->ti_fontWidths,
                      tt->ti_lineBufPos, search, info);
		    bx = MovePast(self, bx, tt->ti_fontWidths,
                      info, sPtr);
		    info->spaceBump++;
		    currentBump++;
		    info->lineBuffer[zapPos] = ' ';
		    LocateHit(self, bx, currentBump,
                      tt->docPos + zapPos - tt->ti_lineBufPos,
                      tt->ti_fontWidths, zapPos, search, info);
		} else if (type == textview_GetCoordinate ||
                  type == textview_GetHeight) {
		    /* Looking for a hit */
		    info->lineBuffer[zapPos] = '\0';
		    LocateCursor(self, bx, currentBump,
                      tt->docPos, tt->ti_fontWidths,
                      tt->ti_lineBufPos, search, info);
		    bx = MovePast(self, bx, tt->ti_fontWidths,
                      info, sPtr);
		    info->spaceBump++;
		    currentBump++;
		    info->lineBuffer[zapPos] = ' ';
		    LocateCursor(self, bx, currentBump,
                      tt->docPos + zapPos - tt->ti_lineBufPos,
                      tt->ti_fontWidths, zapPos, search, info);
		} else {
		    /* Drawing a line: First take care of bar for entire */
                    /* line item (this is done before we mess with the */
                    /* buffer, bx, spaceBump, etc. */
                    if (tt->ti_styleFlags &
                      (style_Underline | style_StrikeThrough | style_OverBar | style_DottedBox))
                        DrawBar(self, tt, bx, by,
                          StringWidth(tt->ti_fontWidths, sPtr) +
                          tt->ti_rSpaces * (currentBump + 1) -
				foundSpaces);
		    if ((tt->ti_styleFlags & style_PassThru) && self->LineThruFormatNotes) {
			DrawBar(self, tt, bx, by,
				StringWidth(tt->ti_fontWidths, sPtr) +
				tt->ti_rSpaces * (currentBump + 1) -
				foundSpaces);
		    }    
		    if (tt->ti_styleFlags & style_ChangeBar)
			DrawChangeBar(self, info, by);
		    info->lineBuffer[zapPos] = '\0';
		    if (info->lineAbove + info->textBelow <= ySize) {
			textview_MoveTo(self, bx, by);
                        if (tt->ti_hasTab)
                            DrawStringNoTabs(self, sPtr, graphic_ATBASELINE);
                        else
                            textview_DrawString(self,
                                    sPtr, graphic_ATBASELINE);
		    }
		    bx = MovePast(self, bx, tt->ti_fontWidths,
                      info, sPtr);
		    textview_SetSpaceShim(self, ++info->spaceBump);
		    currentBump++; /* keep in sync with spaceBump */
                    info->lineBuffer[zapPos] = ' ';   /* Put ' ' back */
		    if (info->lineAbove + info->textBelow <= ySize) {
			textview_MoveTo(self, bx, by);
                        if (tt->ti_hasTab)
                            DrawStringNoTabs(self,
                                 &info->lineBuffer[zapPos], graphic_ATBASELINE);
                        else
                            textview_DrawString(self,
                                    &info->lineBuffer[zapPos],
                                    graphic_ATBASELINE);
		    }
		}
		/* spaceBump has already been incremented */

		j += (tt->ti_rSpaces * currentBump - foundSpaces);
	    }
	}
	i++;
    }

    if (type == textview_FullLineRedraw &&
      search == 0 && bx < SkipPredrawn) {
	/* Clear out garbage that may be at end of line after wrap */
	struct rectangle clearRect;
	textview_SetTransferMode(self, graphic_COPY);
	rectangle_SetRectSize(&clearRect,
          info->totalWidth + initialIndent, info->cursorY,
          SkipPredrawn - info->totalWidth - initialIndent,
          MIN(ySize, info->lineAbove + info->below));
	pat = textview_WhitePattern(self);
	textview_FillRect(self, &clearRect, pat);
	textview_SetTransferMode(self, graphic_BLACK);
    }
    if(type == textview_FullLineRedraw && info->continued)
	drawcontinued(self,info,by);

    /* Finally, set the mark length */

    if (info->markLength == -1) {
	mark_SetLength(currentLine, info->lineLength);
	currentLine->includeEnding = TRUE;
    } else {
 	mark_SetLength(currentLine, info->markLength);
	currentLine->includeEnding = FALSE;
    }

    if (type == textview_FullLineRedraw && info->itemAtDot != NULL) {
	if (FastJustify(info) && info->itemAtDot->type != li_View) {
	    /* Set marks to indicate what text prior to the dot */
            /* we  can avoid redrawing next time around. */
	    int cp, ep, WillClear;
	    long chr;
	    struct fontdesc_charInfo cinfo;
	    cp = mark_GetPos(currentLine);
	    ep = mark_GetPos(self->dot);
	    
	    /* I think what ep is trying to get at is */
	    /* everything on this line before the dot */
	    /* so if the dot is at the end of the document */
	    /* this will try to avoid the EOF -rr2b */
	    if(ep>=text_GetLength(text)) ep--;
	    
	    chr = text_GetChar(text, ep);
	    /* If ep got an EOF anyway make it something */
	    /* innocuous to avoid possible problems */
	    /* getting a font description for EOF */
	    /* Note that this doesn't appear to be necessary */
	    if(chr==EOF) chr=' ';
	    fontdesc_CharSummary(info->itemAtDot->ti_font,
              textview_GetDrawable(self), (unsigned char)chr, &cinfo);
	    WillClear = (cinfo.xOriginOffset <= 0);
	    for (ep--; ep >= cp; ep--) {
		chr = text_GetChar(text, ep);
		if (chr == ' ' || chr == '\t') {
		    if (WillClear) {
			cinfo.xOriginOffset = 0;
			break;
		    }
		    WillClear = TRUE;
		    continue;
		}
		if (ep >= info->lastFontPos) {
		    fontdesc_CharSummary(info->itemAtDot->ti_font,
                      textview_GetDrawable(self),(unsigned char)chr, &cinfo);
		    if (WillClear && cinfo.width <=
                      cinfo.xSpacing + cinfo.xOriginOffset)
			break;
		    WillClear = (cinfo.xOriginOffset <= 0);
		}
	    }
	    ep++;
	    if (ep < cp)
                ep = cp;
	    mark_SetPos(self->predrawn, cp);
	    mark_SetLength(self->predrawn, ep - cp);
	    self->predrawn->modified = 0;
	    /* Set a second mark to prevent redrawing when a */
            /* character is deleted. */
	    ep--;
	    WillClear = (cinfo.xOriginOffset <= 0);
	    for (ep--;ep >= cp;ep--) {
		chr = text_GetChar(text, ep);
		if (chr == ' ' || chr == '\t') {
		    if (WillClear) break;
		    WillClear = TRUE;
		    continue;
		}
		if (ep >= info->lastFontPos) {
		    fontdesc_CharSummary(info->itemAtDot->ti_font,
                      textview_GetDrawable(self), (unsigned char)chr, &cinfo);
		    if (WillClear && cinfo.width <=
                      cinfo.xSpacing + cinfo.xOriginOffset)
			break;
		    WillClear = (cinfo.xOriginOffset <= 0);
		}
	    }
	    ep++;
	    if (ep < cp)
                ep = cp;
	    mark_SetPos(self->prepredrawn, cp);
	    mark_SetLength(self->prepredrawn, ep - cp);
	    self->prepredrawn->modified = 0;
	    self->predrawnY = y + info->lineAbove;
	    self->predrawnX = x;
	} else {
	    /* The mark has been moved, thus this info is useless. */
	    self->prepredrawn->modified = 1;
	    self->predrawn->modified = 1;
	}
    }
    if(color != NULL){
	/* reset the original color */
#ifdef FGC
			textview_SetForegroundColor(self,rcolor,c1,c2,c3);
#else
			textview_SetFGColor(self,c1,c2,c3);
#endif
    }
    if (txheight)
        *txheight = info->lineAbove + info->textBelow;

    if ((type == textview_FullLineRedraw ||
      type == textview_PartialLineRedraw) && cont)
	*cont = (info->inContinueMode && ! info->endOfPara);

    /* finalize the statevector, the tabs aren't needed outside of this routine. */
    text_FinalizeStateVector(&info->sv);
    if (type == textview_GetPosition ||
      type == textview_GetCoordinate)
        return info->locateX;	/* We were just looking */

    return info->lineAbove + info->below;
}

void textview__ViewMove(self, lineStructure, movement)
struct textview *self;
struct linedesc *lineStructure;
long movement;
{
    struct environment *curenv;
    struct view *CurView;
    long end, i, elen;
    struct rectangle enclosingRect;
    struct mark *currentLine;

    currentLine = lineStructure->data;
    i =  mark_GetPos(currentLine);
    end = i + lineStructure->nChars;

    while (i < end) {
	curenv = textview_GetStyleInformation(self, NULL, i, &elen);

	if (elen + i > end)
	    elen = end - i;
	if (curenv->type == environment_View &&
	    ((CurView = (struct view *) 
	      dictionary_LookUp(self,(char *) (curenv->data.viewref))) != NULL)) {
	    textview_RetractViewCursors(self, CurView);
	    if (movement == textview_REMOVEVIEW) {
		rectangle_SetRectSize(&enclosingRect, 0, 0, 0, 0);
		view_InsertView(CurView, self, &enclosingRect);
		view_FullUpdate(CurView, view_Remove, 0, 0, 0, 0);
	    }
	    else if( movement == textview_MOVEVIEW){
		view_GetEnclosedBounds(CurView, &enclosingRect);
		view_InsertView(CurView, self, &enclosingRect);
		view_FullUpdate(CurView, view_MoveNoRedraw, 0, 0, 0, 0);
	    }
	    else {
		view_GetEnclosedBounds(CurView, &enclosingRect);
		rectangle_Top(&enclosingRect) += movement;
		view_InsertView(CurView, self, &enclosingRect);
		view_FullUpdate(CurView,
				view_MoveNoRedraw, 0, 0, 0, 0);
	    }
	}
	elen += i;
	i = elen;

	textview_ReleaseStyleInformation(self, curenv);
    }
}

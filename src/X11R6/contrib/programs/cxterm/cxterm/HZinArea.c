/*
 *	$Id: HZinArea.c,v 3.0 1994/06/04 07:39:38 ygz Exp $
 */

/***********************************************************
Copyright 1992,1994 by Yongguang Zhang.  All Rights Reserved.

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, 
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in 
supporting documentation, and that the names of the authors not
used in advertising or publicity pertaining to distribution of the
software without specific, written prior permission.

THE AUTHOR(S) DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
THE AUTHOR(S) BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

******************************************************************/

/* HZinArea.c		Input Area management module
 *
 * The file maintains the input area (bottom N lines) of the cxterm window.
 * The number of lines in the input area is ROWSINPUTAREA defined in ptyx.h. 
 * The current implementation of the input area uses 2 lines:
 *
 *		--------------------------------------- (the rule line)
 * line 1:	<prompt><input_buffer>         <status>
 * line 2:	<selection_list or message>
 *
 * The input area is divided into 4 parts:
 *	prompt (of the current input method),
 *	inbuf buffer (for the current key stroke sequence),
 *	status (for input processing state report), and
 * 	selection list (for the current on-screen HZ candidate list,
 *		sometimes overloaded to display error messages).
 *
 * The file contains routines to update the data structures and
 * to redraw the actual xterm window (by calling X) if necessary.
 * By calling these routines, cxterm input conversion module do not
 * need to worry how and when the actual text is drawed (just like
 * the curse library for input area).
 *
 * Each of the four parts of input area has its own refresh type.
 * It is possible that some parts have been changed and others not.
 * Due to the nature of incremental input conversion, it is very often
 * that only some portion of the text are updated.  We don't need
 * to completely redraw every characters in the input area.
 */

#include "HZinput.h"		/* X headers included here. */

extern HZ_XDrawImString16();	/* screen.c */

static void HZiaDrawRuleLine();
static void HZiaDrawText();
static void HZiaClearText();
static void HZ_XDrawImageStringMix();

/*
 * Initialize the Input Area.
 */
void HZiaInit(ia, screen)
    HZInputArea *ia;
    TScreen *screen;
{
    ia->maxlen = screen->max_col + 1;
    if (ia->maxlen > MAX_IA_WIDTH)
	ia->maxlen = MAX_IA_WIDTH;

    ia->dpyInbufLen = 0;
    ia->dpyStatusLen = 0;
    ia->dpySelStrLen = 0;
    ia->last_dpyInbufLen = 0;
    ia->last_dpyStatusLen = 0;
    ia->last_dpySelStrLen = 0;
    ia->inbufCount = 0;

    ia->prompt_mark = IA_Redraw;
    ia->inbuf_mark = IA_Redraw;
    ia->status_mark = IA_Redraw;
    ia->selection_mark = IA_Redraw;
}

/*
 * Clear up the data structures of the input area
 */
void HZiaCleanUp (ia)
    HZInputArea *ia;
{
    return ;	/* yes, no action required */
}

/*
 * Set the frame of the input area (prompt, key labels, etc)
 */
void HZiaSetFrame (ia, hzim)
    HZInputArea *ia;
    HZInputMethod *hzim;
{
    ia->prompt = hzim->prompt;
    ia->lenPrompt = hzim->lenPrompt;

    /* even a built-in input method will have a built-in hztbl */
    ia->maxchoice = hzim->hztbl->maxchoice;
    ia->choicelb = hzim->hztbl->choicelb;
    ia->keyprompt = hzim->hztbl->keyprompt;
    ia->prompt_mark = IA_Update;
}

/*
 * Update the data structure (mostly called by HZ input conversion module)
 */
void HZiaSet (ia, cl, inbuf, inbufCount, status)
    HZInputArea *ia;	/* the input area */
    HZChoiceList *cl;	/* the new choice list; NULL if unchanged */
    char *inbuf;	/* the new input buffer; NULL if unchanged */
    int inbufCount;	/* the length of the new input buffer */
    char *status;	/* the status report string */
{
    if (cl)
	HZiaSetChoiceList(ia, cl);
    if (inbuf)
	HZiaSetInBuf(ia, inbuf, inbufCount);
    if (status)
	HZiaSetStatus(ia, status);
}

void HZiaSetChoiceList (ia, cl)
    HZInputArea *ia;	/* the input area */
    HZChoiceList *cl;	/* the new choice list; NULL if unchanged */
{
    if ((cl->selNum == 0) || (! ia->choicelb)) {
	/* the choice list has been cleared */
	ia->dpySelStrLen = 0;
    } else {
	register int i;
	register Char *ptr = ia->dpySelStr;
	register struct hz_choice *pch = &(cl->choices[cl->selPos]);

	/* build the "< 1.XXXX 2.XX .... >" thing from the choice list */

	*ptr++ = (cl->selPos > 0) ? '<' : ' ';	/* mark to the left */
	for (i = 0; i < cl->selNum; i++, pch++) {
	    register int j;

	    *ptr++ = ' ';
	    *ptr++ = ia->choicelb[i];
	    *ptr++ = '.';
	    for (j = 0; j < pch->nhz; j++) {
		*ptr++ = pch->hzc[j].byte1;
		*ptr++ = pch->hzc[j].byte2;
	    }
	}
	*ptr++ = ' ';
	if ((cl->selPos + cl->selNum >= cl->numChoices) && cl->exhaust)
	    *ptr++ = ' ';
	else
	    *ptr++ = '>';				/* mark to the right */
	ia->dpySelStrLen = ptr - ia->dpySelStr;
    }
    ia->selection_mark = IA_Update;
}

void HZiaSetInBuf (ia, inbuf, inbufCount)
    HZInputArea *ia;	/* the input area */
    char *inbuf;	/* the new input buffer; NULL if unchanged */
    int inbufCount;	/* the length of the new input buffer */
{
    if (inbufCount == 0) {		/* the inbuf has been cleared */

	ia->dpyInbufLen = 0;
	ia->inbuf_mark = IA_Update;		/* start from the beginning */

    } else if ((inbufCount > ia->inbufCount) &&
	       (strncmp(inbuf, ia->inbuf, ia->inbufCount) == 0))
    {
	/* optimize for the common case of appending */
	int i, j;
	Char *ptr;

	ptr = ia->dpyInbuf + ia->dpyInbufLen;
	for (i = ia->inbufCount; i < inbufCount; i++) {
	    if ((ia->keyprompt) && !(inbuf[i] & 0x80)) {
		ia->inbuf[i] = inbuf[i];
		for (j = 0; j < ia->keyprompt[inbuf[i]].ptlen; j++)
		    *ptr++ = ia->keyprompt[inbuf[i]].prompt[j];
	    } else {
		ia->inbuf[i] = inbuf[i];  *ptr++ = inbuf[i++];
		ia->inbuf[i] = inbuf[i];  *ptr++ = inbuf[i];
	    }
	}
	ia->dpyInbufLen = ptr - ia->dpyInbuf;
	ia->inbuf_mark = IA_Increment;	/* incrementally */

    } else if ((inbufCount < ia->inbufCount) &&
	       (strncmp(inbuf, ia->inbuf, inbufCount) == 0))
    {
	/* optimize for the common case of deleting from the end */
	int i;

	for (i = inbufCount; i < ia->inbufCount; i++)
	    if ((ia->keyprompt) && !(inbuf[i] & 0x80)) {
		ia->dpyInbufLen -= ia->keyprompt[ ia->inbuf[i] ].ptlen;
	    } else {
		ia->dpyInbufLen -= 2;
		i++ ;
	    }
	ia->inbuf_mark = IA_Increment;	/* incrementally */

    } else if ((inbufCount == ia->inbufCount) &&
	       (strncmp(inbuf, ia->inbuf, inbufCount) == 0))
    {
	/* unchange since last time HZiaSet is called */
	ia->inbuf_mark = IA_Unchange;

    } else {

	/* the normal case: rewrite it */
	int i, j;
	Char *ptr;

	ptr = ia->dpyInbuf;
	for (i = 0; i < inbufCount; i++) {
	    if ((ia->keyprompt) && !(inbuf[i] & 0x80)) {
		ia->inbuf[i] = inbuf[i];
		for (j = 0; j < ia->keyprompt[inbuf[i]].ptlen; j++)
		     *ptr++ = ia->keyprompt[inbuf[i]].prompt[j];
	    } else {
		ia->inbuf[i] = inbuf[i];  *ptr++ = inbuf[i++];
		ia->inbuf[i] = inbuf[i];  *ptr++ = inbuf[i];
	    }
	}
	ia->dpyInbufLen = ptr - ia->dpyInbuf;
	ia->inbuf_mark = IA_Update;		/* start from the beginning */

    }
    ia->inbufCount = inbufCount;
}

void HZiaSetStatus (ia, status)
    HZInputArea *ia;	/* the input area */
    char *status;	/* the status report string */
{
    ia->dpyStatusLen = strlen(status);
    if ((ia->dpyStatusLen == ia->last_dpyStatusLen) &&
	(strncmp(ia->dpyStatus, status, ia->last_dpyStatusLen) == 0))
	    ia->status_mark = IA_Unchange;
    else {
	strncpy((char*)(ia->dpyStatus), status, ia->dpyStatusLen);
	ia->status_mark = IA_Update;
    }
}

/*
 * Resize the input area (caused by the resizing of xterm)
 */
void HZiaResize(ia, screen, cxin)
    HZInputArea *ia;
    TScreen *screen;
    CXtermInputModule *cxin;
{
    ia->maxlen = screen->max_col + 1;
    if (ia->maxlen > MAX_IA_WIDTH)
	ia->maxlen = MAX_IA_WIDTH;
    if (cxin->chzim->type == im_type_Builtin)
	return;

    /* the selection window now may hold more or less choices, re-make it */
    (void) HZclMakeSelection (&cxin->hzcl, ia, cxin->cur_ic);

    /* choice list may have been updated */
    HZiaSetChoiceList (ia, &cxin->hzcl);

    /* don't redraw, leave it to the Expose event */
}

/*
 * Really draw (selectively) on the screen according to the data structures.
 */
void HZiaRefresh(ia, screen)
    HZInputArea *ia;
    TScreen *screen;
{
    /* line 1 (prompt) */
    switch (ia->prompt_mark) {
      case IA_Update:
      case IA_Increment:
	HZiaClearText (screen, 0, ia->lenPrompt, 0, screen->max_col);
	/* fall through */
      case IA_Redraw:
	HZiaDrawRuleLine (screen);
	HZiaDrawText (screen, 0, 0, ia->prompt, ia->lenPrompt);
	ia->prompt_mark = IA_Unchange;
	ia->inbuf_mark = IA_Redraw;	/* must redraw input buffer */
	ia->status_mark = IA_Redraw;	/* must redraw status too */
	break;
    }

    /* line 1 (input buffer) */
    switch (ia->inbuf_mark) {
      case IA_Update:
	if (ia->dpyInbufLen < ia->last_dpyInbufLen)
	    HZiaClearText (screen, 0, ia->lenPrompt + ia->dpyInbufLen,
				   0, ia->lenPrompt + ia->last_dpyInbufLen - 1);
	/* fall through */
      case IA_Redraw:
	HZiaDrawText (screen, 0, ia->lenPrompt, ia->dpyInbuf, ia->dpyInbufLen);
	ia->inbuf_mark = IA_Unchange;
	ia->last_dpyInbufLen = ia->dpyInbufLen;
	break;

      case IA_Increment:
	if (ia->dpyInbufLen < ia->last_dpyInbufLen)
	    HZiaClearText (screen, 0, ia->lenPrompt + ia->dpyInbufLen,
				   0, ia->lenPrompt + ia->last_dpyInbufLen - 1);
	else
	    HZiaDrawText (screen, 0, ia->lenPrompt + ia->last_dpyInbufLen,
			ia->dpyInbuf + ia->last_dpyInbufLen,
			ia->dpyInbufLen - ia->last_dpyInbufLen);
	ia->inbuf_mark = IA_Unchange;
	ia->last_dpyInbufLen = ia->dpyInbufLen;
    }

    /* line 1 (status) */
    switch (ia->status_mark) {
      case IA_Update:
      case IA_Increment:
	if (ia->dpyStatusLen < ia->last_dpyStatusLen)
	    HZiaClearText (screen,
				0, screen->max_col - ia->last_dpyStatusLen + 1,
				0, screen->max_col - ia->dpyStatusLen);
	/* fall through */
      case IA_Redraw:
	HZiaDrawText (screen, 0, screen->max_col - ia->dpyStatusLen + 1,
			      ia->dpyStatus, ia->dpyStatusLen);
	ia->last_dpyStatusLen = ia->dpyStatusLen;
	ia->status_mark = IA_Unchange;
	break;
    }

    /* line 2 */
    switch (ia->selection_mark) {
      case IA_Update:
      case IA_Increment:
	if (ia->dpySelStrLen < ia->last_dpySelStrLen)
	    HZiaClearText (screen, 1, ia->dpySelStrLen,
				   1, ia->last_dpySelStrLen);
	/* fall through */
      case IA_Redraw:
	HZiaDrawText (screen, 1, 0, ia->dpySelStr, ia->dpySelStrLen);
	ia->last_dpySelStrLen = ia->dpySelStrLen;
	ia->selection_mark = IA_Unchange;
    }
}

/*
 * Do a complete redraw on everything in the input area
 */
void HZiaRedraw(ia, screen)
    HZInputArea *ia;
    TScreen *screen;
{
    HZiaClearText (screen, 0, 0, screen->hzIwin.rows, screen->max_col);
    ia->prompt_mark = IA_Redraw;
    ia->inbuf_mark = IA_Redraw;
    ia->status_mark = IA_Redraw;
    ia->selection_mark = IA_Redraw;
    HZiaRefresh(ia, screen);
}

/*
 * Temporally display one line of message on the 2nd row of the input area
 */ 
void HZiaShowMesg (screen, mesgstr)
    TScreen *screen ;
    char *mesgstr;
{
  HZInputArea *ia = &(cxtermInput.hzia);

    ia->dpySelStrLen = strlen (mesgstr);
    strncpy((char *)(ia->dpySelStr), mesgstr, ia->dpySelStrLen);
    ia->selection_mark = IA_Update;
    HZiaRefresh (ia, screen);
}


/*
 * HZiaDrawRuleLine -- draw the line that leading the input area
 */
static void HZiaDrawRuleLine (screen)
    TScreen *screen;
{
    XDrawLine (screen->display, TextWindow(screen),
		screen->hz_normalGC,
		screen->hzIwin.rule_x, screen->hzIwin.rule_y,
		screen->hzIwin.rule_x + screen->hzIwin.rule_length,
		screen->hzIwin.rule_y); 
}

/*
 * HZiaDrawText -- draw text at (row, col) of input area 
 */
static void HZiaDrawText (screen, row, col, str, nbytes)
    TScreen *screen;
    int row, col;
    Char *str;
    int nbytes;
{
    HZ_XDrawImageStringMix (screen->display, TextWindow(screen),
	screen->normalGC, screen->hz_normalGC, screen->hz_normalFGCS,
	screen->hzIwin.x +  FontWidth(screen) * col,
	screen->hzIwin.y + FontHeight(screen) * row + FontAscent(screen),
	str, nbytes, FontWidth(screen));
}

/*
 * HZiaHZiaClearText -- clear from (row1, col1) to (row2, col2) of input area
 */
static void HZiaClearText (screen, row1, col1, row2, col2)
    TScreen *screen;
    int row1, col1, row2, col2;
{
    if ((row1 > row2) || (col1 > col2))
	return;

    XClearArea (screen->display, TextWindow(screen),
	screen->hzIwin.x + FontWidth (screen) * col1,
	screen->hzIwin.y + FontHeight(screen) * row1,
	FontWidth (screen) * (col2 - col1 + 1),
	FontHeight(screen) * (row2 - row1 + 1),
	FALSE);
}

/*
 * HZ_XDrawImageStringMix -- draw mixed Chinese and Ascii strings
 */
static void
HZ_XDrawImageStringMix (display, d, gc, hzgc, fgcs,
			x, y, string, length, fontwidth)
    Display *display;
    Drawable d;
    GC gc, hzgc;
    int fgcs;
    int x, y;
    Char *string;
    register int length;
    int fontwidth;
{
    register Char *ptr = string;
    register int i;

    /* YGZ-XXX:
     * Currently I breaks the string into many segements and
     * draw them interleaving using ASCII and Hanzi fonts.
     * This is slow.  I need to improve it.  One way is to use
     * XDrawImageString to draw ASCII part and than XDrawString16
     * to paint Hanzi over the empty space left.  The other
     * is the use the "vector" draw: XDrawText and XDrawText16.
     */

    /*
     * fontwidth is used to compute the width of the string.
     * A more orthogonal way than this is to XQueryTextExtend using gc/hzgc.
     */
    while (length) {
	Char *sptr;

	/*
	 * Draw substrings as long as possible to avoid overhead.
	 * But if HZ fontwidth is not 2 * (normal fontwidth), we have to
	 * draw them one by one. Suck. I am not going to do this. -YGZ
	 */
	sptr = ptr;
	i = 0;
	while (!(*ptr & 0x80) && length) {
	    ptr++; i++; length --;
	}
	if (ptr != sptr) {
	    XDrawImageString (display, d, gc, x, y, (char *)sptr, i);
	    x += fontwidth * i;
	}

	sptr = ptr;
	i = 0;
	while ((*ptr & 0x80) && length) {
	    if (--length) {
		ptr++; ptr++; i++; length--;
		/* should check 2nd byte according to GB or BIG5 */
	    } else
		break;
	}
	if (ptr != sptr) {
	    HZ_XDrawImString16 (display, d, hzgc, fgcs,
				x, y, (XChar2b *)sptr, i);
	    x += (fontwidth * 2) * i;
	}
    }
}

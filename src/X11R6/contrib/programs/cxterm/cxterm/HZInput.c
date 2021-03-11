/*
 *	$Id: HZInput.c,v 3.0 1994/06/04 07:39:38 ygz Exp $
 */

/***********************************************************
Copyright 1992,1994 by Yongguang Zhang.  All Rights Reserved

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

/* HZInput.c
 *
 * This file defines the interface between xterm and cxterm input module.
 * All the external function defines here are called by xterm.
 *
 *	function		called by
 * -------------------		----------
 * CreateCXtermInput()		charproc.c 	(to create the input module)
 * RealizeCXtermInput()		charproc.c 	(when xterm widget realizes)
 * RefreshCXtermInput()		charproc.c util.c
 *				(when xterm refreshes the screen, e.g. expose)
 * ResetCXtermInput()		charproc.c	(when xterm is full-reset)
 * ResizeCXtermInput()		charproc.c screen.c scrollbar.c
 *				(when xterm resizes the screen)
 * HandleSwitchHZMode()		charproc.c	(switch-HZ-mode Xt action)
 * SetHZMode()			misc.c		(esc seq to switch HZ mode)
 * HZParseInput()		input.c		(keyseq => HZ)
 *
 */

#include "HZinput.h"		/* X headers included here. */

#include "data.h"

/* should be moved to ptyx.h */
extern void CreateCXtermInput();
extern void RealizeCXtermInput();
extern void ResizeCXtermInput();
extern void RefreshCXtermInput();
extern void ResetCXtermInput();
extern void HandleSwitchHZMode();
extern void SetHZMode();
extern  int HZParseInput();

/* The only global variable in the input module: _THE_ CXterm Input Module */
CXtermInputModule cxtermInput;

void CreateCXtermInput(xw)
    XtermWidget xw;
{
  TScreen *screen = &xw->screen;
  char *cp = (char *) getenv ("HZINPUTDIR");

    screen->hzIwin.rows = ROWSINPUTAREA;	/* num of rows in Input Area */
    if ((xw->misc.it_dirs == NULL) && cp) {
	xw->misc.it_dirs = (char *) malloc (strlen(cp) + 1);
	if (xw->misc.it_dirs)
	    strcpy (xw->misc.it_dirs, cp);
    }
    HZimInit (&cxtermInput, screen, &xw->misc);
}

void RealizeCXtermInput(xw)
    XtermWidget xw;
{
    if (xw->misc.hz_mode)
	SetHZMode (&xw->screen, xw->misc.it_dirs, xw->misc.hz_mode, False);
    /* don't draw it now, leave it to the Expose event */
}

void ResizeCXtermInput(screen)
    register TScreen *screen;
{
    screen->hzIwin.rule_x = screen->scrollbar;
    screen->hzIwin.rule_y = Height(screen) + screen->border;
    screen->hzIwin.rule_length = Width(screen) + 2 * screen->border;
    screen->hzIwin.x = screen->scrollbar + screen->border;
    screen->hzIwin.y = screen->hzIwin.rule_y + 1;
    screen->hzIwin.width = Width(screen);
    screen->hzIwin.height = FontHeight(screen) * screen->hzIwin.rows;

    HZiaResize (&cxtermInput.hzia, screen, &cxtermInput);
    /* don't redraw, leave it to the Expose event */
}

void RefreshCXtermInput(screen)
    register TScreen *screen;
{
    HZiaRedraw (&cxtermInput.hzia, screen);
}

void ResetCXtermInput(xw)
    XtermWidget xw;
{
  register TScreen *screen = &xw->screen;

    HZimCleanUp (&cxtermInput);
    HZimInit (&cxtermInput, screen, &xw->misc);
    RefreshCXtermInput(screen);
}

void HandleSwitchHZMode(w, event, params, nparams)
    Widget w;
    XEvent *event;
    String *params;
    Cardinal *nparams;
{
    if (*nparams == 1) {
	/* set the hanzi mode and do redraw */
	SetHZMode (&term->screen, term->misc.it_dirs, params[0], True);
    }
}

void SetHZMode (screen, inputdir, name, doredraw)
    TScreen *screen;
    char *inputdir;
    String name;
    Bool doredraw;
{
    CXtermInputModule *cxin = &cxtermInput;
    register int i;

    for (i = 0; i < cxin->numHZim; i++) {
	if (! strcmp(name, cxin->imtbl[i].name)) {
	    /* already loaded */
	    break;
	}
    }
    if (i == cxin->numHZim) {	/* not loaded */
	if (cxin->numHZim == MAX_HZIM) {	/* too many input methods */
	    char tmpstr[80];
	    sprintf (tmpstr, "Too many input methods (max = %d)", MAX_HZIM);
	    HZiaShowMesg (screen, tmpstr);
	    return;
	} else {
	    if (HZLoadInputMethod (screen, inputdir, name, cxin->encode,
				   &(cxin->imtbl[i].hzim))) {
		/* non-zero return: loading unsuccessfully */
		return;
	    }
	    cxin->imtbl[i].hzif = hzTableFilter;
	    cxin->imtbl[i].name = XtNewString(name);
	    cxin->numHZim++ ;
	}
    }

    HZswitchModeByNum (cxin, i);
    if (doredraw)
	RefreshCXtermInput (screen);
}

int HZParseInput (screen, nbytes, strbuf)
    TScreen *screen;
    int nbytes;
    char strbuf[];
{
    if (cxtermInput.mode == 0)	/* ASCII */
	return (nbytes);

    /* optimize the simply cases */
    if (nbytes == 1) {
	return ((* cxtermInput.chzif) (screen, &cxtermInput,
			strbuf[0], strbuf));
    } else if (nbytes == 0) {
	return (0);
    } else {
	char istr[100];	/* match the STRBUFSIZE in input.c */
	int i, nbytesOut = 0;

	strncpy (istr, strbuf, nbytes);
	for (i = 0; i < nbytes; i++) {
	    nbytesOut += (* cxtermInput.chzif) (screen, &cxtermInput,
			istr[i], &strbuf[nbytesOut]);
	}
	return (nbytesOut);
    }
}

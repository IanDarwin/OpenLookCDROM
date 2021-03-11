/*
 *	$Id: HZinMthd.c,v 3.0 1994/06/04 07:39:38 ygz Exp $
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

/*
 * This file defines some Input Module management routines
 */

#include "HZinput.h"

#include <stdio.h>
#include <ctype.h>
#include <errno.h>
#include <sys/param.h>
#include "data.h"

static void HZinitHistory();
static void HZcleanHistory();
static int HZLoadSimpleInputMethod();

/*
 * Initialize the input module
 */
void HZimInit (cxin, screen, misc)
    CXtermInputModule *cxin;
    TScreen *screen;
    Misc *misc;
{
    cxin->misc = misc;
    cxin->encode = HZencode (misc->hz_encoding);

    cxin->numHZim = 0;
    cxin->hzinbufCount = 0;

    HZclInit (&cxin->hzcl);
    HZiaInit (&cxin->hzia, screen);

    cxin->mode = 0;
    cxin->screen = screen;

    InitICpool ();
    HZinitHistory (cxin);

    HZLoadBuiltin (cxin);
    cxin->assocList = HZassocLoad (misc->assoc_files, misc->it_dirs,
				   misc->hz_encoding);

    HZswitchModeByNum (cxin, 0);	/* initially, ASCII mode */
}

/*
 * Reset the input module, ready for next keystroke sequence
 */
void HZimReset (cxin, dorefresh)
    CXtermInputModule *cxin;
    Boolean dorefresh;
{
    cxin->hzinbufCount = 0;
    cxin->cur_ic = NULL;
    HZclReset (&cxin->hzcl);
    HZiaSet (&cxin->hzia, &cxin->hzcl, "", 0, "");	/* reset */
    if (dorefresh)
	HZiaRefresh (&cxin->hzia, cxin->screen);
}

/*
 * Clear up the input module, discard all loaded external input methods.
 */
void HZimCleanUp (cxin)
    CXtermInputModule *cxin;
{
  int i;

    HZclCleanUp (&cxin->hzcl);
    HZiaCleanUp (&cxin->hzia);

    if (cxin->assocList)
	free ((char *)(cxin->assocList));

    for (i = 0; i < cxin->numHZim; i++) {
	switch (cxin->imtbl[i].hzim.type) {
	  case im_type_Simple:
	    UnloadCIT (cxin->imtbl[i].hzim.hztbl);
	    break;
	}
    }

    HZcleanHistory (cxin);
}

/* Initialize the conversion history (currently history==1) */
static void HZinitHistory (cxin)
    CXtermInputModule *cxin;
{
  int i;

    cxin->cur_ic = NULL;
    for (i = 0; i < MAX_INBUF; i++)
	cxin->cache_ics[i] = NULL;

    cxin->history.inbuflen = 0;
    for (i = 0; i < MAX_INBUF; i++)
	cxin->history.ics[i] = NULL;
}

/* Clean the input conversion history buffer */
static void HZcleanHistory (cxin)
    CXtermInputModule *cxin;
{
  int i;

    for (i = 0; i < MAX_INBUF; i++)
	if (cxin->history.ics[i])
	    FreeIC (cxin->history.ics[i]);
    for (i = 0; i < MAX_INBUF; i++)
	if (cxin->cache_ics[i])
	    FreeIC (cxin->cache_ics[i]);
    cxin->cur_ic = NULL;
}

/*
 * Save the current input conversion into the history
 */
void HZsaveHistory (cxin)
    CXtermInputModule *cxin;
{
  int i;

    for (i = 0; i < cxin->history.inbuflen; i++) {
	if (cxin->history.ics[i])
	    FreeIC (cxin->history.ics[i]);
    }
    for (i = 0; i < cxin->hzinbufCount; i++) {
	cxin->history.inbuf[i] = cxin->hzinbuf[i];
	cxin->history.ics[i] = cxin->cache_ics[i];
	cxin->cache_ics[i] = NULL;
    }
    cxin->history.inbuflen = cxin->hzinbufCount;
}

/*
 * Retrieve the previous input conversion from the history
 */
void HZrestoreHistory (cxin)
    CXtermInputModule *cxin;
{
  int i;

    for (i = 0; i < cxin->hzinbufCount; i++) {
	if (cxin->cache_ics[i])
	    FreeIC (cxin->cache_ics[i]);
    }
    for (i = 0; i < cxin->history.inbuflen; i++) {
	cxin->hzinbuf[i] = cxin->history.inbuf[i];
	cxin->cache_ics[i] = cxin->history.ics[i];
	cxin->history.ics[i] = NULL;
    }
    cxin->hzinbufCount = cxin->history.inbuflen;
    cxin->history.inbuflen = 0;
    if (cxin->hzinbufCount > 0)
	cxin->cur_ic = cxin->cache_ics[ cxin->hzinbufCount - 1 ];
}


/*
 * Switch Hanzi input module
 */
void HZswitchModeByNum (cxin, num)
    CXtermInputModule *cxin;
    int num;
{
    if (num >= cxin->numHZim)
	return;

    /* switch cxterm HZ state to mode i */
    cxin->mode = num;
    cxin->chzim = &(cxin->imtbl[num].hzim);
    cxin->chzif = cxin->imtbl[num].hzif;

    HZiaSetFrame(&(cxin->hzia), cxin->chzim);
    HZimReset (cxin, False);		/* don't refresh the screen now */
}

int HZLoadInputMethod (screen, inputdir, name, encode, hzim)
    TScreen *screen;
    char *inputdir;
    char *name;
    int encode;
    HZInputMethod *hzim;
{
    return (HZLoadSimpleInputMethod (screen, inputdir, name, encode, hzim));
}

/*
 * HZLoadSimpleInputMethod -- load simple input method
 */
static int HZLoadSimpleInputMethod (screen, inputdir, name, encode, hzim)
    TScreen *screen;
    char *inputdir;
    char *name;
    int encode;
    HZInputMethod *hzim;
{
    hzim->type = im_type_Simple;
    hzim->im_hztbl = (HZinputTable *) malloc (sizeof(HZinputTable));
    if (! hzim->im_hztbl) {
	char tmpstr[80];
	sprintf(tmpstr, "No memory to load input method for %s", name);
	HZiaShowMesg (screen, tmpstr);
	return (-1);
    }
    if (LoadCIT (screen, inputdir, name, encode, hzim->im_hztbl) < 0) {
	(void) free ((char *)(hzim->im_hztbl));
	return (-1);
    }
    hzim->hztbl = hzim->im_hztbl;
    hzim->prompt = hzim->hztbl->prompt;
    hzim->lenPrompt = hzim->hztbl->lenPrompt;
    return (0);
}


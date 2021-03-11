/*
 *	$Id: HZfilter.c,v 3.2 1994/06/06 12:28:11 ygz Exp $
 */

/***********************************************************
Copyright 1991,1992,1994 by Yongguang Zhang.  All Rights Reserved.

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
 * This file implements various input filters,
 * including the generic input table filter, built-in filters, etc.
 * 
 * The Function Template for input filters is:
 *
 *	int hzXXXfilter (screen, cxin, ch, strbuf)
 *	    TScreen *screen;
 *	    CXtermInputModule *cxin;
 *	    unsigned char ch;
 *	    char strbuf[];
 */


#include "HZinput.h"
#include <ctype.h>

/*****************************************************************************
 * hzTableFilter -- filter the input character using (external) input method.
 *****************************************************************************/

static int  ShowSelection();
static void DoPostSelection();

int hzTableFilter (screen, cxin, ch, strbuf)
    TScreen *screen;
    CXtermInputModule *cxin;
    unsigned char ch;		/* the input keystroke */
    char strbuf[];		/* the generated character(s) goes here */
{
  HZinputTable *hztbl = cxin->chzim->hztbl;
  HZInputArea *ia = &cxin->hzia;
  HZChoiceList *cl = &cxin->hzcl;

    if ((ch & 0x80) || (hztbl->keytype[ch] == HZ_KEY_INVALID)) {
	/* not a valid key for this input, pass out */
	if (cxin->cur_ic && (cxin->cur_ic->type == ic_type_Assoc))
	    HZimReset (cxin, True);
	strbuf[0] = ch;
	return (1);
    }

    /* Input Key?  (that includes all conjoin, wildcard, wildchar keys) */
    if (hztbl->keytype[ch] & HZ_KEY_INPUT_MASK) {
	HZInputContext *ic;

	if (cxin->hzinbufCount >= MAX_INBUF) {
	    Bell();		/* input keystroke sequence too long */
	    return (0);
	}
	cxin->hzinbuf[ cxin->hzinbufCount ] = ch;

	ic = HZinputSearch (cxin->hzinbuf + cxin->hzinbufCount, 1,
			    cxin->chzim, cxin->cur_ic);

	if (ic) {
	    if (cxin->cache_ics[cxin->hzinbufCount])
		FreeIC (cxin->cache_ics[cxin->hzinbufCount]);
	    cxin->cache_ics[ cxin->hzinbufCount++ ] = ic;
	    cxin->cur_ic = ic;

	    return (ShowSelection (cxin, strbuf, True, True));
	}
    }

    if (hztbl->keytype[ch] & HZ_KEY_SELECTION_MASK) {
	int sel;
	XChar2b *hzc;
	short int nhz;

	sel = (hztbl->keytype[ch] & HZ_KEY_SELECTION_NUM);
	nhz = HZclSelect (cl, sel, &hzc);
	if (nhz == 0) {		/* nothing to select, pass the key out */
	    strbuf[0] = ch;  return (1);
	}
	if (nhz < 0) {		/* invalid selection */
	    Bell();  return (0);
	}
	strncpy (strbuf, (char *)hzc, nhz*2);	/* pass it out */

	/* after a choice has been made */
	DoPostSelection(cxin, hzc, nhz);

	return (nhz * 2);

    } else if (hztbl->keytype[ch] == HZ_KEY_BACKSPACE) {

	if (cxin->hzinbufCount == 0) {
	    /* nothing to delete, pass the key out */
	    if (cxin->cur_ic && (cxin->cur_ic->type == ic_type_Assoc))
		HZimReset (cxin, True);
	    return (1);
	}
	cxin->hzinbufCount-- ;
	if (cxin->hzinbufCount == 0) {	/* empty buffer */
	    HZimReset (cxin, True);
	    return (0);
	}
	cxin->cur_ic = cxin->cache_ics[cxin->hzinbufCount - 1];

	return (ShowSelection (cxin, strbuf, False, True));

    } else if (hztbl->keytype[ch] == HZ_KEY_KILL) {

	if (cxin->hzinbufCount == 0) {
	    /* nothing to kill, pass the key out */
	    if (cxin->cur_ic && (cxin->cur_ic->type == ic_type_Assoc))
		HZimReset (cxin, True);
	    return (1);
	}
	cxin->hzinbufCount = 0;
	HZimReset (cxin, True);
	return (0);

    } else if (hztbl->keytype[ch] == HZ_KEY_RIGHT) {

	Boolean was_exhaust = cl->exhaust;

	if (HZclEmpty (cl))
	    return (1);		/* nothing to move, pass the key out */
	if (HZclMoveForward (cl, ia, cxin->cur_ic) == 0) {
	    Bell ();		/* no more choice to the right */
	    if (was_exhaust)
		return (0);
	    /* if not exhausted before, redisplay the current choice list */
	}
	HZiaSetChoiceList (ia, cl);
	HZiaRefresh (ia, cxin->screen);
	return (0);

    } else if (hztbl->keytype[ch] == HZ_KEY_LEFT) {

	if (HZclEmpty (cl))
	    return (1);		/* nothing to move, pass the key out */
	if (HZclMoveBackward (cl, ia, cxin->cur_ic) == 0) {
	    Bell ();
	    return (0);		/* no more choice to the left */
	}
	HZiaSetChoiceList (ia, cl);
	HZiaRefresh (ia, cxin->screen);
	return (0);

    } else if (hztbl->keytype[ch] == HZ_KEY_REPEAT) {

	if (cxin->history.inbuflen == 0) {
	    /* nothing to repeat, pass the key out */
	    if (cxin->cur_ic && (cxin->cur_ic->type == ic_type_Assoc))
		HZimReset (cxin, True);
	    return (1);
	}

	HZrestoreHistory (cxin);
	if (! cxin->cur_ic)
	    return (1);		/* nothing to repeat, pass the key out */

	/* no auto selection if replaying the last input */
	return (ShowSelection (cxin, strbuf, False, False));

    }

    /* unknown (yet valid) key */
    Bell ();
    return (0);	
}

/*
 * We got a match, prepare the choice list and update on the screen.
 */
static int ShowSelection (cxin, strbuf, first_time, allow_autoselect)
    CXtermInputModule *cxin;
    char strbuf[];
    Boolean first_time;		/* is this the first time to show this IC */
    Boolean allow_autoselect;	/* is auto selection allowed? */
{
  int numchoice;
  register HZInputArea *ia = &cxin->hzia;
  register HZChoiceList *cl = &cxin->hzcl;

    HZclReset (cl);		/* clear the choice list first */
    if (! first_time)
	/* we have shown choice before, so we need to restart the context */
	(void) HZrestartIC (cxin->cur_ic);

    /* get the first N candidates on the choice list */
    numchoice = HZclMakeSelection (cl, ia, cxin->cur_ic);

    if (allow_autoselect && cxin->chzim->hztbl->autoSelect && (numchoice == 1))
    {
	/* If there is only one candidate and auto-select is on, input it. */

	XChar2b *hzc;
	short int nhz;

	nhz = HZclSelect (cl, 0, &hzc);
	strncpy (strbuf, (char *)hzc, nhz*2);

	/* after a chooice has been made */
	DoPostSelection(cxin, hzc, nhz);

	return (nhz * 2);
    } else {
	HZiaSet (ia, cl, cxin->hzinbuf, cxin->hzinbufCount,
		 ( ((cxin->cur_ic) && (cxin->cur_ic->type == ic_type_Assoc))
		     ? (char *)cxin->assocList->prompt : "" ) );
	HZiaRefresh (ia, cxin->screen);
	return (0);
    }
}

/*
 * After a choice is made:  save in history, do association, clear the input
 */
static void DoPostSelection(cxin, hzc, nhz)
    CXtermInputModule *cxin;
    XChar2b *hzc;
    short int nhz;
{
    HZsaveHistory (cxin);
    if (cxin->assocList) {	/* we want association input */
	HZInputContext *ic;

	ic = HZassocSearch( hzc+nhz-1, cxin->chzim, cxin->assocList);
	if (ic) {
	    /* only clear up the counter, no need to do full HZimReset  */
	    cxin->hzinbufCount = 0;

	    if (cxin->cache_ics[0])
		FreeIC (cxin->cache_ics[0]);
	    cxin->cache_ics[0] = ic;
	    cxin->cur_ic = ic;

	    /* no auto selection in association */
	    (void) ShowSelection(cxin, NULL, True, False);

	    /* The associate list is in display, don't HZimReset! */
	    return;
	}
    }
    HZimReset (cxin, True);
}

/********************* Built-in Input Methods *********************/

#define hex2dec(ch)			\
	(isdigit(ch) ? (ch)-'0' :	\
		(((ch)>='a') && ((ch)<='f')) ? (ch)-'a'+10 : (ch)-'A'+10)

static int hzASCIIfilter (screen, cxin, ch, strbuf)
    TScreen *screen;
    CXtermInputModule *cxin;
    unsigned char ch;
    char strbuf[];
{
    strbuf[0] = ch;
    return(1);
}

/* IC: Internal Code */
static int hzICfilter (screen, cxin, ch, strbuf)
    TScreen *screen;
    CXtermInputModule *cxin;
    unsigned char ch;
    char strbuf[];
{
    HZInputArea *ia = &cxin->hzia;

    /*
     * Should call HZiaSet(), but, just want some extra performance ...
     */
    if (isxdigit (ch)) {
	if (cxin->hzinbufCount == 3) {
	    strbuf[0] = hex2dec(cxin->hzinbuf[0]) * 16
			+ hex2dec(cxin->hzinbuf[1]);
	    strbuf[1] = hex2dec(cxin->hzinbuf[2]) * 16 + hex2dec(ch);

	    cxin->hzinbufCount = 0;
	    ia->last_dpyInbufLen = ia->dpyInbufLen;
	    ia->dpyInbufLen = 0;
	    ia->inbuf_mark = IA_Update;
	    HZiaRefresh (ia, cxin->screen);
	    return (2);	/* 2 bytes converted in strbuf[] */
	} else {
	    cxin->hzinbuf[ cxin->hzinbufCount++ ] = ch;
	    ia->last_dpyInbufLen = ia->dpyInbufLen;
	    ia->dpyInbuf[ ia->dpyInbufLen++ ] = ch;
	    ia->inbuf_mark = IA_Increment;
	    HZiaRefresh (ia, cxin->screen);
	    return (0);
	}
    }

    switch (ch) {
	case '\010':	/* \b */
	case '\177':	/* DEL */
	    if (cxin->hzinbufCount == 0)
		return (1);	/* nothing to delete, pass the key out */
	    cxin->hzinbufCount-- ;
	    ia->last_dpyInbufLen = ia->dpyInbufLen;
	    ia->dpyInbufLen-- ;
	    ia->inbuf_mark = IA_Increment;
	    HZiaRefresh (ia, cxin->screen);
	    return (0);
	    break;

	case '\015':	/* \r */
	case '\025':	/* ^U */

	    if (cxin->hzinbufCount == 0)
		return (1);	/* nothing to kill, pass the key out */
	    cxin->hzinbufCount = 0;
	    ia->last_dpyInbufLen = ia->dpyInbufLen;
	    ia->dpyInbufLen = 0;
	    ia->inbuf_mark = IA_Update;
	    HZiaRefresh (ia, cxin->screen);
	    return (0);
	    break;
    }

    return (1);
}

/* GB: QW */
static int hzQWfilter (screen, cxin, ch, strbuf)
    TScreen *screen;
    CXtermInputModule *cxin;
    unsigned char ch;
    char strbuf[];
{
    HZInputArea *ia = &cxin->hzia;

    /*
     * Should call HZiaSet(), but, just want some extra performance ...
     */
    if (isdigit (ch)) {
	if (cxin->hzinbufCount == 3) {
	    strbuf[0] = (cxin->hzinbuf[0] - '0') * 10
			+ (cxin->hzinbuf[1] - '0') + 0xa0;
	    strbuf[1] = (cxin->hzinbuf[2] - '0') * 10 + (ch - '0') + 0xa0;

	    cxin->hzinbufCount = 0;
	    ia->last_dpyInbufLen = ia->dpyInbufLen;
	    ia->dpyInbufLen = 0;
	    ia->inbuf_mark = IA_Update;
	    HZiaRefresh (ia, cxin->screen);
	    return (2);	/* 2 bytes converted in strbuf[] */
	} else {
	    cxin->hzinbuf[ cxin->hzinbufCount++ ] = ch;
	    ia->last_dpyInbufLen = ia->dpyInbufLen;
	    ia->dpyInbuf[ ia->dpyInbufLen++ ] = ch;
	    ia->inbuf_mark = IA_Increment;
	    HZiaRefresh (ia, cxin->screen);
	    return (0);
	}
    }

    switch (ch) {
	case '\010':	/* \b */
	case '\177':	/* DEL */
	    if (cxin->hzinbufCount == 0)
		return (1);	/* nothing to delete, pass the key out */
	    cxin->hzinbufCount-- ;
	    ia->last_dpyInbufLen = ia->dpyInbufLen;
	    ia->dpyInbufLen-- ;
	    ia->inbuf_mark = IA_Increment;
	    HZiaRefresh (ia, cxin->screen);
	    return (0);
	    break;

	case '\015':	/* \r */
	case '\025':	/* ^U */

	    if (cxin->hzinbufCount == 0)
		return (1);	/* nothing to kill, pass the key out */
	    cxin->hzinbufCount = 0;
	    ia->last_dpyInbufLen = ia->dpyInbufLen;
	    ia->dpyInbufLen = 0;
	    ia->inbuf_mark = IA_Update;
	    HZiaRefresh (ia, cxin->screen);
	    return (0);
	    break;
    }

    return (1);
}

/*****  parameters for builtin input methods  *****/

struct pdHZinput {
    char *name;			/* name of input method */
    char *prompt;		/* prompt of the input method */
    unsigned char autoSelect;	/* do automatic selection? */
    int (*filter)();		/* HZ filter routine */
};

/* GB predefined */
static struct pdHZinput GBpredefine[] = {
  {
  /* 0: The first one must be ASCII -- ISO 8859-1 */
	"ASCII",
	"\323\242\316\304\312\344\310\353 (ASCII input)",
	    /* yin wen shu ru (English Input) */
        TRUE,
	hzASCIIfilter,
  },
  {
  /* 1: IC -- Internal Code, encoding independent */
	"IC",
	"\272\272\327\326\312\344\310\353\241\313\304\332\302\353\241\313 ",
	    /* han zi shu ru (Chinese Input) :: nei ma (internal code) :: */
	TRUE,
	hzICfilter,
  },
  {
  /* 2: QW -- Position, for GB coding */
	"QW",
	"\272\272\327\326\312\344\310\353\241\313\307\370\316\273\241\313 ",
	    /* han zi shu ru (Chinese Input) :: qu wei (position) :: */
	TRUE,
	hzQWfilter,
  },
  {
  /* last: null-pad */
	NULL, NULL, FALSE, hzTableFilter,
  },
};

/* BIG5 predefined */
static struct pdHZinput BIG5predefine[] = {
  {
  /* 0: The first one must be ASCII -- ISO 8859-1 */
	"ASCII",
	"\255\136\244\345\277\351\244\112 (ASCII input)",
	    /* yin wen shu ru (English Input) */
        TRUE,
	hzASCIIfilter,
  },
  {
  /* 1: IC -- Internal Code, encoding independent */
	"IC",
	"\272\176\246\162\277\351\244\112\241\107\244\272\275\130\241\107 ",
	    /* han zi shu ru (Chinese Input) : nei ma (internal code) : */
	TRUE,
	hzICfilter,
  },
  {
  /* last: null-pad */
	NULL, NULL, FALSE, hzTableFilter,
  },
};

/* JIS predefined */
static struct pdHZinput JISpredefine[] = {
  {
  /* 0: The first one must be ASCII -- ISO 8859-1 */
	"ASCII",
	"\261\321\270\354 (ASCII input)",
	    /* English (ASCII input) */
        TRUE,
	hzASCIIfilter,
  },
  {
  /* 1: IC -- Internal Code, encoding independent */
	"IC",
	"\306\374\313\334\270\354\241\332hex code\241\333 ",
	    /* Nihongo [hex code] */
	TRUE,
	hzICfilter,
  },
  {
  /* last: null-pad */
	NULL, NULL, FALSE, hzTableFilter,
  },
};

/* KS predefined */
static struct pdHZinput KSpredefine[] = {
  {
  /* 0: The first one must be ASCII -- ISO 8859-1 */
	"ASCII",
	"\241\274 ASCII input \241\275",
	    /* [ English Input ] */
        TRUE,
	hzASCIIfilter,
  },
  {
  /* 1: IC -- Internal Code, encoding independent */
	"IC",
	"\307\321\261\333\300\324\267\302\241\274hex code\241\275 ",
	    /* Hangul Input [ hex code ] */
	TRUE,
	hzICfilter,
  },
  {
  /* last: null-pad */
	NULL, NULL, FALSE, hzTableFilter,
  },
};

static HZinputTable builtin_hztbl;

/*
 * HZLoadBuiltin -- load the built-in HZ input tables
 */
void HZLoadBuiltin(cxin)
    CXtermInputModule *cxin;
{
    register struct pdHZinput *p;
    int i;

    switch (cxin->encode) {
      case GB_ENCODE:
	p = GBpredefine;
	break;
      case BIG5_ENCODE:
	p = BIG5predefine;
	break;
      case JIS_ENCODE:
	p = JISpredefine;
	break;
      case KS_ENCODE:
	p = KSpredefine;
	break;
      default:
	p = GBpredefine;	/* just use the default case! */
    }

    builtin_hztbl.maxchoice = 0;
    builtin_hztbl.lenPrompt = 0;
    for (i = 0; i < 127; i++) {
	builtin_hztbl.keyprompt[i].prompt[0] = (unsigned char)i;
	builtin_hztbl.keyprompt[i].ptlen = 1;
    }

    while (p->name) {
	cxin->imtbl[ cxin->numHZim ].name = p->name;
	cxin->imtbl[ cxin->numHZim ].hzim.type = im_type_Builtin;
	cxin->imtbl[ cxin->numHZim ].hzim.hztbl = &builtin_hztbl;
	cxin->imtbl[ cxin->numHZim ].hzim.prompt = (Char *)(p->prompt);
	cxin->imtbl[ cxin->numHZim ].hzim.lenPrompt = strlen(p->prompt);
	cxin->imtbl[ cxin->numHZim ].hzif = p->filter;

	cxin->numHZim++;
	p++;
    }
}

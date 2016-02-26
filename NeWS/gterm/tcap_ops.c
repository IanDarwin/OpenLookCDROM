/*
 * This file is a product of Sun Microsystems, Inc. and is provided for
 * unrestricted use provided that this legend is included on all tape
 * media and as a part of the software program in whole or part.
 * Users may copy, modify or distribute this file at will.
 * 
 * THIS FILE IS PROVIDED AS IS WITH NO WARRANTIES OF ANY KIND INCLUDING THE
 * WARRANTIES OF DESIGN, MERCHANTIBILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE, OR ARISING FROM A COURSE OF DEALING, USAGE OR TRADE PRACTICE.
 * 
 * This file is provided with no support and without any obligation on the
 * part of Sun Microsystems, Inc. to assist in its use, correction,
 * modification or enhancement.
 * 
 * SUN MICROSYSTEMS, INC. SHALL HAVE NO LIABILITY WITH RESPECT TO THE
 * INFRINGEMENT OF COPYRIGHTS, TRADE SECRETS OR ANY PATENTS BY THIS FILE
 * OR ANY PART THEREOF.
 * 
 * In no event will Sun Microsystems, Inc. be liable for any lost revenue
 * or profits or other special, indirect and consequential damages, even
 * if Sun has been advised of the possibility of such damages.
 * 
 * Sun Microsystems, Inc.
 * 2550 Garcia Avenue
 * Mountain View, California  94043
 *
 * Modifications to the original Sun Microsystems, Inc. source code
 * made by the Grasshopper Group are in the Public Domain.
 *
 * Extensions to this file by Eric Messick of the Grasshopper Group.
 *
 * Grasshopper Group
 * 212 Clayton St
 * San Francisco, CA 94117
 *
 */

#ifndef lint
static	char sccsid[] = "@(#)tcap_ops.c 9.6 88/01/19 Copyright 1985 Sun Micro";
static	char RCSid[] =
	"@(#)$Header: /it/grass/gterm/RCS/tcap_ops.c,v 2.7 1991/04/23 06:52:40 hugh Grass2 $";
#endif

/*
 * Copyright (c) 1985 by Sun Microsystems, Inc.
 */

/*-
	tcap_ops.c

	tcap_ops.c, Mon Mar 24 11:52:31 1986

		David Rosenthal,
		Sun Microsystems
 */


#include	<stdio.h>
#include	<sys/types.h>
#ifdef REF
#include	<ref/config.h>
#endif
#include	"termcap.h"
#include	"tcap.h"

/*
 * Termcap operations module.
 *
 *	The external interface of this module is the array T[] and its
 *	number of elements Ts,  plus the initialization routine:
 *	tc_init_ops()
 */

#include	"screen.h"
extern	int CharsPerLine;
extern	int LinesPerScreen;
extern	struct pair Dot;
extern	struct tcap T[];
extern	int Ts;
extern	char *malloc();
#ifndef bcopy
extern	void bcopy();
#endif
#ifndef bzero
extern	void bzero();
#endif
extern	char *strncpy();
static	struct tcap *tc_lookup();

static	u_short TopLineOfScrollRegion, BottomLineOfScrollRegion;
static	struct pair SavedCursor;
static	u_short ScrollNLKludge = 0;
struct	tcap *CheckCR = 0;
struct	tcap *CheckNL = 0;
struct	tcap *CheckBS = 0;
struct	tcap *CheckTAB = 0;

static unsigned int PermanentModes = 0,  TemporaryModes = 0;

static	char FrameLabel[MaxCharsPerLine] ;
static	u_short FLindex=-1;
static	int (*prevInput)();

struct	tcap interruptedOp = { 0 };
struct	line *lastInputLine;
int	PageFull = 0;
static	int PrevPageMode = -1;
extern	int PageMode;
extern	int userLinesPerScreen;
extern	int userCharsPerLine;

static int al_op(), clear_body(), ce_op(), cm_in(), cs_op();
static int dc_op(), dl_op(), ic_op(), nl_op(), sf_op();

tc_init_ops()
{
    struct tcap *me, *tc;

    if (userLinesPerScreen > 0)
	LinesPerScreen = userLinesPerScreen;
    if (userCharsPerLine > 0)
	CharsPerLine = userCharsPerLine;
    Dot.x = 0 ;
    Dot.y = LinesPerScreen - 1 ;
    TopLineOfScrollRegion = 0;
    BottomLineOfScrollRegion = LinesPerScreen - 1;
    /*
     * Beware of termcap entries that define ue and/or se identical
     * to me (turn off ALL attributes).  Since we just pattern
     * match, it's likely we won't get me_op in normal operation,
     * so force the entries here. (known for vt100)
     */
    me = tc_lookup("me");
    if (me) {
	tc = tc_lookup("ue");
	if (tc && tc->t_size == me->t_size &&
	    bcmp(me->t_text, tc->t_text, (int)tc->t_size) == 0)
	    tc->t_op = me->t_op;
	tc = tc_lookup("se");
	if (tc && tc->t_size == me->t_size &&
	    bcmp(me->t_text, tc->t_text, (int)tc->t_size) == 0)
	    tc->t_op = me->t_op;
    }
    /*
     * Check for sf == \n, if so we must kludge things so that
     * sf_op will call nl_op as needed (exists on bitgraph).
     */
    tc = tc_lookup("sf");
    if (tc && tc->t_size == 1 && tc->t_text[0] == '\n')
	ScrollNLKludge = 1;
    /*
     * To minimize the number of times the regular text processing
     * is stopped to invoke the pattern matcher we check the most
     * strings CR, NL, BS, and TAB to see if they are \r, \n, \b,,
     * and \t, respectively.  If any are, a quick check on the character
     * is made before the pattern matching machinery is invoked.
     */
    tc = tc_lookup("cr");
    if (tc && tc->t_size == 1 && tc->t_text[0] == '\r')
       CheckCR = tc;
    tc = tc_lookup("nl");
    if (tc && tc->t_size == 1 && tc->t_text[0] == '\n')
       CheckNL = tc;
   tc = tc_lookup("bc");
    if (tc && tc->t_size == 1 && tc->t_text[0] == '\b')
       CheckBS = tc;
    tc = tc_lookup("ta");
    if (tc && tc->t_size == 1 && tc->t_text[0] == '\t')
       CheckTAB = tc;
}

tc_initmodemenu()
{

    /*
     * Insure menu items properly reflect initial settings.
     */
    SetPageMode(PageMode);
    SetAutoMargins(PermanentModes & AutoMarginMode);
}

static struct tcap *
tc_lookup(key)
    char *key;
{
    register struct tcap *tp;

    for (tp = T; *tp->t_key; tp++)
	if (strcmp(tp->t_key, key) == 0)
	    return (tp);
    return ((struct tcap *)0);
}

/*
 * scrollreset is called whenever keyboard input is seen, in order to
 * do deferred scrolling and remember the last line on which the user
 * typed something.
 */

scrollreset(l)
    int l;
{
    int n = PageFull;

    PageFull = 0;
    lastInputLine = (l == 1) ? screen[1] : 0;
    while (n-- > 0 && !PageFull)
	sf_op((struct tcap *) 0);
    if (lastInputLine == 0)
	lastInputLine = screen[Dot.y];
}

toggleautomargins()
{

    PermanentModes ^= (unsigned int) AutoMarginMode;
    SetAutoMargins(PermanentModes & AutoMarginMode);
    FlushPostScript();
}

togglepagemode()
{
    SetPageMode(PageMode = !PageMode);
    FlushPostScript();
}

resetscreensize(row, col)
int row, col;
{
	struct tcap t;
	int dotx, doty;

	dotx = Dot.x ;
	if (dotx >= col) dotx = col - 1;

	doty = Dot.y - LinesPerScreen + row ;
	if (doty >= row) doty = row - 1;
	if (doty < 0) doty = 0 ;

	if (PageMode) {
		/* no scroll stop line */
		lastInputLine = screen[doty - row + LinesPerScreen];
	}

	CharsPerLine = col ;
	LinesPerScreen = row ;

	t.t_x = LinesPerScreen - 1 ;
	t.t_y = 0 ;
	cs_op(&t);	/* change scrolling region to full screen */
			/* if it wasn't full screen before, then it's the
			 * program's responsibility to change it after
			 * getting the sigwinch 
			 * note that this trashes Dot, so we save it.
			 */
	Dot.x = dotx ;
	Dot.y = doty ;
}

#ifdef notdef
static
trace(s, t)
    register char *s;
    register struct tcap *t;
{
    register char *q = t->t_text;

    fprintf(stderr, "%s: ", s);
    fprintf(stderr, "[%s,", t->t_key);
    if (q == NULL)
	fprintf(stderr, "(nil)");
    else while (*q) {
	if (*q < ' ') {
	    fprintf(stderr, "^%c", *q + '@');
	} else {
	    fprintf(stderr, "%c", *q);
	}
	q++;
    }
    fprintf(stderr, ",%d,(%d,%d)]\n", t->t_index, t->t_x, t->t_y);
}
#endif

#ifndef	notdef
#define	trace(X, Y)	(void)(X, Y)
#endif
#define ChangeScreen()
#define	MoveCursor()

#ifdef notdef
static
PrintScreen()
{
    register int y;

    for (y = 0; y < LinesPerScreen; y++) {
	register struct line *l = screen[y];
	register int x;

	if (l == NULL)
	    fprintf(stderr, "(nil)");
	else for (x = 0; x < l->length; x++) {
	    if (x == Dot.x && y == Dot.y)
		fprintf(stderr, "+");
	    else if (x == l->changeposition)
		fprintf(stderr, "|");
	    else
	        fprintf(stderr, "%c", (l->body[x] == ' ' ? '.' : l->body[x]));
	}
	fprintf(stderr, "\n");
    }
    fprintf(stderr, "\n");
}
#endif

showc(t)
struct tcap *t;
{
	register char *cp = t->t_text;
	register struct line *l;
	register int dotx, c;
	register unsigned Modes = PermanentModes | TemporaryModes;
	int n = t->t_size;
	register char *bp;
	register u_char *pp;

	trace("==", t);
	TemporaryModes = 0;
	dotx = Dot.x;
	while (n > 0 && !PageFull) {
		l = screen[Dot.y];
		if (l->changeposition > dotx)
			l->changeposition = dotx;
		if (l->length < dotx)
			l->length = dotx;
		bp = &l->body[dotx] ;
		pp = &l->prop[dotx] ;
		if (Modes & InsertMode && l->length > dotx) {
			n-- ;
			c = *cp++;
			if (c < ' ' || c >= 0177)
				continue;
			if (l->length < CharsPerLine)
				l->length++;

			bcopy((char *)pp, (char *)pp+1,(int)(l->length-dotx-1));
			bcopy(        bp,         bp+1,(int)(l->length-dotx-1));

			l->end_of_changes = CharsPerLine + 1 ;
			*bp = c;
			*pp = (Modes & Attributes);
			if (++dotx >= CharsPerLine) {
				if (Modes & AutoMarginMode) {
					trace("cr", t);
					l->end_of_changes = CharsPerLine + 1 ;
					l->flags |= LINE_WRAPPED ;
					if (l->length < dotx)
						l->length = dotx ;
					dotx = 0;
					MoveCursor();
					nl_op(t);
					TemporaryModes |= WrapJustHappenedMode;
					}
				else	dotx = CharsPerLine - 1;
				}
			}
		else	{
			while (n-- > 0 && !PageFull) {
				c = *cp++;
				if (c < ' ' || c >= 0177)
					continue;
				*bp++ = c;
				*pp++ = (Modes & Attributes);
				if (++dotx >= CharsPerLine) {
					if (Modes & AutoMarginMode) {
						trace("cr", t);
						l->end_of_changes =
							CharsPerLine + 1 ;
						l->flags |= LINE_WRAPPED ;
						if (l->length < dotx)
							l->length = dotx ;
						dotx = 0;
						MoveCursor();
						nl_op(t);
						TemporaryModes |=
							WrapJustHappenedMode;
						break;
						}
					else	dotx = CharsPerLine - 1;
					}
				}
			}
		if (l->length < dotx)
			l->length = dotx ;
		if (l->end_of_changes < dotx)
			l->end_of_changes = dotx;
		}
	Dot.x = dotx;
	ChangeScreen();
	/*
	 * If we were interrupted by nl_op setting PageFull, set up
	 * interruptedOp with a tcap that can be used to finish this later.
	 */
	if (PageFull && ++n > 0) {
		interruptedOp = *t;
		interruptedOp.t_text += (t->t_size - n);
		interruptedOp.t_size = n;
		}
	else	interruptedOp.t_op = 0;
}

static int
AL_op(t)	/* add multiple blank lines */
    struct tcap *t;
{
    trace("AL", t);
    /* cheat for now */
    while (t->t_y-- > 0)
	al_op(t);
    ChangeScreen();
}

static int
DC_op(t)
    struct tcap *t;
{
    trace("DC", t);
    /* cheat for now */
    while (t->t_y-- > 0)
	dc_op(t);
    ChangeScreen();
}

static int
DL_op(t)	/* delete multiple lines */
    struct tcap *t;
{
    trace("DL", t);
    /* cheat for now */
    while (t->t_y-- > 0)
	dl_op(t);
    ChangeScreen();
}

static int
DO_in(t)	/* move cursor down n lines */
    struct tcap *t;
{
    trace("DO", t);
    return (cm_in(t));	/* Neat!  It might even work */
}

static int
DO_op(t)	/* move cursor down n lines */
    register struct tcap *t;
{
    trace("DO", t);
    if (t->t_y >= 0 && (Dot.y + t->t_y) < LinesPerScreen)
    	Dot.y += t->t_y;
    MoveCursor();
}

static int
IC_op(t)
    struct tcap *t;
{
    trace("IC", t);
    /* cheat for now */
    while (t->t_y-- > 0)
	ic_op(t);
    ChangeScreen();
}

static int
LE_in(t)	/* move cursor left n characters */
    struct tcap *t;
{
    trace("LE", t);
    return (cm_in(t));	/* Neat!  It might even work */
}

static int
LE_op(t)	/* move cursor left n characters */
    register struct tcap *t;
{
    trace("LE", t);
    if ((Dot.x - t->t_y) >= 0)
    	Dot.x -= t->t_y;
    MoveCursor();
}

static int
RI_in(t)	/* move cursor right n characters */
    struct tcap *t;
{
    trace("RI", t);
    return (cm_in(t));	/* Neat!  It might even work */
}

static int
RI_op(t)	/* move cursor right n characters */
    register struct tcap *t;
{
    trace("RI", t);
    if (t->t_y >= 0 && (Dot.x + t->t_y) <= CharsPerLine)
    	Dot.x += t->t_y;
    MoveCursor();
}

static int
UP_in(t)	/* move cursor up n lines */
    struct tcap *t;
{
    trace("UP", t);
    return (cm_in(t));	/* Neat!  It might even work */
}

static int
UP_op(t)	/* move cursor up n lines */
    register struct tcap *t;
{
    trace("UP", t);
    if ((Dot.y - t->t_y) >= 0)
    	Dot.y -= t->t_y;
    MoveCursor();
}

static int
al_op(t)	/* Add new blank line */
    struct tcap *t;
{
    register struct line **p, **current;
    register struct line *old;

    trace("al", t);
    current = &screen[Dot.y];
    p = &screen[BottomLineOfScrollRegion];
    old = *p;
    for (; p > current; p--)
	*p = *(p-1);
    *p = old;
    clear_body(old, 0);
    old->length = 0;
    old->changeposition = 0;
    old->end_of_changes = CharsPerLine;
    old->usedtobe = LinesPerScreen;
    ChangeScreen();
}

static int
am_in(t)	/* Has auto-margins */
    struct tcap *t;
{
    trace("am", t);
    if (t->t_x) PermanentModes |= AutoMarginMode;
    return (0);
}

static int
bc_op(t)	/* back-character */
    struct tcap *t;
{
    trace("bc", t);
    if (Dot.x > 0)
	Dot.x--;
    MoveCursor();
}

static int
bl_op(t)	/* Bell character */
    struct tcap *t;
{
    trace("bl", t);
    RingBell();
}

/*
 * Clearing the whole line body for the clear ops may be excessive
 * but nothing short of it seems to work for programs which do a lot
 * of cursor manipulation, such as rogue or hack.
 */
static int
clear_body(l, x)
    struct line *l;
    int x;
{
    register int len = l->buffer_length - x;
    register char *cp;

    bzero((char *)&l->prop[x], len);
    l->flags = 0 ;
    for (cp = &l->body[x]; len-- > 0;)
	 *cp++ = ' ';
}

static int
cd_op(t)	/* Clear to end of display */
    struct tcap *t;
{
    register struct line **p, **last;
    register struct line *l;

    trace("cd", t);
    ce_op(t);
    last = &screen[LinesPerScreen];
    for (p = &screen[Dot.y+1]; p < last; p++) {
	(l = *p)->length = 0;
	l->changeposition = 0;
	l->end_of_changes = CharsPerLine ;
	clear_body(l, 0);
    }
    ChangeScreen();
}

static int
ce_op(t)	/* Clear to end of line */
    struct tcap *t;
{
    register struct line *l = screen[Dot.y];

    trace("ce", t);
    l->length = Dot.x;
    if (l->changeposition > Dot.x)
	l->changeposition = Dot.x;
    l->end_of_changes = CharsPerLine ;
    clear_body(l, (int)l->length);
    ChangeScreen();
}

static int
cl_op(t)	/* Clear screen */
    struct tcap *t;
{
    register struct line **p, **last;
    register struct line *l;
    extern int RefreshSuppressed;

    trace("cl", t);
    Dot.x = Dot.y = 0;
    last = &screen[LinesPerScreen];
    for (p = &screen[0]; p < last; p++) {
	(l = *p)->length = 0;
	if (RefreshSuppressed) {
		l->changeposition = 0;
		l->end_of_changes = CharsPerLine ;
        } else {
		l->changeposition = MaxCharsPerLine + 1;
		l->end_of_changes = 0;
        }
	clear_body(l, 0);
    }
    if (!RefreshSuppressed) {
	    ClearScreen();
	    FlushPostScript();
    }
    ChangeScreen();
    if (PageMode)
	lastInputLine = screen[Dot.y];	/* no scroll stop line */
}

#ifdef	HAVE_TERMCAP
static int
cm_in(t)	/* Cursor motion */
    register struct tcap *t;
{
    register u_char c;
    char        buf[128];
    register	char *cp = t->t_text, *bp = buf;

    trace("cm", t);

    if (cp == NULL)
	return(0);
    /* Pre-process out parts of the % escapes */
    while ((c = *cp++) != '\0') {
	if (c == '%') {
	    register u_char c2 = *cp++;

	    switch (c2) {
	    case '+':		/* Subtract next then %. */
		switch (t->t_2nd + t->t_pc_r) {
		case 0:
		case 2:
		    t->t_yi = *cp++;
		    break;
		case 1:
		    t->t_xi = *cp++;
		    break;
		}
		c2 = '.';
		goto percent_dot;
	    case '>':
		switch (t->t_2nd + t->t_pc_r) {
		case 1:
		    t->t_xilim = *cp++;
		    t->t_xi = *cp++;
		    t->t_xilim += t->t_xi + 1;
		    break;
		case 0:
		case 2:
		    t->t_yilim = *cp++;
		    t->t_yi = *cp++;
		    t->t_yilim += t->t_yi + 1;
		    break;
		}
		t->t_2nd = !t->t_2nd;
		break;
	    case 'r':
		t->t_pc_r = 1;
		break;
	    case 'i':
		t->t_xi++;
		t->t_yi++;
		break;
	    case 'n':
		t->t_pc_n = 1;
		break;
	    case 'B':
		t->t_pc_B = 1;
		break;
	    case 'D':
		t->t_pc_D = 1;
		break;
		/* The following ones will be interpreted on the fly */
	    case 'd':		/* series of decimal digits */
	    case '2':		/* two decimal digits */
	    case '3':		/* three decimal digits */
	    case '.':		/* binary character */
	percent_dot:
	    case '%':
		*bp++ = c;
		*bp++ = c2;
		t->t_2nd = !t->t_2nd;
		break;
	    default:		/* Bad % escape */
		return (1);
	    }
	}
	else {
	    *bp++ = c;
	}
    }
    *bp++ = '\0';
    if (bp-buf > 1) {
	if ((t->t_text = malloc((unsigned)(bp-buf+1))) == NULL)
	    return (2);
	strncpy(t->t_text, buf, bp-buf);
	t->t_size = bp-buf - 1;
    } else {
	t->t_text = NULL;
    }
    t->t_2nd = 0;
    return (0);
}

#else	/* !HAVE_TERMCAP */

static int
cm_in(t)	/* Cursor motion */
    register struct tcap *t;
{
    register u_char c;
    char        buf[128];
    register	char *cp = t->t_text, *bp = buf;

    trace("cm", t);

    if (cp == NULL)
	return(0);
    /* Pre-process out parts of the % escapes */
    while ((c = *cp++) != '\0') {
	static int	seen1 = 0;

	if (c == '%') {
	    register u_char c2 = *cp++;

	    switch (c2) {
	    case 'p':
		switch (*cp++) {
		case '1':
			seen1++;
			t->t_2nd = 0;
			break;
		case '2':
			if (!seen1)
				t->t_pc_r = 1;
			t->t_2nd = 1;
			break;
		/*
		 * This capability is not available in termcap so we will
		 * assume that it is not present in terminfo either. This
		 * means that terminal that send more than two pieces of
		 * variable data with the "cm" directive will not work.
		 * Currently for "cm" only two variables are defined.
		 */
		case '3':
		case '4':
		case '5':
		case '6':
		case '7':
		case '8':
		case '9':
		   return(1);
		}
		break;

	    case '\'':		/* store constant for future operator */
		switch (t->t_2nd + t->t_pc_r) {
		case 0:
		case 2:
		    t->t_yi = *cp++;
		    break;
		case 1:
		    t->t_xi = *cp++;
		    break;
		}
		if (*cp == '\'')
			cp++;		/* blow past the trailing '\'' */
		else
			return(1);
		break;
	    case '{':		/* store decimal constant for future operator */
		switch (t->t_2nd + t->t_pc_r) {
		case 0:
		case 2:
		    t->t_yi = ((*cp++) * 10) + (*cp++);
		    break;
		case 1:
		    t->t_xi = ((*cp++) * 10) + (*cp++);
		    break;
		}
		if (*cp == '}')
			cp++;		/* blow past trailing '}' */
		else
			return(1);
		break;
	    /* the following will  not work do to the simplifications here */
	    case '?':		/* if */
	    case 't':		/* then */
	    case 'e':		/* else */
		return(1);	/* lets not delude ourselves, the data
				 *  structures were set up to deal with termcap
				 *  not the complexities of terminfo.
				 */
	    case 'i':
		t->t_xi++;
		t->t_yi++;
		break;
	    case '+':
		if (*cp++ != '%')
			return(1);
		if (*cp++ != 'c')
			return(1);
		*bp++ = '%';		/* make it look like termcap */
		*bp++ = '.';
		break;
		/* use the simpified model of termcap so some of this
		 * stuff is not valid
		 */
	    case '\b':		/* flags in format string */
	    case ':':		/* needed for '-' and '+' flags */
	    case '#':		/* flags in format string */
	    case '.':		/* if only fract part of precision */
		return(1);
		/* just leave a 'd' */
	    case '0':		/* numeric part of precision */
	    case '1':		/* numeric part of precision */
	    case '4':		/* numeric part of precision */
	    case '5':		/* numeric part of precision */
	    case '6':		/* numeric part of precision */
	    case '7':		/* numeric part of precision */
	    case '8':		/* numeric part of precision */
	    case '9':		/* numeric part of precision */
		/* skip past the end of this definition, leave %d in its place*/
		while (*cp != 'd' && *cp != 'o' && *cp != 'x'
				&& *cp != 'X' && *cp != 's')
			cp++;
		/* don't support anything but 'd' */
		if (*cp != 'd')
			return(1);
		*bp++ = c;		/* '%' */
		*bp++ = *cp++;		/* 'd' */
		/* The following ones will be interpreted on the fly. */
	    case '2':		/* numeric part of precision */
	    case '3':		/* numeric part of precision */
		/* remove %[23]->[doxXs], put %[23] in bp */
		*bp++ = c;
		*bp++ = c2;
		/* skip any factional part of the precision */
		while (*cp != 'd' && *cp != 'o' && *cp != 'x'
				&& *cp != 'X' && *cp != 's')
			cp++;
		/* don't support anything but 'd' */
		if (*cp != 'd')
			return(1);
		*cp++;		/* get rid of the 'd' */
		break;
		/* not suported by termcap, so not supported here */
	    case 'o':		/* octal: no other formating is present */
	    case 'x':		/* hex: no other formating is present */
	    case 'X':		/* HEX: no other formating is present */
	    case 's':		/* String: no other formating is present */
	    case 'c':		/* binary character */
	    case 'l':		/* strlen() operator */
	    case '=':		/* equal operator */ 
	    case '>':		/* greater than operator */
	    case '<':		/* less than operator */
	    case '-':		/* subtract operator */
	    case '*':		/* multiply operator */
	    case '/':		/* divide operator */
	    case 'm':		/* modula operator */
	    case '&':		/* bitwise and operator */
	    case '|':		/* bitwise or operator */
	    case '^':		/* bitwise xor operator */
	    case '~':		/* bitwise not operator */
	    case 'A':		/* logical and operator */
	    case 'O':		/* logical or operator */
	    case '!':		/* logical not operator */
		return(1);
	    /* include the string "%A" where A = one of the following cases */
	    case 'd':		/* decimal: no other formating is present */
	    case '%':
		*bp++ = c;
		*bp++ = c2;
		break;
	    default:		/* Bad % escape */
		return (1);
	    }
	}
	else {
	    *bp++ = c;
	}
    }
    *bp++ = '\0';
    if (bp-buf > 1) {
	if ((t->t_text = malloc(bp-buf+1)) == NULL)
	    return (2);
	strncpy(t->t_text, buf, bp-buf);
	t->t_size = bp-buf - 1;
    } else {
	t->t_text = NULL;
    }
    t->t_2nd = 0;
    return (0);
}
#endif	/* !HAVE_TERMCAP */

static int
cm_op(t)	/* Cursor motion */
    register struct tcap *t;
{

    trace("cm", t);
    lastInputLine = 0;			/* no scroll stop line */
    if (t->t_x >= 0 && t->t_x < CharsPerLine)
	Dot.x = t->t_x;
    if (t->t_y >= 0 && t->t_y < LinesPerScreen)
	Dot.y = t->t_y;
    MoveCursor();
}

static int
co_in(t)	/* number of columns */
    struct tcap *t;
{
    trace("co", t);
    if (userCharsPerLine < 1)
	CharsPerLine = t->t_x;
    return (0);
}

static int
cr_op(t)	/* carriage return */
    struct tcap *t;
{
    trace("cr", t);
    Dot.x = 0;
    MoveCursor();
}

static int
cs_in(t)	/* change scroll region */
    struct tcap *t;
{
    trace("cs", t);
    return (cm_in(t));	/* Neat!  It might even work */
}

static int
cs_op(t)	/* change scroll region */
    register struct tcap *t;
{
    trace("cs", t);
    if (t->t_y >= t->t_x || t->t_x >= LinesPerScreen)
	return;
    Dot.x = 0;
    Dot.y = t->t_y;
    TopLineOfScrollRegion = t->t_y;
    BottomLineOfScrollRegion = t->t_x;
    lastInputLine = 0;			/* no scroll stop line */
    MoveCursor();
}

static int
dc_op(t)	/* delete character */
    struct tcap *t;
{
    register struct line *l = screen[Dot.y];
    register int dotx = Dot.x;
    int len;

    trace("dc", t);
    if (l->changeposition > dotx)
	l->changeposition = dotx;
    if (dotx < l->length) {
	l->length--;
	len = l->length - dotx;
	bcopy((char *)&l->prop[dotx+1], (char *)&l->prop[dotx], len);
	l->prop[l->length] = 0 ;
	bcopy(&l->body[dotx+1], &l->body[dotx], len);
	l->body[l->length] = ' ' ;
    }
    l->end_of_changes = CharsPerLine ;
    ChangeScreen();
}

static int
dl_op(t)	/* delete line */
    struct tcap *t;
{
    register struct line **p, **bottom;
    register struct line *old = screen[Dot.y];

    trace("dl", t);
    lastInputLine = 0;			/* no scroll stop line */
    bottom = &screen[BottomLineOfScrollRegion];
    for (p = &screen[Dot.y]; p < bottom; p++)
	*p = *(p+1);
    *p = old;
    clear_body(old, 0);
    old->length = 0;
    old->changeposition = 0;
    old->end_of_changes = CharsPerLine ;
    old->usedtobe = LinesPerScreen ;
    ChangeScreen();
}

static int
do_op(t)	/* down one line */
    struct tcap *t;
{
    trace("do", t);
    if (PermanentModes & IgnoreNewlineAfterWrapMode) {
	if (TemporaryModes & WrapJustHappenedMode) {
	    TemporaryModes &= ~WrapJustHappenedMode;
	    return;
	}
    }
    if (Dot.y < LinesPerScreen - 1)
	Dot.y++;
    else {
	sf_op(t);
    }
    MoveCursor();
}

static int
ei_op(t)	/* end insert mode */
    struct tcap *t;
{
    trace("ei", t);
    PermanentModes &= ~InsertMode;
}

static int
el_op(t)	/* end frame label definition mode */
    struct tcap *t;
{
    if (FLindex > 0) {
	FrameLabel[FLindex] = '\0';
	SetFrameLabel(FrameLabel);
	FLindex = -1 ;
	t = T+Ts;
	t->t_op = prevInput;
    }
}

static int
ke_op(t)	/* leave keyboard transmit mode */
    struct tcap *t;
{
    trace("ke", t);
}

static int
ks_op(t)	/* enter keyboard transmit mode */
    struct tcap *t;
{
    trace("ks", t);
}

static int
ho_op(t)	/* home cursor */
    struct tcap *t;
{

    trace("ho", t);
    lastInputLine = 0;			/* no scroll stop line */
    Dot.x = Dot.y = 0;
    MoveCursor();
}

static int
ic_op(t)	/* insert character */
    struct tcap *t;
{
    trace("ic", t);
    TemporaryModes |= InsertMode;
}

static int
im_op(t)	/* enter insert mode */
    struct tcap *t;
{
    trace("im", t);
    PermanentModes |= InsertMode;
}

static int
le_op(t)	/* cursor left */
    struct tcap *t;
{
    trace("le", t);
    if (Dot.x > 0)
	Dot.x--;
    MoveCursor();
}

static int
li_in(t)	/* number of lines */
    struct tcap *t;
{
    trace("li", t);
    if (userLinesPerScreen < 1)
	LinesPerScreen = t->t_x;
    return (0);
}

static int
ll_op(t)	/* last line first column */
    struct tcap *t;
{
    trace("ll", t);
    Dot.x = 0;
    Dot.y = LinesPerScreen - 1;
    MoveCursor();
}

static int
lm_op(t)	/* label mode input */
    register struct tcap *t;
{
    trace("lm", t);
    if (FLindex < 0) return;
    if (FLindex + t->t_size >= MaxCharsPerLine)
	t->t_size = MaxCharsPerLine - FLindex;
    if (t->t_size > 0) {
	strncpy(&FrameLabel[FLindex], t->t_text, (int)t->t_size);
	FLindex += t->t_size;
    }
}

static int
mb_op(t)	/* enable blink */
    struct tcap *t;
{
    trace("mb", t);
    PermanentModes |= BlinkMode;
}

static int
md_op(t)	/* enter bold mode */
    struct tcap *t;
{
    trace("md", t);
    PermanentModes |= BoldMode;
}

static int
me_op(t)	/* turn off attributes */
    struct tcap *t;
{
    trace("me", t);
    PermanentModes &= ~(Attributes);
}

static int
mr_op(t)	/* enter reverse video */
    struct tcap *t;
{
    trace("mr", t);
    PermanentModes |= ReverseVideoMode;
}

static int
nd_op(t)	/* cursor right */
    struct tcap *t;
{
    trace("nd", t);
    if (Dot.x < (CharsPerLine - 1))
	Dot.x++;
    MoveCursor();
}

static int
nl_op(t)	/* newline */
    struct tcap *t;
{
    trace("nl", t);
    if (PermanentModes & IgnoreNewlineAfterWrapMode) {
	if (TemporaryModes & WrapJustHappenedMode) {
	    TemporaryModes &= ~WrapJustHappenedMode;
	    return;
	}
    }
    if (Dot.y < LinesPerScreen - 1)
	Dot.y++;
    else {
	sf_op(t);
    }
    MoveCursor();
}

static int
rc_op(t)	/* restore cursor */
    struct tcap *t;
{
    trace("rc", t);
    Dot = SavedCursor;
    MoveCursor();
}

static int
sc_op(t)	/* save cursor */
    struct tcap *t;
{
    trace("sc", t);
    SavedCursor = Dot;
    MoveCursor();
}

static int
se_op(t)	/* leave stand-out */
    struct tcap *t;
{
    trace("se", t);
    PermanentModes &= ~StandOutMode;
}

static int
so_op(t)	/* enter stand-out */
    struct tcap *t;
{
    trace("so", t);
    PermanentModes |= StandOutMode;
}

static int
sf_op(t)	/* scroll forwards */
    struct tcap *t;
{
    register struct line **p, **bottom;
    register struct line *old;
    static int SFrecur = 0;

    trace("sf", t);
    /*
     * When sf is "\n", nl_op won't be called, so we emulate
     * it's actions here (beware of recursion).
     */
    if (ScrollNLKludge && !SFrecur) {
	SFrecur++; nl_op(t); SFrecur--;
	return (1);			/* XXX */
    }
    p = &screen[TopLineOfScrollRegion];
    if (PageMode && *p == lastInputLine) {
	PageFull++;			/* can't scroll this line */
	return (0);
    }
    ScrollSaveLine(*p);
    bottom = &screen[BottomLineOfScrollRegion];
    p = &screen[TopLineOfScrollRegion];
    old = *p;
    for (; p < bottom; p++)
	*p = *(p+1);
    *p = old;
    clear_body(old, 0);
    old->length = 0;
    old->changeposition = 0;
    old->end_of_changes = CharsPerLine ;
    old->usedtobe = LinesPerScreen;
    ChangeScreen();
    return (1);
}

static int
sl_op(t)	/* start defining new frame label */
    register struct tcap *t;
{
    trace("sl", t);
    FLindex = 0;
    t = T+Ts;
    prevInput = t->t_op;
    t->t_op = lm_op;
}

static int
sr_op(t)	/* scroll reverse */
    struct tcap *t;
{
    register struct line **p, **top;
    register struct line *old;

    trace("sr", t);
    lastInputLine = 0;			/* no scroll stop line */
    top = &screen[TopLineOfScrollRegion];
    p = &screen[BottomLineOfScrollRegion];
    old = *p;
    for (; p > top; p--)
	*p = *(p-1);
    *p = old;
    clear_body(old, 0);
    old->length = 0;
    old->changeposition = 0;
    old->end_of_changes = CharsPerLine ;
    old->usedtobe = LinesPerScreen;
    ChangeScreen();
}

static int
ta_op(t)	/* tab */
    struct tcap *t;
{
    trace("ta", t);
    Dot.x = (Dot.x & ~07) + 010;
    if (Dot.x >= CharsPerLine - 1)
	Dot.x = CharsPerLine - 1;
    MoveCursor();
}

static int
te_op(t)	/* end use of termcap */
    struct tcap *t;
{
    trace("te", t);
    if (PrevPageMode != -1) {
	PageMode = PrevPageMode;
	PrevPageMode = -1;
    }
}

static int
ti_op(t)	/* begin use of termcap */
{
    trace("ti", t);
    /*
     * Turn off page mode when using termcap.
     */
    if (PrevPageMode == -1) {
	PrevPageMode = PageMode;
	PageMode = 0;
    }
}

static int
ue_op(t)	/* end underline */
    struct tcap *t;
{
    trace("ue", t);
    PermanentModes &= ~UnderlineMode;
}

static int
up_op(t)	/* cursor up */
    struct tcap *t;
{
    trace("up", t);
    if (Dot.y > 0)
	Dot.y--;
    else	/* vi will never do this,  but some ll caps may */
        Dot.y = LinesPerScreen - 1;
    MoveCursor();
}

static int
us_op(t)	/* start underline */
    struct tcap *t;
{
    trace("us", t);
    PermanentModes |= UnderlineMode;
}

static int
vb_op(t)	/* visibile bell */
    struct tcap *t;
{
    trace("bl", t);
    VisibleBell();
}

static int
xn_in(t)
    struct tcap *t;
{
    trace("xn", t);
    if (t->t_x) PermanentModes |= IgnoreNewlineAfterWrapMode;
    return (0);
}

/* These capabilities are numeric or boolean - they dont have ops */
#define am_op	NULL
#define	bs_op	NULL
#define	co_op	NULL
#define	li_op	NULL
#define pc_op	NULL
#define xn_op	NULL

/* These capabilities are strings that dont need initialization */
#define	al_in	NULL
#define	bc_in	NULL
#define	bl_in	NULL
#define	cd_in	NULL
#define	ce_in	NULL
#define	cl_in	NULL
#define	cr_in	NULL
#define	dc_in	NULL
#define	dl_in	NULL
#define	do_in	NULL
#define	ei_in	NULL
#define	ke_in	NULL
#define	ks_in	NULL
#define	ho_in	NULL
#define	ic_in	NULL
#define	im_in	NULL
#define	le_in	NULL
#define	ll_in	NULL
#define	mb_in	NULL
#define	md_in	NULL
#define	me_in	NULL
#define	mr_in	NULL
#define	nd_in	NULL
#define	nl_in	NULL
#define pc_in	NULL
#define rc_in	NULL
#define	sc_in	NULL
#define	se_in	NULL
#define	sf_in	NULL
#define	so_in	NULL
#define	sr_in	NULL
#define	ta_in	NULL
#define	ue_in	NULL
#define	up_in	NULL
#define	us_in	NULL

#define	s	string
#define n	num
#define b	bool

struct tcap T[] = {
/*
 *   key   ty op     in     text  deftx sz in tmp  tf
 *	x  xi xl y  yi yl r  n  B  D  2
 */
    {"AL", s, AL_op, cs_in, NULL,   NULL, 0, 0, NULL, 0},/* 0 */
    {"DL", s, DL_op, cs_in, NULL,   NULL, 0, 0, NULL, 0},
    {"DC", s, DC_op, cs_in, NULL,   NULL, 0, 0, NULL, 0},
    {"DO", s, DO_op, DO_in, NULL,   NULL, 0, 0, NULL, 0},
    {"IC", s, IC_op, cs_in, NULL,   NULL, 0, 0, NULL, 0},
    {"LE", s, LE_op, LE_in, NULL,   NULL, 0, 0, NULL, 0},
    {"RI", s, RI_op, RI_in, NULL,   NULL, 0, 0, NULL, 0},
    {"UP", s, UP_op, UP_in, NULL,   NULL, 0, 0, NULL, 0},
    {"al", s, al_op, al_in, NULL,   NULL, 0, 0, NULL, 0},
    {"am", b, am_op, am_in, NULL,   NULL, 0, 0, NULL, 0},
    {"bc", s, bc_op, bc_in, NULL,  "\10", 0, 0, NULL, 0},/* 10 */
    {"bl", s, bl_op, bl_in, NULL,   "\7", 0, 0, NULL, 0},
    {"cd", s, cd_op, cd_in, NULL,   NULL, 0, 0, NULL, 0},
    {"ce", s, ce_op, ce_in, NULL,   NULL, 0, 0, NULL, 0},
    {"cl", s, cl_op, cl_in, NULL,   NULL, 0, 0, NULL, 0},
    {"cm", s, cm_op, cm_in, NULL,   NULL, 0, 0, NULL, 0},
    {"co", n, co_op, co_in, NULL,   NULL, 0, 0, NULL, 0},
    {"cr", s, cr_op, cr_in, NULL, "\015", 0, 0, NULL, 0},
    {"cs", s, cs_op, cs_in, NULL,   NULL, 0, 0, NULL, 0},
    {"dc", s, dc_op, dc_in, NULL,   NULL, 0, 0, NULL, 0},
    {"dl", s, dl_op, dl_in, NULL,   NULL, 0, 0, NULL, 0},/* 20 */
    {"do", s, do_op, do_in, NULL,   NULL, 0, 0, NULL, 0},
    {"ei", s, ei_op, ei_in, NULL,   NULL, 0, 0, NULL, 0},
    {"el", s, el_op, NULL,  NULL,"\033\\",0, 0, NULL, 0}, /* end label */
    {"ke", s, ke_op, ke_in, NULL,   NULL, 0, 0, NULL, 0},
    {"ks", s, ks_op, ks_in, NULL,   NULL, 0, 0, NULL, 0},
    {"ho", s, ho_op, ho_in, NULL,   NULL, 0, 0, NULL, 0},
    {"ic", s, ic_op, ic_in, NULL,   NULL, 0, 0, NULL, 0},
    {"im", s, im_op, im_in, NULL,   NULL, 0, 0, NULL, 0},
    {"le", s, le_op, le_in, NULL,   NULL, 0, 0, NULL, 0},
    {"li", n, li_op, li_in, NULL,   NULL, 0, 0, NULL, 0},/* 30 */
    {"ll", s, ll_op, ll_in, NULL,   NULL, 0, 0, NULL, 0},
    {"lm", s, lm_op,  NULL, NULL,   NULL, 0, 0, NULL, 0}, /* label mode input */
    {"mb", s, mb_op, mb_in, NULL,   NULL, 0, 0, NULL, 0},
    {"md", s, md_op, md_in, NULL,   NULL, 0, 0, NULL, 0},
    {"me", s, me_op, me_in, NULL,   NULL, 0, 0, NULL, 0},
    {"mr", s, mr_op, mr_in, NULL,   NULL, 0, 0, NULL, 0},
    {"nd", s, nd_op, nd_in, NULL,   NULL, 0, 0, NULL, 0},
    {"nl", s, nl_op, nl_in, NULL, "\012", 0, 0, NULL, 0},
    {"pc", s, pc_op, pc_in, NULL, "\200", 0, 0, NULL, 0},
    {"rc", s, rc_op, rc_in, NULL,   NULL, 0, 0, NULL, 0},/* 40 */
    {"sc", s, sc_op, sc_in, NULL,   NULL, 0, 0, NULL, 0},
    {"se", s, se_op, se_in, NULL,   NULL, 0, 0, NULL, 0},
    {"sf", s, sf_op, sf_in, NULL,   NULL, 0, 0, NULL, 0},
    {"sl", s, sl_op, NULL,  NULL,"\033]l",0, 0, NULL, 0}, /* start label */
    {"so", s, so_op, so_in, NULL,   NULL, 0, 0, NULL, 0},
    {"sr", s, sr_op, sr_in, NULL,   NULL, 0, 0, NULL, 0},
    {"ta", s, ta_op, ta_in, NULL, "\011", 0, 0, NULL, 0},	/* XXX */
    {"te", s, te_op, NULL,  NULL,   NULL, 0, 0, NULL, 0},
    {"ti", s, ti_op, NULL,  NULL,   NULL, 0, 0, NULL, 0},
    {"ue", s, ue_op, ue_in, NULL,   NULL, 0, 0, NULL, 0},/* 50 */
    {"up", s, up_op, up_in, NULL,   NULL, 0, 0, NULL, 0},
    {"us", s, us_op, us_in, NULL,   NULL, 0, 0, NULL, 0},
    {"vb", s, vb_op,  NULL, NULL,   NULL, 0, 0, NULL, 0},
    {"xn", b, xn_op, xn_in, NULL,   NULL, 0, 0, NULL, 0},
    {"",   s, showc,  NULL, NULL,   NULL, 0, 0, NULL, 0},
};

/* T[Ts] is a special case - its the display-character operation */
int Ts = (sizeof T)/(sizeof T[0]) - 1;

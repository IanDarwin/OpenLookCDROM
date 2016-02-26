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
static char sccsid[] = "@(#)display.c 9.5 88/01/19 Copyright 1985 Sun Micro";
static char RCSid[] =
	"@(#)$Header: /it/grass/gterm/RCS/display.c,v 2.12 1991/04/23 06:52:58 hugh Grass2 $";
#endif

/*
 * Copyright (c) 1985 by Sun Microsystems, Inc. 
 */

/*-
	display.c

	display.c, Wed Mar 26 15:56:31 1986

		David Rosenthal,
		Sun Microsystems
 */

#ifdef	notdef
#define Diagnose
#endif
/*
 * Screen display module 
 *
 * The external interface of this module is the routine tc_refresh(), and the
 * initialization routine tc_init_screen(). 
 */

#include	<sys/types.h>
#ifdef REF
#include	<ref/config.h>
#endif
#include	<sys/ioctl.h>
#include	<sys/signal.h>
#include	<sys/file.h>
#include	"screen.h"
#include	"tcap.h"

#ifdef HAVE_TTYCOM_H
#include	<sys/ttycom.h>	/* what systems have this file? */
#endif

/*#define DEBUG*/
#ifdef DEBUG
#define err0(A) fprintf(stderr,A)
#define err1(A,B) fprintf(stderr,A,B)
#define err2(A,B,C) fprintf(stderr,A,B,C)
#define err3(A,B,C,D) fprintf(stderr,A,B,C,D)
#else
#define err0(A)
#define err1(A,B)
#define err2(A,B,C)
#define err3(A,B,C,D)
#endif

extern void free();
#ifndef bzero
extern void bzero();
#endif
#ifndef bcopy
extern void bcopy();
#endif
extern void perror();

#define CHARS_PER_LINE		80
#define LINES_PER_SCREEN	24

#define TypicalWidth		80

#define min(x, y) (((x)<(y))?(x):(y))

int     CharsPerLine = CHARS_PER_LINE;
int     LinesPerScreen = LINES_PER_SCREEN;
struct pair Dot = {0, 0};

int fontisfixedwidth = 1 ;

int ConnectionEstablished = 0 ;

static Repairing;
static struct pair ScreenDot = {0, 0};
static char *CursorChar = " ";
static u_char CursorProp = 0 ;
extern int PageFull;

int RefreshSuppressed = 1;

int TotalScrollLength = 0 ;
int ScrollLength = 0 ;
int ScrollY = 0 ;
int ScrollLinesSaved = 0 ;
int userScrollLength = 0 ;
int FastPaint = 1 ;
int DoScrolling = 1 ;

struct line **scroll_area;

char *
Malloc(size)
int size;
{
	extern char *malloc();
	char *ret;

	if ((ret=malloc((unsigned)size))==NULL)
		Fatal("Out of Memory", (char *)NULL);
	return ret;
}

static
RemoveCursor()
{
	if ((ScreenDot.x >= 0) && (! (CursorProp & ReverseVideoLook)))
		CursorDown(ScreenDot.x, ScreenDot.y, CursorChar, 1);
	ScreenDot.x = -1;
}

static
PlaceCursor(x, y)
{
    register struct line *l;

    if (x >= CharsPerLine) x = CharsPerLine - 1;
    if (x < 0) x = 0;

    if (y >= LinesPerScreen) y = LinesPerScreen - 1;
    if (y < 0) y = 0;

    ScreenDot.x = x ;
    ScreenDot.y = y + ScrollLength - LinesPerScreen - ScrollY ;

    l = screen[y];
    CursorChar = l->body + x ;
    CursorProp = l->prop[x] ;

    if (ScreenDot.y >= LinesPerScreen) {
	ScreenDot.x = -1 ;
	return;
	}

    if (!PageFull)			/* hide cursor if output blocked */
	CursorUp(ScreenDot.x, ScreenDot.y, CursorChar, 1);
}

set_scroll_save(length)
int length;
{
	int i, j, delta;
	struct line *line, **new_lines;
	char *body;
	u_char *prop;

	tc_deselect(0);		/* do this before we move the lines around */
	userScrollLength = length ;
	if (!RefreshSuppressed)
		ToggleScrollBar(length);
	if (length < LinesPerScreen) length = LinesPerScreen ;
	if (length > TotalScrollLength) {
		new_lines = (struct line **) Malloc(2 * length *
						sizeof(struct line *)) ;
		for (i=length-ScrollLength, j=0; j<ScrollLength; j++, i++) {
			new_lines[i] = lines[j] ;
			}
		TotalScrollLength = length ;
		if (scroll_area) free((char *)scroll_area);
		scroll_area = lines = new_lines ;
		}
	else	{
		delta = ScrollLength - length ;
		if (delta>0) {
			for (i=0; i<length; i++) {
				if (lines[i]) {
					if (lines[i]->body)
						free(lines[i]->body);
					if (lines[i]->prop)
						free((char *)lines[i]->prop);
					free((char *)lines[i]);
					}
				lines[i] = lines[i+delta] ;
				lines[i+delta] = NULL ;
				}
			}
		else	for (i=ScrollLength-1; i>=0; i--)
				lines[i-delta] = lines[i] ;
		}
	for (i=0; i<length-ScrollLength; i++) {
		line = (struct line *) Malloc(sizeof(struct line));
		line->body = Malloc(TypicalWidth) ;
		for (j=0; j<TypicalWidth; j++)
			line->body[j] = ' ' ;
		line->prop = (u_char *)Malloc(TypicalWidth) ;
		bzero((char *)line->prop, TypicalWidth);
		line->buffer_length = TypicalWidth ;
		line->length = 0 ;
		line->changeposition = 0 ;
		line->end_of_changes = 0 ;
		line->usedtobe = -1 ;
		line->flags = 0 ;
		lines[i] = line ;
		}
	ScrollY += length - ScrollLength ;
	ScrollLength = length ;
	if (ScrollY < 0) ScrollY = 0 ;
	if (ScrollY > ScrollLength - LinesPerScreen)
		ScrollY = ScrollLength - LinesPerScreen ;
	for (i=ScrollLength-LinesPerScreen; i < ScrollLength; i++) {
		if (lines[i]->buffer_length <= CharsPerLine) {
			body = Malloc(CharsPerLine+1);
			prop = (u_char *) Malloc(CharsPerLine+1);
			length = lines[i]->length ;
			if (lines[i]->buffer_length > 0) {
				bcopy(lines[i]->body, body, length);
				bcopy((char *)lines[i]->prop, (char *)prop, length);
				free(lines[i]->body);
				free((char *)lines[i]->prop);
				}
			bzero((char *)&prop[length], CharsPerLine - length + 1);
			for (; length <= CharsPerLine; length++)
				body[length] = ' ' ;
			lines[i]->body = body ;
			lines[i]->prop = prop ;
			lines[i]->buffer_length = CharsPerLine + 1 ;
			}
		}
/*	if (ScrollLinesSaved > ScrollLength) */
		ScrollLinesSaved = ScrollLength ;
	if (ScrollLinesSaved < LinesPerScreen)
		ScrollLinesSaved = LinesPerScreen ;
	screen = &lines[ScrollLength - LinesPerScreen] ;
}

scroll_to_bottom()
{
	scroll_to(ScrollLinesSaved - LinesPerScreen);
}

scroll_to(scrolly)
int scrolly;
{
	int i, lesser, greater;

	scrolly += ScrollLength - ScrollLinesSaved ;
	if (scrolly < 0) scrolly = 0 ;
	if (scrolly > ScrollLength - LinesPerScreen)
		scrolly = ScrollLength - LinesPerScreen ;
	if (scrolly != ScrollY) {
		if (scrolly < ScrollY) {
			lesser = scrolly;
			greater = ScrollY;
			}
		else	{
			lesser = ScrollY;
			greater = scrolly;
			}
		for (i=lesser; i<greater; i++)
			if (lines[i])
				lines[i]->usedtobe = -1 ;
		for (i=lesser+LinesPerScreen; i<greater+LinesPerScreen; i++)
			if (lines[i])
				lines[i]->usedtobe = -1 ;
		}
	ScrollY = scrolly ;
}

ScrollSaveLine(line)
struct line *line;
{
	int i;
	struct line *new;

	if (ScrollLength <= LinesPerScreen)
		return;
	if (ScrollLinesSaved < ScrollLength)
		ScrollLinesSaved++;
	for (i=ScrollLength; i>ScrollLength-LinesPerScreen; i--)
		lines[i] = lines[i-1] ;
	new = lines[i] = lines[0] ;
	if (new->buffer_length < line->length + 1) {
		free(new->body);
		free((char *)new->prop);
		new->body = Malloc((int)line->length + 1);
		new->prop = (u_char *) Malloc((int)line->length + 1);
		new->buffer_length = line->length + 1 ;
		}
	bcopy(line->body, new->body, (int)line->length+1);
	bcopy((char *)line->prop, (char *)new->prop, (int)line->length+1);
	new->length = line->length ;
	new->changeposition = 0 ;
	new->end_of_changes = CharsPerLine + 1 ;
	new->usedtobe = line->usedtobe ;
	new->flags = line->flags ;
	lines++ ;
	if (&lines[ScrollLength] >= &scroll_area[TotalScrollLength * 2]) {
		bcopy((char *)lines, (char *)scroll_area,
			sizeof(struct line **)*ScrollLength);
		lines = scroll_area ;
		}
	screen = &lines[ScrollLength - LinesPerScreen] ;
}
	
/* --------------- External Routines Below ------------------- */

do_display_resize() {
    register i;
/*    tc_refresh(0);*/
    BeginRepair();
    Repairing++;
    ReInitialize();
    for (i = 0; i<LinesPerScreen; i++) {
	lines[i+ScrollY]->changeposition = 0;
	lines[i+ScrollY]->end_of_changes = CharsPerLine ;
    }
    tc_refresh(0);
    tc_refresh_selection();
    psio_flush(PostScript);
}

PSFILE *
Connect_To_Server()
{
	extern PSFILE *ps_open_PostScript();
	PSFILE *f;

	if ((f = ps_open_PostScript()) == NULL) {
		return (NULL);
		}
	ConnectionEstablished = 1 ;
	return f;
}

PSFILE       *
tc_init_screen(term, xorg, yorg, font_size, framelabel, iconlabel, reload,
		userinit, font, starticonic, iconx, icony, savelines)
char *term;
int xorg, yorg, font_size;
char *framelabel, *iconlabel;
int reload;
char *userinit, *font;
int starticonic, iconx, icony, savelines;
{
	PSFILE	   *f;
	char		frametitle[100];
	char		host[100];
	char		buf[2];

	f = Connect_To_Server();
	err0("Connect_To_Server();\n");
	if (f==NULL) return NULL;

	PSDefs(reload);
	psio_flush(PostScript);

	err0("do read...");
	if (read(psio_fileno(PostScriptInput), buf, 2) != 2) {
		/* FIXUP: This is one place where a failed .psh file
		** will die in the C code!
		*/
	fprintf(stderr, "gterm: gterm could not start is NeWS server code.\n");
	fprintf(stderr, "       Check the console for NeWS error messages.\n");
		perror("gterm: tc_init_screen() read(PostScriptInput)");
		return NULL;
		}
	err0("done.\n");
	if (buf[0]!='y')
		if (JamItDownTheSocket())
			return NULL;

	if (framelabel == NULL) {
		static char te[] = " terminal emulator";
		sprintf(frametitle, "%.*s%s",
		sizeof(frametitle)-sizeof(te)-strlen(term), term, te);
		framelabel = frametitle;
		}
	if (iconlabel == NULL) {
		gethostname(host, sizeof (host));
		host[sizeof(host)-1] = '\0';
 		iconlabel = host;
		}
	if (tc_initialize(term))
		Fatal("%s: Unknown terminal type or bad termcap description",
			term);
	set_scroll_save(savelines);
	scroll_to_bottom();
	tc_init_selection();
	PSInitCode(userinit);
	CreateWindow(xorg, yorg, font_size, CharsPerLine, LinesPerScreen,
		framelabel, iconlabel, font, starticonic, iconx, icony);
	ToggleScrollBar(savelines);
	StartInput();
	/* XXX - set up PostScript process for i/p etc. */
	psio_flush(PostScript);
	return (f);
}

/* I am beging to think that gtermstart.psh is bad idea, but it does
** let someone develop a toolkit adaptor with out touching the c code
** AH! right, its there so that a site can CHOOSE what toolkit it wants.
*/

#define STRT "/gtermstart.psh"
#define LTE1 "/gterm1lte1.psh"
#define LTE2 "/gterm1lte2.psh"

JamItDownTheSocket()
{
	int f, nread;
	char buf[4096], *s, *home, *getenv(), *guess, *tail;

	/* For now the code that is passed back an forith must
	** be a 4 character code, sigh */
	err0("Waiting for PSIO\n");
	if(read(psio_fileno(PostScriptInput), buf, 5) != 5) {
	       fprintf(stderr,
		       "gterm: startup protocall error. (%s) Quitting.\n",
		       buf);
        }
	/* "fail" is only returned if the remote machine does not have the
	** gtermstart.psh file
	*/
	if ( strncmp(buf, "fail", 4) == 0) { /* Get files from our end! */
		tail = STRT;
		/* Now run gtermstart.psh to find the right toolkit */
		err0("therest()\n");
		therest(tail);
		err0("Gettoolkit()\n");
		PSGettoolkit();
		psio_flush(PostScript);
		err0("second JamItDownTheSocket()\n");
		if(JamItDownTheSocket()) {
			fprintf(stderr,
       "gterm: Remote startup error, check NeWS server console for errors.\n");
			Exit(82);
		}
	} else 	if ( strncmp(buf, "lte2", 4) == 0) {
		tail = LTE2;
	} else if ( strncmp(buf, "lte1", 4) == 0) {
		tail = LTE1;
	} else if ( strncmp(buf, "none", 4) == 0) {
		/* Fixup: add a way to find the name of the target NeWS serv */
		fprintf(stderr, "gterm: Your target NeWS server has no toolkits that I know how to use.\n");
		Exit(22);
	} else {
		fprintf(stderr, "gterm: I dont have a toolkit to use.\n");
	}
	err0("Last therest()\n");
	return (therest(tail));
}

therest(tail)
  char *tail;
{
  int f=-1, nread;
  char buf[4096], *s, *getenv(), *guess;

	/* Now try to find the files! */
	if ((s = getenv("OPENWINHOME"))) {
		guess = (char *)malloc(strlen(s) + 4 + strlen(tail));
		sprintf(guess, "%s/etc/%s", s, tail);
		f=open(guess, O_RDONLY);
	}
        if (f<0 && (s = getenv("NEWSHOME"))) {
		guess = (char *)malloc(strlen(s) + 4 + strlen(tail));
		sprintf(guess, "%s/lib/%s", s, tail);
		f=open(guess, O_RDONLY);
	}
        if (f<0) {
		s = "/usr/openwin/etc/";
		guess = (char *)malloc(strlen(s) + strlen(tail));
		sprintf(guess, "%s%s", s, tail);
		f=open(guess, O_RDONLY);
	}
  	if (f<0) {
		s = "/usr/NeWS/lib/";
		guess = (char *)malloc(strlen(s) + strlen(tail));
		sprintf(guess, "%s%s", s, tail);
		f=open(guess, O_RDONLY);
	}
 	if (f<0)
	  Fatal("Cannot access %s on gterm host machine.", tail);
	nread = read(f, buf, 4096);
	while (nread>0) {
		s = buf;
		while(nread-- > 0)
			psio_putc(*s++, PostScript);
		nread = read(f, buf, 4096);
		}
	if (nread<0) {
		fprintf(stderr, "gterm: Failed reading \"%s\"", guess);
		perror("Error:");
	}
	close(f);
	free(guess);
	return nread;
}

/*
PostScriptErrorMessage(s)
char *s;
{
	PopMsg(s);
	psio_flush(PostScript);
}
*/
KillPSSide() { PSSideKill(); }

#define Finished_Dammage	1
#define Finished_NoDammage	2
#define Painting_Dammage	3
#define Painting_NoDammage	4

static int RefreshState = Finished_Dammage ;

end_refresh()
{
	switch (RefreshState) {
	case Painting_Dammage:
		RefreshState = Finished_Dammage ;
		break;
	case Painting_NoDammage:
		if (RefreshSuppressed)
			RefreshState = Finished_Dammage ;
		else	RefreshState = Finished_NoDammage ;
		break;
	case Finished_Dammage:
		RefreshState = Finished_Dammage ;
		break;
	case Finished_NoDammage:
		RefreshState = Finished_NoDammage ;
		break;
		}
}

/*ARGSUSED*/
tc_refresh(full)
    int full;
{
    register struct line *l;
    register int i, pos, c;
    register u_char *cp;
    register struct line **sp;
    int delta, curdelta, len;
    int linesscrolledforward, linesscrolledreverse;
    int paintfromtop;
    int x, y;
    static int CursorLine = 0 ;
    static int CursorCol = 0 ;
    static int oldlinessaved = 0 ;
    static int oldlinesperscreen = 0 ;
    static int oldscrolly = 0 ;
    u_char *tp, *ep, hit[MaxLinesPerScreen];

    if (RefreshSuppressed) return;
    switch (RefreshState) {
	case Painting_Dammage:
		if (FastPaint) return ;
		break;
	case Painting_NoDammage:
		RefreshState = Painting_Dammage ;
		if (FastPaint) return ;
		break;
	case Finished_Dammage:
		RefreshState = Painting_NoDammage ;
		break;
	case Finished_NoDammage:
		RefreshState = Finished_Dammage ;
		return;
	    }
    if (ScrollY != oldscrolly) tc_take_down_selection();
    bzero((char *)hit, LinesPerScreen);
    linesscrolledforward = linesscrolledreverse = 0 ;
    if (!fontisfixedwidth) {
	y = Dot.y + ScrollLength - LinesPerScreen - ScrollY ;
	if (y >= 0 && y < LinesPerScreen)
		hit[y] = 1 ;
	y = CursorLine + ScrollLength - LinesPerScreen - ScrollY ;
	if (y >= 0 && y < LinesPerScreen)
		hit[y] = 1 ;
	}
    else RemoveCursor();
    /*
     * Figure out which lines have moved and by
     * how much.  Accumulate delta line movements
     * and copy lines to perform scrolling.
     */
    curdelta = 0;
		/* positive delta means lines are travelling in this direction
		 *			 |  |
		 *			 |  |
		 *			\    /
		 *			 \  /
		 *			  \/
		 */
    sp = &lines[ScrollY];
    for (i = 0; i < LinesPerScreen; i++) {
	l = *sp++;
	pos = l->usedtobe ;	/* this line just moved from position pos */
	delta = i - pos ;	/* it moved this far in getting here */
	if (pos >= LinesPerScreen || pos < 0 || hit[pos] || (delta && !DoScrolling)) {
		delta = 0 ;		/* this line has been scribbled on,
					 *   so it stays put */
		l->changeposition = 0 ;	/* but has been dammaged */
		l->end_of_changes = CharsPerLine ;
		}
	l->usedtobe = i;
#ifdef lint
	c = 0 ;
#endif
	/*
	 * If the delta changes, we need to start a new
	 * run of lines to copy.  If there was a previous
	 * run, then copy those first.
	 */
	if (delta != curdelta) {
	    if (curdelta != 0) {
		for (pos = c; pos < i; pos++)
			hit[pos] = 1 ;
		if (curdelta > 0)
			linesscrolledreverse += i - c ;
		else	linesscrolledforward += i - c ;
		CopyLines(c - curdelta, curdelta, CharsPerLine, i - c);
		}
	    curdelta = delta;		/* delta for current run */
	    c = i;			/* starting line of run */
	}
    }
    /*
     * Catch any trailing run needing to be copied.
     */
    if (curdelta != 0) {
	if (curdelta > 0)
		linesscrolledreverse += i - c ;
	else	linesscrolledforward += i - c ;
	CopyLines(c - curdelta, curdelta, CharsPerLine, i - c);
	}
#ifdef lint
	paintfromtop = linesscrolledreverse > linesscrolledforward ;
#endif
	paintfromtop = 1 ;

	/*
	 * Finally, perform any erasures
	 * and/or text painting required.
	 */
	if (paintfromtop)
		sp = &lines[ScrollY] ;
	else	sp = &lines[ScrollY + LinesPerScreen - 1];
	for (i = 0; i < LinesPerScreen; (paintfromtop ? sp++ : sp--), i++) {
		l = *sp;
		pos = l->changeposition;
		if ( pos < (min(l->end_of_changes+MaxCharsPerLine, l->length)+1) ) {
			/* FIXUP: This is the cause of the line redraw bug! */
			if (!fontisfixedwidth)
				pos = 0 ;
			x = pos ;
			y = l->usedtobe ;
			MoveTo(x, y);
			if (l->buffer_length == 0) {
				PaintNor(" ", 1);
				continue;
				}
			ep = &l->prop[l->length+1];
			for (cp = &l->prop[pos]; cp < ep; pos += len) {
				/*
				 * Calculate longest sub-string of
				 * changed text with identical properties.
				 */
				tp = cp;
				if (!fontisfixedwidth &&
						y == Dot.y + ScrollLength -
						LinesPerScreen - ScrollY) {
					c = *cp++ ;
					if (x++==Dot.x)
						c ^= ReverseVideoLook ;
					for (; c==*cp && cp<ep && x!=Dot.x;
							cp++, x++)
						;
					}
				else	{
					for (c = *cp++; c==*cp && cp<ep; cp++)
						;
					}
				len = cp - tp;
				/*
				 * Paint sub-string according to properties.
				 */
				if (c & UnderlineLook) {
					if (c & ReverseVideoLook)
						PaintUnderRev(&l->body[pos],
							len);
					else
						PaintUnderNor(&l->body[pos],
							len);
					}
				else	{
					if (c & ReverseVideoLook)
						PaintRev(&l->body[pos], len);
					else
						PaintNor(&l->body[pos], len);
					}
				}
			l->changeposition = CharsPerLine + 1 ;
			l->end_of_changes = 0 ;
			}
		}

    if (fontisfixedwidth) PlaceCursor(Dot.x, Dot.y);
    CursorLine = Dot.y ;
    CursorCol = Dot.x ;
#ifdef lint
    CursorLine = CursorCol ;
#endif
    if (Repairing) {
	EndRepair();
	Repairing = 0;
    }
    if (oldlinessaved != ScrollLinesSaved ||
		oldlinesperscreen != LinesPerScreen) {
	SetScrollBarValue(ScrollLinesSaved,
		ScrollY + ScrollLinesSaved - ScrollLength);
	oldlinessaved = ScrollLinesSaved ;
	oldlinesperscreen = LinesPerScreen ;
	}
    if (ScrollY != oldscrolly) tc_refresh_selection();
    oldscrolly = ScrollY ;
    EndRefresh();
    FlushPostScript();
}

change_rowcol(row, col)
int row, col;
{
	extern int Mfd;		/* Master pty fd */
	extern int Sfd;		/* Slave pty fd */
	extern int pgrp;	/* Process group of slave... */
	int pgroup;

	if (row >= MaxLinesPerScreen) row = MaxLinesPerScreen - 1 ;
	if (col >= MaxCharsPerLine) col = MaxCharsPerLine - 1 ;
	if (row != LinesPerScreen || col != CharsPerLine) {
#ifdef sun
#ifdef TIOCSSIZE
		struct ttysize ts;

		ts.ts_lines = row;
		ts.ts_cols = col;
		ioctl(Mfd, TIOCSSIZE, &ts);
#endif TIOCSSIZE
#else /* !sun */
#ifdef TIOCSWINSZ
		struct winsize ws;

		ws.ws_row = row;
		ws.ws_col = col;
		ws.ws_xpixel = 0; /* don't know */
		ws.ws_ypixel = 0; /* don't know */
		ioctl(Mfd, TIOCSWINSZ, &ws);
#endif TIOCSWINSZ
#endif /* sun */
		ScrollY += LinesPerScreen - row ;
		resetscreensize(row, col); /* internal to gterm */
#ifdef sun
		/* Tell the OS that this pty has changed size */
		if (ioctl(Mfd, TIOCGPGRP, &pgroup)<0)
			perror("gterm: ioctl TIOCGPGRP");
		else if (killpg(pgroup, SIGWINCH)<0)
			perror("gterm: kill SIGWINCH");
#endif sun
#ifdef HAVE_TCGETPGRP
		{
			pid_t notify = tcgetpgrp(Mfd);
			if (notify == -1) {
				perror("gterm: tcgetpgrp() failed");
			} else {
				if (killpg(notify, SIGWINCH) < 0)
				  perror("gterm: failed SIGWINCH");
			}
		}
#endif /* HAVE_TCGETPGRP */
	  }
	for (row=0; row<ScrollLength; row++)
		if (lines[row])
			lines[row]->usedtobe = -1 ;
	/* allocate full length lines for whole screen */
	set_scroll_save(userScrollLength);
	scroll_to(ScrollY - ScrollLength + ScrollLinesSaved);
}

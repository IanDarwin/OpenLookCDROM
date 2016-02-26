/*
	NeWS Distributed Emacs:
	++++++++++++++++++++++
	A Unix (SunOS4 and Unicos) NeWS video driver for MicroEMACS
	by P.R.Ove

	C Client driver program that uses the NeWS PostScript program news.cps
*/

#define	termdef	1			/* don't define "term" external */

#include <ctype.h>
#include <stdio.h>
#include <signal.h>
#include "news.h"
#include "estruct.h"
#include "edef.h"

#if NeWS
#define	MARGIN	8
#define	SCRSIZ	64
#define	NPAUSE	10			/* # times thru update to pause */
#define BEL     0x07
#define ESC     0x1B
#define NEWSBUFSIZ	1000
#define min(x,y)	((x) < (y) ? (x) : (y))
#define max(x,y)	((x) > (y) ? (x) : (y))
#define P(i)		(0x7fff & (i))		/* avoid (-) (long ints) */

extern int	newsopen();
extern int	newsgetc();
extern int	newsputc();
extern int	newsflush();
extern int	newsclose();
extern int	newskopen();
extern int	newskclose();
extern int	newsmove();
extern int	newseeol();
extern int	newseeop();
extern int	newsbeep();
extern int	newsrev();
extern int	newscres();

#if	COLOR
extern	int	newsfcol();
extern	int	newsbcol();

int	cfcolor = -1;		/* current forground color */
int	cbcolor = -1;		/* current background color */
#endif

/*
	Define the type of TERMINAL for microEmacs to interface with.
	Give the function names that drives the editor processes.
*/
TERM term = {
	NULL,	/* these four values are set dynamically at open time */
	NULL,
	NULL,
	NULL,
	MARGIN,
	SCRSIZ,
	NPAUSE,
	newsopen,
	newsclose,
	newskopen,
	newskclose,
	newsgetc,
	newsputc,
	newsflush,
	newsmove,
	newseeol,
	newseeop,
	newsbeep,
	newsrev,
	newscres
#if	COLOR
	, newsfcol,
	newsbcol
#endif
};

/* Called when ^C is received */
newsquit()
{
	newsclose() ;
	exit(0) ;
}

/* Opens a NeWS window */
newsopen()
{
	int	rows, cols ;
	int	height, width ;
	int	init_flag ;

	if (term.t_nrow <= 0) 
		term.t_nrow = 36 ;	/* default size */
	term.t_nrow = min(term.t_nrow, 100) ;
	term.t_nrow = max(term.t_nrow, 5) ;
	ps_open_PostScript() ;
	ps_initialize(term.t_nrow) ;
	ps_flush_PostScript() ;
	signal(SIGHUP, newsquit) ;
	signal(SIGINT, newsquit) ;
	signal(SIGTERM, newsquit) ;
	ps_getscreensize(&rows, &cols) ;
	term.t_ncol = cols ;
	term.t_mrow = term.t_nrow ;
	term.t_mcol = max(term.t_ncol*2, 200) ;	/* to change screen size */
}

/* Closes a NeWS session */
newsclose()
{
	ps_terminate() ;
	ps_flush_PostScript() ;
	sleep(1) ;
	ps_close_PostScript() ;
}


/* Used by mEmacs to open keyboard */
newskopen()
{
	/* taken care by newsopen */
}

/* Used by mEmacs to close keyboard */
newskclose()
{
	/* taken care by newsopen */
}


/* Refresh when the screen changes size */
newsrefresh()
{
	int	rows, cols ;
	int height,width;

	ps_getscreensize(&rows, &cols) ;
	if (cols != term.t_ncol) 
	{
		ps_statefix() ;		/* NeWS "bug" workaround */
		newwidth(P(TRUE), P(cols)) ;
	}
}

/* Updates screen without data transfer (useful for color updates) */
newslocalupdate()
{
	ps_localupdate() ;
}


newsscroll(first, match, count)
int	first, match, count ;
{
	ps_scroller(P(first), P(match), P(count)) ;
}


/* debugging routine */
newsdump(s)
char	*s ;
{
	ps_dump(s) ;
	ps_flush_PostScript() ;
}

newsmove(row, col)
register int row, col;
{
        ps_move(P(row),P(col)) ;
}

newsflush()
{
	if (!typahead()) ps_flush_PostScript() ;
}

/* only ordinary characters will be sent here */
newsputc(c)
char c ;
{
	ps_putc(P(c)) ;
}

newsputline(row, s, fg, bg)
int	row, fg, bg;
char	*s ;
{
#ifdef CRAY		/* trap mysterious Cray-specific bug */
if ( (0x7 & fg) != fg || (0x7 & bg) != bg )
	fprintf(stderr, "PUTLINE: Bad color update attempt, row %d, fg %d, bg %d\n",
		row, fg, bg) ;
else
#endif
	ps_putline(P(row), s, P(fg), P(bg)) ;
}

/* using existing colors */
newsfastputline(row, s)
int	row ;
char	*s ;
{
	ps_fastputline(P(row), s) ;
}

newssetrowcolors(row, fg, bg)
int	row, fg, bg ;
{
#ifdef CRAY		/* trap mysterious Cray-specific bug */
if ( (0x7 & fg) != fg || (0x7 & bg) != bg )
	fprintf(stderr, "SETCOLOR: Bad color update attempt, row %d, fg %d, bg %d\n",
		row, fg, bg) ;
else
#endif
	ps_setrowcolors(P(row), P(fg), P(bg)) ;
}

newscls() 
{
	ps_cls();
}

newsinserton()	
{
	ps_inserton();
}

newsinsertoff()
{
	ps_insertoff();
}

newscmodeon()
{
	ps_cmodeon();
}

newscmodeoff()
{
	ps_cmodeoff();
}

newsimmediateon()
{
	ps_immediateon();
}

newsimmediateoff()
{
	ps_immediateoff();
}


newsgetc()
{
static char inbuf[NEWSBUFSIZ] ;
static int buf_count = 0, buf_index = 0, raw_count = 0 ;
int	i, j ;
 
	if ( buf_index >= buf_count ) {
		ps_gets(&buf_count, inbuf) ;
		buf_index = 0 ;
	}
 
	if ( buf_count == 0 ) return(0) ;	/* shouldn't happen */
	buf_index++ ;
	inhibit_update = ( buf_index < buf_count ) ;
 
	/* force update before 1st special character if not on message line */
/*	if (	buf_index > 1 &&
		( inbuf[buf_index-1]<32 || inbuf[buf_index-1]==127 ) &&
		isprint(inbuf[buf_index-2]) &&
		ttrow != term.t_nrow
	)
		update(TRUE) ;
*/

	/* return the next character */
	/* fprintf(stderr, "received char %d\n", 0xFF & inbuf[buf_index-1]) ; */
	return( 0xFF & inbuf[buf_index-1] ) ;
}



newseeol()
{
        ps_eeol() ;
}


newseeop()
{
	ps_eeop() ;
}


newsrev(state)		/* change reverse video status */
int state;		/* FALSE = normal video, TRUE = reverse video */
{
}


newscres()	/* change screen resolution */
{
	return(TRUE);
}


spal(dummy)	/* change palette string */
{
}


#if	COLOR	/* defined but not used, color is set by lines */
newsfcol()
{
}
newsbcol()
{
}
#endif


newsbeep()
{
}


/* REPORT_MODES
 *
 * Tell the workstation what mode we are in.  Should be called whenever
 * a buffer/window/file/mode is changed.
 */
newsreportmodes()
{
int	i ;

for (i=0; i < NUMMODES; i++) {
	if (strcmp("OVER", modename[i]) == 0) {
		if ( 1 << i & curwp->w_bufp->b_mode ) ps_insertoff() ;
		else ps_inserton() ;
	}
	else if (strcmp("CMODE", modename[i]) == 0) {
		if ( 1 << i & curwp->w_bufp->b_mode ) ps_cmodeon() ;
		else ps_cmodeoff() ;
	}
}
}


#if	FLABEL
fnclabel(f, n)		/* label a function key */

int f,n;	/* default flag, numeric argument [unused] */

{
	/* on machines with no function keys...don't bother */
	return(TRUE);
}
#endif
#else

hello()
{
}

#endif

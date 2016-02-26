/*
 * NeWS interface for GNU Emacs.
 *
 * Author: 	Chris Maio
 * Last edit:	4 Sep 1988
 */

#include <signal.h>
#include "config.h"

#ifdef HAVE_NEWS
#include "NeWS.h"			/* generated from NeWS.cps */
#undef NULL

#ifndef psio_availinputbytes
/* NeWS 1.0 compatibility */
#define psio_availinputbytes(p) ((p)->_cnt)
#define psio_eof(p) (feof ((p)))
#define psio_error(p) (ferror ((p)))
#define psio_flush(p) (fflush ((p)))
#endif

#undef ps_flush_PostScript
#define ps_flush_PostScript() \
    (psio_flush (PostScript), (psio_error (PostScript) ? brokenpipe () : 0))

#ifndef makedev
#include <sys/types.h>
#endif
#include "lisp.h"
#include "dispextern.h"
#include "termopts.h"
#include "termchar.h"
#include "termhooks.h"
#include "window.h"
#include <fcntl.h>
#include <sys/ioctl.h>
#include <sys/errno.h>

static int
    blockinputmask,			/* sigblock mask */
    scrollregion,			/* 0 or bottom of scroll region */
    SavedX, SavedY, VisibleX, VisibleY,	/* our notion of cursor postition */
    updating,				/* 1 => inside update_screen */
    winheight, linewidth,		/* ClientCanvas dimensions */
    lineheight, charwidth, baseline,	/* font metrics */
    repaint,				/* 1 => screen repaint requested */
    highlight,				/* 1 => use inverse video */
    positioned, mapped,			/* 1 => window is positioned/mapped */
    retainwindow,			/* 1 => retain this window */
    keepalive,				/* <0 => window probably dead */
    metakey,				/* 1 => keyboard meta key is down */
    outputstarted,			/* StartPS recursion depth */
    cursordown,				/* 1 => cursor is visible */
    nextevent,				/* next index into event queue */
    eventindex,				/* current index */
    eventcount,				/* count of pending events  */
    sbufstart, sbufend,			/* start, end of sbuf */
    use_reshapefromuser;		/* start up with reshapefromuser */

int ps_fix_screen (),			/* handle screen updates */
    ps_topos (),			/* update cursor location */
    ps_clear_screen (),			/* clear the canvas */
    ps_clear_end_of_line (),		/* clear to end of line */
    ps_ins_del_lines (),		/* insert/delete lines with copyarea */
    ps_insert_chars (),			/* this hurts */
    ps_delete_chars (),			/* so does this */
    ps_write_chars (),			/* raw character output */
    ps_change_line_highlight (),	/* flip foreground/background colors */
    ps_reassert_line_highlight ();	/* " */
    ps_feep (),				/* ring the bell */
    ps_reset_terminal_modes (),		/* do nothing */
    ps_set_terminal_modes (),		/* do nothing */
    ps_update_begin (),			/* buffer output */
    ps_update_end (),			/* flush output */
    ps_set_terminal_window (),		/* set scroll region */
    ps_read_socket ();			/* input handler */

extern struct display_line *DesiredScreen[], *PhysScreen[];

/*
 * Macros to transform screen coordinates to canvas coordinates, assuming
 * the default transformation matrix.  We assume a fixed-width font whose
 * character width is an integral multiple of the default unit width, which
 * is accomplished by defaulting the font to Courier 12, and setting the NeWS
 * "printermatch" variable set to false.
 */

#define BORDER		2		/* internal border */

/*
 * convert character units to PostScript units
 */
#define PSdX(x)		((x)*charwidth)
#define PSdY(y)		((y)*lineheight)

/*
 * convert character coordinates to PostScript coordinates
 */
#define PSX(x)		(PSdX((x))+BORDER)
#define PSY(y)		(PSdY((screen_height-(y)-1)))

/*
 * convert absolute PostScript coordinates to character coordinates
 */
#define PScX(x)		(((x)-BORDER)/charwidth)
#define PScY(y)		(screen_height-(((y)-BORDER)/lineheight)-1)

/*
 * convert PostScript canvas dimensions to character units
 */
#define PSCOLS		((linewidth-(2*BORDER))/charwidth)
#define PSROWS		((winheight-BORDER)/lineheight)

#define min(a,b) ((a)<(b) ? (a) : (b))
#define max(a,b) ((a)>(b) ? (a) : (b))

#ifndef sigmask
#define sigmask(no) (1L << ((no) - 1))
#endif

/*
 * If ps-fix-screen is called more than this many times without doing
 * any real output, we poke the connection to see if it is still alive.
 * This tickle function is what's responsible for the occasional cursor
 * toggle you may notice in an otherwise idle window.
 */
int KEEPALIVE_THRESHHOLD = 10;

/*
 * StartPS and EndPS should be used around any output calls, to take
 * care of buffering output, lifting the cursor, and protecting the
 * NeWS library from SIGIO interrupts.
 */

StartPS ()
{
    if (cursordown)
	togglecursor ();		/* make sure cursor is protected */
    if (!updating && outputstarted == 0)
	    blockinputmask = sigblock (sigmask (SIGIO));
    ++outputstarted;
}

EndPS ()
{
    if (--outputstarted < 1)
	if (!updating) {
	    if (!cursordown)
		togglecursor ();
	    ps_flush_PostScript ();
	    sigsetmask(blockinputmask);
	    keepalive = KEEPALIVE_THRESHHOLD; /* reset keepalive counter */
	}
}

/*
 * Emacs will loop forever on a socket whose other end has gone away; we
 * try to detect this by looking for a lot of calls to ps_fix_screen
 * done with no i/o to the NeWS window.  To confirm that the other end
 * has gone away, we send a linefeed to the server; since this will
 * induce a SIGPIPE signal which is normally not caught, we catch it
 * ourselves.  No checking is done to make sure that the SIGPIPE is for
 * our socket, but since SIGPIPE on another fd would be fatal anyway,
 * at least this way we have a shot at saving dirty buffers.  If SIGPIPE is
 * caught, we ignore it and fake a SIGHUP so that a subsequent SIGPIPE
 * is less likely to interrupt us while we're trying to save buffers.
 *
 */

static
brokenpipe ()
{
    signal (SIGPIPE, SIG_IGN);
    unrequest_sigio ();
    (void) close (0);
    (void) open ("/dev/null", 0);
    fprintf (stderr, "emacs: lost connection to display.\n");
    fatal_error_signal (SIGHUP);
    abort ();				/* should not get here */
}

/*
 * First half of window initialization.  This is complicated by the fact
 * that Emacs wants to be able to write to the window before calling the
 * term_setup_hook, where the user may change the size, location, and font
 * used for the window.
 */

NeWS_term_init ()
{
    MetaFlag = 1;			/* we support meta keys */
    metakey = 0;			/* meta key is not down */
    visible_bell = 1;			/* visible bell is all we've got */
    interrupt_input = 1;		/* we use SIGIO */
    inverse_video = 0;			/* no support for inverse video yet */
    line_ins_del_ok = 1;		/* we do line insert/delete */
    char_ins_del_ok = 1;		/* we do char insert/delete
    scroll_region_ok = 1;		/* we do scroll regions */
    memory_below_screen = 0;		/* no offscreen memory */
    dont_calculate_costs = 1;		/* assume everything is cheap */
    must_write_spaces = 0;		/* this loses big */
    baud_rate = 9600;			/* fake out speed-sensitive code */
    use_reshapefromuser = 0;		/* don't use reshapefromuser */

    outputstarted = 0;			/* not within StartPS/EndPS */
    cursordown = 0;			/* cursor is not down */
    scrollregion = 0;			/* no scroll region defined */
    repaint = 0;			/* no repaint needed yet */
    updating = 0;			/* we are not inside update_screen */
    positioned = 0;			/* window origin not yet set */
    mapped = 0;				/* not fully initialized yet */
    nextevent = eventindex = eventcount = 0; /* no input events yet */
    sbufstart = sbufend = 0;		/* no strings in event queue */
    keepalive = KEEPALIVE_THRESHHOLD;	/* NeWS process hasn't died yet */

    /* set up hooks */
    fix_screen_hook = ps_fix_screen;
    clear_screen_hook = ps_clear_screen;
    clear_end_of_line_hook = ps_clear_end_of_line;
    ins_del_lines_hook = ps_ins_del_lines;
    insert_chars_hook = ps_insert_chars;
    delete_chars_hook = ps_delete_chars;
    write_chars_hook = ps_write_chars;
    ring_bell_hook = ps_feep;
    set_terminal_modes_hook = ps_set_terminal_modes;
    reset_terminal_modes_hook = ps_reset_terminal_modes;
    update_begin_hook = ps_update_begin;
    update_end_hook = ps_update_end;
    set_terminal_window_hook = ps_set_terminal_window;
    read_socket_hook = ps_read_socket;
    topos_hook = ps_topos;
    change_line_highlight_hook = ps_change_line_highlight;
    reassert_line_highlight_hook = ps_reassert_line_highlight;

    /*
     * Handle window 'zaps' as gracefully as possible.  Note that
     * process.c must be modified to preserve the SIGPIPE handler.
     */
    (void) signal (SIGPIPE, brokenpipe);

    screen_height = 24;			/* keep emacs happy until our */
    screen_width = 80;			/*  term-setup-hook is run */

    (void) close (0);			/* emacs wants input on fd 0 */
    if (ps_open_PostScript () == 0) {
	fprintf (stderr, "Cannot connect to NeWS server\n");
	exit (1);
    }
    StartPS ();
    InitializePS ();			/* set up window, etc */
    /* query server for height and width of the window */
    (void) GetWinInfo(&linewidth, &winheight);
    (void) GetFontInfo(&charwidth, &baseline, &lineheight);
    screen_height = PSROWS;
    screen_width = PSCOLS;
    EndPS ();
    return 1;
}

/*
 * handle repaint requests, and try to detect a broken connection
 */

ps_fix_screen ()
{
    if (--keepalive < 0) {
	/*
	 * Poke the connection to avoid a loop if the other end goes away.
	 */
	pprintf (PostScript, "\n", 1);
	ps_flush_PostScript ();
	keepalive = KEEPALIVE_THRESHHOLD;
    }
    if (!mapped) {
	/*
	 * If we get here, Emacs is trying to do output before calling
	 * our term-setup-hook.
	 */
#if 0
	fprintf (stderr, "emacs: NeWS driver was not properly initialized\n");
	exit (1);
#endif
	return;
    }
    if (repaint) {
	repaint = 0;
	StartPS ();
	/*
	 * Reset our idea of the screen size and font metrics, in case
	 * the user changed them on us.
	 */
	(void) GetWinInfo(&linewidth, &winheight);
	(void) GetFontInfo(&charwidth, &baseline, &lineheight);
	/*
	 * Set screen_garbaged so that if change_screen_size() doesn't
	 * refresh the screen, we'll know to run DoDsp below.
	 */
	++screen_garbaged;		/* XXX */
	change_screen_size (PSROWS, PSCOLS, 0);
	EndPS ();
    }
    if (screen_garbaged)		/* XXX */
	DoDsp(1);
}

/*
 * clear the screen, reset the cursor, etc
 */
ps_clear_screen ()
{
    cursordown = highlight = 
	cursX = cursY = SavedX = SavedY = VisibleX = VisibleY = 0;
    StartPS();
    ClearScreen ();
    EndPS();
}

/*
 * clear to end of line.  arg is ignored since there's little point in
 * optimizing this one.
 */
ps_clear_end_of_line (first_blank)
register int first_blank;
{
    StartPS ();
    cleartoeol (cursX, cursY, highlight);
    EndPS ();
}

/*
 * set the current highlight mode, and clear this line to the highlight
 * background color.
 */

ps_change_line_highlight (new_highlight, vpos, first_unused_hpos)
int new_highlight, vpos, first_unused_hpos;
{
    highlight = new_highlight;
    StartPS ();
    ps_topos (vpos, 0);			/* should this be first_unused_hpos? */
    ps_clear_end_of_line (0);
    EndPS ();
}

/*
 * set the highlight color, assuming that the line has already been painted.
 * ignoring vpos doesn't seem to hurt.
 */
ps_reassert_line_highlight (new_highlight, vpos)
int new_highlight, vpos;
{
    highlight = new_highlight;
}

/*
 * dump part of a line to the display.  this needs work.  XXX
 */
static
writechars (start, len)
register char *start;
int len;
{
    register int n;
    char *end;

    StartPS ();
    if (cursY >= 0 && cursY < screen_height) {
	if (updating) {
	    /*
	     * See if we can get a length for the line to be output.
	     * If we're not handed the whole thing, chances are an insert
	     * char sequence will follow so it's not safe to clear the
	     * rest of the line; in this case we just clear enough space
	     * for the string we're supposed to output.  This may be slower
	     * but it looks better on the displays I've tried it on.
	     */
	    n = (DesiredScreen && DesiredScreen[cursY + 1]) ?
		DesiredScreen[cursY + 1]->length : 0;

	    if (n > (cursX + len))
		clearshow (cursX, cursY, start, len);
	    else {
		cleartoeol (cursX, cursY, highlight);
		xyshow (cursX, cursY, start, len);
	    }
	    ps_topos (cursY, cursX + len);
	}
	else {
	    if (VisibleX >= 0 && VisibleX < screen_width &&
		VisibleY >= 0 && VisibleY < screen_height) {
		/* can't trust cursX here? */
		if ((VisibleX + len) >= screen_width)
		    len = screen_width - 1 - VisibleX;
		if (len > 0)
		    xyshow (VisibleX, VisibleY, start, len);
		VisibleX += len;
	    }
	}
    }
    EndPS ();
}

ps_insert_chars (start, len)
register char *start;
register int len;
{
    if (len > 0) {
	StartPS ();
	InsertChars (PSX (VisibleX), PSY (VisibleY), PSdX (len));
	if (start)
	    xyshow (VisibleX, VisibleY, start, len);
	VisibleX += len;
	EndPS ();
    }
}

ps_write_chars (start, len)
register char *start;
register int len;
{
    if (len > 0)
	writechars (start, len);
}

ps_delete_chars (n)
{
    StartPS ();
    DeleteChars(PSX (VisibleX), PSY (VisibleY), PSdX (n));
    EndPS ();
}

ps_feep ()
{
    StartPS ();
    Flash ();
    EndPS ();
}

/*
 * These routines don't seem to need to do anything except occupy their
 * respective hooks so Emacs doesn't mess with stdout.
 */

ps_reset_terminal_modes () {}
ps_set_terminal_modes () {}

ps_set_terminal_window (n)
int n;
{
    if (n <= 0 || n >= screen_height)
	scrollregion = 0;
    else
	scrollregion = n - 1;
}

ps_update_begin ()
{
    highlight = 0;
    StartPS ();
    SavedX = cursX;
    SavedY = cursY;
    updating = 1;
}

ps_update_end ()
{
    updating = 0;
    ps_topos (SavedY, SavedX);
    EndPS ();
    highlight = 0;
}

ps_ins_del_lines (vpos, n)
register int vpos, n;
{
    if ((vpos < 0) || ((vpos + (n > 0 ? n : -n)) >= screen_height))
	return;

    StartPS ();
    ps_topos (vpos, 0);
    if (n > 0) {
	if (scrollregion)
	    InsertLines2 (PSY (vpos-1), PSY (scrollregion), PSdY (n));
	else
	    InsertLines (PSY (vpos), PSdY (n));
    }
    else if (n < 0) {
	n = -n;
	if (scrollregion)
	    DeleteLines2 (PSY (vpos-1), PSY (scrollregion), PSdY (n));
	else
	    DeleteLines (PSY(vpos), PSdY(n));
    }
    EndPS ();
}

/*
 * Beware changing VisibleX while not inside StartPS/EndPS, since they
 * hide the cursor while things are moving.
 */
ps_topos (row, col)
register int row, col;
{
    cursX = col;
    cursY = row;
    /* need to put up cursor, maybe */
    VisibleX = col;
    VisibleY = row;
}

/*
 * Input queue support routines.   ps_read_socket() is called from a signal
 * handler and thus it can't act directly on its input; instead, it saves
 * the input events in a queue where (NeWS-next-event) can find them.
 */

typedef struct {
    int nelements;
    Lisp_Object list[8];
} selectionevent;

#define NEVENTS 8			/* must be a power of 2 */
static selectionevent selectionevents[NEVENTS];

#define EINCR(i) i = ((i+1)&(NEVENTS-1))

/*
 * Queue an event; "what" is one of:
 *	0 => insert string
 *	1 => execute string 
 *	2 => set selection at ...
 *	3 => extend selection to ...
 */
queue_event (what, rank, size, x, y)
int what, rank, size, x, y;
{
    selectionevent *se;
    if (eventcount >= NEVENTS)
	return 0;
    se = &selectionevents[eventindex];
    se->nelements = 5;
    se->list[0] = XFASTINT (what);
    se->list[1] = XFASTINT (rank);	/* for strings, buffer index */
    se->list[2] = XFASTINT (size);	/* for strings, string length */
    se->list[3] = XFASTINT (x);
    se->list[4] = XFASTINT (y);
    EINCR (eventindex);
    eventcount++;
    return 1;
}

/*
 * Queue a string for execution/insertion by Emacs.  We need to use a private
 * string buffer since the strings are queued from signal handlers.  It might
 * be useful to queue the current mouse position with the string, so commands
 * affect the buffer-window under the mouse.
 *
 * Strings are added to sbuf at sbufend, and removed from sbufstart;
 * when the queue is empty, sbufend and sbuftstart are reset to zero.
 */

static unsigned char sbuf[4096];

queue_string (s, exec)
unsigned char *s;
int exec;				/* 0=>insert, 1=>execute */
{
    int n = strlen (s);

    if (sbufend + n < sizeof sbuf) {
	strcpy (&sbuf[sbufend], s);
	if (queue_event (exec, sbufend, n, 0, 0)) {
	    sbufend += n;
	    return 1;
	}
    }
    return 0;
}

ps_read_socket (fd, buffer, length)
int fd, length;
unsigned char *buffer;
{
    int x, y, x1, y1, i, n;

    x = y = i = 0;

    do {
	if (psio_eof (PostScriptInput) || psio_error (PostScriptInput))
	    brokenpipe ();
	if (KeyboardInput (&n)) {
	    buffer[i] = n;
	    if (MetaFlag && metakey)
		buffer[i] |= 0200;	/* add metabit if appropriate */
	    i++;
	}
	else if (MetaKeyDown (&n)) {
	    metakey = n;
	}
	else if (SetSelectionAt (&x1, &n, &x, &y)) {
	    x = PScX (x);
	    x = max (min (x, screen_width - 1), 0);
	    y = PScY (y);
	    y = max (min (y, screen_height - 1), 0);
	    if (queue_event (2, x1, n, x, y)) {
		buffer[i++] = 'x' & 037; /* C-x C-@ */
		buffer[i++] = 0;
	    }
	}
	else if (ExtSelectionTo (&x1, &n, &x, &y)) {
	    unsigned char *cp = &buffer[i];
	    x = PScX (x);
	    x = max (min (x, screen_width - 1), 0);
	    y = PScY (y);
	    y = max (min (y, screen_height - 1), 0);
	    if (queue_event (3, x1, n, x, y)) {
		buffer[i++] = 'x' & 037; /* C-x C-@ */
		buffer[i++] = 0;
	    }
	}
	else if (EmacsInput (&buffer[i])) {
	    i += strlen (&buffer[i]);
	}
	else if (EmacsInsert (&buffer[i])) {
	    if (queue_string (&buffer[i], 0)) {
		buffer[i++] = 'x' & 037; /* C-x C-@ */
		buffer[i++] = 0;
	    }
	}
	else if (EmacsCommand (&buffer[i])) {
	    if (queue_string (&buffer[i], 1)) {
		buffer[i++] = 'x' & 037; /* C-x C-@ */
		buffer[i++] = 0;
	    }
	}
	else if (RepairWindow (&x, &y, &x1, &y1)) {
	    repaint++;
	}
	else if (DebugMessage (&buffer[i])) {
	    fprintf (stderr, "debug: %s\n", &buffer[i]);
	}
	else {
	    fprintf (stderr,
		     "emacs: received invalid message from NeWS server\n");
	    break;
	}
    } while (psio_availinputbytes (PostScriptInput) != 0);

    return i;				/* return count of keyboard chars */
}

/*
 * Display N chars from string S at row X, column Y
 */

xyshow (x, y, s, n)
int x, y, n;
char *s;
{
    if (highlight)
	ShowInverted (PSX (x), PSY (y) + baseline, s, n);
    else
	Show (PSX (x), PSY (y) + baseline, s, n);
}

/*
 * Display N chars from S at X, Y, after clearing background.
 */

clearshow (x, y, s, n)
int x, y, n;
char *s;
{
    if (highlight)
	ClearShowInverted (PSX (x), PSY (y), PSdX (n),
			   PSY (y) + baseline, s, n);
    else
	ClearShow (PSX (x), PSY (y), PSdX (n), PSY (y) + baseline, s, n);
}

cleartoeol(x,y,highlight)
int x, y, highlight;
{
    if (x)
	x = PSX(x);			/* if at left margin, stay there */
    y = PSY(y);
    if (highlight)
	InvertToEOL (x, y);
    else
	ClearToEOL (x, y);
}

togglecursor ()
{
    if (VisibleX >= 0 && VisibleX < screen_width &&
	VisibleY >= 0 && VisibleY < screen_height) {
	ToggleCursor (PSX(VisibleX), PSY(VisibleY));
	cursordown = !cursordown;
    }
}

CheckPS ()
{
    if (!PostScript)
	error ("Not connected to NeWS server");
}

DEFUN ("NeWS-map-window", FNeWS_map_window, SNeWS_map_window,
       0, 0, 0, "Make the NeWS window visible on the display.") ()
{
    CheckPS ();

    if (mapped)
	return;
    
    if (!positioned) {
	if (use_reshapefromuser)
	    ReshapeWindow ();
	else
	    InteractiveMove ();
	++positioned;
    }

    if (retainwindow)
	RetainWindow (retainwindow);

    MapWindow ();
    mapped = 1;
    StartInput ();

    setpgrp (0, getpid ());

    init_sigio ();			/* set up SIGIO handler */
    request_sigio ();
#ifdef F_SETOWN
    {
	extern int old_fcntl_owner;
	old_fcntl_owner = fcntl (0, F_GETOWN, 0);
	fcntl (0, F_SETOWN, getpid ());
    }
#endif F_SETOWN
    return Qnil;
}

DEFUN ("NeWS-set-font", FNeWS_set_font, SNeWS_set_font, 2, 2,
       "sFont: \nnSize: ", "Set NeWS font to FONT at SIZE points.") (font,size)
Lisp_Object font, size;
{
    CHECK_STRING (font, 1);
    CHECK_NUMBER (size, 2);
    if (XINT (size) < 1)
	args_out_of_range (font, size);

    CheckPS ();

    StartPS ();
    SetFont (XSTRING (font)->data, strlen (XSTRING (font)->data), size);
    /*
     * update window size so rows & cols don't change next time NeWS asks
     * for a repaint.
     */
    SetDimensions (screen_width, screen_height);
    EndPS ();
    return Qnil;
}

DEFUN ("NeWS-reshape-window", FNeWS_reshape_window,
       SNeWS_reshape_window, 0, 0, "",
       "Reshape the NeWS window interactively") ()
{
    CheckPS ();
    StartPS ();
    ReshapeWindow ();
    positioned++;
    EndPS ();
}

DEFUN ("NeWS-set-origin", FNeWS_set_origin, SNeWS_set_origin, 2, 2,
       "nX: \nnY: ", "Position NeWS window origin at X, Y.") (x, y)
Lisp_Object x, y;
{
    CHECK_NUMBER (x, 1);
    CHECK_NUMBER (y, 2);
    if (XINT (x) < 0 || XINT (y) < 0)
	args_out_of_range (x, y);
    CheckPS ();

    StartPS ();
    SetOrigin (XFASTINT (x), XFASTINT (y));
    positioned++;
    EndPS ();
    return Qnil;
}

DEFUN ("NeWS-set-dimensions", FNeWS_set_dimensions, SNeWS_set_dimensions, 2, 2,
       "nColumns: \nnRows: ", "Set window COLUMNS and ROWS") (cols, rows)
Lisp_Object cols, rows;
{
    CHECK_NUMBER (cols, 1);
    CHECK_NUMBER (rows, 2);
    if (XINT (cols) < 1 || XINT (cols) >= MScreenWidth ||
	XINT (rows) < 1 || XINT (cols) >= MScreenLength)
	args_out_of_range (cols, rows);
    CheckPS ();

    StartPS ();
    SetDimensions (XFASTINT (cols), XFASTINT (rows));
    EndPS ();
    return Qnil;
}

DEFUN ("NeWS-set-framelabel", FNeWS_set_framelabel, SNeWS_set_framelabel, 1, 1,
       "sFrame Label: ", "Set NeWS window frame label to STRING") (s)
Lisp_Object s;
{
    CHECK_STRING (s, 1);
    CheckPS ();

    StartPS ();
    SetFrameLabel (XSTRING (s)->data, XSTRING (s)->size);
    EndPS ();
    return Qnil;
}

DEFUN ("NeWS-set-option", FNeWS_set_option, SNeWS_set_option, 2, 2, 0,
       "Set OPTION's value to BOOLEAN.  Don't call this.") (option, value)
Lisp_Object option, value;
{
    CHECK_NUMBER (option, 1);
    CheckPS ();
    switch (XINT (option)) {
      case 1:
	line_ins_del_ok = !NULL (value);
	break;
      case 2:
	char_ins_del_ok = !NULL (value);
	break;
      case 3:
	scroll_region_ok = !NULL (value);
	if (!scroll_region_ok)
	    scrollregion = 0;
	break;
      case 4:
	retainwindow = !NULL (value);
	if (mapped) {
	    StartPS ();
	    RetainWindow (retainwindow);
	    EndPS ();
	}
	break;
      case 5:
	StartPS ();
	SetStuff (!NULL (value));
	EndPS ();
	break;
      case 6:
	use_reshapefromuser = !NULL (value);
	if (!mapped)
	    positioned = 0;		/* forget any previous positioning */
	break;
      default:
	args_out_of_range (option, value);
    }
    return Qnil;
}       

/*
 * Return a list representing the current input event:
 *
 *	(0 string)			; insert string
 * 	(1 string)			; execute string
 *	(3 rank size x y)		; set selection at ...
 *	(4 rank size x y)		; extend selection to ...
 */

DEFUN ("NeWS-next-event", FNeWS_next_event, SNeWS_next_event, 0, 0, 0,
       "Return a list describing the next available input event.") ()
{
    selectionevent *se;

    unrequest_sigio ();			/* lock out queue_event */
    se = &selectionevents[nextevent];
    if (eventcount-- < 1)
	return Qnil;

    switch (XFASTINT (se->list[0])) {
      case 0:				/* insert string */
      case 1:				/* execute string */
	se->list[1] = make_string (&sbuf[XFASTINT (se->list[1])],
				   XFASTINT (se->list[2]));
	sbufstart += XFASTINT (se->list[2]); /* chase tail of queue */
	if (sbufstart >= sbufend)
	    sbufstart = sbufend = 0;
	se->nelements = 2;
    }
    EINCR (nextevent);

    request_sigio ();			/* allow sigio interrupts again */
    return Flist (se->nelements, se->list);
}

DEFUN ("NeWS-set-selection", FNeWS_set_selection, SNeWS_set_selection, 3, 3, 0,
       "Set NeWS selection RANK, SIZE, and STRING.")
(rank, size, str)
Lisp_Object rank, size, str;
{
    struct Lisp_String *s;
    CHECK_NUMBER (rank, 1);
    CHECK_NUMBER (size, 2);
    CHECK_STRING (str, 3);
    s = XSTRING (str);
    
    if (XINT (rank) < 0 || XINT (size) < 0)
	args_out_of_range (rank, size);
    CheckPS ();
    StartPS ();
    SetSelection (XINT (rank), XINT (size), s->data, s->size);
    EndPS ();
    return Qnil;
}

DEFUN ("NeWS-send-PostScript", FNeWS_send_PostScript, SNeWS_send_PostScript,
       1, 2, 0,"Queue STRING for execution by Emacs' NeWS process.\n\
Optional second argument, if not NIL, indicates that execution leaves\n\
an object on the stack which should be \"typed\" back in.") (s, response)
Lisp_Object s, response;
{
    CHECK_STRING (s, 1);
    
    CheckPS ();
    StartPS ();
    SendPS (XSTRING (s)->data, XSTRING (s)->size, !NULL (response));
    EndPS ();
    return Qnil;
}

/*
 * This could be implemented in lisp, I suppose, but as long as we're here...
 */

DEFUN ("window-at", Fwindow_at, Swindow_at, 2, 2, 0,
       "Convert COLUMN and ROW into local window coordinates.\n\
Result is a list: (WINDOW LOCAL-COLUMN LOCAL-ROW), or NIL if the window is\n\
inactive.  Do not use this function in code intended to be portable.") (x, y)
Lisp_Object x, y;
{
    Lisp_Object window = selected_window;

    CHECK_NUMBER (x, 1);
    CHECK_NUMBER (y, 1);
    x = XINT (x);
    y = XINT (y);
    do {
	struct window *w = XWINDOW (window);
	int wx = w->left, wy = w->top;

	if (x >= wx && x < wx + w->width && y >= wy && y < wy + w->height) {
	    Lisp_Object list[3];

	    list[0] = window;
	    XFASTINT (list[1]) = x - wx;
	    XFASTINT (list[2]) = y - wy;
	    return Flist (3, list);
	}
    } while ((window = Fnext_window (window, Qnil)) != selected_window);

    return Qnil;
}

syms_of_NeWS ()
{
    defsubr (&SNeWS_map_window);
    defsubr (&SNeWS_reshape_window);
    defsubr (&SNeWS_set_font);
    defsubr (&SNeWS_set_origin);
    defsubr (&SNeWS_set_dimensions);
    defsubr (&SNeWS_set_framelabel);
    defsubr (&SNeWS_set_option);
    defsubr (&SNeWS_next_event);
    defsubr (&SNeWS_set_selection);
    defsubr (&SNeWS_send_PostScript);
    defsubr (&Swindow_at);
}

#endif HAVE_NEWS

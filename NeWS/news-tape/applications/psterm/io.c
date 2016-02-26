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
static char sccsid[] = "@(#)io.c 9.7 88/01/19 Copyright 1985 Sun Micro";
static	char RCSid[] = "@(#)$Header: io.c,v 2.1 88/10/04 04:22:45 eric Release $";
#endif


/*
 * Copyright (c) 1985 by Sun Microsystems, Inc. 
 */
#ifdef REF
#include <sys/types.h>
#include <ref/config.h>
#endif

#ifndef HAVE_SELECT
#ifdef INTERLANTCP
#include <interlan/il_errno.h>
#endif
#include <stropts.h>
#include <poll.h>
#endif
#include <stdio.h>
#include <psio.h>
#include <errno.h>
#include <fcntl.h>
#include <sys/time.h>

extern void perror();

#define	MAX(a,b)	((a)>(b)?(a):(b))
#define	KBDBUFSIZE	4096
#define	PTYBUFSIZE	8192

static	char kbdbuf[KBDBUFSIZE], *kbdfront, *kbdback, *kbdend;
static	char ptybuf[PTYBUFSIZE];	/* buffer for reading from pty slave */
static	int wfproto;			/* prototype for write select mask */
extern	int Mfd;
extern	int errno;
extern	PSFILE *PostScript;
extern	int PageMode;
extern	int PageFull;
extern	int fontisfixedwidth;
extern	int FastPaint;
extern	int DoScrolling;

static	int pty_out();
static	int kbd_input();

/*
 * Input dispatcher: take data from postscript
 * program and pty, dispatching each to the
 * appropriate handler.
 */
terminal(cfd, kfd)
{
    register int n;
#ifdef HAVE_SELECT
    int rf, wf;
    int max;
#else	/* !HAVE_SELECT */
#define PTY 0
#define KEYBOARD 1
#define POSTSCRIPT 2
#define NFDS 3
    struct pollfd rwf[NFDS];
    int i;
#endif	/* !HAVE_SELECT */
    int keyboard, pty, postscript, ndeferred;
    char *deferred;

    PageFull = 0;			/* not inhibiting scrolling */
    ndeferred = 0;			/* no deferred output */
    keyboard = 1<<kfd;
    pty = 1<<cfd;
    postscript = 1<<psio_fileno(PostScript);
#ifdef HAVE_FNDELAY
    (void) fcntl(cfd, F_SETFL, fcntl(cfd, F_GETFL, 0)|FNDELAY);
    (void) fcntl(kfd, F_SETFL, fcntl(kfd, F_GETFL, 0)|FNDELAY);
#else
    (void) fcntl(cfd, F_SETFL, fcntl(cfd, F_GETFL, 0)|O_NDELAY);
    (void) fcntl(kfd, F_SETFL, fcntl(kfd, F_GETFL, 0)|O_NDELAY);
#endif
#ifdef HAVE_SELECT
    max = MAX(kfd, cfd) + 1;
#endif
    kbdend = kbdbuf + KBDBUFSIZE; 
    kbdfront = kbdback = kbdbuf;
    wfproto = 0;
    for (;;) {
	/*
	 * Don't poll for input to be sent to the display
	 * if we have a full screen, or we are blocked already
	 * trying to transmit to the server.
	 */
#ifdef HAVE_SELECT
	rf = (PageFull || (wfproto & postscript)) ? 0 : pty;
	wf = wfproto;
	if (kbdfront != kbdend)		/* space to read from kbd */
	    rf |= keyboard;
	if (select(max, &rf, &wf, (int *)NULL, (struct timeval *)NULL) < 0) {
	    if (errno == EINTR)
		continue;
	    perror("select");
	    break;
	}
#else
	rwf[PTY].fd = -1;
	rwf[PTY].events = 0;
	rwf[KEYBOARD].fd = -1;
	rwf[KEYBOARD].events = 0;
	rwf[POSTSCRIPT].fd = -1;
	rwf[POSTSCRIPT].events = 0;

	if (!(PageFull || (wfproto & postscript))) {
		rwf[PTY].fd = cfd;
		rwf[PTY].events |= POLLIN;
	}

	if (wfproto & pty) {
		rwf[PTY].fd = cfd;
		rwf[PTY].events |= POLLOUT;
	}
	if (wfproto & postscript) {
		rwf[POSTSCRIPT].fd = psio_fileno(PostScript);
		rwf[POSTSCRIPT].events |= POLLOUT;
	}

	if (kbdfront != kbdend) {		/* space to read from kbd */
		rwf[KEYBOARD].fd = kfd;
		rwf[KEYBOARD].events |= POLLIN;
	}
	if (poll(rwf, NFDS, -1) < 0) {
	    if (errno == EINTR)
		continue;
	    perror("poll");
break_here:
	    break;
	}

	/* check to see if any connections were hung up */
	if (rwf[KEYBOARD].revents & POLLHUP || rwf[PTY].revents & POLLHUP
					|| rwf[POSTSCRIPT].revents & POLLHUP)
		break;

	/*  look for exceptional conditions on fd's */
	for (i = 0; i < NFDS; i++) {
		if (rwf[i].revents & (POLLERR | POLLNVAL)) {
			fprintf(stderr, "Error on an fd in poll ");
			fprintf(stderr, "[POLLERR | POLLNVAL]\n");
			goto break_here;	/* should be break, but C has */
						/* no multi-level break */
		}
	}
#endif
	/*
	 * Flush PostScript descriptor.
	 */
#ifdef HAVE_SELECT
	if (wf & postscript && (psio_flush(PostScript) == 0))
#else
	if (rwf[POSTSCRIPT].revents & POLLOUT && (psio_flush(PostScript) == 0))
#endif
	    wfproto &= ~postscript;
	/*
	 * Try to flush pty, if clogged, before reading from keyboard.
	 */
#ifdef HAVE_SELECT
	if (wf & pty)
#else
	if (rwf[PTY].revents & POLLOUT)
#endif
	{
	    wfproto &= ~pty;
	    pty_out();
	}
	/*
	 * Take keyboard input.
	 */
#ifdef HAVE_SELECT
	if (rf & keyboard)
#else
	if (rwf[KEYBOARD].revents & POLLIN)
#endif
	{
	    errno = 0;
	    n = read(kfd, kbdfront, (unsigned)(kbdend-kbdfront));
	    if (n <= 0) {
		if (errno != EWOULDBLOCK) {
#ifdef HAVE_SELECT	/* Connection Reset is checked above in SYSVREF */
		    if (n < 0 && errno != ECONNRESET)
#endif
			    perror("keyboard");
		    break;
		}
	    } else if (n > 0)
		kbd_input(n);
	}
	/*
	 * If pty_out or kbd_input changed
	 * PageFull, loop to inhibit output.
	 */
	if (PageFull)			/* loop if inhibiting output */
	    continue;
	/*
	 * If screen output was blocked due to previous PageFull condition,
	 * resume it.  Note that this may leave us back in PageFull mode,
	 * and interrupted tcap ops may still need the chars in ptybuf.
	 */
#ifdef lint
	deferred = (char *)0 ;
#endif
	if (ndeferred) {
	    n = tc_display((u_char *)deferred, ndeferred);
	    if (n > 0)
		deferred += (ndeferred - n);
	    ndeferred = n;
	    tc_refresh(0);
	    if (PageFull)		/* if PageFull, can't touch ptybuf */
		continue;
	}
	/*
	 * Finally, take pty stuff and send it to the display
	 * except when we're already flush with output.
	 */
#ifdef HAVE_SELECT
	if ((wfproto & postscript) == 0 && rf & pty)
#else
	if ((wfproto & postscript) == 0 && rwf[PTY].revents & POLLIN)
#endif
	{
	    errno = 0;
	    n = read(cfd, ptybuf, PTYBUFSIZE);
#ifdef HAVE_SELECT
	    if (n < 0) {
		if (errno != EIO) {
/* BEGIN SUN BUG WORKAROUND */
			if (errno == EWOULDBLOCK)
			    continue;
/* END SUN BUG WORKAROUND */
			perror("pty");
		}
		break;
	    }
#else
	    if (n == 0)
		continue;
	    if (n < 0) {
		if (errno == EAGAIN)
			continue;
		perror("pty");
		break;
	    }
#endif
	    ndeferred = tc_display((u_char *)ptybuf, n);
	    if (ndeferred)
		deferred = ptybuf + (n - ndeferred);
	    tc_refresh(0);
	}
#ifdef notdef
if (wfproto & postscript) fprintf(stderr, "blocked\n");
#endif
    }
}

/*
 * Flush PostScript destined for window.
 * If all data was not written, then mark
 * the select mask to find out when data
 * has been flushed to the network.
 */
FlushPostScript()
{

    if (!(psio_flush(PostScript) == 0))
	wfproto |= 1<<psio_fileno(PostScript);
}

/*
 * Flush output to pty.
 */
static
pty_out()
{
	register int cc;

	if ((cc = kbdfront - kbdback) > 0) {
	    if (PageMode) {
		if (PageFull) {
		    switch (*kbdback) {
		    case '\r':
			--cc, kbdback++;
			scrollreset(1);
			break;
		    case ' ':
			--cc, kbdback++;
			/* fall thru... */
		    default:
			scrollreset(0);
			break;
		    }
		    if (cc < 1) {
			kbdfront = kbdback = kbdbuf;
			return;
		    }
		} else
		    scrollreset(0);
	    }
	    cc = write(Mfd, kbdback, (unsigned)cc);
	    if (cc < 0) {
		if (errno != EWOULDBLOCK)
		    perror("master pty");
	    } else
		kbdback += cc;
	}
	if (kbdfront != kbdback)
	    wfproto |= 1<<Mfd;		/* must explicity reenable */
	else
	    kbdfront = kbdback = kbdbuf;
}

#define EVENT_ESCAPE		0200	/* introduce event packet */

#define EVENT_REPAIR		'a'	/* window resized or damaged */
#define	EVENT_BEGINSEL		'b'	/* set selection start */
#define	EVENT_EXTSEL		'c'	/* drag end of current selection */
#define	EVENT_ENDSEL		'd'	/* complete selection */
#define EVENT_TOGGLEAM		'e'	/* toggle auto-margins */
#define EVENT_TOGGLEPM		'f'	/* toggle page mode */
#define EVENT_ROWCOL		'g'	/* new row/col size selected */
#define	EVENT_FONTFIXED		'h'	/* just switched to a fixed width font */
#define EVENT_FONTVAR		'i'	/* just switched to a variable width font */
#define	EVENT_RECTSEL		'j'	/* set rect selection start */
#define EVENT_EXTRECT		'k'	/* extend rect selection */
#define EVENT_SCROLL		'l'	/* scrollbar movement report */
#define EVENT_SAVESIZE		'm'	/* change number of lines saved */
#define EVENT_ENDREFRESH	'n'	/* postscript side has completed refresh */
#define EVENT_FASTPAINT		'o'	/* set or clear fastpaint */
#define EVENT_DOSCROLL		'p'	/* set or clear use of scrolling */

/*
 * Handle keyboard input + postscript events.
 */
static
kbd_input(n)
register int n;
{
	register unsigned char *p, *d;
	register n2;
	extern int RefreshSuppressed;
	int	col, row, rank, dorefresh = 0, doresize = 0, rect = 0 ;

#define NEXTVAL		while (n-- > 0 && *p++ != '\n') ;

	for (p = (unsigned char *)kbdfront, n2 = 0, d = p; n > 0;) {
		if (*p == EVENT_ESCAPE) {
			p++ ;
			if (--n > 0) {
				n-- ;
				switch (*p++) {
				case EVENT_TOGGLEAM:
					toggleautomargins();
					break;
				case EVENT_TOGGLEPM:
					togglepagemode();
					if (PageFull) {
						/* resume output if blocked */
						scrollreset(0);
						dorefresh++;
						}
					break;
				case EVENT_REPAIR:
					RefreshSuppressed = 0 ;
					doresize++ ;
					break;
				case EVENT_RECTSEL:
					rect = 1 ;	/* fall through... */
				case EVENT_BEGINSEL:
					/* col row rank size mode preview */
					if (n>0) { col = atoi((char *)p); NEXTVAL; }
					if (n>0) { row = atoi((char *)p); NEXTVAL; }
					if (n>0) {
						rank = atoi((char *)p); NEXTVAL;
						tc_deselect(rank);
						tc_extend_selection(col, row,
								rank, rect);
						dorefresh++;
						rect = 0 ;
						}
					break;
				case EVENT_EXTRECT:
					rect = 1 ;	/* fall through... */
				case EVENT_EXTSEL:
					/* col row rank size mode preview */
					if (n>0) { col = atoi((char *)p); NEXTVAL; }
					if (n>0) { row = atoi((char *)p); NEXTVAL; }
					if (n>0) {
						rank = atoi((char *)p); NEXTVAL;
						tc_extend_selection(col, row,
								rank, rect);
						dorefresh++;
						rect = 0 ;
						}
					break;
				case EVENT_ENDSEL:
					/* col row rank size mode preview */
					if (n>0) { col = atoi((char *)p); NEXTVAL; }
					if (n>0) { row = atoi((char *)p); NEXTVAL; }
					if (n>0) {
						rank = atoi((char *)p); NEXTVAL;
						tc_extend_selection(col, row,
								rank, rect);
						tc_report_selection(rank);
						dorefresh++;
						}
					break;
				case EVENT_ESCAPE:
					*d++ = EVENT_ESCAPE ;
					n2++ ;
					break;
				case EVENT_ROWCOL:
					if (n>0) { col = atoi((char *)p); NEXTVAL; }
					if (n>0) {
						row = atoi((char *)p); NEXTVAL;
						change_rowcol(row, col);
						}
					break;
				case EVENT_FONTFIXED:
					fontisfixedwidth = 1 ;
					break;
				case EVENT_FONTVAR:
					fontisfixedwidth = 0 ;
					break;
				case EVENT_SCROLL:
					if (n>0) {
						row = atoi((char *)p); NEXTVAL;
						scroll_to(row);
						dorefresh++;
						}
					break;
				case EVENT_SAVESIZE:
					if (n>0) {
						row = atoi((char *)p); NEXTVAL;
						set_scroll_save(row);
						dorefresh++;
						}
					break;
				case EVENT_ENDREFRESH:
					end_refresh();
					dorefresh++;
					break;
				case EVENT_FASTPAINT:
					if (n>0) {
						FastPaint = atoi((char *)p); NEXTVAL;
						}
					break;
				case EVENT_DOSCROLL:
					if (n>0) {
						DoScrolling = atoi((char *)p); NEXTVAL;
						}
					break;
					}
				}
			}
		else	{
			*d++ = *p++ ;
			n-- ;
			n2++ ;
			}
		}
	if (doresize)
		do_display_resize();
	if (n2 > 0)
		kbdfront += n2;
	if (kbdfront != kbdback) {
		tc_deselect(1);
		pty_out();
		}
	if (dorefresh)
		tc_refresh(0);
}

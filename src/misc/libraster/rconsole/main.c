/*
 * rconsole
 * Copyright 1990 Tom Lawrence, Brown University
 * Last Modification 6/14/90
 */

#include <sys/types.h>
#include <sys/file.h>
#include <ctype.h>
#include <signal.h>
#include <errno.h>
#include <stdio.h>

#include <sundev/kbd.h>
#include <sundev/kbio.h>

#include "global.h"
#include "ascii.h"
#include "get_char.h"

#include "myfbdevice.h"
static struct fbdevice *myfbdevicep;

void rcons_init();

extern void rcons_puts();
extern void rcons_cursor();
extern void rcons_font();
extern void rcons_clear2eop();

extern void kbd_mode();
extern void setup_utmp();

extern tty_pair init_tty();
tty_pair main_tty;
int errno;

/*****************************************************************************
handle input from the keyboard. Keystrokes are simply passed on to the
rconsole pty.
*****************************************************************************/
void
handle_kbd_input(pty)
	int pty;
{
	char buf[128];
	int size;

	if ((size = read(0, buf, sizeof(buf))) < 1) {
		/* 
		 * This should be done with blocking i/o, but for some reason
		 * it doesn't work under SunOS 4.1 unless in non-blocking mode
		 */
		if (errno != EWOULDBLOCK) {
			perror("rconsole: handle_kbd_input: read");
			exit(1);
		} else
			return;
	}
	if (write(pty, buf, size) != size) {
		perror("rconsole: write");
		exit(1);
	}
}


/*****************************************************************************
handle output from the rconsole pty. get_char() is called initially in
a mode which fills its buffer but returns nothing useful. It is then
called in a loop in such a manner that it returns data until its
buffer is empty; it then returns an error code and this routine exits.
Printable chars are buffered up to the right edge of the screen and
then sent to print_string(). If an ESC is encountered in the output,
the ESC sequence handler is called.  Other non printable chars are
handled separately.
*****************************************************************************/
void
handle_pty_output(pty)
	int pty;
{
	int c, n;
	char buf[100];
	register char *cp;
	register int row = myfbdevicep->fb_maxrow, col = myfbdevicep->fb_maxcol;

	if (get_char(pty, READ | NBIO, row, col) < 0)
		return;

	do {
		n = 0;
		cp = buf;

		do {
			if ((c = get_char(pty, RET_DATA, row, col)) != -1) {

				/* buffer printables up to the right edge of
				 * the screen */

				*cp++ = c;
				++n;
			}
		}

		/* until no more chars to read, a non-printable was reached, or
		 * we've run out of space on this line */

		while (c != -1 && n < (sizeof(buf) - 1));
		/* print what we've got so far */

		rcons_puts(myfbdevicep, buf, n);

	} while (c != -1);
}


/*****************************************************************************
This routine is called when the child shell exits. We clear the utmp
entry and exit.
*****************************************************************************/
void
byebye()
{
	FILE *f;
	char *tty = "/dev/tty";

	rcons_cursor(myfbdevicep);		/* erase cursor */
	setup_utmp(main_tty.tty, 1);

	/* Open tty to clean things up a bit */
	if ((f = fopen(tty, "w")) == 0) {
		fprintf(stderr, "rconsole: byebye fopen");
		perror(tty);
		exit(1);
	}

#ifdef notdef
	/* XXX can't do this because there's no way to erase the prom cursor */
	/* Cursor prom emulator to where were were in the graphics plane */
	fprintf(f, "\033[%d;%dH",
	    *myfbdevicep->fb_row + 1, *myfbdevicep->fb_col + 1);
#else
	/* Erase stuff left in graphics plane... */
	fprintf(f, "\033[0J");
#endif
	fflush(f);

	exit(0);
}

/*****************************************************************************
*****************************************************************************/
main(argc, argv)
	int argc;
	char **argv;
{
	fd_set fdset;

	/* parse options */
	options(argc, argv);

	/* bad things happen if this is run on top of X or sunview */
	if (!debug && strcmp("/dev/console", ttyname(0))) {
		fputs("rconsole can only be run on the system console.\n",
		    stderr);
		exit(1);
	}

	/* set up the window, pty/tty, screen and utmp entry */
	rcons_init();
	main_tty = init_tty(myfbdevicep->fb_maxrow, myfbdevicep->fb_maxcol);

	setup_utmp(main_tty.tty, 0);

	/* watch for the child shell exiting */
	signal(SIGCHLD, byebye);
	signal(SIGINT, byebye);
	signal(SIGHUP, byebye);

	/*
	 * Arrange for keyboard strokes to go to /dev/kbd instead of
	 * /dev/console and attach stdin to /dev/kbd
	 */
	kbd_mode();

	/* manage data flow */
	while (1) {
		FD_ZERO(&fdset);
		FD_SET(main_tty.pty, &fdset);
		FD_SET(0, &fdset);

		if (select(getdtablesize(), &fdset, 0, 0, 0) < 1) {
			if (errno == EINTR)
				continue;
			perror("rconsole: select");
			exit(1);
		}
		if (FD_ISSET(main_tty.pty, &fdset))
			handle_pty_output(main_tty.pty);
		if (FD_ISSET(0, &fdset))
			handle_kbd_input(main_tty.pty);
	}
}

/* Ring the console bell */
void
rcons_bell(fb)
	register struct fbdevice *fb;
{
	register int x;
	int i;

	if (fb->fb_bits & FB_VISBELL) {
		/* invert the screen twice */
		for (x = 0; x < 2; x++)
			raster_op(fb->fb_sp, 0, 0,
			    fb->fb_sp->width, fb->fb_sp->height,
			    RAS_INVERT, (struct raster *) 0, 0, 0);
	}

	/* fd 0 refers to /dev/kbd */
	i = KBD_CMD_BELL;
	if (ioctl(0, KIOCCMD, &i) < 0) {
		perror("rconsole: KBD_CMD_BELL");
		exit(1);
	}
	usleep(100000);

	i = KBD_CMD_NOBELL;
	if (ioctl(0, KIOCCMD, &i) < 0) {
		perror("rconsole: KBD_CMD_NOBELL");
		exit(1);
	}
}

void
rcons_init()
{
	register struct fbdevice *fb;
	register struct fbtype *ft;
	struct winsize winsize;
	static struct fbdevice myfbdevice;	/* XXX fake fbdevice struct */
	register int fd;
	static int row, col;
	register int i;
	register struct raster *rp;
	char *cons = "/dev/console";

	fb = myfbdevicep = &myfbdevice;
	ft = &fb->fb_type;
	memset((char *)fb, 0, sizeof(*fb));
	fb->fb_bits = FB_VISBELL;

	fb->fb_row = &row;
	fb->fb_col = &col;
	row = col = 0;

	if ((fd = open(cons, O_RDONLY)) == 0) {
		perror(cons);
		exit(1);
	}
	if (ioctl(fd, TIOCGWINSZ, &winsize) < 0) {
		perror("rconsole: TIOCGWINSZ");
		exit(1);
	}
	(void)close(fd);
	fb->fb_maxrow = winsize.ws_row;
	fb->fb_maxcol = winsize.ws_col;

	if ((rp = raster_open(FB)) == 0) {
		fputs("rconsole: open: ", stderr);
		perror(FB);
		exit(1);
	}
	/* XXX mostly duplicates of data in other places */
	fb->fb_sp = rp;
	fb->fb_type.fb_width = rp->width;
	fb->fb_type.fb_height = rp->height;

	fb->fb_ras_blank = RAS_CLEAR;

	/* Setup the static font */
	rcons_font(fb);

	/* Center emulator screen (but align x origin to 32 bits) */
	fb->fb_xorigin =
	    ((ft->fb_width - fb->fb_maxcol * fb->fb_font->width) / 2) & ~0x1f;
	fb->fb_yorigin =
	    (ft->fb_height - fb->fb_maxrow * fb->fb_font->height) / 2;

	/* Emulator width and height used for scrolling */
	fb->fb_emuwidth = fb->fb_maxcol * fb->fb_font->width;
	if (fb->fb_emuwidth & 0x1f) {
		/* Pad to 32 bits */
		i = (fb->fb_emuwidth + 0x1f) & ~0x1f;
		/* Make sure emulator width isn't too wide */
		if (fb->fb_xorigin + i <= ft->fb_width)
			fb->fb_emuwidth = i;
	}
	fb->fb_emuheight = fb->fb_maxrow * fb->fb_font->height;

	/* Clear the display */
	rcons_clear2eop(fb);

	/* Draw the initial cursor */
	rcons_cursor(fb);
}

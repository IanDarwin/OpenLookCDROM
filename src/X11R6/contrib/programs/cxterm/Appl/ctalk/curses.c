/*
 * A tiny version of cureses library.  The original BSD curses library
 * uses 7-bit only.
 *
 * Yongguang Zhang		ygz@cs.purdue.edu
 */

#include "curses.h"

#include <signal.h>
extern char *getenv();
extern char *tgetstr();

int  LINES, COLS;
char *TI, *TE, *CE, *CL, *CM, *BC;
char erase_char, kill_char, werase_char;
WINDOW *curscr = NULL;

#define	encurse()	(rawmode(1),_puts(TI),fflush(stdout))
#define	decurse()	(_puts(TE),fflush(stdout),rawmode(0))

static int _tty_ch = 1;
static int need_redraw = 0;
static WINDOW *allwin[16];
static int nallwin;

int sigtstp();
int sigwinch();

WINDOW *
initscr ()
{
	nallwin = 0;
	if (! isatty(_tty_ch))
		_tty_ch = 2;
	getterm();
	encurse();
#ifdef	SIGTSTP
	signal(SIGTSTP, sigtstp);
#endif
#ifdef	SIGWINCH
	signal(SIGWINCH, sigwinch);
#endif
}

endwin ()
{
	decurse();
}

getterm()
{
	char genbuf[2048];
	static char sbuf[2048];
	char *sp = sbuf;
	int dumb = 0;
#ifdef TIOCGWINSZ
	struct winsize win;
#endif

	if (tgetent(genbuf, getenv("TERM")) <= 0)
		dumb = 1;

	LINES = tgetnum("li");
	COLS = tgetnum("co");
#ifdef TIOCGWINSZ
	if (ioctl(_tty_ch, TIOCGWINSZ, (char *)&win) >= 0) {
		if (win.ws_row)
			LINES = win.ws_row;
		if (win.ws_col)
			COLS = win.ws_col;
	}
#endif

	if (dumb || LINES < 0 || tgetflag("hc")) {
		/* terrible, hardcopy or dumb terminal */
		LINES = 24;
		COLS = 80;
	}

	TI = tgetstr("ti", &sp);
	if (! TI)  TI = "";
	TE = tgetstr("te", &sp);
	if (! TE)  TE = "";
	CE = tgetstr("ce", &sp);
	if (! CE)  CE = "";
	CL = tgetstr("cl", &sp);
	if (! CL)  CL = "";
	CM = tgetstr("cm", &sp);
	if (! CM)  CM = "";
	if (dumb || tgetflag("bs"))
		BC = "\b";
	else
		BC = tgetstr("bc", &sp);
}

#ifdef USE_TERMIO
rawmode(on)
     int on;
{
  struct termio tio;
  static struct termio save_tio;
#ifdef	TIOCGLTC
  struct ltchars ltc;
#endif

	if (on) {
		ioctl(_tty_ch, TCGETA, (char *)&tio);
		save_tio = tio;
		erase_char = tio.c_cc[VERASE];
		kill_char = tio.c_cc[VKILL];
		tio.c_lflag &= ~(ICANON|ECHO|ECHOE|ECHOK|ECHONL);
		tio.c_oflag |=  (OPOST|ONLCR|TAB3);
		tio.c_oflag &= ~(OCRNL|ONOCR|ONLRET);
		tio.c_cflag |= CS8;
		tio.c_cflag &= ~(PARENB);
#ifdef	ISTRIP
		tio.c_iflag &= ~(ISTRIP);
#endif
		tio.c_cc[VMIN] = 1;
		tio.c_cc[VTIME] = 0;
		ioctl(_tty_ch, TCSETAW, (char *)&tio);

#ifdef	TIOCGLTC
		if (ioctl(_tty_ch, TIOCGLTC, (char *)&ltc) >= 0)
			werase_char = ltc.t_werasc;
		else
			werase_char = -1;	/* no werase */
#else  /* ! TIOCGLTC */
		werase_char = -1;	/* no werase */
#endif
	} else {
		ioctl(_tty_ch, TCSETAW, (char *)&save_tio);
	}
}
#else	/* ! USE_TERMIO */
rawmode(on)
     int on;
{
	struct sgttyb sg;
	struct ltchars ltc;
	unsigned lmode;
	static struct sgttyb save_sg;
	static unsigned save_lmode;
	
	if (on) {
		ioctl(_tty_ch, TIOCGETP, (char *)&sg);
		save_sg = sg;
		erase_char = sg.sg_erase;
		kill_char = sg.sg_kill;
		sg.sg_flags |= CBREAK;
		sg.sg_flags &= ~(ECHO|XTABS);
		ioctl(_tty_ch, TIOCSETN, (char *)&sg);

		ioctl(_tty_ch, TIOCGLTC, (char *)&ltc);
		werase_char = ltc.t_werasc;

		ioctl(_tty_ch, TIOCLGET, (char *)&lmode);
		save_lmode = lmode;
#ifdef LPASS8
		lmode |= LPASS8;
#endif
		ioctl(_tty_ch, TIOCLSET, (char *)&lmode);
	} else {
		ioctl(_tty_ch, TIOCSETN, (char *)&save_sg);
		ioctl(_tty_ch, TIOCLSET, (char *)&save_lmode);
	}
}
#endif	/* USE_TERMIO */

WINDOW *
newwin(nrows,ncols, begy,begx)
     int nrows,ncols,begy,begx;
{
  WINDOW *win = (WINDOW *)malloc(sizeof(WINDOW));

	win->w = ncols;  win->h = nrows;
	win->w_x = begx;  win->w_y = begy;
	win->x = 0;  win->y = 0;
	allwin[nallwin++] = win;
	return (win);
}

waddch(win,ch)
     WINDOW *win;
     unsigned char ch;
{
  int sp;

	if (win->x >= win->w)
		return;

	curwin(win);
	switch (ch) {
	  case '\t':
		sp = 8 - (win->x & 7);
		while (sp-- > 0) {
			if (win->x >= win->w)
				return;
			win->arr[win->y][(win->x)++] = ' ';
			_putchar(' ');
		}
		break;
	  case '\n':
		wclrtoeol(win);
		wmove(win, win->y+1, 0);
		break;
	  case '\r':
		wmove(win, win->y, 0);
		break;
	  case '\b':
		if (win->x > 0) {
			win->x-- ;
			if (BC && *BC)
				_puts (BC);
			else
				wmove(win, win->y, win->x);
		}
		break;
	  default:
		win->arr[win->y][(win->x)++] = ch;
		_putchar(ch);
	}
}

waddstr(win,str)
     WINDOW *win;
     unsigned char *str;
{
	while (*str)
		waddch(win, *str++);
}

wcerase(win)
     WINDOW *win;
{
	if (win->x == 0)
		return;
	win->arr[win->y][--(win->x)] = ' ';
	if (BC && *BC) {
		curwin(win);
		_puts (BC);
		_putchar(' ');
		_puts (BC);
	} else {
		wmove(win, win->y, win->x);
		_putchar(' ');
		wmove(win, win->y, win->x);
	}
}

wmove(win, y, x)
     WINDOW *win;
     short y, x;
{
	win->y = y;  win->x = x;
	curscr = win;
	_puts(tgoto(CM, win->x+win->w_x, win->y+win->w_y));
}

wclear(win)
     WINDOW *win;
{
  register short i;

	for (i = 0; i < win->h; i++) {
		wmove(win, i, 0);
		wclrtoeol(win);
	}
	wmove(win, 0, 0);
}

wredraw(win)
     WINDOW *win;
{
  register short j, i;
  short oldy, oldx;

	getyx(win, oldy, oldx);
	for (j = 0; j < win->h; j++) {
		wmove(win, j, 0);
		for (i = 0; i < win->w; i++)
			_putchar(win->arr[j][i]);
	}
	wmove(win, oldy, oldx);
}

refresh()
{
	if (need_redraw)
		redraw();
	fflush(stdout);
}

wclrtoeol(win)
     WINDOW *win;
{
  register int i;

	for (i = win->x; i < win->w; i++)
		win->arr[win->y][i] = ' ';
	curwin(win);
	if (CE && *CE)
		_puts(CE);
	else {
		for (i = win->x; i < win->w; i++)
			_putchar(' ');
		wmove(win, win->y, win->x);
	}
}

wfill(win, ch)
     WINDOW *win;
     char ch;
{
  register short j, i;
  short oldy, oldx;

	getyx(win, oldy, oldx);
	for (j = 0; j < win->h; j++) {
		wmove(win, j, 0);
		for (i = 0; i < win->w; i++) {
			win->arr[j][i] = ch;
			_putchar(ch);
		}
	}
	wmove(win, oldy, oldx);
}

# include	<varargs.h>
/* VARARGS2 */
wprintw(win,fmt,va_alist)
    WINDOW *win;
    char *fmt;
    va_dcl
{
	va_list ap;
	char buf[256];

	va_start(ap);
	vsprintf(buf, fmt, ap);
	waddstr(win, buf);
}

redraw()
{
  int i;
  WINDOW *savscr = curscr;

	for (i = 0; i < nallwin; i++)
		wredraw (allwin[i]);
	curwin (savscr);
	need_redraw = 0;
}

curwin(win)
    WINDOW *win;
{
	if (win != curscr) {
		curscr = win;
		wmove (curscr, curscr->y, curscr->x);
	}
}
		
_putchar(c)
     char c;
{
	putchar(c);
}

sigtstp()
{
#ifdef SIGTSTP
  int omask;

	move(LINES-1,0);
	decurse();
	signal(SIGTSTP, SIG_DFL);
	omask = sigsetmask(sigblock(0) & ~sigmask(SIGTSTP));
	kill(0, SIGTSTP);	/* process stop here, and resume here */
	sigblock(sigmask(SIGTSTP));
	encurse();
	signal(SIGTSTP, sigtstp);
	clear();
	need_redraw = 1;
	refresh();
#endif
}

sigwinch()
{
#ifdef SIGWINCH
  short oldLINES, oldCOLS;

	oldLINES = LINES;
	oldCOLS = COLS;
	getterm ();
	if (LINES != oldLINES || COLS != oldCOLS) {
		clear ();
		need_redraw = 1;
	}
	refresh();
#endif
}

/*
 * A tiny version of cureses library.  The original BSD curses library
 * uses the 8th bit for other purpose.
 *
 * Yongguang Zhang		ygz@cs.purdue.edu
 */

#include <stdio.h>
#include <sys/ioctl.h>

#ifdef SYSV
# define USE_TERMIO
# include <sys/termio.h>
# ifdef S5WINSIZE
#  include <sys/stream.h>
#  include <sys/ptem.h>
# else
#  undef TIOCGWINSZ			/* unable to process */
# endif
#else
# include <sgtty.h>
#endif

#define	MAX_WIDTH	256
#define MAX_HEIGHT	128

struct _win_st {
	short		w, h;		/* width, height of the window */
	short		w_x, w_y;	/* window location in the screen */
	short		x, y;		/* cursor location in the window */
	unsigned char	arr[MAX_HEIGHT][MAX_WIDTH];
};

#define	WINDOW	struct _win_st

extern int	LINES, COLS;
extern char	*TI, *TE, *CE, *CL, *CM, *BC;
extern char	erase_char, kill_char, werase_char;
extern WINDOW	*curscr;

extern _putchar();
extern WINDOW	*newwin();

#define	_puts(s)	tputs(s, 0, _putchar)

#define clear()		_puts(CL)
#define move(y,x)	_puts(tgoto(CM,(x),(y)))
#define getyx(w,cy,cx)	(cy=(w)->y, cx=(w)->x)
#define winchyx(w,y,x)	((w)->arr[y][x])
#define wrefresh(w)	refresh()

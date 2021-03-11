/*
 *	display.c
 */

# include	"reversi.h"
# include	<curses.h>
# include	<ctype.h>

# define	toscrx(x)	((x) * 5 - 1)
# define	toscry(y)	((y) * 3 - 2)

# define	LINEX	(toscrx(10)-2)
# define	LINEY	(toscry(5)+1)

static	helpShown;

dispInit ()
{
	register int	i, j;
	int		dispEnd();

	initscr ();
	savetty();
	noecho ();
	crmode ();
	helpShown = 0;
	for (i = 1; i <= SIZE; i++) {
		move (toscry(i), 0);
		printw ("%c", i-1+'a');
		move (toscry(i), toscrx(SIZE + 1));
		printw ("%c", i-1+'a');
		move (0, toscrx(i));
		printw ("%1d", i);
	}
	if (_tty.sg_ospeed > B1200)
		dispGrid();
	else
		refresh ();
}

dispGrid ()
{
	register int	i, j;

	for (i = 1; i <= SIZE; i++) {
		for (j = 1; j <= SIZE + 1; j++) {
			if (i <= SIZE) {
				move (toscry(i)+1, toscrx(j)-2);
				addch ('|');
				move (toscry(i), toscrx(j)-2);
				addch ('|');
				move (toscry(i)-1, toscrx(j)-2);
			}
			if (j <= SIZE) {
				if (i == 1)
					printw ("+-%1d--", j);
				else
					addstr ("+----");
			} else
				addstr ("+");
		}
	}
	refresh ();
}

dispNoGrid ()
{
	register int	i, j;

	for (i = 1; i <= SIZE; i++) {
		for (j = 1; j <= SIZE + 1; j++) {
			move (toscry(i)+1, toscrx(j)-2);
			addch (' ');
			move (toscry(i), toscrx(j)-2);
			addch (' ');
			move (toscry(i)-1, toscrx(j)-2);
			if (j <= SIZE) {
				if (i == 1)
					printw ("  %1d  ", j);
				else
					addstr ("     ");
			} else
				addstr (" ");
		}
	}
	refresh ();
}

dispEnd ()
{
	clearok(stdscr, 1);
	erase ();
	refresh ();
	resetty();
	endwin ();
	exit (0);
}

boardT	old;

display (board)
boardT	board;
{
	register int	i,j;
	extern int	showScore;

	for (i = 1; i <= SIZE; i++)
		for (j = 1; j <= SIZE; j++)
			if (board[i][j] != old[i][j]) {
				dispOne (i, j, board[i][j]);
				old[i][j] = board[i][j];
			}
	refresh ();
	if (showScore)
		dispScore (board);
}

dispOne (x, y, who)
{
	move (toscry (y), toscrx (x));
	switch (who) {
	case BLACK:
		addstr ("/\\");
		break;
	case WHITE:
		addstr ("**");
		break;
	case EMPTY:
		addstr ("  ");
		break;
	}
	move (toscry(y) + 1, toscrx (x));
	switch (who) {
	case BLACK:
		addstr ("\\/");
		break;
	case WHITE:
		addstr ("**");
		break;
	case EMPTY:
		addstr ("  ");
		break;
	}
}

dispScore (board)
register boardT	board;
{
	register int	i,j;
	register int	ws, bs;

	ws = bs = 0;
	for (i = 1; i <= SIZE; i++)
		for (j = 1; j <= SIZE; j++)
			switch (board[i][j]) {
			case WHITE:
				ws++; break;
			case BLACK:
				bs++; break;
			}
	move (LINEY - 3, LINEX);
	printw ("white: %-2d  black: %-2d", ws, bs);
	refresh ();
}

dispNoScore ()
{
	move (LINEY - 3, LINEX);
	clrtoeol ();
	refresh ();
}

static char *helpText[] = {
	"y, x              [no] grid",
	"[no] help         hint",
	"play              quit",
	"restart           record",
	"replay            save",
	"[no] score        undo",
	"level",
	"white|black first",
	"white|black second",
	0,
};

dispTurn (player)
{
	static displayed = EMPTY;

	if (displayed == player)
		return;
	move (LINEY-1, LINEX);
	switch (player) {
	case WHITE:
		addstr ("white's turn");
		break;
	case BLACK:
		addstr ("black's turn");
		break;
	case EMPTY:
		clrtoeol ();
	}
	displayed = player;
	refresh ();
}

dispHelp ()
{
	register int	i;
	register char	**h;

	if (helpShown)
		return;
	i = 0;
	for (h = helpText; *h; ++h) {
		move (i, LINEX);
		addstr (*h);
		++i;
	}
	move (LINEY+4, LINEX);
	printw ("white pieces are  **");
	move (LINEY+5, LINEX+18);
	printw ("**");
	move (LINEY+7, LINEX);
	printw ("black pieces are  /\\");
	move (LINEY+8, LINEX+18);
	printw ("\\/");
	refresh ();
	++helpShown;
}

dispNoHelp ()
{
	register int	i;
	register char	**h;

	if (!helpShown)
		return;
	i = 0;
	for (h = helpText; *h; ++h) {
		move (i, LINEX);
		clrtoeol ();
		++i;
	}
	move (LINEY+4, LINEX);
	clrtoeol ();
	move (LINEY+5, LINEX+18);
	clrtoeol ();
	move (LINEY+7, LINEX);
	clrtoeol ();
	move (LINEY+8, LINEX+18);
	clrtoeol ();
	refresh ();
	helpShown = 0;
}

static char	lexbuf[256];
static char	*lexpnt;

readLine ()
{
	int	ch, x, y;

	move (LINEY, LINEX);
	addstr ("-> ");
loop:
	x = LINEX+3;
	y = LINEY;
	move (y, x);
	clrtoeol ();
	refresh ();
	lexpnt = lexbuf;
	for (;;) {
		ch = getch ();
		if (ch == -1)
			ch = '\004';
		*lexpnt++ = ch;
		if (isprint (ch)) {
			addch (ch);
			++x;
			refresh ();
		} else
			switch (ch) {
			case '\f':
				clearok (stdscr, 1);
			case '\030':
			case '\025':
				goto loop;
			case '\004':
				*lexpnt++ = -1;
			case '\r':
			case '\n':
				move (LINEY+1, LINEX);
				refresh ();
				*lexpnt++ = '\0';
				goto done;
			case '\b':
				if (lexpnt >= lexbuf + 2) {
					lexpnt -= 2;
					--x;
					move (y,x);
					delch ();
					refresh ();
				} else
					--lexpnt;
				break;
			default:
				--lexpnt;
				write (1, "\007", 1);
				break;
			}
	}
done:	lexpnt = lexbuf;
	dispError ("");
}

lexgetc ()
{
	int	c;
	extern int yylineno;

	c = *lexpnt++;
	if (c == -1)
		c = 4;
	c &= 0177;
	if (c == '\r')
		c = '\n';
	if (c == '\n')
		++yylineno;
	return c;
}

lexungetc (c)
{
	--lexpnt;
}

dispError (s)
char *s;
{
	move (LINEY+1, LINEX);
	clrtoeol ();
	addstr (s);
	refresh ();
}

dispMove (x, y, player)
{
	char    ebuf[80];

	sprintf (ebuf, "I move to %c %d\n",
		y-1+'a', x);
	dispError (ebuf);
}

dispHint (x, y, player)
{
    	char	buf[80];
    	sprintf (buf, "I suggest %c %d",
	    	y-1+'a', x);
    	dispError (buf);
}

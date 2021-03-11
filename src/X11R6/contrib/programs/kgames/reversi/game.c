#include <stdio.h>
#include <sys/signal.h>
#include "reversi.h"

boardT	board, saveBoard;
int	saved;
int	savePlayer;
int	atend;
int	atbegin;
int	level;
int	player;
extern int	maxlev, movex, movey;
int	x, y;
int	com;
int	gotsignal;
char	sbuf[80];
char	ebuf[80];
int	sdebug = 0, mdebug = 0;
int	record = 0;
FILE	*rfile;
int	first = WHITE;
int	defcom = BLACK;
int	showScore = 1;

struct move	saveGame[64];
struct move	*saveP;

caught ()
{
	gotsignal++;
	signal (SIGINT, caught);
}

main (argc, argv)
char **argv;
{
	signal (SIGINT, caught);
	level = 2;
	dispInit (argc, argv);
	srand (getpid());
	while (*++argv && **argv == '-') {
		while (*++*argv) {
			switch (**argv) {
			case 'b':
				defcom = BLACK;
				break;
			case 'w':
				defcom = WHITE;
				break;
			case '1':
				if (!*++*argv)
					continue;
				if (**argv == WHITE)
					first = WHITE;
				else
					first = BLACK;
				break;
			case 'g':
				dispGrid ();
				break;
			case 's':
				showScore = 1;
			}
		}
	}
	do {
		if (rfile)
			fclose (rfile);
		rfile = 0;
		player = first;
		com = defcom;
		atend = 0;
		atbegin = 1;
		setup ();
		saved = 0;
		saveP = saveGame;
		display (board);
		if (*argv) {
			replay (*argv);
			++argv;
		}
	} while (playGame());
	dispEnd ();
}

setup ()
{
	register int	i,j;

	for (i = 1; i <= SIZE; i++)
		for (j = 1; j <= SIZE; j++)
			board[i][j] = 0;
	board[4][4] = WHITE;
	board[4][5] = BLACK;
	board[5][4] = BLACK;
	board[5][5] = WHITE;
}

replay (file)
char *file;
{
	int	x, y, p;
	if (rfile)
		fclose (rfile);
	if ((rfile = fopen (file, "r")) == NULL) {
		sprintf (ebuf, "could not open %s", file);
		dispError (ebuf);
		return;
	}
	while (fscanf (rfile, "%d: %d, %d\n", &p, &x, &y) == 3) {
		if (x == -1 && y == -1) {
			player = p;
			continue;
		}
		if (!hasmove (player, board)) {
			player = -player;
			if (!hasmove (player, board))
				return;
		}
		if (p != player) {
			sprintf (ebuf, "not %s's turn\n",
			    player == WHITE? "white":"black");
			dispError (ebuf);
			return;
		}
		if (!legal (p, x, y, board)) {
			sprintf(ebuf, "illegal move: %d, %d\n", x, y);
			dispError (ebuf);
			return;
		}
		move (p, x, y, board);
		atbegin = 0;
		player = -player;
		display (board);
	}
	fclose (rfile);
	rfile = 0;
}

domove (x,y)
{
	if (1 <= x && x <= SIZE &&
	    1 <= y && y <= SIZE &&
	    legal (player, x, y, board)) {
		copy (saveBoard, board);
		savePlayer = player;
		++saved;
		move (player, x, y, board);
		atbegin = 0;
		if (record)
			fprintf (rfile, "%d: %d,%d\n",
			    player, x, y);
		saveP->x = x;
		saveP->y = y;
		saveP->p = player;
		++saveP;
		player = -player;
		display (board);
	} else {
		sprintf (ebuf, "illegal move: %c %d",
			y+'a'-1, x);
		dispError (ebuf);
	}
}

checkInput ()
{
	if (!atend) {
		loop:	;
		dispTurn (player);
		if (!hasmove (player, board)) {
			if (!hasmove (-player, board)) {
				fini (board);
				if (com == 0)
					com = BLACK;
				++atend;
				dispTurn (EMPTY);
				return;
			} else {
				if (player == WHITE)
					dispError ("white has no move");
				else
					dispError ("black has no move");
				player = -player;
			}
		}
		if (com == 0 || com == player) {
			dispError ("thinking...");
			if (computer (player, board, level)) {
				atbegin = 0;
				display (board);
				dispMove (movex, movey, player);
				saveP->x = movex;
				saveP->y = movey;
				saveP->p = player;
				++saveP;
				if (record)
					fprintf (rfile, "%d: %d,%d\n",
					    player, movex, movey);
				player = -player;
				if (gotsignal && com != 0)
					gotsignal = 0;
			}
			if (gotsignal && com == 0) {
				com = -player;
				gotsignal = 0;
			}
			goto loop;
		}
	}
}

undo ()
{
	if (saved) {
		copy (board, saveBoard);
		player = savePlayer;
		saved = 0;
		display (board);
	}
}

doHint ()
{
	if (hasmove (player, board)) {
		hint (player, board, level);
		dispHint (movex, movey, player);
	}
}

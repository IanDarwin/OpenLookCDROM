/*
 *	fini.c
 *
 *	count up score and display winner
 */

# include	"reversi.h"

fini (board)
boardT	board;
{
	register int	x,y;
	register int	wscore, bscore;
	char			sbuf[80];

	wscore = bscore = 0;

	for (x = 1; x <= SIZE; x++)
		for (y = 1; y <= SIZE; y++)
			if (board[x][y] == WHITE)
				++wscore;
			else if (board[x][y] == BLACK)
				++bscore;
	if (wscore > bscore)
		sprintf (sbuf, "white wins %d to %d.", wscore, bscore);
	else if (bscore > wscore)
		sprintf (sbuf, "black wins %d to %d.", bscore, wscore);
	else
		sprintf (sbuf, "tie game %d to %d.", wscore, bscore);
	dispError (sbuf);
}

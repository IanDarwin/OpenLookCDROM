/*
 *	hasmove.c
 *
 *	figure out if player has move in board
 */

# include	"reversi.h"

hasmove (player, board)
boardT	board;
{
	register int	x,y;

	for (x = 1; x <= SIZE; x++)
		for (y = 1; y <= SIZE; y++)
			if (legal (player, x, y, board))
				return 1;
	return 0;
}

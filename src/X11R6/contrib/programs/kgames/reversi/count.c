/*
 *	count.c
 *
 *	count up the board
 */

# include	"reversi.h"

count (player, board)
boardT	board;
{
	register int	x, y, count;

	count = 0;
	for (x = 1; x <= SIZE; x++)
		for (y = 1; y <= SIZE; y++)
			count += board[x][y];
	return count * player;
}

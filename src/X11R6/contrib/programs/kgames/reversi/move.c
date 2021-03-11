/*
 *	move.c
 *
 *	move player to x,y in board
 */

# include	"reversi.h"

int		offsets[] = { -11, -10, -9, -1, 1, 9, 10, 11, 0 };

move (player, x, y, board)
register int	player;
int				x, y;
boardT			board;
{
	register boardE	*b, *m;
	register int	*o, i;

	b = & board[x][y];
	*b = player;
	player = -player;
	for (o = offsets; i = *o++;) {
		if (b[i] == player) {
			m = b+i;
			while (*m == player)
				m += i;
			if (*m == -player) {
				while (m != b) {
					*m = -player;
					m -= i;
				}
			}
		}
	}
}

legal (player, x, y, board)
register int	player;
int		x, y;
boardT		board;
{
	register int	i;
	register boardE	*m;
	register boardE	*b;
	register int	*o;

	b = & board[x][y];
	player = -player;
	if (*b == EMPTY) {
		for (o = offsets; i = *o++;) {
			if (*(m=b+i) == player) {
				do
					m += i;
				while (*m == player);
				if (*m == -player)
					return 1;
			}
		}
	}
	return 0;
}

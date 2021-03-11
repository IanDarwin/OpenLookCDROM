/*
 *	minmax.c
 */

# include	"reversi.h"
# include	<setjmp.h>
# include	<stdio.h>

int	maxlev, movex, movey;

/*
 *	this defines the order in which the board
 *	is searched for the best move.  It is
 *	here to shorten the time to best move,
 *	this increasing the chance of hitting
 *	a good trimming point as well as
 *	increasing the possibility of making
 *	a reasonable move when an interrupt is
 *	caught.
 */
short	morder[64][2] = {
	1,1, 1,8, 8,1, 8,8,
	1,3, 1,6, 3,1, 3,8, 6,1, 6,8, 8,3, 8,6,
	3,3, 3,6, 6,3, 6,6,
	1,4, 1,5, 4,1, 4,8, 5,1, 5,8, 8,4, 8,5,
	3,4, 3,5, 4,3, 4,6, 5,3, 5,6, 6,4, 6,5,
	2,3, 2,6, 3,2, 3,7, 6,2, 6,7, 7,3, 7,6,
	2,4, 2,5, 4,2, 4,7, 5,2, 5,7, 7,4, 7,5,
	1,2, 1,7, 2,1, 2,8, 7,1, 7,8, 8,2, 8,7,
	2,2, 2,7, 7,2, 7,7, 4,4, 4,5, 5,4, 5,5,
};

jmp_buf	stopsearch;
/* #define MDEBUG	/* turn on minmax debugging */
#ifdef MDEBUG
FILE	*debug;
#endif

#ifdef pdp11
# define	NOMOVE	(-32100)
#else
# define	NOMOVE	(-300000)
#endif

#define	UNROLL
#define	USECOPY
#ifdef	USECOPY
# ifdef UNROLL

#define Copy5(a,b,o)	a[o+0] = b[o+0]; \
			a[o+1] = b[o+1]; \
 			a[o+2] = b[o+2]; \
			a[o+3] = b[o+3]; \
			a[o+4] = b[o+4];
	
#define Copy25(a,b,o)	Copy5(a,b,o+0) \
			Copy5(a,b,o+5) \
			Copy5(a,b,o+10) \
			Copy5(a,b,o+15) \
			Copy5(a,b,o+20)

#define copyb(a,b) { \
	register int	*dst = (int *) a, *src = (int *) b; \
	Copy25 (dst,src,0) \
}

# else
struct copyB {
	boardT	data;
};
#  define	copyb(next,board)	(*((struct copyB *)next) = *((struct copyB *) board))
# endif
#else
# define	copyb(next,board)	copy(next,board)
#endif

copy(next, board)
register int	*next, *board;
{
	register int	count;

	count = sizeof (boardT) / sizeof (int);
	do {
		*next++ = *board++;
	} while (--count);
}

/*
 *	this is the base score matrix - note that only
 *	the center 16 squares are used.
 */
 
boardT	base = {
	  0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
	  0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
	  0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
	  0,   0,   0,   3,   1,   1,   3,   0,   0,   0,
	  0,   0,   0,   1,   0,   0,   1,   0,   0,   0,
	  0,   0,   0,   1,   0,   0,   1,   0,   0,   0,
	  0,   0,   0,   3,   1,   1,   3,   0,   0,   0,
	  0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
	  0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
	  0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
};

# define bget(b, x, y)	*(((boardE *) (b)) + ((((x) << 2) + x) << 1) + (y))

scoreT edgemod[10][3] = {
	0,	0,	0,
	0,	0,	0,
	0,	0,	0,
	5,	-10,	5,
	5,	-5,	5,
	5,	-5,	5,
	5,	-10,	5,
	0,	0,	0,
	0,	0,	0,
	0,	0,	0,
};

extern scoreT	cornerscores[4][4][4][4];

# define cornersc(a,b,c,d)		cornerscores[a+1][b+1][c+1][d+1];
	
extern scoreT	edgescores[4][4][4][4][4][4][4][4];

# define edgesc(a,b,c,d,e,f,g,h)	edgescores[a+1][b+1][c+1][d+1][e+1]\
						  [f+1][g+1][h+1];

computer (player, board, level)
boardT	board;
{
	int	i;
	extern int	com, defcom;

#ifdef MDEBUG
	if (!debug)
		debug = fopen ("debug", "w");
#endif
	maxlev = level;
	movex = movey = -1;
	i = seek (player, board, 0, 1, -NOMOVE);
	if (movex == -1 || movey == -1)
		return 0;
#ifdef MDEBUG
	fprintf (debug, "player %d move to %d, %d\n", player, movex, movey);
	fflush (debug);
#endif
	move (player, movex, movey, board);
	return 1;
}

hint (player, board, level)
boardT board;
{
	int	i;

#ifdef MDEBUG
	if (!debug)
		debug = fopen ("debug", "w");
#endif
	maxlev = level;
	i = seek (player, board, 0, 1, -NOMOVE);
#ifdef MDEBUG
	fprintf (debug, "player %d hint %d, %d\n", player, movex, movey);
	fflush (debug);
#endif
	if (movex == -1 || movey == -1)
		return 0;
	return 1;
}

extern int offsets[];

seek (player, board, level, moved, best)
#ifdef pdp11
int	player;
#else
register int	player;
#endif
boardT	board;
{
	boardT		next;
	int		x, y;
	int		max;
	int		bestx, besty;
	int		moves, j;
	extern int	gotsignal;
	register int	opponent = -player;

	max = NOMOVE;
	moves = 0;
	for (j = 0; j < 60; j++) {
		x = morder[j][0];
		y = morder[j][1];
		if (gotsignal)
			return 0;
		{
			register boardE	*m;
			boardE	*b;
			int	*o;

			b = & board[x][y];
			if (*b == EMPTY) {
#define CheckMove(off)	if (*(m=b+(off)) == opponent) {		\
				do				\
					m += off;		\
				while (*m == opponent);		\
				if (*m == player)		\
					goto goodmove;		\
			}

				CheckMove(-11)
				CheckMove(-10)
				CheckMove(-9)
				CheckMove(-1)
				CheckMove(1)
				CheckMove(9)
				CheckMove(10)
				CheckMove(11)
			}

			continue;
		}
goodmove:	;
		{
#ifdef pdp11
			int	s;
#else
			register int	s;
#endif

			copyb (next, board);
			/*
			 *	moved is set if we come from
			 *	a command, not if the previous
			 *	level resulted in no move
			 */
			if (moved && level == 0 && movex == -1) {
				movex = x;
				movey = y;
			}
			move (player, x, y, next);
			++moves;
			if (level >= maxlev) {
			/*
			 *	score this board - this
			 *	is extracted from score.c and
			 *	inserted here for speed
			 */
				register int	i;
				register boardE	*bo, *ba;
	
				bo = (boardE *) next;
				s  = edgesc (bget(bo,1,1),bget(bo,1,2),
					     bget(bo,1,3),bget(bo,1,4),
					     bget(bo,1,5),bget(bo,1,6),
					     bget(bo,1,7),bget(bo,1,8));
				s += edgesc (bget(bo,8,1),bget(bo,8,2),
					     bget(bo,8,3),bget(bo,8,4),
					     bget(bo,8,5),bget(bo,8,6),
					     bget(bo,8,7),bget(bo,8,8));
				s += edgesc (bget(bo,1,1),bget(bo,2,1),
					     bget(bo,3,1),bget(bo,4,1),
					     bget(bo,5,1),bget(bo,6,1),
					     bget(bo,7,1),bget(bo,8,1));
				s += edgesc (bget(bo,1,8),bget(bo,2,8),
					     bget(bo,3,8),bget(bo,4,8),
					     bget(bo,5,8),bget(bo,6,8),
					     bget(bo,7,8),bget(bo,8,8));
				s += cornersc (bget(bo,1,1),bget(bo,1,2),
					       bget(bo,2,1),bget(bo,2,2));
				s += cornersc (bget(bo,1,8),bget(bo,1,7),
					       bget(bo,2,8),bget(bo,2,7));
				s += cornersc (bget(bo,8,1),bget(bo,8,2),
					       bget(bo,7,1),bget(bo,7,2));
				s += cornersc (bget(bo,8,8),bget(bo,8,7),
					       bget(bo,7,8),bget(bo,7,7));
				for (i = 3; i <= 6; i++) {
					s += bget(bo,2,i) *
						edgemod[i][bget(bo,1,i)+1];
					s += bget(bo,7,i) *
						edgemod[i][bget(bo,8,i)+1];
					s += bget(bo,i,2) *
						edgemod[i][bget(bo,i,1)+1];
					s += bget(bo,i,7) *
						edgemod[i][bget(bo,i,8)+1];
				}
				for (i = 3; i <= 6; i++) {
					bo = &next[i][3];
					ba = &base[i][3];
					s += *bo++ * *ba++;
					s += *bo++ * *ba++;
					s += *bo++ * *ba++;
					s += *bo++ * *ba++;
				}
				s *= player;
			} else
				s = seek (-player, next, level+1, 1, -max);
#ifdef MDEBUG
fprintf (debug, "%.*s l %d %d,%d %d\n", level+1, "          ", level, x, y, s);
#endif
			if (s >= max) {
stopscore:			;
				/*
				 *	try to make the game appear random
				 *	by choosing among equal moves
				 *	randomly
				 */
				if (s == max && (rand() >> 3) & 01)
					continue;
				if (s > best)
					return -s;
				bestx = x;
				besty = y;
				if (level == 0) {
					movex = bestx;
					movey = besty;
				}
				max = s;
			}
		}
	}
	if (moves == 0) {
		if (moved) {
#ifdef MDEBUG
fprintf (debug, "         no move but not using count, level %d\n", level);
#endif
			max = seek (-player, board, level, 0, -best);
		} else {
#ifdef MDEBUG
fprintf (debug, "         using count, level %d\n", level);
#endif
#ifdef pdp11
			max = count (player, board) * 500;
#else
			max = count (player, board) * 1000;
#endif
		}
#ifdef MDEBUG
fprintf (debug, "%.*s l %d no move %d\n", level+1, "          ", level, max);
#endif
		return - max;
	}
	return -max;
}

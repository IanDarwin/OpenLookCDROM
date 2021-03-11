/*
 *	reversi.h
 *
 *	include file for game program
 */

# define	SIZE	8

typedef char	boardE;

typedef short	scoreT;

typedef boardE	boardT[SIZE+2][SIZE+2];

typedef boardT	*boardP;

struct move {
	int	p, x, y;
};

# define	EMPTY	0
# define	WHITE	1
# define	BLACK	-1

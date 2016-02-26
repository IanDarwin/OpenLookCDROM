/*
 * This file is a product of Sun Microsystems, Inc. and is provided for
 * unrestricted use provided that this legend is included on all tape
 * media and as a part of the software program in whole or part.
 * Users may copy, modify, or distribute this file at will.
 * 
 * THIS FILE IS PROVIDED AS IS WITH NO WARRANTIES OF ANY KIND INCLUDING THE
 * WARRANTIES OF DESIGN, MERCHANTIBILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE, OR ARISING FROM A COURSE OF DEALING, USAGE OR TRADE PRACTICE.
 * 
 * This file is provided with no support and without any obligation on the
 * part of Sun Microsystems, Inc. to assist in its use, correction,
 * modification or enhancement.
 * 
 * SUN MICROSYSTEMS, INC. SHALL HAVE NO LIABILITY WITH RESPECT TO THE
 * INFRINGEMENT OF COPYRIGHTS, TRADE SECRETS OR ANY PATENTS BY THIS FILE
 * OR ANY PART THEREOF.
 * 
 * In no event will Sun Microsystems, Inc. be liable for any lost revenue
 * or profits or other special, indirect and consequential damages, even
 * if Sun has been advised of the possibility of such damages.
 * 
 * Sun Microsystems, Inc.
 * 2550 Garcia Avenue
 * Mountain View, California  94043
 */

/* @(#)defs.h 9.2 88/08/16 SMI	 */

/*
 * Copyright (c) 1987 by Sun Microsystems, Inc.
 */

#include <stdio.h>
#include "bkgcodes.h"

#define STARTGAME		0	/* all possible states of play */
#define COMPUTERDOUBLING	1
#define HUMANDOUBLING		2
#define ROLL			3
#define MOVE			4
#define THINKING		5
#define GAMEOVER		6

#define MSG	0	/* two kinds of messages for message() */
#define ERR	1

#define HUMAN		0	/* constants for whose turn it is, */
#define COMPUTER	1	/* who last doubled, etc. */
#define NOBODY		2
#define BOTH		3	/* also applies to both black and white */

#define STRAIGHT	1	/* types of wins */
#define GAMMON		2
#define BACKGAMMON	3
#define RESIGNED	4
#define REFUSEDDBLE	5

#define WHITE	0	/* colors of playing pieces */
#define BLACK	1
#define NONE	2
#define computercolor	(humancolor == WHITE ? BLACK : WHITE)

#define OUTOFBOUNDS	-1

#define BAR	25	/* the indices of the obvious in the board arrays */
#define HOME	0

#define ROLLUSED	8	/* bit mask indicating roll has been used */
#define SECONDROLLUSED	16	/* same for second use when doubles rolled */
#define NUMMASK		7	/* mask to get actual number of roll */

#define BLOT	-1	/* used by findmaxmoves */
#define BLOCKED	-2

#define TOP	0
#define BOTTOM	1
#define isontop(point)	(point > 12 && point != BAR && point != HOME)
#define isonbot(point)	(point < 13 && point != BAR && point != HOME)

#define FROM	0
#define TO	1
#define HITBLOT	2
#define DIEUSED	3

#define FLASH	1
#define NOFLASH	0

#define	MAXNAME	15

#define DEFAULT_BINARY	"backgammon"

int state;		/* the current state of play */
char *gammonbin;	/* actual pathname of backgammon program */
int humanscore, computerscore;	/* cumulative scores */
char humanname[MAXNAME+1];	/* name of human player */
int gammonpid;		/* pid of backgammon child */
int gammonfd;		/* fd of pipe to read from backgammon */
int gamevalue;		/* point value of current game */
int humancolor;		/* color of human */
int humanboard[26], computerboard[26];	/* actual playing boards */
int humandieroll[2], computerdieroll[2];	/* current roll of dice */
int numhdice, numcdice;		/* number of dice which are valid */
int movesmade;		/* moves actually accomplished this turn */
int maxmoves;		/* numbers of moves human can make this turn */
int lastdoubled;	/* player who last doubled */
int diddouble;		/* player double this turn */
int lastmoved;		/* for showlastmove: player who last moved */
int dicedisplayed;	/* for showlastmove: dice on display */
int alreadyrolled;	/* special flag for first roll */
FILE *logfp;		/* log file */
int currentdice[4], lastdice[4];    /* cached dice for show-last-move */

# include	<ctype.h>
# include	<stdio.h>

typedef char	bool;
# define reg register
# define TRUE	1
# define FALSE	0

/*
 * @(#)mille.h	1.1 (Berkeley) 4/1/82
 */

/*
 * Miscellaneous constants
 */

# define	unsgn		unsigned
# define	CARD		short

# ifdef  vax
#	define	ARNOLD		78	/* my uid			*/
# else
#	define	ARNOLD		24601	/* my uid			*/
# endif

# define	GURP		28672	/* bad uid			*/
# define	MAXUSERS	35	/* max # of users for startup	*/
# define	HAND_SZ		7	/* number of cards in a hand	*/
# define	DECK_SZ		101	/* number of cards in decks	*/
# define	NUM_SAFE	4	/* number of saftey cards	*/
# define	NUM_MILES	5	/* number of milestones types	*/
# define	NUM_CARDS	20	/* number of types of cards	*/

# define	PLAYER		0
# define	COMP		1

# define	W_SMALL		0	/* Small (initial) window	*/
# define	W_FULL		1	/* Full (final) window		*/

/*
 * Move types
 */

# define	M_DISCARD	0
# define	M_DRAW		1
# define	M_PLAY		2
# define	M_ORDER		3
# define	M_REASONABLE	4

/*
 * Scores
 */

# define	SC_SAFETY	100
# define	SC_ALL_SAFE	300
# define	SC_COUP		300
# define	SC_TRIP		400
# define	SC_SAFE		300
# define	SC_DELAY	300
# define	SC_EXTENSION	200
# define	SC_SHUT_OUT	500

/*
 * safety descriptions
 */

# define	S_UNKNOWN	0	/* location of safety unknown	*/
# define	S_IN_HAND	1	/* safety in player's hand	*/
# define	S_PLAYED	2	/* safety has been played	*/
# define	S_GAS_SAFE	0	/* Gas safety card index	*/
# define	S_SPARE_SAFE	1	/* Tire safety card index	*/
# define	S_DRIVE_SAFE	2	/* Driveing safety card index	*/
# define	S_RIGHT_WAY	3	/* Right-of-Way card index	*/
# define	S_CONV		15	/* conversion from C_ to S_	*/

/*
 * card numbers
 */

# define	C_INIT		-1
# define	C_25		0
# define	C_50		1
# define	C_75		2
# define	C_100		3
# define	C_200		4
# define	C_EMPTY		5
# define	C_FLAT		6	
# define	C_CRASH		7
# define	C_STOP		8
# define	C_LIMIT		9
# define	C_GAS		10
# define	C_SPARE		11
# define	C_REPAIRS	12
# define	C_GO		13
# define	C_END_LIMIT	14
# define	C_GAS_SAFE	15
# define	C_SPARE_SAFE	16
# define	C_DRIVE_SAFE	17
# define	C_RIGHT_WAY	18

typedef struct {
	bool	coups[NUM_SAFE];
	bool	can_go;
	bool	new_battle;
	bool	new_speed;
	short	safety[NUM_SAFE];
	short	nummiles[NUM_MILES];
	CARD	hand[HAND_SZ];
	CARD	battle;
	CARD	speed;
	int	mileage;
	int	hand_tot;
	int	safescore;
	int	coupscore;
	int	total;
	int	games;
} PLAY;

/*
 * animation constants
 */

# define ANIMATE
# define ANIMATE_HAND		0
# define ANIMATE_DECK		1
# define ANIMATE_DISC		2
# define ANIMATE_MILES		3
# define ANIMATE_BATTLE		4
# define ANIMATE_SPEED		5
# define ANIMATE_OBATTLE	6
# define ANIMATE_OSPEED		7
# define ANIMATE_SAFETY		8

/*
 * macros
 */

# define	other(x)	(1 - x)
# define	nextplay()	(Play = other(Play))
# define	nextwin(x)	(1 - x)
# define	opposite(x)	(Opposite[x])
# define	issafety(x)	(x >= C_GAS_SAFE)

/*
 * externals
 */

extern bool	Debug, Finished, Next, On_exit, Order, Saved;

extern char	*C_fmt, **C_name, *Fromfile, Initstr[];

extern int	Card_no, End, Handstart, Movetype, Numcards[], Numgos,
		Numneed[], Numseen[NUM_CARDS], Play, Value[], WIndow;

extern CARD	Deck[DECK_SZ], Discard, Opposite[NUM_CARDS], *Topcard;

extern FILE	*outf;

extern PLAY	Player[2];

/*
 * functions
 */

CARD	getcard();

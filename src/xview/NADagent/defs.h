
/*	@(#)defs.h	2.2 8/16/88		*/

/*
 * -------------------------------------------------------------------------
 *	ROLO - A Sun Tool to implement a Rolodex-style list of notes
 *
 *	This code manipulates "cards" in a visual manner approximating
 *	a rolodex file.  All the cards are stored in one real file, the
 *	cards are seperated by a ^L (form-feed).  The default path
 *	name is $HOME/.rolo.  A different pathname may be specified at
 *	startup on the command line.  The pathname is relative to the
 *	user's home directory.
 *
 *	Due to bugs in the 3.0 distribution, especially with text subwindows,
 *	this code is only guaranteed to compile and run properly with 3.2
 *	or greater.
 *
 *	This code is public domain, anyone and everyone is welcome to it.
 *	All I ask is that my name and this notice remain on it.  If Sun would
 *	like to bundle it with their product they are welcome to do so,
 *	I only ask that the sources be included in the binary distribution.
 *
 *	Please return any fixes, improvements, gripes, etc to me.
 *
 *	Ron Hitchens		ronbo@vixen.uucp
 *	March 1987 (V1.0)	hitchens@cs.utexas.edu
 *	August 1988 (V2.0)
 * -------------------------------------------------------------------------
 */


/*
 *	Definitions for Rolo (This is version 2.0)
 */


#ifndef TRUE
#define TRUE 1
#define FALSE 0
#endif

#define ROLOFILE		"~/.rolo"	/* name of Rolo file in $HOME */
#define NEWMODE			0600	/* file mode for creating new ROLOFILE */
#define NAME			"Rolo"
#define MAX_SELN_LEN		80	/* Max length of selection */
#define MAX_INDEX_LINE		80	/* Max length of an index line */
#define MAX_MENU_LEN		30

#define HELPDISPLAYED		0x800000 /* "#" of card if help displayed */
#define LISTALLCARDS		0x400000 /* "#" of card if listing all */

#define PLAIN_CLICK		0	/* Symbolic names for the bit      */
#define SHIFT_CLICK		1	/* encodings of the eight possible */
#define CTRL_CLICK		2	/* combinations of the three shift */
#define CTRL_SHIFT_CLICK	3	/* modifiers.  These values are    */
#define META_CLICK		4	/* returned by the function        */
#define META_SHIFT_CLICK	5	/* value_from_mask().              */
#define META_CTRL_CLICK		6
#define META_CTRL_SHIFT_CLICK	7


struct card {
	char		*c_text;		/* the text of the card */
	int		c_num;			/* logical card number */
	struct	card	*c_next,		/* pointer to the next card */
			*c_prev;		/* pointer to the previous */
};

#define NULL_CARD	(struct card *)0


#define DUMMY_CARD_CONTENTS	"\n\n\t  RoloTool - by Ron Hitchens\t(hitchens@vixen.uucp)\n\n\t  Ported to XView by Luis Soltero (luis@rice.edu)\n\n\n\n\t      delete this card";


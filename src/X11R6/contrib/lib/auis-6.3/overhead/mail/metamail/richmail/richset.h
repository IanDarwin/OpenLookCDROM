/*-------------------------------------------------------------------------

  richset.h - Handling for different character sets in richtext.
 
  Copyright (c) 1992 Rhys Weatherley

  Permission to use, copy, modify, and distribute this material
  for any purpose and without fee is hereby granted, provided
  that the above copyright notice and this permission notice
  appear in all copies, and that the name of Rhys Weatherley not be
  used in advertising or publicity pertaining to this
  material without specific, prior written permission.
  RHYS WEATHERLEY MAKES NO REPRESENTATIONS ABOUT THE ACCURACY OR
  SUITABILITY OF THIS MATERIAL FOR ANY PURPOSE.  IT IS PROVIDED
  "AS IS", WITHOUT ANY EXPRESS OR IMPLIED WARRANTIES.

  Revision History:
  ================

   Version  DD/MM/YY  By  Description
   -------  --------  --  --------------------------------------
     1.0    19/06/92  RW  Original Version of richset.h

  You may contact the author by:
  =============================

   e-mail: rhys@cs.uq.oz.au
     mail: Rhys Weatherley
	   5 Horizon Drive
	   Jamboree Heights
	   Queensland 4074
	   Australia

-------------------------------------------------------------------------*/

#ifndef	__RICHSET_H__
#define	__RICHSET_H__

#ifdef	__cplusplus
extern "C" {
#endif

/*
 * Define the interface structure for a character set processor.
 * The fields are as follows:
 *
 *	names	- Colon-separated list of character set names.
 *	init	- Initialize the character set processor.
 *	command	- Process a command before the default processing.
 *		  Returns non-zero if processed, zero if not.
 *	single	- Should return non-zero for a singleton command.
 *	width	- Get the width in terminal characters of the character.
 *	fold	- Returns non-zero if the character can be folded at.
 *	render	- Render the character through RichtextPutc.
 *	encoding- Enter or leave an encoding. newenc is -1 to leave.
 *
 */
struct	charsetproc
		{
		    char *names;
		    int (*init) ( /* char *name */ );
		    int (*command) ( /* char *token, int negated */ );
		    int (*single) ( /* char *token */ );
		    int (*width) ( /* RCHAR c */ );
		    int (*fold) ( /* RCHAR c */ );
		    int (*render) ( /* RCHAR c, void *param */ );
		    int (*encoding) ( /* int newenc */ );
		};

/*
 * Define some standard character set processors.
 */
extern	struct 	charsetproc	usascii_charset;
extern	struct	charsetproc	iso2022_charset;

/*
 * Define the information to be kept in the internal buffers
 * about a character.  If "charset" is NULL, it is a control
 * character.
 */
struct	charsetmember
		{
		    RCHAR ch;			 /* The character itself */
		    struct charsetproc *charset; /* Character set of ch */
		};

/*
 * Initialise the stack of character set processors, starting with
 * a particular base processor.  The initialisation function of all
 * character set processors is called.
 */
extern	charsetinit	( /* struct charsetproc *charset, char *name */ );

/*
 * Initialise the stack, starting with a character set processor with
 * a particular name.
 */
extern	charsetnameinit	( /* char *name */ );

/*
 * Push a new character set processor onto the stack.
 */
extern	charsetpush	( /* struct charsetproc *charset */ );

/*
 * Pop the top-most character set processor off the stack
 * if it matches the given processor.  Note: the base
 * processor is never popped off.
 */
extern	charsetpop	( /* struct charsetproc *charset */ );

/*
 * See if the character set processor on the top of the stack
 * matches the given processor.
 */
extern	int	charsettop ( /* struct charsetproc *charset */ );

/*
 * Set the details for a character set member in the top-most
 * character set.
 */
extern	charmember	( /* struct charsetmember *member, RCHAR ch */ );

/*
 * Set the details for a member of a specific character set.
 */
extern	charmemberspec	( /* struct charsetmember *member, RCHAR ch,
			     struct charset *charset */ );

/*
 * Set the details for a output control code character.
 */
extern	charmemberctrl	( /* struct charsetmember *member, RCHAR ch */ );

/*
 * Determine if the given character is a control code character.
 */
#define	charisctrl(member)	((member).charset == (struct charsetproc *)0)

/*
 * Attempt to process a richtext command by passing it to the
 * "command" function of all character set processors.  Returns
 * zero if the command was not processed.
 */
extern	int	charsetcommand	( /* char *token, int negated */ );

/*
 * Test for an extension singleton command.
 */
extern	int	charsetsingle	( /* char *token */ );

/*
 * Get the width of a particular character.
 */
#define	charmemberwidth(member) \
		((*((member).charset -> width)) ((member).ch))

/*
 * Determine if a character can be folded at.
 */
#define	charmemberfold(member) \
		((*((member).charset -> fold)) ((member).ch))

/*
 * Render a character on an output stream.
 */
#define	charmemberrender(member,param) \
		((*((member).charset -> render)) ((member).ch,param))

#ifdef	__cplusplus
};
#endif

#endif	/* __RICHSET_H__ */

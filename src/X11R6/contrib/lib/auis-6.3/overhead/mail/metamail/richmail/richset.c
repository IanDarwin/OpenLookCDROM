/*-------------------------------------------------------------------------

  richset.c - Handling for different character sets in richtext.
 
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
     1.0    21/06/92  RW  Original Version of richset.c

  You may contact the author by:
  =============================

   e-mail: rhys@cs.uq.oz.au
     mail: Rhys Weatherley
	   5 Horizon Drive
	   Jamboree Heights
	   Queensland 4074
	   Australia

-------------------------------------------------------------------------*/

#include <stdio.h>
#include "richlex.h"
#include "richset.h"

/*
 * Define a list of all character set processors in memory.
 */
static	struct	charsetproc	*CharacterSets[] =
	  {&usascii_charset,
	   &iso2022_charset,
	   NULL};

/*
 * Global data for this module.
 */
#define	MAX_CHAR_SETS	100
static	int	NumCharSets=0;
static	struct	charsetproc	*CharSets[MAX_CHAR_SETS];
static	int	CharEncStack[MAX_CHAR_SETS];

/*
 * Initialise the stack of character set processors, starting with
 * a particular base processor.  The initialisation function of all
 * character set processors is called.
 */
charsetinit (charset,name)
struct	charsetproc *charset;
char	*name;
{
    int temp = 0;
    CharSets[0] = charset;
    NumCharSets = 1;
    while (CharacterSets[temp]) {
        (*(CharacterSets[temp] -> init))
		((CharacterSets[temp] == charset ? name : NULL));
	++temp;
    }
}

/*
 * Initialise the stack, starting with a character set processor with
 * a particular name.
 */
charsetnameinit	(name)
char	*name;
{
    int temp = 0;
    int succeed = 0;
    char *temp1,*temp2;
    struct charsetproc *charset = &usascii_charset;
    while (!succeed && CharacterSets[temp]) {
	temp2 = CharacterSets[temp] -> names;
	while (!succeed && *temp2) {
	    temp1 = name;
	    while (*temp1 && *temp2 && *temp2 != ':' && *temp2 == *temp1) {
		++temp1;
		++temp2;
	    }
	    if (*temp1 == '\0' && (*temp2 == '\0' || *temp2 == ':')) {
		succeed = 1;
		charset = CharacterSets[temp];
	    }
	    while (*temp2 && *temp2 != ':') {
		++temp2;
	    }
	    if (*temp2 == ':') {
		++temp2;
	    }
	}
	++temp;
    }
    charsetinit (charset,name);
}

/*
 * Push a new character set processor onto the stack.
 */
charsetpush (charset)
struct	charsetproc *charset;
{
    if (NumCharSets >= MAX_CHAR_SETS) {
    	fprintf (stderr,"Too many nested character sets: aborting\n");
	exit (1);
    } else {
	CharEncStack[NumCharSets - 1] = RichtextCharEncoding;
	(*(CharSets[NumCharSets - 1] -> encoding)) (-1); /* Leave current */
    	CharSets[NumCharSets++] = charset;
    }
}

/*
 * Pop the top-most character set processor off the stack
 * if it matches the given processor.  Note: the base
 * processor is never popped off.
 */
charsetpop (charset)
struct	charsetproc *charset;
{
    if (NumCharSets > 1 && CharSets[NumCharSets - 1] == charset) {
        --NumCharSets;
	richtextencoding (CharEncStack[NumCharSets - 1]);
	(*(CharSets[NumCharSets - 1] -> encoding)) (RichtextCharEncoding);
    }
}

/*
 * See if the character set processor on the top of the stack
 * matches the given processor.
 */
int	charsettop (charset)
struct	charsetproc *charset;
{
    if (NumCharSets > 1) {
	return (CharSets[NumCharSets - 1] == charset);
    } else {
	return (0);
    }
}

/*
 * Set the details for a character set member in the top-most
 * character set.
 */
charmember (member,ch)
struct	charsetmember *member;
RCHAR	ch;
{
    member -> ch = ch;
    member -> charset = CharSets[NumCharSets - 1];
}

/*
 * Set the details for a member of a specific character set.
 */
charmemberspec (member,ch,charset)
struct	charsetmember *member;
RCHAR	ch;
struct	charsetproc *charset;
{
    member -> ch = ch;
    member -> charset = charset;
}

/*
 * Set the details for a output control code character.
 */
charmemberctrl (member,ch)
struct	charsetmember *member;
RCHAR	ch;
{
    member -> ch = ch;
    member -> charset = NULL;
}

/*
 * Attempt to process a richtext command by passing it to the
 * "command" function of all character set processors.  Returns
 * zero if the command was not processed.
 */
int	charsetcommand (token,negated)
char	*token;
int	negated;
{
    int temp = 0;
    while (CharacterSets[temp]) {
    	if ((*(CharacterSets[temp] -> command)) (token,negated))
	    return (1);
	++temp;
    }
    return (0);
}

/*
 * Test for an extension singleton command.
 */
int	charsetsingle (token)
char	*token;
{
    int temp = 0;
    while (CharacterSets[temp]) {
    	if ((*(CharacterSets[temp] -> single)) (token))
	    return (1);
	++temp;
    }
    return (0);
}

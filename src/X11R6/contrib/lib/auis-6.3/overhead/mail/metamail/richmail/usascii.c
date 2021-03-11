/*-------------------------------------------------------------------------

  usascii.c - Code for the US-ASCII specific parts of the richtext processor.
 
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
     1.0    21/06/92  RW  Original Version of usascii.c

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
#include <ctype.h>
#include "richlex.h"
#include "richset.h"

/*
 * Initialise the US-ASCII character set processor.
 */
usascii_init (name)
char	*name;
{
    if (name)
	richtextencoding (RICH_ENC_US_ASCII);
}

/*
 * Process a command for the US-ASCII processor.
 */
int	usascii_command (token,negated)
char	*token;
int	negated;
{
    if (!strcmp(token,"us-ascii")) {
    	if (negated) {
	    charsetpop (&usascii_charset);
	} else {
	    charsetpush (&usascii_charset);
	    richtextencoding (RICH_ENC_US_ASCII);
	}
	return (1);
    } else {
    	return (0);
    }
}

/*
 * Check for singleton US-ASCII tokens.
 */
int	usascii_single (token)
char	*token;
{
    return (0);
}

/*
 * Determine the width of a US-ASCII character.
 */
int	usascii_width (ch)
RCHAR	ch;
{
    return (1);
}

/*
 * Determine if the current character can be used as a folding point.
 */
int	usascii_fold (ch)
RCHAR	ch;
{
    return (ch < 0x7F && isspace (ch));
}

/*
 * Render the given US-ASCII character.
 */
usascii_render (ch,param)
RCHAR	ch;
void	*param;
{
    (*RichtextPutc) ((int)ch,param);
}

/*
 * Enter or leave the US-ASCII encoding.
 */
usascii_encoding (newenc)
int	newenc;
{
    /* Nothing to be done in this version */
}

/*
 * Define the US-ASCII character set processor.
 */
struct 	charsetproc	usascii_charset =
	  {"us-ascii",
	   usascii_init,
	   usascii_command,
	   usascii_single,
	   usascii_width,
	   usascii_fold,
	   usascii_render,
	   usascii_encoding};

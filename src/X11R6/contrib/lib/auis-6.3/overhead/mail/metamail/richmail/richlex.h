/*-------------------------------------------------------------------------

  richlex.h - Lexical analysis routines for parsing richtext messages.
 
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
     1.0    31/01/92  RW  Original Version of richlex.h
     1.1    19/06/92  RW  Add support for multi-byte ISO-2022 codes.

  You may contact the author by:
  =============================

   e-mail: rhys@cs.uq.oz.au
     mail: Rhys Weatherley
	   5 Horizon Drive
	   Jamboree Heights
	   Queensland 4074
	   Australia

-------------------------------------------------------------------------*/

#ifndef	__RICHTEXT_H__
#define	__RICHTEXT_H__

#ifdef	__cplusplus
extern "C" {
#endif

/*
 * Define the data type to be used by characters in the richtext
 * system.  Note: this is a long because some machines, MS-DOS
 * machines for example, only have 16-bit integers and hence won't
 * be able to cope with large multi-byte character codes.  If
 * something is strange with your character processing, it is
 * probably because you aren't using the right types.
 */
typedef	long	RCHAR;

/*
 * Set the following variable to zero to disable the
 * correction of richtext commands.  The default
 * value is non-zero.
 */
extern	int CorrectionEnabled;

/*
 * Set the following value to non-zero to enable the
 * multi-byte '<' hack which ignores richtext commands
 * in multi-byte modes.  The default value is zero.
 */
extern	int RichtextLessThanFlag;

/*
 * Define the function to call to get characters from
 * the message.  The calling convention of this
 * function is: "int func (void *param)".  The default
 * value is "fgetc".  The function must return EOF
 * at the end of the messsage;
 */
extern	int (*RichtextGetc) ();

/*
 * Define the function to call to output characters from
 * richtextcorrect.  The calling convention of this
 * function is: "int func (int c,void *param)".  The default
 * value is "fputc".
 */
extern	int (*RichtextPutc) ();

/*
 * Define the maximum size of richtext command tokens.
 */
#define	MAX_TOKEN_SIZE		50

/*
 * Define the special token values that are returned by
 * the "richtextlex" function.  These values were chosen
 * to keep away from legal ASCII.
 *
 * Version 1.1: modified to negative values to keep away
 * from legal ISO-2022 and other multi-byte characters.
 */
#define	RICHTEXT_COMMAND	((RCHAR)(-2))
#define	RICHTEXT_NEG_COMMAND	((RCHAR)(-3))

/*
 * Reset the richtext parsing mechanism.
 */
extern	richtextreset();

/*
 * Get the next token from the input stream.  RICHTEXT_COMMAND
 * or RICHTEXT_NEG_COMMAND are returned if it is a richtext command.
 * e.g. "<cmd>" or "</cmd>".  The "token" buffer will receive the
 * name of the command (without <,> or /) if it is a command.  This
 * function will also truncate commands longer than MAX_TOKEN_SIZE - 1
 * characters and abort command parsing if white space is encountered,
 * so, for example, errors like "<bold hi kids</bold>" don't cause
 * problems: it will be corrected to "<bold>hi kids</bold>".
 * The "file" parameter is passed to the function pointed to by
 * "RichtextGetc" on each call.
 */
extern	RCHAR	richtextlex( /* void *file,char *token */ );

/*
 * Read the input stream, correct the richtext, and write the
 * results to the output stream.  "outparam" is passed to the
 * "RichtextPutc" function as the second argument, and "inparam"
 * is passed to "richtextlex" during parsing.
 */
extern	richtextcorrect( /* void *inparam,void *outparam */ );

#define	RICH_ENC_US_ASCII	0	/* US-ASCII encoding: one-byte */
#define	RICH_ENC_JP_ASCII	1	/* JP-ASCII encoding: one-byte */
#define	RICH_ENC_KR_ASCII	2	/* KR-ASCII encoding: one-byte */
#define	RICH_ENC_JIS_1978	100	/* JIS-X-0208-1978: two-byte */
#define	RICH_ENC_JIS_1983	101	/* JIS-X-0208-1983: two-byte */
#define	RICH_ENC_KSC_5601	200	/* KSC-5601 (Korean): two-byte */

/*
 * The following variable contains the current character
 * encoding in use.  This should only be read.  Setting it
 * is done via "richtextencoding".
 */
extern	int	RichtextCharEncoding;

/*
 * Change the encoding used for characters not present in
 * richtext command sequences.
 */
extern	richtextencoding( /* int encoding */ );

/*
 * Define a number of macros for decoding multi-byte character
 * codes.  The names of the macros have the form "RICHCHn_xxx"
 * where "n" is the number of bytes in the character.
 */
#define	RICHCH2_FIRST(c)	(((c) >> 8) & 0xFF)
#define	RICHCH2_SECOND(c)	((c) & 0xFF)

#ifdef	__cplusplus
};
#endif

#endif	/* __RICHTEXT_H__ */

/*-------------------------------------------------------------------------

  richlex.c - Lexical analysis routines for parsing richtext messages.
 
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
     1.0    31/01/92  RW  Original Version of richlex.c
     1.1    19/06/92  RW  Add support for multi-byte ISO-2022 codes.

  You may contact the author by:
  =============================

   e-mail: rhys@cs.uq.oz.au
     mail: Rhys Weatherley
	   5 Horizon Drive
	   Jamboree Heights
	   Queensland 4074
	   Australia

  Caveats:
  =======

  If a multi-byte character contains "<lt>", and a richtext command or
  escape sequence is started before all bytes of the multi-byte character
  have been read in, then some characters may be discarded.

-------------------------------------------------------------------------*/

#include <stdio.h>
#include <ctype.h>
#include "richlex.h"
#include "richset.h"

int CorrectionEnabled = 1;	/* Zero if correction has been disabled */
int RichtextLessThanFlag = 0;	/* Non-zero to turn on multi-byte '<' hack */

#ifndef AMIGA
extern	int fgetc ();
extern	int fputc ();
#endif

int (*RichtextGetc) () = fgetc;	/* Function to call to get characters */
int (*RichtextPutc) () = fputc; /* Function to call to put characters */
int RichtextCharEncoding = RICH_ENC_US_ASCII; /* Current encoding mode */

#define	MAX_STACK_SIZE	500
#define	MAX_FLUSH_SIZE	3
#define	MAX_PUSH_BACK	20
static	int	StackSize=0;
static	char	Stack[MAX_STACK_SIZE][MAX_TOKEN_SIZE];
static	char	NextToken[MAX_TOKEN_SIZE];
static	int	FlushStack=0;
static	int	FlushSize=0;
static	int	EndInpFile=0;
static	int	CharSize=1;
static	int	PushbackBuffer[MAX_PUSH_BACK];
static	int	PushbackSize=0;
static	int	PushbackExtract=0;

#define	ESC	033
#define	SI	017
#define	SO	016

#define	RGETRAW(f)	((*RichtextGetc)(f))
#define	RGET(f)		(PushbackSize ? richtextgetback() : RGETRAW(f))
#define	RPUT(c,f)	((*RichtextPutc)(((int)(c)),(f)))
#define	RUNGET(c)	(richtextunget(c))
#define	RPUSHBACK(c)	(richtextpushback(c))

/*
 * Define a "printf" format for a generic ISO-2022 character
 * set name that includes the hexadecimal representation of
 * the escape sequence character that turns ISO-2022 on or off
 * on the terminal.
 */
#define	ISO2022_GENERIC	"x-iso-2022-gen-%2x"
#define	ISO2022_CHARSET "x-iso-charset-"

/*
 * Define the character set shift characters for ISO-2022-KR.
 */
#define	ISO2022_SHIFTIN  "x-iso-shift-in"
#define	ISO2022_SHIFTOUT "x-iso-shift-out"

/*
 * Construct multi-byte character codes.
 */
#define	RICHCH_2(first,second)	((RCHAR)(((first) << 8) | (second)))

/*
 * Reset the richtext parsing mechanism.
 */
richtextreset()
{
    StackSize = 0;
    FlushStack = 0;
    FlushSize = 0;
    EndInpFile = 0;
    CharSize = 1;
    PushbackSize = 0;
    PushbackExtract = 0;
    RichtextCharEncoding = RICH_ENC_US_ASCII;
    CorrectionEnabled = 1;
    RichtextLessThanFlag = 0;
}

/*
 * Push a character into the push-back buffer for later
 * retrieval by RGET.
 */
static	void	richtextpushback(c)
int	c;
{
    PushbackBuffer[PushbackSize++] = c;
}

/*
 * Unget a character that has been read from the input stream.
 */
static	void	richtextunget(c)
int	c;
{
    if (PushbackSize)
	--PushbackExtract;	/* Character was retrieved from push-back */
    else
	richtextpushback(c);	/* Put character into empty push-back */
}

/*
 * Unget two characters that have been read from the input stream.
 */
static	void	richtextunget2(c1,c2)
int	c1,c2;
{
    if (PushbackExtract > 1) {
	PushbackExtract -= 2;	/* Go back two characters in the push-back */
	PushbackBuffer[PushbackExtract++] = c1;
	PushbackBuffer[PushbackExtract++] = c2;
    } else {
	richtextpushback(c1);	/* Put the characters into the push-back */
	richtextpushback(c2);
    }
}

/*
 * Retrieve a character from the push-back buffer.
 */
static	int	richtextgetback()
{
    int c;
    c = PushbackBuffer[PushbackExtract++];
    if (PushbackExtract >= PushbackSize) {
	PushbackSize = 0;
	PushbackExtract = 0;
    }
    return(c);
}

/*
 * Find a match between NextToken and an element on the stack.
 * Returns the number of elements down from the top it is.
 * i.e. 0 if not on the stack, 1 if at the top, etc.
 */
static int richtextmatchup()
{
    int i = StackSize;
    while (i > 0 && i > (StackSize - MAX_FLUSH_SIZE)) {
	--i;
	if (!strcmp(NextToken,Stack[i]))
	    return(StackSize - i);
    }
    return(0);
}

/*
 * Determine if the current token is one of the singleton
 * richtext commands: <nl>, <lt>, <np>.
 */
static richtextsingle()
{
    return (charsetsingle (NextToken) ||
	    !strcmp(NextToken,"nl") ||
	    !strcmp(NextToken,"lt") ||
	    !strcmp(NextToken,"np"));
}

/*
 * Recognise a character that can start a richtext command.
 */
#define	iscmdch(c)	(isalpha(c) || isdigit(c) || (c) == '/' || (c) == '-')
#define iscmdch2(c)	(isalpha(c) || isdigit(c) || (c) == '-')
#define TOLOWER(c)	(isupper(c)?tolower(c):c)
#define valid_command(c1,c2) \
	(( c1 == '/' && iscmdch2(c2) ) || \
	 ( TOLOWER(c1) == 'l' && TOLOWER(c2) == 't' ))

/*
 * Get the next token from the input stream.  RICHTEXT_COMMAND
 * or RICHTEXT_NEG_COMMAND are returned if it is a richtext command.
 * e.g. "<cmd>" or "</cmd>".  The "token" buffer will receive the
 * name of the command (without <,> or /) if it is a command.  This
 * function will also truncate commands longer than MAX_TOKEN_SIZE - 1
 * characters and abort command parsing if white space is encountered,
 * so, for example, errors like "<bold hi kids</bold>" don't cause
 * problems: it will be corrected to "<bold>hi kids</bold>".
 */
RCHAR richtextlex(file,token)
void *file;
char *token;
{
    int c,i,lastch;
    RCHAR cmd;

    lastch = 0;		/* No previous character for multi-byte chars as yet */

    /* Perform any flushing of balancing commands that is necessary */
    if (FlushStack) {
	/* Flush out some extra closing commands */
	strcpy(token,Stack[StackSize - FlushSize + (--FlushStack)]);
	return(RICHTEXT_NEG_COMMAND);
    } else if (FlushSize) {
	/* Finished flushing: output the pending close command */
	StackSize -= FlushSize;
	if (StackSize > 0)
	    --StackSize; /* Remove the command that was being matched up */
	FlushSize = 0;
	strcpy(token,NextToken);
	if (EndInpFile)
	    return((RCHAR)EOF); /* The last flush was the end-of-file cleanup */
	else
	    return(RICHTEXT_NEG_COMMAND);
    }

    /* Fetch a new character or richtext command */
    for (;;) {		/* Loop so we can come back on ignored commands */
        c = RGET(file);
        if (c == '<') { 
	    /* Check for multi-byte mode, where "<" is special */
	    int c2;
	    if (CharSize > 1) {
		if (RichtextLessThanFlag) {
		    /* The multi-byte '<' hack is in effect: not a command */
		    if (lastch)
		        return(RICHCH_2(lastch,'<'));
		    lastch = '<';
		    continue;	/* Back around for another character */
		}
		if ((c = RGET(file)) == EOF) {
		    RUNGET(c);
		    return((RCHAR)'<');
		}
		c2 = RGET(file);
		richtextunget2(c,c2);
		if( !valid_command(c,c2) ){
		    /* We have a stray less-than symbol */
	            if (lastch)
		        return(RICHCH_2(lastch,'<'));
	            lastch = '<';
	            continue;	/* Back around for another character */
		}
	    }

	    /* Read a command token from the input file */
	    cmd = RICHTEXT_COMMAND;
	    if ((c = RGET(file)) == '/') {
	        cmd = RICHTEXT_NEG_COMMAND;
	        c = RGET(file);
	    }
            for (i = 0; i < (MAX_TOKEN_SIZE - 1) && c != '>'
	    		&& c != EOF && !isspace(c); ++i) {
                NextToken[i] = isupper(c) ? tolower(c) : c;
	        c = RGET(file);
            }
	    if (c != '>' && c != EOF && !isspace(c)) {
		/* We have a long command: skip the rest of it */
		while (c != '>' && c != EOF && !isspace(c))
		    c = RGET(file);
	    }
            if (c == EOF) {
		if (!StackSize)
	    	    return((RCHAR)EOF);
	        /* Flush the remaining commands at the end of the input file */
	        FlushSize = StackSize;
	        FlushStack = FlushSize;
	        EndInpFile = 1;
	        return(richtextlex(file,token)); /* Flush something out */
	    }
            NextToken[i] = '\0';

	    /* Process <lt> specially for multi-byte characters */
	    if (CharSize > 1 && !strcmp(NextToken,"lt")) {
	        if (lastch)
		    return(RICHCH_2(lastch,'<'));
	        lastch = '<';
	        continue;	/* Back around for another character */
	    }

	    /* Check to see if we need to correct anything */
	    if (!CorrectionEnabled) {
		/* No correction to do: just skip the correction phase */
		strcpy(token,NextToken);
		return(cmd);
	    }
	    if (cmd == RICHTEXT_COMMAND) {
		/* Save the command on the stack if not a singleton command */
		if (!richtextsingle()) {
		    strcpy (Stack[StackSize++],NextToken);
		}
	    }
	    else if (!(i = richtextmatchup()))
		continue;	/* No matchup - just drop it */
	    else if (i == 1)
		--StackSize;	/* Correct match at the stack top */
	    else {
		/* Flush some correction elements from the stack */
		FlushSize = i - 1;
		FlushStack = FlushSize;
		return(richtextlex(file,token));
	    }
	    strcpy(token,NextToken);
	    return(cmd);
        } else if (c == SI) {
	    /* Shift-in character: translate to a singleton */
	    strcpy(token,ISO2022_SHIFTIN);
	    return(RICHTEXT_COMMAND);
	} else if (c == SO) {
	    /* Shift-out character: translate to a singleton */
	    strcpy(token,ISO2022_SHIFTOUT);
	    return(RICHTEXT_COMMAND);
	} else if (c == ESC) {
	    /* Check for escape sequences that change character sizes */
	    int newc;
	    c = RGET(file);
	    if (c == '$') {
		newc = RGET(file);
		if (newc == ')') {
		    newc = RGET(file);	/* 4-byte ESC-$-)-? sequence */
		    sprintf(token,ISO2022_CHARSET,newc);
		} else {
		    sprintf(token,ISO2022_GENERIC,newc);
		}
		return(RICHTEXT_COMMAND);
	    } else if (c == '(') {
		newc = RGET(file);
		sprintf(token,ISO2022_GENERIC,newc);
		return(RICHTEXT_NEG_COMMAND);
	    } else {
		RUNGET(c);
		return((RCHAR)ESC);
	    }
        } else if (c == EOF && StackSize) {
	    /* Flush the remaining commands at the end of the input file */
	    FlushSize = StackSize;
	    FlushStack = FlushSize;
	    EndInpFile = 1;
	    return(richtextlex(file,token)); /* Flush something out */
	} else if (CharSize > 1) {
	    /* Recognise a multi-byte character */
	    int newc;
	    if (!lastch && isspace (c))
	      return ((RCHAR)c); /* Hack for spaces in 2-byte modes */
	    if (lastch)
		return (RICHCH_2(lastch,c)); /* This is second of 2 chars */
	    if ((newc = RGET(file)) == EOF) {
		RUNGET(newc);	/* Push the EOF back into the input stream */
		return((RCHAR)c); /* Just return the partial single-byte char */
	    } else if (newc == '<') {
		/* The second character could be "<lt>", so loop around */
		lastch = c;
		RUNGET(newc);
		continue;
	    } else {
	    	return(RICHCH_2(c,newc));
	    }
	} else {
	    return((RCHAR)c);
	}
    }
}

/*
 * Output a string via "RichtextPutc".
 */
static richtextoutstr(str,outparam)
char *str;
void *outparam;
{
    while (*str) {
	RPUT(*str,outparam);
	++str;
    }
}

/*
 * Read the input stream, correct the richtext, and write the
 * results to the output stream.
 */
richtextcorrect(inparam,outparam)
void *inparam,*outparam;
{
    RCHAR c;
    char token[MAX_TOKEN_SIZE];
    while ((c = richtextlex(inparam,token)) != (RCHAR)EOF) {
	if (c == RICHTEXT_COMMAND) {
	    RPUT('<',outparam);
	    richtextoutstr(token,outparam);
	    RPUT('>',outparam);
	} else if (c == RICHTEXT_NEG_COMMAND) {
	    RPUT('<',outparam);
	    RPUT('/',outparam);
	    richtextoutstr(token,outparam);
	    RPUT('>',outparam);
	} else if (c >= 256) {
	    RPUT(RICHCH2_FIRST(c),outparam);
	    RPUT(RICHCH2_SECOND(c),outparam);
	} else {
	    RPUT(c,outparam);
	}
    }
}

/*
 * Change the encoding used for characters not present in
 * richtext command sequences.
 */
richtextencoding(encoding)
int	encoding;
{
    RichtextCharEncoding = encoding;
    switch (RichtextCharEncoding) {
	case RICH_ENC_US_ASCII:
	case RICH_ENC_JP_ASCII:
	case RICH_ENC_KR_ASCII:
		CharSize = 1;
		break;

	case RICH_ENC_JIS_1978:
	case RICH_ENC_JIS_1983:
	case RICH_ENC_KSC_5601:
		CharSize = 2;
		break;

	default:CharSize = 1;
		break;
    }
}

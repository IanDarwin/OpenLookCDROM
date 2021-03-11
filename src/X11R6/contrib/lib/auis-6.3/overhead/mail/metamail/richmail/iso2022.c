/*-------------------------------------------------------------------------

  iso2022.c - Code for the ISO-2022 specific parts of the richtext processor.
 
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
     1.0    21/06/92  RW  Original Version of iso2022.c

  You may contact the author by:
  =============================

   e-mail: rhys@cs.uq.oz.au
     mail: Rhys Weatherley
	   5 Horizon Drive
	   Jamboree Heights
	   Queensland 4074
	   Australia

  Acknowledgements:
  ================

  Many thanks to Yutaka Sato (ysato@etl.go.jp) for protyping ISO-2022
  support in a previous version of richtext, on whose code this is based.

-------------------------------------------------------------------------*/

#include <stdio.h>
#include <ctype.h>
#include "richlex.h"
#include "richset.h"

/*
 * Global data for this module.
 */
static	int	SwToAscii;
static	int	SwToOther;
static	int	OutPrevChar;
static	int	OutPrevPrevChar;
static	int	OutCharLen;
static	int	OutCharSet;
static	int	OutAsciiMode;

#define	OUT_ASCII	0
#define	OUT_JIS		1
#define	OUT_KSC		2

#define	ESC		033
#define	SO		016
#define	SI		017

#define	ISO_GENERIC_PREFIX	"x-iso-2022-gen-"
#define	ISO_GENERIC_LEN		15
#define	ISO_SHIFT_PREFIX	"x-iso-shift-"
#define	ISO_SHIFT_LEN		12
#define	ISO_CHARSET_PREFIX	"x-iso-charset-"
#define	ISO_CHARSET_LEN		14

/*
 * Initialise the ISO-2022 character set processor.
 */
iso2022_init (name)
char	*name;
{
    SwToAscii = 'B';
    SwToOther = 'B';
    OutPrevChar = 0;
    OutPrevPrevChar = 0;
    OutCharLen = 1;
    OutCharSet = OUT_ASCII;
    OutAsciiMode = RICH_ENC_US_ASCII;
    if (!name)
	return;
    if (!strncmp (name,"iso-2022-jp",11))
	richtextencoding (RICH_ENC_JP_ASCII);
    else if (!strncmp (name,"iso-2022-kr",11)) {
	richtextencoding (RICH_ENC_KR_ASCII);
	controloutput("\033$)C",0);
    } else
	richtextencoding (RICH_ENC_US_ASCII);
}

/*
 * Process a command for the ISO-2022 processor.
 */
int	iso2022_command (token,negated)
char	*token;
int	negated;
{
    int swchar;
    if (!strcmp(token,"iso-2022-jp")) {
    	if (negated) {
	    /* Return to previous output mode */
	    controloutput("\033(B",0);
	    SwToAscii = 'B';
	    charsetpop(&iso2022_charset);
	} else {
	    /* Enter JIS-X0208-1983 output mode */
	    charsetpush(&iso2022_charset);
	    richtextencoding(RICH_ENC_US_ASCII);
	    controloutput("\033(B",0);
	    SwToOther = 'B';
	}
	return (1);
    } else if (!strcmp(token,"iso-2022-kr")) {
	if (negated) {
	    /* Return to previous output mode */
	    controloutput("\017",0);
	    charsetpop(&iso2022_charset);
	} else {
	    /* Enter KSC-5601 output mode */
	    charsetpush(&iso2022_charset);
	    richtextencoding(RICH_ENC_KR_ASCII);
	    controloutput("\033$)C\017",0);
	}
	return (1);
    } else if (!strncmp(token,"x-jis-x0201",11)) {
    	if (negated) {
	    /* Return to previous output mode */
	    controloutput("\033(B",0);
	    SwToAscii = 'B';
	    charsetpop(&iso2022_charset);
	} else {
	    /* Enter JIS-X-0201-1976 output mode */
	    charsetpush(&iso2022_charset);
	    richtextencoding(RICH_ENC_JP_ASCII);
	    controloutput("\033(J",0);
	    SwToAscii = 'J';
	}
	return (1);
    } else if (!strncmp(token,"x-jis-x0208",11)) {
    	if (negated) {
	    /* Return to previous output mode */
	    controloutput("\033(B",0);
	    SwToAscii = 'B';
	    charsetpop(&iso2022_charset);
	} else {
	    /* Enter JIS-X0208-* output mode */
	    charsetpush(&iso2022_charset);
	    if (!strcmp (token + 11,"-1978"))
		SwToOther = '@';
	    else
		SwToOther = 'B';
	    richtextencoding((SwToOther == '@' ? RICH_ENC_JIS_1978 :
					      RICH_ENC_JIS_1983));
	    controloutput("\033$",0);
	    controlputc(SwToOther);
	}
	return (1);
    } else if (!strcmp(token,"x-ksc-5601")) {
    	if (negated) {
	    /* Return to previous output mode */
	    controloutput("\017",0);
	    charsetpop(&iso2022_charset);
	} else {
	    /* Enter KSC-5601 output mode */
	    charsetpush(&iso2022_charset);
	    richtextencoding(RICH_ENC_KSC_5601);
	    controloutput("\033$)C\016",0);
	}
	return (1);
    } else if (!strncmp (token,ISO_GENERIC_PREFIX,ISO_GENERIC_LEN)) {
    	/* Process an escape sequence for changing character sets */
	sscanf(token + ISO_GENERIC_LEN,"%x",&swchar);
	if (swchar <= ' ' || swchar >= 0x7F)
	    return (1);
	if (negated) {
	    /* Return to previous output mode from multi-byte mode */
	    if (!charsettop(&iso2022_charset))
	      charsetpush(&iso2022_charset);
	    richtextencoding((swchar == 'J' ? RICH_ENC_JP_ASCII :
					      RICH_ENC_US_ASCII));
	    controloutput("\033(",0);
	    controlputc(swchar);
	    SwToAscii = swchar;
	} else {
	    /* Enter multi-byte (Japanese) mode */
	    if (!charsettop(&iso2022_charset))
	      charsetpush(&iso2022_charset);
	    richtextencoding((swchar == '@' ? RICH_ENC_JIS_1978 :
					      RICH_ENC_JIS_1983));
	    controloutput("\033$",0);
	    controlputc(swchar);
	    SwToOther = swchar;
	}
	return (1);
    } else if (!strncmp (token,ISO_SHIFT_PREFIX,ISO_SHIFT_LEN)) {
    	/* Process a character set shift sequence */
	if (!strcmp(token + ISO_SHIFT_LEN,"out") &&
	    	RichtextCharEncoding == RICH_ENC_KR_ASCII) {
	    /* Enter KSC-5601 2-byte mode */
	    if (!charsettop(&iso2022_charset))
	      charsetpush(&iso2022_charset);
	    richtextencoding(RICH_ENC_KSC_5601);
	    controloutput("\016",0);
	} else if (!strcmp (token + ISO_SHIFT_LEN,"in") &&
		RichtextCharEncoding == RICH_ENC_KSC_5601) {
	    /* Return to US-ASCII from KSC-5601 */
	    if (!charsettop(&iso2022_charset))
	      charsetpush(&iso2022_charset);
	    richtextencoding(RICH_ENC_KR_ASCII);
	    controloutput("\017",0);
	}
	return (1);
    } else {
    	return (0);
    }
}

/*
 * Check for singleton ISO-2022 tokens.
 */
int	iso2022_single (token)
char	*token;
{
    return (!strncmp (token,ISO_GENERIC_PREFIX,ISO_GENERIC_LEN) ||
	    !strncmp (token,ISO_SHIFT_PREFIX,ISO_SHIFT_LEN) ||
	    !strncmp (token,ISO_CHARSET_PREFIX,ISO_CHARSET_LEN));
}

/*
 * Determine the width of a ISO-2022 character.
 */
int	iso2022_width (ch)
RCHAR	ch;
{
    return (ch & 0xFF00 ? 2 : 1);
}

/*
 * Determine if the current character can be used as a folding point.
 */
int	iso2022_fold (ch)
RCHAR	ch;
{
    if (ch < 0x7F && isspace (ch)) {
    	return (1);
    } else {
        return ((ch & 0xFF00) != 0);
    }
}

/*
 * Render the given ISO-2022 character.
 */
iso2022_render (ch,param)
RCHAR	ch;
void	*param;
{
    if (ch & 0xFF00) {
	if (OutCharLen < 2) {
	    /* Add extra escape sequences after stray ASCII characters */
	    /* This normally happens in excerpts and signatures, etc.  */
	    if (OutAsciiMode == RICH_ENC_JP_ASCII) {
		(*RichtextPutc) (033,param);
		(*RichtextPutc) ('$',param);
		(*RichtextPutc) (SwToOther,param);
	    } else if (OutAsciiMode == RICH_ENC_KR_ASCII) {
		(*RichtextPutc) (SO,param);
	    }
	}
        (*RichtextPutc) ((int)((ch & 0xFF00) >> 8),param);
        (*RichtextPutc) ((int)(ch & 0xFF),param);
    } else {
	if (OutCharLen > 1 && ch >= 0x20) {
	    /* Add extra escape sequences before stray ASCII characters */
	    /* This normally happens in excerpts and signatures, etc.   */
	    if (OutAsciiMode == RICH_ENC_JP_ASCII) {
		(*RichtextPutc) (033,param);
		(*RichtextPutc) ('(',param);
		(*RichtextPutc) (SwToAscii,param);
	    } else if (OutAsciiMode == RICH_ENC_KR_ASCII) {
		(*RichtextPutc) (SI,param);
	    }
	}
	(*RichtextPutc) ((int)(ch & 0xFF),param);
    }
}

/*
 * Enter or leave the ISO-2022 encoding.
 */
iso2022_encoding (newenc)
int	newenc;
{
    switch (newenc) {
	case RICH_ENC_US_ASCII: controloutput("\033(B",0); break;
	case RICH_ENC_JP_ASCII: controloutput("\033(J",0); break;
	case RICH_ENC_KR_ASCII: controloutput("\017",0);   break;
	case RICH_ENC_JIS_1978: controloutput("\033$@",0); break;
	case RICH_ENC_JIS_1983: controloutput("\033$B",0); break;
	case RICH_ENC_KSC_5601: controloutput("\016",0);   break;
	default:		controloutput("\033(B",0); break;
    }
}

/*
 * Define the ISO-2022-JP and ISO-2022-KR character set processor.
 */
struct 	charsetproc	iso2022_charset =
	  {"iso-2022-jp:iso-2022-kr",
	   iso2022_init,
	   iso2022_command,
	   iso2022_single,
	   iso2022_width,
	   iso2022_fold,
	   iso2022_render,
	   iso2022_encoding};

/*
 * Define an output routine for slotting into RichtextPutc so
 * that ISO-2022 escape sequences are treated correctly.
 */
int	iso2022_fputc (ch,file)
int	ch;
FILE	*file;
{
    if (OutPrevChar == ESC && ch == '(') {
	/* Process escape sequences that end JIS 2-byte modes */
	OutCharLen = 1;
	OutCharSet = OUT_ASCII;
	OutAsciiMode = RICH_ENC_JP_ASCII;
    }
    if (OutPrevPrevChar == ESC && OutPrevChar == '$') {
	/* Process escape sequences that start JIS 2-byte modes */
        if (ch != ')') {	/* ')' is for Korean, so ignore it */
	    OutCharLen = 2;
	    OutCharSet = OUT_JIS;
	    OutAsciiMode = RICH_ENC_JP_ASCII;
	}
    }
    if (ch == SO) {
	OutCharLen = 2;
	OutCharSet = OUT_KSC;
	OutAsciiMode = RICH_ENC_KR_ASCII;
    } else if (ch == SI) {
	OutCharLen = 1;
	OutCharSet = OUT_ASCII;
	OutAsciiMode = RICH_ENC_KR_ASCII;
    }
    if (ch == '\n' && OutCharLen == 2) {
        /* Two-byte characters cannot cross line boundaries */
	if (OutCharSet == OUT_JIS) {
            fputc (ESC,file);
            fputc ('(',file);
            fputc (SwToAscii,file);
            fputc ('\n',file);
            fputc (ESC,file);
            fputc ('$',file);
            fputc (SwToOther,file);
	} else if (OutCharSet == OUT_KSC) {
	    fputc (SI,file);
	    fputc ('\n',file);
	    fputc (SO,file);
	}
    } else {
        fputc (ch,file);
    }
    OutPrevPrevChar = OutPrevChar;
    OutPrevChar = ch;
}

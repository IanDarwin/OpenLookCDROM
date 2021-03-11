/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */

/*
	$Disclaimer: 
 * Permission to use, copy, modify, and distribute this software and its 
 * documentation for any purpose is hereby granted without fee, 
 * provided that the above copyright notice appear in all copies and that 
 * both that copyright notice, this permission notice, and the following 
 * disclaimer appear in supporting documentation, and that the names of 
 * IBM, Carnegie Mellon University, and other copyright holders, not be 
 * used in advertising or publicity pertaining to distribution of the software 
 * without specific, written prior permission.
 * 
 * IBM, CARNEGIE MELLON UNIVERSITY, AND THE OTHER COPYRIGHT HOLDERS 
 * DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
 * ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.  IN NO EVENT 
 * SHALL IBM, CARNEGIE MELLON UNIVERSITY, OR ANY OTHER COPYRIGHT HOLDER 
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
 * DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, 
 * WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS 
 * ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
 * OF THIS SOFTWARE.
 *  $
*/

#ifndef NORCSID
#define NORCSID
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/fonts/lib/RCS/fntnamng.c,v 2.7 1992/12/15 21:02:39 rr2b R6tape $";
#endif


 

/*********************************************************\
* 							  *
* 	File: fntnamng.c				  *
* 							  *
* Routines for manipulating font names.			  *
* 							  *
* HISTORY						  *
* 							  *
\*********************************************************/

#include "font.h"
#include <ctype.h>


/* ************************************************************ */
/*								*/
/*  FormatFontname						*/
/*								*/
/* ************************************************************ */

/*
	Create a FileName from a FontName structure 
*/

char *FormatFontname (n)
register struct FontName  *n;
{
   static char buf[128];
   char  rbuf[5];
   register char *p;

   /* first create the rotation substring */
   if (n->rotation)
      sprintf(rbuf, "r%d", n->rotation);
   else
      rbuf[0] = '\0';

   /* format the various pieces together */
   sprintf(buf, "%s%d%s%s%s%s%s",
	 n->FamilyName,
	 n->height,
	 rbuf,
	 n->FaceCode & BoldFace ? "b" : "",
	 n->FaceCode & ItalicFace ? "i" : "",
	 n->FaceCode & FixedWidthFace ? "f" : "",
	 n->FaceCode & ShadowFace ? "s" : "");

   /* make the file name all lower case */
   for (p = buf; *p; p++)
      if (isupper(*p))
	 *p = tolower(*p);

   return(buf);
}


/* ************************************************************ */
/*								*/
/*  parsefname							*/
/*								*/
/* ************************************************************ */

/*
	Take a file name (FileName) and parse it to create
	a FontName structure (Fontname)
*/

parsefname(FileName, Fontname)
register char *FileName;
register struct FontName  *Fontname;
{
   register char *p;
   register int   i;
   int   err = 0;

   /* strip off Family Name (e.g. TimesRoman); copy to struct */
   p = Fontname->FamilyName;
   i = sizeof(Fontname->FamilyName);
   while (isalpha(*FileName))
      if (--i > 0)
	 *p++ = *FileName++;
      else
	 FileName++;
   *p = '\0';
   if (FileName[0] == '\0')
      return(1) /* no Family Name */ ;

   /* now pick up the point size */
   Fontname->height = 0;
   while (isdigit(*FileName))
      Fontname->height = Fontname->height * 10 + (*FileName++ - '0');
   if (Fontname->height == 0)
      return(1) /* no point size */ ;

   /* rotation and facecodes are optional */
   Fontname->rotation = 0;
   Fontname->FaceCode = 0;
   while (*FileName != '\0')
      switch (*FileName++)
	 {
	    case 'b': 
		  Fontname->FaceCode |= BoldFace;
		  break;
	    case 'i': 
		  Fontname->FaceCode |= ItalicFace;
		  break;
	    case 'f': 
		  Fontname->FaceCode |= FixedWidthFace;
		  break;
	    case 'r': 
		  while (isdigit(*FileName))
		     Fontname->rotation = Fontname->rotation * 10
			+ *FileName++ - '0';
		  break;
	    case 's': /* Shadow font */
		  Fontname->FaceCode |= ShadowFace;
		  break;

	    case '.': /* ignore extensions of .fwm and .fdb */
		  if (strcmp(FileName, "fwm") == 0
			|| strcmp(FileName, "fdb") == 0)
		     FileName += 3;
		  break;

	    default: 
		  err++;
		  break;
	 }
   return(err);
}

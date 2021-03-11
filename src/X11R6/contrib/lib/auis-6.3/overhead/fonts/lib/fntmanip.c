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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/fonts/lib/RCS/fntmanip.c,v 2.9 1992/12/15 21:02:39 rr2b R6tape $";
#endif


 


/* ***************************************************************** */

/*	fntmanip.c	Pack and unpack font structures */

/*	
	This module is designed to convert fonts from the internal
	wm format defined by font.h to an exploded format defined
	by fntmanip.h; this exploded format is more convenient for
	most programs to manipulate.

*/

/* ***************************************************************** */

/* missing type from C language */
#define Boolean short
#define true 1
#define false 0

/* ***************************************************************** */

#include <stdio.h>
#include "font.h"
#include "fntmanip.h"

/*
	The "fntmanip.h" header file defines an 4 data structures:

	fonthead  -- although this is a struct font, only the header
			fields are used, they store the name and
			global properties.

	generic[128]
	specific[128] -- the generic and specific structures for each
			character in the font

	bits[128] -- a pointer to the bit map; remember the actual
			bits are packed into shorts, padded with zeros
			to 16 bit multiples and then stored as 16-bit
			columns in column major

	MAXCOMMENTS -- maximum number of comments that can be stored
	char *comments[MAXCOMMENTS] -- the comments
	int ncomments -- the number of comments
*/

/*
	ExplodeFont (f) takes the font f and puts its parts in
	fonthead, generic, specific, bits, and into comments

	ComputeGenericFromSpecific computes the generic information from
	the specific, and the global font information as the maximums
	of the character information. 

	ImplodeFont converts the exploded font (in fonthead,
	generic, specific, bits, and comments) and writes it to a file
	in compressed wm format.
*/

/*
	One possible problem is that ImplodeFont not only compresses
	and writes the font to the file, it also decides where to
	put it.  If you want to decide the file to write it to
	yourself, use WriteExplodedFont(FILE *) instead.
*/

/* ***************************************************************** */

#if (defined(vax) || defined(MIPSEL))
static unsigned char inv[] = {
			      0x0,0x8,0x4,0xc,
			      0x2,0xa,0x6,0xe,
			      0x1,0x9,0x5,0xd,
			      0x3,0xb,0x7,0xf};
unsigned char reverse(c)
unsigned char c;
{
return (inv[c & 0x0f] << 4) | inv[c >> 4];
}


WordReverse(i)
register short i;
       {
	struct TwoChar{
	    unsigned char c1;
	    unsigned char c2;
	} *word;
	unsigned char tmp;
	int j;
	int WordsPerColumn=((specific[i].cols) +15)/16;
	int WordsPerRow = specific[i].rows;
	int BytesPerIcon = WordsPerColumn * WordsPerRow;
        for (j=0;j<BytesPerIcon ; j++)
	    {word =((struct TwoChar *)(bits[i]+j));

	     tmp = reverse(word->c1);
	     word->c1 = reverse (word->c2);
	     word->c2 = tmp;


	    }
       }
#endif /* vax || MIPSEL */



ExplodeFont(f)
register struct font *f;
{
   register short i;
   struct BitmapIconSpecificPart *maxspecific;
   register char *commentp;

   /* copy font header information */
   fonthead = *f;

   /* now copy the generic, specific and bits for each character */
   maxspecific = 0;
   for (i = 0; i <= 0177; i++)
      {
	 register struct icon *p = &f->chars[i];
	 if (p->OffsetToSpecific != 0 && p->OffsetToGeneric != 0)
	    {
	       register struct BitmapIconSpecificPart *SPp = SpecificPart(p);
	       generic[i] = *GenericPart(p);
	       specific[i] = *SPp;
	       bits[i] = (unsigned short *)(SPp->bits);
#if (defined(vax) || defined(MIPSEL))
	       WordReverse(i);
#endif /* vax || MIPSEL */
	       if  (SPp > maxspecific)
		maxspecific = SPp;
	    }
	 else
	    {
	       /* generic or specific part missing, use zero */
	       /* font structure values must interpret zero as a
	          reasonable default; depends on memory being
	          initialized to zero by C */
	       static struct IconGenericPart GenericZero;
	       static struct BitmapIconSpecificPart   SpecificZero;

	       generic[i] = GenericZero;
	       specific[i] = SpecificZero;
	       bits[i] = 0;
	    }
      }
    /* find the comments.  They are after the bits for the character
	with largest address for specific part 
	the end is marked with a '\0' put there by ReadFwmFont in fontio.c */
    commentp = (char *)(maxspecific->bits 
		+ (maxspecific->cols + 15) / 16 * maxspecific->rows);
    ncomments = 0;
    while (*commentp && ncomments<MAXCOMMENTS) {
	comments[ncomments++] = commentp;
	while (*commentp && *commentp != '\n')
	    commentp++;
	if (*commentp == '\n') 
	    *commentp++ = '\0';
    }
}

/* ***************************************************************** */

ComputeGenericFromSpecific()
{
/*
	ComputeGenericFromSpecific computes the generic information from
	the specific, and the global font information as the maximums
	of the character information. 
*/

/* 
   for a bit map representation, we have:

	rows, cols -- the size of the bounding box
	orow, ocol -- the position of the origin
*/

/**************************************************\
* 						   *
* 	The generic representation is:		   *
* 						   *
* 						   *
* 	           North			   *
*         |---------WtoE----------->|		   *
*      -  +-------------------------+		   *
*      |  |          /             /|		   *
*      |  |         /             / |		   *
*      |  |        /             /  |		   *
*      |  |       /             /   |		   *
*      N  |      /             /    |	East	   *
*      t  |     /             /     |		   *
*      o  |    /             /      |		   *
*      S  |---*-------------/-------|		   *
*      |  |  / Origin      /        |		   *
*      |  | /             /         |		   *
*      V  |/             /          |		   *
*      -  +-------------------------+		   *
*         <---|---Spacing-->|			   *
*           ^Wbase				   *
* 	        South				   *
* 						   *
\**************************************************/

   if (fonthead.fn.rotation == 0)
      {
	 register struct IconGenericPart *p;
	 register short c;
	 short    above = 0;
	 short    below = 0;
	 short    left = 0;
	 short    right = 0;

	 for (c = 0; c <= 0177; c++)
	    {
	       register short t;

	       /* minimal check of consistency of specific */
	       if (specific[c].rows == 0 || specific[c].cols == 0)
		  {
		     specific[c].rows = 0;
		     specific[c].cols = 0;
		     specific[c].orow = 0;
		     specific[c].ocol = 0;
		  }

	       /* define generic from specific */
	       p = &generic[c];
	       p->NWtoOrigin.x = BUG(specific[c].ocol);
	       p->NWtoOrigin.y = BUG(specific[c].orow);
	       p->Wbase.x = -BUG(specific[c].ocol);
	       p->Wbase.y = 0;
	       p->NtoS.x = 0;
	       p->NtoS.y = specific[c].rows;
	       p->WtoE.x = specific[c].cols;
	       p->WtoE.y = 0;

	       /* compute the maximums over all characters */
	       if (p->NWtoOrigin.x > left)
		  left = p->NWtoOrigin.x;
	       if (p->NWtoOrigin.y > above)
		  above = p->NWtoOrigin.y;
	       if ((t = p->NtoS.y - p->NWtoOrigin.y) > below)
		  below = t;
	       if ((t = p->WtoE.x - p->NWtoOrigin.x) > right)
		  right = t;
	    }

	 /* save the font global information */
	 fonthead.NWtoOrigin.x = left;
	 fonthead.NWtoOrigin.y = above;
	 fonthead.Wbase.x = -left;
	 fonthead.Wbase.y = 0;
	 fonthead.NtoS.x = 0;
	 fonthead.NtoS.y = above + below;
	 fonthead.WtoE.x = left + right;
	 fonthead.WtoE.y = 0;
	 fonthead.newline.x = 0;
	 fonthead.newline.y = above + below;
      }
}

/* ***************************************************************** */

char *DetermineFontFileForWriting (FontFileFormat)
int FontFileFormat;
{
   char *FileName;
   static char OutputFileName[255];
   register char *p;
   register char *d;

   /* get the font file name */
   FileName = (char *) FormatFontname(&fonthead.fn);

   /* get the fontpath, if no fontpath use usr/andrew/fonts */
   p = (char *) getprofile("fontpath");
   if (p == (char *) NULL)
      p = (char *) AndrewDir("/fonts");

   /* copy the first directory name in front of the fontname */
   d = OutputFileName;
   while (*p != '\0' && *p != ':')
      *d++ = *p++;
   /* if there is a directory name, add a separator */
   if (d != OutputFileName)
      *d++ = '/';
   /* now copy the file name */
   strcpy(d, FileName);

   /* add the proper extension: .fwm or .fdb */
   if (FontFileFormat == 0)
	strcat(OutputFileName,".fwm");
   else strcat(OutputFileName,".fdb");

   return(OutputFileName);
}


/* ***************************************************************** */

WriteExplodedFont(outf,FontFileFormat)
FILE * outf;
int FontFileFormat;
{
    if (FontFileFormat == 0)
	WriteFwmFont(outf);
    else  {
#ifdef WRITEFDB
	WriteFdbFont(outf);
#endif /* WRITEFDB */
    }
}

WriteFwmFont(outf)
FILE *outf;
{
    short matchg[128];
    short matchs[128];
    int UniqueGenerics = 0;
    int UniqueSpecifics = 0;
    register short c;
    register short d;

    fonthead.magic = FONTMAGIC;
    fonthead.type = BitmapIcon;
    for (c = 0; c <= 0177; c++) {
	specific[c].type = BitmapIcon;
	matchg[c] = -1;
	UniqueGenerics += 1;
	for (d = 0; d < c; d++)
	    if (generic[c].Spacing.x == generic[d].Spacing.x &&
		    generic[c].Spacing.y == generic[d].Spacing.y &&
		    generic[c].NWtoOrigin.x == generic[d].NWtoOrigin.x &&
		    generic[c].NWtoOrigin.y == generic[d].NWtoOrigin.y &&
		    generic[c].NtoS.x == generic[d].NtoS.x &&
		    generic[c].NtoS.y == generic[d].NtoS.y &&
		    generic[c].WtoE.x == generic[d].WtoE.x &&
		    generic[c].WtoE.y == generic[d].WtoE.y &&
		    generic[c].Wbase.x == generic[d].Wbase.x &&
		    generic[c].Wbase.y == generic[d].Wbase.y) {
		matchg[c] = d;
		UniqueGenerics -= 1;
		break;
	    }
	matchs[c] = -1;
	UniqueSpecifics += 1;
	for (d = 0; d < c; d++) {
	    register unsigned short *p1;
	    register unsigned short *p2;
	    register n;

	    p1 = bits[c];
	    p2 = bits[d];
	    n = (specific[c].cols + 15) / 16 * specific[c].rows;
	    if (p1 && p2)
		while (n > 0 && *p1++ == *p2++)
		    n--;
	    if (n == 0 &&
		    specific[c].type == specific[d].type &&
		    specific[c].rows == specific[d].rows &&
		    specific[c].cols == specific[d].cols &&
		    BUG(specific[c].orow) == BUG(specific[d].orow) &&
		    BUG(specific[c].ocol) == BUG(specific[d].ocol)) {
		matchs[c] = d;
		UniqueSpecifics -= 1;
		break;
	    }
	}
    }
    {
	short   CurGOffset = sizeof (struct font);
	short   CurSOffset = sizeof (struct font) + UniqueGenerics * sizeof (struct IconGenericPart);

#define nbsz (((int)specific[0].bits)-((int)&specific[0]))

	for (c = 0; c <= 0177; c++) {
	    if (matchs[c] >= 0)
		fonthead.chars[c].OffsetToSpecific = fonthead.chars[matchs[c]].OffsetToSpecific;
	    else {
		fonthead.chars[c].OffsetToSpecific = CurSOffset;
		CurSOffset += nbsz
		    + ((specific[c].cols + 15) / 16 * specific[c].rows) * 2;
	    }
	    if (matchg[c] >= 0)
		fonthead.chars[c].OffsetToGeneric = fonthead.chars[matchg[c]].OffsetToGeneric;
	    else {
		fonthead.chars[c].OffsetToGeneric = CurGOffset;
		CurGOffset += sizeof(struct IconGenericPart);
	    }
	}

	for (c = 0; c <= 0177; c++) {
	    register struct icon   *p = &fonthead.chars[c];
	    int     offset = ((int) p) - ((int) & fonthead);
	    p->OffsetToSpecific -= offset;
	    p->OffsetToGeneric -= offset;
	}

	fonthead.magic = FONTMAGIC;
	fonthead.NonSpecificLength = CurGOffset;
	fwrite(&fonthead, sizeof fonthead, 1, outf);
	for (c = 0; c <= 0177; c++)
	    if (matchg[c] < 0)
		fwrite(&generic[c], sizeof generic[c], 1, outf);
	for (c = 0; c <= 0177; c++)
	    if (matchs[c] < 0) {
		fwrite(&specific[c], nbsz, 1, outf);
#if (defined(vax) || defined(MIPSEL))	
		WordReverse(c);	/*for output only*/
#endif /* vax || MIPSEL */
		fwrite(bits[c],(specific[c].cols + 15) / 16 * specific[c].rows * 2,
			1, outf);
#if (defined(vax) || defined(MIPSEL))	
		WordReverse(c);	/* back to previous for internal use */
#endif /* vax || MIPSEL */


	    }
    }

    {
	int i;
	for (i=0; i<ncomments; i++) {
	    fprintf(outf, "%s\n", comments[i]);
	}
    }    

    fclose(outf);
}


/* ***************************************************************** */

short ImplodeFont (FontFileFormat)
int FontFileFormat;
{
   char *OutputFontFileName;
   FILE * OutputFile;

   OutputFontFileName = DetermineFontFileForWriting(FontFileFormat);
   OutputFile = fopen(OutputFontFileName, "w");
   if (OutputFile == (FILE *) NULL)
      return(1);

   WriteExplodedFont(OutputFile,FontFileFormat);
   return(0);
}

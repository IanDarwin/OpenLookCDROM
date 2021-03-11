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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/fonts/lib/RCS/fontio.c,v 2.7 1992/12/15 21:02:39 rr2b R6tape $";
#endif


 

/* ************************************************************ *\

	fontio.c	Input/Output of fonts.


\* ************************************************************ */

#include "font.h"
#include <stdio.h>
#include <andrewos.h> /* sys/types.h */
#include <sys/stat.h> /* for fstat */
static unsigned char reverse_bits[] = {
	0x0, 0x8, 0x4, 0xc,
	0x2, 0xa, 0x6, 0xe,
	0x1, 0x9, 0x5, 0xd,
	0x3, 0xb, 0x7, 0xf
};
#ifdef READFDB
struct font *ReadFdbFont ();
#endif /* READFDB */

struct font *ReadFwmFont (FileName)
char *FileName;
{
    struct stat st;
    register struct font   *f;
    int     fd = open (FileName, 0);
    char *end;	/* point to end of space for f to set \0 */

 /* the fwm form of a font file is a straight binary dump */
 /* get the size of the file, allocate space and read it in */
 /* append a '\0' to terminate scan for comments in ExplodeFont */

    if (fd >= 0) {
	if (fstat(fd, &st) >= 0) {
	    int     n;
	    f = (struct font   *) malloc (st.st_size+1); /* Never freed */
	    n = read(fd, f, st.st_size);
	    close(fd);
	    end = ((char *)f) + st.st_size - 1;
	    *end = '\0';
	 if ((f->magic & 0xffff) == ((FONTMAGIC >> 8) | ((FONTMAGIC & 0377) << 8)))fbbswap(f);
	    return(f);
	}
	close(fd);
    }
    return(NULL);
}

/* ************************************************************ */
/*								*/
/*								*/
/* ************************************************************ */

char *FormatFontname () /* from fntnamng.c */ ;

struct font *GetFontFromFile (dir, n)
char *dir;
struct FontName  *n;
{
   char *basename;
   char  FullFileName[256];
   int   i /* length of FullFileName */ ;
   struct font *f;

   /* 
    Try to find the font file in the given directory.
    First try the .fwm extension (e.g. TimesRoman10i.fwm),
    Then try the .fdb extension (e.g. TimesRoman10i.fdb)
    Then try the simple file name (e.g. TimesRoman10i),
    */

   basename = FormatFontname(n);
   if (*dir == '\0')
      strcpy(FullFileName, basename);
   else
      sprintf(FullFileName, "%s/%s", dir, basename);
   i = strlen(FullFileName);

   /* try  .fwm */
   strcpy(&FullFileName[i], ".fwm");
   f = ReadFwmFont(FullFileName);
   if (f != NULL)
      return(f);
#ifdef READFDB
   /* try  .fdb */
   strcpy(&FullFileName[i], ".fdb");
   f = ReadFdbFont(FullFileName);
   if (f != NULL)
      return(f);
#endif /* READFDB */

   /* try  with no extension  */
   strcpy(&FullFileName[i], "");
   f = ReadFwmFont(FullFileName);
   if (f != NULL)
      return(f);

   return(NULL);
}

#define minimize(A,B) if(!A || A > B) A = B;
#define maximize(A,B) if(B > A) A = B;
#define sb(A) cc2 =(unsigned char *) &(A);cc1 = cc2++ ;tmp = *cc2;*cc2 = *cc1;*cc1=tmp
#define ssb(A) sb(A.x); sb(A.y)
fbbswap(foo)
struct font *foo;
{
    /*	Takes a font compiled on a byte reversed machine and converts it for use. 
	Assumes BITMAP type icons 	*/
    register unsigned char *cc1,*cc2,tmp;
    register int i;
    register struct BitmapIconSpecificPart *SPp ,*minspec,*maxspec;
    register struct IconGenericPart *GPp ,*mingen,*maxgen;
    register unsigned short *ip;
    sb(foo->magic);
    sb( foo->NonSpecificLength);
    sb(foo->fn.rotation) ;
    ssb(foo->NWtoOrigin);
    ssb(foo->NtoS);
    ssb(foo->WtoE);
    ssb(foo->Wbase);
    ssb(foo->newline);
    sb(foo->NIcons);
    minspec = NULL;	maxspec = NULL; 
    mingen = NULL ; maxgen = NULL;
    for (i = 0; i <= 0177; i++)
    {
	register struct icon *p = &foo->chars[i];
	sb(p->OffsetToGeneric);
	sb(p->OffsetToSpecific );
	if (p->OffsetToSpecific != 0 && p->OffsetToGeneric != 0)
	{
	    SPp = SpecificPart(p);
	    GPp = GenericPart(p);
	    minimize(minspec, SPp);
	    maximize(maxspec, SPp);
	    minimize(mingen, GPp);
	    maximize(maxgen, GPp);
	}
    }
    for(GPp =mingen; GPp <=  maxgen; GPp++){
	ssb(GPp->Spacing);
	ssb(GPp->NWtoOrigin);
	ssb(GPp->NtoS);
	ssb(GPp->WtoE);
	ssb(GPp->Wbase);
    }
    for(SPp = minspec; SPp <= maxspec; SPp = (struct BitmapIconSpecificPart *) ip){
	i =  (SPp->cols  + 15) /16 * (SPp->rows); 
	for(ip = SPp->bits; i--; ip++){
	    *ip = (reverse_bits[*ip & 0xF]  << 4 ) | 
	      (reverse_bits[*ip >>4 & 0xF] ) |
	      (reverse_bits[*ip >>8 & 0xF] <<  12 ) | 
	      (reverse_bits[*ip >>12] << 8  );
	} 
    }
} 

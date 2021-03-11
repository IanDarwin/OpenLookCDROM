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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/fonts/cmd/RCS/wmfdb.c,v 2.7 1992/12/15 21:02:39 rr2b R6tape $";
#endif


 


/* ***************************************************************** */

/*	wmdb		convert a font file to an ASCII file */

/*	
	Read and parse a font file in wm format.
	Copy the font patterns to an ASCII file for
	storage in a data base, in human readable form.

*/

/* ***************************************************************** */


#include <stdio.h>
FILE * output;
char *InputFileName;

char  DirName[256];
char  FileName[256];
struct font *InputFont;

char *OutputFileName;
char *ExplicitOutputFileName = NULL;

#define Boolean char
#define byte char

#define false 0
#define true 1

#define writeln(p) (putc('\n',p),(void)fflush(p))

/* ***************************************************************** */
#include <andyenv.h>
#ifdef WM_ENV
#include "font.h"
#include "fntmanip.h"
#endif /* WM_ENV */
/* ************************************************************ */

/*
   The external representation of fonts is defined by font.h.

   The font structure is supported by:

	fntnamng.c
	=>	char *FormatFontname(n):
			 converts a struct Fontname
			 to a character string name

	=>	parsefname (name, n):
			 converts a string name to a struct Fontname.

		The struct Fontname has a separate field for the
		FamilyName (like "TimesRoman"), height (in points),
		rotation (0 to 360), FaceCode (bold, italic, ...).
		The struct Fontname is defined in font.h

	fntmanip.h
	fntmanip.c
	=>	ExplodeFont(f): 
			 converts the internal font.h format
			 of a font to a representation which
			 should be easier to work with.
			 This format is yet to be understood.


	font.h
	font.c
	=>	GetFontFromFile (dir,n):
			open the named font in the supplied 
			directory and read it in.  n is a 
			struct FontName which is converted to 
			a filename by FormatFontname.
*/
/* ************************************************************ */


Boolean debug = false;
Boolean verbose = true;
Boolean check = false;


/* ************************************************************ */

/* print representation of a character for debugging */
char *printrep (c)
int   c;
{
   static char s[5];

   if (32 < c && c < 127)
      {
	 s[0] = c;
	 s[1] = 0;
      }
   else
      if (c == 32)
	 {
	    s[0] = 's';
	    s[1] = 'p';
	    s[2] = 0;
	 }
   else
      if (c < 32)
	 {
	    s[0] = '^';
	    s[1] = c + 64;
	    s[2] = 0;
	 }
   else
      if (c == 127)
	 {
	    s[0] = 'd';
	    s[1] = 'e';
	    s[2] = 'l';
	    s[3] = 0;
	 }
   else
      {
	 sprintf(s, "\\%03o", c);
	 s[4] = 0;
      }
   return(s);
}

/* ************************************************************ */
unsigned char reverse_bits[] = {
	0x0, 0x8, 0x4, 0xc,
	0x2, 0xa, 0x6, 0xe,
	0x1, 0x9, 0x5, 0xd,
	0x3, 0xb, 0x7, 0xf
};
/* ************************************************************ */

/*								*/

/* ************************************************************ */


/* procedure DumpCharacter (c) */
DumpCharacter(c)
short    c /* character to dump */ ;
{
   register struct IconGenericPart *gp;
   register struct BitmapIconSpecificPart *sp;

   Boolean ShowType;
   Boolean ShowImage;
   Boolean ShowOrigin;
   Boolean ShowSpacing;
   Boolean ShowActualInfo;
   Boolean ShowNWtoOrigin;
   Boolean ShowNtoS;
   Boolean ShowWtoE;
   Boolean ShowWbase;
   Boolean ShowRedundantInfo;
   Boolean ShowCharacter;


   sp = SpecificPartOfChar(InputFont, c);
   gp = GenericPartOfChar(InputFont, c);

   /* Check What To Print */
   {
      ShowType = (sp->type != BitmapIcon);
      ShowImage = (sp->cols != 0 && sp->rows != 0);
      if (!ShowImage)
	 {
	    sp->cols = 0;
	    sp->rows = 0;
	 }
      ShowOrigin = (BUG(sp->ocol) != 0 || BUG(sp->orow) != 0);
      ShowSpacing = !(gp->Spacing.x == 0 && gp->Spacing.y == 0);
      ShowActualInfo = (ShowType || ShowImage || ShowOrigin || ShowSpacing);

      ShowNWtoOrigin = !(gp->NWtoOrigin.x == BUG(sp->ocol) && gp->NWtoOrigin.y == BUG(sp->orow));
      ShowNtoS = !(gp->NtoS.x == 0 && gp->NtoS.y == sp->rows);
      ShowWtoE = !(gp->WtoE.x == sp->cols && gp->WtoE.y == 0);
      ShowWbase = !(gp->Wbase.x == -BUG(sp->ocol) && gp->Wbase.y == 0);
      ShowRedundantInfo = (ShowNWtoOrigin || ShowNtoS || ShowWtoE || ShowWbase);
      ShowCharacter = (ShowActualInfo || ShowImage || ShowRedundantInfo);
   }

   if (check)
      if (ShowRedundantInfo)
	 fprintf(stderr, "Check character %s of %s\n", printrep(c), FileName);


   if (!ShowCharacter)
      return;
   fprintf(output, "$character %d %s\n", c, printrep(c));

   /* the fields that are expected to have real information content */

   if (ShowActualInfo)
      {
	 if (ShowType)
	    fprintf(output, "$specifictype %d BitmapIcon\n", sp->type);
	 if (ShowImage)
	    fprintf(output, "$box %d,%d\n", sp->cols, sp->rows);
	 if (ShowOrigin)
	    fprintf(output, "$origin %d,%d\n", BUG(sp->ocol), BUG(sp->orow));
	 if (ShowSpacing)
	    fprintf(output, "$spacing %d,%d\n", gp->Spacing.x, gp->Spacing.y);
      }

   /* the image */
   if (ShowImage)
      {
	 unsigned short *bitptr /* pointer to bit array */ ;
	 short    w /* width in shorts (16-bit chunks */ ;
	 register short j /* x direction; from 1 to width in shorts */ ;
	 register short i /* y direction; from 1 to height */ ;
	 register unsigned short *p /* pointer to packed bits */ ;
	 unsigned short lastwordmask;

	 fprintf(output, "$raster\n");
	 bitptr = bits[c];

	 /* compute number of shorts */
	 w = ((sp->cols + 15) / 16);
	 lastwordmask = ~0 << (16 * w - sp->cols /* extra bits in lastword */ );
	 for (i = 1; i <= sp->rows; i++)
	    {
	       p = &bitptr[i - 1];
	       for (j = 1; j < w; j++)
		  {

		     fprintf(output, "%04X", *p);
		     p += sp->rows;
		  }
	       /* last word */
	       *p = *p & lastwordmask;

	       fprintf(output, "%04X\n", *p);
	    }
      }

   /* the fields which are expected to be computed from others */
   if (ShowRedundantInfo)
      {
	 if (ShowNWtoOrigin)
	    fprintf(output, "$NWtoOrigin %d,%d\n", gp->NWtoOrigin.x, gp->NWtoOrigin.y);
	 if (ShowNtoS)
	    fprintf(output, "$NtoS %d,%d\n", gp->NtoS.x, gp->NtoS.y);
	 if (ShowWtoE)
	    fprintf(output, "$WtoE %d,%d\n", gp->WtoE.x, gp->WtoE.y);
	 if (ShowWbase)
	    fprintf(output, "$Wbase %d,%d\n", gp->Wbase.x, gp->Wbase.y);
      }

}


/* ************************************************************ */

/*								*/

/* ************************************************************ */


DumpFont()
{
   short    i;

   /* print overall font information */
   fprintf(output, "$magic %d\n", fonthead.magic);
   fprintf(output, "$fontname %s\n", FileName);
   fprintf(output, "$familyname %s\n", fonthead.fn.FamilyName);
   fprintf(output, "$rotation %d\n", fonthead.fn.rotation);
   fprintf(output, "$pointsize %d\n", fonthead.fn.height);
   if (fonthead.fn.FaceCode != 0)
      {
	 fprintf(output, "$facecodes %d ", fonthead.fn.FaceCode);
	 if ((BoldFace & fonthead.fn.FaceCode) != 0)
	    fprintf(output, "B");
	 if ((ItalicFace & fonthead.fn.FaceCode) != 0)
	    fprintf(output, "I");
	 if ((ShadowFace & fonthead.fn.FaceCode) != 0)
	    fprintf(output, "S");
	 if ((FixedWidthFace & fonthead.fn.FaceCode) != 0)
	    fprintf(output, "F");
	 fprintf(output, "\n");
      }
   /* dump the comments */
   for (i=0; i<ncomments; i++)
	fprintf(output, "$Comment %s\n", comments[i]);
   fprintf(output, "$MaxNWtoOrigin %d,%d\n", fonthead.NWtoOrigin.x, fonthead.NWtoOrigin.y);
   fprintf(output, "$MaxNtoS %d,%d\n", fonthead.NtoS.x, fonthead.NtoS.y);
   fprintf(output, "$MaxWtoE %d,%d\n", fonthead.WtoE.x, fonthead.WtoE.y);
   fprintf(output, "$MaxWbase %d,%d\n", fonthead.Wbase.x, fonthead.Wbase.y);
   fprintf(output, "$MaxNewline %d,%d\n", fonthead.newline.x, fonthead.newline.y);
   fprintf(output, "$FontRepresentationType %d ", fonthead.type);
   if (fonthead.type == AssortedIcon)
      fprintf(output, "AssortedIcon");
   else
      if (fonthead.type == BitmapIcon)
	 fprintf(output, "BitmapIcon");
   else
      if (fonthead.type == VectorIcon)
	 fprintf(output, "VectorIcon");
   fprintf(output, "\n");

   fprintf(output, "$NIcons %d\n", fonthead.NIcons);


   /* now each of the characters */
   for (i = 0; i <= 127; i++)
      DumpCharacter(i);

   fprintf(output, "$end\n");
}


/* ************************************************************ */

/*								*/

/* ************************************************************ */

struct font *GetFEFont (name)
char *name;
{
   struct FontName   fn;
   struct font *f;

   {
      /* check if a directory path has been specified for this file
         name; if so separate it out */

      register short i;
      register short j;

      if (debug)
	 fprintf(stderr, "Try font %s\n", name);

      j = -1;
      for (i = 0; name[i] != '\0'; i++)
	 if (name[i] == '/')
	    j = i;
      if (j == -1)
	 {
	    /* no directory path in name; try current directory */
	    strcpy(DirName, ".");
	    strcpy(FileName, name);
	 }
      else
	 {
	    /* directory path given; strip it off */
	    strcpy(DirName, name);
	    DirName[j] = '\0';
	    strcpy(FileName, &name[j + 1]);
	 }
   }

   if (debug)
      fprintf(stderr, "Try font %s/%s\n", DirName, FileName);

   /* change the given name (Roman10b) to a struct FontName */
   if (parsefname(FileName, &fn))
      {
	 fprintf(stderr, "Badly formed font name %s\n", FileName);
	 return(NULL);
      }

   /* Now that the name seems reasonable, try to find the font file */
   /* First check if the font is in the specified directory */
   if (verbose)
      fprintf(stderr, "\"%s/%s\"\n", DirName, FileName);
   f = (struct font *) GetFontFromFile (DirName, &fn);
   if (f != NULL)
      return(f);

   {
      /* now check the fontpath entries */

      register char *p;
      register char *st;
      Boolean more;

      p = (char *) getprofile("fontpath");
      if (p == NULL)
	 p = (char *) AndrewDir("/fonts");

      do
	 {
	    st = p;
	    while (*p && *p != ':')
	       p++;
	    more = (*p != '\0');
	    *p++ = '\0';
	    if (verbose)
	       fprintf(stderr, "\"%s/%s\"\n", st, FileName);
	    f = (struct font *) GetFontFromFile (st, &fn);
	    if (f != NULL)
	       return(f);
      } while (more);
   }

   fprintf(stderr, "Can't find font %s\n", FileName);
   return(NULL);
}

/* ***************************************************************** */
/* ***************************************************************** */

/* ************************************************************ */

/*								*/

/* ************************************************************ */


ScanArgs(s)
char *s;
{
   /* check each character of the option list for its meaning. */
   switch (*++s)
      {

	 case 'd': /* debug option */
	       debug = true;
	       break;

	 case 'c': /* check fields option */
	       check = true;
	       break;

	 case 'q': /* quiet option */
	       verbose = false;
	       break;

	 case 'v': /* verbose option */
	       verbose = true;
	       break;


	 case 'O': 
	 case 'o': /* -ofile: output file name should be file */
	       ExplicitOutputFileName = ++s;
	       if (debug)
		  fprintf(stderr, "ExplicitOutputFileName %s\n",
			ExplicitOutputFileName);
	       break;


	 default: 
	       fprintf(stderr, "wmdb: Bad option %c\n", *s);
	       fprintf(stderr, "usage: wmdb [-d] file...\n");
	       exit(1);
      }
}

DetermineFiles(s)
char *s;
{
   short    n;

   n = strlen(s);
   InputFileName = (char *) malloc(n + 1);
   strcpy(InputFileName, s);
   InputFont = GetFEFont(InputFileName);
   if (InputFont == 0)
      exit(1);
   ExplodeFont(InputFont);

   if (ExplicitOutputFileName == NULL)
      {
	 n = strlen(s);
	 OutputFileName = (char *) malloc(n + 5);
	 strcpy(OutputFileName, s);
	 if (      n > 4
		&& OutputFileName[n-4] == '.'
		&& OutputFileName[n-3] == 'f'
		&& OutputFileName[n-2] == 'w'
		&& OutputFileName[n-1] == 'm'
		&& OutputFileName[n] == '\0' ) n = n-4;
	 OutputFileName[n + 0] = '.';
	 OutputFileName[n + 1] = 'f';
	 OutputFileName[n + 2] = 'd';
	 OutputFileName[n + 3] = 'b';
	 OutputFileName[n + 4] = '\0';
      }
   else
      {
	 OutputFileName = ExplicitOutputFileName;
	 ExplicitOutputFileName = NULL;
      }

   output = fopen(OutputFileName, "w");
   if (output == NULL)
      {
	 fprintf(stderr, "Can't open %s\n", OutputFileName);
	 exit(1);
      }
}



/* ************************************************************ */

/*								*/

/* ************************************************************ */
 
char ProgramName[] = "wmfdb";

main(argc, argv)
int   argc;
char **argv;
{
   Boolean namenotgiven = true;

   /* main driver program.  Define the input font from the name on the
      command line.  Process all arguments. */

   while (argc > 1)
      {
	 argc--, argv++;
	 if (debug)
	    fprintf(stderr, "argc: %d, argv: %s\n", argc, *argv);
	 if (**argv == '-')
	    ScanArgs(*argv);
	 else
	    {
	       namenotgiven = false;
	       DetermineFiles(*argv);
	       DumpFont();
	       fclose(output);
	       if (verbose)
		  fprintf(stderr, "%s -> %s\n", InputFileName, OutputFileName);
	    }
      }

   if (namenotgiven)
      {
	 fprintf(stderr, "usage: wmdb [-d] file...\n");
	 exit(1);
      }

   exit(0);
}

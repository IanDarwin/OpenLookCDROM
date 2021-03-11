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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/fonts/cmd/RCS/fdbwm.c,v 2.12 1992/12/15 21:02:39 rr2b R6tape $";
#endif


 

/* ************************************************************ */

/*	fdbwm		convert ASCII to wm font representation */

/*	
	A separate program (wmfdb) converts from the wm
	font representation (defined in font.h) to an
	ASCII human-readable, edittable representation.
	This program converts back.

*/

/*
 *	Modified by pgc accept the -F switch which causes
 *	the output file name to be based on the input file
 *	name.
 */

/* ************************************************************ */
/*								*/
/* ************************************************************ */

/* I/O files and structures */

#include <stdio.h>
#include <ctype.h>/* for isspace, isdigit */
#include <andyenv.h>

FILE * input;
char *InputFileName;
char  *OutputFileName;

char *OutputDirName = "";

#ifdef ibm032
/* get around compiler alignment bug--this should go away soon */
char *foobar = "ab";
#endif /* ibm032 */

/* ************************************************************ */
/* ************************************************************ */

/* types from pascal that we find useful */

#define Boolean char
#define byte char

#define false 0
#define true 1


/* ************************************************************ */
/* ************************************************************ */

Boolean debug = false;
Boolean verbose = false;
Boolean quiet = false;
Boolean OptimizeOutput = false;
Boolean CheckAllBits = true;
Boolean UseInputFileName = false;

char  FileName[255];


char *Printrep(c)
char c;
{
    static char pr[4];

    if (c < 32)
    {
	pr[0] = '^';
	pr[1] = c+64;
	pr[2] = '\0';
    }
    else if (c == 32) return("sp");
    else if (c < 127)
    {
	pr[0] = c;
	pr[1] = '\0';
    }
    else if (c = 127) return("del");
    else
    {
	pr[2] = '0'+ (c&7); c = c >> 3;
	pr[1] = '0'+ (c&7); c = c >> 3;
	pr[0] = '0'+ (c&3);
	pr[3] = '\0';
    }
    return(pr);
}

/* ************************************************************ */
/*								*/
/*								*/
/* ************************************************************ */


ScanArgs(s)
char *s;
{

    /* check each character of the option list for its meaning. */

    switch (*++s)
    {

	case 'v': /* verbose option */
	    verbose = true;
	    break;

	case 'q': /* quiet option */
	    quiet = true;
	    break;

	case 'g': /* take bits as given -- no optimization */
	    OptimizeOutput = false;
	    break;

	case 'O': /* optimize */
	    OptimizeOutput = true;
	    break;

	case 'c': /* check all bits, not just the box */
	    CheckAllBits = true;
	    break;

	case 'b': /* check only the box bits */
	    CheckAllBits = false;
	    break;

	case 'd': /* debug option */
	    debug = true;
	    fprintf(stderr, "debugging on\n");
	    break;

	case 'D': /* directory for output files */
	    OutputDirName = ++s;
	    break;

	case 'F': /* directory for output files */
	    UseInputFileName = true;
	    break;

	default: 
	    fprintf(stderr, "fdbwm: Bad option %s\n", Printrep(*s));
	    fprintf(stderr, "usage: fdbwm [-dbcOgqvF] [-Ddir] file\n");
	    exit(1);
    }
}

DetermineFiles(s)
char *s;
{
    short    n;

    if (debug) fprintf(stderr,"file parameter is %s\n",s);
    n = strlen(s);
    InputFileName = (char *) malloc(n + 5);
    OutputFileName = (char *) malloc(n + 1);
    if ((InputFileName == NULL) || (OutputFileName == NULL)) {
	fprintf(stderr, "Can't allocate enough memory\n");
	exit(1);
    }	

    strcpy(InputFileName, s);
    strcpy(OutputFileName, s);
    if (InputFileName[n - 4] == '.'
	 && InputFileName[n - 3] == 'f'
	 && InputFileName[n - 2] == 'd'
	 && InputFileName[n - 1] == 'b')
    {
	/* extension already .fdb */
	OutputFileName[n - 4] =	'\0';	/* throw away the extension */
    }
    else
    {
	/* add .fdb extension */
	InputFileName[n] = '.';
	InputFileName[n + 1] = 'f';
	InputFileName[n + 2] = 'd';
	InputFileName[n + 3] = 'b';
	InputFileName[n + 4] = '\0';
    }

    input = fopen(InputFileName, "r");
    if (input != NULL)
	return;

    fprintf(stderr, "Can't open %s\n", InputFileName);
    exit(1);

}


/* ************************************************************ */
/*								*/
/*								*/
/* ************************************************************ */

short NIcons = -1;

/* the internal structure to be defined for a font */

#ifdef	WM_ENV	/* to avoid makedepend "errors" */
#include <font.h>
#include <fntmanip.h>
#endif /* WM_ENV	 */


/* ************************************************************ */
/*								*/
/*								*/
/* ************************************************************ */


/* procedure Initialize  */
Initialize()
{
    register short i;
    register short c;

    /* ------------------------------------------ */
    /* We must define all of the following fields */
    /* ------------------------------------------ */

    fonthead.magic = FONTMAGIC;
    for (i = 0; i < sizeof(fonthead.fn.FamilyName); i++)
	fonthead.fn.FamilyName[i] = '\0';
    fonthead.fn.rotation = 0;
    fonthead.fn.height = 0;
    fonthead.fn.FaceCode = 0;
    fonthead.NWtoOrigin.x = 0;
    fonthead.NWtoOrigin.y = 0;
    fonthead.Wbase.x = 0;
    fonthead.Wbase.y = 0;
    fonthead.NtoS.x = 0;
    fonthead.NtoS.y = 0;
    fonthead.WtoE.x = 0;
    fonthead.WtoE.y = 0;
    fonthead.newline.x = 0;
    fonthead.newline.y = 0;
    fonthead.type = BitmapIcon;

    /* ------------------------------------------ */


    for (c = 0; c < MAXICONS; c++)
    {
	/* ------------------------------------------ */
	/* We must define all of the following fields */
	/* ------------------------------------------ */

	generic[c].Spacing.x = 0;
	generic[c].Spacing.y = 0;
	generic[c].NWtoOrigin.x = 0;
	generic[c].NWtoOrigin.y = 0;
	generic[c].NtoS.x = 0;
	generic[c].NtoS.y = 0;
	generic[c].WtoE.x = 0;
	generic[c].WtoE.y = 0;
	generic[c].Wbase.x = 0;
	generic[c].Wbase.y = 0;
	specific[c].type = BitmapIcon;
	specific[c].rows = 0;
	specific[c].cols = 0;
	specific[c].orow = 0;
	specific[c].ocol = 0;
	bits[c] = (unsigned short *) NULL;

    }
}



/* ************************************************************ */
/*								*/
/*								*/
/* ************************************************************ */




/* ************************************************************ */

/*
  The following tables convert a symbolic keyword to a
  internal numeric one, which is then used in a switch
	  */

#define db_unknown 0
#define db_FontRepresentationType 1
#define db_MaxNWtoOrigin 2
#define db_MaxNewline 3
#define db_MaxNtoS 4
#define db_MaxWbase 5
#define db_MaxWtoE 6
#define db_NIcons 7
#define db_NWtoOrigin 8
#define db_NtoS 9
#define db_spacing 10
#define db_Wbase 11
#define db_WtoE 12
#define db_box 13
#define db_character 14
#define db_familyname 15
#define db_fontname 16
#define db_magic 17
#define db_origin 18
#define db_pointsize 19
#define db_raster 20
#define db_rotation 21
#define db_specifictype 22
#define db_facecodes 23
#define db_end 24
#define db_comment 25

/* ************************************************************ */

#define GetChar() getc(input)

static
char *GetString ()
{
    static char StringBuffer[255];
    register char  *sb;
    register int    c;

    /* skip leading spaces */
    while (isspace(c = GetChar()) && c != '\n');

    sb = StringBuffer;
    while (!isspace(c) && c != EOF) {
	*sb++ = c;
	c = GetChar();
    }
    *sb = '\0';

    if (c == EOF && StringBuffer[0] == '\0')
	return("$end");

    if (debug)
	fprintf(stderr, "string = <%s>\n", StringBuffer);
    return(StringBuffer);
}

static
char *GetLine ()
{
    static char StringBuffer[255];
    register char  *sb;
    register int    c;

    /* skip leading spaces */
    while (isspace(c = GetChar()) && c != '\n');

    sb = StringBuffer;
    while (c!='\n' && c != EOF) {
	*sb++ = c;
	c = GetChar();
    }
    *sb = '\0';

    return(StringBuffer);
}


#define val(c) (('0'<=c&&c<='9')?c-'0':('A'<=c&&c<='F')?c-'A'+10:('a'<=c&&c<='f')?c-'a'+10:0)

#define value(c) val(c)

unsigned short GetHexShort ()
{
    int   c;
    short    n;
    short    i;

    /* skip leading spaces */
    while (isspace(c = GetChar()));

    n = value(c);
    i = 1;
    while (i < 4 && !isspace(c = GetChar()))
    {

	n = (n << 4) + value(c);

	i++;
    }
    if (isspace(c))
	ungetc(c, input);
    while (i < 4)
    {
	n = (n << 4);
	i++;
    }

    if (debug)
	fprintf(stderr, "hex number = %6o\n", n);
    return(n);
}


short    GetShort ()
{
    int   c;
    Boolean Minus;
    short    n;

    /* skip leading spaces */
    while (isspace(c = GetChar()) && c != '\n');

    /* check for sign */
    if (c == '-')
    {
	Minus = true;
	c = GetChar();
    }
    else
	if (c == '+')
	{
	    Minus = false;
	    c = GetChar();
	}
	else
	    Minus = false;

    n = 0;
    while (isdigit(c))
    {
	n = 10 * n + c - '0';
	c = GetChar();
    }
    ungetc(c, input);

    if (Minus)
	n = -n;
    if (debug)
	fprintf(stderr, "number = %d\n", n);
    return(n);
}

skipsymbolic()
{
    register int   c;

    /* skip leading spaces */
    while (isspace(c = GetChar()) && c != '\n');

    /* skip non-spaces */
    if (c != '\n')
	while (!isspace(c = GetChar()) && c != EOF);
}

GetSVector(x, y)
short   *x;
short   *y;
{
    char  c;

    *x = GetShort();
    c = GetChar();
    *y = GetShort();
}

/* ************************************************************ */

struct {
    char *word;
    short    number;
} KeyWordTable[] = {
    "$character", db_character,
    "$specifictype", db_specifictype,
    "$box", db_box,
    "$origin", db_origin,
    "$spacing", db_spacing,
    "$raster", db_raster,

    "$nwtoorigin", db_NWtoOrigin,
    "$ntos", db_NtoS,
    "$wbase", db_Wbase,
    "$wtoe", db_WtoE,

    "$magic", db_magic,
    "$fontname", db_fontname,
    "$familyname", db_familyname,
    "$rotation", db_rotation,
    "$pointsize", db_pointsize,
    "$facecodes", db_facecodes,
    "$fontrepresentationtype", db_FontRepresentationType,
    "$maxnwtoorigin", db_MaxNWtoOrigin,
    "$maxnewline", db_MaxNewline,
    "$maxntos", db_MaxNtoS,
    "$maxwbase", db_MaxWbase,
    "$maxwtoe", db_MaxWtoE,
    "$nicons", db_NIcons,
    "$end", db_end,
    "$comment", db_comment,
    "$COMMENT", db_comment,

    "", 0
};

char KeywordString[255];

short  GetKeyWord ()
{
    register short i;
    register char *p;

    do
	strcpy(KeywordString, GetString());
    while (KeywordString[0] != '$');

    for (p = KeywordString; *p != '\0'; p++)
	if (isupper(*p))
	    *p = *p + 32;

    for (i = 0; KeyWordTable[i].number > 0; i++)
	if (strcmp(KeyWordTable[i].word, KeywordString) == 0)
	{
	    return(KeyWordTable[i].number);
	}

    return(db_unknown);
}


/* ************************************************************ */
/*								*/
/*								*/
/* ************************************************************ */

char  CurrentChar;
struct IconGenericPart *gp;
struct BitmapIconSpecificPart *sp;


#define rg_undefined  0
#define rg_header 1
#define rg_charheader 2
#define rg_charinfo 3
#define rg_redundantinfo 4

short    previousregion = rg_undefined;

EnterRegion(r)
short    r;
{
    if (r == previousregion)
	return;
    if (debug) fprintf(stderr,"leave region %d -> region %d\n",previousregion,r);
    switch (previousregion)
    {
	case rg_header: 
	    {
	    if (NIcons < 0)
		NIcons = 128;
	    break;
	    }

	case rg_charinfo: 
	    {
	    /* define generic from specific */
	    gp->NWtoOrigin.x = BUG(sp->ocol);
	    gp->NWtoOrigin.y = BUG(sp->orow);
	    gp->Wbase.x = -BUG(sp->ocol);
	    gp->NtoS.y = sp->rows;
	    gp->WtoE.x = sp->cols;
	    break;
	    }

	default: 
	    break;
    }
    previousregion = r;
}


/* ************************************************************ */
/*								*/
/*								*/
/* ************************************************************ */


unsigned short *copyraster (cols, rows)
unsigned short cols, rows;
{
    long  bytesneeded;
    unsigned short *BitArray;

    short    i, j;
    unsigned short *rowbase;
    unsigned short *p;

    /* convert cols to number of 16-bit shorts needed */
    cols = (cols + 15) / 16;

    /* allocate space */
    bytesneeded = 2 * rows * cols;
    if (bytesneeded == 0) return(NULL);
    BitArray = (unsigned short *) malloc(bytesneeded);

    /* read the raster */
    rowbase = BitArray;
    for (i = 0; i < rows; i++)
    {
	p = rowbase;
	for (j = 0; j < cols; j++)
	{
	    *p = GetHexShort();
	    p += rows;
	}
	rowbase += 1;
    }

    /* return the input raster image */
    return(BitArray);
}


/* ************************************************************ */
/*								*/
/*								*/
/* ************************************************************ */


/* procedure InputDBFont  */
InputDBFont()
{
    short key;
    short x, y;
    short cols, rows;
    Boolean GenericValueDefined;

    ncomments = 0;

    GenericValueDefined = false;
    while (true) {
	key = GetKeyWord();
	switch (key) {
	    case db_magic: 
		EnterRegion(rg_header);
		fonthead.magic = GetShort();
		break;

	    case db_fontname: 
		EnterRegion(rg_header);
		strcpy(FileName, GetString());
		break;

	    case db_familyname: 
		EnterRegion(rg_header);
		strncpy(fonthead.fn.FamilyName,
			GetString(),
			sizeof(fonthead.fn.FamilyName));
		break;

	    case db_facecodes: 
		EnterRegion(rg_header);
		fonthead.fn.FaceCode = GetShort();
		skipsymbolic();
		break;

	    case db_rotation: 
		EnterRegion(rg_header);
		fonthead.fn.rotation = GetShort();
		break;

	    case db_pointsize: 
		EnterRegion(rg_header);
		fonthead.fn.height = GetShort();
		break;

	    case db_FontRepresentationType: 
		EnterRegion(rg_header);
		fonthead.type = GetShort();
		skipsymbolic();
		break;

	    case db_NIcons: 
		EnterRegion(rg_header);
		NIcons = GetShort();
		if (NIcons > MAXICONS) {
		    fprintf(stderr, "Too many Icons (%d > %d)\n", NIcons, MAXICONS);
		    NIcons = MAXICONS;
		}
		fonthead.NIcons = NIcons;
		break;


	    case db_MaxNWtoOrigin: 
		EnterRegion(rg_header);
		GetSVector(&x, &y);
		fonthead.NWtoOrigin.x = x;
		fonthead.NWtoOrigin.y = y;
		GenericValueDefined = true;
		break;

	    case db_MaxNewline: 
		EnterRegion(rg_header);
		GetSVector(&x, &y);
		fonthead.newline.x = x;
		fonthead.newline.y = y;
		GenericValueDefined = true;
		break;

	    case db_MaxNtoS: 
		EnterRegion(rg_header);
		GetSVector(&x, &y);
		fonthead.NtoS.x = x;
		fonthead.NtoS.y = y;
		GenericValueDefined = true;
		break;

	    case db_MaxWbase: 
		EnterRegion(rg_header);
		GetSVector(&x, &y);
		fonthead.Wbase.x = x;
		fonthead.Wbase.y = y;
		GenericValueDefined = true;
		break;

	    case db_MaxWtoE: 
		EnterRegion(rg_header);
		GetSVector(&x, &y);
		fonthead.WtoE.x = x;
		fonthead.WtoE.y = y;
		GenericValueDefined = true;
		break;

	    case db_comment:
		if (ncomments < MAXCOMMENTS) {
		    char *s = GetLine();
		    comments[ncomments] = (char *) strcpy(malloc(strlen(s)+1), s);
		    ncomments++;
		}
		break;

	    case db_character: 
		EnterRegion(rg_charheader);
		CurrentChar = GetShort();
		if (CurrentChar >= NIcons)
		    CurrentChar = 0;
		gp = &generic[CurrentChar];
		sp = &specific[CurrentChar];
		skipsymbolic();
		break;

	    case db_specifictype: 
		EnterRegion(rg_charinfo);
		sp->type = GetShort();
		skipsymbolic();
		break;

	    case db_box: 
		EnterRegion(rg_charinfo);
		GetSVector(&cols, &rows);
		sp->cols = cols;
		sp->rows = rows;
		break;

	    case db_origin: 
		EnterRegion(rg_charinfo);
		GetSVector(&x, &y);
		sp->ocol = x;
		sp->orow = y;
		break;

	    case db_spacing: 
		EnterRegion(rg_charinfo);
		GetSVector(&x, &y);
		gp->Spacing.x = x;
		gp->Spacing.y = y;
		break;

	    case db_raster: 
		EnterRegion(rg_charinfo);
		bits[CurrentChar] = copyraster(cols, rows);
		break;

	    case db_NWtoOrigin: 
		EnterRegion(rg_redundantinfo);
		GetSVector(&x, &y);
		gp->NWtoOrigin.x = x;
		gp->NWtoOrigin.y = y;
		break;

	    case db_NtoS: 
		EnterRegion(rg_redundantinfo);
		GetSVector(&x, &y);
		gp->NtoS.x = x;
		gp->NtoS.y = y;
		break;

	    case db_Wbase: 
		EnterRegion(rg_redundantinfo);
		GetSVector(&x, &y);
		gp->Wbase.x = x;
		gp->Wbase.y = y;
		break;

	    case db_WtoE: 
		EnterRegion(rg_redundantinfo);
		GetSVector(&x, &y);
		gp->WtoE.x = x;
		gp->WtoE.y = y;
		break;

	    case db_end: 
		EnterRegion(rg_undefined);
		if (!GenericValueDefined) {
		    if (verbose)
			fprintf(stderr,
				"Compute Generic\n");
		    ComputeGenericFromSpecific();
		}
		return;

	    default: 
		if (verbose)
		    fprintf(stderr, "Unknown keyword %s\n",
			    KeywordString);
	}
    }
}

/* ************************************************************ */
/*								*/
/*								*/
/* ************************************************************ */

DumpCharacter(c)
short    c /* character to dump */ ;
{
    struct IconGenericPart *gp;
    struct BitmapIconSpecificPart *sp;
    unsigned short *bitptr /* pointer to bit array */ ;

    short    w /* width in shorts (16-bit chunks */ ;
    short    j /* x direction; from 1 to width in shorts */ ;
    short    i /* y direction; from 1 to height */ ;
    unsigned short *p /* pointer to packed bits */ ;

    fprintf(stderr, "$character %d %s\n", c, Printrep(c));

    /* first the generic parts */
    gp = &(generic[c]);
    fprintf(stderr, "$spacing %d,%d\n", gp->Spacing.x, gp->Spacing.y);
    fprintf(stderr, "$NWtoOrigin %d,%d\n", gp->NWtoOrigin.x, gp->NWtoOrigin.y);
    fprintf(stderr, "$NtoS %d,%d\n", gp->NtoS.x, gp->NtoS.y);
    fprintf(stderr, "$WtoE %d,%d\n", gp->WtoE.x, gp->WtoE.y);
    fprintf(stderr, "$Wbase %d,%d\n", gp->Wbase.x, gp->Wbase.y);

    /* then the specific parts */
    sp = &(specific[c]);
    fprintf(stderr, "$specifictype %d BitmapIcon\n", sp->type);
    fprintf(stderr, "$box %d,%d\n", sp->cols, sp->rows);
    fprintf(stderr, "$origin %d,%d\n", BUG(sp->ocol), BUG(sp->orow));

    fprintf(stderr, "$raster\n");
    bitptr = bits[c];
    if (bitptr != NULL)
    {

	/* compute number of shorts */
	w = ((sp->cols + 15) / 16);
	for (i = 1; i <= sp->rows; i++)
	{
	    p = &bitptr[i - 1];
	    for (j = 1; j <= w; j++)
	    {
		fprintf(stderr, "%04X", *p);
		p += sp->rows;
	    }
	    fprintf(stderr, "\n");
	}
    }
}


/* ************************************************************ */


dumpfont()
{
    short    i;

    /* print overall font information */
    fprintf(stderr, "$magic %d\n", fonthead.magic);
    fprintf(stderr, "$fontname %s\n", FileName);
    fprintf(stderr, "$familyname %s\n", fonthead.fn.FamilyName);
    fprintf(stderr, "$rotation %d\n", fonthead.fn.rotation);
    fprintf(stderr, "$pointsize %d\n", fonthead.fn.height);
    if (fonthead.fn.FaceCode != 0)
    {
	fprintf(stderr, "$facecodes %d ", fonthead.fn.FaceCode);
	if ((BoldFace & fonthead.fn.FaceCode) != 0)
	    fprintf(stderr, "B");
	if ((ItalicFace & fonthead.fn.FaceCode) != 0)
	    fprintf(stderr, "I");
	if ((ShadowFace & fonthead.fn.FaceCode) != 0)
	    fprintf(stderr, "S");
	if ((FixedWidthFace & fonthead.fn.FaceCode) != 0)
	    fprintf(stderr, "F");
	fprintf(stderr, "\n");
    }
    fprintf(stderr, "$MaxNWtoOrigin %d,%d\n", fonthead.NWtoOrigin.x, fonthead.NWtoOrigin.y);
    fprintf(stderr, "$MaxNtoS %d,%d\n", fonthead.NtoS.x, fonthead.NtoS.y);
    fprintf(stderr, "$MaxWtoE %d,%d\n", fonthead.WtoE.x, fonthead.WtoE.y);
    fprintf(stderr, "$MaxWbase %d,%d\n", fonthead.Wbase.x, fonthead.Wbase.y);
    fprintf(stderr, "$MaxNewline %d,%d\n", fonthead.newline.x, fonthead.newline.y);
    fprintf(stderr, "$FontRepresentationType %d ", fonthead.type);
    if (fonthead.type == AssortedIcon)
	fprintf(stderr, "AssortedIcon");
    else
	if (fonthead.type == BitmapIcon)
	    fprintf(stderr, "BitmapIcon");
	else
	    if (fonthead.type == VectorIcon)
		fprintf(stderr, "VectorIcon");
    fprintf(stderr, "\n");

    fprintf(stderr, "$NIcons %d\n", fonthead.NIcons);


    /* now each of the characters */
    for (i = 0; i < NIcons; i++)
	DumpCharacter(i);
}

/* ************************************************************ */
/*								*/
/*								*/
/* ************************************************************ */


/* ************************************************************ */

/*
  The following routine (ConvertInternalToExternal)
  was lifted from fntmanip.c by J.Gosling of the ITC.
  It was copied rather than simply being included or used
  from the library to allow the OutputDirName to
  be used.

  */

/* ************************************************************ */

/* Pack font structures */

Boolean ConvertInternalToExternal()
{
    register FILE * OutputFontFile;

    /* Zero the unused portion of the font family name */
    {
	register char *p = fonthead.fn.FamilyName;
	short n = sizeof (fonthead.fn.FamilyName);
	while (*p != '\0' && --n > 0)
	    p++;
	while (--n >= 0)
	    *p++ = '\0';
    }

    /* Now determine the output file name and open it */
    {
	char *NewFontName;
	char CompleteFileName[1024];

	if (UseInputFileName) {
	    NewFontName = OutputFileName;
	} else {
	    NewFontName = (char *) FormatFontname(&fonthead.fn);
	}

	if (*OutputDirName != '\0')
	    sprintf(CompleteFileName, "%s/%s", OutputDirName, NewFontName);
	else
	    sprintf(CompleteFileName, "%s", NewFontName);
	strcat(CompleteFileName, ".fwm");
	OutputFontFile = fopen(CompleteFileName, "w");
	if (OutputFontFile == NULL) {
	    fprintf(stderr, "Couldn't write %s\n", CompleteFileName);
	    return(false);
	}
	if (!quiet)
	    fprintf(stderr, "%s -> %s\n",
		    InputFileName, CompleteFileName);
    }

    /* Now convert from the exploded internal format to wm form */

    if (OptimizeOutput) {
	CheckFixedWidths();
#if (! defined(vax) && ! defined(MIPSEL))
	/* These dont work on the vax */
	TightenAllBoundingBoxes();
	CheckOriginAndSpacing();
#endif /* ! vax  && ! MIPSEL */
    }
    ComputeGenericFromSpecific();

    WriteExplodedFont(OutputFontFile, 0);

    fclose(OutputFontFile);
    return(true);
}



/* ************************************************************ */
/*								*/
/*								*/
/* ************************************************************ */


/* Main program */

char ProgramName[] = "fdbwm";

main(argc, argv)
int   argc;
char **argv;
{

    /* main driver program.  Define the input file from either standard
      input or a name on the command line.  Process all arguments. */

    while (argc > 1)
    {
	argc--, argv++;
	if (**argv == '-')
	    ScanArgs(*argv);
	else
	{
	    DetermineFiles(*argv);
	    Initialize();
	    InputDBFont();
	    fclose(input);
	    if (debug)
		dumpfont();

	    /* now convert to the wm format */
	    ConvertInternalToExternal();
	}
    }

    exit(0);
}



/* ************************************************************ */
/*								*/
/*   Optimizations and Consistency checks			*/
/*								*/
/* ************************************************************ */


CheckOriginAndSpacing()
{
    int   a,b,c;
    for (c = 0; c < NIcons; c++)
    {
	a = BUG(specific[c].ocol);
	b = a + generic[c].Spacing.x;
	if (b < a) { int t; t=a; a=b; b=t;}
	if (b < 0 || a > (int)specific[c].cols)
	    if (!quiet)
		fprintf(stderr,"Box does not intersect spacing for %s\n",
			Printrep(c));
	if (debug)
	    fprintf(stderr,"Box from 0 to %d, spacing %d to %d = %d+%d\n",
		    specific[c].cols,a,b,BUG(specific[c].ocol),generic[c].Spacing.x);
    }
}

/* ************************************************************ */

/* check that a fixed width font is fixed width */

CheckFixedWidths()
{
    register int c;
    register int x,y;

    if ((fonthead.fn.FaceCode & FixedWidthFace) == 0) return;

    /* first compute the maximum spacing vector */
    x = 0;
    y = 0;
    for (c = 0; c < NIcons; c++)
    {
	register struct SVector *sv = &generic[c].Spacing;

	if (sv->x > x) x = sv->x;
	if (sv->y > y) y = sv->y;
    }

    y = 0; /* y components should always be zero */
    /* then enforce that each spacing vector is the maximum
      spacing vector (or zero) */
    for (c = 0; c < NIcons; c++)
    {
	register struct SVector *sv = &generic[c].Spacing;

	if ((sv->x != x || sv->y != y)
	    && (sv->x != 0 || sv->y != 0))
	{
	    if (!quiet)
		fprintf(stderr,"Fixed width of %s is %d,%d (%d,%d)\n",
			Printrep(c),sv->x,sv->y,x,y);
	    sv->x = x;
	    sv->y = y;
	}
    }
}


/* ************************************************************ */

/*
  * Eliminate blank pixels at left and right edges of character
  */


TightenAllBoundingBoxes()
{
    int   c;
    for (c = 0; c < NIcons; c++)
	if (bits[c] != NULL)
	    TightenBoundingBox(c);
}


/* macro to see if bit i,j of character c is set */
#define isset(c,i,j) (bits[c][(j>>4)*specific[c].rows+i] & (1<<(15-(j&017))))

TightenBoundingBox(c)
int c;
{
    register struct BitmapIconSpecificPart *sp = &(specific[c]);
    int top, bottom, left, right;

    /* determine the extremes of the bit map */
    {
	register  i, j;
	int checkcols;

	/* we have had problems of extra bits in some of the fdb files; to
	  * check we look at all bits that are present, even those that are
	* "outside" the box, but present due to rounding to 16 bit boundaries
	*/
	if (CheckAllBits)
	    checkcols = (sp->cols + 15) / 16 * 16;
	else
	    checkcols = sp->cols;
	top = sp->rows;
	bottom = -1;
	left = checkcols;
	right = -1;
	for (i = 0; i < sp->rows; i++)
	    for (j = 0; j < checkcols; j++)
		if (isset(c, i, j)) {
		    if (i < top) top = i;
		    if (i > bottom) bottom = i;
		    if (j < left) left = j;
		    if (j > right) right = j;
		}
    }

    if (debug)
	fprintf(stderr, "For %s %d,%d to %d,%d\n",
		Printrep(c), left, top, right, bottom);

    /* if the box is empty, free the bits for it */
    if (bottom == -1 && right == -1) {
	if (bits[c])
	    free(bits[c]);
	bits[c] = NULL;
	sp->rows = 0;
	sp->cols = 0;
	sp->orow = 0;
	sp->ocol = 0;
	return;
    }

    if (right >= sp->cols || bottom >= sp->rows)
	if (!quiet)
	    fprintf(stderr, "Extra bits in box for %s\n", Printrep(c));

    /* if box is already tight, just return */
    if (left == 0 && top == 0 && bottom == sp->rows - 1) {
	sp->cols = right + 1;
	return;
    }

    /* copy and shift the bits from the old to the new */
    /* 
      * The non-zero bits are only in the area from left,top to right,bottom.
      * 
      * Space for the font is always allocated in chunks of 
	  * 16-bit ints for each row.  Thus, a box of 1,2,3,..., or 
	      * 16 columns always has one 16-bit word allocated for it.  
		  * In addition, all rows of the first 16-bits come first, 
		  * then all rows for the next 16-bits, etc.
		      * 
		      * In the following code, we shift everything to the right
		      * to eliminate the zero columns on the right.  This requires
		      * a left shift of 'shiftcount'.  Since the bits are packed
		      * in 16-bit words, we need  'left%16' bits from one word
		      * and '16 - *left%16' bits from the next column over (which
									    * is 'sp->rows' further in the bit array).  'mask' is used
			* to extract the right bits from the next column over.
			* (Unless, of course, the next column over is past the end
			   * of the array, in which case 'mask' is zero.).
			* 
			* The main loop of the code counts the columns and rows of
			* the new bit array, and defines each element in terms of
			* the old element.  The simplest way to understand the code
			* is to assume first that 'left' is zero (so we are simply
								  * moving rows up), and then to assume that 'top' is zero
			* (and we are shifting columns over).  In the latter case,
			    * assume first that 'left' is a multiple of 16, and then that
			    * it is not.
			    */

    {
	int     newrows,
	newcols;
	int     newshorts;
	unsigned short *newbits,
	*oldbits;
	int     shiftcount;
	unsigned short  mask;
	register int    row,
	col;

	newrows = bottom - top + 1;
	newcols = right - left + 1;
	newshorts = (newcols + 15) / 16;

	/* copy font bits */
	newbits = bits[c];
	oldbits = bits[c] + (left / 16 * sp->rows) + top;
	shiftcount = left % 16;
	mask = (1 << shiftcount) - 1;
	for (col = 0; col < newshorts; col++) {
	    /* this time thru we are defining bits col*16..(col+1)*16-1 we
	     only want columns 0..newcols-1; so if (col+1)*16-1 is greater
		   than newcols-1, we need to adjust the mask to be sure that the
		   extra columns are always zero */
	    if (16 * (col + 1) > newcols)
		mask = mask & ~((1 << 16 * (col + 1) - newcols) - 1);
	    if (debug)
		fprintf(stderr, "start col=%d, %d, %o\n",
			col, shiftcount, mask);
	    for (row = 0; row < newrows; row++) {
		*newbits = (*oldbits << shiftcount);
		if (mask != 0)
		    *newbits |= (*(oldbits + sp->rows) >> (16 - shiftcount)) & mask;
		newbits++, oldbits++;
	    }
	    oldbits += sp->rows - newrows;
	}
    }

    /* adjust the specific parameters */
    sp->cols = right - left + 1;
    sp->rows = bottom - top + 1;
    sp->orow = BUG(sp->orow) - top;
    sp->ocol = BUG(sp->ocol) - left;
}

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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/text/RCS/txttroff.c,v 2.73 1994/02/04 17:42:37 wjh Exp $";
#endif

/*
 * Rofftext: Write ATK multimedia text document to file in troff
 *
 * Bugs:
 *
 * The PB/PE macros do not work at a diversion level greater than 0.
 * This means they do not work in tables.
 * Does not handle hyphenation properly - will have to be fixed when
 * scanning the profile routines are put into camlib.
 *
 * Fixed:
 *
 * Uses \s<SIZE>  handles need new line right
 * Changes \ to \\  sub and superscripts
 * Passthru not handled (should set needNewLine )
 */

#include <andrewos.h>
#include <class.h>
#include <ctype.h>

#include <text.ih>
#include <fontdesc.ih>
#include <environ.ih>
#include <print.ih>
#include <dict.ih>
#include <viewref.ih>
#include <view.ih>
#include <tabs.ih>
#include <textv.ih>
#include <txtstvec.h>
#include <envrment.ih>
#include <style.ih>
#include <stylesht.ih>
#include <matte.ih>
#include <fnote.ih>
#include <fnotev.ih>
#include <proctbl.ih>
#include <pcompch.ih>

#define BOGUS3812 1 /* avoids new bug in psdit */

static int tabscharspaces = 8;

struct tran
{
    char out[3];
};

/*        The following two translation tables use the array index
           as the character code. The key xx is a place holder for no char.
      These tables translate between the characters in the andysymbol 
      and andysymbola fonts and the escape sequences used by troff 
      to represent these symbols */


static struct tran symtran[128] =
{
"xx","xx","xx","xx","xx","xx","xx","xx","xx","xx","xx","xx","xx","xx","xx","xx",
"xx","xx","xx","xx","xx","xx","xx","xx","xx","xx","xx","xx","xx","xx","xx","xx",
"xx","xx","xx","xx","xx","xx","xx","xx","xx","xx","**","pl","xx","mi","xx","sl",
"xx","xx","xx","xx","xx","xx","xx","xx","xx","xx","xx","xx","xx","xx","xx","xx",
"~=","*A","*B","*X","*D","*E","*F","*G","*Y","*I","xx","*K","*L","*M","*N","*O",
"*P","*H","*R","*S","*T","*U","xx","*W","*C","*Q","*Z","xx","xx","xx","xx","ul",
"rn","*a","*b","*x","*d","*e","*f","*g","*y","*i","xx","*k","*l","*m","*n","*o",
"*p","*h","*r","*s","*t","*u","xx","*w","*c","*q","*z","xx","or","xx","ap","xx"
};

static struct tran symatran[128] =
{
"xx","xx","xx","xx","xx","xx","xx","xx","xx","xx","xx","xx","xx","xx","xx","xx",
"xx","xx","xx","xx","xx","xx","xx","xx","xx","xx","xx","xx","xx","xx","xx","xx",
"xx","xx","aa","<=","sl","if","xx","xx","xx","xx","xx","xx","<-","ua","->","da",
"de","+-","xx",">=","mu","pt","pd","bu","di","!=","==","xx","xx","br","xx","xx",
"xx","xx","xx","xx","xx","xx","es","ca","cu","sp","ip","xx","sb","ib","mo","xx",
"xx","gr","rg","co","xx","*P","sr","xx","no","xx","xx","xx","<-","ua","->","da",
"xx","xx","rg","co","xx","lt","bv","lb","lc","bv","lf","lt","lk","lb","bv","xx",
"xx","is","xx","xx","xx","rt","bv","rb","rc","bv","rf","rt","rk","rb","xx","xx"
};



#define ENDNOTESONLY FALSE /* Administrators should define as TRUE if local troff can't support footnotes */
#define	CONTENTSBYDEFAULT FALSE	/* define the default behavior 
   regarding the printing of tables
   of  contents if the appropriate styles are present */
#define	DUPLEXBYDEFAULT	FALSE	/* default behavior: simplex printing or duplex printing */

#define ENUMERATE 
#define INDENTSPACE 6
#ifdef ENUMERATE
#include <content.ih>
static boolean enumerate;
#endif

#include <txttroff.eh>

static FILE *troffFile;
static int addNewLine;      /* True if \n should be added to keep lines from getting */
                            /* Too long, and a space has been found to insert it */
static boolean needNewLine;     /* True if must put out a new line before troff cmd */
static boolean passthru;
/* static boolean HideText; 	*/ /* True if text should be hidden (not completed) */
static boolean underlining;     /* True if text in underlining mode */
static boolean changebar;       /* True if text in changebar mode */
static boolean strikethrough;   /* True if text in strikethrough mode */
static boolean overbar;         /* True if text in overbar mode */
static long dFont;		/* Desired index of name of the font */
static long dSize;		/* Desired size of the font */
static long dFace;		/* Desired faces of the font */
static long cSize;		/* Current size of the font */
static long PageOffset;         /* Faked pg. offset (left marg.) for troff calcs. */
static long LineLength;         /* Faked line length for troff calculations */
static boolean InlineMacros;	/* TRUE if macro files should be included inline */
static int symbola;		/* >0  if the current font is a special symbol font */
static int leadingwhitespace;   /* counts space-char equivalent of leading whitespace that was ignored for ContinueIndent*/ /*RSKadd*/
static int NegOffset;		/* Negative offset for use in faking troff */

static boolean resetTabs = FALSE;
static long currentVS;
static long latestVS;
static long extraVS;
static long currentSpread;
static long latestSpread;
struct content_chapentry *lastcentry;

static struct style *defaultStyle = NULL;
static int textLevel = -1;	/* For generating proper .ev argument */

static struct text_statevector sv, nsv;     /* Current and new state vectors */
static boolean printContents; /* Flag to indicate if we are printing a table of contents */
static boolean printDuplex; /* Flag to indicate if we process for duplex printing. */

/* this is relative to ANDREWDIR */
#define	FONT_DESC_FILE	"/lib/tmac/PrintFontMap"

typedef struct font_desc
{
    char    *fontname;
    char    *fontcodes[9];
    /* Fontcodes are in this order:  */
    /*  plain, italic, bold, bolditalic,  fixed-plain, fixed-italic, */
    /* fixed-bold, fixed-bolditalic, shadow. */
    /* All shadowface is bold for now */ 
} FontDesc;

FontDesc *fonttable = NULL;

FontDesc default_fonttable[] =
{
#if !defined(TROFF_FONTS_ENV) || defined(EROFF_ENV)
    {"timesroman", {"R",  "I",  "B",  "BI", "C",  "CO", "CB", "CD", "B"}},
    {"helvetica",  {"H",  "HO", "HB", "HD", "C",  "CO", "CB", "CD", "B"}},
    {"andy",       {"R",  "I",  "B",  "BI", "C",  "CO", "CB", "CD", "B"}},
    {"andysans",   {"H",  "HO", "HB", "HD", "C",  "CO", "CB", "CD", "B"}},
    {"andytype",   {"C",  "CO", "CB", "CD", "C",  "CO", "CB", "CD", "C"}},
    {"gacha",      {"C",  "CO", "CB", "CD", "C",  "CO", "CB", "CD", "C"}},
    {0,            {"R",  "I",  "B",  "BI", "C",  "CO", "CB", "CD", "B"}}
#else
    {"timesroman", {"R",  "I",  "B",  "BI", "CO", "CI", "CB", "CX", "B"}},
    {"helvetica",  {"H",  "HI", "HB", "HX", "CO", "CI", "CB", "CX", "B"}},
    {"andy",       {"R",  "I",  "B",  "BI", "CO", "CI", "CB", "CX", "B"}},
    {"andysans",   {"H",  "HI", "HB", "HX", "CO", "CI", "CB", "CX", "B"}},
    {"andytype",   {"CO", "CI", "CB", "CX", "CO", "CI", "CB", "CX", "CO"}},
    {"gacha",      {"CO", "CI", "CB", "CX", "CO", "CI", "CB", "CX", "CO"}},
    {0,            {"R",  "I",  "B",  "BI", "CO", "CI", "CB", "CX", "B"}}
#endif /* !defined(TROFF_FONTS_ENV) || defined(EROFF_ENV) */
};


static struct {
    char *fontname;
    int tablenumber;
} specfonttable[] = {
    {"symbol", 1},
    {"andysymbol", 1},
    {"symbola", 2},
    {"andysymbola", 2},
    {0,0}
};

static int endnotes;
struct content *con;
static boolean addindex;
#define ModBits 4
#define FaceCodeBits 4
#ifndef text_TMACBASE
#define text_TMACBASE "/lib/tmac/tmac.atk"
#endif /* text_TMACBASE */

#ifndef text_TMACFILE
#define text_TMACFILE environ_AndrewDir(text_TMACBASE)
#endif/* text_TMACFILE */

#ifndef text_TMACACCENTSFILE
#define text_TMACACCENTSFILE environ_AndrewDir("/lib/tmac/tmac.acc")
#endif /* text_TMACACCENTSFILE */

#ifndef text_TMACGROFFILE
#define text_TMACGROFFFILE environ_AndrewDir("/lib/tmac/tmac.psatk")
#endif

#ifndef text_INLINEMACROS
#define text_INLINEMACROS FALSE
#endif


/* C O L O R  -- this code has been moved to print_LookUpColor()*/

#define COLORPTN "'if  \\n(zT  \\{\\\n\\X'%f %f %f setrgbcolor'\n\\}\n"
#define PUTCOLOR(R, G, B, F)   fprintf(F, COLORPTN, R, G, B )


	static void
setcolor(color,f)
	char *color;
	FILE *f;
{
	double r, g, b;
	print_LookUpColor(color, &r, &g, &b);
	PUTCOLOR(r, g, b, f);
}

static char *speclookup(c,f)
long c,f;
{
    static char foo[6];
    *foo = 0;
    switch(f){
	case 1: 
	    if ( symtran[c].out[0] == 'x' && symtran[c].out[1] == 'x')
		return NULL;
	    sprintf(foo,"\\(%c%c",symtran[c].out[0],symtran[c].out[1]);
	    break;
	case 2:
	    if ( symatran[c].out[0] == 'x' && symatran[c].out[1] == 'x')
		return NULL;
	    sprintf(foo,"\\(%c%c",symatran[c].out[0],symatran[c].out[1]);
	    break;
    }
    return foo;
}

static void PutNewlineIfNeeded()
{
    if (needNewLine) {
        putc('\n', troffFile);
        needNewLine = 0;
    }
}

static void InitializeFonts()
{
    static char *filename = NULL;
    FILE	*fontfile;
    FontDesc	*fp;
    int		font_count = 0;
    int		i, j;
    char	*tmp;
    char	name[256];
    char	font0[256];
    char	font1[256];
    char	font2[256];
    char	font3[256];
    char	font4[256];
    char	font5[256];
    char	font6[256];
    char	font7[256];
    char	font8[256];

    
    if (!filename) {
	filename = environ_Get("TROFFFONTMAPFILE");
	if (!filename)
	    filename = environ_GetProfile("TroffFontMapFile");
	if (!filename)
	    filename = environ_GetConfiguration("TroffFontMapFile");
	if (!filename)
	    filename = environ_AndrewDir(FONT_DESC_FILE);
    }

#ifdef FONT_DEBUG
    fprintf(stderr, "checking for font map file: %s\n", filename);
#endif

    if (!filename || (fontfile = fopen(filename, "r")) == NULL) {
	if (!filename)
	    filename = "NULL";
	fprintf(stderr, "Warning: unable to read %s; using default fonts!\n", filename);
	filename = NULL;
	fonttable = default_fonttable;
	return;
    }

    /*
     *   Get the number of fonts.
    */
    
#ifdef FONT_DEBUG
    fprintf(stderr, "reading the number of fonts\n");
#endif
    
    if ((i = fscanf(fontfile, "%d", &font_count)) != 1)
    {
	fprintf(stderr, "Error: bad font count line token count: %d != 1\n", i);
	fclose(fontfile);
	fonttable = default_fonttable;
	return;
    }
    
#ifdef FONT_DEBUG
    fprintf(stderr, "got: %d\n", font_count);
#endif
    
    /*
     *    Check for a valid font count.
    */
    
    if (font_count < 1)
    {
	fprintf(stderr, "Error: bad font count: %d\n", font_count);
	fclose(fontfile);
	fonttable = default_fonttable;
	return;
    }
    
    /*
     *    Allocate the font array.
    */
    
#ifdef FONT_DEBUG
    fprintf(stderr, "allocating font table.\n");
#endif
    
    fonttable =	(FontDesc *)malloc(sizeof(FontDesc) * (font_count + 1));
    
    /*
     * Read in the fonts.
    */
    
#ifdef FONT_DEBUG
    fprintf(stderr, "reading in fonts.\n");
#endif
    
    for( j = 0; j < font_count; ++j)
    {
	fp = &(fonttable[j]);
	
#ifdef FONT_DEBUG
	fprintf(stderr, "reading in font: %d\n", j);
#endif
    
	i = fscanf(fontfile, "%s %s %s %s %s %s %s %s %s %s", name,
		   font0, font1, font2, font3, font4, font5, font6, font7, font8);
	
	if (i != 10)
	{
	    fprintf(stderr, "Error: bad font line token count: %d != 10\n", i);
	    fclose(fontfile);
	    fonttable = default_fonttable;
	    return;
	}
	
#ifdef FONT_DEBUG
	fprintf(stderr, "got: [%s] [%s] [%s] [%s] [%s] [%s] [%s] [%s] [%s] [%s]\n",
		name, font1, font2, font3, font4, font5, font6, font7, font8);
#endif
	/*
	 *  Copy into font entry.
	*/
	
#ifdef FONT_DEBUG
	fprintf(stderr, "copying name.\n");
#endif

	/* convert '^' chars in name to spaces */
	for (tmp = name; *tmp; tmp++)
	    if (*tmp=='^')
		*tmp = ' ';
	fp->fontname = (char *)malloc(strlen(name)+1);
	strcpy(fp->fontname, name);

#ifdef FONT_DEBUG
	fprintf(stderr, "copying font0.\n");
#endif
	
	fp->fontcodes[0] = (char *)malloc(strlen(font0)+1);
	strcpy(fp->fontcodes[0], font0);
	
#ifdef FONT_DEBUG
	fprintf(stderr, "copying font1.\n");
#endif
	
	fp->fontcodes[1] = (char *)malloc(strlen(font1)+1);
	strcpy(fp->fontcodes[1], font1);
	
#ifdef FONT_DEBUG
	fprintf(stderr, "copying font2.\n");
#endif
	
	fp->fontcodes[2] = (char *)malloc(strlen(font2)+1);
	strcpy(fp->fontcodes[2], font2);
	
#ifdef FONT_DEBUG
	fprintf(stderr, "copying font3.\n");
#endif
	
	fp->fontcodes[3] = (char *)malloc(strlen(font3)+1);
	strcpy(fp->fontcodes[3], font3);
	
#ifdef FONT_DEBUG
	fprintf(stderr, "copying font4.\n");
#endif
	
	fp->fontcodes[4] = (char *)malloc(strlen(font4)+1);
	strcpy(fp->fontcodes[4], font4);
	
#ifdef FONT_DEBUG
	fprintf(stderr, "copying font5.\n");
#endif
	
	fp->fontcodes[5] = (char *)malloc(strlen(font5)+1);
	strcpy(fp->fontcodes[5], font5);
	
#ifdef FONT_DEBUG
	fprintf(stderr, "copying font6.\n");
#endif
	
	fp->fontcodes[6] = (char *)malloc(strlen(font6)+1);
	strcpy(fp->fontcodes[6], font6);
	
#ifdef FONT_DEBUG
	fprintf(stderr, "copying font7.\n");
#endif
	
	fp->fontcodes[7] = (char *)malloc(strlen(font7)+1);
	strcpy(fp->fontcodes[7], font7);
	
#ifdef FONT_DEBUG
	fprintf(stderr, "copying font8.\n");
#endif
	
	fp->fontcodes[8] = (char *)malloc(strlen(font8)+1);
	strcpy(fp->fontcodes[8], font8);

#ifdef FONT_DEBUG
	fprintf(stderr, "finished font.\n");
#endif
    }
    
    /*
     * Build default font.
    */
    
#ifdef FONT_DEBUG
	fprintf(stderr, "creating default font.\n");
#endif
    
    fp = &(fonttable[font_count]);
    fp->fontname = NULL;
    fp->fontcodes[0] = "R";
    fp->fontcodes[1] = "I";
    fp->fontcodes[2] = "B";
    fp->fontcodes[3] = "BI";
    fp->fontcodes[4] = "C";
    fp->fontcodes[5] = "CO";
    fp->fontcodes[6] = "CB";
    fp->fontcodes[7] = "CD";
    fp->fontcodes[8] = "B";
    fclose(fontfile);
}

static void ComputeTroffFont(name, FaceCodemodifier, FontSize)
register char *name;
register long FaceCodemodifier; 
long FontSize;
{
    register family, mod,specfamily;

    symbola = 0;
    for (family = 0; fonttable[family].fontname; family++) {
	register char *s, *t;

	for (s = name, t = fonttable[family].fontname; *s && *t; s++, t++) {
	    if (*s != *t && *s != (*t - 32) && *s != (*t + 32))
                break;
	}
	if (*s == '\0' && *t == '\0')
            break;
    }
    if(!fonttable[family].fontname){
	/* try to look up symbol table font */
	for (specfamily = 0; specfonttable[specfamily].fontname; specfamily++) {
	    register char *s, *t;

	    for (s = name, t = specfonttable[specfamily].fontname; *s && *t; s++, t++) {
		if (*s != *t && *s != (*t - 32) && *s != (*t + 32))
		    break;
	    }
	    if (*s == '\0' && *t == '\0')
		break;
	}
	symbola = specfonttable[specfamily].tablenumber;
    }
    /* Take the first modifier we find (italic-bold will be italic) */

    mod = (FaceCodemodifier & (long) fontdesc_Italic) ? 1 : 0;
    if (FaceCodemodifier & (long) fontdesc_Bold)
        mod += 2;
    if (FaceCodemodifier & (long) fontdesc_Fixed)
        mod += 4;
    if (FaceCodemodifier & (long) fontdesc_Shadow)
        mod = 8;
    dFace = mod;
    dFont = family;
    dSize = FontSize;
}

static void ChangeFont()
{
    register char *code = fonttable[dFont].fontcodes[dFace];

    if (needNewLine) {
	if (!code[1]) /* one-char font name */
	    fprintf(troffFile, "\\&\\f%s", code);
	else {
	    if (!code[2]) /* two-char font name */
		fprintf(troffFile, "\\&\\f(%s", code);
	    else /* >=3 chars, groff-style. */
		fprintf(troffFile, "\\&\\f[%s]", code);
	}
    }
    else
	fprintf(troffFile, "'ft %s\n", code);
}

static void ChangeJustification(old, new,putbreak)
enum style_Justification old, new;
boolean putbreak;
{
    if (old != new) {
	PutNewlineIfNeeded();
	if(putbreak && !(old == style_LeftJustified && new == style_LeftAndRightJustified))
	    fputs(".br\n", troffFile);
	switch (new) {
	    case style_Centered:
		fputs(".ad c\n", troffFile);
		break;
	    case style_LeftJustified:
	    case style_LeftThenRightJustified:
		fputs(".ad l\n", troffFile);
		break;
	    case style_RightJustified:
		fputs(".ad r\n", troffFile);
		break;
	    case style_LeftAndRightJustified:
		fputs(".ad b\n", troffFile);
		break;
	    default:
                /* Unknown justification code */;
		break;
	}
    }
}

static void ChangeState()
{
    int recomputefont = 0;
    static int cstatus = 0;
    int changetabs = 0;

    /* Figure out what to do for each change in state vector */
    if (sv.CurLeftMargin != nsv.CurLeftMargin) {
        PutNewlineIfNeeded();
        fprintf(troffFile, "'in %dp\n", NegOffset + nsv.CurLeftMargin);
	changetabs = 1;
    }

    if(sv.CurColor != nsv.CurColor){
	PutNewlineIfNeeded();
	setcolor(nsv.CurColor, troffFile);
    }

    if (sv.CurRightMargin != nsv.CurRightMargin) {
        PutNewlineIfNeeded();

        /* This is currently wrong, since it assumes that the page */
        /* width is 6 + NegOffset across, and since the real right margin */
        /* will be measured from the left edge, as apparently */
        /* troff does as well. See comment in troff init cmds */
        /* fprintf(troffFile, "'ll %dp\n",  LineLength  - nsv.CurRightMargin); */
        /* Fix For Above */

        fprintf(troffFile, "'ll \\n(.lu-(%dp)\n",
          nsv.CurRightMargin - sv.CurRightMargin);
    }

    if (sv.CurRightEdge != nsv.CurRightEdge) {
    }

    if (sv.CurLeftEdge != nsv.CurLeftEdge) {
    }

    if (sv.CurTopMargin != nsv.CurTopMargin) {
    }

    if (sv.CurBottomMargin != nsv.CurBottomMargin) {
    }

    if (sv.CurFontAttributes != nsv.CurFontAttributes) {
	recomputefont = 1;
    }

    if (sv.CurScriptMovement != nsv.CurScriptMovement) {
        /* fprintf(stderr,"<%d>",nsv.CurScriptMovement); */
        fprintf(troffFile, "\\v'%dp'", nsv.CurScriptMovement - cstatus);
	needNewLine = 1;
        cstatus = nsv.CurScriptMovement;
    }

    if (sv.CurFontSize != nsv.CurFontSize)
	recomputefont = 1;

    if (sv.CurIndentation != nsv.CurIndentation) {
        PutNewlineIfNeeded();
        if (nsv.CurIndentation < 0)
            fprintf(troffFile, ".ti %dp\n", nsv.CurIndentation);
        else
	    fprintf(troffFile, ".ti +%dp\n", nsv.CurIndentation);
	changetabs = TRUE;
    }

    if (sv.CurJustification != nsv.CurJustification)
	ChangeJustification(sv.CurJustification, nsv.CurJustification,TRUE);

    if (sv.SpecialFlags != nsv.SpecialFlags) {
	if(passthru && !(nsv.SpecialFlags & style_PassThru))
	    PutNewlineIfNeeded();
	if (changebar && !(nsv.SpecialFlags & style_ChangeBar)) {
	    /* ChangeBar was on, now off. */
	    fprintf(troffFile, "\n.cB\n\\&");
	    needNewLine = 1;
	} else if (!changebar && (nsv.SpecialFlags & style_ChangeBar)) {
	    /* ChangeBar was off, now on. */
	    fprintf(troffFile, "\n.Cb\n\\&");
	    needNewLine = 1;
	}
	passthru = (nsv.SpecialFlags & style_PassThru);
        underlining = (nsv.SpecialFlags & style_Underline);
        changebar = (nsv.SpecialFlags & style_ChangeBar);
        strikethrough = (nsv.SpecialFlags & style_StrikeThrough);
        overbar = (nsv.SpecialFlags & style_OverBar);
	/* HideText = (nsv.SpecialFlags & style_Hidden); */
    }

    if (sv.CurFontFamily != nsv.CurFontFamily)
	recomputefont = 1;

    /* Check for tab stop changes */
    /* The changetabs state variable is used to force
      reinitializing of tabs after the margin moved.  This is
      unnecessary if we are tabbing with spaces. -wdc */
    /* Actions:
      nsv.tabchars == 1 && sv.tabchars == 1: do nothing
      nsv.tabchars == 1 && sv.tabchars == 0: do space tabs
      nsv.tabchars == 0 && sv.tabchars == 1: force regular tabs
      nsv.tabchars == 0 && sv.tabchars == 0: do regular tabs if necessary */

    if (nsv.SpecialFlags & style_TabsCharacters) {
	if (!(sv.SpecialFlags & style_TabsCharacters)) {
	    /* only output tabstops if we havent done so already */
	    fprintf (troffFile, "'ta %dn\n", tabscharspaces);
	}
    } else {
	if (sv.SpecialFlags & style_TabsCharacters) changetabs = TRUE; /* Force change tabs if we just stopped tabbing with spaces */
	if (!changetabs)
	    changetabs = tabs_Different(sv.tabs, nsv.tabs);

	if (changetabs) {
            if (needNewLine) fprintf(troffFile,"\\c");
	    PutNewlineIfNeeded();
	    tabs_OutputTroff(nsv.tabs, nsv.CurIndentation, troffFile);
	    resetTabs = FALSE;
	}
    }
    
    tabs_Death(sv.tabs);
    sv = nsv;
    sv.tabs = tabs_Copy(nsv.tabs);

    if (recomputefont) {
        ComputeTroffFont(sv.CurFontFamily,
          sv.CurFontAttributes, sv.CurFontSize);    /* Sets dFont, dFace, dSize */
	ChangeFont();                               /* Set default font */
	if (cSize != dSize) {
	    if (needNewLine) {
#ifdef GROFF_ENV
		if (dSize > 39)  /* BUG GROFF */
		    fprintf(troffFile, "\n.ps %d\n\\&", dSize);
		else
		    fprintf(troffFile, "\\s%d\\&", dSize);
#else
		fprintf(troffFile, "\\s%d\\&", dSize);
#endif /* GROFF_ENV */
	    }
	    else
		fprintf(troffFile, ".ps %d\n", dSize);	/* set point size */
	    
	    cSize = dSize;
	}

    }

    latestVS = ((cSize <= 12) ? (cSize + 2) : (cSize * 14 / 12)) + sv.CurSpacing;

    if (currentVS < latestVS)  {
	extraVS = latestVS - currentVS;
    }
    else  {
	extraVS = 0;
    }

    if (sv.CurSpread > sv.CurSpacing) {
	latestSpread = sv.CurSpread - sv.CurSpacing;
    }
    else {
	latestSpread = 0;
    }
}

static setdefaultstate()
{
    /*	Encounted a style that encompasses whole document */
    /*  dFont, dFace, dSize are already set at this point */
/*     code for this need to be written 
     printf("Setting default to %d, %d, %d\n",dFont,dSize,dFace);
     fflush(stdout);
*/
}
/* PSmacros define PB and PE to surround a postscript insertion.  */
/*
 * These macros now moved into tmac.atk */

static void InitializeStyle()
{
    long fontSize = 12;
    char bodyFont[100];
    char *font;
    long fontStyle = fontdesc_Plain;
    boolean justify;

    defaultStyle = style_New();
    style_SetName(defaultStyle, "printdefault");

    if (environ_ProfileEntryExists("print.bodyfont", FALSE))
	font = environ_GetProfile("print.bodyfont");
    else
	font = environ_GetProfile("bodyfont");

    if (font == NULL || ! fontdesc_ExplodeFontName(font, bodyFont,
      (long) sizeof(bodyFont), &fontStyle, &fontSize))
	strcpy(bodyFont, "Andy");

    justify = environ_GetProfileSwitch("justified", TRUE);

    style_SetFontFamily(defaultStyle, bodyFont);
    style_SetFontSize(defaultStyle, style_ConstantFontSize, (long) fontSize);
    style_AddNewFontFace(defaultStyle, fontStyle);

    if (! justify)
	style_SetJustification(defaultStyle, style_LeftJustified);
}

/* OutputInitialTroff(f, cenv) */
/* Generates the standard stuff at the beginning of the troff stream */
/* The current environment is used to set font, font size, and adjust mode. */
static handlemac(f,s)
FILE *f;
char *s;
{
    FILE *fi,*fopen();
    register int c;
    if(InlineMacros && ((fi = fopen(s,"r")) != NULL)){
	while((c = getc(fi)) != EOF) putc(c,f);
	fclose(fi);
    }
    else fprintf(f, ".so %s\n",s);
}

static void OutputInitialTroff(f, toplevel, cenv)
register FILE *f;
boolean toplevel;
struct environment *cenv;
{
/*     register char **mx; */
    register int i;

    tabscharspaces = environ_GetProfileInt("TabsCharSpaces", 8);
    
    troffFile = f;
    needNewLine = 0;
    addNewLine = 0;

    /*
     * We don't want to use environments, troff uses themselves
     * fprintf(f, ".ev %d\n", (textLevel > 2) ? 2 : textLevel);
     *
     */

    if (toplevel) {
	char *macfile;
	/*
	 * cleaning up
	 * everything done here before will no be done in a macro file
	 */

	/*
	 * Built in pathname for now. Needs to be cleaned up
	 */
	if((macfile = environ_GetProfile("tmacaccentsfile")) != NULL || (macfile = environ_GetConfiguration("tmacaccentsfile")) != NULL)
	    handlemac(f,macfile);
	else handlemac(f,text_TMACACCENTSFILE);

	if((macfile = environ_GetProfile("tmacfile")) != NULL ||
	   (macfile =environ_GetConfiguration("tmacfile")) != NULL)
	    handlemac(f,macfile);
	else handlemac(f,text_TMACFILE);

	fprintf(f, ".\\\" If \"\\*(.T\" is \"ps\", we're presumably using \"groff\"; set \"zT\" to 1,\n.\\\" and load up the \"groff\" version of the macros (which use \"groff\"\n.\\\" features, and, as such, might not be usable in a package that\n.\\\" Boring Old \"troff\" and \"ditroff\" also have to read).\n.\\\"\n.if \"\\*(.T\"ps\" \\{\\\n.	nr zT 1\n");
	if((macfile = environ_GetProfile("tmacgrofffile")) != NULL ||
	   (macfile =environ_GetConfiguration("tmacgrofffile")) != NULL || (macfile = environ_Get("ATKTMACGROFFFILE")) != NULL)
	    handlemac(f,macfile);
	else {
	    handlemac(f, text_TMACGROFFFILE);
	}
	fprintf(f, ".\\}\n");

	fprintf(f, ".IZ\n");	/*initialise random defaults */
#ifdef GROFF_ENV
	i = strlen(macfile);
	/* strange hack to change the layout parameters if your macro file ends with the string ".fullpage". I don't know if this is pretty, but it will satisfy (or at least not confuse) everyone */
	if (i>=9 && !strcmp(macfile+i-9, ".fullpage")) {
	    PageOffset = 18;                    /* Page offset in points */
	    LineLength = 558;                   /* Line length in points */
	    NegOffset  = 18;
	}
	else {
	    PageOffset = 54;                    /* Page offset in points */
	    LineLength = 468;                   /* Line length in points */
	    NegOffset = 36;
	}
#else
	PageOffset = 54;                    /* Page offset in points */
	LineLength = 468;                   /* Line length in points */
	NegOffset = 36;
#endif /* GROFF_ENV */
        fprintf(f,".nr IN %dp\n", NegOffset);   /* Makes "0" indentation */

	/* Adjust the lengths of the title lens and margins for */
        /* headers (w/o phony left space for outdenting) */

	fprintf(f, ".nr LT %dp\n", LineLength - NegOffset);

	/* Reset the left hand margin for the document */

	fprintf(f,".nr PO %dp\n", PageOffset);

    }

    if (defaultStyle == NULL) {
	InitializeStyle();
	InitializeFonts();
    }

    text_InitStateVector(&sv);
    text_ApplyEnvironment(&sv, defaultStyle, cenv);
    ChangeJustification((enum style_Justification) - 1, sv.CurJustification,toplevel);

    ComputeTroffFont(sv.CurFontFamily, sv.CurFontAttributes,
      sv.CurFontSize);              /* Sets dFont, dFace, dSize */

    ChangeFont();                   /* Set default font */
    fprintf(f, ".nr PS %d\n", dSize);  /* Set point size */
    fprintf(f, ".ps \\n(PS\n");
    cSize = dSize;

    currentVS = ((dSize <= 12) ? (dSize + 2) : (dSize * 14 / 12)) + sv.CurSpacing;
    latestVS = currentVS;
    extraVS = 0;
    if (sv.CurSpread > sv.CurSpacing) {
	currentSpread = sv.CurSpread - sv.CurSpacing;
    }
    else {
	currentSpread = 0;
    }

    fprintf(f, ".nr VS %dp\n", currentVS);        /* Set interline spacing and tabs */
    fprintf(f, ".vs \\n(VSu\n");
    fprintf(f, ".nr EN %dn\n", tabscharspaces);


    if (toplevel) {
	char *val;

	fputs(".sp 0.5i\n", f);     /* Space down for start of page */

	if((val = environ_Get("Duplex")) != NULL){
	    printDuplex = (*val == 'n' || *val == 'N')? FALSE:TRUE;
	}
	else {
	    printDuplex = environ_GetProfileSwitch("Duplex",DUPLEXBYDEFAULT);
	}
	if (printDuplex) fputs(".nr DP 0\n", f);
	else fputs(".nr DP 1\n", f);

	/* one or two char font name? */
	i = strlen(fonttable[dFont].fontcodes[dFace]);
	/* set font for footnote number in text, I assume here that
	    dFont & dFace are actually the typeface used for the body font
		(from the templates) but a quick test learned they are not
		  Aaaaargh */
	{
	    char *code = fonttable[dFont].fontcodes[dFace];
	    /*fprintf(f, ".ds Fn %s%s\n", i > 1 ? "\\f(" : "\\f", code);*/
	    if (!code[1]) /* one-char font name */
		fprintf(troffFile, ".ds Fn \\f%s\n", code);
	    else {
		if (!code[2]) /* two-char font name */
		    fprintf(troffFile, ".ds Fn \\f(%s\n", code);
		else /* >=3 chars, groff-style. */
		    fprintf(troffFile, ".ds Fn \\f[%s]\n", code);
	    }
	}

	fprintf(f, ".ds HF %s\n", fonttable[dFont].fontcodes[dFace]);
	fputs(".nr HS \\n(.s\n", f);
	fprintf(f, ".ds FF %s\n", fonttable[dFont].fontcodes[dFace]);
	fputs(".nr FS \\n(.s\n", f);
	fprintf(f, ".RS\n");	/* init real defaults */
	if (sv.CurLeftMargin != 0) {
	    fprintf(troffFile, ".in %dp\n", NegOffset + sv.CurLeftMargin);
	}
	if (sv.CurRightMargin != 0) {
	    fprintf(troffFile, ".ll \\n(.lu-(%dp)\n", sv.CurRightMargin);
	}
	if (sv.SpecialFlags & style_TabsCharacters) {
#if 1
	    fprintf (troffFile, "'ta %dn\n", tabscharspaces);
#else
	    fprintf (troffFile, "'ta \\w'        'u +\\w'        'u +\\w'        'u +\\w'        'u +\\w'        'u +\\w'        'u +\\w'        'u +\\w'        'u +\\w'        'u +\\w'        'u +\\w'        'u +\\w'        'u\n");
#endif
	} else {
	    tabs_OutputTroff(sv.tabs, sv.CurIndentation, troffFile);
	}
	/*RSKadd:*/
	if (sv.SpecialFlags & style_ContinueIndent) {
	    int i=5;
	    fprintf(troffFile, "'in \\w'");
	    while (i--) fprintf(troffFile, " ");
	    fprintf(troffFile, "'u\n'ti -\\w'     'u\n");
	    leadingwhitespace= 0;
	} else
	    /*:RSKadd*/

	    if (sv.CurIndentation < 0)
		fprintf(troffFile, ".ti %dp\n", sv.CurIndentation);
	    else if (sv.CurIndentation > 0)
		fprintf(troffFile, ".ti +%dp\n", sv.CurIndentation);
    }

    if (environ_GetProfileSwitch("hyphenate", 0))
	fputs(".hy\n", f);
    else
	fputs(".nh\n", f);
}

static int barPending;

static int FlushBars(f)
FILE *f;
{
    if (barPending) {
        char buf[128];

        barPending = 0;

        /* Start position of bar(s) is currently in troff register X. */
        /* Find length of bar(s) in troff register Y */

        strcpy(buf, "\\kY");

        if (underlining)    /* Move back and draw underline */
            strcat(buf, "\\h'|\\nXu'\\l'|\\nYu\\(ul'");
        if (strikethrough)      /* Move back and draw change-bar */
            strcat(buf, "\\h'|\\nXu'\\u\\l'|\\nYu\\(ul'\\d");
        if (overbar)        /* Move back and draw over-bar */
            strcat(buf, "\\h'|\\nXu'\\l'|\\nYu\\(rn'");

        fputs(buf, f);
        return strlen(buf);
    } else
        return 0;
}

static void FlushLineSpacing(cs, hitchars, needbreak)
int cs;
int hitchars;
boolean needbreak;
{
    /* Put out .sp for subsequent new lines  */

    PutNewlineIfNeeded();

    if (needbreak) {
        fputs(".OC\n", troffFile);
    }
    if (cs == 1) {
	if (currentSpread != 0) {
	    fprintf(troffFile, ".sp %dp\n", currentSpread);
	}
    }
    else if (cs > 1) {
	cs--;
	if(hitchars == 0) /* space past trap */
	    fprintf(troffFile, ".sv %d\n", cs);
	else fprintf(troffFile, ".sp %dp\n", cs * (currentVS + currentSpread) + currentSpread);
    }
    currentSpread = latestSpread;

    /* Restore tabs after temporary setting of indent to 0.
      If we're doing character spaced tabs, resetTabs wasnt set by
      us.  We save it for who ever DID set it. 
      I'm unwilling to say that it will never be set under these
      conditions. -wdc */
    if (sv.CurIndentation != 0) {
	if (resetTabs  && !(sv.SpecialFlags & style_TabsCharacters)) {
	    tabs_OutputTroff(sv.tabs, sv.CurIndentation, troffFile);
	    resetTabs = FALSE;
	}

	/*RSKadd:*/
	if (sv.SpecialFlags & style_ContinueIndent) {
	    int i=leadingwhitespace+5;
	    fprintf(troffFile, "'in \\w'");
	    while (i--) fprintf(troffFile, " ");
	    fprintf(troffFile, "'u\n'ti -\\w'     'u\n");
	    leadingwhitespace= 0;
	} else
	    /*:RSKadd*/

	    if (sv.CurIndentation < 0) 
		fprintf(troffFile, "'ti %dp\n", sv.CurIndentation);
	    else 
		fprintf(troffFile, "'ti +%dp\n", sv.CurIndentation);
    }
    if (latestVS != currentVS)  {
	fprintf(troffFile, ".vs %d\n", latestVS);
	currentVS = latestVS;
	extraVS = 0;
    }
}
#define UNMATCHED -1
#define FOOTNOTE 0
#define INDEX 1
#define INVINDEX 2
#define NORMAL 0
#define NOFORMAT 1
#define NOPRINT 2
static char **namelist;
static char listheader[] = 
    "footnote,index,indexi"
   ;
static char defaultlist[] = 
/*    "majorheading,heading,subheading,chapter,section,subsection,paragraph,function" */
"chapter,section,subsection,paragraph"
;
static formatnote;
static findinlist(lst,cnt,str)
char **lst; 
int cnt;
char *str;
{
    int i;
    for(i = 0; i < cnt; i++,lst++){
	if(*lst == NULL || str == NULL) return -1;
	if(**lst == *str && (*lst == str || strcmp(*lst,str) == 0)){
	    return i;
	}
    }
    return -1;
}
static appendlist(lst,cnt,ostr,TEST)
char **lst;
int cnt;
char *ostr;
int TEST;
{   /* BUG -- OVERFLOWS NOT DETECTED */

    char *str;
    long len;
    int next = 1;
    ;
    if(ostr == NULL || (len = strlen(ostr)) == 0 || ((str = malloc(len + 1)) == NULL)) return;
/* NEED A REASONABLE WAY TO NOTE str TO FREE LATER */
    strcpy(str,ostr);
    if(TEST){
	if(findinlist(lst,cnt,str) != -1) return cnt;
    }
    while(*str){
	if(*str == ',' || *str == '\n') {
	    *str = '\0';
	    next++;
	}
	else if(*str == '\0') break;
/*	else if(*str == ' ') ; */
	else if(next){
	    lst[cnt++] = str;
	    next = 0;
	}
	str++;
    }
    lst[cnt] = NULL;
    return cnt;
}
static int lookup(s)
char *s;
{
    char **p;
    int i = 0;
    for(p = namelist ; p && *p; p++,i++){
/* fprintf(stderr,"Testing %s %s\n",*p,s); */
	if(**p == *s && strcmp(*p,s) == 0) return i;
    }
    return UNMATCHED;
}
/* Writes the document out in troff format into a file  */
static void endspecialformating()
{
    PutNewlineIfNeeded();
    fprintf(troffFile,".FE\n");
    formatnote = -1;
}
static deletenewlines(buf)
char *buf;
{
    register char *c;
    for(c = buf; *c != '\0'; c++){
	if(*c == '\n') *c = ' ';
    }
    for(c--; c > buf; c--){
	if(isspace(*c))  *c = '\0';
	else break;
    }
}
static deletechapnumbers(buf)
char *buf;
{
    register char *c,*s;
    s = buf;
    if(*s <= '9' && *s >= '0' && (c = index(buf,'\t')) != NULL){
	c++;
	do{
	    *s++ = *c;
	} while (*c++ != '\0');
    }
}
static insert(src,c)
char *src,*c;
{   /* inserts string src into the begining of string c , assumes enough space */
    char *p,*enddest;
    enddest = c + strlen(c);
    p = enddest + strlen(src);
    while(enddest >= c) *p-- = *enddest-- ;
    for(p = src; *p != '\0';p++)
	*c++ = *p;
}
static quote(buf,c,len)
char *buf,c;
int len;
{
    char *ebuf ;
    int cfree;
    int clen = strlen(buf);
    ebuf = buf +  clen - 1;
    cfree = len - clen;
    if(cfree <= 0) return;
    while(ebuf >= buf){
	if(*ebuf == c){    
	    insert("\\",ebuf);
	    if(--cfree == 0) return;
	}
	ebuf--;
    }
}
static outputendnote()
{
    fprintf(troffFile,"%d ",endnotes++);
}
static handlespecialformating(d,env,pos,len)
struct text *d;
struct environment *env;
long pos,len;
{
    struct style *st;
    struct content_chapentry *centry;
    char buf[256],*sn,*bbf,*sbuf;
    long type;
    int ch;
/* printf("pos = %d formatnore = %d\n",pos,formatnote);
fflush(stdout); */
    if(pos == formatnote){
	if(endnotes == FALSE){
	    endspecialformating();
	}
    }
    else if(pos < formatnote){
	if(endnotes == TRUE){
	    return NOPRINT;
	}
	else {
	    return NOFORMAT; /* could probably handle allowing bold and italic here */
	}
    }
    if(env->type == environment_View){
	struct viewref *vr;
	vr = env->data.viewref;
	if(*(vr->viewType) == 'f' && strcmp(vr->viewType,"fnotev") == 0)
	    return NOFORMAT;
    }
    if(env->type != environment_Style) return NORMAL;
    st = env->data.style;
    if(st == NULL || ((sn = style_GetName(st)) == NULL)){
/* fprintf(stderr,"Null style\n"); */
	return NORMAL;
    }
    type = lookup(sn);
    switch(type){
	case UNMATCHED :
 /* fprintf(stderr,"Returning Normal\n"); */
	    return NORMAL;
#ifdef REFER
	case REFER:
	    PutNewlineIfNeeded();
	    if(len > 255) len = 255;
	    text_CopySubString(d,pos,len,buf,TRUE);
	    deletenewlines(buf);
	    quote(buf,'"',255);
	    fprintf(troffFile,".[ [\n%s\n.]]\n",buf);
	    return NOPRINT ;
#endif
	case FOOTNOTE :
	    fprintf(troffFile, "\\**\n");	/* automatic footnote counter string */
	    needNewLine = 0;
	    if(endnotes == TRUE){
/*		outputendnote(); */
		formatnote = pos + environment_GetLength(env) - 1;
		return NOPRINT;
	    }
	    else {
		fprintf(troffFile,".FS\n");
		formatnote = pos + environment_GetLength(env) - 1;
		return NOFORMAT;
	    }
	case INDEX :
	case INVINDEX :
	    if(addindex) {
		PutNewlineIfNeeded();
		if(len > 255) len = 255;
		text_CopySubString(d,pos,len,buf,TRUE);
		deletenewlines(buf);
		deletechapnumbers(buf);
		quote(buf,'"',255);
		fprintf(troffFile,".ix \"%s\"\n",buf);
	    }
	    if(type == INVINDEX) return NOPRINT ;
	default:    /* table of contents entry */
	    if(con && ((centry = content_CopyEntry(con,pos,len,buf,255)) != NULL)) {
		if(centry == lastcentry){
		    return NORMAL;
		}
		lastcentry = centry;
		PutNewlineIfNeeded();
		/*
		 printf("Copy Entry returned %s<\n",buf);
		 fflush(stdout);
		 */
	    }
	    else {
		if(type == INDEX) return NORMAL;
		PutNewlineIfNeeded();
		if(len > 255) len = 255;
		text_CopySubString(d,pos,len,buf,FALSE);
		deletenewlines(buf);
		centry = NULL;
	    }
	    
	    quote(buf,'"',255);
	    for(sbuf = buf;*sbuf != '\0' && isspace(*sbuf);sbuf++) ;
	    bbf = index(sbuf,'\t');
	    if(bbf && isdigit(*sbuf) && (centry == NULL || centry->space == -1)) {
		char *c;
		long n;
		*bbf++ = '\0';
		for(c = sbuf,n = 1; *c; c++)
		    if(*c == '.') n++;
		for(ch = text_GetChar(d,pos); isspace(ch); ch = text_GetChar(d,pos))
		    pos++;
		if(con != NULL && *sbuf != text_GetChar(d,pos) && text_Strncmp(d,pos,sbuf,strlen(sbuf)) != 0){
		    fprintf(troffFile,".HE\n");
		    if(printContents)fprintf(troffFile,".IC %d \"%s\" NO\n",n,bbf);
		}
		else {
		    fprintf(troffFile,".HE\n");
		    if(printContents)fprintf(troffFile,".IC %d \"%s\" %s\n",n,bbf,sbuf);
		    fprintf(troffFile,".iw %s \"%s\"\n",sbuf,bbf);
		}
	    }
	    else if(*sbuf){
		fprintf(troffFile,".HE\n");
		if(printContents){
		    long n;
		    if(centry != NULL) n = centry->space;
		    else n = -1;
		    fprintf(troffFile,".IC %d \"",n + 1);
		    n = n *INDENTSPACE;
		    while(n-- > 0) putc(' ',troffFile);
		    fprintf(troffFile,"%s\" NO\n",sbuf);

		}
	    }
	    return NORMAL;
    }

}
#if 0
struct text *texttroff__CompileNotes(classID,txt)
struct classheader *classID;
struct text *txt;
{
    return CompileNotes(self,txt,txt->rootEnvironment, 0,TRUE);

}
static struct text *CompileNotes(srctext,  env, startpos, topLevel)
    struct text *srctext;
    struct environment *env;
    long startpos;
    int topLevel;		/* top level call is slightly different */
{
    struct environment *child;
    register int pos, cpos;
    long endpos,place;
    static long count; 
    static struct text *txt,*newtxt;
    char foo[36],*sn;
    struct viewref *vr;
    struct style *st;

    endpos = startpos + environment_GetLength(env);
    if (topLevel ){
	endpos = text_GetLength(srctext);
	count = 1;
	txt = text_New();
    }
    for (pos = startpos; pos < endpos;) {
	child = environment_GetInnerMost(env, pos-startpos);
	cpos = pos + environment_GetNextChange(env, pos-startpos);
	if (cpos > endpos)
	    cpos = endpos;

	if (child != env) {
	    CompileNotes(srctext, child, pos, FALSE);
	    pos += environment_GetLength(child);
	}
	if(env->type == environment_Style){
	    st = env->data.style;
	    if(st != NULL && ((sn = style_GetName(st)) != NULL) &&
	       *sn == 'f' && strcmp(sn,"footnote") == 0){
		sprintf(foo,"\n%d   ",count++);
		text_InsertCharacters(txt,text_GetLength(txt),foo, strlen(foo));
		place = text_GetLength(txt);
		text_CopyText(txt,place,srctext ,pos,environment_GetLength(env));
		environment_Remove(txt->rootEnvironment, place, environment_GetLength(env), environment_Style, FALSE);
	    }
	}
	else  /* look for footnote object */
	    if(env->type == environment_View){
		vr = env->data.viewref;
		if(*(vr->viewType) == 'f' && strcmp(vr->viewType,"fnotev") == 0){
		    newtxt = (struct text *) vr->dataObject;
		    sprintf(foo,"\n%d   ",count++);
		    text_InsertCharacters(txt,text_GetLength(txt),foo, strlen(foo));
		    text_CopyText(txt,text_GetLength(txt), newtxt,0,text_GetLength(newtxt));
		}
	    }
	pos = cpos;
    }
    return txt;
}
  
#endif /* 0 */

void texttroff__WriteSomeTroff(classID, view, dd, f, toplevel, flags)
struct classheader *classID;
struct view *view;
struct dataobject *dd;
FILE * f;
int toplevel;
unsigned long flags;
{
    int elen, cs, ln , flag,count,indexfontface,hitchars;
    register long i, doclen;
    register struct text *d,*ttxt;
    register boolean quotespace; 
    struct environment *cenv, *nenv;
    char *list[64],*p,*val;	
    struct style *IndexStyle;
    struct text_statevector safesv;
#ifdef ENUMERATE
    char *ChapNumber;
    if(toplevel){
	if(environ_Get("AutoEnumerate") != NULL ||
	   environ_GetProfileSwitch("AutoEnumerate",FALSE) == TRUE ||
	   environ_Get("InitialChapNumber") != NULL ||
	   environ_GetProfileSwitch("InitialChapNumber",FALSE) == TRUE)
	    enumerate = TRUE;
	else enumerate = FALSE;
	lastcentry = NULL;
    }
#endif /* ENUMERATE */
    ttxt = NULL;
    
    if(toplevel){
	count = appendlist(list,0,listheader,FALSE);
	if((p = environ_Get("ContentsList")) == NULL){
	    if((p = environ_GetProfile("ContentsList"))== NULL)
		p = defaultlist;
	}
	appendlist(list,count,p,FALSE);
	namelist = list;
	if((val = environ_Get("PrintContents")) != NULL){
	    printContents = (*val == 'n' || *val == 'N')? FALSE:TRUE;
	}
	else {
	    printContents = environ_GetProfileSwitch("PrintContents",CONTENTSBYDEFAULT);
	}
	if((val = environ_Get("InlineMacros")) != NULL ){
	    InlineMacros = (*val == 'n' || *val == 'N')? FALSE:TRUE;
	}
	else {
	    if((val = environ_GetConfiguration("InlineMacros")) != NULL){
		InlineMacros = (*val == 'n' || *val == 'N')? FALSE:TRUE;
	    }
	    else InlineMacros = text_INLINEMACROS;
	    InlineMacros = environ_GetProfileSwitch("InlineMacros",InlineMacros);
	}
	fflush(stderr);
    }
    passthru = 0;
    underlining = 0;
    changebar = 0;
    strikethrough = 0;
    overbar = 0;
    if(toplevel){
	fnote_CloseAll((struct text *)dd);
	endnotes = TRUE;
	if((!ENDNOTESONLY) && (environ_Get("Endnotes") == NULL) &&
	   (environ_GetProfile("Endnotes")== NULL)){
	    endnotes = FALSE;   
	}
    }
/* fprintf(stderr,"endnotes = %s\n",(endnotes == FALSE) ? "FALSE":"TRUE");fflush(stderr); */  
    textLevel++;
    d = (struct text *) dd;
    cenv = d->rootEnvironment;  /* Initial environment */
    if(toplevel && (IndexStyle = stylesheet_Find(d->styleSheet,"index" )) != NULL){
	indexfontface = style_GetAddedFontFaces(IndexStyle);
	style_ClearNewFontFaces(IndexStyle);
    }
    if(toplevel) addindex = (environ_Get("IndexOnly") != NULL);

    if(toplevel == TRUE || (class_IsTypeByName(class_GetTypeName(dd),"fnote") == FALSE)){
	if(toplevel != TRUE) {
	    /* don't kill safesv tabs - safesv is uninitialized */
	    safesv = sv;
	    /* Clever optimization:
	     We're done with sv's tabs.  We could:
	    safesv.tabs = tabs_Copy(sv.tabs);
	    text_FinalizeStateVector(&sv);
	     but since that just has the effect of an increment
	     and a decrement of the link count, we cleverly
	     do nothing -wdc */
	}
	OutputInitialTroff(f, toplevel, cenv);
    }
    else {
	/* don't kill safesv tabs - safesv is uninitialized */
	safesv = sv;
	/* Clever optimization:
	 We're done with sv's tabs.  We could:
	safesv.tabs = tabs_Copy(sv.tabs);
	text_FinalizeStateVector(&sv);
	 but since that just has the effect of an increment
	 and a decrement of the link count, we cleverly
	 do nothing -wdc */

	if (defaultStyle == NULL)
	    InitializeStyle();

	text_InitStateVector(&sv);
	text_ApplyEnvironment(&sv, defaultStyle, cenv);
    }
    ln = 0;
    i = 0;
    cs = 0;                     /* start w/ .br or proper line spacing */
    doclen = text_GetLength(d);
#ifdef ENUMERATE
    if(toplevel){
	ChapNumber =  environ_Get("InitialChapNumber");
	con	= content_New();
	content_SetSourceText(con,d);
	if(enumerate){
	    content_Enumerate(con,0,doclen,ChapNumber);
	    content_UpdateSource(con,0,doclen);
	    doclen = text_GetLength(d);
	}
    }
#endif /* ENUMERATE */
    formatnote = -1;
    if(toplevel){
	int lastnote;
	ttxt = text_New();
	lastnote = fnote_CopyAll(d,ttxt,1,TRUE);
	if(lastnote == 1){ /* no footnotes */
	    text_Destroy(ttxt);
	    ttxt = NULL;
	}
	else{
#ifdef NOCHILDREN
	    if(environment_NumberOfChildren(ttxt->rootEnvironment) > 0)
		/* footnotes can't be displayed with styles, 
		    so process as endnotes */
		endnotes = TRUE;
	    else 
#endif
		if(endnotes == FALSE){
		    text_Destroy(ttxt);
		    ttxt = NULL;
	    }
	}
	fnotev_SetEndnote(endnotes);
    }

/*fprintf(stderr,"::endnotes = %s\n",(endnotes == FALSE) ? "FALSE":"TRUE");fflush(stderr); */
    hitchars = 0;
    quotespace = TRUE;
    while (i < doclen) {
	nenv = environment_GetInnerMost(d->rootEnvironment, i);
	elen = environment_GetNextChange(d->rootEnvironment, i);
	if (elen > doclen)
	    elen = doclen - i;
	if (cenv != nenv) {	/* change environment */
	    text_InitStateVector(&nsv);
	    text_ApplyEnvironment(&nsv, defaultStyle, nenv);
	    if(toplevel)
		flag = handlespecialformating(d,nenv,i,elen); /* flag = Normal,NoFormat, or NoPrint */
	    else{
		flag = NORMAL;
	    }
	    cenv = nenv;
	    if(flag == NORMAL){ 
		ChangeState();
		if(i == 0){
		    if(environment_GetLength(nenv) == doclen) setdefaultstate();
		    FlushLineSpacing(0,hitchars,FALSE); /* sets vertical spacing */
		}
	    }
	    else if(flag == NOPRINT){
		i += elen ;
		continue;
	    }
            if (nenv->type == environment_View) {
		boolean needta;
                struct viewref *vr = nenv->data.viewref;
                if ((sv.CurView = (struct view *)
                  dictionary_LookUp(view, (char *) vr)) == NULL) {
	            if (class_IsTypeByName(vr->viewType, "view")
                      && (sv.CurView = (struct view *)
                      matte_Create(vr, (struct view *) view)) != NULL) {
                        viewref_AddObserver(vr, view);
			dictionary_Insert(view, (char *) vr, (char *) sv.CurView);
                    }
                } else
                    if (sv.CurView == (struct view *) textview_UNKNOWNVIEW)
                        sv.CurView = NULL;

		 if (sv.CurView != NULL) {
		     if(strcmp(vr->viewType,"fnotev") == 0){
			 if (cs) {
			     FlushLineSpacing(cs,hitchars,TRUE);
			     cs = 0;
			 }
			 needNewLine = 0;
			 needta = FALSE;
		     }
		     else if(strcmp(vr->viewType,"bpv") == 0){
			 if(cs){
			     FlushLineSpacing(1,hitchars,TRUE);/* throw away unneeded newlines */
			     cs = 0;
			 }
			 needta = FALSE;
			 hitchars = 0;
			 PutNewlineIfNeeded();
		     }
		     else {
			 if (cs) {
			     FlushLineSpacing(cs,hitchars,TRUE);
			     cs = 0;
			 }
			 needta = TRUE;
			 PutNewlineIfNeeded();
		     }
#ifdef VIEWREF_DESIREDSIZE_SET	
		     /* I would use this code, but at the moment the desired width and height don't get set in the viewref. */	     
		     if(vr->desw>0 && vr->desh>0) {
			 /* set up the desired width and height in troff registers,
			  this isn't really sensible, to do it right requires that we know the
			  desired physical size. not pixel size. oh well, individual insets
			  can make whatever assumptions they like. */
			 fprintf(".nr vw %d\n", vr->desw); /* put the desired width and height in regs */
			 fprintf(".nr vh %d\n", vr->desh);
			 fprintf(".nr ds 1\n"); /* flag that the vh and vw regs are set. */
		     } else fprintf(".nr ds 0\n"); /* flag that the vh and vw regs are nonsense */
#endif /* VIEWREF_DESIREDSIZE_SET */		     
		     view_Print(sv.CurView, f, "troff", "PostScript", 0);
		     if(needta)
			 /* reset tab stops, as table is want to mess them up */
			 if (sv.SpecialFlags & style_TabsCharacters) {
			     fprintf (troffFile, "'ta %dn\n", tabscharspaces);
			 } else {
			     tabs_OutputTroff(sv.tabs, (resetTabs) ? 0 : sv.CurIndentation, troffFile);
			 }
		 }
	    }
	    text_FinalizeStateVector(&nsv);
	}	/* End change environment */

	elen += i;		/* Bump by current position */

	if (! needNewLine){
            ln = 0;
	    quotespace = TRUE;
	}

	if (nenv->type != environment_View) {
            int c, insideWord = 0;

            barPending = 0;
	    if(!needNewLine && i > 0 && text_GetChar(d, i) == ' '){
		/* Fix bug with space following style change forceing newline*/
		i++;
		if(i < elen && text_GetChar(d, i) == ' '){
		    fputs("\\c\n",f);
		}
	    }
	    if(toplevel && i == formatnote && endnotes == FALSE)
		endspecialformating();
            while (i < elen) {
		if (passthru){
		    if (cs) {
			FlushLineSpacing(cs,hitchars,TRUE);
			cs = 0;
		    }
		    /* Put out passthru stuff as we see it */
		    if ((c = text_GetChar(d, i)) == '\n') {
			needNewLine = 0;
			ln = 0;
		    } else {
			needNewLine = 1;
			ln++;
		    }
		    fputc(c, f);
		    i++;
		    continue;
		}  /* end passthru */

		if ((c = text_GetChar(d, i)) == '\n') {
		    cs++;	/* count line Spacing */
		    i++;
                    insideWord = 0;
                    ln = FlushBars(f);

		    /*RSKadd:*/
		    if (sv.SpecialFlags & style_ContinueIndent) {
			int ch;
			for (leadingwhitespace=0; (ch=text_GetChar(d, i))==' ' || ch=='\t'; i++)
			    if (ch=='\t')
				/* just hope nobody's ever gonna use ContinueIndent WITHOUT TabsCharacters.  If they do, this calculation is totally bogus: */
				/* leadingwhitespace= (leadingwhitespace+8)&~7; */
				leadingwhitespace = ((leadingwhitespace+tabscharspaces)/tabscharspaces)*tabscharspaces;
			    else
				leadingwhitespace++;
		    }
		    /*:RSKadd*/

		    continue;
		}

		if (cs) {
                    FlushLineSpacing(cs,hitchars,TRUE);
		    cs = 0;
		    quotespace = TRUE;
                }
		hitchars++;
                /* The bar style is broken up into a separate region */
                /* surrounding each word and each intervening */
                /* group of white spaces.  This is because we cannot */
                /* know when the filled troff output will wrap and */
                /* mess up the drawing. */

                if (insideWord) {
                    if (c == ' ' || c == '\t') {
                        insideWord = 0;
			ln += FlushBars(f);
                    }
                } else {
                    if (c != ' ' && c != '\t') {
			insideWord = 1;
			if (extraVS != 0)  {
			    fprintf(f, "\\x'-%dp'", extraVS);
			    ln += 7;
			}
                        ln += FlushBars(f);
                    }
		    else leadingwhitespace++; /*RSKadd*/
                }

                if (! barPending && (i > formatnote) && (underlining || strikethrough || overbar)) {
                    /* Use backslash to start a new line; */
                    /* the bar-generating troff code can get pretty wide. */
                    fputs("\\\n\\kX", f);       /* Save start pos of item */
                    ln = 3;
                    barPending = 1;
                }

		i++;

		if (ln++ > 80 && (c == ' ' || c == '\t')) {
		    /* Don't let lines get too long */
		    addNewLine++;
		} else if (addNewLine) {
		    /* Add the newline before the first */
                    /* non-blank character (if still needed) */
		    if (ln > 80) {
			fputc('\\', f); /* troff is going to ignore the trailing whitespace if we don't quote this newline! */ /*RSKadd*/
			fputc('\n', f);
			ln = 0;
			needNewLine = 0;
		    }
		    addNewLine = 0;
		}
		if(symbola != 0){
		    /* handle special characters in the symbol fonts 
		     by inserting troff escape codes */
		    char *outst; 
		    outst = speclookup(c,symbola);
		    if(outst){
			if (ln > 80) {
			    fputc('\\', f);
			    fputc('\n', f);
			    ln = 0;
			}
			fputs(outst,f);
			ln += strlen(outst);
		    }
		   else {
		       /* code here for unknown symbol */
		   }
		}
		else {
		    if ((c == '\\') || (! needNewLine && (c == '\'' || c == '.'))) {
			/* quote special characters */
			fputc('\\', f);
			if (c == '.') {
			    fputc('&', f);
			    ln++;
			}
			ln++;
		    }
		    if (c == '\r') {
			fputs("\n.br\n", f);
			if (sv.CurIndentation != 0) {
			    if (sv.SpecialFlags & style_TabsCharacters) {
				fprintf (troffFile, "'ta %dn\n", tabscharspaces);
			    } else {
				tabs_OutputTroff(sv.tabs, 0, f);
				resetTabs = TRUE;
			    }
			}
		    }
		    else if((!isascii(c) || !isprint(c)) && c != '\t') {
			char *ccp= pcompch_CharacterToTroff(c,nenv,&sv);
#ifdef GROFF_ENV
			fputc(c,f);
			ln++;
#else
			if(ccp) {
			    while(*ccp) {
				fputc(*ccp,f);
				ccp++;
				ln++;
			    }
			} else {
			    if (ln > 80) {
				fputc('\n', f);
				ln = 0;
			    }
			    fprintf(f,"\\\\%3.3o",c);
			    ln += 3;
			}
#endif
		    }
		    else {
#ifdef BOGUS3812
			/* this code isn't quite right, but some 3812 printers can't handle hard spaces  */
			if(quotespace){			
			    if(c == ' ') {
				fputc('\\',f);
				fputc('&',f);
				ln++;
			    }
			    quotespace = FALSE;
			}
#else /*BOGUS3812 */
			/* put hard spaces at the beginning of lines so that initial spacings are consistent 
			 and no extra newlines are added */
			if(quotespace){
			    if(c == ' ') {
				fputc('\\',f);
				ln++;
			    }
			    else quotespace = FALSE;
			}
#endif /*BOGUS3812 */
			fputc(c, f);
		    }
		}
		needNewLine = 1;
	    }
            FlushBars(f);
	    
        } else      /* nenv->type ==  environment_View */
	    i = elen; 
	if(i == formatnote && endnotes == FALSE)
	    endspecialformating();
   }

    PutNewlineIfNeeded();

    /*
     * troff ueses envs itself
     * fputs(".ev\n", f);
     *
     */
    textLevel--;
#ifdef ENUMERATE
    if(toplevel){
	if(IndexStyle != NULL){
	    style_AddNewFontFace(IndexStyle,indexfontface);
	}
	if(enumerate && con){
	    content_Denumerate(con,0,doclen);
	    content_UpdateSource(con,0,doclen);
	}
	if(con) content_Destroy(con);
    }
    else {
	/* write troff to return to parents state */

        /*
         * nsv may not have been initialized.
         * (if we're not toplevel and there are no style changes?)
         * Until this is figured out, don't garbage collect. -dba
         */
#if 0
	tabs_Death(nsv.tabs);
#endif

	nsv = safesv;
	nsv.tabs = tabs_Copy(safesv.tabs);

	if(flags&texttroff_Revert) ChangeState();
	tabs_Death(sv.tabs);
	sv = safesv;

	/* Clever optimization:
	 We're done with safesv's tabs.  We could:
	sv.tabs = tabs_Copy(safesv.tabs);
	tabs_Death(safesv.tabs);
	 but since that just has the effect of an increment
	 and a decrement of the link count, we cleverly
	 do nothing -wdc */

    }
#endif  /* ENUMERATE */

    if(ttxt){	/* print the endnotes */
	struct textview *tv;
	tv = textview_New();
	textview_SetDataObject(tv,ttxt);
	textview_LinkTree(tv,view);
	PutNewlineIfNeeded();
	fputs(".bp\n", f);
	textview_Print(tv,f,"troff","PostScript",0);
	textview_UnlinkTree(tv);
	textview_Destroy(tv);
	text_Destroy(ttxt);
    }
#ifdef GROFF_ENV
    fputs(".if \\\\n(Tc=1 \\{\\\n",f);
    fputs(".OC\n",f);
    fputs(".OC\n",f);
    fputs(".\\}\n",f);
#endif /* GROFF_ENV */
}

void texttroff__WriteTroff(classID, view, dd, f, toplevel)
struct classheader *classID;
struct view *view;
struct dataobject *dd;
FILE * f;
int toplevel;
{
    texttroff_WriteSomeTroff(view,dd,f,toplevel,texttroff_Revert);
}

void texttroff__BeginDoc(classID, f)
struct classheader *classID;
FILE *f;
{
    textLevel++;
    OutputInitialTroff(f, TRUE, NULL);
    fputs(".br\n", f);
}

void texttroff__EndDoc(classID, f)
struct classheader *classID;
FILE *f;
{
    /*
     *fputs(".ev\n",f);
     *
     */
    textLevel--;
}

void texttroff__BeginPS(classID, f, width, height)
struct classheader *classID;
FILE *f;
long width, height;
{
#ifdef BOGOSITYWANTED
	/* I see no reason for this extra dot to print.  It looks bad. */
#ifndef GROFF_ENV
    fprintf(f, "\\&.\n");
#endif
#endif
    fprintf(f, "'PB %d %d\n", width, height);
    fprintf(f, "'if  \\n(zT  \\{\\\n");
    fprintf(f, "\\!    %d troffadjust %d neg translate\n", width, height);
}

void texttroff__EndPS(classID, f, width, height)
struct classheader *classID;
FILE *f;
long width, height;
{
    fprintf(f, "\\}\n");
    fprintf(f, "'PE %d %d\n", width, height);
}

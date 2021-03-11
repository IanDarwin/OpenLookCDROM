/* bdffont.c	font editor object */

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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/bdffont/RCS/bdffont.c,v 1.17 1993/08/25 20:42:40 susan Exp $";
#endif

/*
	Copyright Carnegie Mellon University 1991, 1992 - All rights reserved
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


#include <andrewos.h>
#include <ctype.h>

#include <bdffont.eh>
#include <tlex.ih>
#include <parse.ih> /* parse object */
#include <text.ih>

#include <ansitext.h>
#include <mathaux.h>


static struct bdffont_fontchar bdffont_ReadCharDefn;
static void EnsureDefns();

/* declarations for parser */

static int bdfparse_Property;
static long *bdfparse_PropertyValue;

#define bdfprop_INT_LEN (10)
#define bdfparse_Increment (128)
static char *bdfparse_Limit;
static char *bdfparse_Next;
static void bdfparse_EnsureStorage();

/* include parser tables and actions */

#include <bdfparse.tab.h>
#include <bdfparse.tab.c>	/* parse tables */
#include <parsedesc.h>	/* declare parse_description */
#undef parse_ROCK
#define parse_ROCK self
#undef parse_ROCKTYPE
#define parse_ROCKTYPE struct bdffont *
#include <parsepre.h>	/* begin function 'reduceActions' */
#include <bdfparse.act>	/* body of function 'reduceActions' */
#include <parsepost.h>	/* end of function 'reduceActions' */

/* declarations for lexor */

static struct parse *parser;

static int SaveTokenNumber(/* struct tlex *tlex */);
static unsigned char bdflex_ComposeByte(/* char c1, char c2 */);

#include <bdffont.tlc>


static char bdffont_FONT_VERSION[] = "2.1";
static char bdffont_FONT_NAME[] = "NewFont";
static char bdffont_COMMENT[] = "COMMENT Created with bdffont (Andrew User Interface System)\n";
static char bdffont_FOUNDRY[] = "AUIS";

static char bdfprop_FOUNDRY[] = "FOUNDRY";		/* string */
static char bdfprop_DEFAULTCHAR[] = "DEFAULT_CHAR";	/* non-neg int */
static char bdfprop_DEFAULTWIDTH[] = "DEFAULT_WIDTH";	/* positive int */
static char bdfprop_DEFAULTHEIGHT[] = "DEFAULT_HEIGHT";	/* positive int */
static char bdfprop_DEFAULTX[] = "DEFAULT_X";		/* non-neg int */
static char bdfprop_DEFAULTY[] = "DEFAULT_Y";		/* non-neg int */
static char bdfprop_DEFAULTDX[] = "DEFAULT_DX";		/* non-neg int */
static char bdfprop_DEFAULTDY[] = "DEFAULT_DY";		/* non-neg int */
static char bdfprop_ASCENT[] = "FONT_ASCENT";		/* positive int */
static char bdfprop_DESCENT[] = "FONT_DESCENT";		/* positive int */
static char bdfprop_FAMILY[] = "FAMILY";		/* string */
static char bdfprop_RESX[] = "RESOLUTION_X";		/* positive int */
static char bdfprop_RESY[] = "RESOLUTION_Y";		/* positive int */
static char bdfprop_WEIGHTNAME[] = "WEIGHT_NAME";	/* string */
static char bdfprop_WEIGHT[] = "WEIGHT";		/* non-neg int */


	static int
SaveTokenNumber(tlex)
	struct tlex *tlex;
{
	tlex_SetTokenValue(tlex, (void *)tlex_GetTokenNumber(tlex));
	tlex_SetTokenNumber(tlex, bdffont_global.token);
	return tlex_ACCEPT;
}

#if 0
	static void
CopyLineToTokenText(tlex)
	struct tlex *tlex;
{
	/* copy entire line to TokenText */
	long p = tlex_CurrPos(tlex);	/* pos after newline */
	tlex_BackUp(tlex, p - tlex_GetTokPos(tlex));
	tlex_StartToken(tlex);			/* include "COMMENT" */
	while (p-1 > tlex_CurrPos(tlex)) 	/* but not \n */
		tlex_Advance(tlex);		/* copy comment to TokenText */
	tlex_EndToken(tlex);
	tlex_NextChar(tlex);			/* skip newline */
}
#endif
	static unsigned char 
bdflex_ComposeByte(c1, c2)
    char c1, c2;
{
    unsigned char result;

    if (('0' <= c1) && (c1 <= '9')) {
	result = (c1 - '0') << 4;
    }
    else if (('a' <= c1) && (c1 <= 'f')) {
	result = (c1 - 'a' + 10) << 4;
    }
    else { /* (('A' <= c1) && (c1 <= 'F')) */
	result = (c1 - 'A' + 10) << 4;
    }

    if (('0' <= c2) && (c2 <= '9')) {
	return (result | (c2 - '0'));
    }
    else if (('a' <= c2) && (c2 <= 'f')) {
	return (result | (c2 - 'a' + 10));
    }
    else { /* (('A' <= c2) && (c2 <= 'F')) */
	return (result | (c2 - 'A' + 10));
    }
} /* bdflex_ComposeByte */


static void bdfparse_EnsureStorage(stg, length)
char **stg;
long length;
{
    long next;
    long newsize;

    if (bdfparse_Next + length > bdfparse_Limit) {
	next = bdfparse_Next - *stg;
	newsize = bdfparse_Limit - *stg;

	do {
	    newsize += bdfparse_Increment;
	} while (next + length > newsize);

	*stg = (char *) realloc(*stg, newsize);
	bdfparse_Next = *stg + next;
	bdfparse_Limit = *stg + newsize;
    }
} /* bdfparse_EnsureStorage */

static void EnsureDefns(self, num)
struct bdffont *self;
long num;
{
    int ix;
    long old;

    if (!self->defns) {
	self->defns_size = num+1;
	self->defns = (struct bdffont_fontchar *)malloc(sizeof(struct bdffont_fontchar) * self->defns_size);

	for (ix=0; ix<self->defns_size; ix++) {
	    self->defns[ix].encoding = ix;
	    self->defns[ix].bitmap = NULL;
	}
    }
    else {
	if (self->defns_size >= num+1)
	    return;
	old = self->defns_size;
	while (self->defns_size < num+1)
	    self->defns_size *= 2;

	self->defns = (struct bdffont_fontchar *)realloc(self->defns, sizeof(struct bdffont_fontchar) * self->defns_size);

	for (ix=old; ix<self->defns_size; ix++) {
	    self->defns[ix].encoding = ix;
	    self->defns[ix].bitmap = NULL;
	}
    }
}

boolean bdffont__InitializeObject(c, self)
struct classheader *c;
struct bdffont *self;
{
    int i;

    self->version = bdffont_FONT_VERSION;
    self->comments = bdffont_COMMENT;
    self->fontname = NULL;
    self->pointsize = 0;
    self->resx = 0;
    self->resy = 0;
    self->bbw = 0;
    self->bbh = 0;
    self->bbx = 0;
    self->bby = 0;
    self->fontfamily = NULL;
    self->fontweight = -1;	/* indicates default weight for given face */
    self->fontface = 0;		/* indicates roman, normal type face */
    self->ascent = 0;
    self->descent = 0;
    self->properties = NULL;
    self->proplength = 0;
    self->activedefns = 0;
    self->defaultw = 0;
    self->defaulth = 0;
    self->defaultx = 0;
    self->defaulty = 0;
    self->defaultdx = 0;
    self->defaultdy = 0;
    self->defaultchar = -1;

    self->defns_size = 0;
    self->defns = NULL;

    EnsureDefns(self, 255);

    return (TRUE);
} /* bdffont__InitializeObject */

void bdffont__FinalizeObject(c, self)
struct classheader *c;
struct bdffont *self;
{
    if (self->version && (self->version != bdffont_FONT_VERSION)) {
	free(self->version);
    }
    if (self->comments && (self->comments != bdffont_COMMENT)) {
	free(self->comments);
    }
    if (self->fontname) {
	free(self->fontname);
    }
    if (self->fontfamily) {
	free(self->fontfamily);
    }
    if (self->properties) {
	free(self->properties);
    }
} /* bdffont__FinalizeObject */

#define bdffont_DefaultWidth	(64)
#define bdffont_DefaultHeight	(64)
#define bdffont_DefaultOriginX	(0)
#define bdffont_DefaultOriginY	(0)

#define RoundUp(x) ((long) ((x) + 0.5))

/* pts in points, resx/y in dots per inch */
struct bdffont *bdffont__CreateNewFont(c, pts, resx, resy)
struct classheader *c;
long pts;
long resx;
long resy;
{
    struct bdffont *self;
    double fontsize;

    self = bdffont_New();

    self->comments = bdffont_COMMENT;
    self->pointsize = pts;
    self->resx = resx;
    self->resy = resy;

    fontsize = bdffont_ComputeFontSize(self);

    self->ascent = /*mathaux_*/RoundUp(ansitext_ComputeAscent(fontsize));
    self->descent = /*mathaux_*/RoundUp(ansitext_ComputeDescent(fontsize));
    bdffont_SetBoundingBox(self,
			    bdffont_DefaultWidth, bdffont_DefaultHeight,
			    bdffont_DefaultOriginX, bdffont_DefaultOriginY);

    self->defaultw = bdffont_DefaultWidth;
    self->defaulth = bdffont_DefaultHeight;
    self->defaultx = bdffont_DefaultOriginX;
    self->defaulty = bdffont_DefaultOriginY;
    self->defaultdx = bdffont_DefaultWidth;
    self->defaultdy = 0;
    self->defaultchar = 32;

    return (self);
} /* bdffont__CreateNewFont */

static void parseerror(p, s, m)
struct parse *p;
int s;
char *m;
{
    /* don't print the message */
}

long bdffont__Read(self, file, id)
struct bdffont *self;
FILE *file;
long id;
{
    int severity;
    long read_status;
    struct text *text;

    text = text_New();
    text_Read(text, file, 0);

    self->lex = tlex_Create(&bdffont_tlex_tables, NULL, 
		text, 0, text_GetLength(text));

    parser = parse_Create(
			   &parse_description, 	/* declared in parsedesc.h */
			   self->lex,	 	/* created just above */
			   reduceActions, 	/* function defined during includes */
			   self, 		/* rock for reduceActions */
			   parseerror);		/* function called for errors */

    self->activedefns = 0;
    self->defaultchar = -1;
    self->lastcharloaded = (-1);

    severity = parse_Run(parser);	/* PARSE IT */

    if (severity != parse_OK) {
	if (tlex_CurrChar(self->lex) == EOF) 
	    read_status = dataobject_PREMATUREEOF;
	else 
	    read_status = dataobject_BADFORMAT;
    }
    else {
	if (self->defaultchar == -1) 
	    self->defaultchar = (long)bdffont_GetDefaultChar(self);
	read_status = dataobject_NOREADERROR;
    }
    
    if (read_status != dataobject_NOREADERROR)
	fprintf(stderr, "bdffont: an error occurred while reading the font file.\n");
    tlex_Destroy(self->lex);
    parse_Destroy(parser);

    return (read_status);
} /* bdffont__Read */

static void WriteCharacter(file, defn)
FILE *file;
struct bdffont_fontchar *defn;
{
    unsigned char *bm;
    long row, col, rowbytes, pad;

    if (defn->bitmap) {
	if (defn->name) {
	    fprintf(file, "STARTCHAR %s\n", defn->name);
	}
	else {
	    fprintf(file, "STARTCHAR c%o\n", (unsigned) defn->encoding);
	}

	fprintf(file, "ENCODING %d\n", (unsigned) defn->encoding);
	fprintf(file, "SWIDTH %d %d\n", defn->swx, defn->swy);
	fprintf(file, "DWIDTH %d %d\n", defn->dwx, defn->dwy);
	fprintf(file, "BBX %d %d %d %d\n",
		defn->bbw, defn->bbh, defn->bbx, defn->bby);

	if (defn->attributes) {
	    fprintf(file, "ATTRIBUTES %X\n", defn->attributes);
	}

	bm = defn->bitmap;
	rowbytes = bdffont_WidthInBytes(defn->bbw);
	pad = bdffont_AlignedWidthInBytes(defn) - rowbytes;

	fprintf(file, "BITMAP\n");
	for (row = 0; row < defn->bbh; row++) {
	    for (col = 0; col < rowbytes; col++) {
		fprintf(file, "%02X", *bm++);
	    }
	    bm += pad;
	    fprintf(file, "\n");
	}
	fprintf(file, "ENDCHAR\n");
    }
} /* WriteCharacter */

int bdffont__GetDefaultChar(self)
struct bdffont *self;
{
    int encoding;

    if (self->defaultchar == -1) {
	encoding = (int) ' ';
	while ((encoding < self->defns_size) &&
	       ! bdffont_IsActive(&self->defns[encoding]))
	{
	    encoding++;
	}

	if (encoding == self->defns_size) {
	    encoding = 0;
	    while ((encoding < (int) ' ') &&
		   ! bdffont_IsActive(&self->defns[encoding]))
	    {
		encoding++;
	    }
	}

	return (encoding);
    }

    return ((int) self->defaultchar);
} /* bdffont_GetDefaultChar */

#define bdffont_IsPrefix(main, prefix) \
	((strncmp(main, prefix, sizeof(prefix) - 1) == 0) && \
	isspace(main[sizeof(prefix)]))

#define bdffont_FillInValue(out, v) \
	sprintf((out) - (bdfprop_INT_LEN + 1) /* counts line-feed */, \
		"%*d", bdfprop_INT_LEN, (v));

static long bdffont_AppendProperty(props, length, p, psize, v)
char **props;
long length;
char *p;
long psize;
long v;
{
    long newlength = length + psize + bdfprop_INT_LEN + 1;
    /* includes property label, space, value, and line-feed */

    *props = (char *) realloc(*props, newlength + 1); /* include NUL */
    sprintf(*props + length, "%s %*d\n", p, bdfprop_INT_LEN, v);

    return (newlength);
} /* bdffont_AppendProperty */

static long FilterPropertiesOut(self, props)
struct bdffont *self;
char **props;
{
    static char *FaceNames[] = {
	"Roman",
	"Bold",
	"Italic",
	"BoldItalic",
	"Fixed",
	"FixedBold",
	"FixedItalic",
	"FixedBoldItalic",
	"Shadowed",
	"ShadowedBold",
	"ShadowedItalic",
	"ShadowedBoldItalic",
	"ShadowedFixed",
	"ShadowedFixedBold",
	"ShadowedFixedItalic",
	"ShadowedFixedBoldItalic" 
    };
    long count = 0;
    boolean needASCENT = TRUE;
    boolean needDESCENT = TRUE;
    boolean needDEFAULTCHAR = TRUE;
    boolean needDEFAULTW = TRUE;
    boolean needDEFAULTH = TRUE;
    boolean needDEFAULTX = TRUE;
    boolean needDEFAULTY = TRUE;
    boolean needDEFAULTDX = TRUE;
    boolean needDEFAULTDY = TRUE;
    boolean needFOUNDRY = TRUE;
    boolean needFAMILY = TRUE;
    boolean needWEIGHTNAME = TRUE;
    boolean needWEIGHT = TRUE;
    boolean needRESX = TRUE;
    boolean needRESY = TRUE;
    char *inattr = self->properties;
    char *next;
    char *outattr;
    long length;

    outattr =
	*props = (char *) malloc(self->proplength ? self->proplength + 1 : 72);

    if (inattr) while (*inattr) {
	count++;

	next = inattr;
	while ((*outattr++ = *next++) != '\n') {
	    /* continue; */
	}

	if (bdffont_IsPrefix(inattr, bdfprop_FOUNDRY)) {
	    needFOUNDRY = FALSE;
	}
	else if (bdffont_IsPrefix(inattr, bdfprop_ASCENT)) {
	    bdffont_FillInValue(outattr, self->ascent);
	    needASCENT = FALSE;
	}
	else if (bdffont_IsPrefix(inattr, bdfprop_DESCENT)) {
	    bdffont_FillInValue(outattr, self->descent);
	    needDESCENT = FALSE;
	}
	else if (bdffont_IsPrefix(inattr, bdfprop_DEFAULTCHAR)) {
	    bdffont_FillInValue(outattr, self->defaultchar);
	    needDEFAULTCHAR = FALSE;
	}
	else if (bdffont_IsPrefix(inattr, bdfprop_DEFAULTWIDTH)) {
	    bdffont_FillInValue(outattr, self->defaultw);
	    needDEFAULTW = FALSE;
	}
	else if (bdffont_IsPrefix(inattr, bdfprop_DEFAULTHEIGHT)) {
	    bdffont_FillInValue(outattr, self->defaulth);
	    needDEFAULTH = FALSE;
	}
	else if (bdffont_IsPrefix(inattr, bdfprop_DEFAULTX)) {
	    bdffont_FillInValue(outattr, self->defaultx);
	    needDEFAULTX = FALSE;
	}
	else if (bdffont_IsPrefix(inattr, bdfprop_DEFAULTY)) {
	    bdffont_FillInValue(outattr, self->defaulty);
	    needDEFAULTY = FALSE;
	}
	else if (bdffont_IsPrefix(inattr, bdfprop_DEFAULTDX)) {
	    bdffont_FillInValue(outattr, self->defaultdx);
	    needDEFAULTDX = FALSE;
	}
	else if (bdffont_IsPrefix(inattr, bdfprop_DEFAULTDY)) {
	    bdffont_FillInValue(outattr, self->defaultdy);
	    needDEFAULTDY = FALSE;
	}
	else if (bdffont_IsPrefix(inattr, bdfprop_FAMILY)) {
	    /* should never happen, input removed this from properties! */
	}
	else if (bdffont_IsPrefix(inattr, bdfprop_WEIGHTNAME)) {
	    /* should never happen, input removed this from properties! */
	}
	else if (bdffont_IsPrefix(inattr, bdfprop_WEIGHT)) {
	    bdffont_FillInValue(outattr, self->fontweight);
	    needWEIGHT = FALSE;
	}
	else if (bdffont_IsPrefix(inattr, bdfprop_RESX)) {
	    bdffont_FillInValue(outattr, self->resx);
	    needRESX = FALSE;
	}
	else if (bdffont_IsPrefix(inattr, bdfprop_RESY)) {
	    bdffont_FillInValue(outattr, self->resy);
	    needRESY = FALSE;
	}

	*(outattr - 1) = '\n';
	inattr = next;
    }
    *outattr = '\0';

    length = inattr - self->properties;	/* should be self->proplength!! */

    if (needFOUNDRY && (self->version == bdffont_FONT_VERSION)) {
	count++;
	*props = (char *)
		   realloc(*props,
			   length
			      + sizeof(bdfprop_FOUNDRY) /* counts space */
			      + sizeof(bdffont_FOUNDRY) /* counts line-feed */
			      + 3);		        /* quotes and NUL */
	sprintf(*props + length,
		"%s \"%s\"\n",
		bdfprop_FOUNDRY,
		bdffont_FOUNDRY);
	length += sizeof(bdfprop_FOUNDRY) + sizeof(bdffont_FOUNDRY) + 2;
    }

    if (needASCENT) {
	count++;
	length = bdffont_AppendProperty(props,
					length,
					bdfprop_ASCENT,
					sizeof(bdfprop_ASCENT),
					self->ascent);
    }

    if (needDESCENT) {
	count++;
	length = bdffont_AppendProperty(props,
					length,
					bdfprop_DESCENT,
					sizeof(bdfprop_DESCENT),
					self->descent);
    }

    if (needDEFAULTCHAR) {
	count++;
	length = bdffont_AppendProperty(props,
					length,
					bdfprop_DEFAULTCHAR,
					sizeof(bdfprop_DEFAULTCHAR),
					(long) bdffont_GetDefaultChar(self));
    }

    if (needDEFAULTW) {
	count++;
	length = bdffont_AppendProperty(props,
					length,
					bdfprop_DEFAULTWIDTH,
					sizeof(bdfprop_DEFAULTWIDTH),
					self->defaultw);
    }

    if (needDEFAULTH) {
	count++;
	length = bdffont_AppendProperty(props,
					length,
					bdfprop_DEFAULTHEIGHT,
					sizeof(bdfprop_DEFAULTHEIGHT),
					self->defaulth);
    }

    if (needDEFAULTX) {
	count++;
	length = bdffont_AppendProperty(props,
					length,
					bdfprop_DEFAULTX,
					sizeof(bdfprop_DEFAULTX),
					self->defaultx);
    }

    if (needDEFAULTY) {
	count++;
	length = bdffont_AppendProperty(props,
					length,
					bdfprop_DEFAULTY,
					sizeof(bdfprop_DEFAULTY),
					self->defaulty);
    }

    if (needDEFAULTDX) {
	count++;
	length = bdffont_AppendProperty(props,
					length,
					bdfprop_DEFAULTDX,
					sizeof(bdfprop_DEFAULTDX),
					self->defaultdx);
    }

    if (needDEFAULTDY) {
	count++;
	length = bdffont_AppendProperty(props,
					length,
					bdfprop_DEFAULTDY,
					sizeof(bdfprop_DEFAULTDY),
					self->defaultdy);
    }

    if (needFAMILY && self->fontfamily) {
	count++;
	*props = (char *)
		    realloc(*props,
			    length
				+ sizeof(bdfprop_FAMILY)    /* counts space */
				+ 2 * strlen(self->fontfamily) /* expansion */
				+ 4);		/* space, 2 quotes, and NUL */
	sprintf(*props + length,
		"%s \"",
		bdfprop_FAMILY);
	length += sizeof(bdfprop_FAMILY) + 1;	/* point to NUL */

	inattr = self->fontfamily;	/* as a temporary */
	while (*inattr) {
	    if (*inattr == '"') {
		(*props)[length++] = '"';
	    }
	    (*props)[length++] = *inattr++;
	}
	(*props)[length++] = '"';
	(*props)[length++] = '\n';
	(*props)[length] = '\0';
    }

    if (needWEIGHTNAME) {
	char *facename;

	count++;
	facename = (self->fontface < sizeof(FaceNames)
			? FaceNames[self->fontface]
			: FaceNames[0]);
	*props = (char *)
		    realloc(*props,
		 	    length
				+ sizeof(bdfprop_WEIGHTNAME) /* counts space */
				+ strlen(facename)	    /* just the name */
				+ 4);		/* 2 quotes, line-feed & NUL */
	sprintf(*props + length,
		"%s \"%s\"\n",
		bdfprop_WEIGHTNAME,
		facename);
	length += sizeof(bdfprop_WEIGHTNAME) + strlen(facename) + 3;
    }

    if (needWEIGHT && (0 <= self->fontweight)) {
	count++;
	*props = (char *) realloc(*props,
		 	 length
			    + sizeof(bdfprop_WEIGHT)	/* counts space */
			    + bdfprop_INT_LEN		/* just the number */
			    + 2);			/* line-feed and NUL */
	sprintf(*props + length,
		"%s %*d\n",
		bdfprop_WEIGHT,
		bdfprop_INT_LEN,
		self->fontweight);
	length += sizeof(bdfprop_WEIGHT) + bdfprop_INT_LEN + 1;
    }

    if (needRESX) {
	count++;
	length = bdffont_AppendProperty(props,
					length,
					bdfprop_RESX,
					sizeof(bdfprop_RESX),
					self->resx);
    }

    if (needRESY) {
	count++;
	length = bdffont_AppendProperty(props,
					length,
					bdfprop_RESY,
					sizeof(bdfprop_RESY),
					self->resy);
    }

    return (count);
} /* FilterPropertiesOut */

long bdffont__Write(self, file, id, level)
struct bdffont *self;
FILE *file;
long id;
int level;
{
    long i;
    char *props;

    bdffont_NotifyObservers(self, bdffont_Writing);

    fprintf(file, "STARTFONT %s\n", self->version);

    if (self->comments) {
	fprintf(file, "%s", self->comments);
    }

    fprintf(file, "FONT %s\n",
	     self->fontname ? self->fontname : bdffont_FONT_NAME);

    fprintf(file, "SIZE %d %d %d\n", self->pointsize, self->resx, self->resy);

    fprintf(file, "FONTBOUNDINGBOX %d %d %d %d\n",
	     self->bbw, self->bbh, self->bbx, self->bby);

    fprintf(file, "STARTPROPERTIES %d\n", FilterPropertiesOut(self, &props));
    fprintf(file, "%s", props);
    free(props);
    fprintf(file, "ENDPROPERTIES\n");

    if (self->activedefns > 0) {
	fprintf(file, "CHARS %d\n", self->activedefns);

	for (i = 0; i < self->defns_size; i++) {
	    WriteCharacter(file, &self->defns[i]);
	}
    }

    fprintf(file, "ENDFONT\n");

    return (bdffont_GetID(self));
} /* bdffont__Write */

void bdffont__SetFontName(self, fn)
struct bdffont *self;
char *fn;
{
    if (self->fontname) {
	free(self->fontname);
	self->fontname = NULL;
    }

    if (fn) {
	self->fontname = (char *) malloc(strlen(fn) + 1);
	strcpy(self->fontname, fn);
    }
} /* bdffont__SetFontName */

void bdffont__SetFontFamily(self, fn)
struct bdffont *self;
char *fn;
{
    if (self->fontfamily) {
	free(self->fontfamily);
	self->fontfamily = NULL;
    }

    if (fn) {
	self->fontfamily = (char *) malloc(strlen(fn) + 1);
	strcpy(self->fontfamily, fn);
    }
} /* bdffont__SetFontFamily */

void bdffont__GetBoundingBox(self, w, h, x, y)
struct bdffont *self;
long *w, *h, *x, *y;
{
    *w = self->bbw;
    *h = self->bbh;
    *x = self->bbx;
    *y = self->bby;
} /* bdffont__GetBoundingBox */

void bdffont__SetBoundingBox(self, w, h, x, y)
struct bdffont *self;
long w, h, x, y;
{
    self->bbw = w;
    self->bbh = h;
    self->bbx = x;
    self->bby = y;
} /* bdffont__SetBoundingBox */

void bdffont__GetResolution(self, rx, ry)
struct bdffont *self;
long *rx, *ry;
{
    *rx = self->resx;
    *ry = self->resy;
} /* bdffont__GetResolution */

void bdffont__SetResolution(self, rx, ry)
struct bdffont *self;
long rx, ry;
{
    self->resx = rx;
    self->resy = ry;
} /* bdffont__SetResolution */

void bdffont__GetDefaultExtent(self, w, h)
struct bdffont *self;
long *w, *h;
{
    *w = self->defaultw;
    *h = self->defaulth;
} /* bdffont__GetDefaultExtent */

void bdffont__SetDefaultExtent(self, w, h)
struct bdffont *self;
long w, h;
{
    self->defaultw = w;
    self->defaulth = h;
} /* bdffont__SetDefaultExtent */

void bdffont__GetDefaultOrigin(self, x, y)
struct bdffont *self;
long *x, *y;
{
    *x = self->defaultx;
    *y = self->defaulty;
} /* bdffont__GetDefaultOrigin */

void bdffont__SetDefaultOrigin(self, x, y)
struct bdffont *self;
long x, y;
{
    self->defaultx = x;
    self->defaulty = y;
} /* bdffont__SetDefaultOrigin */

void bdffont__GetDefaultDelta(self, dx, dy)
struct bdffont *self;
long *dx, *dy;
{
    *dx = self->defaultdx;
    *dy = self->defaultdy;
} /* bdffont__GetDefaultDelta */

void bdffont__SetDefaultDelta(self, dx, dy)
struct bdffont *self;
long dx, dy;
{
    self->defaultdx = dx;
    self->defaultdy = dy;
} /* bdffont__SetDefaultDelta */

void bdffont__SetCharDWidth(self, which, x, y)
struct bdffont *self;
int which;
long x, y;
{
    struct bdffont_fontchar *defn;

    defn = bdffont_GetDefinition(self, which);

    bdffont_SetDWidth(defn, x, y);
    bdffont_SetSWidth(defn,
		      (long) ((x * 72000.0) / (self->resx * self->pointsize)),
		      (long) ((y * 72000.0) / (self->resy * self->pointsize)));
} /* bdffont__SetCharDWidth */

static void RecomputeFontExtent(self)
struct bdffont *self;
{
    struct bdffont_fontchar *defn, *limit;
    long w, h;

    defn = &self->defns[0];
    limit = &self->defns[self->defns_size];

    self->bbw = mathaux_MININT;
    self->bbh = mathaux_MININT;
    while (defn < limit) {
	if (bdffont_IsActive(defn)) {
	    bdffont_GetExtent(defn, &w, &h);

	    if (self->bbw < w) {
		self->bbw = w;
	    }

	    if (self->bbh < h) {
		self->bbh = h;
	    }
	}

	defn++;
    }
} /* RecomputeFontExtent */

static void RecomputeFontOrigin(self)
struct bdffont *self;
{
    struct bdffont_fontchar *defn, *limit;
    long x, y;

    defn = &self->defns[0];
    limit = &self->defns[self->defns_size];

    self->bbx = mathaux_MAXINT;
    self->bby = mathaux_MAXINT;
    while (defn < limit) {
	if (bdffont_IsActive(defn)) {
	    bdffont_GetOrigin(defn, &x, &y);

	    if (x < self->bbx) {
		self->bbx = x;
	    }

	    if (y < self->bby) {
		self->bby = y;
	    }
	}

	defn++;
    }
} /* RecomputeFontOrigin */

void bdffont__SetCharExtent(self, which, w, h)
struct bdffont *self;
int which;
long w, h;
{
    struct bdffont_fontchar *defn;
    long oldw, oldh;
    boolean recompute = FALSE;

    defn = bdffont_GetDefinition(self, which);

    bdffont_GetExtent(defn, &oldw, &oldh);

    if (self->bbw <= w) {
	self->bbw = w;
    }
    else if (oldw == self->bbw) {
	recompute = TRUE;
    }

    if (self->bbh <= h) {
	self->bbh = h;
    }
    else if (oldh == self->bbh) {
	recompute = TRUE;
    }

    if (recompute) {
	RecomputeFontExtent(self);
    }

    bdffont_SetExtent(defn, w, h);
} /* bdffont__SetCharExtent */

void bdffont__SetCharOrigin(self, which, x, y)
struct bdffont *self;
int which;
long x, y;
{
    struct bdffont_fontchar *defn;
    long oldx, oldy;
    boolean recompute = FALSE;

    defn = bdffont_GetDefinition(self, which);

    bdffont_GetOrigin(defn, &oldx, &oldy);

    if (x <= self->bbx) {
	self->bbx = x;
    }
    else if (oldx == self->bbx) {
	recompute = TRUE;
    }

    if (y <= self->bby) {
	self->bby = y;
    }
    else if (oldy == self->bby) {
	recompute = TRUE;
    }

    if (recompute) {
	RecomputeFontOrigin(self);
    }

    bdffont_SetOrigin(defn, x, y);
} /* bdffont__SetCharOrigin */

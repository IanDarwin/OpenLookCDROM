/* ********************************************************************** *\
 *         Copyright IBM Corporation 1986,1991 - All Rights Reserved      *
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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/toez/RCS/readscrb.c,v 1.13 1993/05/04 01:37:02 susan Exp $";
#endif

 


/* ReadScribe.c - 
	 convert an ASCII file (with Scribe commands) to ATK text
*/

/* 
	$Log: readscrb.c,v $
 * Revision 1.13  1993/05/04  01:37:02  susan
 * RCS Tree Split
 *
 * Revision 1.12.1.1  1993/02/02  04:36:43  rr2b
 * new R6tape branch
 *
 * Revision 1.12  1992/12/15  21:45:38  rr2b
 * more disclaimerization fixing
 *
 * Revision 1.11  1992/12/14  21:00:02  rr2b
 * disclaimerization
 *
 * Revision 1.10  1991/09/12  16:37:57  bobg
 * Update copyright notice and rcsid
 *
 * Revision 1.9  1991/01/16  15:22:32  wjh
 * fixed bug which inserted newlines in the middle of paragraphs if a previous line of the same length ended with a style that has the KeepNextNL flag
 *
 * Revision 1.8  90/06/22  11:38:22  wjh
 * modified to ignore \r and ^Z, both of which are useless DOS-ian artifacts
 * 
 * Revision 1.7  89/10/16  12:16:19  cfe
 * Clean up after compiler warnings.
 * 
 * Revision 1.6  89/10/05  11:23:15  cfe
 * typos
 * 
 * Revision 1.5  89/09/11  11:02:49  wjh
 * oops
 * 
 * Revision 1.4  89/09/11  10:52:38  wjh
 * locks
 * remove the pseudo-file tricks to avoid type mismatch error on Sun
 * replace remaining references to appendvalue with appropriate references to appendtobuf
 * reformat appendtobuf
 * 
 * Revision 1.3  89/07/28  17:02:45  tpn
 * added support for inserting new textref and texttag objects.
 * 
 * Revision 1.2  89/07/12  11:02:05  wjh
 * changed so that more than 2 embedded blanks cause newlines only if NOT after a sentence end
 * fixed three compiler warnings on vaxen by making InPtr be 'char' instead of 'unsigned char'
 * 
 * Revision 1.1  89/07/11  17:26:03  wjh
 * Initial revision
 * 
 * Revision 1.2  89/07/05  10:20:49  wjh
 * fixed compile errors and warnings
 * 
 * Revision 1.1  89/07/01  15:46:08  wjh
 * Initial revision
 * 
WJHansen,  26 May, 1989
	converted from base editor 1 
*/

/* 
@bigger[   
= * = * = * = * = * = * = * = * = * = * = * = * =
*                                                         *
=	NOTE:  ReadScribe.c DEPENDS ON    =
*	scribe.tpl and default.tpl.                      *
=	STYLE NAMES WITHIN THEM MUST HAVE THE      =
*	SAME NAMES AS EACH OTHER AND AS THE          *
=	THE STANDARD Scribe ENVIRONMENTS.               =
*            FormatNote is checked for and used to set passthru	*
=     	Exceptions: italic, bold, and underline are mapped to i,b,u    =
  		defered: treat other passthru styles sames as format note

    the following is a defered feature
=	WORSE NEWS:  THE UPPER MENU NAMES             =
*	"REGION", "HEADING", AND "JUSTIFY" ARE             *
=	CHECKED FOR HERE.  (ONLY THESE STYLES CAUSE     =
*	BREAK BEFORE AND AFTER                            *
=	IF THEY ARE ADJACENT TO NEWLINE.)                =
*             
*                                                        *
= * = * = * = * = * = * = * = * = * = * = * = * =
]

-Backslash-newline is converted to a space.
-Lines that begin with white space are preceded by a newline.
-Lines that contain tab or triple space after a non-white character
     are preceded and followed by newlines.
-An empty line is given a real newline before and after.
-@* is converted to a newline.

Sentence enders are : ; ? . !  If they occur at end of line, they 
will be followed by at least two blanks.  (Same if followed by quote 
or right parenthesis.)

defered: A start style op for Region, Justify, or Title at beginning of line
     will usually cause a newline (unless bold or italic).

defered: An end of a style for Region, Title, or Justify 
    will be followed by a newline


In general, @word is examined for three cases:
	1) word is a standard stylesheet name in default.tpl or scribe.tpl
		An appropriate style is given the enclosed text
	2) word is one of the reserved Scribe command bytes
		begin  end  i  b  u index indexentry indexsecondary newpage
		each is treated appropriately
	3) otherwise
		The entire scribe construct is output with passthru

for @special-char, only @@ and @* are processed.  others are entered
with passthru set. 

*/

/* Problems:
     @define is not processed.
     Indented text is not given an indent look.
     Probably ought to handle @style and @modify.
     should handle @. @+ @- @:
     . - &  should be accepted in style names, but they are also single char 
		@ operands
     Ought to have different environments based on @device.  E.g.
	dover.tpl, apa6670.tpl, ...
     We could have a template as argument to abe.

*/

#include <ctype.h>
#include <stdio.h>
#include <text.ih>
#include <stylesht.ih>
#include <style.ih>
#include <fontdesc.ih>



/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static struct text *ScribeDoc = NULL;	/* has Scribe.tpl */
static struct stylesheet *ScribeSheet;	/* stylesheet for ScribeDoc */
static void InitCharType (), SaveWhiteSpace(), DoText(),appendtobuf();

struct stylemap {
	struct style *s;
	long pos, len;
	struct stylemap *next;
} *StyleMap;


/* output macros and variables */

static struct text *Doc;		/* where to put result */
static struct stylesheet *DocSheet;	/* stylesheet for Doc */

#define BUFSIZE 4096
static unsigned char buf[BUFSIZE];
static unsigned char *outp;
static long DocPos;  /* where the next character will go */
static long WhereTo;  /* where to put buf into doc */

#define pout(c) (*outp++ = c, DocPos++)
#define putstring(s,len)   {strcpy(outp, s); outp += len; DocPos += len;}

static void (*error)();
static unsigned char ErrBuf[100];
static boolean HadError;
static int LineNo=1;   /* current line */ 

static boolean HaveNonWhiteSpace = FALSE;		/* TRUE after 1st non-white */
static int EmbeddedWhiteSpace = 0;		/* count number of embedded tabs or triple spaces */
static boolean StartsWhite = FALSE;				/* line starts with space or tab */



/* input macros and variables */

static FILE *fIN;		/* read and write the text */
static int nextch;     /* set prior to entry to a parsing routine and 
			holds next character after exit */
#define getnextch()  (nextch = (getc(fIN)))


static boolean AfterNewline = FALSE;  /* TRUE if buf[0]
		is a blank substituted for a newline.  */

static boolean LineStartCommand = FALSE,
		LineEndCommand = FALSE;


/* stack recording nested style invocations */

static int endsp;
		/* the ending char for current envt is in scribeEnd[endsp]
		   the current envt is nofill iff NoFill[endsp] */
static unsigned char scribeEnd[100];  
		  /* ) } ] > ' "   'e' for @end;  'p' is passStyle*/
static boolean NoFill[100];
static struct stylemap *currMap[100];   /* what stylemap for this range */
static unsigned char endMode[100];	/* which form of end to use:
			'-'  just unstack and save end loc
			')'  write scribeEnd, then unstack and save loc
			'@' write '@end[word]', then unstack and save loc  */

static unsigned char *LatestEndLoc = NULL;
static struct stylemap *LatestEndMap = NULL;


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

	boolean
ReadScribeFromFileToDoc(f, doc, pos, errhandler)  
	FILE *f; 
	struct text *doc; 
	int pos; 
	void (*errhandler)();
{
	error = errhandler;
	LineNo = 1;
	HadError = FALSE;
	outp = &(buf[0]);
	Doc = doc;
	DocPos = pos;
	WhereTo = pos;
	InitCharType();
	InitStyles(doc);

	fIN = f;
	getnextch();
	DoText();	/*      <===  parse input and make output    */

   /*  	CleanUpStyles();  */

	return HadError;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

	static void
Error()
{
	error(LineNo, ErrBuf);
	HadError = TRUE;
}
	static void
Error0(msg) 
	char *msg; 
{	sprintf (ErrBuf, msg);   Error();   }
	static void
Error1(msg, a) 
	char *msg, *a; 
{	sprintf (ErrBuf, msg, a);   Error();   }
Error2(msg, a, b) 
	char *msg, *a, *b; 
{	sprintf (ErrBuf, msg, a, b);   Error();   }
Error3(msg, a, b, c) 
	char *msg, *a, *b, *c; 
{	sprintf (ErrBuf, msg, a, b, c);   Error();   }
Error4(msg, a, b, c, d) 
	char *msg, *a, *b, *c, *d; 
{	sprintf (ErrBuf, msg, a, b, c, d);   Error();   }
Error5(msg, a, b, c, d, e) 
	char *msg, *a, *b, *c, *d, *e; 
{	sprintf (ErrBuf, msg, a, b, c, d, e);   Error();   }

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/* reserved words are  (* means special processing)

 *b, *begin, bibliography, blankpage, blankspace, caption, case, cite,
 citemark, comment, counter, *define, device, *end, equate, font, 
 foot, form, hinge, hsp, *i, include, *index, *indexentry, *indexsecondary, 
 itag, label, libraryfile, make, message, modify, *newpage, ovp, pagefooting,
 pageheading, pageref, part, picture, presspicture, ref, send, set,
 specialfont, string, style, tabclear, tabdivide, tabset, tag,
 textform, title, *u, use, value
*/

static int ResInx[26] = {
/*a*/		0,1,7,14,17,
/*f*/		20,0,24,27,0,
/*k*/		0,34,37,41,43,
/*p*/		45,0,52,54,60,
/*u*/		67,70,0,0,0,0
};

static char *ResWord[] = {0, 
/*  1 */ "b", "begin", "bibliography", "blankpage", "blankspace", 0,
/*  7 */ "caption", "case", "cite", "citemark", "comment", "counter", 0,
/* 14 */ "define", "device", 0,
/* 17 */ "end", "equate", 0,
/* 20 */ "font", "foot", "form", 0,
/* 24 */ "hinge", "hsp", 0,
/* 27 */ "i", "include", "index", "indexentry", "indexsecondary", "itag", 0,
/* 34 */ "label", "libraryfile", 0,
/* 37 */ "make", "message", "modify", 0,
/* 41 */ "newpage", 0,
/* 43 */ "ovp", 0,
/* 45 */ "pagefooting", "pageheading", "pageref", "part", "picture", 
				"presspicture", 0,
/* 52 */ "ref", 0,
/* 54 */ "send", "set", "specialfont", "string", "style", 0,
/* 60 */ "tabclear", "tabdivide", "tabset", "tag", "textform", "title", 0,
/* 67 */ "u", "use", 0,
/* 70 */ "value", 0
};
#define bNUMBER		 1
#define beginNUMBER		2
#define defineNUMBER 14
#define endNUMBER		17
#define iNUMBER		27
#define indexNUMBER	29
#define ientryNUMBER	30
#define isecNUMBER	31
#define	labelNUMBER	34
#define pageNUMBER	41
#define	pagerefNUMBER	47
#define	refNUMBER	52
#define uNUMBER	67

static unsigned char CharType[256];

	static void
InitCharType ()
{
	int i;
	static char upper[] = "ABCDEFGHJIKLMNOPQRSTUVWXYZ";
	static char lower[] = "abcdefghjiklnmoqprstuvwxyz";
	static char other[] = "0123456789%#";
		/* '.' '-' '&' ought to be in other, but they are valid as @. @- @& */
	for (i=256; i; )				 CharType[--i] = 0;
	for (i=strlen(lower); i; )		 CharType[lower[--i]] = 1;
	for (i=strlen(upper); i; )		 CharType[upper[--i]] = 1;
	for (i=strlen(other); i; )		 CharType[other[--i]] = 1;
}

	static unsigned char *
CollectWord(len) 
	int *len; 
{
	static unsigned char word[102];
	register unsigned char c, *wx = word;
	register cnt = 100;
	SaveWhiteSpace();
	c = nextch;
	while (cnt-- && CharType[c]) {
		*wx++ = (isupper(c)) ? tolower(c) : c;
		c = getnextch();
	}
	*wx = '\0';
	*len = wx-word;
	return(word);
}

	static int
IsReserved (word) 
	register unsigned char *word; 
{
	register inx;

	if (*word >='a' && *word <= 'z') {
		inx = ResInx[*word - 'a'] - 1;
		while (ResWord[++inx]) 
			if (strcmp(word, ResWord[inx]) == 0) 
				return inx;
	}
	return 0;
}

/* ScribeDelimiter(tc)
	scan for a scribe delimiter:  {  [  (  <  "  `  '
	return the corresponding right delimiter
	set *tc to the left delimiter
	if the first non-blank character is not a Scribe delimiter,
		it is returned in *tc and remains in nextch
	blank characters skipped over are saved
		and can be output with OutputWhiteSpace()
*/
	static unsigned char
ScribeDelimiter(tc)
	unsigned char *tc;
{
	static char
		left[]  = "{[(<\"`'",	/* at end, null is paired to blank*/
		right[] = "}])>\"'' ";
	register endc;
	register char *pc = left;
	SaveWhiteSpace();
	*tc = nextch;
	while (*pc && nextch != *pc) pc++;
	endc = right[pc-left];
	if (endc != ' ') getnextch();
	return(endc);
}

static unsigned char SavedWhite[200];
static int WhiteLen;

	static void
SaveWhiteSpace() 
{
	WhiteLen = 0;
	while (WhiteLen<199 && (nextch==' ' || nextch=='\t')) {
		SavedWhite[WhiteLen++] = nextch;
		getnextch();
	}
}

/* OutputWhiteSpace() 
	dumps out white space saved by SaveWhiteSpace
	returns TRUE if there was any 
*/
	static boolean 
OutputWhiteSpace()
{
	if (WhiteLen) putstring (SavedWhite, WhiteLen);
	return (WhiteLen!=0);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

struct style *iStyle, *bStyle, *uStyle, *inxiStyle, *passStyle;

	static struct style *
createStyle(name, mnm)
	char *name, *mnm;
{
	struct style *s;
	s = style_New();
	style_SetName(s, name);
	style_SetMenuName(s, mnm);
	stylesheet_Add(DocSheet, s);
	return s;
}

	static 
InitStyles(doc)
	struct text *doc;
{
	if (ScribeDoc == NULL) {
		ScribeDoc = text_New();
		text_ReadTemplate(ScribeDoc, "scribe", FALSE);
		ScribeSheet = ScribeDoc->styleSheet;
	}
	StyleMap = NULL;

	DocSheet = doc->styleSheet;
	iStyle = stylesheet_Find(DocSheet, "italic");
	if (iStyle == NULL) 
		style_AddNewFontFace(
			iStyle = createStyle("italic", "Font~1,Italic~11"), 
			fontdesc_Italic);
	bStyle = stylesheet_Find(DocSheet, "bold");
	if (bStyle == NULL) 
		style_AddNewFontFace(
			bStyle = createStyle("bold", "Font~1,Bold~10"), 
			fontdesc_Bold);
	uStyle = stylesheet_Find(DocSheet, "underline");
	if (uStyle == NULL) 
		style_AddUnderline(
			uStyle = createStyle("underline", "Font~1,Underline~41"));
	inxiStyle = stylesheet_Find(DocSheet, "indexi");
	if (inxiStyle == NULL) {
		style_AddNewFontFace(
			inxiStyle = createStyle("indexi", "Title~3,Invisible Index~41"), 
			fontdesc_Italic);
		style_SetFontScript(inxiStyle, style_PreviousScriptMovement,
			-2, style_Points);
	}
	passStyle = stylesheet_Find(DocSheet, "formatnote");
	if (passStyle == NULL) 
		style_AddPassThru(passStyle = createStyle("formatnote", 
						"Region~4,FormatNote~60"));
}

#if 0
	static void
DefineSheet() 
{
	/* process @define to make stylesheet */
}
#endif /* 0 */


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
/* appendtobuf(buffer, buflen, doit)
	scan across a Scribe attribute value
	if 'doit' is true, append to output
	if buffer is non-NULL, put the output in the buffer
		instead of the regular output
	terminate either for matching " or for one of the delimiters
	skip = and "
	leave character after termination in nextch
	Assumes whitespace has been skipped.
	Skips whitespace after terminating "

	To allow styles inside index item, this routine has a miniature DoText
	and uses DoAt and checks for terminating characters equal to endsp.
*/
	static void
appendtobuf(buf,buflen,doit)
	char *buf;
	long buflen;
	boolean doit;
{
	boolean quoted, scanning;
	if (nextch == '=')  {
		getnextch();
		SaveWhiteSpace();
	}
	quoted = (nextch == '\"');
	if (quoted) getnextch();   /* skip " */
	scanning = TRUE;
	while (scanning){
		switch(nextch) {
		case '@':
			getnextch();
			if (doit) DoAt();
			break;
		case '\"':
			if (quoted) {
				SaveWhiteSpace();
				getnextch();
			}
			scanning = FALSE;
			break;
		case ',':
			if ( ! quoted)  scanning = FALSE;
			else {
				if(buf){
					*buf = nextch;
					if(--buflen > 1) buf++; 
				}
				if (doit) pout(nextch);
				getnextch();
			}
			break;
		case '}':
		case ')':
		case ']':
		case '>':
		case '\'':
			if (endsp && nextch == scribeEnd[endsp]) {
				/* end an environment */
				if (doit) CloseEnvt(nextch, NULL);
				getnextch();
				break;
			}
			else if ( ! quoted) {
				scanning = FALSE;
				break;
			}
			else /* FALL THROUGH */;
		default:
			if (doit) pout(nextch);
			if(buf){
				*buf = nextch;
				if(--buflen > 1) buf++;
			}
			getnextch();
			break;
		}
	}
	if(buf) *buf = '\0';
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

	static void
DoText() 
{
		/* reads inf and  writes it appropriately to Doc
		   Converts newlines and @word`s according to rules 
		*/
#define ouch(c)  (*op++ = c, DocPos++)
	register c;
	register unsigned char *op, *ep;
	op = outp;
	ep = &(buf[BUFSIZE-1024]);
	endsp = 0;
	NoFill[endsp] = FALSE;
	c = nextch;
	while (c != EOF) {
		if (op >= ep) {
			outp = op;
			ProcessLine(FALSE);
			op = outp;
		}
		switch (c) {
		case '\t': 
			if (HaveNonWhiteSpace)
				EmbeddedWhiteSpace++;
			else StartsWhite = TRUE;
			ouch(c);
			c = getnextch();
			break;
		case '\015':			/* ignore carriage returns */
		case '\032':		/* ignore  ^Z  from DOS */
			c = getnextch();
			break;
		case ' ': 
			{register nsp = 0;
				do {
					nsp++;
					ouch(c);
					c = getnextch();
				} while (c==' ');
				if (!HaveNonWhiteSpace)
					StartsWhite = TRUE;
				else if (nsp>2) {
					/*  >2  spaces.  count it as embedded white
					   space unless it follows a "sentence" end */
					register unsigned char *p = op-nsp-1;
					if (*p == '"' || *p == ')') p--;
					switch (*p) {
						case '.': case ';': 
						case ':': case '?': 
						case '!':
							break;
						default:
							EmbeddedWhiteSpace++;
							break;
					}
				}
			}
			break;
		case '\n':
			outp = op;
			ProcessLine(TRUE);
			op = outp;
			c = getnextch();
			break;
		case '@': 
			outp = op; 
			if (getnextch() == '*') {
					/* out front of line */
					ProcessLine(FALSE);
					pout('\n');
					getnextch();
					SaveWhiteSpace();
					if (nextch == '\n') 
						getnextch();
					else StartsWhite = OutputWhiteSpace();
			}
			else DoAt();
			op = outp;
			c = nextch;
			break; 
		case '}':
		case ')':
		case ']':
		case '>':
		case '\'':
		case '"':
			if (endsp && c == scribeEnd[endsp]) {
				/* end an environment */
				getnextch();
				outp = op;
				CloseEnvt(c, NULL);
				op = outp;
				c = nextch;
				break;
			}
			else /* FALL THROUGH */;
		default:
			ouch(c);
			c = getnextch();
			HaveNonWhiteSpace = TRUE;
			break;
		} /* end switch(c) */

	} /* end while */
	outp = op;
	if (endsp > 0)
		CloseEnvt('z', NULL);	 /* close excess stuff */
	ProcessLine(FALSE);
	while (StyleMap != NULL) {
		struct stylemap *t = StyleMap;
		StyleMap = StyleMap->next;
		text_AddStyle(Doc, t->pos, t->len, t->s);
		free(t);
	}
#undef ouch
}

	static 
DoAt()
{
	struct style *style;
	register unsigned char *word, endc;
	unsigned char tc;
	int len,code;
	char *subobjtype;
	struct text *subobj;
	static unsigned char C1[2] = " ", C2[2] = " ", C3[2] = " ";

	if (nextch == ' ' || nextch == '\t') {
		OpenEnvt(passStyle, 'p', '-');
		pout('@');
		pout(nextch);
		CloseEnvt('p', NULL);
		getnextch();
		return;
	}

	word = CollectWord(&len);

	if (len == 0) {
		/* @* is done above */
		if (nextch == '@') 
			pout('@');
		else {
			OpenEnvt(passStyle, 'p', '-');
			pout('@');
			pout(nextch);
			CloseEnvt('p', NULL);
		}
		getnextch();
		return;
	}
	subobjtype = "textref";
	switch ((code = IsReserved(word))) {
		case 0:   /* not reserved */
			style = stylesheet_Find(DocSheet, word);
			if (style == NULL) {
				style = stylesheet_Find(ScribeSheet, word);
				if (style != NULL) {
					struct style *newstyle = style_New();
					style_Copy(style, newstyle);
					stylesheet_Add(DocSheet, newstyle);
					style = newstyle;
				}
			}
			endc = ScribeDelimiter(&tc);
			if (style == NULL) {
				OpenEnvt(passStyle, endc, ')');
				pout('@');
				putstring(word, len);
				pout(tc);
			}
			else {
				if (outp == &buf[(AfterNewline?1:0)]
						&& style_TestAddFlag(style,
							 style_KeepPriorNL)) 
					LineStartCommand = TRUE;
				OpenEnvt(style, endc, '-');
			}
			if (endc == ' ') {
				/* @word without delimited text */
				CloseEnvt(endc, NULL);
				OutputWhiteSpace();
			}
			break;
		case beginNUMBER:
			if (outp == &buf[(AfterNewline?1:0)])
				LineStartCommand = TRUE;
			endc=ScribeDelimiter(&tc);
			if (endc == ' ') {
				Error0 ("No environment name after @begin; ignoring it");
				OutputWhiteSpace();
				break;
			}
			word = CollectWord(&len);
			SaveWhiteSpace();
			if (nextch == endc) {
				getnextch();
				endc = ScribeDelimiter(&tc);
				style = stylesheet_Find(DocSheet, word);
				if (style == NULL) {
					style = stylesheet_Find(ScribeSheet, word);
					if (style != NULL) {
						struct style *newstyle = style_New();
						style_Copy(style, newstyle);
						stylesheet_Add(DocSheet, newstyle);
						style = newstyle;
					}
				}
				if (style == NULL) {
					OpenEnvt(passStyle, 'e', '@');
					putstring("@begin", 6);
					pout(tc);
					putstring(word, len);
					pout(endc);
				}
				else
					OpenEnvt(style, 'e', '-');
			}
			else {
				if (nextch != ',') {
					C1[0] = tc;  C2[0] = nextch;  C3[0] = endc;
					Error4("after @begin%s%s, '%s' should be '%s' or ','", 
						C1, word, C2, C3);
				}
				OpenEnvt(passStyle, 'e', '@'); 
				putstring("@begin", 6);
				pout(tc);
				putstring(word, len);
				pout(',');
				while (getnextch() != endc) {
					if (nextch == '@') {
						C1[0] = tc;  C2[0] = endc;
						Error3("found '@' inside \"@begin%s%s, ... %s\"", 
								C1, word, C2);
						break;
					}
					pout(nextch);
					if (nextch == '\n') 
						LineNo++;
				}
				pout(endc);
				if (nextch!='@') 
					getnextch();
			}
			SaveWhiteSpace();
/*
			if (nextch == '\n')
				LineEndCommand = TRUE;
*/
			OutputWhiteSpace();
			break;
		case endNUMBER:
			endc = ScribeDelimiter(&tc);
			if (endc == ' ') {
				Error0("No environment name after @end");
				CloseEnvt('e', word);
				OutputWhiteSpace();
				break;
			}
/*
			if (outp == &buf[(AfterNewline?1:0)]) 
				LineStartCommand = TRUE;
*/
			word = CollectWord(&len);
			SaveWhiteSpace();
			if (strcmp(style_GetName(currMap[endsp]->s), word) != 0) {
				C1[0] = tc;  C2[0] = nextch;
				Error3("@end%s%s%s does not match @begin",
						C1, word, C2);
			}
			if (nextch == endc) 
				getnextch();
			else {
				C1[0] = nextch;  C2[0] = endc;
				Error2("after @end, '%s' should be '%s'", 
						C1, C2);
			}
			CloseEnvt('e', word);
			SaveWhiteSpace();
			if (nextch == '\n')
				LineEndCommand = TRUE;
			OutputWhiteSpace();
			break;
		case iNUMBER:  
			OpenEnvt(iStyle, ScribeDelimiter(&tc), '-');  break;
		case bNUMBER:   
			OpenEnvt(bStyle, ScribeDelimiter(&tc), '-');  break;
		case uNUMBER:   
			OpenEnvt(uStyle, ScribeDelimiter(&tc), '-');  break;


		/* for @newpage we generate a page break object */
		case pageNUMBER:
			/* flush buf, simulating newline */
			if ( ! AfterNewline && outp == &buf[0])   {
				/* we are probably just after a newline.  
				   add nothing before the bp object */
			}
			else {
				if (outp > buf + (AfterNewline ? 1 : 0)) {
					/* the line is not empty.  flush it */
					ProcessLine(TRUE);
				}
				ProcessLine(FALSE);	/* flush the newline */
			}
			text_InsertObject(Doc, WhereTo, "bp", "bpv");
			DocPos++;
			WhereTo++;
			ProcessLine(TRUE);	/* output a newline after the bp */

			/* since we invented a newline, skip one if present. */
			if (nextch == '\n') getnextch();
			break;

		/*  transformations of index entries:
			@index(xyz)  => index object for xyz
			@indexentry[Key="a xyz",Entry="@i(xyz)",Number]
				=> index object for " xyz"  (note initial space;  text be italic)
			@indexsecondary[Primary="xyz",Secondary="abc",Number]
				=> index object for  xyz++abc
		*/
		case indexNUMBER:  
			endc = ScribeDelimiter(&tc);
			if (endc == ' ') break;	/* ignore error */
			SaveWhiteSpace();

			OpenEnvt(inxiStyle, 'x', '-');
			appendtobuf(NULL, 0, TRUE);
			CloseEnvt('x', NULL);

			if (nextch == endc)
				getnextch();	/* skip terminating delimiter */
			else {
				C1[0] = endc;
				Error1("Missing %s after @index", C1);
			}
			break;
		case labelNUMBER:
		    subobjtype = "texttag";
		case pagerefNUMBER:
		case refNUMBER:
		    if (outp > buf + (AfterNewline ? 1 : 0)) {
			/* the line is not empty.  flush it */
			ProcessLine(FALSE);
		    }
		    endc = ScribeDelimiter(&tc);
		    if (endc == ' ') break;	/* ignore error */
		    SaveWhiteSpace();
		    if((subobj = (struct text *)class_NewObject(subobjtype)) == NULL){
			appendtobuf(NULL, 0, FALSE);
		    }
		    else {
			/* read the stuff in delimiter and stuff in subobj */
			char cbuf[512] ;
			appendtobuf(cbuf,512,FALSE);
			text_InsertCharacters(subobj,0,cbuf,strlen(cbuf));
			if(code == pagerefNUMBER) text_InsertCharacters(subobj,0,"# ",2);
			text_AddView(Doc,WhereTo,text_ViewName(subobj), subobj);
			DocPos++;
			WhereTo++;
		    }
		    if (nextch == endc)
			getnextch();	/* skip terminating delimiter */
		    else {
			C1[0] = endc;
			Error1("Missing %s after @index", C1);
		    }
		    break;
		case ientryNUMBER:
		case isecNUMBER:
			endc = ScribeDelimiter(&tc);
			if (endc == ' ') break;	/* ignore error */
			OpenEnvt(inxiStyle, endc, '-');
			while (nextch != endc) {
				word = CollectWord(&len);
				SaveWhiteSpace();
				if (strcmp(word, "entry") == 0) {
					pout(' ');
					appendtobuf(NULL, 0, TRUE);
				}
				else if (strcmp(word, "primary") == 0) 
					appendtobuf(NULL, 0, TRUE);
				else if (strcmp(word, "secondary") == 0) {
					pout('+');
					pout('+');
					appendtobuf(NULL, 0, TRUE);
				}
				else if (nextch == '=') 
					appendtobuf(NULL, 0, FALSE);
				else if (len == 0)
					/* skip error character */
					getnextch();
			}
			CloseEnvt(endc, NULL);
			getnextch();	/* skip terminating delimiter */
			break;

		/* XXX later, need to parse @defines */
		/* for now, pass it along by DROP THROUGH to default case */
		case defineNUMBER: 
		default:   /* process a reserved word environment */
			if (outp == &buf[(AfterNewline?1:0)])
				LineStartCommand = TRUE;
			endc = ScribeDelimiter(&tc);
			OpenEnvt(passStyle, endc, ')');
			pout('@');
			putstring(word, len);
			if (endc == ' ') {
				/* @word without delimited text */
				CloseEnvt(endc, NULL);
				OutputWhiteSpace();
			}
			else 
				pout(tc);
			SaveWhiteSpace();
			OutputWhiteSpace();
			break;
	}
}

	static 
OpenEnvt(style, endc, closemode) 
	register struct style *style; 
	unsigned char endc; 
	unsigned closemode;
{
	struct stylemap *map;
	scribeEnd[++endsp] = endc;
	endMode[endsp] = closemode;

	map = (struct stylemap *)malloc(sizeof(struct stylemap));
	map->pos = DocPos;
	map->s = style;
	map->next = StyleMap;
	StyleMap = map;
	currMap[endsp] = map;

	if (style_IsPassThruUnchange(style) 
			&& style_TestUseOldFlag(style, style_NoWrap)) 
		NoFill[endsp] = NoFill[endsp-1];
	else 
		NoFill[endsp] = style_IsPassThruAdded(style)  
			||  style_TestAddFlag(style, style_NoWrap);
}

	static
CloseEnvt(c, word)
	unsigned char c;
	unsigned char *word;
{
	static unsigned char C1[2];
	while (endsp > 0) {

		/* save end info to see if line break needed */
		LatestEndLoc = outp;
		LatestEndMap = currMap[endsp];

		if (scribeEnd[endsp] == c) {
			switch(endMode[endsp]) {
			case ')':  
				pout(c);
				break;
			case '@':
				putstring("@end[", 5);
				putstring(word, strlen(word));
				pout(']');
				break;
			}
			currMap[endsp]->len = DocPos - currMap[endsp]->pos;
			endsp--;
			return;
		}
		currMap[endsp]->len = DocPos - currMap[endsp]->pos;
		C1[0] = scribeEnd[endsp];
		Error2("Missing '%s' detected by %s", C1,
				(c == 'z' ? "end of file" : "@end"));
		endsp--;
	}
	endsp = 0;
	if (c == 'e') 
		Error0("Excess @end");
}

	static
ProcessLine(NextIsNewline)
	boolean NextIsNewline;
{
		/* Scans the output buffer and decides whether
		preceding newline is real or should be a blank. 

		Also may decide to put a for-real newline at the end of this line
		or a possible newline at the beginning of the next.  */
	/* initial state is 
		text of line is in buf, 
		AfterNewline indicates whether buf[0] is a newline 
				at end of previous line

		Processing may do any of the following:
				replace old buf[0] with ' '
				add 1 or 2 spaces at end of line (after .;:?!)
				remove \ from end of line
				set AfterNewline
				InsertString of text into output
				set buf[0] for next line
	*/
	register unsigned char *cx;
	boolean TabOnlyAtEnd = FALSE;  /* true for line with 
				EmbeddedWhiteSpace==1 and last char =='\t' 
				(This catches the mail kludge of using
				\t at end of line instead of @*) */

	LineNo++;

	*outp = '#';  /* dummy characters after line end */
	*(outp+1) = ' ';
	if (EmbeddedWhiteSpace == 1 && *(outp-1)=='\t') {
		EmbeddedWhiteSpace = 0;
		TabOnlyAtEnd = TRUE;
	}
	if (AfterNewline) {
		/* decide whether to replace buf[0] with blank 
		   If one of the conditions after the ! below is TRUE,
		   then the newline at beginning of line will remain a newline */
		if (! (outp <= buf+1 || EmbeddedWhiteSpace || StartsWhite
				|| LineStartCommand ) )
			/* replace \n with space */
			*buf = ' '; 
	}


	/* now decide whether to put newline at end of this line
		or leave it at front of the next */

	AfterNewline = FALSE;

	/* find last non-blank character */
	cx = outp-1;   
	while (cx>=buf && *cx==' ') {cx--;}
	if (cx<buf) cx = outp;  /* we know that *outp=='#' */

	if ( ! NextIsNewline)  
		/* we are not really at end of line, do nothing */
		{}
	else if (*(outp-1)=='\\') {
		/* backslash-newline is converted to blank 
			and partial line is flushed below */
		*(outp-1) = ' ';
	}
	else if (EmbeddedWhiteSpace || ! HaveNonWhiteSpace
			|| TabOnlyAtEnd   /* MailKludge */
			|| LineEndCommand 
			|| NoFill[endsp]
			||	/* want a newline at end if line ends with a style
				   that has KeepNextNL set */
				(LatestEndLoc == cx  &&
					style_TestAddFlag(LatestEndMap->s,
						style_KeepNextNL))
		) {
		/* unconditionally get newline at end of line */
		pout('\n');
	}
	else {  /* is tentative \n, which may be replaced with space;
			ensure two spaces after punct at end of line:
				punct ["] space* => punct ["] space
				non-punct space space* => non-punct space* 
				An additional space will occur from \n */
		register havespace = outp-cx-1;
		if ((*cx=='"' || *cx==')') && cx>buf) cx--;
		if (*cx=='.'||*cx=='!'||*cx==';'||*cx==':'||*cx=='?') {
			if (havespace<1) pout(' ');
			else if (havespace>1) {
				outp--;
				DocPos--;
			}
		}
		else if (havespace>0){
			outp--;
			DocPos--;
		}

		AfterNewline = TRUE;
	}

	/* output the line */
	text_InsertCharacters (Doc, WhereTo, buf, outp-buf);
	WhereTo += outp-buf;
	outp = &(buf[0]);

	/* start next line */
	if (AfterNewline) 
		pout('\n');		/* may be replaced by blank later */

	LineStartCommand = FALSE;
	LineEndCommand = FALSE;
	StartsWhite = HaveNonWhiteSpace = FALSE;
	EmbeddedWhiteSpace = 0;
	LatestEndLoc = NULL;
}


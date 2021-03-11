/* readtlx.c - read .tlx file and build Classes and Thongs lists */
/*
	Copyright Carnegie Mellon University 1992 - All rights reserved
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
	char *tlex_readtlx_rcsid = "$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/syntax/tlex/RCS/readtlx.c,v 1.5 1992/12/19 21:51:43 wjh R6tape $";
#endif


#include <stdio.h>
#include <ctype.h>

#include <global.h>
#include <gentlex.h>

static char InputBuffer[200];	/* usually contains the next line of input */
static char *InputText;		/* pts to first non-whitespace in InputBuffer*/


/* keywords */
#define TOKENCLASS  0
#define RECOGNIZER  1
#define SEQ	    2
#define SET	    3

/* types */
#define CHAR	    4
#define CHARSET     5
#define TOKENNUMBER 6
#define BOOLEAN     7
#define INT	    8
#define LONG	    9
#define FLOAT	    10
#define DOUBLE	    11
#define ACTION	    12


/* table of keywords (words that start lines)*/
static struct symbol keyword[] = {
	{"tokenclass",	TOKENCLASS },
	{"recognizer",	RECOGNIZER},
	{"seq",		SEQ},
	{"set",		SET},
	{"char",	CHAR},
	{"charset",	CHARSET},
	{"tokennumber",	TOKENNUMBER},
	{"boolean",	BOOLEAN},
	{"int",		INT},
	{"long",	LONG},
	{"float",	FLOAT},
	{"double",	DOUBLE},
	{"action",	ACTION},
	{NULL, -99}
};

/* builtin recognizer names */
static struct symbol recognizer[] = {
	{"ScanWhitespace", 	tlex_WHITESPACE},
	{"ScanWhiteSpace", 	tlex_WHITESPACE},
	{"ScanError",		tlex_ERROR},
	{"ScanComment",	tlex_COMMENT},
	{"ScanToken",		tlex_TOKEN},
	{"ScanID",		tlex_ID},
	{"ScanId",			tlex_ID},
	{"ScanNumber",	tlex_NUMBER},
	{"ScanString",		tlex_STRING},
	{NULL, 			-99}
};


/* lookup(sx,ex,table)
	search table for the string starting at sx and extending to ex-1
	return table->n for the successful table entry
	if none found, return the table entry for NULL
*/
	static int
lookup(sx, ex, table)
	char *sx, *ex;
	struct symbol *table;
{
	for ( ; table->s; table++)
		if (strncmp(sx, table->s, ex-sx) == 0)
			return table->n;
	return table->n;	/* not found */
}


/* GetLine(f, deblank) 
	reads a line into InputBuffer
	and increments LineNo appropriately
	InputText is set to the first non-whitespace character
	if deblank is True, blank and comment lines are skipped
	(lines may be read in chunks rather than ending in newlines
		this works for C code, but not tlex stuff)
	at eof, sets *InputBuffer = '\0'
*/
	static void
GetLine(f, deblank)
	FILE *f;
	boolean deblank;
{
	while (TRUE) {
		/* increment LineNo if reached the end of the previous line */
		if (*(InputText+strlen(InputText)-1) == '\n')
			LineNo ++;
		/* get chunk of next line */
		if (fgets(InputBuffer, sizeof(InputBuffer)-1, f) == NULL) {
			/* end of file */
			*InputBuffer = '\0';
			InputText = InputBuffer;
			return;
		}
		/* set pointer to first non-whitespace character */
		for (InputText = InputBuffer; 
				*InputText && isspace(UNSIGN(*InputText));
				InputText++) 
			{}
		if ( ! deblank) return;	/* got a line */
		if ( ! *InputText) continue;	/* blank line */
		if (*InputText == '-' && *(InputText+1) == '-') 
			/* comment-only line */
			continue;
		return;
	}
}


/* parseClines(f)
	The InputBuffer begins with left brace ({) as its first non-whitespace
	character.  This line and subsequent lines are read from f and 
	merged to a linked list of struct line's having type C.
	The C code extends to either
		a line beginning with } at the same or less indent 
			than the initial line
		or a line beginning with the keyword "tokenclass"
	Returns the list.
	The InputBuffer is left containing the next input line.
*/
	struct line *
parseClines(f)
	FILE *f;
{
	int firstindent;	/* indent of first line */
	char *cx;
	struct line *result, *last, *tl;

	firstindent = InputText - InputBuffer;

	tl = (struct line *)malloc(sizeof(struct line));
	tl->lineno = LineNo;
	tl->next = NULL;
	tl->type = C;
	tl->u.c.text = freeze(InputBuffer, NULL);
	result = last = tl;

	while (TRUE) {
		GetLine(f, FALSE);
		if (strncmp(InputText, "tokenclass", sizeof("tokenclass"))
				== 0)
			/* reached start of next tokenclass */
			return result;
		/* accumulate line into result */
		tl = (struct line *)malloc(sizeof(struct line));
		tl->lineno = LineNo;
		tl->next = NULL;
		tl->type = C;
		tl->u.c.text = freeze(InputBuffer, NULL);
		last->next = tl;
		last = tl;		

		if (*InputText == '}' 
				&& InputText - InputBuffer <= firstindent) {
			/* this has been the last line */
			GetLine(f, TRUE);
			return result;			
		}
	}
}


/* ParseLine(fin)
	Reads a line, processes it as part of the block for hdr
	Returns FALSE for EOF or a line starting with "tokenclass"
	Has not read ahead unless returns FALSE
Line types:
	whitespace or comments - ignored, not returned
	Rec	- starts with "recognizer"
	Set, Seq - starts with "set" or "seq"
	Elt	- any other line is parsed to: type, var, value
	C	- parseClines
*/
	boolean
ParseLine(fin, hdr)
	FILE *fin;
	struct line *hdr;
{
	char *bx, *kx;
	struct line *v;
	int keywd, i;
	char *type, *var, *val;
	char buf[20];

	GetLine(fin, TRUE);
	if (*InputText == '{') {
		hdr->u.h.Ccode = parseClines(fin);
		return FALSE;
	}

	/* get and check keyword */
	bx = InputText;	/* start of keyword */
	while (*bx && isalpha(UNSIGN(*bx))) bx++;
	keywd = lookup(InputText, bx, keyword);

	/* get start of next word */
	while (*bx && isspace(UNSIGN(*bx))) bx++;
	kx = bx;	/* start of next word */

	switch (keywd) {

	case TOKENCLASS:
		return FALSE;
	case RECOGNIZER: 
		/* fill in recognizer value */
		while (*bx && ! isspace(UNSIGN(*bx))) bx++;
		hdr->u.h.recognizer = lookup(kx, bx, recognizer);
		if (hdr->u.h.recognizer == -99) {
			*bx = '\0';
			ErrorA(ERROR, "Unknown recognizer name", kx);
			hdr->u.h.recognizer = tlex_TOKEN;
		}
		return TRUE;
	case SEQ:
		/* store hdr as a thong under argument of the seq line */
		if (hdr->u.h.action == -888)
			Error(WARNING, "Unexpected 'seq' line");
		else {
			val = ScanToken(kx, NULL);
			if (*val == '"' || *val == '\'') {
				/* remove quotes */
				strcpy(val, val+1);
				*(val + strlen(val)-1) = '\0';
			}
			if (*val == '\0') ErrorA(ERROR, 
	"'seq' should be followed by a non-empty quoted string; not", kx);
			else
				ThongAdd(val, hdr, FALSE);
			hdr->u.h.action = 0;
		}
		return TRUE;
	case SET:
		/* read charset and store hdr in Actions indicated */
		if (hdr->u.h.action == -888)
			Error(WARNING, "Unexpected 'set' line");
		else {
			val = CharsetParse(kx);
			for (i = 0; i < 256; i++) 
				if (val[i]) {
					/* store hdr in Actions[i] */
					char buf[2];
					buf[0] = i;   buf[1] = '\0';
					ThongAdd(buf, hdr, TRUE);
				}
			hdr->u.h.action = 0;
		}
		return TRUE;
	case CHAR:
		type = "char *";
		if (*kx == '*') {
			/* advance past '*' */
			bx = kx+1;
			while (*bx && isspace(UNSIGN(*bx))) bx++;
			kx = bx;
		}
		while (*bx && isalnum(UNSIGN(*bx))) bx++;
		var = freeze(kx, bx);
		while (*bx && (isspace(UNSIGN(*bx)) || *bx == '=')) 
			bx++;  /* skip whitespace and leading equal signs */
		val = ScanToken(bx, NULL);
		if (*val == '"'  || *val == '\'') {
			/*  "  is the usual case: 
				ignore existing quotes, Escapify */
			kx = Escapify(val+1, &i);
			i -= 2;		/* ignore trailing quote */
		}
		else {
			ErrorA(WARNING, "Unquoted string", val);
			kx = val;
			i = strlen(val);
		}
		/* freeze and add quotes */
		val = (char *)malloc(i+3);
		*val = '"';
		strncpy(val+1, kx, i);
		val[i+1] = '"';
		val[i+2] = '\0';
		goto decl;
	case CHARSET:
		type = "Charset";
		while (*bx && isalnum(UNSIGN(*bx))) bx++;
		var = freeze(kx, bx);
		while (*bx && (isspace(UNSIGN(*bx)) || *bx == '=')) 
			bx++;  /* skip whitespace and leading equal signs */
		val = CharsetValue(CharsetParse(bx));
		goto decl;
	case TOKENNUMBER:
		type = "int";
		while (*bx && isalnum(UNSIGN(*bx))) bx++;
		var = freeze(kx, bx);
		while (*bx && (isspace(UNSIGN(*bx)) || *bx == '=')) 
			bx++;  /* skip whitespace and leading equal signs */
		keywd = ScanTokenToNumber(bx, NULL);
		if (keywd == 0)
			ErrorA(ERROR, "Unrecognized token representation", bx);
		sprintf(buf, "%d", keywd);
		val = freeze(buf, NULL);
		goto decl;
	case ACTION:
		type = "int";
		while (*bx && isalnum(UNSIGN(*bx))) bx++;
		var = freeze(kx, bx);
		while (*bx && isspace(UNSIGN(*bx))) bx++;
		val = (char *)malloc(4);  /* space for sprintf of action */
		val[0] = '$';  val[1] = '3';  val[2] = '\0';  val[3] = '\0';
		if (strncmp(bx, "-none-", sizeof("-none-")-1) == 0) {
			keywd = 0;
			for ( ; *bx && *bx != '(' && *bx != '\n'; bx++) {}
			if (*bx == '(') 
				val[2] = bx[1];
		}
		else if (strncmp(bx, "-reservedwords-", 
				sizeof("-reservedwords-")-1) == 0) {
			keywd = 2;
		}
		else {
			keywd = ScanTokenToNumber(bx, val+2);
			if (keywd == 0)
				ErrorA(ERROR, "Unrecognized token for action", bx);
		}
		goto decl;
	case BOOLEAN:
		type = "boolean";
		goto vdecl;
	case INT:
		type = "int";
		goto vdecl;
	case LONG:
		type = "long";
		goto vdecl;
	case FLOAT:
		type = "float";
		goto vdecl;
	case DOUBLE:
		type = "double";
		goto vdecl;
	vdecl:
		while (*bx && isalnum(UNSIGN(*bx))) bx++;
		var = freeze(kx, bx);
		/* use as value everything to end of line or -- */
		while (*bx && (isspace(UNSIGN(*bx)) || *bx == '=')) 
			bx++;  /* skip whitespace and leading equal signs */
		kx = bx;
		while (*bx && *bx != '\n' && ! (*bx == '-' && *(bx+1) =='-'))
			bx++;
		while (isspace(UNSIGN(bx[-1])) || bx[-1] == ';') 
			bx--;	/* delete trailing space and semicolons */
		val = freeze(kx, bx);
		/* DROPTHRU */
	decl:
		/* Build Elt value */
		v = (struct line *)malloc(sizeof(struct line));
		v->lineno = LineNo;
		v->next = hdr->u.h.decls;
		v->type = Elt;
		v->u.d.type = type;
		v->u.d.var = var;
		v->u.d.val = val;
		v->u.d.toknum = keywd;	/* only defined for action */
		hdr->u.h.decls = v;
		return TRUE;
	default:
		/* error ! */
		ErrorA(ERROR, "Unintelligible keyword -- line ignored", 
				InputText);
		return TRUE;
	}   /* end of switch(keywd) */
}

/* ParseTokenClass(f)
	process a single tokenclass block
	- create the Hdr
	- add to Classes list
	- compute token number
	- initialize u.h.recognizer to tlex_TOKEN
	- process each individual line
		line processing for set and seq stows the Hdr
		in Thongs or Actions

	-none-
		set ->u.h.tokennumber to 0
	-global-
		set ->u.h.tokennumber to 0
		set u.h.recognizerindex to tlex_GLOBAL
	-errorhandler-
		set ->u.h.tokennumber to 0
		set u.h.recognizerindex to tlex_ERRHAND
		set ErrorHandler to point to the struct for this class
	-reservedwords-
		set recognizer to tlex_RESWD 
		(process further in defaults.c)
*/
	static void 
ParseTokenClass(f)
	FILE *f;
{
	struct line *hdr;
	char *bx;
	char sub[2];

	if (strncmp(InputText, "tokenclass", sizeof("tokenclass")-1) != 0) {
		ErrorA(ERROR, "expected \"tokenclass\".  Got", InputText);
		do {
			GetLine(f, TRUE);
		} while (*InputBuffer && strncmp(InputText, "tokenclass", 
					sizeof("tokenclass")-1) != 0);
		if (*InputBuffer)
			ErrorA(WARNING, "Restart with", InputText);
	}

	hdr = (struct line *)malloc(sizeof(struct line));
	hdr->lineno = LineNo;
	hdr->type = Hdr;
	hdr->u.h.decls = NULL;
	hdr->u.h.Ccode = NULL;
	hdr->u.h.tokennumber = 0;
	hdr->u.h.recognizer = tlex_TOKEN;
	hdr->u.h.structname = GenSym();
	hdr->u.h.subtoken = '\0';
	hdr->u.h.action = -999;		/* reset to 0 by SEQ or SET */
	hdr->next = Classes;
	Classes = hdr;

	for (bx = InputText+sizeof("tokenclass")-1; 
			isspace(UNSIGN(*bx)); 
			bx++)   {}
	/* bx points to tokenclass name or representation:
		-...-	for special classes
		setXxx	for sets
		Xxxx	for named token
		"xxx"	for thong
		'x'	for single character
	*/
	if (*bx == '-') {
		if (strncmp(bx, "-none-", sizeof("-none-")-1) == 0) {
			for ( ; *bx && *bx != '(' && *bx != '\n'; bx++) {}
			if (*bx == '(') 
				hdr->u.h.subtoken = bx[1];
		}
		else if (strncmp(bx, "-global-", sizeof("-global-")-1) == 0) {
			hdr->u.h.tokennumber = 1;	/* (disallow as action) */
			hdr->u.h.recognizer = tlex_GLOBAL;
			hdr->u.h.structname = (char *)malloc(strlen(Prefix)+12);
			sprintf(hdr->u.h.structname, "%s_global", Prefix);
			if (GlobalHandler != NULL) {
				Error(WARNING, 
					"Overriding Prior -global-");
				/* arrange to have the old one pruned */
				GlobalHandler->u.h.recognizer = tlex_TOKEN;
				GlobalHandler->u.h.Ccode = NULL;
			}
			GlobalHandler = hdr;
			hdr->u.h.action = -888;
		}
		else if (strncmp(bx, "-errorhandler-", 
					sizeof("-errorhandler-")-1) == 0) {
			hdr->u.h.tokennumber = 1;	/* (disallow as action) */
			hdr->u.h.recognizer = tlex_ERRHAND;
			if (ErrorHandler != NULL)
				Error(WARNING, 
					"Overriding Prior -errorhandler-");
			ErrorHandler = hdr;
			hdr->u.h.action = -888;
		}
		else if (strncmp(bx, "-reservedwords-", 
					sizeof("-reservedwords-")-1) == 0) {
			hdr->u.h.tokennumber = 2;	/* (special action) */
			hdr->u.h.recognizer = tlex_RESWD;
			if (ResWordHandler != NULL)
				Error(WARNING, 
					"Overriding Prior -reservedwords-");
			ResWordHandler = hdr;
			hdr->u.h.action = -888;
		}
		else {
			ErrorA(ERROR, "unknown special tokenclass", bx);
		}
	}
	else {
		hdr->u.h.tokennumber = ScanTokenToNumber(bx, sub);
		if (hdr->u.h.tokennumber == 0)
			ErrorA(ERROR, "Not a valid token representation", bx);
		hdr->u.h.subtoken = *sub;	/* may be \0 */
	}

	/* process lines of token class description */
	while (ParseLine(f, hdr)) {}

	if (hdr->u.h.action != 0 && hdr->u.h.action != -888)
		Error(ERROR, "There must be a 'seq' or 'set' line");
	hdr->u.h.action = 0;
}

	void
ReadTlx(f)
	FILE *f;
{
	LineNo = 0;
	InputText = InputBuffer;
	*InputBuffer = '\n';		/* to increment LineNo correctly */
	GetLine(f, TRUE);		/* get first line */
	while (*InputBuffer) 
		ParseTokenClass(f);		
}

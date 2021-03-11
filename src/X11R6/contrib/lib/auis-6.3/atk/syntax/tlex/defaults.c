/* defaults.c - generate .tlc file for gentlex */
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
	char *tlex_defaults_rcsid = "$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/syntax/tlex/RCS/defaults.c,v 1.5 1992/12/19 21:51:43 wjh R6tape $";
#endif


#include <ctype.h>
#include <global.h>
#include <gentlex.h>

struct line *ResWords = NULL;


static struct line *WhiteSpace;  /* points to a hdr for tlex_WHITESPACE */


/* hdr->u.h.action is assigned one of four ways:
	reserved words
		ProcessReserved generates for each reserved word a 
		struct line/Hdr which is not in the Classes list.
		It has the correct action for the reserved word 
		consisting of    
			token-number | tlex_ACTRESWD
	thongs
		when output.c is outputting actions, it checks
		to see if the Action is a struct line/Thong
		and, if so, gets the action from ThongAct()
	simple tokens
		a struct line/Hdr is generated for each token 
		in CharAndThongTokens.  its u.h.action is zero, but the
		u.h.tokennumber has the token number.  It is in the 
		Classes list. 
	tokenclass blocks from .tlx file
		each generates a struct line/Hdr in the Classes list
		with a valid u.h.tokennumber and a zero u.h.action.

	for the latter two groups, the code below computes the u.h.action value
		tlex_ACTSCAN | index of hdr in rectbl
	and prunes from the Classes list all entries with recognizer 
	tlex_TOKEN and a NULL u.h.Ccode value

	{hdr->u.h.action is also used for a temporary purpose in readtlx.c
	the values -999 and -888 are replaced so the value is always zero 
	going into defaults.c}
*/


/* templates for building struct in output */

struct recparmtemplate {
	char *type;
	char *name;
	char *defaultval;
};

/* defaultval == "$1" is a flag for handler/recognizer
	replace val with procname 
  defaultval = "$2" means to replace with tokennumber

  $3 in val field tells NormalizeStruct to find the referenced class
  disambiguator letter follows $3
  tokennumber is in u.d.toknum field

*/

struct recparmtemplate ErrorRecparm[] = {
	{"short", "tokennumber", "$2"},
	{"int", "recognizerindex", "tlex_ERROR"},
	{"int", "(*handler)()", "$1"},
	{"char *", "msg", "\"Unspecified error\""},
	{NULL, NULL, NULL}
};
struct recparmtemplate WhitespaceRecparm[] = {
	{"short", "tokennumber", "$2"},
	{"int", "recognizerindex", "tlex_WHITESPACE"},
	{"int", "(*handler)()", "$1"},
	{"boolean", "SaveText", "FALSE"},
	{"Charset", "continueset", "{NULL,0}"},
	{NULL, NULL, NULL}
};
struct recparmtemplate CommentRecparm[] = {
	{"short", "tokennumber", "$2"},
	{"int", "recognizerindex", "tlex_COMMENT"},
	{"int", "(*handler)()", "$1"},
	{"boolean", "SaveText", "FALSE"},
	{"char *", "endseq", "\"\\n\""},
	{NULL, NULL, NULL}
};
struct recparmtemplate Recparm[] = {
	{"short", "tokennumber", "$2"},
	{"int", "recognizerindex", "tlex_TOKEN"},
	{"int", "(*handler)()", "$1"},
	{NULL, NULL, NULL}
};
struct recparmtemplate GlobalRecparm[] = {
	{"short", "tokennumber", "0"},
	{"int", "recognizerindex", "tlex_TOKEN"},
	{"int", "(*handler)()", "$1"},
	{NULL, NULL, NULL}
};
struct recparmtemplate IDRecparm[] = {
	{"short", "tokennumber", "$2"},
	{"int", "recognizerindex", "tlex_ID"},
	{"int", "(*handler)()", "$1"},
	{"boolean", "SaveText", "TRUE"},
	{"Charset", "continueset", "{NULL,0}"},
	{NULL, NULL, NULL}
};
struct recparmtemplate NumberRecparm[] = {
	{"short", "tokennumber", "$2"},
	{"int", "recognizerindex", "tlex_NUMBER"},
	{"int", "(*handler)()", "$1"},
	{"boolean", "IsInt", "TRUE"},
	{"long", "intval", "0"},
	{"double", "realval", "0.0"},
	{NULL, NULL, NULL}
};
struct recparmtemplate StringRecparm[] = {
	{"short", "tokennumber", "$2"},
	{"int", "recognizerindex", "tlex_STRING"},
	{"int", "(*handler)()", "$1"},
	{"boolean", "SaveText", "FALSE"},
	{"char *", "endseq", "\"\\\"\""},
	{"char *", "escapechar", "\"\\\\\""},
	{"char *", "badchar", "\"\\n\""},
	{NULL, NULL, NULL}
};

/* table of pointers to descriptions of recparms */
static struct tmpltbl {
	int recognizer;
	struct recparmtemplate *tmpl;
} rectmpltbl[] = {
		/* recognizers from gentlex.h */
	{tlex_WHITESPACE,	WhitespaceRecparm},
	{tlex_ERROR,		ErrorRecparm},
	{tlex_COMMENT,	CommentRecparm},
	{tlex_TOKEN,		Recparm},
	{tlex_ID,		IDRecparm},
	{tlex_NUMBER,	NumberRecparm},
	{tlex_STRING,	StringRecparm},
		/* pseudo recognizers from global.h */
	{tlex_RESWD,		Recparm},
	{tlex_GLOBAL,	GlobalRecparm},
	{tlex_ERRHAND,	ErrorRecparm},
	{-1, 			NULL}
};

/* NormalizeStruct()
	normalize and expand Elt list as per template
	add other unspecified fields as per table below
*/
	void
NormalizeStruct(hdr)
	struct line *hdr;
{
	struct tmpltbl *ttx;
	struct recparmtemplate *tpl;
	struct line eltanchor;
			/* eltanchor is a struct itself and serves as a dummy
			so prevx is always non-NULL */
	struct line *ex, *prevx, *lastx;

	/* find template */
	for (ttx = rectmpltbl; 
			ttx->recognizer >= 0  
				&& ttx->recognizer != hdr->u.h.recognizer; 
			ttx++)
		{}
	if (ttx->tmpl == NULL) {
		char buff[10];
		sprintf(buff, "%d", hdr->u.h.recognizer);
		LineNo = hdr->lineno;
		ErrorA(FATAL, "Internal error: unknown recognizer type", buff);
		exit(9);
	}
	tpl = ttx->tmpl;

	/* detach existing Decls from hdr and attach to eltanchor */
	eltanchor.next = hdr->u.h.decls;
	hdr->u.h.decls = NULL;
	lastx = NULL;

	/* for each element of template (tpl),  find existing Elt (from 
		list at eltanchor.next)  or create a new one
	*/
	for ( ; tpl->type; tpl++) {
		prevx = &eltanchor;
		ex = prevx->next;
		while (ex && strcmp(ex->u.d.var, tpl->name) != 0) {
			prevx = ex; 
			ex = ex->next;
		}
		if (ex == NULL) {
			/* create an Elt suiting tpl */
			ex = (struct line *)malloc(sizeof(struct line));
			ex->lineno = 0;
			ex->type = Elt;
			ex->next = NULL;
			ex->u.d.type = tpl->type;
			ex->u.d.var = tpl->name;
			ex->u.d.val = tpl->defaultval;
		}
		else {
			/* disconnect from list */
			prevx->next = ex->next;
			ex->next = NULL;
		}

		/* here:  tpl points to a template 
			and ex to the corresponding Elt from .tlx
			check type correspondence
		*/
		if (strcmp(ex->u.d.type, tpl->type) != 0) {
			LineNo = ex->lineno;
			ErrorA(WARNING, "incorrect type specified", 
				ex->u.d.type);
			ErrorA(WARNING, "	should be", tpl->type);
		}

		if (lastx == NULL) 
			hdr->u.h.decls = ex;
		else
			lastx->next = ex;
		lastx = ex;
	}

	/*  attach remaining elts from .tlx at end of list of elts */
	if (lastx)
		lastx->next = eltanchor.next;
	else hdr->u.h.decls = eltanchor.next;

	if (hdr->u.h.Ccode == NULL  &&  hdr->u.h.recognizer != tlex_GLOBAL) 
		for (ex = eltanchor.next; ex; ex = ex->next) {
			LineNo = ex->lineno;
			ErrorA(WARNING, "unused field", ex->u.d.var);
		}
	else for (ex = eltanchor.next; ex; ex = ex->next)
		/* check for action values */
		if (ex->u.d.val[0] == '$' && ex->u.d.val[1] == '3') {
			/* it is an action field */
			for (hdr = Classes; hdr; hdr = hdr->next)
				if (hdr->u.h.tokennumber == ex->u.d.toknum
					&& hdr->u.h.subtoken== ex->u.d.val[2])
				    break;
			sprintf(ex->u.d.val, "%d", (hdr) ? hdr->u.h.action 
						: tlex_ACCEPT);
		}
}


/* BuildDefaultStructs()
	defaults for WhiteSpace, error character processing, and ErrorHandler
*/
	void
BuildDefaultStructs()
{
	struct line *hdr;
	int c;
	static char buf[2] = " ";

	/* if there is no whitespace tokenclass, create one
		(i.e., if there is no tlex_WHITESPACE)
		make whitespace for any char for which isspace() 
				is true
		but do not override existing tokens for these chars
	*/
	if (WhiteSpace == NULL) {
		hdr = (struct line *)malloc(sizeof(struct line));
		hdr->lineno = 0;
		hdr->type = Hdr;
		hdr->u.h.decls = NULL;
		hdr->u.h.Ccode = NULL;
		hdr->u.h.tokennumber = 0;
		hdr->u.h.recognizer = tlex_WHITESPACE;
		hdr->u.h.structname = GenSym();
		hdr->u.h.action = 0;
		hdr->next = Classes;
		Classes = hdr;
		for (c = 0; c < 256; c++) 
			if (isspace(c)) {
				*buf = c;
				ThongAdd(buf, hdr, FALSE);
			}
	}

	/* generate error character handler 
		and replace NULL's in Actions and ThongAct */
	hdr = (struct line *)malloc(sizeof(struct line));
	hdr->lineno = 0;
	hdr->type = Hdr;
	hdr->u.h.decls = (struct line *)malloc(sizeof(struct line));
		hdr->u.h.decls->lineno = 0;
		hdr->u.h.decls->type = Elt;
		hdr->u.h.decls->next = NULL;
		hdr->u.h.decls->u.d.type = "char *";
		hdr->u.h.decls->u.d.var = "msg";
		hdr->u.h.decls->u.d.val = "\"illegal character\"";
	hdr->u.h.Ccode = NULL;
	hdr->u.h.tokennumber = 0;
	hdr->u.h.recognizer = tlex_ERROR;
	hdr->u.h.structname = GenSym();
	hdr->u.h.action = 0;
	hdr->next = Classes;
	Classes = hdr;

	for (c = 0; c < 256; c++)
		if (Actions[c] == NULL)
			Actions[c] = hdr;
	ThongReplaceNulls(hdr);	

	/* if there is no ErrorHandler tokenclass,  generate
			an ErrorHandler struct with a NULL handler  */
	if (ErrorHandler == NULL) {
		hdr = (struct line *)malloc(sizeof(struct line));
		hdr->lineno = 0;
		hdr->type = Hdr;
		hdr->u.h.decls = NULL;
		hdr->u.h.Ccode = NULL;
		hdr->u.h.tokennumber = 0;
		hdr->u.h.recognizer = tlex_ERROR;
		hdr->u.h.structname = GenSym();
		hdr->u.h.action = 0;
		hdr->next = Classes;
		Classes = hdr;

		ErrorHandler = hdr;
	}
}


/* ProcessReserved(hdr)
	clone hdr for each reserved word
	process through ThongAdd
	set recognizer to tlex_RESWD
	set action to tlex_ACTRESWD | tokennumber
	all the Hdr's are given a pointer to the same code block
		and only one struct and function are actually generated
	the Hdr for each reserved word has the token number for that word
*/
	static void
ProcessReserved(hdr)
	struct line *hdr;
{
	struct line *clone, *last;
	int i;
	char *tx, *bx;
	char buff[200];

	last = NULL;
	ResWords = NULL;
	for (i = 0; i < numtokens; i++) {
		tx = Token[i];
		switch (*tx) {
		case '$': continue;
		case '"': continue;
		case '\'': continue;
		case 'e': if (strcmp(tx, "error") == 0) continue;
		case 's': if (tx[1] == 'e' && tx[2] == 't') continue;
		case 't': if (tx[1] == 'o' && tx[2] == 'k') tx += 3;
		}
		/* swap upper and lower case */
		for (bx = buff ; *tx && bx-buff	< 199; tx++)
			*bx++ = (isupper(UNSIGN(*tx))) ? tolower(UNSIGN(*tx)) : 
				(islower(UNSIGN(*tx))) ? toupper(UNSIGN(*tx)) :
				*tx;
		*bx = '\0';

		clone = (struct line *)malloc(sizeof(struct line));
		*clone = *hdr;
		clone->lineno = 0;	/* (so ThongAdd can override) */
		clone->u.h.decls = (struct line *)freeze(buff, NULL); /* for output*/
		clone->u.h.tokennumber = i; /*  tokennumber for the token */
		clone->u.h.action = tlex_ACTRESWD | i;
		clone->next = NULL;
		if (last == NULL) 
			ResWords = clone;
		else 
			last->next = clone;
		last = clone;

		ThongAdd(buff, clone, FALSE);
	}
}


/* ComputeDefaults()
	check each block in Classes.  if it is "simple", set ->u.h.action
			and remove from Classes list to avoid output 
	if recognizer is tlex_RESERVED, ProcessReserved()
*/
	void
ComputeDefaults()
{
	struct line *hdr, *prev;
	int i;

	/* prune simple blocks;  process special classes */
	prev = NULL;
	for (hdr = Classes; hdr; hdr = hdr->next) {
		switch (hdr->u.h.recognizer) {
		case tlex_RESWD: 
			ProcessReserved(hdr);		
			break;
		case tlex_TOKEN:
			if (hdr->u.h.Ccode == NULL) {
				/* simple token,  remove from Classes list */
				hdr->u.h.action = hdr->u.h.tokennumber;
				if (prev == NULL) Classes = hdr->next;
				else prev->next = hdr->next;
				/* prev does not change */
				continue;
			}
			break;
		case tlex_WHITESPACE:
			WhiteSpace = hdr;
			break;
		}
		prev = hdr;
	}

	BuildDefaultStructs();

	/* assign ->u.h.action value 
		needed before NormalizeStruct in case of action values
		!!! This loop uses same traversal order as WriteRectbl !!! */
	for (hdr = Classes, i = 0; hdr; hdr = hdr->next, i++) 
		hdr->u.h.action = tlex_ACTSCAN | i;
	for (hdr = Classes, i = 0; hdr; hdr = hdr->next, i++) 
		NormalizeStruct(hdr);
}

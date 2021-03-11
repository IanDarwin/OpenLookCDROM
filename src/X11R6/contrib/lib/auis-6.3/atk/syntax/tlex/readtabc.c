/* readtabc.c - read .tab.c files for gentlex */
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
	char *tlex_readtabc_rcsid = "$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/syntax/tlex/RCS/readtabc.c,v 1.5 1992/12/19 21:51:43 wjh R6tape $";
#endif


#include <stdio.h>
#include <ctype.h>

#include <global.h>
#include <gentlex.h>


/* TransEscape(buf, plen)
	translate the characters in buf as though they follow backslash
	in a C string
	store the number of characters used in *plen
	return the character value computed (type is int)

		  The translations are a superset of C:
		      escape seq      :  translation
		      --------------- :  ------------
		      \\ \' \" \b \t  :  as in C
		      \n \v \f \r     :  as in C
		      \ddd	      :  octal digits, as in C
		      \?	      :  DEL  or  \177
		      \e	      :  ESC  or  ctl-[ or \033
		      \^@	      :  NUL  or  \000
		      \^a ... \^z     :  ctl-a ... ctl-z  or  \001 ... \032
		      \^[  \^\	\^]   :  \033  \034  \035
		      \^^  \^_	      :  \036  \037
		      \o	      :  other characters, unchanged
	(This code was once identical to a portion of parse/lexan.c)
*/
	int
TransEscape(buf, plen)
	char *buf;
	int *plen;
{
	static char esctab[]
   = "r\rn\nf\ft\tb\bv\v\"\"\'\'\\\\?\177e\033E\033R\rN\nF\fT\tB\bV\v";
	char *cx;
	int val, len;

	if (isdigit(UNSIGN(*buf))) {
		/* parse digit string */
		len = 0;
		val = 0;
		cx = buf;
		while (isdigit(UNSIGN(*cx)) && len < 3)  {
			val = 8 * val + (*cx++ - '0');
			len++;
		}
	}
	else if (*buf == '^') {
		/* extended syntax for control-x */
		val = '\037' & *(buf+1);
		len = 2;
	}
	else {
		len = 1;
		for (cx = esctab ; *cx && *cx != *buf; cx +=2) {}
		val = (*cx) ? *(cx+1) : *buf;
	}
	if (plen) *plen = len;
	return val;
}



/* ScanToken(cx, sub)
	reads characters starting at cx to find a token representation:
		"..."	process \ escapes
		'.'	process \ escapes
		aaa	isalnum characters
	returns static buffer
	for error, first character of returned string will be '\0'
	If the token is followed by a disambiguating letter in parentheses
		that letter will be stored in sub[0].  (Unless sub==NULL)
*/
	char *
ScanToken(cx, sub)
	char *cx;
	char *sub;
{
	static char buff[200];
	char *bx;
	char t;
	int len;

	while (isspace(UNSIGN(*cx))) cx++;    /* skip preceding whitespace */

	if (isalnum(UNSIGN(*cx))) {
		bx = cx;
		while (isalnum(UNSIGN(*cx))) cx++;
		if (cx-bx > sizeof(buff)-1) 
			cx = bx + sizeof(buff)-1;
		strncpy(buff, bx, cx-bx);
		buff[cx-bx] = '\0';
	}
	else if (*cx != '"' && *cx != '\'') {
		/* not a valid token */
		*buff = '\0';
		*sub = '\0';
		return buff;
	}
	else {
		/* read quoted string */
		t = *cx++;	/* terminator */
		bx = buff;
		*bx++ = t;	/* terminator is part of token */
		while (*cx != t) {		
			if (bx - buff > 197) {
				*bx = '\0';
				ErrorA(WARNING, "truncated long token", buff);
				break;
			}
			/* copy a char into *bx (but check for \ ) */
			if (*cx == '\\') {
				*bx++ = TransEscape(cx+1, &len);
				cx += len+1;
			}
			else
				*bx++ = *cx++;
		}
		*bx++ = t;
		*bx = '\0';	/* truncate long tokens */
	}
	if (sub) {
		while (isspace(UNSIGN(*cx))) cx++;    /* skip whitespace */
		if (*cx == '(') 
			*sub++ = *++cx;
		*sub = '\0';
	}
	return buff;
}


/* ScanTokenToNumber(cx, sub)
	call Read Token to get a token representation
	look it up in Token array and return its token number
	returns 0 for failure
	If the token is followed by a disambiguating letter in parentheses
		that letter will be stored in sub[0].  (Unless sub==NULL)
*/
	int 
ScanTokenToNumber(cx, sub)
	char *cx;
	char *sub;
{
	char *tx = ScanToken(cx, sub);
	int i;
	for (i = 0; i < numtokens; i++)
		if (strcmp(tx, Token[i]) == 0) return i;
	return 0;
}



/* GetToken -  get succeeding letters and digits 
	discards the character following the token
	returns pointer to static storage
	return value is empty if found non-letter-non-digit first
	skips preceding whitespace
*/
	static char *
GetToken(f)
	FILE *f;
{
	static char buf[30];
	char *bx, *bend;
	int c = getc(f);
	bend = buf + sizeof(buf) -1;
	while (isspace(UNSIGN(c)))		/* deblank */
		c = getc(f);
	for (bx = buf; isalnum(UNSIGN(c)) && bx < bend; bx++) {
		*bx = c;	/* save alnum character and get another */
		c = getc(f);
	}
	*bx = '\0';
	return buf;
}


/* CharAndThongTokens()
	make entries in Actions and Thongs for 
		single character and thong tokens  
	The action constructed is a Hdr 
		->decls pts to the Elt
		->tokenclass is the tokennumber
		->recognizer is tlex_TOKEN
		->structname is "tlex_Recparm"
*/
	static void
CharAndThongTokens()
{
	int i, n;
	struct line *hdr;
	char buf[100];

	for (i = 0; i < numtokens; i++) {
		if (*(Token[i]) != '\'' && *(Token[i]) != '\"')
			continue;
		n = strlen(Token[i]);
		if (n > 100)  {
			ErrorA(ERROR, "thong too long", Token[i]);
			n = 100;
		}
		strncpy(buf, Token[i]+1, n-2);
		buf[n-2] = '\0';

		/* construct action */
		hdr = (struct line *)malloc(sizeof(struct line));
		hdr->lineno = 0;	/* is a default */
		hdr->type = Hdr;
		hdr->u.h.decls = NULL;
		hdr->u.h.Ccode = NULL;
		hdr->u.h.tokennumber = i;
		hdr->u.h.recognizer = tlex_TOKEN;
		hdr->u.h.structname = NULL;
		hdr->u.h.action = 0;
		hdr->next = Classes;
		Classes = hdr;

		ThongAdd(buf, hdr, FALSE);
	}
}


/* ReadTabc(f)
	Read filename.tab.c file from f.
	Find YYNTOKENS and set numtokens
	Find yytname and save the token names in TokenNames
*/
	void
ReadTabc(f)
	FILE *f;
{
	char c, *TNx, *tx;
	int i, j, t;
	int namessize;	/* space allocated to TokenNames */

	namessize = 2000;
	TokenNames = (char *)malloc(namessize);
	numtokens = 0;

	while (TRUE) {
		c = getc(f);
		if (c == EOF) {
			Error(FATAL, "yytname not found in .tab.c file");
			exit(9);
		}
		else if (c == 'Y') {
			char *tok = GetToken(f);
			if (strcmp(tok,  "YNTOKENS") == 0 
					|| strcmp(tok, "YNTBASE") == 0)
				numtokens = atoi(GetToken(f));
		}
		else if (c == 'y' && strcmp(GetToken(f), "ytname") == 0)
			break;
		else while (isalnum(UNSIGN(c))) 
			c = getc(f);
	}
	if (numtokens == 0)
		Error(ERROR, "Missing YYNTBASE and YYNTOKENS in .tab.c file\n");

	/* we should be just past 'yytname' in tabc.  expect "[] = {" */
	if (feof(f)) {
		Error(ERROR, "Missing yytname in .tab.c file\n");
		numtokens = 0;
	}
	else if (	/* getc(f) != '[' ||     (discarded by GetToken) */ 
			getc(f) != ']' || getc(f) != ' '
			|| getc(f) != '=' || getc(f) != ' '
			|| getc(f) != '{') {
		Error(ERROR, 
			"Missing \"[] = {\" after 'yytname' in .tab.c file\n");
		numtokens = 0;
	}

	Token = (char **)malloc(numtokens * sizeof (char *));

	/* read each token */
	TNx = TokenNames;
	for (i = 0; i < numtokens; i++) {
		/* first skip whitespace and commas */
		do {
			c = getc(f);
		} while (c == ',' || isspace(UNSIGN(c)));
		if (c != '\"') {
			char buff[10];
			sprintf(buff, "%d", i+1);
			ErrorA(ERROR, 
			"Garbage character in yytnames before token number\n",
				buff);
		}
		/* now amass the characters of the token name */
		Token[i] = TNx;
		while (TRUE) {
			/* ensure space for at least two more characters */
			if (TNx - TokenNames >= namessize - 3) {
				tx = TokenNames;
				namessize += 500;
				TokenNames = (char *)realloc(TokenNames, namessize);
				t = TokenNames - tx;
				TNx += t;
				for (j = 0; j <= i; j++)
					Token[j] += t;
			}

			/* copy a char into *TNx (but check for " and \) */
			c = getc(f);
			if (c == '\"') {
				*TNx++ = '\0';
				break;
			}
			if (c == '\\') {
				/* process an escape, set c to the char */
				char buf[4];
				char *bx = buf;
				int i;
				*bx++ = getc(f);
				if (buf[0] == '^') *bx++ = getc(f);
				else if (isdigit(UNSIGN(buf[0]))) {
					for (i = 2; i; i--, bx++) {
						*bx = getc(f);
						if ( ! isdigit(UNSIGN(*bx)))
							break;
					}
					ungetc(*bx, f);
				}
				*bx = '\0';
				c = TransEscape(buf, NULL);
			}
			*TNx++ = c;
		}  /* end while(TRUE) */
		/*  printf("%s\n", Token[i]);  */
	}  /* end for */

	CharAndThongTokens();    /* generate Hdr's for thongs and tokens */
}

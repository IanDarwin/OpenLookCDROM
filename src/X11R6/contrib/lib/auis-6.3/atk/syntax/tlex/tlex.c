/* tlex.c - lexical analyzer for ATK text */
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
	char *tlex_tlex_rcsid = "$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/syntax/tlex/RCS/tlex.c,v 1.9 1993/05/04 01:34:30 susan Exp $";
#endif

/*
 *    $Log: tlex.c,v $
 * Revision 1.9  1993/05/04  01:34:30  susan
 * RCS Tree Split
 *
 * Revision 1.8.1.1  1993/02/02  04:11:36  rr2b
 * new R6tape branch
 *
 * Revision 1.8  1993/01/05  12:42:32  wjh
 * fixed a bug wherein any character above \177 would terminate a comment
 * fixed a bug wherein illegal characters were not reported thru -errorhandler-
 *
 * Revision 1.7  92/12/14  20:57:48  rr2b
 * disclaimerization
 * 
 * Revision 1.6  1992/12/09  02:58:45  wjh
 * fixed the ">>> Your <<< options" bug
 * by setting correct end condition in FetchChar
 * .
 *
 * Revision 1.5  1992/11/26  01:40:38  wjh
 * fixes and extensions to make it work with bdffont and ness/type
 * .
 *
 * Revision 1.4  92/10/23  10:41:46  wjh
 * fixed bugs in scanning numbers
 * fixed bug in scanning ids whose prefixes matched a prefix of a thong
 * added TruncateTokenText
 * changed names to EndToken, ClearTokenText, and AppendToTokenText
 * 
 * Revision 1.3  92/10/18  19:44:40  wjh
 * final fixes
 * 
 * Revision 1.2  92/10/15  20:05:32  wjh
 * replaced thong scan with one that works
 * conditioned debugging on define of DODEBUG (it can be set in Makefile)
 * .
 * 
 * Revision 1.1  1992/10/11  22:45:27  wjh
 * Initial revision
 *
 * Revision 1.1 1992/8/5   wjh
 * Created from old lex.c
 */

#include <ctype.h>

#include <text.ih>

#include <lexan.ih>
#include <tlex.eh>


#ifdef DODEBUG
#define DEBUG(p) (printf p, fflush(stdin))
#else
#define DEBUG(p)
#endif

/* calls to this routine can arise:
	via a direct call from tlex.c or user handler to tlex_Error
	via a tokenclass with recognizer ScanError
	via an illegal character 
*/
	static int
ErrorWithParm(self, parm)
	register struct tlex *self;
	struct tlex_ErrorRecparm *parm;
{
	register long savex = self->RecentIndex;
	int val;

	if (parm->handler == NULL) {
		fprintf(stderr, "tlex: error at or before position %d - %s\n",
			self->currpos, parm->msg);
		return tlex_IGNORE;
	}

	/* make dummy last token for RecordError */
	self->RecentPos[self->RecentIndex] = self->tokpos;
	self->RecentLen[self->RecentIndex] = self->tokend-self->tokpos+1;
	self->RecentIndex++;
	if (self->RecentIndex >= tlex_RECENTSIZE) self->RecentIndex = 0;

	val = (parm->handler)(self, parm);

	self->RecentIndex = savex;
	return val;
}


	static void
tlex__Error(self, msg)
	register struct tlex *self;
	char *msg;
{
	struct tlex_ErrorRecparm *eparm = self->lextab->ErrorHandler;
	eparm->msg = msg;
	ErrorWithParm(self, eparm);
}


/* fetches character at self->pos; sets charx/lastcharx
	uses text_GetBuffer
	for BackUp, it checks that pos >= startpos
	for end-of-file, returns EOF
*/
	int
tlex__FetchChar(self)
	register struct tlex *self;
{
	long lenwant, lengot;
	if (self->currpos < self->startpos)
		self->currpos = self->startpos;
	if (self->currpos > self->lastpos) {
		self->currpos = self->lastpos+1;
		self->charx = self->lastcharx+1;
		self->currchar = EOF;
	}
	else {
		lenwant = self->lastpos - self->currpos + 1;
		self->charx = text_GetBuf(self->text, self->currpos,
				lenwant, &lengot);
		self->lastcharx = self->charx + lengot - 1;
		self->currchar = (int)(*((unsigned char *)(self->charx)));
	}
	return self->currchar;
}

/* stores the character c in the token buffer;
	usually called only when token buffer size is too small
*/
	int
tlex__PutTokChar(self, c)
	struct tlex *self;
	char c;
{
	register int where = self->tokbufx - self->tokenbuffer;
	int size = self->tokbuflastx - self->tokenbuffer + 2;
	if (where > size - 5) {
		self->tokenbuffer = (char *)realloc(self->tokenbuffer, size+100);
		if (self->tokenbuffer == 0) {
			tlex_Error(self, "Token too big for memory");
			self->currpos = self->lastpos+1;
			return;
		}
		self->tokbufx = self->tokenbuffer + where;
		self->tokbuflastx = self->tokenbuffer + size + 100 - 2;
	}
	*self->tokbufx++ = c;
	return c;
}


/* the rock is available to any function passed this tlex
    The text, pos, and len specify a portion of a text to be processed
*/
	struct tlex *
tlex__Create(ClassID, description, rock, text, pos, len)
	struct classhdr *ClassID;
	struct tlex_tables *description;
	void *rock;
	struct text *text;
	long pos;
	long len;
{
	struct tlex *result = tlex_New();
	struct tlex_Recparm *global;

	result->lextab = description;
	result->rock = rock;
	if (text)
		tlex_SetText(result, text, pos, len);
	global = tlex_Global(result);
	if (global && global->handler)
		(global->handler)(result, global);
	return result;
}

	void
tlex__SetText(self, text, pos, len)
	register struct tlex *self;
	struct text *text;
	long pos, len;
{
	long i;
	self->text = text;
	self->currpos = pos;
	self->startpos = pos;
	self->lastpos = pos + len - 1;
	tlex_FetchChar(self);

	self->RecentIndex = 0;
	for (i = tlex_RECENTSIZE; i--; )
		self->RecentPos[i] = self->RecentLen[i] = 0;
}

	static boolean
tlex__InitializeClass(ClassID)
	struct classhdr *ClassID;
{
	return TRUE;
}

	boolean
tlex__InitializeObject(ClassID, self)
	struct classhdr *ClassID;
	register struct tlex  *self;
{
	self->lextab = NULL;
	self->text = NULL;
	self->alttokenbuf = (char *)malloc(200);
	self->altbuflastx = self->alttokenbuf+200-2;
	self->tokenbuffer = (char *)malloc(200);
	self->tokbuflastx = self->tokenbuffer+200-2;
	if (self->tokenbuffer)
		*self->tokenbuffer = '\0';
	return self->tokenbuffer != 0;
}

	void
tlex__FinalizeObject(ClassID, self)
	struct classhdr *ClassID;
	register struct tlex  *self;
{
	free(self->tokenbuffer);
}


/* tlex_RecentPosition(self, index, plen)
	returns the character index of the first character of the 'index'th 
	token.  Indices are zero for the most recent token, -1 for its 
	predecessor, and so on.
	Indices must be in range [- RECENTSIZE+1 . . . 0]
*/
	long
tlex__RecentPosition(self, index, ploc)
	struct tlex *self;
	long index;
	long *ploc;
{
	long x;
	x = self->RecentIndex - 1 + index;
	while (x < 0) x += tlex_RECENTSIZE;
	if (ploc != NULL)
		*ploc = self->RecentLen[x];
	return self->RecentPos[x];
}

/* tlex_RecentIndent(self, index)
	returns the indent of 'index'th most recent token
	'index' must be in range [- RECENTSIZE+1 . . . 0]
	spaces count as 1 and tabs as 8
	if token is preceded by anything other than whitespace,
		its indent is 999
*/
	long
tlex__RecentIndent(self, index)
	struct tlex *self;
	long index;
{
	long x;
	long indent = 0;
	long currpos = self->currpos;
	int c;

	x = self->RecentIndex - 1 + index;
	while (x < 0) x += tlex_RECENTSIZE;
	tlex_BackUp(self, currpos - self->RecentPos[x]);

	while (self->currpos > self->startpos) {
		c = tlex_BackUp(self, 1);
		if (c == ' ')  indent ++;
		else if (c == '\t') indent +=8;
		else break;
	}
	if (c != '\n')
		indent = 999;

	self->currpos = currpos;
	tlex_FetchChar(self);

	return (indent);
}

/* tlex_Repeat(self, index)
	backup so the next token reported is the index'th, where
	index is as for RecentPosition.
*/
	void
tlex__Repeat(self, index)
	struct tlex *self;
	long index;
{
	long x;
	x = self->RecentIndex - 1 + index;
	while (x < 0) x += tlex_RECENTSIZE;
	self->currpos = self->RecentPos[x];
	self->RecentIndex = x;
	tlex_FetchChar(self);
}



/* = = = = = = = = = = = = = = = = = = = = = = =
	the rest of this file is for
		NextToken()
= = = = = = = = = = = = = = = = = = = = = = = = */


/* ScanNumber(self, parm)
	Parses the input string for an integer or real number
	tokenstartpos is first digit.
	Stores into TokenText a canonical version of the number.
	leaves curpos set to next character after the number
	returns value of calling parm->handler, if any
		otherwise TRUE

*/
	static int
ScanNumber(self, parm)
	register struct tlex *self;
	struct tlex_NumberRecparm *parm;
{
	long len;
	int success;
	union {double d; long i[2];} treal;
	long tlen, got;

	tlen = self->currpos - self->tokpos;  /* usually : 1 */
	if (tlen > 0)
		tlex_BackUp(self, tlen);
		DEBUG(("ScanNumber: tlen %d start %d pos %d\n", 
			tlen, self->tokpos, self->currpos));
	tlen = text_GetLength(self->text) - self->tokpos;
	success = lexan_ParseNumber(
			text_GetBuf(self->text, self->tokpos, tlen, &got),
			&len, &parm->intval, &treal.d);
	if (success == 0) {
		tlex_Error(self, "Error ends number");
		parm->intval = -999;
		if (len <= 0) {
			DEBUG(("number too short: length %d\n", len));
			len = 1;	/* scan at least one character */
		}
	}
	if (len > got) {
		/* SIGH.  GetBuf didn't get enough 
				or ParseNumber went too far. */
		tlex_Error(self, "tlex_ScanNumber: GetBuf failure.  SIGH.");
		len = got;
	}
		DEBUG(("        Number.  pos before: %d\n", self->currpos));
	tlex_BackUp(self, -len);
		DEBUG(("        Number.  pos after: %d\n", self->currpos));
	tlex_EndToken(self);	/* set tokend */
	parm->realval = treal.d;
	parm->IsInt = (success != 2);
	if (success == 2)
		/* double value */
		sprintf(self->tokenbuffer, "0x%lx%lx",
				treal.i[0], treal.i[1]);
	else
		sprintf(self->tokenbuffer, "0x%lx", parm->intval);

	if (parm->handler)
		return (parm->handler)(self, parm);
	return tlex_ACCEPT;
}



/* ScanID(self, parm)
	parses a keyword or identifier
	first character is already in tokenbuffer
	second character is in currchar and is at currpos
	accepts characters having bits set in parm->continueset
	leaves next character in the text to be read
	returns tlex_ACCEPT (found a token)
		or value from handler
*/
	static int
ScanID(self, parm)
	register struct tlex *self;
	struct tlex_IDRecparm *parm;
{
	if (parm->SaveText) {
		if (parm->continueset.vector)
			while (tlex_BITISSET(parm->continueset, 
						self->currchar))
				tlex_Advance(self);
		else 
			while (isalnum(self->currchar))
				tlex_Advance(self);
	}
	else {
		if (parm->continueset.vector)
			while (tlex_BITISSET(parm->continueset, 
					self->currchar))
				tlex_NextChar(self);
		else 
			while (isalnum(self->currchar))
				tlex_NextChar(self);
	}
	tlex_EndToken(self);

	if (parm->handler)
		return (parm->handler)(self, parm);
	else return tlex_ACCEPT;
}


/* ScanString(self, parm)
	parses a string upto parm->endseq
	using parm->escapechar as the escape character

	string cannot contain parm->badchar
	unless it is escaped, in which case it is ignored

	at start, the opening 'delim' must be in tokenbuffer
	and first content character is in currchar and at currpos

	at completion, currpos is the char after closing 'delim'
*/
	static int
ScanString(self, parm)
	register struct tlex *self;
	struct tlex_StringRecparm *parm;
{
	register int c;
	register int delim = *parm->endseq;
	register int badch = *parm->badchar;
	register int escape = *parm->escapechar;

	if (parm->SaveText) {
		tlex_StartToken(self);
		c = self->currchar;
		while (c != delim) {
			if (c == escape)
				tlex_Advance(self);
			else if (c == badch || c == EOF) {
				tlex_Error(self, "Missing end delimiter in string");
				break;
			}
			c = tlex_Advance(self);
		}
		/* delim is currchar */
		tlex_EndToken(self);
		tlex_NextChar(self);
	}
	else {	
		c = self->currchar;
		while (c != delim) {
			if (c == escape)
				tlex_NextChar(self);
			else if (c == badch || c == EOF) {
				tlex_Error(self, "Missing end delimiter in string");
				break;
			}
			c = tlex_NextChar(self);
		}
		/* delim is currchar */
		tlex_NextChar(self);
		tlex_EndToken(self);
	}
	if (parm->handler)
		return (parm->handler)(self, parm);
	else return tlex_ACCEPT;
}


/* ScanComment(self, parm)
	parses a comment upto parm->endseq

	at start, the opening 'delim' must be in tokenbuffer
	and first content character is in currchar and at currpos
	characters are NOT put in tokenbuffer

	at completion, currpos is the char after endseq
*/
	static int
ScanComment(self, parm)
	register struct tlex *self;
	struct tlex_CommentRecparm *parm;
{
	char *delim = parm->endseq;
	register char *cx;
	register int c, dfirst = *delim, dlen;

	/* at the bottom of each while loop, c is a char that did not match, 
		but might start delim.  Because the delimiter must not have
		its first character within it, it suffices to continue 
		looking for the first char */

	if (parm->SaveText) {
		c = self->currchar;
		tlex_StartToken(self);		/* start body */
	
		/* scan for commentdelimiter */
		while (c != EOF) {
			if (c != dfirst)
				c = tlex_Advance(self);
			else {
				/* found first char of delim */
				c = tlex_Advance(self);
				cx = delim+1;
				while (*cx && *cx == c) {
					c = tlex_Advance(self);
					cx++;
				}
				if (*cx == '\0')
					break;
			}
		}
		/* arrange for token text to include only the body
			and not the delimiter */
		dlen = strlen(delim);
		tlex_TruncateTokenText(self, dlen);
		tlex_BackUp(self, dlen);
		tlex_EndToken(self);
		tlex_BackUp(self, -dlen);
	}
	else {	
		c = self->currchar;
	
		/* scan for commentdelimiter */
		while (c != EOF) {
			if (c != dfirst)
				c = tlex_NextChar(self);
			else {
				/* found first char of delim */
				c = tlex_NextChar(self);
				cx = delim+1;
				while (*cx && *cx == c) {
					c = tlex_NextChar(self);
					cx++;
				}
				if (*cx == '\0')
					break;
			}
		}
		tlex_EndToken(self);  /* record end of comment delimiter */
	}
	if (parm->handler)
		return (parm->handler)(self, parm);
	return tlex_IGNORE;
}


/* tlex__NextToken(self, pyylval)
	scan source text for the next token
	Set *pyylval to the symbol for the token and return the token number
	Assumes that currchar is the first to be tested
	Leaves currchar to the character after the token
	returns 0 for end-of-file
*/
	int
tlex__NextToken(self, pyylval)
	register struct tlex *self;
	void **pyylval;
{
	register struct tlex_tables *tab = self->lextab;
	register int action;
	struct tlex_Recparm *parm;
	int success;
	char *tbuf;

	self->tokenvalue = NULL;	/* default value for *pyylval */
	tbuf = self->tokenbuffer;
	self->tokenbuffer = self->alttokenbuf;
	self->alttokenbuf = tbuf;
	tbuf = self->tokbuflastx;
	self->tokbuflastx = self->altbuflastx;
	self->altbuflastx = tbuf;
	self->tokbufx = self->tokenbuffer;
	*self->tokenbuffer = '\0';

tryagain:  /* loop in case encountered whitespace or comment */

	DEBUG(("tryagain at  currchar '%c'  currpos %d\n",
		self->currchar, self->currpos));

	tlex_StartToken(self);
	if (self->currchar == EOF) 
		action = tlex_ACTEOF;
	else if (self->currchar > tab->hichar)
		action = tab->defaultaction;
	else {
		action = tab->action[self->currchar];
/*		DEBUG(("currchar '%c'   actflags %d   actval %d\n",
			self->currchar, action >> 8, action & 0xFF));
*/	}

	if ((action & tlex_ACTTHONG) == 0) {
		/* single character determines token type
		    some token types want the initial character
		    as a token in the tokenbuffer,
		    so we put it there for all token types
		*/
		tlex_Advance(self);
		tlex_EndToken(self);
	}
	else {
		/* recognize a thong */

		/* the action value is an index into the thong
			table.	Traverse table and input
			to find thong and thus the final
			tokennumber and action */
		char **thongx;	/* index into thongtbl.
			Just before the Advance() *thongx is the first thong
			having as prefix the characters in
				text[tokpos...tokpos+currlen] */
		char *samex;	/* pointer to thongsame elt for *thongx */
		int currlen;	/* position in *thongx to consider */
		char **matchx;	/* index of longest recognized thong
				 matchx <= thongx */
		int matchlen;	/* length of *matchx */
		int i;
		int c;		/* currchar */

		i = action & (~tlex_ACTTHONG);
		thongx = &(tab->thongtbl[i]);
		samex = &(tab->thongsame[i]);
		currlen = 0;
		c = tlex_CurrChar(self);

		/*
			Each circuit of the loop either advances through the 
			input and increases currlen, or it tries to
			advance to the next possible thong.
			Because thongtbl is sorted, we never visit an 
			entry more than once for each value of currlen.
		*/
		while (TRUE) {
			if (c == (*thongx)[currlen]) {
				c = tlex_Advance(self);
				currlen++;
			}
			else {
				if ((*thongx)[currlen] == '\0') {
					matchx = thongx;
					matchlen = currlen;
				}
				samex++;
				thongx++;
				if (*samex < currlen) break;
			}
		}
		/* here, matchx pts to the pointer to the recognized thong  */

		DEBUG(("found thong \"%s\"  (# %d)\n",
			*matchx, matchx - tab->thongtbl));

		i = currlen - matchlen;
		if (i > 0) {
			/* scanned too far */
			tlex_BackUp(self, i);
			tlex_TruncateTokenText(self, i);
		}
		tlex_EndToken(self);
		action = tab->thongact[matchx - tab->thongtbl];
	}   /* end of thong processing */

haveprefix:
	DEBUG(("haveprefix: currchar '%c' currpos %d actflags %d  actval %d\n",
		self->currchar, self->currpos, action >> 8, action & 0xFF));

	if (action & tlex_ACTRESWD) {
		parm = tab->reservedwordparm;
		self->tokennumber = action & (~tlex_ACTRESWD);
	}
	else if (action & tlex_ACTSCAN) {
		parm = tab->rectbl[action & (~tlex_ACTSCAN)];
		self->tokennumber = parm->tokennumber;		/* default */
	}
	else if (action == tlex_ACTEOF) {
		self->tokennumber = 0;
		self->tokenvalue = NULL;
		goto gotone;
	}
	else {
		/* no recognizer specified.  character or
			thong is a token by itself */
		self->tokennumber = action;
		self->tokenvalue = NULL;
		goto gotone;
	}

	DEBUG(("recognizer %d  token %d\n", parm->recognizerindex,
			parm->tokennumber));

	switch (parm->recognizerindex) {

	case tlex_WHITESPACE: {
		struct tlex_WhitespaceRecparm *wp
				= (struct tlex_WhitespaceRecparm *)parm;
		if (parm->SaveText) {
			if (wp->continueset.vector)
				while (tlex_BITISSET(wp->continueset, 
						self->currchar))
					tlex_Advance(self);
			else
				while (isspace(self->currchar))
					tlex_Advance(self);
			tlex_EndToken(self);
			success = (wp->handler)(self, parm);
			break;
		}
		else {
			if (wp->continueset.vector) 
				while (tlex_BITISSET(wp->continueset, 
							self->currchar))
					tlex_NextChar(self);
			else
				while (isspace(self->currchar))
					tlex_NextChar(self);
			goto tryagain;
		}
	}
	case tlex_COMMENT:
		success = ScanComment(self, 
				(struct tlex_CommentRecparm *)parm);
		break;
	case tlex_ID:
		success = ScanID(self, (struct tlex_IDRecparm *)parm);
		break;
	case tlex_NUMBER:
		success = ScanNumber(self, 
			(struct tlex_NumberRecparm *)parm);
		break;
	case tlex_STRING:
		success = ScanString(self, 
			(struct tlex_StringRecparm *)parm);
		break;
	case tlex_ERROR: {
		struct tlex_ErrorRecparm *thisparm =
			(struct tlex_ErrorRecparm *)parm;
		struct tlex_ErrorRecparm *ehparm = self->lextab->ErrorHandler;

		/* if thisparm has a handler proc, 
			call errorwithparm on thisparm
		otherwise emulate tlex_ERROR:
			copy the msg to -errorhandler- parm
			and call errorwithparm on the latter
		*/

		if (thisparm->handler)
			success = ErrorWithParm(self, thisparm);
		else {
			ehparm->msg = thisparm->msg;
			success = ErrorWithParm(self, ehparm);
		}

		if (success != tlex_IGNORE) 
			break;

		if (self->currchar == EOF) {
			self->tokennumber = 0;
			self->tokpos = self->lastpos+1;
			self->tokend = self->lastpos;
			success = tlex_ACCEPT;
		}
		break;
	}
	case tlex_TOKEN: {
		if (parm->handler == NULL)
			success = tlex_ACCEPT;
		else success = (parm->handler)(self, parm);
		break;
	}
	}  /* end switch(parm->recognizerindex) */

	DEBUG(("success: %d  tokennumber %d\n", success, self->tokennumber));

	if (success == tlex_IGNORE)
		goto tryagain;
	if (success != tlex_ACCEPT) {
		action = success;
		goto haveprefix;
	}
	
gotone:  /* success == tlex_ACCEPT */

	self->RecentPos[self->RecentIndex] = self->tokpos;
	self->RecentLen[self->RecentIndex] = self->tokend-self->tokpos+1;
	self->RecentIndex++;
	if (self->RecentIndex >= tlex_RECENTSIZE) self->RecentIndex = 0;
	*pyylval = self->tokenvalue;
	return self->tokennumber;
}

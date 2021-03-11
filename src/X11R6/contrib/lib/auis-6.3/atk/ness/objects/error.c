/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
	Copyright Carnegie Mellon University 1992 - All Rights Reserved
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
\* ********************************************************************** */
#ifndef NORCSID
#define NORCSID
static char *rcsid = "$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/ness/objects/RCS/error.c,v 1.27 1993/12/21 22:39:44 wjh Exp $";
#endif

/* error.c
	error processing for ness

	Entry points:

general:

freeze(string)		{in error.h}
codelocStore(nesssym)
codelocForget(nesssym)
codelocFind(loc)

compile time errors:

SetupErrorHandling(ness)
SaveError(msg, loc, len)
ReportError(msr, index)
ExprError(msg, expr)
errorfromparse(parse, severity, msg)
errsynch()
isFuncStart(tok, nexttok, ncheck)

run time errors:

LocateErrorFunc(loc, base, msg, ness)
MapRunError(ness)

*/

/*
 * $Log: error.c,v $
 * Revision 1.27  1993/12/21  22:39:44  wjh
 * change && to & in logical expression.  fix potential core dumnp
 *
 * Revision 1.26  1993/12/10  01:11:42  wjh
 * curNess ==> curComp->ness
 *
 * Revision 1.25  1993/05/04  01:23:55  susan
 * RCS Tree Split
 *
 * Revision 1.23.1.2  1993/02/08  17:34:02  wjh
 * leave warning message text as part of the copunt of lines for error message
 * change isFuncStart to better check and avoid core dump
 * change to use mark in funcnode
 *
 * Revision 1.23.1.1  1993/02/02  03:00:40  rr2b
 * new R6tape branch
 *
 * Revision 1.23  1992/12/16  04:10:48  wjh
 * addjust location reported with error message so it excludes any warning text
 * .
 *
 * Revision 1.22  1992/12/14  20:49:20  rr2b
 * disclaimerization
 *
 * Revision 1.21  1992/12/08  04:14:46  wjh
 * fixed so it will not crash when a runtime error occurs
 * 	moved the recompile to locate error operations to compile.c
 * .
 *
 * Revision 1.20  1992/11/26  02:39:52  wjh
 * converted CorrectGetChar to GetUnsignedChar
 * moved ExtendShortSign to interp.h
 * remove xgetchar.h; use simpletext_GetUnsignedChar
 * nessrun timing messages go to stderr
 * replaced curNess with curComp
 * replaced genPush/genPop with struct compilation
 * created compile.c to handle compilation
 * moved scope routines to compile.c
 * converted from lex to tlex
 * convert to use lexan_ParseNumber
 * truncated logs to 1992 only
 * use bison and gentlex instead of yacc and lexdef/lex
 *
 * .
 *
 * Revision 1.19  91/12/03  22:36:59  gk5g
 * Patch submitted by Todd Inglett:
 * I found a nasty little bug in MapRunError() in atk/ness/objects/error.c. 
 * The errornode that was picked off the front of the list didn't have it's 
 * ``next'' field NULLed, so it remained dangling into errornode's that are 
 * free'd later in the routine.  I also moved the check for NULL before the 
 * errornode is dereferenced (just in case).
 
   log purged Nov, 1992 -wjh
  
 * Creation 0.0  88/05/28 10:16:00  wjh
 * Initial creation by WJHansen
 * 
*/

/* the first byte of an error message indicates whether (:) it is in static memory
		(which means it never changes)
 	or (*) is the result of 'freeze' (which is used when the message is 
		newly generated for each occurrence)
*/

#include <parse.ih>
#include <tlex.ih>
#include <nodeclss.h>
#include <ness.ih>
#include <nesssym.ih>
#include <nessmark.ih>
#include <compdefs.h>
#include <envt.h>
#include <nevent.h>	/* for objnode */

#include <error.h>

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
	errornode functions
\* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

	struct errornode *
errornode_New()
{
	return (struct errornode *)malloc(sizeof(struct errornode));
}

	struct errornode *
errornode_Create(ness, loc, len, execloc, msg, ownmsg, next)
	struct ness *ness;
	long loc, len, execloc;
	char *msg;
	boolean ownmsg;
	struct errornode *next;
{
	struct errornode *enode = (struct errornode *)malloc(sizeof(struct errornode));
	enode->where = ness_CreateMark(ness, loc, len);
	enode->execloc = execloc;
	enode->msg = (unsigned char *)msg;
	enode->ownmsg = ownmsg;
	enode->next = next;
	return enode;
}


	void 
errornode_Destroy(enode)
	struct errornode *enode;
{
	if (enode->ownmsg) free(enode->msg);
	ness_RemoveMark((struct ness *)mark_GetObject(enode->where), enode->where);
	mark_Destroy(enode->where);
	free(enode);
}


/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
	OBJECT CODE LOCATION ROUTINES
	Map from object code location to the nesssym
\* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

	void
codelocStore(fness)
	struct nesssym *fness;
{}
	void
codelocForget(fness)
	struct nesssym *fness;
{}

	static boolean
HasLoc(s, loc)
	struct nesssym *s;
	long loc;
{
	struct nessmark *resmark = &SysMarkLowEnd [
			nesssym_NGetINode(s, funcnode)->SysMarkOffset   ];
	return (loc >= nessmark_GetPos(resmark)  
			&&  loc < nessmark_GetEndPos(resmark));
}

/* codelocFind(loc)
	find the nesssym corresponding to 'loc' in object code 

	scan NessList and scan globals for each Ness  
		(and scan children for each XObject)
*/
	struct nesssym *
codelocFind(loc)
	long loc;
{
	struct ness *ness;
	struct nesssym *g, *xg;

	for (ness = ness_GetList(); ness != NULL; ness = ness->next) 
	  if (ness->compiled)
	    for (g = ness->globals;  g != NULL;  g = g->next) 

		if (g->flags == (flag_function | flag_ness)) {
			if (HasLoc(g, loc)) return g;
		}
		else if (g->flags == flag_xobj)
			for (xg = nesssym_NGetINode(g, objnode)->attrs;
					xg != NULL;
					xg = xg->next) 
				if (xg->flags == (flag_function | flag_ness | flag_xfunc)
						||  xg->flags == flag_event)
					if (HasLoc(xg, loc)) return xg;

	return NULL;
}


/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
	COMPILE TIME ERRORS
\* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

static long lastloc, lastlen;	/* loc/len of last ":. . . restart" */
static boolean Restarted;

	void
SetupErrorHandling(ness)
	struct ness *ness;
{
	lastloc = -1;
	Restarted = FALSE;
	while (ness->ErrorList != NULL) {
		struct errornode *t = ness->ErrorList;
		ness->ErrorList = t->next;
		errornode_Destroy(t);
	}
	ness->ErrorList = NULL;
}


/* ReportError(msg, index)
	calls SaveError after getting the loc and len
		from the token at 'index'
*/
	void
ReportError(msg, index)
	char *msg;
	long index;
{
	long loc, len;
	loc = tlex_RecentPosition(curComp->tlex, index, &len);
	SaveError(msg, loc, len);
}

/* ExprError(msg, expr)
	calls SaveError after getting the loc and len from 'expr'
*/
	void
ExprError(msg, expr)
	char *msg;
	struct exprnode *expr;
{
	SaveError(msg, expr->loc, expr->len);
}

/* SaveError(msg, loc, len)
	adds the message to the list of errors
	list is maintained with earliest message first
*/
	void
SaveError(msg, loc, len)
	char *msg;
	long loc, len;
{
	struct errornode *err;
	struct ness *ness = curComp->ness;

	err = errornode_Create(ness, loc, len, 0, msg, (*msg == '*'), NULL);
	if (ness->ErrorList == NULL)
		ness->ErrorList = err;
	else {
		struct errornode *t;
		for (t = ness->ErrorList; t->next != NULL; t = t->next) 
			{}
		t->next = err;
	}
}


	void
errorfromparse(parser, severity, msg)
	struct parse *parser;
	int severity;
	unsigned char *msg;
{
	/* struct toksym *token; */
	long loc, len;
	static char *syntaxerror = "Syntax error";

	loc = tlex_RecentPosition(curComp->tlex, 0, &len);

	/* don't report error if we have Restarted from a former error 
			at the same loc/len */
	if (Restarted && loc == lastloc  &&  len ==lastlen)
		return;

	if (strcmp(msg, syntaxerror) == 0)
		SaveError(":syntax error", loc, len);
	else {
		char buf[300];
		buf[0] = '*';
		strncpy(buf+1, msg, 298);
		buf[299] = '\0';	/* truncate if needed */
		SaveError(freeze(buf), loc, len);
	}
	Restarted = FALSE;
	if (severity & parse_FREEMSG)
		free(msg);
}

/* errorsynch(index)
	generate an error message that parser will restart after error 
	However, if it is the same restart token as the last, skip it
	returns value to return to parse
*/
	long
errorsynch(index)
	long index;
{
	long loc, len;

	loc = tlex_RecentPosition(curComp->tlex, index, &len);

	if ( ! Restarted || loc > lastloc || len != lastlen) { 
		/* this is the first restart at this token */
		Restarted = TRUE;
		lastloc = loc;
		lastlen = len;
		tlex_Repeat(curComp->tlex, index);
		SaveError(": . . . restart with token", loc, len); 
		return parse_CLEARIN | parse_ERROROK;	
	}
	return parse_ERROR;
}

/* isFuncStart(tok, nexttok, ncheck)
	check to see if this token may be an error restart point
	if 'nexttok' is not 0, it must be the token type
		of the subsequent token
		moreover, the preceding token must not be END
	furthermore, in order to be a restart, one of the 'ncheck' tokens
			starting with this one must be unindented
	The position in the token stream is unaffected; the next lex_NextToken
	will get the token it would have returned had this function not been called
*/
	boolean
isFuncStart(tok, nexttok, ncheck)
	struct nesssym *tok;
	int nexttok;
	long ncheck;
{
	struct nesssym *ttok;
	long n;
	int ttoknum;

	static int ntEND = 0;
	if (ntEND == 0) 
		ntEND = parse_TokenNumberFromName(curComp->parser, "END");

	if (nexttok) {
		/* reject if predecessor is "end" or successor is not nexttok*/
		tlex_Repeat(curComp->tlex, -1);	 /* reread preceding token */
		ttoknum = tlex_NextToken(curComp->tlex, &ttok); /* previous */
		tlex_NextToken(curComp->tlex, &tok); /* reread current token */
		if (ttoknum == ntEND)
			return FALSE;
		ttoknum = tlex_NextToken(curComp->tlex, &ttok); /* next token*/
		tlex_Repeat(curComp->tlex, 0);	/* reset to current token */
		if (ttoknum == 0 /* EOF */  ||  ttoknum != nexttok)
			return FALSE;
	}
	/* here tok still has its original value and lex will return the next token */
	for (n = 0; n < ncheck; n++) {
		if (tlex_RecentIndent(curComp->tlex, 0) == 0) {
			/* succeed */
			if (n>0) tlex_Repeat(curComp->tlex, 1-n);
					/*  ( n==1 ==> 0;  n==2 ==> -1 ) */
			return TRUE;
		}
		if (tlex_NextToken(curComp->tlex, &ttok) == 0)  /* EOF */
			return FALSE;
	}
	/* found none unindented: fail */
	tlex_Repeat(curComp->tlex, 2-ncheck);	
			/*  ( ncheck==2 ==> 0;   ncheck==3 ==> -1 )  */
	return FALSE;
}



/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
	RUNTIME ERRORS
\* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */


/* MapRunError (ness)
	Modify the errornode ness->ErrorList to have the source code location 
	corresponding to the object code location.
*/
	void
MapRunError(ness)
	struct ness *ness;
{
	register struct funcnode *fnode;
	struct nesssym *errsym;
	register struct nessmark *objcode;
	struct errornode *err;
	long loc, len;

	err = ness->ErrorList;
	if (err == NULL) 
		return;
	ness->ErrorList = err->next;	/* remove err from ErrorList
				before it is cleared by SetUpErrorHandling */
	err->next = NULL;
	errsym = codelocFind(err->execloc);
	if (errsym == NULL) 
		return;

	fnode = nesssym_NGetINode(errsym, funcnode);	/* (might be
			an event node.  KLUDGE leading fields are the same) */
	objcode = &SysMarkLowEnd[fnode->SysMarkOffset];
	if (errsym->flags == (flag_function | flag_ness)) {
		/* top level function */
		if (ness != errsym->parent.ness)
			fprintf(stderr, "MapRunErr - ness mismatch 1\n");
	}
	else {
		/* event or function defined in an 'extend' */
		if (ness != errsym->parent.nesssym->parent.ness)
			fprintf(stderr, "MapRunErr - ness mismatch 2\n");
	}

	mark_SetPos(err->where, mark_GetPos(fnode->where));
	mark_SetLength(err->where, mark_GetLength(fnode->where));

	if (compileForLocation(ness, 
			mark_GetPos(fnode->where), 
			mark_GetLength(fnode->where),
			err->execloc - nessmark_GetPos(objcode),
			errsym == ness->InitFunc, 
			&loc, &len)) {
		/* found the line */
		mark_SetPos(err->where, loc);
		mark_SetLength(err->where, len);
	}

	/* delete compilation error messages */
	while (ness->ErrorList != NULL) {
		struct errornode *t = ness->ErrorList;
		ness->ErrorList = t->next;
		errornode_Destroy(t);
	}
	ness->ErrorList = err;
}


/* LocateErrorFunc(loc, base, msg, ness)
	Generates an errornode of 'msg' for the point of error.
	The error occurred at object code location 'loc', where
	the start of the object code is 'base'.
	If the top stack frame is in the 'ness', return the generated message.
	If the top stack frame is not in the 'ness', the actual failure
		is attached to its own ness, which is then exposed.
		Then the stack frames are traversed looking for one that 
		is in the 'ness' and the returned error indicates a 
		failure in the library routine called at that point.
*/
	struct errornode *
LocateErrorFunc(loc, base, msg, ness)
	unsigned char *loc, *base;
	char *msg;
	struct ness *ness;
{
	struct nesssym *wheresym;
	struct ness *whereness;
	struct frameelt *frame;

	wheresym = codelocFind(loc - base);
	if (wheresym == NULL) 
		return errornode_Create(ness, 0, 0, loc - base, msg, (*msg == '*'), NULL);
	whereness = (wheresym->flags == (flag_function | flag_ness)) 
				? wheresym->parent.ness
				: wheresym->parent.nesssym->parent.ness;
	if (whereness == ness) 
		/* the error is in the ness itself */
		/* the msg will be MapRunError'ed in the ness */
		return errornode_Create(whereness, 0, 0, loc - base, msg, (*msg == '*'), NULL);

	/* the message is not in the root ness.
		attach an error message to the ness with the error */
	whereness->ErrorList = errornode_Create(whereness, 0, 0, loc - base, msg, (*msg == '*'),
				whereness->ErrorList);
	MapRunError(whereness);
	ness_Expose(whereness);

	/* go up stack looking for the root ness to give it a message */
	frame = FramePtr;
	while (TRUE) {
		if (frame->returnaddress == NULL) 
			/* this is the outermost frame, give up */
			return errornode_Create(ness, 0, 0, loc - base, 
				":error in subroutine called from unknown location", 
				FALSE, NULL);
		wheresym = codelocFind(frame->returnaddress - base - 2);
		whereness = (wheresym->flags == (flag_function | flag_ness)) 
				? wheresym->parent.ness
				: wheresym->parent.nesssym->parent.ness;
		if (whereness == ness) 
			return errornode_Create(whereness, 0, 0, frame->returnaddress - base - 2,
				":a library function had an error", 
				FALSE, NULL);
		frame = frame->prevframe;
	} 
}

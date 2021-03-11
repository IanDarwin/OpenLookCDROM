/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/ness/objects/RCS/search.c,v 1.36 1993/09/23 21:10:40 wjh Exp $";
#endif

/* 
	search.c  -  implement searches and pattern matches

	Entry points:

	DoSearch(code) - does the search indicated by code
 */

/*
 * $Log: search.c,v $
 * Revision 1.36  1993/09/23  21:10:40  wjh
 * fixed regsearchreverse
 *
 * Revision 1.35  1993/05/04  01:23:55  susan
 * RCS Tree Split
 *
 * Revision 1.33.1.1  1993/02/02  03:05:50  rr2b
 * new R6tape branch
 *
 * Revision 1.33  1992/12/15  21:38:20  rr2b
 * more disclaimerization fixing
 *
 * Revision 1.32  1992/12/15  00:53:01  rr2b
 * fixed disclaimerization
 *
 * Revision 1.31  1992/12/14  20:50:00  rr2b
 * disclaimerization
 *
 * Revision 1.30  1992/12/14  03:02:50  wjh
 * fix search failure where the target spans the gap
 *
 * .
 *
 * Revision 1.29  1992/11/26  02:42:25  wjh
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
 * Revision 1.28  92/11/15  13:39:42  wjh
 * fix the DeltaSearch algorithm to do subscripting with unsigned chars
 * this fixes the problem in dict.n and probably xmas.d as well
 * 
 * Revision 1.27  91/09/12  16:27:04  bobg
 * Update copyright notice and rcsid
 * 
 * Revision 1.26  1991/02/08  14:37:53  wjh
 * clearstyles now does an additional step when clearing the styles from an entire text: it deletes all style definitions which have been added and are not part of a template.
 * The effect is that writefile of text that has been processed with clearstyles will not have ATK formatting.
 *
 * Revision 1.25  91/01/31  22:22:25  wjh
 * fixed a bug in addstyles wherein if the pattern had multiple styles the innermost was applied as many times as the depth and others were not applied at all
 * 
 * Revision 1.24  91/01/14  17:19:18  wjh
 * updated for changes to styles
 * This fixes the problem that 'extend' did not work.
 * The problem was that short strings were treated as having styles.
 * Also added line spacing, tabs, and color to MergeStyles
 * and fixed AddStyles to not put on styles that are already shared
 * (by pointer) between target and source.
 * 
 * Revision 1.23  90/12/04  14:58:37  wjh
 * corrected search() to check for empty pattern argument
 * 
 * Revision 1.22  90/11/30  15:42:01  wjh
 * fixed a bug that caused crash in search.c on pmaxen
 * 
 * Revision 1.21  90/11/28  15:23:54  wjh
 * fixed a bug in the search() function which was crashing it during present.n
 * 
 * Revision 1.20  90/10/12  21:16:24  wjh
 * added regSearch and regSearchReverse
 * 	made search() faster
 * 
 * 
 * Revision 1.19  90/09/16  20:15:35  wjh
 * see ness/objects/changes.sept.90
 * 
 * Revision 1.18  90/07/28  17:53:29  wjh
 * added functions searchforstyle, definestyle, and addstylebyname
 * fixed bugs in addstyles
 * 
 * Revision 1.17  90/07/15  15:24:24  wjh
 * call.c:  fix a coreleak in class()
 * ness.ch: defined CURRENTMODIFICATIONLEVEL
 * nessruna.c: set version number as 
 * 	CURRENTSYNTAXLEVEL.CURRENTMODIFICATIONLEVEL
 * nessruna.c: add -f switch to cause a fork after compile and before exec
 * Imakefile:  switch to using nessrunapp instead of nessrun.ci
 * nessfunc.d: document the new proctable entries in framecmd.c
 * nessauth.d: describe usage of the framecmd stuff.
 * nesssauth.d: describe --$syntaxlevel
 * nessauth.d: give example of a long string
 * 
 * search.c: redo nextstylegroup
 * search.c: addstyles will now complain if the argument is a constant
 * gen.c: fix so it will not crash after allocating all the initial sysmarks
 * 		that is: it won't crash after a few compiles
 * search.c: revise HasStyles to check for name equality as well as style location equality
 * search.c: add nextstylesegment() which gives the segment from end of subject to next style change
 * search.c: fix enclosingstyle, nextstylegroup, and nextstylesegment to know about 
 * 	the bogus length values used in the outermost environment
 * 
 * 
 * Revision 1.16  89/09/20  23:50:43  wjh
 * fix firstobject for happybday.d
 * 
 * Revision 1.15  89/09/17  08:56:29  wjh
 * fix span function so it doesn't fail at end of text
 * 
 * Revision 1.14  89/09/03  22:49:04  wjh
 * newness
 * 
 * Revision 1.13  89/06/23  17:23:47  wjh
 * (Items that should not be sent to downstairs are marked "R4".)
 * 
 * Added a call to CheckForInterrupt in the function calls and internal gotos.  Thus loops can be terminated with ^G.   (interp.c)  R4
 * 
 * Changed dokeys() so it uses im_DoKeySequence.  This means it is now possible to send a key sequence which involves the message line.  (interp.c)  R4
 * 
 * Implemented DoMenu(object, menustring).   This function causes the same behavior as if the user selected the menu option.  At present the menu string must be exactly as originally defined;  see the warning above for im_DoMenu().  (interp.c, call.c)  R4
 * 
 * Changed access to Ness library routines so they are always compiled.  (They used ot get the default of NoCompilation, so they were useless.)  (call.c)  
 * 
 * Removed a superflous {} pair.  {This is purely a cosmetic change.}  (nessmark.c) 
 * 
 * Fixed value_GetStringValue.  Formerly it was getting an invalid initial value.  {The fix was done by adding a call to nessmark_Initialize() in the stackString section.}  (nevent.c) 
 * 
 * Modified the data stream so errors will not occur when a ness object is the outermost object.  The fix was to add two bytes, "00", at the end of the origin string to prevent the former occurence of a spurious "\}".  (ness.c) 
 * 
 * Fixed menu handling so Ness and child menus get posted when there is a mouse click in an inset within the Ness.  Formerly neither set of menus was posted. (nessv.c) 
 * 
 * Fixed dostmt, the function called from ness-load, which is recommended to be bound to ESC-ESC.  It was using a NULL pointer, so people were getting core dumps if they typed ESC-ESC before doing a compile.  (ness.c) 
 * 
 * Avoided an infinite loop which occurred if a library function referred to a non-existent entry point within itself.  Did this by checking to see if the library function is already Compiling just before trying to compile it.  (call.c call.hn, call.h) 
 * 
 * Revised system marker allocation so the compilation will not get a subsequent error.  (gen.c)
 * 
 * Revised system marker allocation so it expands the space available if necessary. This will make it possible to compile larger programs.  (gen.c)
 * 
 * Changed the type of TType to long from struct object *.  This will allow compilation on stricter compilers.  (interp.h) 
 * 
 * Fixed nessmark_FinalizeObject so it would not reference a NULL pointer.  {Somehow the assembler noticed this bug!}  (nessmark.c) 
 * 
 * Changed functions which deal with constant strings to have (char *) as there argument type (SaveError, exprnode_Create, ReportError, ExprError, RunError, LocateErrorFunc, QueryReadOnly, makeConst, printallerrors) or return type (Freeze, argcounterr, argtypeerr).  This prevents compile errors on picky compilers.  (interp.c, error.c, call.c, ness.c, nessv.c, search.c, nevent.c, nessmark.c, nessrun.ci)  R4 
 * 
 * Changed Imakefile to store Imakefile in checkin rule.  (Imakefile)
 * 
 * 
 * Revision 1.12  89/06/01  16:02:02  wjh
 * campus release version
 * 
 * Revision 1.5  88/12/20  19:47:13  wjh
 * fixed various bugs
 * 
 * Revision 1.4  88/12/08  15:53:08  wjh
 * fix a bug in HasStyles()
 * 
 * Revision 1.3  88/12/07  22:44:51  wjh
 * 
 * 9 Nov
 * implemented access level control
 * skip first line of script if starts with # (for shell script)
 * changed so the name of all initialization functions is init()
 * added ness-load
 * moved execution of init() from compilation to first execution
 * deferred compilation to first FullUpdate time
 * 
 * 22 Nov
 * proc table calls now work correctly with type-free procs  (the first arg can be anything)
 * added "cheat_" functions which will remain undocumented
 * changed inset() to a function
 * fixed some bugs in initial access
 * 
 * 25 November
 * added long strings
 * added Next Error menu option
 * made replace() work correctly in all cases
 * added class() and new()
 * 
 * 29 Nov
 * added ^<upper-case> and \e as characters in strings
 * added nextn() and length()
 * 
 * 6 Dec
 * added functions: parseint(), parsereal(), firstobject(), whereitwas(), replacewithobject(), addstyles(), nextstylegroup(), enclosingstylegroup(), clearstyles(), hasstyles()
 * catch bus and segmentation errors
 * 
 * 
 * Revision 1.2  88/11/02  14:43:07  wjh
 * fixed bugs with byte swapping
 * added some more corrected copyrights
 * 
 * Revision 1.1  88/10/21  11:02:53  wjh
 * Initial revision
 * 
 * Creation 0.0  88/06/01 10:02:00  wjh
 * Initial creation by WJHansen
 * 
*/

#include <andrewos.h>	/* for bzero() bcmp() */
#include <ctype.h>
#include <dataobj.ih>
#include <text.ih>
#include <smpltext.ih>
#include <textv.ih>
#include <viewref.ih>
#include <dict.ih>
#include <envrment.ih>
#include <stylesht.ih>
#include <style.ih>
#include <cel.ih>
#include <search.ih>

#include <lexan.ih>
#include <nessmark.ih>
#include <interp.h>		/* for frameelt for envt.h */
#include <envt.h>
#include <compdefs.h>
#include <nevent.h>	/* for ProperPtr */

/* XXX  KLUDGE.  The following definition should be exported by text.ch */
#define TEXT_VIEWREFCHAR  '\377'

#define PTSTOMARK(arg,iar)  ((TType)((struct nessmark *)arg)->header.nessmark_methods \
		== nessmarkHdr) ? TRUE :   \
		RunError(":not a pointer to a mark (uninitialized variable?)", iar);


/* boo hiss Sun chars */
#define UNSIGN(c) ((c) & 0xFF)


/* The list of styles created with DefineStyle is maintained 
	so it can be used for addstylebyname */
static struct stylist {
	struct style *s;
	struct stylist *next;
} *DefinedStyles = NULL;


/* conventions for search-type functions:
	the subject in which the search is made is the first argument
	if this is an empty string, the search extends to the end of the base
		otherwise the search extends only over the argument string
	For success the return value is the non-empty substring matching the 
		criteria.
	For failure, the return value is the empty string at the end of the 
		original subject string.  (If the original subject were empty
		the return value for failure is that original subject.)
*/


/* the string location of most recent ParseInt, ParseReal, or FirstObject */
static struct nessmark *WhereItWas = NULL;


static boolean CharTable[256];	/* assumed initially zero */
	/* the CharTable is used to test for characters in sets */

/* SetCharTable(pat)
	Set to TRUE each element of CharTable that is in pat.
*/
	static void
SetCharTable(pat)
	struct nessmark *pat;
{
	register struct simpletext *ptext;
	register long pos, end;
	ptext = nessmark_GetText(pat);
	pos = nessmark_GetPos(pat);
	end = pos + nessmark_GetLength(pat);
	for ( ; pos < end; pos++)
		CharTable[simpletext_GetUnsignedChar(ptext, pos)] = TRUE;
}

/* ClearCharTable(pat)
	Set to FALSE each element of CharTable.
	As an optimization, the 'pat' arg must list all elts of table which are TRUE.
*/
	static void
ClearCharTable(pat)
	struct nessmark *pat;
{
	register struct simpletext *ptext;
	register long pos, end, len;
	len = nessmark_GetLength(pat);
	if (len > 20) {
		bzero(CharTable, sizeof(CharTable));
		return;
	}
	ptext = nessmark_GetText(pat);
	pos = nessmark_GetPos(pat);
	end = pos + len;
	for ( ; pos < end; pos++)
		CharTable[simpletext_GetUnsignedChar(ptext, pos)] = FALSE;
}

#ifdef nodef
/* old search algorithm.  slow, but sure */
	void
search(subject, pat)
	struct nessmark *subject, *pat;
{
	register unsigned char first;
	register struct simpletext *stext, *ptext;
	register long pos, end, patpos, patlen;
	long spos, slen;

	ptext = nessmark_GetText(pat);
	patpos = nessmark_GetPos(pat);
	patlen = nessmark_GetLength(pat);
	stext = nessmark_GetText(subject);
	spos = nessmark_GetPos(subject);
	slen = nessmark_GetLength(subject);
	end = ((slen == 0) ? simpletext_GetLength(stext) : spos + slen)
			- patlen;

	first = simpletext_GetUnsignedChar(ptext, patpos);

	for (pos = spos; pos <= end; pos++)  
		if (first == simpletext_GetUnsignedChar(stext, pos)) {
			/* found first char. try to match rest of pat */
			register long t;
			for (t = 1;  t < patlen && 
					    simpletext_GetUnsignedChar(stext, pos+t)
					    == simpletext_GetUnsignedChar(ptext, patpos+t); 
					t++)
				{}
			if (t == patlen) {
				/* bingo */
				nessmark_SetPos(subject, pos);
				nessmark_SetLength(subject, patlen);
				return;
			}
		}
	/* fail: return empty mark at end of subject */
	nessmark_SetPos(subject, spos + slen);
	nessmark_SetLength(subject, 0);
}
#else /* notdef */


/* buffers global to the search algorithms */
static char *patBuf, *bufA, *bufB;
static long patlen, bufAlen, bufBlen;

/* BufMatch(offset)
	check the concatenation of
	bufA (length bufAlen) and bufB (length bufBlen)
	to see if patBuf (length patlen) matches the portion beginning
	at 'offset'
	return TRUE for a match and FALSE otherwise
 */
	static boolean
BufMatch(offset)
	register long offset;
{
	register long Apart;	/* how much is in A */
	if (offset >= bufAlen) 
		/* entirely in B */
		return (bcmp(patBuf, bufB + offset - bufAlen, patlen) == 0);
	else if (offset + patlen > bufAlen) {
		/* spans the boundary */
		Apart = bufAlen - offset;
		return (bcmp(patBuf, bufA + offset, Apart) == 0
			&& bcmp(patBuf+Apart, bufB, patlen - Apart) == 0);
	}
	else {
		/* entirely in A */
		boolean val = (bcmp(patBuf, bufA+offset, patlen) == 0);
		return val;
	}
}


/* SimpleSearch()
	look for patBuf (length patlen) in the concatenation of
	bufA (length bufAlen) and bufB (length bufBlen)
	return the offset of the match from the start of bufA
	return -1 if not found

	This code is the same as Delta search except:
	a) It does not declare or use the delta array, and
	b) The increment in the final loops is 1.
*/
	static long
SimpleSearch()
{
	register char *cx, *curend,  target;
	register long i, toff;
	
	/* compute target character:  first low probability character in pat
		the high probability characters are deemed to be
		space, newline, e, t, a, i, o, n, r, s, l, c  
		(only about 6% of all short words consist entirely of these characters) */
	curend = patBuf + patlen;
	target = *(curend-1);	/* use last char of pat if none other */
	toff = patlen - 1;
	for (cx = patBuf; cx < curend; cx++) 
		switch(*cx) {
		case ' ': case '\n': case 'e': case 't': case 'a': case 'i': 
		case 'o': case 'n': case 'r': case 's': case 'l': case 'c':
			break;
		default: 
			target = *cx;
			toff = cx - patBuf;
			cx = curend;	/* break the for-loop */
			break;		/* break the switch */
		}

	/* the value of cx in the algorithm refers to the position
		scanned for the target character.  The pattern is
		being matched against position cx-toff */

	/* search starting in bufA */
	cx = bufA + toff;
	curend = bufA + bufAlen;
	if (bufBlen < patlen-toff)
		curend -= patlen-toff-bufBlen - 1;
	while (cx < curend) {
		if (target == *cx) {
			i = cx - bufA - toff;
			if (BufMatch(i))
				return i;
		}
		cx ++;
	}
	if (bufBlen == 0) return -1;
	/* search starting in bufB  (continue cx from above) */
	cx = bufB + (cx-bufA) - bufAlen;
	curend = bufB + bufBlen - patlen + 1 + toff;
	while (cx < curend) {
		if (target == *cx) {
			i =  cx - bufB + bufAlen - toff;
			if (BufMatch(i))
				return i;
		}
		cx ++;
	}
	/* not found */
	return -1;
}


static short freq[256] = {
	/* ctl chars */	9,9,9,9,9,9,9,9,9,29,9,9,9,35,9,9,
	/* ctl chars */	9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,
	/* punctuation */	950,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,
	/* digits */		30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30, 
	/* uppercase */	11,88,82,84,83,91,81,82,82,87,12,81,85,83,86,86,
	/* uppercase */	23,12,87,85,87,83,81,81,8,82,12,11,11,11,11,11,
	/* lowercase */	11,889,823,845,832,911,815,824,829,878,82,811,855,832,868,869,
	/* lowercase */	431,82,874,856,871,836,810,811,83,820,82,11,11,11,11,11,
	1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
	3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,
	3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,
	3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,
};


/* DeltaSearch()
	look for patBuf (length patlen) in the concatenation of
	bufA (length bufAlen) and bufB (length bufBlen)
	return the offset of the match from the start of bufA
	return -1 if not found
	Use a fast delta search algorithm
*/
	static long
DeltaSearch()
{
	long delta[256];
	register char *cx, *curend, target;
	register long i, toff;

	/* for each character of pattern, compute a delta value
		such that if the pattern is aligned at cx-toff
		it should next align at
		cx-toff+delta[*(cx+patlen)]
		If there are multiple instances of a character, 
		use the rightmost because it advances cx the least
		During the scan, find as target the character
		with lowest frequency.
	*/
	toff = patlen + 1;	/* delta for chars not in pat  (toff is just a convenient register) */
	for (i = 0; i < 64; i++)
		delta[i] = toff;
	bcopy(&delta[0], &delta[64], 64*sizeof(long));
	bcopy(&delta[0], &delta[128], 128*sizeof(long));

	curend = patBuf + patlen;
	target = *(curend-1);	/* use last char of pat if none other */
	toff = patlen - 1;
	i = freq[UNSIGN(target)];

	for (cx = patBuf; cx < curend; cx++) {
		delta[UNSIGN(*cx)] = curend - cx;
		if (freq[UNSIGN(*cx)] < i) {
			target = *cx;
			toff = cx - patBuf;
			i = freq[UNSIGN(target)];
		}
	}

	/* the value of cx in the algorithm refers to the position
		scanned for the target character.  The pattern is
		being matched against position cx-toff */

	/* search starting in bufA */
	cx = bufA + toff;
		/* terminate early so *(cx-toff+patlen) is valid */
	curend = bufA + bufAlen - patlen + toff;
	while (cx < curend) {
		if (target == *cx) {
			i = cx - toff - bufA;
			if (BufMatch(i))
				return i;
		}
		/* advance according to delta for the char 
		   next beyond where we are currently matching the pattern */
		cx += delta[UNSIGN(*(cx-toff+patlen))]; 
	}

	/* search in the last little bit of bufA  (continue cx) */
	curend = bufA + bufAlen; /* one past last possible starting place */
	if (bufBlen < patlen-toff)
		curend -= patlen-toff-bufBlen - 1;
	while (cx < curend) {
		if (target == *cx) {
			i = cx - bufA - toff;
			if (BufMatch(i))
				return i;
		}
		cx ++;
	}

	/* search starting in bufB */
	if (bufBlen == 0) return -1;
	cx = bufB;
	curend = bufB + bufBlen - patlen + 1 + toff;
	while (cx < curend) {
		if (target == *cx) {
			i =  cx - toff - bufB + bufAlen;
			if (BufMatch(i))
				return i;
		}
		cx += delta[UNSIGN(*(cx-toff+patlen))];
	}
	/* not found */
	return -1;
}

	void
search(subject, pat)
	struct nessmark *subject, *pat;
{
	struct simpletext *stext, *ptext;
	long patpos, spos, slen, failpos; /* (patlen is global) */
	boolean needToFreePat;
	long tlen;

	ptext = nessmark_GetText(pat);
	patpos = nessmark_GetPos(pat);
	patlen = nessmark_GetLength(pat);

	patBuf = simpletext_GetBuf(ptext, patpos, patlen, &tlen);
	if (tlen < patlen) {
		/* crosses boundary, make a copy of pattern */
		bufA = patBuf;
		patBuf = malloc(patlen);
		strncpy(patBuf, bufA, tlen);
		bufB = simpletext_GetBuf(ptext, patpos+tlen, patlen-tlen, &bufBlen);
		if (bufBlen < patlen - tlen) 
			fprintf(stderr, "ness search: pattern buffer failure\n");
		strncpy(patBuf+tlen, bufB, patlen - tlen);
		needToFreePat = TRUE;
	}
	else
		needToFreePat = FALSE;

	stext = nessmark_GetText(subject);
	spos = nessmark_GetPos(subject);
	slen = nessmark_GetLength(subject);
	failpos = spos + slen;
	if (patlen == 0) 
		slen = 0;	/* don't search */
	else if (slen == 0) 
		slen = simpletext_GetLength(stext) - spos;

	bufA = simpletext_GetBuf(stext, spos, slen, &bufAlen);
	if (bufAlen < slen) {
		/* we need the other buffer, too */
		bufB = simpletext_GetBuf(stext, spos+bufAlen, 
				slen-bufAlen, &bufBlen);
		if (bufAlen + bufBlen < slen)
			fprintf(stderr, "Ness search: subject buffer failure\n");
	}
	else bufBlen = 0;

	if (slen == 0)
		patpos = -1;
	else if (slen < 200 || patlen == 1) 
		/* don't build table, use a simple sequential search */
		patpos = SimpleSearch();
	else 
		/* use delta search with table */
		patpos = DeltaSearch();
	if (patpos < 0) {
		/* fail: return empty mark at end of subject */
		nessmark_SetPos(subject, failpos);
		nessmark_SetLength(subject, 0);
	}
	else {
		/* bingo */
		nessmark_SetPos(subject, spos + patpos);
		nessmark_SetLength(subject, patlen);
	}
	if (needToFreePat) free(patBuf);
}


#endif /* notdef */


/* match(subject, pat)
	checks subject to see if it begins with pat.  
	If so, returns that substring;  otherwise returns finish(subject)
	The match will fail unless the subject is of length zero or some 
		length longer than the pattern.
*/
	void
match(subject, pat)
	struct nessmark *subject, *pat;
{
	register struct simpletext *stext, *ptext;
	register long pos, end, patpos, patend;
	long spos, slen;

	stext = nessmark_GetText(subject);
	spos = nessmark_GetPos(subject);
	slen = nessmark_GetLength(subject);
	end = (slen == 0) ? simpletext_GetLength(stext) : spos+slen;
	ptext = nessmark_GetText(pat);
	patpos = nessmark_GetPos(pat);
	patend = patpos + nessmark_GetLength(pat);

	pos = spos;
	if (end-pos >= patend-patpos)
		while (patpos < patend 
				&& simpletext_GetUnsignedChar(stext, pos)
				== simpletext_GetUnsignedChar(ptext, patpos))
			pos++, patpos++;
	if (patpos == patend && nessmark_GetLength(pat) > 0) {
		/* succeed */
		nessmark_SetLength(subject, nessmark_GetLength(pat));
	}
	else {
		/* fail */
		nessmark_SetPos(subject, spos + slen);
		nessmark_SetLength(subject, 0);
	}
}

	void
anyof(subject, pat)
	struct nessmark *subject, *pat;
{
	register struct simpletext *stext;
	register long pos, end;
	long slen, spos;

	stext = nessmark_GetText(subject);
	spos = nessmark_GetPos(subject);
	slen = nessmark_GetLength(subject);
	end = (slen == 0) ? simpletext_GetLength(stext) : spos + slen;

	SetCharTable(pat);

	for (pos = spos; pos < end; pos++)
		if (CharTable[simpletext_GetUnsignedChar(stext, pos)]) 
			break;
	if (pos < end) {
		/* bingo */
		nessmark_SetPos(subject, pos);
		nessmark_SetLength(subject, 1);
	}
	else {
		/* nope */
		nessmark_SetPos(subject, spos + slen);
		nessmark_SetLength(subject, 0);
	}

	ClearCharTable(pat);
}

	void
span(subject, pat)
	struct nessmark *subject, *pat;
{
	register struct simpletext *stext;
	register long pos, end;
	long spos, slen;

	stext = nessmark_GetText(subject);
	spos = nessmark_GetPos(subject);
	slen = nessmark_GetLength(subject);
	end = (slen == 0) ? simpletext_GetLength(stext) : spos + slen;

	SetCharTable(pat);

	for (pos = spos; pos < end; pos++)
		if ( ! CharTable[simpletext_GetUnsignedChar(stext, pos)]) 
			break;

	if (pos > spos)
		nessmark_SetLength(subject, pos - spos);
	else {
		nessmark_SetPos(subject, spos + slen);
		nessmark_SetLength(subject, 0);
	}
	
	ClearCharTable(pat);
}

	void
token(subject, pat)
	register struct nessmark *subject, *pat;
{
	register struct simpletext *stext;
	long slen, spos;
	stext = nessmark_GetText(subject);
	spos = nessmark_GetPos(subject);
	slen = nessmark_GetLength(subject);

	anyof(subject, pat);
	if (nessmark_GetLength(subject) == 0)
		return;

	/* revise subject to extend to the end of the base 
			or the end of the original subject */
	nessmark_SetLength(subject, 
		((slen == 0) ? simpletext_GetLength(stext) : spos + slen)
			- nessmark_GetPos(subject));
	span(subject, pat);
}

#undef environment_GetParent
#define environment_GetParent(env) \
		((struct environment *)(env)->header.nestedmark.parent)


/* AddOneStyle(text, pos, len, style)
	Adds to text at pos for length len teh given style.
*/
	static void
AddOneStyle(text, pos, len, style)
	struct text *text;
	long pos, len;
	struct style *style;
{
	struct style *newstyle;
	if (style == NULL) return;
	newstyle = stylesheet_Find(text->styleSheet, style_GetName(style));
	if (newstyle == NULL)  {
		/* XXX Why doesn't text_AddStyle do this ??? */
		/* XXX How do we decide we should use default.template */
		newstyle = style_New();
		style_Copy(style, newstyle);	
		newstyle->template = FALSE;
		stylesheet_Add(text->styleSheet, newstyle); 
	}
	text_AddStyle(text, pos, len, newstyle);
}

/* AddStyles(text, pos, len, env)
	Adds to text at pos for length len all the styles had by env,
	except not those from environments that are already shared by both.
*/
	static void
AddStyles(text, pos, len, env)
	struct text *text;
	long pos, len;
	struct environment *env;
{
	struct environment *subjenv, *tenv;
	boolean notify = FALSE;

	subjenv = environment_GetEnclosing(text->rootEnvironment, pos);
	if (len > 0)
		subjenv = environment_GetCommonParent(subjenv,
			environment_GetEnclosing(text->rootEnvironment, pos+len-1));
	for (tenv = env; tenv != NULL; tenv = environment_GetParent(tenv)) {
		if (tenv->type != environment_Style) continue;
		/* if tenv is (identically) subjenv or an ancestor, break */
		if (environment_Distance(subjenv, tenv) >= 0) break;
		AddOneStyle(text, pos, len, tenv->data.style);
		notify = TRUE;
	}
	if (notify) text_NotifyObservers(text, 0);
}


/* HasStyle(env, penv)
	checks to see if env or any of its parents has the style penv 
	Returns TRUE if so and FALSE if not
*/
	static boolean
HasStyle(env, penv)
	struct environment *env, *penv;
{
	char *pname, *name;
	enum nametype {None, Name, Menu} nt = None;
	if (penv->data.style == NULL) return TRUE;
	pname = style_GetName(penv->data.style);
	if (pname != NULL) nt = Name;
	else {
		pname = style_GetMenuName(penv->data.style);
		if (pname != NULL) nt = Menu;
	}

	for ( ; env != NULL; env = environment_GetParent(env))  {
		if (env->type != environment_Style  ||  env->data.style == NULL) 
			continue;
		if (env->data.style == penv->data.style)
			return TRUE;
		switch (nt) {
		    case None:  name = NULL;   break;
		    case Name:  name = style_GetName(env->data.style);   break;
		    case Menu:  name = style_GetMenuName(env->data.style);   break;
		}
		if (name != NULL && strcmp(name, pname) == 0)
			return TRUE;
		/* XXX could go on to test style attributes YUCHH */
	}
	return FALSE;   /* not found */
}

/* HasAllStyles(env, penv)
	checks to see if env has all styles that are around penv
	return bolean value
	return TRUE unless we find a style on penv that is not on env
*/
	static boolean
HasAllStyles(env, penv)
	struct environment *env, *penv;
{
	for ( ; penv != NULL;  penv = environment_GetParent(penv)) 
		if (penv->type == environment_Style && ! HasStyle(env, penv)) 
			/*  could not find a pat style */
			return FALSE;
	return TRUE;
}


/* AdjustOperandValue(s, p, isConst)
	KLUDGE
	s and p point to struct marginstyle or struct fontscriptstyle, BOTH
		OF WHICH HAVE THE SAME FORMAT (right now)
	The value of s is adjusted per that of p.
	If isConst is TRUE, the value of s is set to that of p,
	otherwise {the value of s is incremented by that of p,
		the Units are reset to RawDots,
		and the DotCvtOperand is used as the Operand value.}

	It would be preferable to do this is a cleaner fashion, 
	but there is no good way to add Operands if Units differ.
*/
AdjustOperandValue(s, p, isConst)
	struct marginstyle *s, *p;
	boolean isConst;
{
	if (isConst) {
		/* just change to the size set by p */
		*s = *p;
	}
	else if (p->DotCvtOperand != 0) {
		/* p makes a relative change to s */
		s->DotCvtOperand += p->DotCvtOperand;
		s->MarginUnit = style_RawDots;
		s->Operand = s->DotCvtOperand;
	}
}


/* MergeStyles(style, env)
	merges all styles from the env and its parents into style
	returns style as modified
	Recurses to merge parent styles first so pat itself will override its parents
		This would be better if style.c had a method to apply
		one style to another.
		As it is, we process only some of the attributes.  
		The style attributes  
			LeftMargin,   RightMargin,  Indent,  Script, and FontSize
		are treated incrementally.  The attributes 
			Justification,  Flags,  FontFamily,  FontFace,
			Line Spacing, Paragraph Spacing, tabs, and color
		are treated as new values.
*/
	static struct style *
MergeStyles(style, env)
	struct style *style;
	struct environment *env;
{  
	struct environment *penv;
	struct style *patstyle;

	enum style_FontSize SizeBasis;
	long Operand, newOperand;
	enum style_Justification Justification;
	long mask;

	enum style_Unit Unit;
	enum style_SpacingValue Basis;
	char *color;
	struct tabentry **tabchanges;
	long n;

	penv = environment_GetParent(env);
	if (penv != NULL)
		/* recur to do parent styles */
		style = MergeStyles(style, penv);

	patstyle = env->data.style;
	if (env->type != environment_Style  ||  patstyle == NULL) 
		return style;

	/* LeftMargin */
		AdjustOperandValue(&style->NewLeftMargin,
			&patstyle->NewLeftMargin,
			patstyle->NewLeftMargin.MarginBasis ==
				style_ConstantMargin);
	/* RightMargin */
		AdjustOperandValue(&style->NewRightMargin,
			&patstyle->NewRightMargin,
			patstyle->NewRightMargin.MarginBasis ==
				style_ConstantMargin);
	/* Indent */
		AdjustOperandValue(&style->NewIndentation,
			&patstyle->NewIndentation,
			patstyle->NewIndentation.MarginBasis ==
				style_ConstantMargin);
	/* Script */
		AdjustOperandValue((struct marginstyle *)&style->FontScript,
			(struct marginstyle *)&patstyle->FontScript,
			patstyle->FontScript.ScriptBasis ==
				style_ConstantScriptMovement);
	/* FontSize  */
		style_GetFontSize(patstyle, &SizeBasis, &Operand);
		if (SizeBasis == style_ConstantFontSize)
			style_SetFontSize(style, SizeBasis, Operand);
		else if (SizeBasis == style_PreviousFontSize) {
			style_GetFontSize(style, &SizeBasis, &newOperand);
			style_SetFontSize(style, SizeBasis,
				newOperand+Operand);
		}
	/* FontFamily */
		if (patstyle->FontFamily != NULL)
			style_SetFontFamily(style, patstyle->FontFamily);
	/* Justification */
		Justification = style_GetJustification(patstyle);
		if (Justification != style_PreviousJustification)
			style_SetJustification(style, Justification);
	/* Flags */
		mask = patstyle->AddMiscFlags  |  ~ patstyle->OutMiscFlags;
		style->AddMiscFlags = 
			(mask & patstyle->AddMiscFlags)
			| ((~mask) & style->AddMiscFlags);
		style->OutMiscFlags = 
			(mask & patstyle->OutMiscFlags)
			| ((~mask) & style->OutMiscFlags);
	/* FontFace */
		mask = patstyle->AddFontFaces  |  ~ patstyle->OutFontFaces;
		style->AddFontFaces = 
			(mask & patstyle->AddFontFaces)
			| ((~mask) & style->AddFontFaces);
		style->OutFontFaces = 
			(mask & patstyle->OutFontFaces)
			| ((~mask) & style->OutFontFaces);
	/* Spacing (of lines) */
		style_GetNewInterlineSpacing(style, &Basis, &Operand, &Unit);
		if (Basis == style_ConstantSpacing && Unit == style_Points)
			style_SetNewInterlineSpacing(style, Basis, Operand, Unit);
	/* Spread (between paragraphs) */
		style_GetNewInterparagraphSpacing(style, &Basis, &Operand, &Unit);
		if (Basis == style_ConstantSpacing && Unit == style_Points)
			style_SetNewInterparagraphSpacing(style, Basis, Operand, Unit);
	/* Tabs */
		style_GetTabChangeList(patstyle, &n, &tabchanges);
		if (n != 0) {
			style_ClearTabChanges(style);
			while (n-- > 0) 
				style_AddTabChange(style,
					tabchanges[n]->TabOpcode,
					tabchanges[n]->Location,
					tabchanges[n]->LocationUnit);
		}
		if (tabchanges)
			free(tabchanges);
	/* Color */
		color = style_GetAttribute(patstyle, "color");
		if (color != NULL) 
			style_AddAttribute(style, "color", color);
	return style;
}

	static boolean 
DeleteNonTemplateStyles(style, ss)
	struct style *style;
	struct stylesheet *ss;
{
	if ( ! style->template) 
		/* not in template, delete it */
		stylesheet_Delete(ss, style);
	return FALSE;
}

/* SearchOp(op, NSP, opiar)
	First five ops perform searches, depending on op.
	Each search routine modifies its first argument marker to indicate the
		result of searching for the second.
	Other operations parse strings, handle objects, and process styles.
*/
	void
SearchOp(op, opiar)
	unsigned char op;
	unsigned char *opiar;	/* iar of the opcode */
{
	register union stackelement *NSP = NSPstore; 

	/* these definitions are global because GDB can't get at local decls. */

	struct nessmark *pat, *subject;
	boolean boolval;
	long intval;
	double realval;
	char *cstring, *cx;
	struct basicobject *objval;
	long pos, len, finish;
	struct text *text, *pattext;
	struct viewref *vr;
	static boolean Inited = FALSE;
	long success;
	long envpos, envlen;
	struct environment *env, *penv, *tenv;
	struct style *style;
	struct stylist *stelt;
	char menuname[100];
	static struct SearchPattern *regPat;

	if (! Inited) {
		Inited = TRUE;
		WhereItWas = nessmark_New();
	}

	/* check arguments */
	if (op == 'w')	{}		/* no args */
	else {
		/* at least one arg */
		PTSTOMARK(&NSP->m, opiar);
		if (op >= 'p') {
			/* this is both one and three arg ops */
			subject = &NSP->m;
		}
		else {
			/* two marker args */
			PTSTOMARK(&(&(NSP->m))[1], opiar);
			subject = &(&(NSP->m))[1];
			pat = &NSP->m; 
		}
	}
	switch (op) {

	case 'a':  search(subject, pat);  NSP = popValue(NSP);  break;
	case 'b':  match(subject, pat);  NSP = popValue(NSP);  break;
	case 'c':  anyof(subject, pat);  NSP = popValue(NSP);  break;
	case 'd':  span(subject, pat);  NSP = popValue(NSP);  break;
	case 'e':  token(subject, pat);  NSP = popValue(NSP);  break;

	/* {"regSearch", "Ff", {Tstr, Tstr, Tstr, Tend}, ness_codeOrange} */
	case 'f':
		/* forward regular search */
		text = (struct text *)nessmark_GetText(subject);
		pos = nessmark_GetPos(subject);
		len = nessmark_GetLength(subject);
		finish = pos + len;  /* failure location */
		cstring = (char *)nessmark_ToC(pat);
		cx = search_CompilePattern(cstring, &regPat);
		free(cstring);
		/* XXX save pat's cstring and compiled pattern
		test and avoid recompilation (? will this really be faster) */

		if (cx != 0) {
			/* not a valid pattern */
			fprintf(stderr, "Ness regSearch: %s\n", cx);
			nessmark_Set(subject, EmptyText, 0, 0);
		}
		else {
			if (len != 0) {
				/* XXX HACK to delimit the search to the
					argument */
				envlen = text_GetLength(text);
				((struct simpletext *)text)->length = finish;
				pos = search_MatchPattern(text, pos, regPat);
				((struct simpletext *)text)->length = envlen;
			}
			else  
				pos = search_MatchPattern(text, pos, regPat);
				
			if (pos >= 0) {
				/* succeed */
				nessmark_SetPos(subject, pos);
				nessmark_SetLength(subject,
					search_GetMatchLength());
			}
			else {
				/* fail */
				nessmark_SetPos(subject, finish);
				nessmark_SetLength(subject, 0);
			}
		}

		NSP = popValue(NSP);  /* discard pat, leaving revised subject as result */
		break;

	/* {"regSearchReverse", "Fg", {Tstr, Tstr, Tstr, Tend}, ness_codeOrange}, */
	case 'g':
		/* reverse regular search */
		text = (struct text *)nessmark_GetText(subject);
		pos = nessmark_GetPos(subject);
		len = nessmark_GetLength(subject);
		finish = pos + len;  /* failure location */
		cstring = (char *)nessmark_ToC(pat);
		cx = search_CompilePattern(cstring, &regPat);
		free(cstring);
		/* XXX save cstring and test to avoid recompilation (? is this really faster) */
		if (cx != 0) {
			/* not a valid pattern */
			fprintf(stderr, "Ness regSearchReverse: %s\n", cx);
			nessmark_Set(subject, EmptyText, 0, 0);
		}
		else {
			envpos = search_MatchPatternReverse(text, finish, regPat);
				
			if (envpos >= 0 && (len == 0 || envpos >= pos)) {
				/* succeed */
				nessmark_SetPos(subject, envpos);
				nessmark_SetLength(subject,
					search_GetMatchLength());
			}
			else {
				/* fail */
				nessmark_SetPos(subject, finish);
				nessmark_SetLength(subject, 0);
			}
		}


		NSP = popValue(NSP);  /* discard pat, leaving revised subject as result */
		break;

	/*  {"searchforstyle", "Fh", {Tstr, Tstr, Tstr, Tend}, ness_codeOrange},*/
	case 'h':
		pattext = (struct text *)nessmark_GetText(pat);
		if ( ! class_IsType(pattext, textClass))
			goto nosuchstyle;	/*  no styles in 'pat' */
		penv = environment_GetInnerMost(pattext->rootEnvironment,
				nessmark_GetPos(pat));

		text = (struct text *)nessmark_GetText(subject);
		if ( ! class_IsType(text, textClass)) 
			goto nosuchstyle;	/*  subject has no styles */

		pos = nessmark_GetPos(subject);
		finish = pos + nessmark_GetLength(subject);
		if (finish == pos)
			finish = text_GetLength(text);
			
		do {
			/* loop through each change position and see if 
				the required style is at that point */
			env = environment_GetInnerMost(text->rootEnvironment,
				pos);
			if (HasAllStyles(env, penv))
				break;
			pos += environment_GetNextChange(text->rootEnvironment,
				pos);
			if (pos >= finish)
				goto nosuchstyle;
		}  while ( TRUE);
		nessmark_SetPos(subject, pos);	/* the start of the style */

		do {
			/* continue looping through each change position 
				until we find a place that doesn't have the style */
			pos += environment_GetNextChange(text->rootEnvironment,
				pos);
			if (pos >= finish) {
				/* style extends beyond finish */
				pos = finish;
				break;
			}
			env = environment_GetInnerMost(text->rootEnvironment,
				pos);
		}  while (HasAllStyles(env, penv));
		nessmark_SetLength(subject, pos - nessmark_GetPos(subject));

		/* succeed.  The Pos and Length of subject have been set */
		NSP = popValue(NSP);	/* discard style marker */
		break;

	nosuchstyle:
		/* fail, return finish(subject) */
		nessmark_Next(subject);
		nessmark_Start(subject);
		NSP = popValue(NSP);	/* discard style marker */
		break;

	/*  {"definestyle", "zFi", {Tvoid, Tstr, Tstr, Tend}, ness_codeOrange},*/
	/* (The arguments are swapped by the z operator.)
	    'pat' is style name.  'subject' is text with desired style set 
	    return value is a copy of the stylename text, but with the defined style
	*/
	case 'i':
		/* create a style and merge into it the styles from the source */
		style = style_New();
		text = (struct text *)nessmark_GetText(subject);
		if (class_IsType(text, textClass)) {
			env = environment_GetInnerMost(text->rootEnvironment,
			nessmark_GetPos(subject));
			style = MergeStyles(style, env);
		}

		cstring = (char *)nessmark_ToC(pat);	/* style name basis */

		/* set menuname and cleaned up name 
			if name has a comma, use name as menuname
				and then discard the part before the comma
			otherwise construct a name putting it on "Style" menu card
			the name itself is cleaned by terminating at
				first non-alphameric and
				changing uppercase to lower 
		*/
		if (strlen(cstring) > 80)
			cstring[80] = '\0';	/* truncate to 80 characters */
		cx = index(cstring, ',');
		if (cx != NULL) {
			style_SetMenuName(style, cstring);
			strcpy(cstring, cx+1);		/* discard prefix */
		}
		else {
			if (islower(*cstring)) *cstring = toupper(*cstring);
			sprintf(menuname, "Style~8,%s", cstring);
			style_SetMenuName(style, menuname);
		}
		
		len = 0;
		for (cx = cstring; *cx != '\0'; cx++) {
			if ( ! isalnum(*cx))    {*cx = '\0'; break;}
			len++;
			if (isupper(*cx))   *cx = tolower(*cx);
		}
		style_SetName(style, cstring);

		/* Add to DefinedStyles list */
		for (stelt = DefinedStyles; stelt != NULL; stelt = stelt->next)
			if (strcmp(style_GetName(stelt->s), cstring) == 0) 
				break;
		if (stelt == NULL) {
			/* create a new element in DefinedStyles list */
			stelt = (struct stylist *)malloc(sizeof(struct stylist));
			stelt->next = DefinedStyles;
			DefinedStyles = stelt;
		}
		stelt->s = style;
			/* CORELEAK:  styles are never discarded (can't be) */

		/* replace subject marker with one for result */
		/* give the new style to the result */
		text = text_New();
		text_InsertCharacters(text, 0, cstring, len);
		AddOneStyle(text, 0, len, style);
		nessmark_Set(subject, text, 0, len);

		free(cstring);
		NSP = popValue(NSP);  	/* discard topmost arg  (i.e., pat) */

		break;

	/* {"addstyles", "Fj", {Tstr, Tstr, Tstr, Tvoid}, ness_codeYellow} */
	case 'j':
		/* revise subject to have style of pat */
		text = (struct text *)nessmark_GetText(subject);
		if (text_GetReadOnly(text))
			RunError(":Tried to change styles on a constant", opiar);
		pattext = (struct text *)nessmark_GetText(pat); 		
		if (class_IsType(pattext, textClass) 
				&& class_IsType(text, textClass))
			AddStyles(text, nessmark_GetPos(subject),
				nessmark_GetLength(subject), 				environment_GetInnerMost(
						pattext->rootEnvironment,
						nessmark_GetPos(pat)));
		NSP = popValue(NSP);  /* discard pat, leaving revised subject as result */
		break;

	/* {"hasstyles", "Fk", {Tbool,Tstr,Tstr,Tvoid}, ness_codeOrange} */
	case 'k':
		/* set boolval indicating whether subject has style of pat */
		boolval = TRUE;

		pattext = (struct text *)nessmark_GetText(pat);
		if ( ! class_IsType(pattext, textClass))
			goto gotval;	/*  no styles: trivially TRUE */
		pos = nessmark_GetPos(pat);
		penv = environment_GetInnerMost(pattext->rootEnvironment, pos);
		while (penv != NULL && penv->type != environment_Style)
			penv = environment_GetParent(penv);
		if (penv == NULL)
			goto gotval;	/*  pat has no styles: trivially TRUE */

		/* now we know pat has at least one style */
		text = (struct text *)nessmark_GetText(subject);
		if ( ! class_IsType(text, textClass)) {
			boolval = FALSE;
			goto gotval;	/*  text is simple: trivially FALSE */
		}
		pos = nessmark_GetPos(subject);
		env = environment_GetInnerMost(text->rootEnvironment, pos);
		while (env != NULL && env->type != environment_Style)
			env = environment_GetParent(env);
		if (env == NULL) {
			boolval = FALSE;
			goto gotval;	/*  text has no styles: trivially FALSE */
		}

		/* now we know text and pattext both have at least one style 
			return TRUE unless we find a style on pat that is
			not on text */
		boolval = HasAllStyles(env, penv);

		/* found all styles of pat,  return the TRUE set far above */

	gotval:
		NSP = popValue(NSP);  
		NSP = popValue(NSP);  

		/* push boolval */
		NSPushSpace(boolstkelt); 
		NSP->b.hdr = boolHdr;
		NSP->b.v = boolval;
		break;

	/* {"addstylebyname", "Fl", {Tstr, Tstr, Tstr, Tend}, ness_codeYellow}, */
	case 'l':
		/* look up the style name 'pat' in the styles of  'subject'
			and in Default template (added to subject)
			and in styles of 'pat' 
			and in the DefinedStyles list*/

		cstring = (char *)nessmark_ToC(pat);
		style = NULL;
		text = (struct text *)nessmark_GetText(subject);
		if (class_IsType(text, textClass)) {
			style = stylesheet_Find(text_GetStyleSheet(text), cstring);
			if (style == NULL && text->templateName == NULL) {
				text_ReadTemplate(text, "default",
					text_GetLength(text) == 0);
				style = stylesheet_Find(text_GetStyleSheet(text),
					cstring);
			}
		}
		if (style == NULL) {
			pattext = (struct text *)nessmark_GetText(pat);
			if (class_IsType(pattext, textClass))
				style = stylesheet_Find(text_GetStyleSheet(pattext),
					cstring);
		}
		if (style == NULL) {
			for (stelt = DefinedStyles; stelt != NULL; stelt = stelt->next)
				if (strcmp(style_GetName(stelt->s), cstring) == 0) {
					style = stelt->s;
					break;
				}
		}
		AddOneStyle(text, nessmark_GetPos(subject),
				nessmark_GetLength(subject), style);
		text_NotifyObservers(text, 0);

		free(cstring);
		NSP = popValue(NSP);	/* discard the marker for stylename */
		break;

	/* {"parseint", "Fp", {Tlong, Tstr, Tvoid}, ness_codeOrange} */
	case 'p':
		/* set intval by scanning subject, use -2**31 for error 
			save matched portion w/ RememberWhereItWas */
		text = (struct text *)nessmark_GetText(subject);
		pos = nessmark_GetPos(subject);

		cstring = text_GetBuf(text, pos, 200, &envlen);
		success = lexan_ParseNumber(cstring, &len, &intval, &realval);
		if (success == 0 || len > envlen)
			intval = (1<<31);
		nessmark_Set(WhereItWas, text, pos, len);
		NSP = popValue(NSP);

		/* push intval */
		NSPushSpace(longstkelt); 
		NSP->l.hdr = longHdr;
		NSP->l.v = intval;
		break;
	/* {"parsereal", "Fq", {Tdbl, Tstr, Tvoid}, ness_codeOrange} */
	case 'q':
		text = (struct text *)nessmark_GetText(subject);
		pos = nessmark_GetPos(subject);

		/* set realval by scanning subject, use NaN for error 
			save matched portion in WhereItWas */

		cstring = text_GetBuf(text, pos, 200, &envlen);
		success = lexan_ParseNumber(cstring, &len, &intval, &realval);
		if (success == 0 || len > envlen)
			realval = sqrt(-1.0);
		else if (success == 1)
			realval = intval;
		nessmark_Set(WhereItWas, text, pos, len);
		NSP = popValue(NSP);

		/* push realval */
		NSPushSpace(dblstkelt); 
		NSP->d.hdr = dblHdr;
		NSP->d.v = realval;
		break;
	/* {"firstobject", "Fr", {Tptr, Tstr, Tvoid}, ness_codeOrange} */
	case 'r':
		/* set objval by scanning subject, use NULL for error 
			save matched location w/ RememberWhereItWas */
		nessmark_MakeConst(WhereItWas, "");	/* EmptyText */
		text = (struct text *)nessmark_GetText(subject);
		objval = NULL;
		if (class_IsType(text, textClass)) {
			len = text_GetLength(text);
			objval = NULL;
			for (pos = nessmark_GetPos(subject); pos < len; pos++) {
				if (text_GetChar(text, pos) 
						!= TEXT_VIEWREFCHAR)
					continue;
				vr = text_FindViewreference(text, pos, 1);
				if (vr == NULL)  continue;

				/* try to get the view, if there is one */
				objval = (struct basicobject *)
						dictionary_LookUp(text, (char *)vr);
				if (objval == NULL || objval == (struct basicobject *)
						textview_UNKNOWNVIEW) {
					objval =  (struct basicobject *)vr->dataObject;
				}
				nessmark_Set(WhereItWas, text, pos, 1);
				break;
			}
		}
		NSP = popValue(NSP);
		/* push objval */
		NSPushSpace(ptrstkelt); 
		NSP->p.hdr = ptrHdr;
		NSP->p.v = objval;
		break;
	/* {"nextstylegroup", "Fs", {Tstr, Tstr, Tvoid}, ness_codeOrange} */
	case 's':
		/* revise subject, advancing to next larger style group 
				or return start of arg 
		if subject is at start of style and a larger group starts at same place
			return the smallest enclosing style;  
		otherwise find the next place after start of subject where a style starts
			and return the smallest style group starting there
		A Ness program can find all styles by successive application of
			NextStyleGroup().   */
		text = (struct text *)nessmark_GetText(subject);
		if ( ! class_IsType(text, textClass)) {
			nessmark_Start(subject);
			break;
		}
		pos = nessmark_GetPos(subject);
		len = nessmark_GetLength(subject);

		/* Part I.  If one or more style groups start where subject does, 
			find the smallest that is longer than subject.  */

		env = environment_GetInnerMost(text->rootEnvironment, pos);
		tenv = env;
		while (tenv != NULL  &&  
				(tenv->type != environment_Style
				|| tenv->data.style == NULL
				|| tenv->header.nestedmark.length <= len)) 
			tenv = environment_GetParent(tenv);
		if (tenv != NULL &&  environment_Eval(tenv) == pos) {
			/* tenv satisfies:
				a) is a style
				b) is an ancestor of subject's style
				c) starts same place as subject
				d) is longer than subject
			    so set length of subject to the length of tenv
			BUT.  the environment may have length 99999999,
			so we need to check condition (d) again
			*/
			envlen = tenv->header.nestedmark.length;
			if (pos + envlen > text_GetLength(text))
				envlen = text_GetLength(text) - pos;
			if (envlen > len) {
				nessmark_SetLength(subject, envlen);
				break;	/* exit from nextstylegroup */
			}
		}

		/* Part II.  There is no larger style group that starts where subject does.
			Find the next place where a style starts and take the
			smallest group starting there.
			To do this we check each place where the style changes
			and find the innermost there.  If that innermost is a parent
			of subject, we have a style end and not a style start.  */

		envpos = pos;
		do {
			envpos += environment_GetNextChange(text->rootEnvironment,
							envpos);
			if (envpos >= text_GetLength(text)) {
				/* we've reached the end of text */
				nessmark_Start(subject);
				break;
			}
			tenv = environment_GetInnerMost(text->rootEnvironment,
							envpos);
			while (tenv != NULL  &&  tenv->type != environment_Style) 
				tenv = environment_GetParent(tenv);
			if (tenv == NULL) 
				/* tenv is root envt 
					and thus must be a parent of env */
				continue;
			penv = env;
			while (penv != NULL && penv != tenv)
				penv = environment_GetParent(penv);
			/* if tenv == penv, the style at tenv is a parent of subject's style,
					so it has already been processed */
			if (tenv != penv) {
				/* at this point, tenv points to the environment for the 
				    next group,  and that group starts at envpos */
				nessmark_SetPos(subject, envpos);
				envlen = tenv->header.nestedmark.length;
				len = text_GetLength(text);
				if (pos + envlen > len)
					envlen = len - pos;
				nessmark_SetLength(subject, envlen);
				break;
			}
		} while (TRUE);

		break;

	/* {"enclosingstylegroup","Ft", {Tstr, Tstr, Tvoid}, ness_codeOrange} */
	case 't':
		/* subject must be empty or correspond to a style group
			revise subject, advancing to next outer style group 
			or return start of arg (?)
			entire doc is not a style group unless explicitly set */
		text = (struct text *)nessmark_GetText(subject);
		if ( ! class_IsType(text, textClass)) {
			nessmark_Start(subject);
			break;
		}
		pos = nessmark_GetPos(subject);
		len = nessmark_GetLength(subject);

		/* get an enclosing style group.*/
		env = environment_GetInnerMost(text->rootEnvironment, pos);

		/* be sure the selected text is a style group */
		while (env != NULL  &&  len > env->header.nestedmark.length)
			env = environment_GetParent(env);
		if (env != NULL && len > 0 
				&& (pos != environment_Eval(env)  
				    ||  len !=  env->header.nestedmark.length))
			env = NULL;

		/* scan further up to find enclosing style */
		while (env != NULL && 
				(len == env->header.nestedmark.length  
				|| env->type != environment_Style))
			env = environment_GetParent(env);

		/* now set the mark to the enclosing range */
		if (env != NULL) {
			nessmark_SetPos(subject, environment_Eval(env));
			pos = nessmark_GetPos(subject);
			envlen = env->header.nestedmark.length;
			len = text_GetLength(text);
			if (pos + envlen > len)
				envlen = len - pos;
			nessmark_SetLength(subject, envlen);
		}
		else /* no enclosing style */
			nessmark_Start(subject);
		break;

	/* {"clearstyles", "Fu", {Tstr, Tstr, Tvoid}, ness_codeOrange} */
	case 'u':
		/* remove all styles that include exactly the subject */
		text = (struct text *)nessmark_GetText(subject);
		if ( ! class_IsType(text, textClass)) break;
#if 0	
/* debugging code */	
for (pos = 0; pos <= text_GetLength(text); pos ++) {
	/* print values of getnextchange and getinnermost */
	envpos = environment_GetNextChange(text->rootEnvironment, pos);
	env = environment_GetInnerMost(text->rootEnvironment, pos);
	printf("%d:  + %d    %c   ", pos, envpos, 
		simpletext_GetUnsignedChar((struct simpletext *)text, pos+envpos));
	while (env != NULL  &&  env->header.nestedmark.length < 999999999) {
		printf("    %d : %d", environment_Eval(env),
				env->header.nestedmark.length);
		if (env->type != environment_Style)
			printf("(not Style)");
		env = environment_GetParent(env);
	}
	printf("\n");
}
#else
		pos = nessmark_GetPos(subject);
		len = nessmark_GetLength(subject);
		if (len == 0) break;
		if (environment_Remove(text->rootEnvironment,
				pos, len, environment_Style, TRUE))
			text_SetModified(text);
		text_RegionModified(text, pos, len);
		text_NotifyObservers(text, 0);

		if (pos == 0 && len >= text_GetLength(text)) 
			/* We have removed all style invocations from entire document.  
				Remove also all non-template style definitions. */
			stylesheet_EnumerateStyles(text->styleSheet,
					 (procedure) DeleteNonTemplateStyles, text->styleSheet);
#endif
		break;

	/* {"nextstylesegment", "Fv", {Tstr, Tstr, Tvoid}, ness_codeOrange} */
	case 'v':
		/* advance subject to the segment extending 
			from its former finish to the next style change.  
			If at finish(base()), return finish(base()).*/
		text = (struct text *)nessmark_GetText(subject);
		pos = nessmark_GetPos(subject) + nessmark_GetLength(subject);
		nessmark_SetPos(subject, pos);
		envlen = environment_GetNextChange(text->rootEnvironment, pos);
		len = text_GetLength(text);
		if (pos + envlen > len)
			envlen = len - pos;
		nessmark_SetLength(subject, envlen);
		break;

	/* {"whereitwas", "Fw", {Tstr, Tvoid}, ness_codeOrange} */
	case 'w':
		/* push marker WhereItWas onto stack */
		if (WhereItWas == NULL)
			WhereItWas = nessmark_New();
		NSPushSpace(nessmark); 
		nessmark_Initialize(&NSP->m);
		nessmark_SetFrom(&NSP->m, WhereItWas);
		break;
	/* {"replacewithobject", "Fx", {Tstr, Tstr, Tptr, Tstr, Tvoid},
						ness_codeYellow} */
	case 'x': {
		cstring = (char *)nessmark_ToC(subject);   /* get view name */
		NSP = popValue(NSP);	
		/* get object pointer */
		if (NSP->p.hdr != ptrHdr || NSP->p.v == NULL 
				|| (objval=ProperPtr((struct basicobject *)NSP->p.v,
					dataobjectClass)) == NULL)
			RunError(":Item to insert should be a dataobject", 
					opiar);
		NSP = popValue(NSP);
		/* ??? If the pointer is to a cel, and the type is not celview,
			we use the object in the cel.
			XXX should use view_CanView(), but it is not fully implemented */
		if (class_IsType(objval, celClass) && strcmp(cstring, "celview") != 0)
			objval = (struct basicobject *)cel_GetObject((struct cel *)objval);

		PTSTOMARK(&NSP->m, opiar);
		subject = &NSP->m;		/* get text to be replaced */
		/* replace contents of marker now atop stack with
			a viewref for the object pointed at by objval 
			and having view given by cstring */
		text = (struct text *)nessmark_GetText(subject);
		if ( ! class_IsType(text, textClass))
			RunError(":objects cannot go in simple text", opiar);
		pos = nessmark_GetPos(subject);
		len = nessmark_GetLength(subject);
		if (*cstring == '\0') {
			free(cstring);
			cstring = (char *)freeze(dataobject_ViewName(
					(struct dataobject *)objval  ));
		}

		text_AddView(text, pos+len, cstring, objval);  /* DO IT ! */

		if (len > 0)
			text_DeleteCharacters(text, pos, len);
		/* XXX note that nessmarks do not get the special
			treatment given by the normal ness replace() */
		free(cstring);
		nessmark_SetLength(subject, 1);    /* set returned mark */
	}	break;
	}
}

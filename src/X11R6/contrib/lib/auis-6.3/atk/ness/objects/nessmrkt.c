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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/ness/objects/RCS/nessmrkt.c,v 1.7 1993/05/04 01:23:55 susan Exp $";
#endif

/* nessmarktest.c
	test the nessmark object

	reads stdin, expands the tabs to spaces, and writes it out to stdout
	tab positions are assumed to be 9, 17, 25, 33, . . .

*/

/*
 * $Log: nessmrkt.c,v $
 * Revision 1.7  1993/05/04  01:23:55  susan
 * RCS Tree Split
 *
 * Revision 1.6.1.1  1993/02/02  03:03:04  rr2b
 * new R6tape branch
 *
 * Revision 1.6  1992/12/15  21:38:20  rr2b
 * more disclaimerization fixing
 *
 * Revision 1.5  1992/12/15  00:50:53  rr2b
 * fixed disclaimerization
 *
 * Revision 1.4  1992/12/14  20:49:20  rr2b
 * disclaimerization
 *
 * Revision 1.3  1992/11/26  02:39:52  wjh
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
 * Revision 1.2  91/09/12  16:26:21  bobg
 * Update copyright notice and rcsid
 * 
 * Revision 1.1  1989/06/01  15:39:58  wjh
 * Initial revision
 *
 * Creation 0.0  88/03/25 11:02:00  wjh
 * Initial creation by WJHansen
 * 
*/

#include <stdio.h>
#include <class.h>
#include <smpltext.ih>
#include <mark.ih>

/* include the ones utilized, but not by this .c file itself */
#define class_StaticEntriesOnly
#	include <observe.ih>
#	include <proctbl.ih>
#	include <dataobj.ih>
#undef class_StaticEntriesOnly

#include <nessmark.ih>

static long writeid = 1;

main(argc, argv)
	register int	  argc;
	register char  **argv;
{
	register struct nessmark *m;
	register struct simpletext *t;

	printf("Start\n"); fflush(stdout);
	class_Init(".");		/* use current directory for dataobject path (???) */
	printf("Init done\n"); fflush(stdout);

	observable_StaticEntry;
	proctable_StaticEntry;
	dataobject_StaticEntry;
	simpletext_StaticEntry;
	mark_StaticEntry;
/* note that nessmark itself is dynamically loaded 
so nessmarktest need not be recompiled when nessmark is*/

	printf("About to New\n"); fflush(stdout);
	m = nessmark_New();

	nessmark_Destroy(m);

	printf("\n Reading stdin\n");  fflush(stdout);
	m = nessmark_New();
	t = simpletext_New();
	simpletext_SetAttributes(t, NULL);   /* XXX needed to set pendingReadOnly 
					to FALSE !!! */
	simpletext_Read(t, stdin, 0);

	nessmark_Set(m, t, 0, simpletext_GetLength(t));
	printdata(m);
	replacetabs(m);
	printdata(m);

	simpletext_Write(t, stdout, ++writeid, 0);
}

printdata(m)
	register struct nessmark *m;
{
	long loc, lend;
	lend = mark_GetEndPos((struct mark *)m);
	if (lend > 31) lend = 31;
	printf("(%d,%d) ", mark_GetPos((struct mark *)m), 
			mark_GetLength((struct mark *)m));
	for (loc = mark_GetPos((struct mark *)m); loc < lend; loc++)
		putchar(simpletext_GetChar(nessmark_GetText(m), loc));
	if (lend == 31 && mark_GetLength((struct mark *)m) > 31)
		printf(" . . .");
	putchar('\n');
	fflush(stdout);
}

/*
 * function ReplaceTabs(m) == {
 *	marker f, tab, eight;
 *	eight := "        ";  -- 8 spaces
 *	tab := eight;		-- initial distance to tab
 *	while m /= "" do {
 *		f := first(m);
 *		m := rest(m);
 *		if f = "\t" then {
 *			-- replace tab with spaces 
 *			replace (f, tab);
 *			tab := eight;
 *		}
 *		else if  f = "\n"  or  tab = " "  then
 *			-- newline or single space for this tab,
 *			--	start next tab
 *			tab := eight;
 *		else
 *			-- non-tab: shorten distance to tab stop
 *			tab := rest(tab);
 *	}
 * }
 */

replacetabs(file)
	struct nessmark *file;
{
	struct nessmark *m = nessmark_New(), 
			*f = nessmark_New(),
			*tab = nessmark_New(), 
			*eight = nessmark_New(), 
			*temp = nessmark_New(),
			*tabchar = nessmark_New(),
			*spacechar = nessmark_New(),
			*nlchar = nessmark_New();
	nessmark_SetFrom(m, file);
	nessmark_MakeConst(eight, "        ");
	nessmark_MakeConst(tabchar, "\t");
	nessmark_MakeConst(spacechar, " ");
	nessmark_MakeConst(nlchar, "\n");
	nessmark_SetFrom(tab, eight);
	while ( ! nessmark_IsEmpty(m)) {
		/* f := first(m); */
		nessmark_SetFrom(f, m);
		nessmark_Start(f);
		nessmark_Next(f);
		/* m := rest(m); */
		nessmark_SetFrom(temp, m);
		nessmark_Start(m);
		nessmark_Next(m);
		nessmark_Next(m);
		nessmark_Extent(m, temp);

		if (nessmark_Equal(f, tabchar)) {
			/* replace tab with spaces */
			nessmark_Replace(f, tab);
			nessmark_SetFrom(tab, eight);
		}
		else if (nessmark_Equal(f, nlchar)  ||  nessmark_Equal(tab, spacechar)) 
			/* non-tab at tab stop, set for next tab stop */
			nessmark_SetFrom(tab, eight);
		else {
			/* non-tab: shorten distance to tab stop */
			/* tab := rest(tab); */
			nessmark_SetFrom(temp, tab);
			nessmark_Start(tab);
			nessmark_Next(tab);
			nessmark_Next(tab);
			nessmark_Extent(tab, temp);
		}
	}
	nessmark_Destroy(m);
	nessmark_Destroy(f);
	nessmark_Destroy(tab);
	nessmark_Destroy(tabchar);
	nessmark_Destroy(spacechar);
	nessmark_Destroy(nlchar);
	nessmark_Destroy(eight);
	nessmark_Destroy(temp);

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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/ness/objects/RCS/interpt.c,v 1.13 1993/05/04 01:23:55 susan Exp $";
#endif

/* interptest.c
	test the interp object

	builds an object byte stream into ObjectCode
	and executes it
*/

/*
 * $Log: interpt.c,v $
 * Revision 1.13  1993/05/04  01:23:55  susan
 * RCS Tree Split
 *
 * Revision 1.11.1.2  1993/02/08  17:34:02  wjh
 * change for new definition of funcnode
 *
 * Revision 1.11.1.1  1993/02/02  03:01:50  rr2b
 * new R6tape branch
 *
 * Revision 1.11  1992/12/15  21:37:21  rr2b
 * more disclaimerization fixing
 *
 * Revision 1.10  1992/12/15  00:48:27  rr2b
 * fixed disclaimerization
 *
 * Revision 1.9  1992/12/14  20:49:20  rr2b
 * disclaimerization
 *
 * Revision 1.8  1992/11/26  02:38:01  wjh
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
 * Revision 1.7  91/09/12  16:25:50  bobg
 * Update copyright notice and rcsid
 * 
 * Revision 1.6  1989/09/03  22:48:05  wjh
 * newness
 *
 * Revision 1.5  89/06/01  16:00:15  wjh
 * campus release version
 * 
 * Revision 1.1  88/11/02  14:41:55  wjh
 * Initial revision
 * 
 * Creation 0.0  88/04/06 10:36:00  wjh
 * Initial creation by WJHansen
 * 
*/

#include <stdio.h>
#include <class.h>
#include <nodeclss.h>
#include <smpltext.ih>
#include <nessmark.ih>
#include <ness.ih>
#include <interp.h>
#include <compdefs.h>
#include <gen.h>
#include <nesssym.ih>
#include <envt.h>		/* for NSPstore */


/* include the ones utilized, but not by this .c file itself */
#define class_StaticEntriesOnly
#	include <observe.ih>
#	include <proctbl.ih>
#	include <dataobj.ih>
#undef class_StaticEntriesOnly


YYSTYPE yylval;

/* kludgery needed here because of kludgery in gen.c to do yyerrok and yyclearin */
int yychar = -1;
short yyerrflag = 0;

long LoadReplaceTabs();


main(argc, argv)
	register int  argc;
	register char  **argv;
{
	register struct nessmark *func, *printit;
	FILE *f;
	TSysMarkRef   f1, f2, hi, nl;
	TSysMarkRef   piloc, floc, reptabsloc;
	struct nesssym *reptabsym, *pisym, *testsym, *proto;
	nesssym_scopeType scope = nesssym_GLOBAL;

	printf("Start\n"); fflush(stdout);
	class_Init(".");		/* use current directory for dataobject path (???) */
	printf("Init done\n"); fflush(stdout);

	observable_StaticEntry;
	proctable_StaticEntry;
	dataobject_StaticEntry;
	simpletext_StaticEntry;
	mark_StaticEntry;
	nessmark_StaticEntry;

	if ((f=fopen("/tmp/foo", "r")) == NULL) {
		printf("There needs to be a file /tmp/foo with tabs in it.\n");
		exit(1);
	}
	fclose(f);

	initializeEnvt(NULL);	/* initialize interpreter */
	dumpStack(stdout, NSPstore);

	reptabsloc = LoadReplaceTabs();

	f1 = makeConst("/tmp");
	f2 = makeConst("/foo");
	hi = makeConst("Goodbye");
	nl = makeConst("\n");

	printit = makeFunction(&piloc);
	refStack(printit, 'P', 0);	/* enter func, no locals */
	refStack(printit, 'l', 0);	/* load 1st arg */
	addOp(printit, 'm');		/* dup */		/* creates return value */
	addOp(printit, 'j');		/* print */
	addOp(printit, 'y');		/* pop discard function value */
	refSysMark(printit, 'k', nl);	/* "\n" */
	addOp(printit, 'j');		/* print */
	addOp(printit, 'y');		/* pop discard function value */
	refStack(printit, 'Q', sizeof(struct nessmark));	/* leave function (1 arg)*/

  	func = makeFunction(&floc);
	refStack(func, 'P', 0);		/* enter function, with no locals  */
	addOp(func, 'q');		/* newbase */
	refSysMark(func, 'k', f1);	/* "/tmp" */
	refSysMark(func, 'O', piloc); /* call printit */
	addOp(func, 'm');		/* dup */
	addOp(func, 'j');		/* print */
	addOp(func, 'y');		/* pop discard function value */
	addOp(func, 'r');		/* replace */
	addOp(func, 'n');		/* next */
	refSysMark(func, 'k', f2);	/* "/foo" */
	refSysMark(func, 'O', piloc); /* call printit */
	addOp(func, 'r');		/* replace */
	addOp(func, 'p');		/* base */
	refSysMark(func, 'O', piloc); /* call printit */
	addOp(func, 'i');		/* readfile */
	refSysMark(func, 'O', piloc); /* call printit */
	addOp(func, 'm');		/* dup, save a copy for replacetabs call */

	addOp(func, 'o');		/* start */
	addOp(func, 'n');		/* next */
	refSysMark(func, 'O', piloc); /* call printit */
	addOp(func, 'n');		/* next */
	addOp(func, 'm');		/* dup */
	addOp(func, 'n');		/* next */
	addOp(func, 'n');		/* next */
	addOp(func, 'n');		/* next */
	refSysMark(func, 'O', piloc); /* call printit */
	addOp(func, 'x');		/* extent */
	refSysMark(func, 'O', piloc); /* call printit */
	refSysMark(func, 'k', hi);	/* "Goodbye" */
	addOp(func, 'r');		/* replace */
	addOp(func, 'm');		/* dup */
	addOp(func, 'j');		/* print */
	addOp(func, 'y');		/* pop discard function value */
	refSysMark(func, 'k', nl);	/* "\n" */
	addOp(func, 'j');		/* print */
	addOp(func, 'y');		/* pop discard function value */
	addOp(func, 'p');		/* base */
	addOp(func, 'j');		/* print */
	addOp(func, 'y');		/* pop discard function value */
	refSysMark(func, 'k', nl);	/* "\n" */
	addOp(func, 'j');		/* print */
	addOp(func, 'y');		/* pop discard function value */

	refSysMark(func, 'O', reptabsloc);	/* call replace tabs, using saved copy of file */
	addOp(func, 'p');		/* base */
	addOp(func, 'j');		/* print */
	addOp(func, 'y');		/* pop discard function value */
	refSysMark(func, 'k', nl);	/* "\n" */
	addOp(func, 'j');		/* print */
	addOp(func, 'y');		/* pop discard function value */
	refStack(func, 'Q', 0);	/* leave function */

	printdata(func);

	proto = nesssym_New();
	reptabsym = nesssym_NDefine("ReplaceTabs", proto, scope);
	reptabsym->header.toksym.info.node = (struct node *)
			funcnode_Create(reptabsloc, 0,0, NULL, NULL, NULL, 0);
	reptabsym->flags = flag_function | flag_ness;

	testsym = nesssym_NDefine("Test", proto, scope);
	testsym->header.toksym.info.node = (struct node *)
			funcnode_Create(floc, 0,0, NULL, NULL, NULL, 0);
	testsym->flags = flag_function | flag_ness;

	pisym = nesssym_NDefine("PrintIt", proto, scope);
	pisym->header.toksym.info.node = (struct node *)
			funcnode_Create(piloc, 0,0, NULL, NULL, NULL, 0);
	pisym->flags = flag_function | flag_ness;

	pisym->next = testsym;
	testsym->next = reptabsym;
	dumpAttrList(stdout, pisym);

	dumpStack(stdout, NSPstore);

	interpretNess(floc, ness_New());	/* initiate interpretation */

	dumpStack(stdout, NSPstore);
	
}

printdata(m)
	register struct nessmark *m;
{
	long loc, lend;
	lend = mark_GetEndPos((struct mark *)m);
	if (lend > 31) lend = 31;
	printf("(%d,%d) ", mark_GetPos((struct mark *)m), 
			mark_GetLength((struct mark *)m));
	for (loc = mark_GetPos((struct mark *)m); loc < lend; loc++) {
		char c = simpletext_GetChar(nessmark_GetText(m), loc);
		if (' ' <= c && c <= '~')
			putchar(c);
		else 	printf("\\%3o", c);
	}
	if (lend == 31 && mark_GetLength((struct mark *)m) > 31)
		printf(" . . .");
	putchar('\n');
	fflush(stdout);
}

/*
 * function ReplaceTabs(m) == {
 *	marker c, tab, eight;
 *	eight := "        ";  -- 8 spaces
 *	tab := eight;		-- initial distance to tab
 *	while m /= "" do {
 *		c := first(m);  -- same as front(m)
 *		m := rest(m);
 *		if c = "\t" then {
 *			-- replace tab with spaces 
 *			replace (c, tab);
 *			tab := eight;
 *		}
 *		else if  c = "\n"  or  tab = " "  then
 *			-- newline or single space for this tab,
 *			--	start next tab
 *			tab := eight;
 *		else
 *			-- non-tab: shorten distance to tab stop
 *			tab := rest(tab);
 *	}
 *	return (base(m));
 * }
 */

/* LoadReplaceTabs()
	builds a function for replacing tabs
	returns the index needed to call the function 
*/
	long
LoadReplaceTabs()
{
	short floc;
	struct nessmark *f;
	short sp8, tab, nl, sp;
	long mloc, cloc, eightloc, tabloc;  /* offsets of local vars on stack */
	long g1, g2, g4, g5, g6, g7;		/* goto targets */

	sp8 = makeConst("        ");
	tab = makeConst("\t");
	nl = makeConst("\n");
	sp = makeConst(" ");

	tabloc = 0;				/* local */
	eightloc = tabloc + sizeof(struct nessmark);	/* local */
	cloc = eightloc + sizeof(struct nessmark);	/* local */
	mloc = cloc + sizeof(struct nessmark);	/* arg */

	f = makeFunction(&floc);
	refStack(f, 'P', 3 * sizeof(struct nessmark));	/* enter func, 3 local vars */
	setcurfuncmark(f);				/* for generating branches */

	/* initialize locals */
	refSysMark(f, 'k', sp);	/* c := " "; */
	refStack(f, 's', cloc);

	refSysMark(f, 'k', sp8);	/* eight := "        "; */
	addOp(f, 'm');		/* dup */
	refStack(f, 's', eightloc);
	refStack(f, 's', tabloc);	/* tab := eight; */

	/* while m /= "" do {  */
/* g1:   repeat the while loop */
	g1 = genlabel();
	refStack(f, 'l', mloc);	/* load m */
	addOp(f, 'u');		/* cmp EMPTY */
	g2 = genbranch('e', 0);	/* brEQ to after loop */
	
		/* c := first(m);
		    m := rest(m);    -- extent (next(next(start(m))), m)
		*/
		refStack(f, 'l', mloc);	/* load m */
		addOp(f, 'o');		/* start */
		addOp(f, 'n');		/* next */
		refStack(f, 's', cloc);	/* store c */
		refStack(f, 'l', mloc);	/* load m */
		addOp(f, 'm');		/* dup */
		addOp(f, 'o');		/* start */
		addOp(f, 'n');		/* next */
		addOp(f, 'n');		/* next */
		addOp(f, 'z');		/* swap */
		addOp(f, 'x');		/* extent */
		refStack(f, 's', mloc);	/* store m */
		
		 /* if c = "\t" then {
			-- replace tab with spaces 
			chars(c) := tab;    -- replace(c,tab)
			tab := eight;
		  } */

		predpush(FALSE, 0, 'I');
		refStack(f, 'l', cloc);	/* load c */
		refSysMark(f, 'k', tab);	/* load "\t" */
		addOp(f, 't');		/* string compare */
		predtarget(predEQ);	/* brNE to after then clause */
		predfixdropthru();
		refStack(f, 'l', cloc);	/* load c */
		refStack(f, 'l', tabloc);	/* load tab */
		addOp(f, 'r');		/* replace */
		refStack(f, 'l', eightloc);	/* load eight */
		refStack(f, 's', tabloc);	/* store tab */
		g4 = genbranch('g', 0);	/* goto after else */
		predfixtarget();
		predpop();


		/*  else if  c = "\n"  or  tab = " "  then
			-- newline or single space for this tab,
			--	start next tab
			tab := eight;
		*/
		refStack(f, 'l', cloc);	/* load c */
		refSysMark(f, 'k', nl);	/* load "\n" */
		addOp(f, 't');		/* cmp */
		g7 = genbranch('e');	/* brEQ to then clause */
		refStack(f, 'l', tabloc);	/* load tab */
		refSysMark(f, 'k', sp);	/* load " " */
		addOp(f, 't');		/* cmp */
		g5 = genbranch('f');	/* brNE to after then clause */
/* g7: */
		fixbranch(g7);
		refStack(f, 'l', eightloc);	/* load eight */
		refStack(f, 's', tabloc);	/* store tab */
		g6 = genbranch('g', 0);	/* goto after else */
/* g5: */
		fixbranch(g5);

		/* else
			-- non-tab: shorten distance to tab stop
			tab := rest(tab);
		*/
		refStack(f, 'l', tabloc);	/* load tabloc */
		addOp(f, 'm');		/* dup */
		addOp(f, 'o');		/* start */
		addOp(f, 'n');		/* next */
		addOp(f, 'n');		/* next */
		addOp(f, 'z');		/* swap */
		addOp(f, 'x');		/* extent */
		refStack(f, 's', tabloc);	/* store tabloc */
/* g6: */
		fixbranch(g6);
/* g4: */
		fixbranch(g4);

	/* } -- end while */
	(void)genbranch('g', g1);	/* goto while test */
/* g2:  after the while loop */
	fixbranch(g2);

	/*  return (base(m));
	*/
	refStack(f, 'l', mloc);		/* load m */
	addOp(f, 'p');			/* base */
	refStack(f, 'Q', 4 * sizeof(struct nessmark));	/* leave function, 4 args & locals */
	return floc;

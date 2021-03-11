/* ********************************************************************** *\
 *         Copyright Carnegie Mellon University 1992 - All Rights Reserved
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
static char *rcsid = "$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/ness/objects/RCS/compile.c,v 1.9 1993/12/10 01:11:42 wjh Exp $";
#endif

/* compile.c
	Compile a ness 
*/
/*
 *    $Log: compile.c,v $
 * Revision 1.9  1993/12/10  01:11:42  wjh
 * curNess ==> curComp->ness
 *
 * Revision 1.8  1993/09/14  16:29:34  wjh
 * removed some compiler warnings due to assigments to void * variables
 *
 * Revision 1.7  1993/05/04  01:23:55  susan
 * RCS Tree Split
 *
 * Revision 1.5.1.2  1993/02/08  17:34:02  wjh
 * declare some ntXXX variables for ness.gra
 * when compiling for location, use same scopes as original compilation so global variables are found
 *
 * Revision 1.5.1.1  1993/02/02  03:07:38  rr2b
 * new R6tape branch
 *
 * Revision 1.5  1992/12/14  20:50:00  rr2b
 * disclaimerization
 *
 * Revision 1.4  1992/12/08  22:28:00  wjh
 * fixed another core dump that occurred for runtime errors
 * .ved th
 *
 * Revision 1.3  1992/12/08  04:14:46  wjh
 * fixed so it will not crash when a runtime error occurs
 * 	moved the recompile to locate error operations to compile.c
 * .
 *
 * Revision 1.2  1992/12/04  18:51:18  rr2b
 * removed extra definition of textClass it's gotten from interp.c
 * (sigh, should probably make it ness_textClass or such...)
 * .
 *
 * Revision 1.1  1992/12/03  03:01:34  wjh
 * Initial revision
 *
 */

#include <ctype.h>

#include <im.ih>
#include <environ.ih>
#include <envrment.ih>
#include <text.ih>
#include <toksym.ih>
#include <parse.ih>
#include <lexan.ih>
#include <tlex.ih>

#include <compdefs.h>
#include <gen.h>		/* (for pssp) */
#include <error.h>
#include <ness.ih>

/* (needed for ness.gra.act) */
static int ntsetID = 0, ntsetSTRINGCON = 0, ntRETURN = 0;

#include <ness.gra.tab.c>	/* parse tables */
#include <parsedesc.h>		/* declare parse_description */
#include <parsepre.h>		/* begin function 'action' */
#include <ness.gra.act>		/* body of function 'action' */
#include <parsepost.h>		/* end of function 'action' */

/* declarations needed for ness.tlc */

#undef environment_GetParent
#define environment_GetParent(env) \
		((struct environment *)(env)->header.nestedmark.parent)
static toksym_scopeType GrammarScope;
static struct nesssym *SymProto;
/* struct classinfo *textClass = NULL; from interp.c */
static long GenNum = 0;		/* generates a sequence of integer values */
static struct toksym *EmptyStringToken;
static void parsepragmat(/* struct ness * self, char *prag */);
#define SETPOS(sym) \
	((sym)->loc = tlex_GetTokPos(tlex), \
	(sym)->len = tlex_GetTokEnd(tlex)-tlex_GetTokPos(tlex)+1)
#define CURCOMP ((struct compilation *)(tlex->rock))

#include <ness.tlc>

struct compilation *curComp = NULL;	/* compilation in progress */
int ntEXTEND = 0, ntON = 0, ntFUNCTION = 0, ntWHILE = 0, ntIF = 0, 
	ntEVENT = 0, ntMENU = 0, ntMOUSE = 0, ntKEYS = 0;

static struct compilation *Idle = NULL;
static struct nesssym *proto;
static int idtok;


static boolean FoundIt = FALSE;	/* TRUE if search satisfied */
static long FoundLoc, FoundLen;	/* where we found it */


/* compileLocate(bytes)
	Test to see if we have found the offset we wanted. 
	Update the notion of what the current offset is.
*/
	void
compileLocate(bytes)
	long bytes;
{
	if ( ! curComp->LocationAdvancing)
		return;
	if (! FoundIt && curComp->CurrOffset >= curComp->Sought) {
		long loc, len;
		FoundIt = TRUE;
		loc = tlex_RecentPosition(curComp->tlex, 0, &len); 
		FoundLoc = (pssp->loc > curComp->StmtStart)
			? pssp->loc : curComp->StmtStart;
		FoundLen = loc+len - FoundLoc;
	}
	curComp->CurrOffset += bytes;
}

/* compileStmtStart(tokeninx)
	save location of statment start in source for errors 
*/
	void
compileStmtStart(tokeninx)
	long tokeninx;
{
	if (curComp->Locating) 
		curComp->StmtStart = tlex_RecentPosition(curComp->tlex, tokeninx, 0); 
}


	static void
EnterReservedWord(rock, w, i)
	void *rock;
	char *w;
	int i;
{
	struct nesssym *s;
	boolean new;
	s = nesssym_NLocate(w, SymProto, GrammarScope, &new);
	s->header.toksym.toknum = i;
}

	static void
parsepragmat(self, prag)
	struct ness * self;
	char *prag;
{
	char *enable = "enable class access";
	char *Ness = "Ness";
	
	if (strncmp(prag, enable, strlen(enable)) == 0)
		/* --$enable class access */
		self->ClassEnabled = TRUE;
	else if (strncmp(prag, Ness, strlen(Ness)) == 0) {
		/*  --$Ness <version-number>
			 check the version number */
		while (*prag && !isdigit(*prag)) prag++;
		if (*prag) 
			self->syntaxlevel = atoi(prag);
		else {
			/* Illegal $Ness pragmat */
			self->syntaxlevel = UNSPECIFIEDSYNTAXLEVEL;
		}
	}
}

compileInit()
{
	if (Idle == NULL) {
		/* create a new compilaton record */
		Idle = (struct compilation *)malloc(sizeof(struct compilation));
		Idle->tlex = tlex_Create(&ness_tlex_tables, NULL, NULL, 0, 0);
		Idle->parser = parse_Create(
			&parse_description,
			Idle->tlex,
			reduceActions,
			NULL,		/* set rock later */
			errorfromparse);   /* error.c */
		Idle->next = NULL;
	}
	if (GrammarScope == NULL) {
		boolean new;
		struct toksym *t;

		ntEXTEND = parse_TokenNumberFromName(Idle->parser, "EXTEND");
		ntON = parse_TokenNumberFromName(Idle->parser, "ON");
		ntFUNCTION = parse_TokenNumberFromName(Idle->parser, "FUNCTION");
		ntWHILE = parse_TokenNumberFromName(Idle->parser, "WHILE");
		ntIF = parse_TokenNumberFromName(Idle->parser, "IF");
		ntEVENT = parse_TokenNumberFromName(Idle->parser, "EVENT");
		ntMENU = parse_TokenNumberFromName(Idle->parser, "MENU");
		ntMOUSE = parse_TokenNumberFromName(Idle->parser, "MOUSE");
		ntKEYS = parse_TokenNumberFromName(Idle->parser, "KEYS");

		/* local to this file (for ness.gra) */
		ntsetID = parse_TokenNumberFromName(Idle->parser, "setID");
		ntsetSTRINGCON = parse_TokenNumberFromName(Idle->parser, 
				"setSTRINGCON");
		ntRETURN = parse_TokenNumberFromName(Idle->parser, "RETURN");

		GrammarScope = nesssym_NNewScope(toksym_GLOBAL);
		SymProto = nesssym_New();
		initializeEnvt();	/* initialize Ness interpreter */

		parse_EnumerateReservedWords(Idle->parser, 
				EnterReservedWord, NULL);
		idtok = parse_TokenNumberFromName(Idle->parser, "setID");
		proto = nesssym_New();
		callInit(GrammarScope, idtok, proto);

		t = toksym_TLocate("\"", proto, GrammarScope, &new);
		t->info.str = NULL;
		t->toknum = parse_TokenNumberFromName(Idle->parser, 
			"setSTRINGCON");
		t->loc = 0;  t->len = 0;
		EmptyStringToken = t;
	}
}

	static void
doCompile(ness, pos, len, locating)
	struct ness *ness;
	long pos, len;
	boolean locating;
{
	curComp->ness = ness;
	tlex_SetRock(curComp->tlex, (void *)curComp);
	parse_SetRock(curComp->parser, (void *)curComp);
	curComp->idtok = idtok;
	curComp->proto = proto;

	curComp->scopex = 0;
	curComp->scopes[0] = ness->outerScope;
	curComp->predcond = FALSE;	
	curComp->predtargetlist = NULL;
	curComp->preddropthrulist = NULL;
	curComp->varfixups = NULL;
	curComp->freefixups = NULL;
	curComp->Locating = locating;

	tlex_SetText(curComp->tlex, ness, pos, len);

	ness->compilationid = im_GetWriteID();
	ness->AttrDest = &ness->globals;  /* where to put defined symbols */
	SetupErrorHandling(ness);
	ness->CurrentObject = NULL;

	parse_Run(curComp->parser);
}


	struct errornode *
compile(ness, pos, len)
	struct ness *ness;
	long pos, len;
{
	struct compilation *saveComp = curComp;  /* save to restore below */
	compileInit();
	curComp = Idle;
	Idle = curComp->next;

	ness->outerScope = nesssym_NNewScope(GrammarScope);
	ness->constScope = nesssym_NNewScope(GrammarScope);

	doCompile(ness, pos, len, FALSE);

	curComp->next = Idle;
	Idle = curComp;
		/* XXX are there resources to release here ??? */
	curComp = saveComp;	/* restore prior compile */
	return ness->ErrorList;
}

/* compileForLocation(ness, pos, len, objloc, LocateInInitFunc, Ploc, Plen)
	run the compiler in a mode to find the source code that generated
	the code at objloc.  Set the position and length of the source 
	expression into *Ploc and *Plen.
	Return TRUE iff found the location.
	curComp->LocationAdvancing toggles back and forth in 
		genSaveFuncState and genRestoreFuncState.
		If it is true, the offset advances in compileLocate.  
		Otherwise not.  Initially curComp->LocationAdvancing 
		is True iff the location we want is within the InitFunc.
*/
	boolean
compileForLocation(ness, pos, len, objloc, LocateInInitFunc, Ploc, Plen)
	struct ness *ness;
	long pos, len;
	unsigned long objloc;
	boolean LocateInInitFunc;
	unsigned long *Ploc, *Plen;
{
	struct compilation *saveComp = curComp;  /* save to restore below */

	compileInit();

	curComp = Idle;
	Idle = curComp->next;

	curComp->Sought = objloc;
	FoundIt = FALSE;
	curComp->CurrOffset = 0;
	curComp->StmtStart = 0;
	curComp->LocationAdvancing = LocateInInitFunc;
	pssp++;		/* allocate a dummy so pssp->loc is always defined */
	pssp->loc = 0;

	doCompile(ness, pos, len, TRUE);

	compileLocate(0);
	curComp->next = Idle;
	Idle = curComp;
		/* XXX are there resources to release here ??? */
	curComp = saveComp;	/* restore prior compile */

	*Ploc = FoundLoc;
	*Plen = FoundLen;
	return FoundIt;
}



/* compNewScope()
	pushes the current scope and makes a new one
	returns the new scope 
*/
	nesssym_scopeType
compNewScope()
{
	nesssym_scopeType newscope;
	if (curComp->scopex+1 >=
 			(sizeof(curComp->scopes)/sizeof(curComp->scopes[0]))) {
		/* XXX compiler errror */
		ReportError(":disastrous compile error: scope stack overflow", 0);
		return curComp->scopes[curComp->scopex];
	}
	curComp->scopex++;
	curComp->scopes[curComp->scopex] 
		= nesssym_NNewScope(curComp->scopes[curComp->scopex-1]);
	return 	curComp->scopes[curComp->scopex];
}

/* compPopScope()
	revert to a prior scope
*/
	void
compPopScope()
{
	if (curComp->scopex <= 0) return;
	curComp->scopex--;
}

/* compPushScope(scope)
	enter a scope 
	(used for the global initialization function)
*/
	void
compPushScope(scope)
	nesssym_scopeType scope;
{
	if (curComp->scopex+1 >= 
			(sizeof(curComp->scopes)/sizeof(curComp->scopes[0]))) {
		ReportError(":disastrous compile error: scope stack overflow", 0);
		return;
	}
	curComp->scopex++;
	curComp->scopes[curComp->scopex] = scope;
}

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
static char *rcsid = "$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/ness/objects/RCS/gen.c,v 1.35 1993/12/10 01:11:42 wjh Exp $";
#endif

/* 
	gen.c  -  generate interpreter code for ness

*/
/*
 * $Log: gen.c,v $
 * Revision 1.35  1993/12/10  01:11:42  wjh
 * curNess ==> curComp->ness
 *
 * Revision 1.34  1993/07/23  00:20:51  rr2b
 * Split off a version of CopyText which will copy surrounding
 * styles as well as embedded styles.
 *
 * Revision 1.33  1993/07/21  19:34:22  rr2b
 * Fixed to use CopyTextExactly
 *
 * Revision 1.32  1993/05/13  20:17:14  wjh
 * R6split
 *
 * Revision 1.29.1.2  1993/02/08  17:34:02  wjh
 * change to use mark in funcnode
 *
 * Revision 1.29.1.1  1993/02/02  03:00:57  rr2b
 * new R6tape branch
 *
 * Revision 1.29  1992/12/14  20:49:20  rr2b
 * disclaimerization
 *
 * Revision 1.28  1992/12/09  04:25:22  wjh
 * fixed extraneous \...n around definitions in dict.n
 * by fixing BackSlashReduce to keep track of where it is in the source
 * .
 *
 * Revision 1.27  1992/12/08  04:14:46  wjh
 * fixed so it will not crash when a runtime error occurs
 * 	moved the recompile to locate error operations to compile.c
 * .
 *
 * Revision 1.26  1992/12/04  19:14:57  wjh
 * comment out the definition of parencheck, which is no longer called
 * .
 *
 * Revision 1.25  1992/11/26  02:42:25  wjh
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
 * Revision 1.24  92/06/05  16:39:31  rr2b
 * corrected sysmark allocation and made arrangements
 * for deallocation as well, sysmarks should no longer be
 * "leaked"
 * 
 * Revision 1.23  1991/09/24  18:34:39  wjh
 * make BackSlashReduce non-static
 * .c inside freeze
 * .
 * ...	log elided, Nov 92 wjh
 * 
 * Creation 0.0  88/03/29 10:25:00  wjh
 * Initial creation by WJHansen
 * 
*/
#include <andrewos.h>	/* for bzero() */
#include <ctype.h>

#include <text.ih>
#include <lexan.ih>
#include <tlex.ih>

#include <nodeclss.h>
#include <nessmark.ih>
#include <interp.h>
#include <compdefs.h>
#include <envt.h>
#include <gen.h>
#include <error.h>
#include <call.h>
#include <ness.ih>
#include <toksym.ih>


static sysMarkIndex freeSysMarks = (-1);

/* defining instance for declaration in compdefs.hn */
char *TypeName[/* Texpr */] = {
	"list-end", 
	"integer", 
	"boolean", 
	"boolean", 
	"real", 
	"marker", 
	"object", 
	"function", 
	"void",
	"unknown",
	"Cstring",
	"short",
	"char"
};

/* the order of operators in Xoptbl is EQ, NE, LT, GE, GT, LE, BR, NOP
		BR is used for predT and predAND
		NOP is used for predF and predOR
	the FopTbl has the opposite operation from the TopTbl */
static unsigned char TopTbl[] = {'e', 'f', 'a', 'd', 'b', 'c', 'g', '\n'};
static unsigned char FopTbl[] = {'f', 'e', 'd', 'a', 'c', 'b', '\n', 'g'};


/* these two may have elements from more than one
	compilation if there is a compile during compile */
static struct predstatenode predstack[MAXEXPRDEPTH];
struct predstatenode *pssp = predstack - 1;   /* points to the in-use elt of predstate stack */


#define ENTERARGOFFSET 1	/* offset from start of object for function
				to the operand that must be fixed */


/* allocSysMark(erroff)
	allocate an element of the SysMark array
	if space exhausted, used 'erroff' to identify the error token 

	if we ever extend SysMark, we must not go over 65535 entries
		because there are only two bytes for addressing in operands
*/
	static struct nessmark *
allocSysMark(erroff, theNess)
	long erroff;
	struct ness *theNess;
{
	struct nessmark * v;

	if(freeSysMarks>=0) {
	    v = &SysMarkLowEnd[freeSysMarks];
	    freeSysMarks=SysMarkNextPtrs[freeSysMarks];
	} else {
	    v = SysMarkFree;

	    if (v == NULL && SysMarkSize < 65535) {
		/* allocate more space for sysmark array */
		long i;
		long oldsize = SysMarkSize;
		SysMarkSize += 1000;
		if (SysMarkSize > 65535) SysMarkSize = 65535;
		SysMarkLowEnd = (struct nessmark *)
		  realloc(SysMarkLowEnd, SysMarkSize * sizeof(struct nessmark));
		SysMarkNextPtrs = (long *)realloc(SysMarkNextPtrs, SysMarkSize * sizeof(long));
		bzero(SysMarkLowEnd +oldsize /* * sizeof(struct nessmark) */, 
		      (SysMarkSize - oldsize) * sizeof(struct nessmark));
		bzero(SysMarkNextPtrs+oldsize,  
		      (SysMarkSize - oldsize) * sizeof(long));
		SysMarkHiEnd = SysMarkLowEnd + SysMarkSize;
		for (i = oldsize; i < SysMarkSize; i++)
		    nessmark_SetNext(SysMarkLowEnd+i, 
				     (struct mark *)(SysMarkLowEnd+i+1));
		nessmark_SetNext(SysMarkHiEnd-1, NULL);
		SysMarkFree = SysMarkLowEnd + oldsize;
		v = SysMarkFree;
	    }
	    else if (v == NULL) {
		/* XXX need to realloc SysMark, but would exceed 65535 */
		ReportError(":Global storage exceeded", erroff);
		/* KLUDGE to turn off compilation:   XXX */
			curComp->Locating = TRUE;  
			curComp->LocationAdvancing = TRUE;
			curComp->Sought = curComp->CurrOffset;
		return NULL;
	    }
	    SysMarkFree = (struct nessmark *)nessmark_GetNext(v);
	}
	if(theNess) {
	    SysMarkNextPtrs[v-SysMarkLowEnd]=theNess->marks;
	    theNess->marks=v-SysMarkLowEnd;
	}
	return v;
}

	void 
deallocSysMarks(ind)
	sysMarkIndex ind;
{
    sysMarkIndex i=ind, j;

    while(i>0) {

	j=SysMarkNextPtrs[i];
	if(((TType)SysMarkLowEnd[i].header.nessmark_methods) == nessmarkHdr) {
	    if(nessmark_GetText(&SysMarkLowEnd[i]) == ObjectCode) {
		if(nessmark_GetLength(&SysMarkLowEnd[i])>0) simpletext_DeleteCharacters(ObjectCode, nessmark_GetPos(&SysMarkLowEnd[i]), nessmark_GetLength(&SysMarkLowEnd[i])+1);
		nessmark_DetachFromText(&SysMarkLowEnd[i]);
	    }

	    nessmark_Finalize(&SysMarkLowEnd[i]);
	}
	SysMarkNextPtrs[i]=freeSysMarks;
	freeSysMarks=i;
	i=j;
    }
}
    

/* makeFunction(loc)
	returns a pointer to a mark for space to store a function in ObjectCode
	sets 'loc' to the index of the function for use in calls
	
	adds a dummy RETURN after the function to separate its marks 
	from that of the next function
*/
	struct nessmark *
makeFunction(loc) 
	TSysMarkRef *loc;
{
	long endpos;
	struct nessmark *v = allocSysMark(-1, curComp->ness);
	if (curComp->Locating) {*loc = 0; return NULL;}
	if (v == NULL) return NULL;

	nessmark_Initialize(v);
	endpos = simpletext_GetLength(ObjectCode);
	simpletext_InsertCharacters(ObjectCode, endpos, "Q", 1);
	nessmark_Set(v, ObjectCode, endpos, 0);
	*loc = v - SysMarkLowEnd;
	return v;	
}

/* makeConst(s)
	make a Ness constant for C string s
	returns an index into SysMark
*/
	TSysMarkRef
makeConst(s)
	char *s;
{
	struct nessmark *v = allocSysMark(0, NULL);

	if (curComp->Locating) return NULL;
	if (v == NULL) return 0;

	nessmark_Initialize(v);
	nessmark_MakeConst(v, s);
	return (v - SysMarkLowEnd);
}


/*BackSlashReduce(text)
	reduce any backslash constructs from the text
	if text ends with backslash, it will be deleted
	return length of resulting text
*/
	long
BackSlashReduce(text)
	register struct text *text;
{
	register long pos, len;
	register unsigned char c;
	len = text_GetLength(text);

	for (pos = 0; pos < len; pos++) {
		c = text_GetChar(text, pos);
		if (c == '\\') {
			char buf[4];
			int elen;
			buf[0] = text_GetChar(text, pos+1);
			buf[1] = text_GetChar(text, pos+2);
			buf[2] = text_GetChar(text, pos+3);
			buf[3] = '\0';
			elen = len - pos;
			if (elen < 3) buf[elen] = '\0';
			c = lexan_TransEscape(buf, &elen);
			text_DeleteCharacters(text, pos, elen+1);
			if (elen > 0) {
				char ch = c;
				text_InsertCharacters(text, pos, &ch, 1);
				len -= elen;
			}
			else len--;
		}
	}
	return len;
}


/* makeStlyedConst(text, pos, len)
	make a Ness constant from the string in 'text' of length 'len'
		starting at 'pos'
	returns an index into SysMark
*/
	TSysMarkRef
makeStyledConst(text, pos, len, bsReduce)
	struct text *text;
	long pos, len;
	boolean bsReduce;
{
	struct nessmark *v;
	struct text *newt;

	if (curComp->Locating) return NULL;
	if (len == 0)
		return makeConst("");

	v = allocSysMark(0, NULL);
	if (v == NULL) return 0;

	nessmark_Initialize(v);
	newt = text_New();
	text_AlwaysCopyTextExactly(newt, 0, text, pos, len);
	if (bsReduce) 
		len = BackSlashReduce(newt);
	text_SetReadOnly(newt, TRUE);
	nessmark_Set(v, newt, 0, len);
	return (v - SysMarkLowEnd);
}

/* makeGlobal()
	make a Ness global variable
	returns an index into SysMark
*/
	TSysMarkRef
makeGlobal()
{
	struct nessmark *v = allocSysMark(0, curComp->ness);

	if (curComp->Locating) return NULL;
	if (v == NULL)  return 0;

	v->header.nessmark_methods = NULL;
	return (v - SysMarkLowEnd);
}


/* addOp(m, op);
	appends operator 'op' to 'm'
*/
	void
addOp(m, op)
	struct nessmark *m;
	unsigned char op;
{
	long pos, len;
	if (curComp->Locating) {compileLocate(1); return;}
	pos = nessmark_GetPos(m);
	len = nessmark_GetLength(m);
	simpletext_InsertCharacters(nessmark_GetText(m), pos+len, &op, 1);
	nessmark_SetLength(m, len+1);
}

/* refStack(m, op, rand)
	append to m the opcode op with the one-byte operand rand 
	return location of the operand as an arg to RememberFixup

	the case of rand>0xFF is tested for in finishfunc 
*/
	TCodeRef
refStack(m, op, rand)
	struct nessmark *m;
	unsigned char op;
	TStackRef rand;
{
	long pos, len;
	unsigned char s[2];
	if (curComp->Locating) {compileLocate(2); return 0;}
	pos = nessmark_GetPos(m);
	len = nessmark_GetLength(m);
	s[0] = op;
	s[1] = rand & 0xFF;
	simpletext_InsertCharacters(nessmark_GetText(m), pos+len, s, 2);
	nessmark_SetLength(m, len+2);
	return (len+1);
}

	void
fixRefStack(m, loc, val)
	struct nessmark *m;
	TCodeRef loc;
	TStackRef val;
{
	if (curComp->Locating)  {compileLocate(1); return;}
	simpletext_ReplaceCharacters(nessmark_GetText(m), nessmark_GetPos(m)+loc,
			sizeof(TStackRef), &val, sizeof(TStackRef));
}

/* refSysMark(m, op, rand)
	append to m the opcode op with the two-byte operand rand
	return location in code of the operand field

	overflow of the address field is covered in the MakeXxx functions above, 
	which do not allocate more than 65535 SysMarks
*/
	TCodeRef
refSysMark(m, op, rand)
	struct nessmark *m;
	unsigned char op;
	TSysMarkRef rand;
{
	long pos, len;
	unsigned char s[3];
	if (curComp->Locating) {compileLocate(3); return 0;}
	pos = nessmark_GetPos(m);
	len = nessmark_GetLength(m);
	s[0] = op;
	s[1] = rand>>8;
	s[2] = rand & 0xFF;
	simpletext_InsertCharacters(nessmark_GetText(m), pos+len, s, 3);
	nessmark_SetLength(m, len+3);
	return len+1;
}

	void
fixRefSysMark(m, loc, val)
	struct nessmark *m;
	TCodeRef loc;
	TSysMarkRef val;
{
	if (curComp->Locating)  {compileLocate(1); return;}
	simpletext_ReplaceCharacters(nessmark_GetText(m), nessmark_GetPos(m)+loc,
			sizeof(TSysMarkRef), &val, sizeof(TSysMarkRef));
}
/* refAddress(m, op, address)
	append to m the opcode op with the four-byte operand rand 
*/
	void
refAddress(m, op, address)
	struct nessmark *m;
	unsigned char op;
	struct object *address;
{
	long pos, len;
	unsigned char s[5];
	if (curComp->Locating)  {compileLocate(5); return;}
	s[0] = op;
	s[1] = ((unsigned long)address)>>24;
	s[2] = ((unsigned long)address)>>16;
	s[3] = ((unsigned long)address)>>8;
	s[4] = ((unsigned long)address) & 0xFF;
	pos = nessmark_GetPos(m);
	len = nessmark_GetLength(m);
	simpletext_InsertCharacters(nessmark_GetText(m), pos+len, s, 5);
	nessmark_SetLength(m, len+5);
}

/* genLinkGlobal(sym)
	attach sym to the global list given by *curComp->ness->AttrDest
*/
	void
genLinkGlobal(sym)
	struct nesssym *sym;
{
	if (curComp->Locating) return;
	sym->next = *curComp->ness->AttrDest;
	*curComp->ness->AttrDest = sym;
}


/* genLocStore(sym)
	store the code location in codelocTable
*/
	void
genLocStore(sym)
	struct nesssym *sym;
{
	if (curComp->Locating) return;
	codelocStore(sym);
}

	static void
RememberFixup(refloc, sym)
	TCodeRef refloc;
	struct nesssym *sym;
{
	struct fixupstackrefnode *f;
	if (curComp->freefixups != NULL) {
		f = curComp->freefixups;
		curComp->freefixups= f->next;
		f->next = curComp->varfixups;
		f->refloc = refloc;
		f->sym = sym;
		curComp->varfixups = f;
	}
	else
		curComp->varfixups = fixupstackrefnode_Create(refloc, sym, curComp->varfixups);
}
	static void
DoFixups(lpsize)
	unsigned long lpsize;
{
	struct fixupstackrefnode *f, *t;
	for (f = curComp->varfixups; f != NULL; f = t) {
		fixRefStack(curComp->curfuncmark, f->refloc, 
			(f->sym == NULL) ? lpsize : nesssym_NGetInfo(f->sym, long));
		t = f->next;
		f->next = curComp->freefixups;
		curComp->freefixups = f;
	}
	curComp->varfixups = NULL;
}

/* allocate (syml, prior)
	set to the proper stack offset the 
		nesssym_GetInfo(s, long) of each s that is a symbol on the list at 'syml'
	begin with offset 'prior'
	Return total size allocated, including 'prior'

	Note that the list of parameters is in reverse order, but that this
	means they are in the order of increasing positive offset from the 
	frame header, which is at a higher location and lower address in the stack.
*/
	static unsigned long 
allocate(syml, prior)
	register struct nesssym *syml;
	register unsigned long prior;
{
	for ( ; syml != NULL; syml = syml->next) {
		nesssym_NSetInfo(syml, long, prior);
		if (syml->type == Tbool)
			prior += sizeof(struct boolstkelt);
		else if (syml->type == Tlong)
			prior += sizeof(struct longstkelt);
		else if (syml->type == Tdbl)
			prior += sizeof(struct dblstkelt);
		else if (syml->type == Tptr)
			prior += sizeof(struct ptrstkelt);
		else
			prior += sizeof(struct nessmark);
	}
	return prior;
}

	
	TSysMarkRef
genEnter()
{
	TSysMarkRef loc;
	curComp->varfixups = NULL;
	curComp->curfuncmark = makeFunction(&loc);
	/* generate: ENTER (sizeof locals)   (check return value in case the offset changes) */
	if (refStack(curComp->curfuncmark, 'P', 0) != ENTERARGOFFSET) 
		ReportError(":URP!", -1);
	return loc;
}

	void
genreturn(hasExpr)
	boolean hasExpr;
{
	if ( ! hasExpr)
		addOp(curComp->curfuncmark, '0');	/* provide zero as the default value */
	RememberFixup(refStack(curComp->curfuncmark, 'Q', 0), NULL);
}

/* genExit(parmlist, locallist)
	generates code at end of function and does fixups
	returns TRUE if offsets are less than 255 and FALSE otherwise
*/
	boolean
genExit(parmlist, locallist)
	struct nesssym *parmlist, *locallist;
{
	long locsize, totsize;
	locsize = allocate(locallist, 0);
	fixRefStack(curComp->curfuncmark, ENTERARGOFFSET, locsize);
	genreturn(FALSE);
	totsize=allocate(parmlist, locsize);
	DoFixups(totsize);
	return (totsize <= 255);
}

	void
startfunc(fname)
	struct nesssym *fname;
{
	nesssym_scopeType curscope = curComp->scopes[curComp->scopex];
	nesssym_NSetINode(fname, funcnode, 
		funcnode_Create(genEnter(), curscope, compNewScope(), 
			ness_CreateMark(curComp->ness,
				tlex_RecentPosition(curComp->tlex, -1, 0), 
				999),
			NULL, NULL, Tstr));
}

	void
finishfunc(fname, locallist)
	struct nesssym *fname, *locallist;
{
	long lastloc, lastlen, srclen, srcloc;
	struct funcnode *func = nesssym_NGetINode(fname, funcnode);

	srcloc = mark_GetPos(func->where);
	lastloc = tlex_RecentPosition(curComp->tlex, 0, &lastlen);
	srclen = lastloc + lastlen - srcloc;
	mark_SetLength(func->where, srclen);
	func->locallist = locallist;
	if ( ! genExit(func->parmlist, locallist))
		SaveError(":More than 10 arguments and locals", srcloc, srclen);
	compPopScope();
	genLocStore(fname);   genLinkGlobal(fname);
}

	void
abortfunc()
{
	/* XXX  gets wrong scope if inside EXTEND...END, but outside a function */
	compPopScope();
}


/* appendlists(list1, list2)
	Concatenate the symbol lists 'list1' and 'list2'
	The lists are in _reverse_ order, so put 'list2' before 'list1'.
*/
	struct nesssym *
appendlists(list1, list2)
	register struct nesssym *list1, *list2;
{
	register struct nesssym *lastElt, *t;
	if (list2 == NULL) return list1;
	if (list1 == NULL) return list2;

	lastElt = list2;
	while ((t=lastElt->next) != NULL)
		lastElt = t;
	lastElt->next = list1;	
	return list2;
}

	struct exprnode *
genarith(op, left, right)
	unsigned char op;
	struct exprnode *left, *right;
{
	struct exprnode *r;
	if (left->type == Tdbl) {
		demandnodetype(right, Tdbl);
		r = exprnode_Combine(left, right, Tdbl);
		if (op == '%') {
			ExprError(":% is not defined for real values", r);
			return r;
		}
		genop('M');
	}
	else {
		demandnodetype(left, Tlong);
		demandnodetype(right, Tlong);
		r = exprnode_Combine(left, right, Tlong);
	}
	genop(op);
	return r;
}

	void
gencomp(left, right)
	struct exprnode *left, *right;
{
	if (left->type != right->type) 
		SaveError(":comparison of different types", left->loc,
				right->loc+right->len - left->loc);
	else if (left->type == Tstr)
		{}
	else if (left->type == Tbool) 
		addOp(curComp->curfuncmark, 'B');
	else if (left->type == Tptr) 
		addOp(curComp->curfuncmark, 'V');
	else if (left->type == Tlong) 
		addOp(curComp->curfuncmark, 'I');
	else if (left->type == Tdbl) 
		addOp(curComp->curfuncmark, 'R');
	else 
		SaveError(":comparison is not implemented for this type",
				left->loc, right->loc+right->len - left->loc);
	addOp(curComp->curfuncmark, 't');
}

	void
genvarref(var)
	struct nesssym *var;
{
	if ((var->flags & flag_undef) != 0) 
		ReportError(":refers to undefined var", -1);
	else if (var->flags & flag_builtin)
		/* pre-defined identifier */
		callPredefId(var);
	else {
		if (var->type == Tbool)
			addOp(curComp->curfuncmark, 'B');
		else if (var->type == Tptr)
			addOp(curComp->curfuncmark, 'V');
		else if (var->type == Tlong)
			addOp(curComp->curfuncmark, 'I');
		else if (var->type == Tdbl)
			addOp(curComp->curfuncmark, 'R');
		else if (var->type != Tstr)
			ReportError(":cannot load variables of this type", -1);
		if (var->flags & flag_globalvar)
			refSysMark(curComp->curfuncmark, 'k', nesssym_NGetInfo(var, long));
		else
			RememberFixup(refStack(curComp->curfuncmark, 'l', 0), var);
	}
}
	void
genvarstore(varnode)
	struct varnode *varnode;
{
	struct nesssym *var = varnode->sym;
	if (var->type == Tbool)
		addOp(curComp->curfuncmark, 'B');
	else if (var->type == Tptr)
			addOp(curComp->curfuncmark, 'V');
	else if (var->type == Tlong)
			addOp(curComp->curfuncmark, 'I');
	else if (var->type == Tdbl)
			addOp(curComp->curfuncmark, 'R');
	else if (var->type != Tstr)
		SaveError(":cannot store into variables of this type", 
					varnode->loc, varnode->len);
	if (var->flags & flag_globalvar)
		refSysMark(curComp->curfuncmark, 'v', nesssym_NGetInfo(var, long));
	else
		RememberFixup(refStack(curComp->curfuncmark, 's', 0), var);
	varnode_Destroy(varnode);
}

	struct varnode *
genvarnode(sym)
	struct nesssym *sym;
{
	long loc, len;
	if ((sym->flags & (flag_function | flag_var)) == 0)
		sym->flags |= flag_undef;
	loc = tlex_RecentPosition(curComp->tlex, 0, &len);
	return varnode_Create(loc, len, sym, NULL);
}

	void
genconstref(constant)
	struct nesssym *constant;
{
		if (constant->type == Tlong)
			addOp(curComp->curfuncmark, 'I');
		else if (constant->type == Tdbl)
			addOp(curComp->curfuncmark, 'R');
		else if (constant->type != Tstr)
			ReportError(":cannot load constants of this type", 0);
	refSysMark(curComp->curfuncmark, 'k', nesssym_NGetInfo(constant, long));
}

	void
genop(op)
	unsigned char op;
{
	addOp(curComp->curfuncmark, op);
}


/* = = = = = = = = = = = = = = = = = = =
 *    Branch Instructions
 * = = = = = = = = = = = = = = = = = = */
/* setcurfuncmark(m)
	set 'curComp->curfuncmark' to 'm'
	Use This ONLY for Testing 
	and nevent
*/
	void
setcurfuncmark(m)
	struct nessmark *m;
{
	curComp->curfuncmark = m;
}
/* getcurfuncmark()
	return current value of curfuncmark
	used ONLY by call.c
*/
	struct nessmark *
getcurfuncmark()
{
	return curComp->curfuncmark;
}



/*
	The operand for a branch opcode is the offset from the opcode to the target.
*/

/* genlabel()
	returns the current location in the object code 
*/
	long
genlabel()
{
	if (curComp->Locating) return 0;
	return nessmark_GetLength(curComp->curfuncmark);
}

/* genbranch(op, dest)
	emits to the object code a branch with opcode 'op' to location 'dest'
	returns the location of the opcode of the branch
*/
	long
genbranch(op, dest)
	unsigned char op;
	long dest;
{
	long branchloc = genlabel();
	unsigned char buf[3];
	short t = dest - branchloc;	/* compute offset */
	long len;

	if (curComp->Locating) {compileLocate(3); return 0;}
	len = nessmark_GetLength(curComp->curfuncmark);

	buf[0] = op;
	buf[1] = ((unsigned short) t) >> 8;
	buf[2] = ((unsigned short) t) & 0xFF;
	simpletext_InsertCharacters(nessmark_GetText(curComp->curfuncmark),
			nessmark_GetPos(curComp->curfuncmark)+branchloc, buf, 3);
	nessmark_SetLength(curComp->curfuncmark, len+3);	
	return branchloc;
}

/* fixbranch(loc)
	fixes up the branch op at 'loc' to branch to next generated opcode
*/
	void
fixbranch(loc)
	long loc;
{
	long dest = genlabel();
	short t = dest - loc;		/* compute offset */
	unsigned char buf[2];
	if (curComp->Locating) return;
	buf[0] = ((unsigned short) t) >> 8;
	buf[1] = ((unsigned short) t) & 0xFF;
	simpletext_ReplaceCharacters(nessmark_GetText(curComp->curfuncmark), 
			nessmark_GetPos(curComp->curfuncmark)+loc+1, 2, buf, 2);
}



/* = = = = = = = = = = = = = = = = = = =
 *    Predicate Engine
 * = = = = = = = = = = = = = = = = = = */

/* state variables for predicate engine

	The predicate state is saved in a predstatenode having
		fixuploc - fixup loc (for ELIF)
		objloc - current location in object code (for WHILE)
		loc - the beginning of an expression (for compileLocate)
		targetlist - the prior curComp->predtargetlist
		dropthrulist - the prior curComp->preddropthrulist 
		cond - the prior curComp->predcond value 
		construct - a code saying what construct this is in  (see compdefs.hn)
*/

/* predpush(cond, loc, construct)
	Package the current predicate state in a predstatenode and 
	initialize for a new predicate, setting curComp->predcond from 'cond'
	save loc in predstack for compileLocate
*/
	struct predstatenode *
predpush(cond, loc, construct)
	boolean cond;
	long loc;
	char construct;
{
	if (pssp - predstack >= MAXEXPRDEPTH - 1) {
		ReportError(":expression is too complicated", 0);
		return NULL;
	}
	pssp ++;
	pssp->fixuploc = 0;
	pssp->objloc = genlabel();
	pssp->loc = loc;
	pssp->targetlist = curComp->predtargetlist;
	pssp->dropthrulist = curComp->preddropthrulist;
	pssp->cond = curComp->predcond;
	pssp->construct = construct;

	/* establish new conditions */
	curComp->predtargetlist = NULL;
	curComp->preddropthrulist = NULL;
	curComp->predcond = cond;		/* store new cond */
	return pssp;
}

	static void
appendgotolists(list, addon)
	struct gotonode **list, *addon;
{
	if (*list == NULL) *list = addon;
	else if (addon == NULL) {}
	else {
		register struct gotonode *t = *list;
		while (t->next != NULL) t = t->next;
		t->next = addon;
	}
}

/* predpop()
	restore the state of the predicate engine from the fields of 'state'
*/
	void
predpop()
{
	if (pssp < predstack) {
		ReportError(":COMPILER ERROR: expression stack underflow", 0);
		return;
	}
	curComp->predcond = pssp->cond;
	appendgotolists(&curComp->predtargetlist, pssp->targetlist);
	appendgotolists(&curComp->preddropthrulist, pssp->dropthrulist);
	pssp--;
}

/* predvalue(Ptype)
	Emit code to generate a value from a branching predicate
*/
	void
predvalue(Ptype)
	Texpr *Ptype;
{
	long branchloc;

	if (*Ptype != Tbra) 
		return;		/* it is already a value */
	/* convert branching to a value */
	predfixdropthru();
	addOp(curComp->curfuncmark, '1');		/* load TRUE */
	branchloc = genbranch('g', 0);
	predfixtarget();
	addOp(curComp->curfuncmark, '9');		/* load FALSE */
	fixbranch(branchloc);
	*Ptype = Tbool;
}
/* predbranch(expr)
	Emit code to convert boolean value to branching
*/
	void
predbranch(expr)
	struct exprnode *expr;
{
	if (expr->type == Tbra) 
		return;		/* it is already a branching predicate */
	else if (expr->type != Tbool) {
		ExprError(":expression should be of type Boolean", expr);
		return;
	}
	addOp(curComp->curfuncmark, 'T');	/* generate a comparison */
	predtarget(predEQ);	/* cmp to TRUE and branch */
}

/*
	An element of curComp->preddropthrulist or curComp->predtargetlist is a gotonode having:
		gotoloc - the location to be patched
		next - the next element on the list
*/
/* preddropthru(rop)
	Generates a branch and put a fixup for it on 'preddropthru' list
	opcode depends on curComp->predcond: 'topTbl[rop]' for TRUE, 'fopTbl[rop]' for FALSE
*/
	void
preddropthru(rop)
	long rop;
{
	unsigned char opcode = (curComp->predcond) ? TopTbl[rop] : FopTbl[rop];
	if (opcode == '\n')  return;
	curComp->preddropthrulist = gotonode_Create(genbranch(opcode, 0), curComp->preddropthrulist);
}

/* predtarget(rop)
	Generates a branch and put a fixup for it on 'predtarget' list
	opcode depends on curComp->predcond: 'topTbl[rop]' for TRUE, 'fopTbl[rop]' for FALSE
*/
	void
predtarget(rop)
	long rop;
{
	unsigned char opcode = (curComp->predcond) ? TopTbl[rop] : FopTbl[rop];
	if (opcode == '\n')  return;
	curComp->predtargetlist = gotonode_Create(genbranch(opcode, 0), curComp->predtargetlist);
}


/* fixbranchlist(list)
	generate fixups for all locations listed on 'list'
	cause them to branch to next opcode generated
	Discard list elements.
*/
	static void
fixbranchlist(list)
	struct gotonode * list;
{
	while (list != NULL) {
		struct gotonode *next = list->next;
		fixbranch(list->gotoloc);
		gotonode_Destroy(list);
		list = next;
	}
}

/* predfixdropthru()
	Fixes all branches on list 'curComp->preddropthrulist' to branch to 
	current position in object code.
*/
	void
predfixdropthru()
{
	fixbranchlist(curComp->preddropthrulist);
	curComp->preddropthrulist = NULL;
}

/* predfixtarget()
	Fixes all branches on list 'curComp->predtargetlist' to branch to 
	current position in object code.
*/
	void
predfixtarget()
{
	fixbranchlist(curComp->predtargetlist);
	curComp->predtargetlist = NULL;
}

/* predexit(construct)
	generate a branch to the 'target' location for the enclosing construct
	construct value must be 'I' for if or 'W' for while
	The curComp->predtargetlist we want may be the current one or the one on the stack
	in the frame one more recent than the most recent instance of 'construct'.
*/
	void
predexit(construct)
	char construct;
{
	struct gotonode **targetlist;
	struct predstatenode *sp;

	if (pssp->construct == construct)
		targetlist = &curComp->predtargetlist;
	else {
		/* scan the stack for desired construct  
			when the targetlist is empty, we must be in the Elsepart already*/
		for (sp = pssp-1; sp >= predstack; sp--) 
			if (sp->construct == construct && (sp+1)->targetlist != NULL)
				break;
		if (sp >= predstack)
			targetlist = &((sp+1)->targetlist);
		else {
			ReportError(":Not within a suitable context", 0);
			return;
		}
	}
	*targetlist = gotonode_Create(genbranch('g', 0), *targetlist);
}


/*  = = = = = = = = = = = = = = = = = = =
 *    symbols and variables
 * = = = = = = = = = = = = = = = = = =*/

/*demandsymboltype(sym, type) 
	Check that the symbol is of the required type 
	and give an error message if not.
	Cannot generate coercions because the symbol is not on the stack.
*/
	void
demandsymboltype(sym, type)
	struct nesssym *sym;
	Texpr type;
{
	if (sym->type != type)
		ReportError(":wrong type of variable", -1);
} 

/*demandnodetype(node, type, tokloc) 
	Check that the exprnode 'node' specifies the required 'type'.
	and give an error message if not.
	
*/
	void
demandnodetype(node, type)
	struct exprnode *node;
	Texpr type;
{
	if (node->type != type)
		ExprError(":expression is of the wrong type", node);
}

	struct varnode *
varIsStorable(var)
	struct varnode *var;
{
	struct nesssym *sym = var->sym;
	if ((sym->flags & (flag_var | flag_undef | flag_builtin)) != flag_var) {
		if (sym->flags & flag_function)
			SaveError(":cannot assign to function name", 
					var->loc, var->len);
		else if (sym->flags & flag_builtin)
			SaveError(":cannot assign to predefined name", 
					var->loc, var->len);
		else
			SaveError(":refers to an undefined variable", 
					var->loc, var->len);
	}
	return var;
}

	struct varnode *
varIsFunction(var)
	struct varnode *var;
{
	struct nesssym *sym = var->sym;
	if ((sym->flags & (flag_var | flag_const)) != 0) {
		ReportError(":not a proper function name", -1);
		var->sym = NULL;
	}
	else if (sym->flags == flag_undef) 
		/* unknown name:  assume it is a forward function call 
			or a call to an external function */
		callUnknown(sym);
	return var;
}

	struct nesssym *
uniqueinscope(var, flags, tokoff)
	struct nesssym* var;
	unsigned long flags;
	long tokoff;
{
	static long junknum = 91;
	 /* if var->flags is not zero, we have some old symbol
		and need to redefine it.
		XXX How better to know if previously defined within this function???
	*/
	if (var->flags != 0 && nesssym_NGetScope(var) 
					== curComp->scopes[curComp->scopex]) {
		/* redefining a symbol already defined in this function */
		char buf[12];
		ReportError(":previously defined", tokoff);
		sprintf(buf, "X:%d", junknum++);
		var = nesssym_NDefine(buf, var, 
				curComp->scopes[curComp->scopex]); 
	}
	else if (var->flags != 0) {
		/* redefining something global, use curScope to avoid conflict  */
		var = nesssym_NDefine(nesssym_NGetName(var), var,
				curComp->scopes[curComp->scopex]); 
		var->header.toksym.toknum = curComp->idtok;
	} 
	var->flags = flags; 
	return var;
}

/* process a list of id's being declared.  
	set flags and kind to given values. 
*/
	void
ProcessIdList(type, idlist, flags)
	Texpr type;
	struct nesssym *idlist;
	long flags;
{
	struct nesssym *id;
	for (id = idlist; id != NULL; id = id->next) {
		id-> flags = flags;
		id->type = type;
	}
}


#if 0
/* this function is no longer used.
		[] and {} are no longer mapped to ()
		any way, ness.tlx does not return symbols for constant tokens
*/
	void
parencheck(left, right)
	struct varnode *left;
	struct toksym *right;
{
	char l;
	char r; 
	if(l==NULL || right==NULL) {
	    fprintf(stderr, "NULL encountered in parencheck.\n");
	    return;
	}
	l= *left->paren->header.sym.name;
	r= *right->header.sym.name;
	if ( ! ((l == '(' && r == ')')  ||  (l == '[' && r == ']')  ||  (l == '{' && r == '}'))) {
		/* mismatched parens */
		long loc, len;
		loc = tlex_RecentPosition(curComp->tlex, 0, &len);
		len = loc + len - left->loc;
		loc = left->loc;
		SaveError(":mismatched parentheses", loc, len);
	}	
}
#endif


/* genCheckEndtag(tag, desired)
	checks that the tag is the one desired
	pop the predstate stack to get to the desired one
		or until get to function or extend
*/
	void
genCheckEndtag(tag, desired)
	struct nesssym *tag;
	long desired;
{
	long tagtok;
	char *desiredname;
	char buf[300];
	char constructForTag;

	tagtok = tag->header.toksym.toknum;
	if (tagtok == desired)
		return;
	if (desired == ntON && (tagtok == ntEVENT  ||  tagtok == ntMENU  
				||  tagtok == ntMOUSE || tagtok == ntKEYS))
		return;	/* will be dealt with in nevent */

	if (desired == ntFUNCTION) {
		constructForTag = 'L';  desiredname = "function";
	}
	else if (desired == ntEXTEND) {
		constructForTag = 'X';  desiredname = "extend";
	}
	else if (desired == ntIF) {
		constructForTag = 'I';  desiredname = "if";
	}
	else if (desired == ntWHILE) {
		constructForTag = 'W';  desiredname = "while";
	}
	else if (desired == ntON) {
		constructForTag = 'V';  desiredname = NULL;
	}
		/* error message for ON will come from neventFinishEvent */

	if (desiredname != NULL)  {
		sprintf(buf, "*Should be:   end %s.  Trying to fix by inventing ends.\n", 
				desiredname);
		ReportError(freeze(buf), -1);
	}
	while (pssp->construct != constructForTag
			&& pssp->construct != 'X'
			&& pssp->construct != 'L')
		predpop();
}


/* the next two functions are used by the grammar to save 
	the function state for the global init function 
	They provide only ONE level of save restore. 
*/
	void
genSaveFuncState()
{
	curComp->savedCurfuncmark = curComp->curfuncmark;
	curComp->savedVarFixups = curComp->varfixups;
	curComp->LocationAdvancing = ! curComp->LocationAdvancing;
	compPopScope();	/* scope is saved in the funcnode */
}
	void
genRestoreFuncState(func)
	 struct nesssym *func;
{
	curComp->curfuncmark = curComp->savedCurfuncmark;
	curComp->varfixups = curComp->savedVarFixups;
	curComp->LocationAdvancing = ! curComp->LocationAdvancing;
	compPushScope(nesssym_NGetINode(func, funcnode)->ownscope);
}

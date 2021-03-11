/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
\* ********************************************************************** */

/*
	$Disclaimer: 
*Permission to use, copy, modify, and distribute this software and its 
*documentation for any purpose is hereby granted without fee, 
*provided that the above copyright notice appear in all copies and that 
*both that copyright notice, this permission notice, and the following 
*disclaimer appear in supporting documentation, and that the names of 
*IBM, Carnegie Mellon University, and other copyright holders, not be 
*used in advertising or publicity pertaining to distribution of the software 
*without specific, written prior permission.
*
*IBM, CARNEGIE MELLON UNIVERSITY, AND THE OTHER COPYRIGHT HOLDERS 
*DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
*ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.  IN NO EVENT 
*SHALL IBM, CARNEGIE MELLON UNIVERSITY, OR ANY OTHER COPYRIGHT HOLDER 
*BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
*DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, 
*WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS 
*ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
*OF THIS SOFTWARE.
* $
*/





/*****************************************************************************\
\*****************************************************************************/


/* nesssym.H
 *
 * A nesssym object is a subclass of toksym.  Its adds these fields:
 *	
 *	next	- for linking syms in a list
 *	
 *
 */

#include "toksym.ih"

typedef long nesssym_scopeType;
#define nesssym_GLOBAL toksym_GLOBAL

typedef unsigned long Texpr;

class nesssym : toksym
{

overrides:

macros:

	NNewScope(enclosingScope) ((nesssym_scopeType)  \
			toksym_TNewScope ((toksym_scopeType)(enclosingScope)))
	NDestroyScope(scope) (toksym_TDestroyScope((toksym_scopeType)(scope)))
	NParentScope(scope) ((nesssym_scopeType)  \
			toksym_TParentScope((toksym_scopeType)(scope)))
	NDefine(name, proto, scope) ((struct nesssym *)toksym_TDefine(name, proto, scope))
	NUndefine(name, scope) (toksym_TUndefine(name, scope))
	NFind(name, scope) ((struct nesssym *)toksym_TFind(name, scope))
	NLocate(name, proto, scope, new) \
			((struct nesssym *)toksym_TLocate(name, proto, scope, new))
	NFindAll(name, scope, proc, rock) (toksym_TFindAll(name, scope, proc, rock))

macromethods:

	NGetName()	((unsigned char *)(self)->header.sym.name)
	NGetScope()	((nesssym_scopeType)(self)->header.sym.scope)
	NGetINode(type)	((struct type *)((self)->header.toksym.info.node))
	NGetInfo(type)	((type)((self)->header.toksym.info.intval))
	NSetINode(type, val)	((self)->header.toksym.info.node  \
					= (struct node *)(struct type *)(val))
	NSetInfo(type, val)	((self)->header.toksym.info.intval = (long)(type)(val))

classprocedures:

	InitializeObject(/* struct classhdr *ClassID, */ struct nesssym *self) returns boolean;
	FinalizeObject(/* struct classhdr *ClassID, */ struct nesssym *self);

data:

	struct nesssym *next;
	union {		struct ness *ness;  
			struct nesssym *nesssym;
			struct node *node; 
		} parent;
	Texpr type;
	long flags;	/* see compdefs.hn */

};



/*
 *    $Log: nesssym.ch,v $
*Revision 1.10  1993/05/04  01:23:55  susan
*RCS Tree Split
*
*Revision 1.9.1.1  1993/02/02  03:03:47  rr2b
*new R6tape branch
*
*Revision 1.9  1992/12/14  23:20:33  rr2b
*add $Logs back after disclaimerization took them out
*
Revision 1.7  1992/11/26  02:38:01  wjh
converted CorrectGetChar to GetUnsignedChar
moved ExtendShortSign to interp.h
remove xgetchar.h; use simpletext_GetUnsignedChar
nessrun timing messages go to stderr
replaced curNess with curComp
replaced genPush/genPop with struct compilation
created compile.c to handle compilation
moved scope routines to compile.c
converted from lex to tlex
convert to use lexan_ParseNumber
truncated logs to 1992 only
use bison and gentlex instead of yacc and lexdef/lex

.

Revision 1.6  91/09/12  19:44:21  bobg
Update copyright notice

Revision 1.5  1989/09/03  22:46:25  wjh
newness

Revision 1.4  89/06/01  15:58:12  wjh
campus release version

Revision 1.1  88/10/21  11:00:48  wjh
Initial revision
 
 * Revision 1.0  88/07/14  08:37:00  WJHansen
 * Copied from toksym.H
 */

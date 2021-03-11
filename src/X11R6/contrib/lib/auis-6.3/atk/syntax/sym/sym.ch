/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
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
\* ********************************************************************** */
#if !defined(lint) && !defined(LOCORE) && defined(RCS_HDRS)
static char *sym_H_rcsid = "$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/syntax/sym/RCS/sym.ch,v 1.11 1993/05/04 01:34:45 susan Exp $";
#endif


/* sym.c
 *
 * A sym object is an entry in a symbol table.  Each symbol has associated with it a uniqe
 * name and scope.  sym should be subclassed if any additional data is desired.
 *
 * History:
 *
 *	5-24-88 Fred Hansen:	original symbol table module created.
 *	6-29-88 George Baggott:	rewritten as an object.
 *	7-07-88 George Baggott:	no longer a subclass of node.
 *	7-13-88 George Baggott:	partitions axed.
 */
/*
 *    $Log: sym.ch,v $
*Revision 1.11  1993/05/04  01:34:45  susan
*RCS Tree Split
*
*Revision 1.10.1.1  1993/02/02  04:13:08  rr2b
*new R6tape branch
*
*Revision 1.10  1992/12/14  20:57:48  rr2b
*disclaimerization
*
Revision 1.9  1992/11/26  02:02:58  wjh
updated header
.

Revision 1.8  92/06/05  16:50:26  rr2b
added support for proper destruction of symbols
. . .
Revision 1.1  88/10/21  10:42:15  wjh
Initial revision
 
 * Revision 1.0  88/06/23  12:15:00  gb17
 * Copied from /usr/andrew/lib/nullinset
 */


typedef long sym_ScopeType;

#define sym_GLOBAL ((sym_ScopeType) 0L)

class sym
{

overrides:

methods:

macromethods:

	GetName()		((self)->name)

	GetScope()		((self)->scope)
	SetScope(newScope)	((self)->scope = (newScope))

classprocedures:

	InitializeClass(/* struct classhdr *ClassID*/) returns boolean;
	InitializeObject(/* struct classhdr *ClassID, */ struct sym *self) returns boolean;
	FinalizeObject(/* struct classhdr *ClassID, */ struct sym *self);

	NewScope(sym_ScopeType enclosingScope) returns sym_ScopeType;
		/* creates a new scope in enclosingScope
		 */

	DestroyScope(sym_ScopeType scope);
		/* destroys all the symbols and scopes defined in the given scope
		 */

	ParentScope(sym_ScopeType scope) returns sym_ScopeType;
		/* returns the enclosing scope for the given scope 
		 */

	Define(unsigned char *name, struct sym *proto, sym_ScopeType scope) returns struct sym*;
		/* defines the symbol (name, scope).  If it already exists, NULL is
		 * returned.
		 */

	Undefine(unsigned char *name, sym_ScopeType scope) returns boolean;
		/* undefines (name, scope).  Returns TRUE if successful,
		 * false otherwise.
		 */

	Find(unsigned char *name, sym_ScopeType scope) returns struct sym*;
		/* attempts to find a match for (name, scope).  If one is found
		 * it is returned.  If not, then NULL is returned.
		 */

	Locate(unsigned char *name, struct sym *proto, sym_ScopeType scope, boolean *new)
	          returns struct sym*;
		/* attempts to find a match for (name, scope).  If one is found, 
		 * it is returned.  If not, (name, scope) is defined.
		 */

	FindAll(unsigned char *name, sym_ScopeType scope, long (*proc)(), long rock) returns long;
		/* finds all of name's matches that are defined in scope or any of scope's
		 * descendent scopes.  For each match, proc is called with sym and rock.
		 * proc should be declared as:
		 *
		 * 	long proc(sym, rock);
		 *	sym_partition part;
		 *	struct sym *sym;
		 *	long rock;
		 *
		 * if a call to proc returns a value other than NULL, the search
		 * is terminated, and FindAll will return that value.  Otherwise,
		 * FindAll will return NULL.
		 */

	printtable();

data:
	unsigned char *name;		/* the name of the symbol */
	long scope;		/* the scope in which the symbol is defined */
	struct sym *next;		/* used for handling collisions in hash table */
	boolean intable; /* whether this sym has been put in the table or not */
};


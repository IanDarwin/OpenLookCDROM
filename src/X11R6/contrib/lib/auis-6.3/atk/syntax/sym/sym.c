/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
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
#if !defined(lint) && !defined(LOCORE) && defined(RCS_HDRS)
static char *rcsid = "$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/syntax/sym/RCS/sym.c,v 1.12 1993/05/04 01:34:45 susan Exp $";
#endif

/* sym.c		

	Code for the sym object
*/
/*
 *    $Log: sym.c,v $
 * Revision 1.12  1993/05/04  01:34:45  susan
 * RCS Tree Split
 *
 * Revision 1.11.1.1  1993/02/02  04:13:17  rr2b
 * new R6tape branch
 *
 * Revision 1.11  1992/12/14  20:57:48  rr2b
 * disclaimerization
 *
 * Revision 1.10  1992/11/26  02:02:58  wjh
 * updated header
 * .
 *
 * Revision 1.9  92/06/05  16:50:26  rr2b
 * added support for proper destruction of symbols
. . .  
 * Revision 1.0  88/06/23  12:15:01  gb17
 * Copied from /usr/andrew/lib/nullinset
 */

/*****************************************************************************\
\*****************************************************************************/



#include <class.h>
#include <sym.eh>

#include <andrewos.h>

/*****************************************************************************\
 *
 * CLASS CONSTANTS
 *
\*****************************************************************************/

#define	initialScopes	    4		/* initial size of enclosing scope table   */

#define	SCOPE_NULL	    -1		/* indicates a NULL scope		   */
#define	SCOPE_FREE	    -2		/* indicates a free slot in a table          */


/*****************************************************************************\
 *
 * CLASS DATA
 *
\*****************************************************************************/

static long maxScopes, nextFreeScope, *enclosingScope;
/* this table gives the enclosing scope for each scope.  maxScopes is the size of the
 * table, and nextFreeScope is the index of the lowest numbered free slot in the
 * table.
 */

static long noOfEntries;
static struct sym **table;
/* the hash table.  tableSize gives the size of the table, noOfEntries
 * gives the number of entries in the table, and table is the table itself.
 */

static long primes[] = {5, 11, 17, 37, 67, 131,
                        257, 521, 1031,2053, 4099,8209, 16411,  32771,
                        65537, 131101, 262147, 524309,1048583, 2097169,
                        4194319, 8388617, 16777259, NULL };
static long *tableSize;
/* this is the sequence of prime numbers used for determining the tableSize.
 */

/*****************************************************************************\
 *
 * PRIVATE FUNCTIONS
 *
\*****************************************************************************/

static long
hash(name)
unsigned char *name;
{
    register unsigned long val;
    register unsigned char *pos;

    for (val = 0, pos=name; *pos; ++pos)
	val = ((val<<5) | (val>>27)) ^ *pos;

    return val % *tableSize;
}

static struct sym**
lookup(name, scope, found)
char *name;
long scope;
boolean *found;
{
    register struct sym **s, **start = table+hash(name);

    while (scope != SCOPE_NULL) {

	/* check all the other valid scopes for the symbol, working outward */

	for(s = start; *s != NULL; s = &((*s)->next)) {

	    /* look for name in the current scope, and return if found */

	    if (strcmp(name, (*s)->name) == 0 && scope == (*s)->scope) {
		*found = TRUE;
		return (s);
	    }
	}

	scope = enclosingScope[scope];

    };

    *found = FALSE;
    return s;
}

static struct sym**
lookupInScope(name, scope, found)
char *name;
long scope;
boolean *found;
{
    register struct sym **s;

    if (name == NULL) {
	*found = FALSE;
	return NULL;
    }
    for(s = table+hash(name); *s != NULL; s = &((*s)->next)) {

	/* look for name in the current scope, and return if found */

	if (strcmp(name, (*s)->name) == 0 && scope == (*s)->scope) {
	    *found = TRUE;
	    return (s);
	}
    }

    *found = FALSE;
    return s;
}

static void
insert(self)
struct sym *self;
{
    boolean found;
    struct sym** loc = lookupInScope(self->name, self->scope, &found);

    ++noOfEntries;
    self->next = *loc;
    *loc = self;
}

static void
resizeTable()
{
    struct sym **old = table;
    long oldSize = *tableSize;
    struct sym *pos, *next;
    long i;

    ++tableSize;
    table = (struct sym **) malloc(sizeof(struct sym*) * *tableSize);
    noOfEntries = 0;

    for (i = 0; i < *tableSize; ++i)
	table[i] = NULL;

    for (i = 0; i < oldSize; ++i)
	for (pos = old[i]; pos != NULL;) {
	    next = pos->next;
	    insert(pos);
	    pos = next;
	}

    free(old);
}

static void
resizeArray(a, from, to)
long **a, from, to;
{
    long *new = (long*) malloc(sizeof(long) * to);
    long i;

    for (i = 0; i < from; ++i)
	new[i] = (*a)[i];

    for (i = from; i < to; ++i)
	new[i] = SCOPE_FREE;
    free(*a);
    *a = new;
}

static void
removeScopeFromScopes(scope)
sym_ScopeType scope;
{
    register long i;
 
    for (i = 0; i < maxScopes; ++i)
	if (enclosingScope[i] == scope)
	    removeScopeFromScopes(i);

    enclosingScope[scope] = SCOPE_FREE;
    if (scope < nextFreeScope)
	nextFreeScope = scope;
}

/*****************************************************************************\
 *
 * CLASS METHODS
 *
\*****************************************************************************/

sym_ScopeType
sym__NewScope(ClassID, scope)
struct classhdr *ClassID;
sym_ScopeType scope;
{
    long new = nextFreeScope;

    if (nextFreeScope == maxScopes-1) {
	resizeArray(&enclosingScope, maxScopes, 2*maxScopes);
	maxScopes *= 2;
    }

    enclosingScope[new] = scope;

    do
	++nextFreeScope;
    while (enclosingScope[nextFreeScope] != SCOPE_FREE);

    return new;
}

void
sym__DestroyScope(ClassID, scope)
struct classhdr *ClassID;
sym_ScopeType scope;
{
    register long i, s;
    register struct sym **pos;

    for (i = 0; i < *tableSize; ++i)
	for (pos = &table[i]; *pos != NULL;) {
	    for(s = (*pos)->scope; s != scope && s != SCOPE_NULL;)
		s = enclosingScope[s];

	    if (s == scope) {
		struct sym *trash = *pos;

		*pos = trash->next;
		free(trash->name);
		trash->name = NULL;
		sym_Destroy(trash);
	    } else
		pos = &((*pos)->next);
	}

    removeScopeFromScopes(scope);
}

sym_ScopeType
sym__ParentScope(ClassID, scope)
struct classhdr *ClassID;
sym_ScopeType scope;
{
	return enclosingScope[scope];
}


struct sym*
sym__Define(ClassID, name, proto, scope)
struct classhdr *ClassID;
char *name;
struct sym *proto;
sym_ScopeType scope;
{
    boolean found;
    struct sym *newSym, **loc;

    if (noOfEntries > *tableSize)
	resizeTable();

    loc = lookupInScope(name, scope, &found);

    if (found)
	return NULL;
    else {

	newSym = proto == NULL ? sym_New() : sym_NewFromObject(proto);
	++noOfEntries;

	newSym->name = (unsigned char *)malloc(strlen(name)+1);
	strcpy(newSym->name, name);
	newSym->scope = scope;
	newSym->next = *loc;
	newSym->intable= TRUE;
	*loc = newSym;
	return newSym;
    }
}


boolean
sym__Undefine(ClassID, name, scope)
struct classhdr *ClassID;
char *name;
sym_ScopeType scope;
{
    boolean found;
    struct sym **loc = lookupInScope(name, scope, &found);

    if (found) {
	struct sym *trash = *loc;

	*loc = trash->next;
	free(trash->name);
	trash->name = NULL;
	sym_Destroy(trash);
	return TRUE;
    }
    else
	return FALSE;
}


struct sym*
sym__Find(ClassID, name, scope)
struct classhdr *ClassID;
char *name;
sym_ScopeType scope;
{
    boolean found;
    struct sym **loc = lookup(name, scope, &found);

    if (found)
	return *loc;
    else
	return NULL;
}

struct sym*
sym__Locate(ClassID, name, proto, scope, new)
struct classhdr *ClassID;
char *name;
struct sym *proto;
sym_ScopeType scope;
boolean *new;
{
    boolean found;
    struct sym *newSym, **loc;

    if (noOfEntries > *tableSize)
	resizeTable();

    loc = lookup(name, scope, &found);

    if (found) {
	*new = FALSE;
	return *loc;
    } else {
	newSym = proto == NULL ? sym_New() : sym_NewFromObject(proto);
	++noOfEntries;

	newSym->name = (unsigned char *)malloc(strlen(name)+1);
	strcpy(newSym->name, name);
	newSym->scope = scope;
	newSym->next = *loc;
	newSym->intable = TRUE;
	*loc = newSym;
	*new = TRUE;
	return newSym;
    }
}

long
sym__FindAll(ClassID, name, scope, proc, rock)
struct classhdr *ClassID;
char *name;
sym_ScopeType scope;
long (*proc)();
long *rock;
{
    register long i, s;
    register struct sym **pos;

    for (i = 0; i < *tableSize; ++i)
	for (pos = &table[i]; *pos != NULL;) {
	    for(s = (*pos)->scope; s != scope && s != SCOPE_NULL;)
		s = enclosingScope[s];

	    if (s == scope && strcmp(name, (*pos)->name) == 0) {
		long val = proc(*pos, rock);

		if (val != NULL)
		    return val;
	    }
	    pos = &((*pos)->next);
	}
}


boolean
sym__InitializeClass(ClassID)
struct classhdr *ClassID;
{
    long i;

    maxScopes = initialScopes;
    enclosingScope = (long *) malloc(sizeof(long) * maxScopes);
    for (i = 1; i < maxScopes; ++i)
	enclosingScope[i] = SCOPE_FREE;
    enclosingScope[sym_GLOBAL] = SCOPE_NULL;

    nextFreeScope = 1;

    tableSize = primes;
    table = (struct sym **) malloc(sizeof(struct sym*) * *tableSize);
    for(i = 0; i < *tableSize; ++i)
	table[i] = NULL;
    noOfEntries = 0;

    return TRUE;
}


boolean
sym__InitializeObject(ClassID, self)
struct classhdr *ClassID;
struct sym *self;
{
    self->name = NULL;
    self->next = NULL;
    self->scope = 0;
    self->intable = FALSE;
    return TRUE;
}

void
sym__FinalizeObject(ClassID, self)
struct classhdr *ClassID;
struct sym *self;
{
    if(self->name) {
	free(self->name);
	self->name=NULL;
    }
    if(self->intable) {
	self->intable=FALSE;
	noOfEntries--;
    }
    return;
}





void
sym__printtable(ClassID)
struct classhdr *ClassID;
{
        long i;
	struct sym *pos;

	printf("enclosingScope:");
	for (i = 0; i < maxScopes; ++i)
	    printf(" %d", enclosingScope[i]);
	printf(" <%d> \n", nextFreeScope);

	printf("table <%d>:\n", noOfEntries);
	for (i = 0; i < *tableSize; ++i) {
	    printf("%d:\n",i);
	    for(pos = table[i]; pos != NULL; pos = pos->next)
		printdata(pos);
	}

	printf("\n\n");

	fflush(stdout);
}


printdata(self)
struct sym *self;
{
    if (self == NULL)
	printf("NULL\n");
    else
	printf("%x:%s,%d\n", self, self->name, self->scope);
}

/*****************************************************************************\
 *
 * INSTANCE METHODS
 *
\*****************************************************************************/


/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/basics/common/RCS/proctbl.c,v 2.15 1992/12/15 21:28:38 rr2b R6tape $";
#endif


 

/* proctbl.c -- A module that manages a mapping from strings to procedure pointers.
 */

#include <class.h>
#include <util.h>
#include <ctype.h>
#include <proctbl.eh>

static struct proctable_Entry *LookupHash();
static int ModuleClear();
static int HashName();

#define HASHMAX	128		/* must be power of two */
#define HASHMASK	(HASHMAX-1)

static struct proctable_Entry *hashTable[HASHMAX];


/* Initialize the package. 
*/
	boolean 
proctable__InitializeClass(classID)
	struct classheader *classID;
{
	int i;
	
	for (i = 0; i < HASHMAX; ++i)
		hashTable[i] = NULL;
	return TRUE;
}

/* Add an entry to the table.  Only the name needs to be defined, 
	but all storage pointed to must be permanent 
	(do the mallocs in the caller or pass literals). 
*/
	struct proctable_Entry *
proctable__DefineProc(classID, name, proc, type, module, doc)
	struct classheader *classID;
	char *name;		/* these match the fields in an entry */
	int (*proc)();
	struct classinfo *type;
	char *module;
	char *doc;
{
	int hash;
	struct proctable_Entry *pe = NULL;

	if (name == NULL) {
		pe = (struct proctable_Entry *)malloc(sizeof(struct proctable_Entry));
		pe->name = NULL;
		pe->proc = proc;
		pe->type = type;
		pe->module = module;
		pe->doc = doc;
		pe->returntype = proctable_Void;
	}
	else {
		hash = HashName(name);
		pe = LookupHash(name, hash);
		if (pe == NULL) {
			pe = (struct proctable_Entry *)malloc(sizeof(struct proctable_Entry));
			if (pe == NULL)
				return NULL;
			pe->hnext = hashTable[hash];
			hashTable[hash] = pe;
			pe->name = name;
			pe->type = NULL;
			pe->module = NULL;
			pe->doc = NULL;
			pe->proc = NULL;
			pe->returntype = proctable_Void;
		}
		if (proc != NULL)
			pe->proc = proc;
		if (type != NULL)
			pe->type = type;
		if (module != NULL)
			pe->module = module;
		if (doc != NULL)
			pe->doc = doc;
	}
	return pe;
}

void proctable__DefineProcs(classID,procs)
struct classheader *classID;
struct proctable_Description *procs;
{
	while(procs->name!=NULL){
		proctable_DefineProc(procs->name, procs->proc, procs->type, 
				procs->module, procs->doc);
		procs++;
	}
}

/* Add a typed entry to the table.  Only the name needs to be defined, 
	but all storage pointed to must be permanent (do the mallocs in the caller or pass literals). 
*/
	struct proctable_Entry *
proctable__DefineTypedProc(classID, name, proc, type, 
		module, doc, returntype)
	struct classheader *classID;
	char *name;		/* these match the fields in an entry */
	int (*proc)();
	struct classinfo *type;
	char *module;
	char *doc;
	enum proctable_type returntype;
{
	struct proctable_Entry *pe;
	pe = proctable_DefineProc(name, proc, type, module, doc);
	if (pe != NULL)
		pe->returntype = returntype;
	return pe;
}

void proctable__DefineProcsWithTypes(classID,procs)
struct classheader *classID;
struct proctable_DescriptionWithType *procs;
{
	while(procs->name!=NULL){
		proctable_DefineTypedProc(procs->name, procs->proc, procs->type, 
					procs->module, procs->doc, procs->returntype);
		procs++;
	}
}

/* Given a name, look up its entry. */
struct proctable_Entry *proctable__Lookup(classID,name)
	struct classheader *classID;
	register char *name;
{
	register int hash;
	hash = HashName(name);
	return LookupHash(name, hash);
}

/* Call the proc with each entry and with the rock. */
void proctable__Enumerate(classID, proc, procdata)
	struct classheader *classID;
	int (*proc)();
	char *procdata;
{
	int hash;
	struct proctable_Entry *pe;

	for (hash = 0; hash < HASHMAX; ++hash)
		for (pe = hashTable[hash]; pe != NULL; pe = pe->hnext)
			(*proc)(pe, procdata);
}

/* Force the package for this function to be loaded if possible. */
void proctable__ForceLoaded(classID, pe)
	struct classheader *classID;
	struct proctable_Entry *pe;
{
	if (proctable_Defined(pe) || pe->module == NULL)
		return;
	(void) class_Load(pe->module);
	/* Go ahead and mark all other procs for this module as loaded. */
	proctable_Enumerate(ModuleClear, pe->module);
}

/* Potentially clear the module pointer for this entry. */
static int ModuleClear(pe, module)
	struct proctable_Entry *pe;
	char *module;
{
	if (pe != NULL && pe->module != NULL && strcmp(pe->module, module) == 0)
		pe->module = NULL;
	return(0);
}


/* Compute the hash function for this name. */
static int HashName(name)
	char *name;
{
	register int hash = 0;

	while (*name != 0) {
		hash = hash + isupper(*name)?tolower(*name):*name;
		name++;
	}
	return hash & HASHMASK;
}

/* Given a name and a hash index, look up the name. */
static struct proctable_Entry *LookupHash(name, hash)
	int hash;
	char *name;
{
	struct proctable_Entry *pe;

	for (pe = hashTable[hash]; pe != NULL; pe = pe->hnext)
		if (FOLDEDEQ(pe->name, name))
			return pe;
	return NULL;
}

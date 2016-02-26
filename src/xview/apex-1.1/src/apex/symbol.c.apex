#ifndef lint
static char    *RCSid = "$Header: /n/homeserver/i/gounares/pu/apex/src/apex/RCS/symbol.c,v 1.1 93/01/06 03:27:53 gounares Exp Locker: gounares $";

#endif

/*
 * $Log:	symbol.c,v $
 * Revision 1.1  93/01/06  03:27:53  gounares
 * Initial revision
 * 
 */

/*
 * symbol.c
 * 
 * for the apeX environment
 */

/*
 * Copyright 1993 Alexander Gounares
 * 
 * This source is covered by the 
<!@ 1 COPYING 1 #Link to the GNU General Public License
#when run this description will be passed to the shell
cat COPYING
 @!>
GNU General Public License Version 2
<!@ 1 COPYING 1 #Link to the GNU General Public License
#when run this description will be passed to the shell
cat COPYING
 @!>

 * 
 * see the apeX manual for more details
 */

#include <stdio.h>
#include "symbol.h"
#include <assert.h>
#include <string.h>

#define EOS                     '\0'
#define HASHSIZE                211

struct linked_symbol {
	Symbol          sym;
	struct linked_symbol *next;
};
typedef struct linked_symbol *Linked_symbol;

struct symbol_name {
	char           *name;
	struct symbol_name *next;
};
typedef struct symbol_name *Symbol_name;

struct symbol_list {
	Linked_symbol   symbols;
	int             lev;
	struct symbol_list *next;
};
typedef struct symbol_list *Symbol_list;


static Symbol_name name_space[HASHSIZE];
static Linked_symbol symbol_table[HASHSIZE];
static struct symbol_list sym_list_0;
static Symbol_list sym_list = &sym_list_0;

int             level = 0;	       /* global variable for level */

/*
 * *This function will store unique strings into a name space (hash ta *to a
 * number between 0 and HASHSIZE -1 *obtained from the book *Compilers*, by
 * Aho, Sethi, and Ullman *
 * 
 *NB:  this function presumes a 32-bit integer
 */
static unsigned int 
hash(s)
    char           *s;
{
	char           *p;
	unsigned        h = 0,
	                g;

	for (p = s; *p != EOS; p++) {
		h = (h << 4) + (*p);
		if (g = h & 0xf0000000) {
			h ^= (g >> 24);
			h ^= g;
		}
	}
	return h % HASHSIZE;
}

/*
 * *this function will return a pointer to a NULL-terminated array of
 * pointers *to symbols in scope lev in the order the symbols were installed
 * by installat *the caller is responsible for freeing the array
 */
Symbol         *
allsymbols(lev)
    int             lev;
{
	int             count = 0;
	Symbol_list     sl;
	Linked_symbol   p;
	Symbol         *array;

	assert(lev <= level && lev >= 0);

	for (sl = sym_list; sl->lev != lev && sl->next != NULL; sl = sl->next) {;
	}
	for (p = sl->symbols; p != NULL; p = p->next) {
		count++;
	}
	array = (Symbol *) malloc((count + 1) * (sizeof *array));
	assert(array);
	array[count] = NULL;
	for (p = sl->symbols, count--; p != NULL; p = p->next, count--) {
		array[count] = p->sym;
	}
	return array;
}

/*
 * *This function will begin a new scope level
 */
void 
enterscope()
{
	Symbol_list     sl;

	level++;
	sl = sym_list;
	sym_list = (Symbol_list) malloc(sizeof *sl);
	assert(sym_list);
	sym_list->next = sl;
	sym_list->lev = level;
	sym_list->symbols = NULL;
}

/*
 * clear_table -- clear up the symbol table
 */
void 
clear_table()
{
	int             i;
	Linked_symbol   p,
	                pp;

	for (p = sym_list->symbols; p != NULL;) {
		pp = p->next;
		free(p);
		p = pp;
	}
	sym_list->symbols = NULL;

	/* now go through the hash table and chop out the entries there */
	for (i = 0; i < HASHSIZE; i++) {
		p = symbol_table[i];
		while (p != NULL) {
			pp = p->next;
			if (p->sym->calls)
				free(p->sym->calls);	/* ugg--this is ugly :( */
			free(p->sym);
			free(p);
			p = pp;
		}
		symbol_table[i] = p;
	}
}

/*
 * *This function will install a symbol into both the symbol table at level
 * lev *and the symbol list, a linked list of symbols used will the
 * allsymbols *function. *name must be a pointer returned by strsave
 */
Symbol 
installat(n_symbol, lev)
    Symbol          n_symbol;
    int             lev;
{
	Linked_symbol   p,
	               *prev,
	                n_sym_list,
	                n_hash_table;
	Symbol_list     sl;
	static struct symbol z;
	unsigned int    h;

	assert(lev <= level && lev >= 0);

	/* first create a new symbols and stuff */
	n_sym_list = (Linked_symbol) malloc(sizeof *n_sym_list);
	n_hash_table = (Linked_symbol) malloc(sizeof *n_hash_table);
	assert(n_hash_table);
	assert(n_sym_list);

	n_sym_list->sym = n_hash_table->sym = n_symbol;

	for (sl = sym_list; sl->lev != lev && sl->next != NULL; sl = sl->next) {;
	}
	n_sym_list->next = sl->symbols;
	sl->symbols = n_sym_list;

	for (p = symbol_table[h = hash(n_symbol->szName)], prev = &(symbol_table[h]);
		p != NULL; prev = &(p->next), p = p->next) {

		if (p->sym->scope <= level) {
			break;
		}
	}
	*prev = n_hash_table;
	n_hash_table->next = p;

	return n_symbol;

}

/*
 * *This function will return a Symbol pointer to the entry with name *and
 * the largest scope, or NULL if no entries are found. *name must be a
 * pointer returned by strsave
 */
Symbol 
lookup(szName, szFilename)
    char           *szName,
                   *szFilename;
{
	Linked_symbol   p;
	Symbol          s = NULL;

	assert(szName);
	for (p = symbol_table[hash(szName)]; p != NULL; p = p->next) {
		if (p->sym->szName == szName)
			if (szFilename) {
				if (szFilename == p->sym->szFilename) {
					return p->sym;
				} else {
					s = p->sym;
				}
			} else if (!p->sym->fStatic) {
				return p->sym;
			}
	}
	return (s) ? s : NULL;
}

/*
 * *This function will store unique strings in the name space *(hash table)
 * and return a pointer to the string
 */
char           *
strsave(str)
    char           *str;
{
	unsigned int    h;
	Symbol_name     p,
	                t,
	                next,
	               *prev;

	assert(str);

	for (p = name_space[h = hash(str)], prev = &(name_space[h]);
		p != NULL; prev = &(p->next), p = p->next) {
		if (!strcmp(str, p->name)) {

			/*
			 * put name at front of list for optimization of hash
			 * lookups
			 */
			*prev = p->next;
			p->next = name_space[h];
			name_space[h] = p;
			return p->name;
		}
	}
	t = (Symbol_name) malloc(sizeof *t);
	assert(t);
	t->name = (char *) malloc(strlen(str) + 1);
	assert(t->name);
	strcpy(t->name, str);
	t->next = name_space[h];
	name_space[h] = t;
	return t->name;
}

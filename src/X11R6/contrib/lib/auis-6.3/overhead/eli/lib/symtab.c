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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/eli/lib/RCS/symtab.c,v 2.6 1992/12/15 21:02:39 rr2b R6tape $";
#endif

#include  <symtab.h>

/*
 * Functions to handle manipulation of the global symbol table (basically,
 * front-end to ht.c) MOST OF THESE ASSUME THAT SYM'S ARE IN THE TABLE 
 */

EliSym_t       *eliSymTab_Find(symtab, name)
eliHashTable_t *symtab;
char           *name;
{
    EliSexp_t      *tmp = eliHT_Find(symtab, name);

    return (tmp ? tmp->data.datum.symval : NULL);
}


/*
  * Like eliSymTab_Find, but if it doesn't already exist, it is created,
  * with NULL as its value.  This is as opposed to symtab_getcheck, which
  * binds a value even to an existing node. 
  */

EliSym_t       *eliSymTab_FindOrMake(st, symtab, name)
EliState_t     *st;
eliHashTable_t *symtab;
char           *name;
{
    EliSym_t       *result;

    if (!(result = eliSymTab_Find(symtab, name))) {
	result = eliSymTab_Make(st, symtab, name);
	if (EliErr_ErrP(st))
	    return (NULL);
    }
    return (result);
}

EliSym_t *eliSymTab_Make(st, tab, name)
EliState_t *st;
eliHashTable_t *tab;
char *name;
{
    EliSym_t *tmp;
    EliStr_t *strtmp;
    EliSexp_t *nodetmp;

    strtmp = eliStringTable_FindOrMake(st, EliStringTable(st), name);
    if (EliErr_ErrP(st))
	return (NULL);
    if (!(tmp = eliSym_GetNew_trace(st, EliTraceStk(st), strtmp)))
	return (NULL);
    eliSym_SetScope(tmp, e_sym_known);
    if (!(nodetmp = eliSexp_GetNew_trace(st, EliTraceStk(st))))
	return (NULL);
    EliSexp_SetSym(st, nodetmp, tmp);
    eliHT_Insert(st, tab, nodetmp, name);
    if (EliErr_ErrP(st))
	return (NULL);
    return (tmp);
}

/*
  * Check if node with name "name" is in symtab.  If so, bind the value to it.
      * If not, make a new node and bind the value, then place it in symtab.
      * Return the symnode to which the value was bound (probably rarely used). 
	*/
EliSym_t       *eliSymTab_FindOrMakeAndBind(st, symtab, name, val)
EliState_t     *st;
eliHashTable_t *symtab;
char           *name;
EliSexp_t      *val;
{
    EliSym_t       *tmp;

    if (tmp = eliSymTab_Find(symtab, name))
	EliSym_BindSexp(st, tmp, val);
    else {
	tmp = eliSymTab_MakeAndBind(st, symtab, name, val);
	if (EliErr_ErrP(st))
	    return (NULL);
    }
    return (tmp);
}

/*
  * Like eliSymTab_FindOrMakeAndBind, but always creates a new node (doesn't check for
      * preexistence) 
*/

EliSym_t       *eliSymTab_MakeAndBind(st, symtab, name, val)
EliState_t     *st;
eliHashTable_t *symtab;
char           *name;
EliSexp_t      *val;
{
    EliSym_t       *tmp;
    EliSexp_t      *nodetmp;
    EliStr_t       *strtmp;

    strtmp = eliStringTable_FindOrMake(st, EliStringTable(st), name);
    if (EliErr_ErrP(st))
	return (NULL);
    if (!(tmp = eliSym_GetNew_trace(st, EliTraceStk(st), strtmp)))
	return (NULL);
    eliSym_SetScope(tmp, e_sym_known);
    EliSym_BindSexp(st, tmp, val);
    if (!(nodetmp = eliSexp_GetNew_trace(st, EliTraceStk(st))))
	return (NULL);
    EliSexp_SetSym(st, nodetmp, tmp);
    eliHT_Insert(st, symtab, nodetmp, name);
    if (EliErr_ErrP(st))
	return (NULL);
    return (tmp);
}

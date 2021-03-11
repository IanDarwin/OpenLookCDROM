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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/eli/lib/RCS/intrface.c,v 2.7 1992/12/15 21:01:25 rr2b R6tape $";
#endif

#include  <intrface.h>

void            EliReset(st, freeP)
EliState_t     *st;
int             freeP;
{
    EliClearErr(st);
    eliTraceStk_Purge(st, EliTraceStk(st));
    if (freeP) {
        EliFreeAllEvalStackNodes(st);
        EliFreeAllTraceStackNodes(st);
    }
}

EliSexp_t      *EliSGetSexp(st, string)
EliState_t     *st;
char           *string;
{
    return (eliSGetSexp_trace(st, EliTraceStk(st), string));
}

EliSexp_t      *EliFGetSexp(st, fp)
EliState_t     *st;
FILE           *fp;
{
    return (eliFGetSexp_trace(st, EliTraceStk(st), fp));
}

EliSexp_t      *EliGetSexp(st)
EliState_t     *st;
{
    return (eliGetSexp_trace(st, EliTraceStk(st)));
}

EliSexp_t      *EliSexp_GetNew(st)
EliState_t     *st;
{
    return (eliSexp_GetNew_trace(st, EliTraceStk(st)));
}

EliCons_t      *EliCons_GetNew(st)
EliState_t     *st;
{
    return (eliCons_GetNew_trace(st, EliTraceStk(st)));
}

EliSym_t       *EliSym_GetNew_String(st, name)
EliState_t     *st;
char           *name;
{
    EliStr_t       *str = eliStringTable_FindOrMake(st, EliStringTable(st), name);

    if (str)
	return (eliSym_GetNew_trace(st, EliTraceStk(st), str));
    else
	return (NULL);
}

EliSym_t       *EliSym_GetNew_StrNode(st, name)
EliState_t     *st;
EliStr_t       *name;
{
    return (eliSym_GetNew_trace(st, EliTraceStk(st), name));
}

EliStr_t       *EliStr_GetNew(st, string)
EliState_t     *st;
char           *string;
{
    return (eliStr_GetNew_trace(st, EliTraceStk(st), string));
}

/*
This one is cool.
Basically, this takes a list, checks that its length is within
the range min to max, and fills in sexpBuf like EliGetListCars,
except that it will evaluate sexp i if evalV[i] is TRUE.
Also, it will type-check the stuff it puts into sexpBuf;
sexpBuf[i] must have type typeV[i].  If sexpBuf[i] is not
to be type-checked, typeV[i] must be e_data_none.

If typeV is NULL, no type checking occurs.
If evalV is NULL, no evaluation occurs.

Finally, if errBuf is not NULL, then if an error occurs,
it will contain the offending sexp (except in the case
of evaluation errors, in which case an error has already
been flagged in st and we do not wish to risk resetting
it by calling EliSexp_GetNew).  If the list has the wrong
length, allocating such a node may fail, in which case
*errBuf will be NULL upon return.

Returns:
-1 if the list was too short;
-2 if the list was too long;
-(1000 + i) if sexpBuf[i] is of the wrong type;
-(2000 + i) if evaluation of sexp [i] failed;
otherwise returns the length of the list, possibly zero.
  
Note: In type checking, the empty list () will match e_data_symbol,
and the symbol NIL will match e_data_list.
*/

int             EliProcessList(st, list, min, max, sexpBufV, errBuf, typeV, evalV)
EliState_t     *st;
EliCons_t      *list;
int             min, max;
EliSexp_t      *sexpBufV[], **errBuf;
eliDataTypes_t  typeV[];
int             evalV[];

{
    int             i, len = EliListLen(list);
    EliCons_t      *listPtr = list;
    EliSexp_t      *cellCar, *theSexp;
    eliDataTypes_t  theType;

    if (len < min) {
        if (errBuf && ((*errBuf) = EliSexp_GetNew(st)))
            EliSexp_SetCons(st, *errBuf, list);
	return (-1);
    }
    if (len > max){
        if (errBuf && ((*errBuf) = EliSexp_GetNew(st)))
            EliSexp_SetCons(st, *errBuf, list);
        return (-2);
    }
    for (i = 0; i < len; ++i, listPtr = EliGetNextCell(listPtr)) {
	cellCar = EliCons_GetCar(listPtr);
	if (evalV && evalV[i]) {
	    if (!(theSexp = EliEval(st, cellCar)))
		return (-2000 - i);
	}
	else
	    theSexp = cellCar;
	if (typeV && ((theType = typeV[i]) != e_data_none)) {
	    if (EliSexp_GetType(theSexp) != theType) {
		if ((theType == e_data_symbol) || (theType == e_data_list)) {
                    if (!EliNilP(st, theSexp)) {
                        if (errBuf)
                            *errBuf = theSexp;
                        return (-1000 - i);
                    }
		}
                else {
                    if (errBuf)
                        *errBuf = theSexp;
		    return (-1000 - i);
                }
	    }
	}
	sexpBufV[i] = theSexp;
    }
    return (len);
}

int EliBind(st, varlist, arglist)
EliState_t *st;
EliCons_t *varlist, *arglist;
{
    return (eliBind(st, EliEvalStack(st), varlist, arglist));
}

int EliEvalAndBind(st, varlist, arglist)
EliState_t *st;
EliCons_t *varlist, *arglist;
{
    return (eliEvalAndBind(st, EliEvalStack(st), varlist, arglist));
}

EliSym_t *EliEvalStk_FindSym(st, name)
EliState_t *st;
char *name;
{
    return (eliEvalStk_FindSym(EliEvalStack(st), name));
}

void EliEvalStk_PopN(st, n)
EliState_t *st;
int n;
{
    eliEvalStk_PopN(st, EliEvalStack(st), n);
}

EliStr_t *EliStringTable_Find(st, name)
EliState_t *st;
char *name;
{
    return (eliStringTable_Find(EliStringTable(st), name));
}

EliStr_t *EliStringTable_FindOrMake(st, string)
EliState_t *st;
char *string;
{
    return (eliStringTable_FindOrMake(st, EliStringTable(st), string));
}

EliStr_t *EliStringTable_Make(st, string)
EliState_t *st;
char *string;
{
    return (eliStringTable_Make(st, EliStringTable(st), string));
}

EliSym_t *EliSymTab_Find(st, name)
EliState_t *st;
char *name;
{
    return (eliSymTab_Find(EliSymbolTable(st), name));
}

EliSym_t *EliSymTab_FindOrMake(st, name)
EliState_t *st;
char *name;
{
    return (eliSymTab_FindOrMake(st, EliSymbolTable(st), name));
}

EliSym_t *EliSymTab_FindOrMakeAndBind(st, name, val)
EliState_t *st;
char *name;
EliSexp_t *val;
{
    return (eliSymTab_FindOrMakeAndBind(st, EliSymbolTable(st), name, val));
}

EliSym_t *EliSymTab_Make(st, name)
EliState_t *st;
char *name;
{
    return (eliSymTab_Make(st, EliSymbolTable(st), name));
}

EliSym_t *EliSymTab_MakeAndBind(st, name, val)
EliState_t *st;
char *name;
EliSexp_t *val;
{
    return (eliSymTab_MakeAndBind(st, EliSymbolTable(st), name, val));
}

void EliTraceStk_Purge(st)
EliState_t *st;
{
    eliTraceStk_Purge(st, EliTraceStk(st));
}

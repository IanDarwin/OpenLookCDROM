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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/eli/lib/RCS/prims1.c,v 1.16 1994/04/21 16:27:16 rr2b Exp $";
#endif

#include <prmtives.h>
#include <sys/errno.h>

static struct {
    char           *name;
    void            (*fn) ();
}               primList[] = {{

        "TRACE", Prim_TRACE
},
{
    "PLUS", Prim_PLUS
},
{
    "+", Prim_PLUS
},
{
    "SETQ", Prim_SETQ
},
{
    "DEFUN", Prim_DEFUN
},
{
    "DEFUNQ", Prim_DEFUNQ
},
{
    "CONS", Prim_CONS
},
{
    "PROGN", Prim_PROGN
},
{
    "EVAL", Prim_EVAL
},
{
    "CAR", Prim_CAR
},
{
    "CDR", Prim_CDR
},
{
    "LIST", Prim_LIST
},
{
    "COND", Prim_COND
},
{
    "PRINT", Prim_PRINT
},
{
    "TERPRI", Prim_TERPRI
},
{
    "EQ", Prim_EQ
},
{
    "STRCONTAINS", Prim_STRCONTAINS
},
{
    "ASSOC", Prim_ASSOC
},
{
    "STRSTARTS", Prim_STRSTARTS
},
{
    "LET*", Prim_LETSTAR
},
{
    "AND", Prim_AND
},
{
    "OR", Prim_OR
},
{
    "NOT", Prim_NOT
},
{
    "NULL", Prim_NOT
},
{
    "DO*", Prim_DOSTAR
},
{
    "READ", Prim_READ
},
{
    "CONSP", Prim_CONSP
},
{
    "STRINGP", Prim_STRINGP
},
{
    "ATOM", Prim_ATOM
},
{
    "NUMBERP", Prim_NUMBERP
},
{
    "LESSP", Prim_LESSP
},
{
    "BOUNDP", Prim_BOUNDP
},
{
    "MINUS", Prim_MINUS
},
{
    "-", Prim_MINUS
},
{
    "TIMES", Prim_TIMES
},
{
    "*", Prim_TIMES
},
{
    "DIV", Prim_DIV
},
{
    "STRCAT", Prim_STRCAT
},
{
    "INDEX", Prim_INDEX
},
{
    "RINDEX", Prim_RINDEX
},
{
    "STRDECOMPOSE", Prim_STRDECOMPOSE
},
{
    "STRLEN", Prim_STRLEN
},
{
    "LCSTRING", Prim_LCSTRING
},
{
    "APPEND", Prim_APPEND
},
{
    "RE-STRCONTAINS", Prim_RE_STRCONTAINS
},
{
    "SUBSTRING", Prim_SUBSTRING
},
{
    "SYMBOLP", Prim_SYMBOLP
},
{
    "PLUMBER", Prim_PLUMBER
},
{
    "RE-STRDECOMPOSE", Prim_RE_STRDECOMPOSE
},
{
    "GENSYM", Prim_GENSYM
},
{
    "FUNCTION", Prim_FUNCTION
},
{
    "LOAD", Prim_LOAD
},
{
    "RE-STRDECOMPOSE+", Prim_RE_STRDECOMPOSEPLUS
},
{
    "LET", Prim_LET
},
{
    "DO", Prim_DO
},
{
    "SYM-TO-STR", Prim_SYM_TO_STR
},
{
    "STR-TO-INT", Prim_STR_TO_INT
},
{
    "INT-TO-STR", Prim_INT_TO_STR
},
{
    "PRINTF", Prim_PRINTF
},
{
    "PUTS", Prim_PUTS
},
{
    "SYSTEM", Prim_SYSTEM
},
{
    "GETENV", Prim_GETENV
},
{
    "DEBUG", Prim_DEBUG
},
{
    "EQUAL", Prim_EQUAL
},
{
    "UCSTRING", Prim_UCSTRING
},
{
    "UNBINDFN", Prim_UNBINDFN
},
{
    "UNBIND", Prim_UNBIND
},
{
    "DISCARD", Prim_DISCARD
},
{
    "ERROR", Prim_ERROR
},
{
    "DEFUNV", Prim_DEFUNV
},
{
    "DEFUNVQ", Prim_DEFUNVQ
},
{
    "VERSION", Prim_VERSION
},
{
    "FILTER", Prim_FILTER
},
{
    "CATCHERR", Prim_CATCHERR
},
{
    NULL, NULL
}
};                                     /* Must end this way */

static jmp_buf  brokenPipeEnv, alarmEnv;

void            eliPrimInit(st)
EliState_t     *st;
{
    int             i;

    for (i = 0; primList[i].name; ++i) {
        eliPrimDefCompiled(st, primList[i].name, primList[i].fn);
        if (EliErr_ErrP(st))
            return;
    }
}

void            eliPrimDefCompiled(st, name, fn)
EliState_t     *st;
char           *name;
void            (*fn) ();

{
    EliFn_t        *tmp;

    if (!(tmp = eliFn_GetNew_trace(st, EliTraceStk(st))))
        return;
    eliFn_SetCompiled(st, tmp, fn);
    EliPrimDef(st, name, tmp);
    if (EliErr_ErrP(st))
        return;                        /* Another nutso place to put this */
}


/* Define a function: bind it to a symbol & place in global table */

void            EliPrimDef(st, name, fn)
EliState_t     *st;
char           *name;
EliFn_t        *fn;
{
    EliSym_t       *symtmp;

    symtmp = eliSymTab_FindOrMake(st, EliSymbolTable(st), name);
    if (EliErr_ErrP(st))
        return;
    EliSym_BindFn(st, symtmp, fn);
}

/* If the string s1 is found in s2, return that part of s2 that
 * begins with s1, else NULL
 */
char           *eliFindPrefix(s1, s2, ignoreCase)
char           *s1, *s2;
int             ignoreCase;
{
    int             len = strlen(s1), looping = TRUE;
    char           *place = NULL, *ptr2, *pat, *ref, *orig = s2, *ref2;

    if (!(pat = EliSaveString(s1)))
        return (NULL);
    if (!(ref = EliSaveString(s2))) {
        free(pat);
        return (NULL);
    }
    if (ignoreCase) {
        EliUpCaseStr(pat);
        EliUpCaseStr(ref);
    }
    ref2 = ref;
    if (*pat) {
        while (looping) {
            if (!(*ref))
                looping = FALSE;
            else {
                if (!(ptr2 = (char *) index(ref, *pat)))
                    looping = FALSE;
                else
                    if (!(looping = strncmp(pat, ptr2, len)))
                        place = ptr2;
                ref = ptr2 + 1;
            }
        }
        if (place)
            place = orig + (place - ref2);
    }
    free(pat);
    free(ref2);
    return (place);
}

char           *eliStrCat(s1, s2)
char           *s1, *s2;
{
    char           *buf = NULL;
    int             s1len = strlen(s1), len = 1 + s1len + strlen(s2);

    buf = EliStringOpBuf(len);
    if (buf) {
        strcpy(buf, s1);
        strcpy(buf + s1len, s2);
    }
    return (buf);
}

/* The next bit deals with the library mechanism. */

static          eliCountElts(s)
char           *s;
{
    int             tot = 0;

    if (s) {
        ++tot;
        while (*s) {
            if (*s++ == ':')
                ++tot;
        }
    }
    return (tot);
}


static          eliInitLibraries(st)
EliState_t     *st;
{
    int             numelts, whichelt;
    char           *elilib = NULL, *clientlib = NULL, *s;

    elilib = getprofile("ELIPATH");
    if (!elilib) {
        elilib = EliSaveString(AndrewDir("/lib/eli"));
    }
    else
        elilib = EliSaveString(elilib);
    if (!elilib)
        return (-1);                   /* Should raise an error here */
    if (st->ClientLibraryPreference) {
        clientlib = getprofile(st->ClientLibraryPreference);
    }
    if (!clientlib)
        clientlib = EliSaveString(st->DefaultClientLibraryPath);
    else
        clientlib = EliSaveString(clientlib);
    numelts = eliCountElts(elilib) + eliCountElts(clientlib);
    st->LibElts = (eliLibElts_t *) malloc((1 + numelts) * sizeof(eliLibElts_t));
    if (!st->LibElts) {
        return (-1);                   /* Should raise an error here */
    }
    whichelt = 0;
    s = clientlib;
    while (s) {
        st->LibElts[whichelt].dir = s;
        st->LibElts[whichelt++].ext = st->DefaultClientExtension;
        s = index(s, ':');
        if (s)
            *s++ = '\0';
    }
    s = elilib;
    while (s) {
        st->LibElts[whichelt].dir = s;
        st->LibElts[whichelt++].ext = "eli";
        s = index(s, ':');
        if (s)
            *s++ = '\0';
    }
    st->LibElts[whichelt].dir = NULL;
    st->LibElts[whichelt].ext = NULL;
    return (0);
}

static          eliLoadFromLibrary(st, resbuf, loadfileSexp)
EliState_t     *st;
EliSexp_t      *resbuf, *loadfileSexp;
{
    char            FileName[1 + MAXPATHLEN], *loadfile;
    int             i, unixErr = 0;
    short           FoundIt = FALSE;

    if (!(st->initializedLibraries)) {
        if (eliInitLibraries(st))
            return;                    /* should raise an error here */
        else
            st->initializedLibraries = TRUE;
    }
    loadfile = EliStr_GetString(EliSexp_GetStr(loadfileSexp));
    for (i = 0; st->LibElts[i].dir; ++i) {
        strcpy(FileName, st->LibElts[i].dir);
        strcat(FileName, "/");
        strcat(FileName, loadfile);
        if (!access(FileName, R_OK)) {
            FoundIt = TRUE;
            break;
        }
        else {
            if (errno != ENOENT) {
                unixErr = errno;
                break;
            }
        }
        strcat(FileName, ".");
        strcat(FileName, st->LibElts[i].ext);
        if (!access(FileName, R_OK)) {
            FoundIt = TRUE;
            break;
        }
        else {
            if (errno != ENOENT) {
                unixErr = errno;
                break;
            }
        }
    }
    if (FoundIt) {
        char            Bogoid[25 + MAXPATHLEN];        /* BOGUS -- perhaps bobg
                                                         * can optimize? */
        EliSexp_t      *sexp;

        sprintf(Bogoid, "(read \"%s\")", FileName);

        if (!(sexp = eliSGetSexp_trace(st, EliTraceStk(st), Bogoid)))
            return;
        eliEval(st, sexp, resbuf);
/*      if (EliErr_ErrP(st)) return; */
    }
    else {
        EliError(st, ELI_ERR_BAD_ARGS, loadfileSexp, "ELI-PRIMITIVE [LOAD (error opening file)]", unixErr ? unixErr : ENOENT);
        return;
    }
}

/***** BEGIN DEFINITIONS OF LISP FUNCTIONS HERE *****/


void            Prim_DOSTAR(st, arglist, resbuf)
EliState_t     *st;
EliCons_t      *arglist;
EliSexp_t      *resbuf;
{
    EliCons_t      *consptr, *consptr2, *vars = NULL, *constmp, *endstuff;
    EliSexp_t      *nodeptr, *resnode, *nodetmp, *nodeerr, *varsnode, *body = NULL, *bindval, *oneMoreTmp;
    EliSym_t       *symtmp, *symtmp2;
    int             looping = TRUE, i, l3, l2, l = EliListLen(arglist), varsp, numvars = 0, bound = 0;

    EliDebug(20, "Entering primitive DO*", st, FALSE);
    if ((l < 2) || (l > 3)) {
        if (!(nodeerr = eliSexp_GetNew_trace(st, EliTraceStk(st))))
            return;
        EliSexp_SetCons(st, nodeerr, arglist);
        EliError(st, ELI_ERR_ARGLISTSIZE, nodeerr, "ELI-PRIMITIVE [DO* (checking arglist size)]", 0);
    }
    varsnode = EliCons_GetCar(arglist);
    if ((EliSexp_GetType(varsnode) != e_data_list) && !EliNilP(st, varsnode)) {
        EliError(st, ELI_ERR_BAD_ARGS, varsnode, "ELI-PRIMITIVE [DO* (1st arg not a list)]", 0);
        return;
    }
    if (varsp = !EliNilP(st, varsnode))
        numvars = EliListLen(vars = EliSexp_GetCons(varsnode));
    nodetmp = EliCons_GetCdr(arglist);
    constmp = EliSexp_GetCons(nodetmp);
    nodetmp = EliCons_GetCar(constmp);
    if (EliSexp_GetType(nodetmp) != e_data_list) {
        EliError(st, ELI_ERR_BAD_ARGS, nodetmp, "ELI-PRIMITIVE [DO* (2nd arg not a list)]", 0);
        return;
    }
    l2 = EliListLen(endstuff = EliSexp_GetCons(nodetmp));
    if ((l2 < 1) || (l2 > 2)) {
        EliError(st, ELI_ERR_BAD_ARGS, nodetmp, "ELI-PRIMITIVE [DO* (2nd arg not 1 or 2 elements long)]", 0);
        return;
    }
    if (l == 3) {
        nodetmp = EliCons_GetCdr(constmp);
        constmp = EliSexp_GetCons(nodetmp);
        body = EliCons_GetCar(constmp);
    }
    if (varsp) {
        consptr = vars;
        for (i = 0; i < numvars; ++i) {
            nodeptr = EliCons_GetCar(consptr);
            if (EliSexp_GetType(nodeptr) != e_data_list) {
                EliError(st, ELI_ERR_BAD_ARGS, nodeptr, "ELI-PRIMITIVE [DO* (1st arg contains a non-list element)]", 0);
                eliEvalStk_PopN(st, EliEvalStack(st), bound);
                return;
            }
            l3 = EliListLen(consptr2 = EliSexp_GetCons(nodeptr));
            /* This is a sublist (vari initi stepi) */
            if ((l3 < 1) || (l3 > 3)) {
                EliError(st, ELI_ERR_BAD_ARGS, nodeptr, "ELI-PRIMITIVE [DO* (1st arg contains a list with wrong size)]", 0);
                eliEvalStk_PopN(st, EliEvalStack(st), bound);
                return;
            }
            nodeptr = EliCons_GetCar(consptr2);
            if (EliSexp_GetType(nodeptr) != e_data_symbol) {
                EliError(st, ELI_ERR_BAD_ARGS, nodeptr, "ELI-PRIMITIVE [DO* (1st arg contains a binding to a non-symbol)]", 0);
                eliEvalStk_PopN(st, EliEvalStack(st), bound);
                return;
            }
            symtmp = EliSexp_GetSym(nodeptr);
            if (!(symtmp2 = eliSym_GetNew_trace(st, EliTraceStk(st), EliSym_GetName(symtmp)))) {
                eliEvalStk_PopN(st, EliEvalStack(st), bound);
                return;
            }
            if (l3 == 1) {
                if (!(oneMoreTmp = EliSexp_GetNew(st))) {
                    eliEvalStk_PopN(st, EliEvalStack(st), bound);
                    return;
                }
                EliSexp_SetSym(st, oneMoreTmp, EliNilSym(st));
                EliSym_BindSexp(st, symtmp2, oneMoreTmp);
            }
            else {
                nodeptr = EliCons_GetCdr(consptr2);
                consptr2 = EliSexp_GetCons(nodeptr);
                nodeptr = EliCons_GetCar(consptr2);
                if (!(bindval = eliSexp_GetNew_trace(st, EliTraceStk(st)))) {
                    eliEvalStk_PopN(st, EliEvalStack(st), bound);
                    return;
                }
                eliEval(st, nodeptr, bindval);
                if (EliErr_ErrP(st)) {
                    eliEvalStk_PopN(st, EliEvalStack(st), bound);
                    return;
                }
                EliSym_BindSexp(st, symtmp2, bindval);
            }
            if (!eliEvalStk_Push(st, EliEvalStack(st), symtmp2)) {
                EliError(st, ELI_ERR_OUT_OF_MEM, NULL, "ELI-PRIMITIVE [DO* (pushing symbol)]", 0);
                eliEvalStk_PopN(st, EliEvalStack(st), bound);
                return;
            }
            ++bound;
            nodeptr = EliCons_GetCdr(consptr);
            if (EliSexp_GetType(nodeptr) == e_data_list)
                consptr = EliSexp_GetCons(nodeptr);
        }
    }

/* Now begin the loop: test the end-clause, assign result-clause to resbuf
 * if it's non-nil and return; if it's nil, evaluate the body (if any),
 * then update all the variables.
 */

    if (!(resnode = eliSexp_GetNew_trace(st, EliTraceStk(st)))) {
        eliEvalStk_PopN(st, EliEvalStack(st), bound);
        return;
    }
    while (looping) {
        nodeptr = EliCons_GetCar(endstuff);
        eliEval(st, nodeptr, resnode);
        if (EliErr_ErrP(st)) {
            eliEvalStk_PopN(st, EliEvalStack(st), bound);
            return;
        }
        if (EliNilP(st, resnode)) {
            if (l == 3) {              /* That is to say, if there exists a
                                        * body */
                eliEval(st, body, resnode);
                if (EliErr_ErrP(st)) {
                    eliEvalStk_PopN(st, EliEvalStack(st), bound);
                    return;
                }
            }
            /* Now update vars */
            if (varsp) {
                consptr = vars;
                for (i = 0; i < numvars; ++i) {
                    nodeptr = EliCons_GetCar(consptr);
                    if (EliSexp_GetType(nodeptr) != e_data_list) {
                        EliError(st, ELI_ERR_BAD_ARGS, nodeptr, "ELI-PRIMITIVE [DO* (1st arg contains a non-list element [in update])]", 0);
                        eliEvalStk_PopN(st, EliEvalStack(st), bound);
                        return;
                    }
                    l3 = EliListLen(consptr2 = EliSexp_GetCons(nodeptr));
                    /* This is a sublist (vari initi stepi) */
                    if ((l3 < 1) || (l3 > 3)) {
                        EliError(st, ELI_ERR_BAD_ARGS, nodeptr, "ELI-PRIMITIVE [DO* (1st arg contains a list with wrong size [in update])]", 0);
                        eliEvalStk_PopN(st, EliEvalStack(st), bound);
                        return;
                    }
                    nodeptr = EliCons_GetCar(consptr2);
                    if (EliSexp_GetType(nodeptr) != e_data_symbol) {
                        EliError(st, ELI_ERR_BAD_ARGS, nodeptr, "ELI-PRIMITIVE [DO* (1st arg contains a binding to a non-symbol [in update])]", 0);
                        eliEvalStk_PopN(st, EliEvalStack(st), bound);
                        return;
                    }
                    symtmp = EliSexp_GetSym(nodeptr);
                    symtmp2 = EliFindSym(st, EliStr_GetString(EliSym_GetName(symtmp)));
                    if (l3 == 3) {     /* That is, if there's a step clause */
                        nodeptr = EliCons_GetCdr(consptr2);
                        consptr2 = EliSexp_GetCons(nodeptr);
                        nodeptr = EliCons_GetCdr(consptr2);
                        consptr2 = EliSexp_GetCons(nodeptr);
                        nodeptr = EliCons_GetCar(consptr2);
                        if (!(bindval = eliSexp_GetNew_trace(st, EliTraceStk(st)))) {
                            eliEvalStk_PopN(st, EliEvalStack(st), bound);
                            return;
                        }
                        eliEval(st, nodeptr, bindval);
                        if (EliErr_ErrP(st)) {
                            eliEvalStk_PopN(st, EliEvalStack(st), bound);
                            return;
                        }
                        EliSym_BindSexp(st, symtmp2, bindval);
                    }
                    nodeptr = EliCons_GetCdr(consptr);
                    if (EliSexp_GetType(nodeptr) == e_data_list)
                        consptr = EliSexp_GetCons(nodeptr);
                }
            }
        }
        else
            looping = FALSE;
    }
    /* Now evaluate result clause into resbuf */
    if (l2 == 1)                       /* That is to say, no result clause */
        EliSexp_SetSym(st, resbuf, EliNilSym(st));
    else {
        nodeptr = EliCons_GetCdr(endstuff);
        consptr = EliSexp_GetCons(nodeptr);
        nodeptr = EliCons_GetCar(consptr);
        eliEval(st, nodeptr, resbuf);
        if (EliErr_ErrP(st)) {
            eliEvalStk_PopN(st, EliEvalStack(st), bound);
            return;
        }
    }
    eliEvalStk_PopN(st, EliEvalStack(st), bound);
}

void            Prim_READ(st, arglist, resbuf)
EliState_t     *st;
EliCons_t      *arglist;
EliSexp_t      *resbuf;
{
    EliSexp_t      *args[1], *err, *sexp, *evaledSexp;
    eliDataTypes_t  typeV[1];
    int             evalV[1], paramStat, exit = FALSE;
    FILE           *fp, *fopen();
    EliCons_t      *resultList = NULL;

    EliDebug(20, "Entering primitive READ", st, FALSE);
    typeV[0] = e_data_string;
    evalV[0] = TRUE;
    paramStat = EliProcessList(st, arglist, 0, 1, args, &err, typeV, evalV);
    if ((paramStat == -1) || (paramStat == -2)) {
        EliError(st, ELI_ERR_ARGLISTSIZE, err, "ELI-PRIMITIVE [READ (checking arglist size)]", 0);
        return;
    }
    if (paramStat == -1000) {
        EliError(st, ELI_ERR_BAD_ARGS, err, "ELI-PRIMITIVE [READ (arg is not a string)]", 0);
        return;
    }
    if (paramStat < 0)
        return;
    if (paramStat) {
        if (!(fp = fopen(EliStr_GetString(EliSexp_GetStr(args[0])), "r"))) {
            EliError(st, ELI_ERR_BAD_ARGS, args[0], "ELI-PRIMITIVE [READ (error opening file)]", errno);
            return;
        }
        while (!exit) {
            if (!(sexp = EliFGetSexp(st, fp))) {
                fclose(fp);
                return;
            }
            exit = EliProcessInfo.u_wrap;
            if (!exit) {
                if (!(evaledSexp = EliEval(st, sexp))) {
                    fclose(fp);
                    return;
                }
                if (!(resultList = EliAddToList(st, resultList, evaledSexp))) {
                    fclose(fp);
                    return;
                }
            }
        }
        fclose(fp);
        if (resultList)
            EliSexp_SetCons(st, resbuf, resultList);
        else
            EliSexp_SetSym(st, resbuf, EliNilSym(st));
    }
    else {
        if (!(sexp = EliGetSexp(st)))
            return;
        EliSexp_SetSexp(st, resbuf, sexp);
    }
}

void            Prim_CONSP(st, arglist, resbuf)
EliState_t     *st;
EliCons_t      *arglist;
EliSexp_t      *resbuf;
{
    EliSexp_t      *args[1], *tmp, *err;

    EliDebug(20, "Entering primitive CONSP", st, FALSE);
    if (1 != EliGetListCars(arglist, args, 1)) {
        if (!(err = eliSexp_GetNew_trace(st, EliTraceStk(st))))
            return;
        EliSexp_SetCons(st, err, arglist);
        EliError(st, ELI_ERR_ARGLISTSIZE, err, "ELI-PRIMITIVE [CONSP (checking arglist size)]", 0);
        return;
    }
    if (!(tmp = eliSexp_GetNew_trace(st, EliTraceStk(st))))
        return;
    eliEval(st, args[0], tmp);
    if (EliErr_ErrP(st))
        return;
    EliSexp_SetSym(st, resbuf, ((EliSexp_GetType(tmp) == e_data_list) || EliNilP(st, tmp)) ? EliTSym(st) : EliNilSym(st));
}

void            Prim_STRINGP(st, arglist, resbuf)
EliState_t     *st;
EliCons_t      *arglist;
EliSexp_t      *resbuf;
{
    EliSexp_t      *args[1], *tmp, *err;

    EliDebug(20, "Entering primitive STRINGP", st, FALSE);
    if (1 != EliGetListCars(arglist, args, 1)) {
        if (!(err = eliSexp_GetNew_trace(st, EliTraceStk(st))))
            return;
        EliSexp_SetCons(st, err, arglist);
        EliError(st, ELI_ERR_ARGLISTSIZE, err, "ELI-PRIMITIVE [STRINGP (checking arglist size)]", 0);
        return;
    }
    if (!(tmp = eliSexp_GetNew_trace(st, EliTraceStk(st))))
        return;
    eliEval(st, args[0], tmp);
    if (EliErr_ErrP(st))
        return;
    EliSexp_SetSym(st, resbuf, (EliSexp_GetType(tmp) == e_data_string) ? EliTSym(st) : EliNilSym(st));
}

void            Prim_ATOM(st, arglist, resbuf)
EliState_t     *st;
EliCons_t      *arglist;
EliSexp_t      *resbuf;
{
    EliSexp_t      *args[1], *tmp, *err;
    int             atomic;
    eliDataTypes_t  tmptype;

    EliDebug(20, "Entering primitive ATOM", st, FALSE);
    if (1 != EliGetListCars(arglist, args, 1)) {
        if (!(err = eliSexp_GetNew_trace(st, EliTraceStk(st))))
            return;
        EliSexp_SetCons(st, err, arglist);
        EliError(st, ELI_ERR_ARGLISTSIZE, err, "ELI-PRIMITIVE [ATOM (checking arglist size)]", 0);
        return;
    }
    if (!(tmp = eliSexp_GetNew_trace(st, EliTraceStk(st))))
        return;
    eliEval(st, args[0], tmp);
    if (EliErr_ErrP(st))
        return;
    tmptype = EliSexp_GetType(tmp);
    atomic = (tmptype == e_data_symbol) || (tmptype == e_data_integer) || (tmptype == e_data_string) || EliNilP(st, tmp);
    EliSexp_SetSym(st, resbuf, atomic ? EliTSym(st) : EliNilSym(st));
}

void            Prim_NUMBERP(st, arglist, resbuf)
EliState_t     *st;
EliCons_t      *arglist;
EliSexp_t      *resbuf;
{
    EliSexp_t      *args[1], *tmp, *err;

    EliDebug(20, "Entering primitive NUMBERP", st, FALSE);
    if (1 != EliGetListCars(arglist, args, 1)) {
        if (!(err = eliSexp_GetNew_trace(st, EliTraceStk(st))))
            return;
        EliSexp_SetCons(st, err, arglist);
        EliError(st, ELI_ERR_ARGLISTSIZE, err, "ELI-PRIMITIVE [NUMBERP (checking arglist size)]", 0);
        return;
    }
    if (!(tmp = eliSexp_GetNew_trace(st, EliTraceStk(st))))
        return;
    eliEval(st, args[0], tmp);
    if (EliErr_ErrP(st))
        return;
    EliSexp_SetSym(st, resbuf, (EliSexp_GetType(tmp) == e_data_integer) ? EliTSym(st) : EliNilSym(st));
}

void            Prim_LESSP(st, arglist, resbuf)
EliState_t     *st;
EliCons_t      *arglist;
EliSexp_t      *resbuf;
{
    EliSexp_t      *args[2], *tmp1, *tmp2, *err;

    EliDebug(20, "Entering primitive LESSP", st, FALSE);
    if (2 != EliGetListCars(arglist, args, 2)) {
        if (!(err = eliSexp_GetNew_trace(st, EliTraceStk(st))))
            return;
        EliSexp_SetCons(st, err, arglist);
        EliError(st, ELI_ERR_ARGLISTSIZE, err, "ELI-PRIMITIVE [LESSP (checking arglist size)]", 0);
        return;
    }
    if (!(tmp1 = eliSexp_GetNew_trace(st, EliTraceStk(st))))
        return;
    eliEval(st, args[0], tmp1);
    if (EliErr_ErrP(st))
        return;
    if (!(tmp2 = eliSexp_GetNew_trace(st, EliTraceStk(st))))
        return;
    eliEval(st, args[1], tmp2);
    if (EliErr_ErrP(st))
        return;
    if (EliSexp_GetType(tmp1) != e_data_integer) {
        EliError(st, ELI_ERR_BAD_ARGS, tmp1, "ELI-PRIMITIVE [LESSP (1st arg not an int)]", 0);
        return;
    }
    if (EliSexp_GetType(tmp2) != e_data_integer) {
        EliError(st, ELI_ERR_BAD_ARGS, tmp2, "ELI-PRIMITIVE [LESSP (2nd arg not an int)]", 0);
        return;
    }
    EliSexp_SetSym(st, resbuf, (EliSexp_GetInt(tmp1) < EliSexp_GetInt(tmp2)) ? EliTSym(st) : EliNilSym(st));
}

void            Prim_BOUNDP(st, arglist, resbuf)
EliState_t     *st;
EliCons_t      *arglist;
EliSexp_t      *resbuf;
{
    EliSexp_t      *args[1], *err, *tmp;
    EliSym_t       *symtmp;

    EliDebug(20, "Entering primitive BOUNDP", st, FALSE);
    if (1 != EliGetListCars(arglist, args, 1)) {
        if (!(err = eliSexp_GetNew_trace(st, EliTraceStk(st))))
            return;
        EliSexp_SetCons(st, err, arglist);
        EliError(st, ELI_ERR_ARGLISTSIZE, err, "ELI-PRIMITIVE [BOUNDP (checking arglist size)]", 0);
        return;
    }
    if (!(tmp = eliSexp_GetNew_trace(st, EliTraceStk(st))))
        return;
    eliEval(st, args[0], tmp);
    if (EliErr_ErrP(st))
        return;
    if (EliSexp_GetType(tmp) != e_data_symbol) {
        EliError(st, ELI_ERR_BAD_ARGS, tmp, "ELI-PRIMITIVE [BOUNDP (arg is not a symbol)]", 0);
        return;
    }
    if (!(symtmp = EliFindSym(st, EliStr_GetString(EliSym_GetName(EliSexp_GetSym(tmp))))))
        EliSexp_SetSym(st, resbuf, EliNilSym(st));
    else {
        if (EliSexp_GetType(EliSym_GetSexp(symtmp)) == e_data_none)
            EliSexp_SetSym(st, resbuf, EliNilSym(st));
        else
            EliSexp_SetSym(st, resbuf, EliTSym(st));
    }
}

void            Prim_MINUS(st, arglist, resbuf)
EliState_t     *st;
EliCons_t      *arglist;
EliSexp_t      *resbuf;
{
    EliSexp_t      *args[2], *err;
    eliDataTypes_t  typeV[2];
    int             evalV[2], paramStat;

    EliDebug(20, "Entering primitive MINUS", st, FALSE);
    typeV[0] = typeV[1] = e_data_integer;
    evalV[0] = evalV[1] = TRUE;
    paramStat = EliProcessList(st, arglist, 1, 2, args, &err, typeV, evalV);
    if ((paramStat == -1) || (paramStat == -2)) {
        EliError(st, ELI_ERR_ARGLISTSIZE, err, "ELI-PRIMITIVE [MINUS (checking arglist size)]", 0);
        return;
    }
    if (paramStat == -1000) {
        EliError(st, ELI_ERR_BAD_ARGS, err, "ELI-PRIMITIVE [MINUS (1st arg not an int)]", 0);
        return;
    }
    if (paramStat == -1001) {
        EliError(st, ELI_ERR_BAD_ARGS, err, "ELI-PRIMITIVE [MINUS (2nd arg not an int)]", 0);
        return;
    }
    if (paramStat < 0)
        return;
    if (paramStat == 1)
        EliSexp_SetInt(st, resbuf, -EliSexp_GetInt(args[0]));
    else
        EliSexp_SetInt(st, resbuf, EliSexp_GetInt(args[0]) - EliSexp_GetInt(args[1]));
}

void            Prim_TIMES(st, arglist, resbuf)
EliState_t     *st;
EliCons_t      *arglist;
EliSexp_t      *resbuf;
{
    EliCons_t      *argptr = arglist;
    EliSexp_t      *curarg, *evalarg, *tmp, *tmperr;
    int             args = EliListLen(arglist), i;
    long            result = 1L;

    EliDebug(20, "Entering primitive TIMES", st, FALSE);
    if (!args) {
        if (!(tmperr = eliSexp_GetNew_trace(st, EliTraceStk(st))))
            return;
        EliSexp_SetCons(st, tmperr, arglist);
        EliError(st, ELI_ERR_ARGLISTSIZE, tmperr, "ELI-PRIMITIVE [TIMES (checking arglist size)]", 0);
        return;
    }
    for (i = 0; i < args; ++i) {
        curarg = EliCons_GetCar(argptr);
        if (!(evalarg = eliSexp_GetNew_trace(st, EliTraceStk(st))))
            return;
        eliEval(st, curarg, evalarg);
        if (EliErr_ErrP(st))
            return;
        if (EliSexp_GetType(evalarg) != e_data_integer) {
            EliError(st, ELI_ERR_BAD_ARGS, evalarg, "ELI-PRIMITIVE [TIMES (an arg is not an int)]", 0);
            return;
        }
        result *= EliSexp_GetInt(evalarg);
        if (i < args - 1) {
            tmp = EliCons_GetCdr(argptr);
            argptr = EliSexp_GetCons(tmp);
        }
    }
    EliSexp_SetInt(st, resbuf, result);
}

void            Prim_DIV(st, arglist, resbuf)
EliState_t     *st;
EliCons_t      *arglist;
EliSexp_t      *resbuf;
{
    EliSexp_t      *args[2], *tmp1, *tmp2, *err;
    long            longtmp;

    EliDebug(20, "Entering primitive DIV", st, FALSE);
    if (2 != EliGetListCars(arglist, args, 2)) {
        if (!(err = eliSexp_GetNew_trace(st, EliTraceStk(st))))
            return;
        EliSexp_SetCons(st, err, arglist);
        EliError(st, ELI_ERR_ARGLISTSIZE, err, "ELI-PRIMITIVE [DIV (checking arglist size)]", 0);
        return;
    }
    if (!(tmp1 = eliSexp_GetNew_trace(st, EliTraceStk(st))))
        return;
    eliEval(st, args[0], tmp1);
    if (EliErr_ErrP(st))
        return;
    if (EliSexp_GetType(tmp1) != e_data_integer) {
        EliError(st, ELI_ERR_BAD_ARGS, tmp1, "ELI-PRIMITIVE [DIV (1st arg not an int)]", 0);
        return;
    }
    if (!(tmp2 = eliSexp_GetNew_trace(st, EliTraceStk(st))))
        return;
    eliEval(st, args[1], tmp2);
    if (EliErr_ErrP(st))
        return;
    if (EliSexp_GetType(tmp2) != e_data_integer) {
        EliError(st, ELI_ERR_BAD_ARGS, tmp2, "ELI-PRIMITIVE [DIV (2nd arg not an int)]", 0);
        return;
    }
    if ((longtmp = EliSexp_GetInt(tmp2)) == 0L) {
        EliError(st, ELI_ERR_BAD_ARGS, tmp2, "ELI-PRIMITIVE [DIV (division by zero)]", 0);
        return;
    }
    EliSexp_SetInt(st, resbuf, (long) (EliSexp_GetInt(tmp1) / longtmp));
}

void            Prim_STRCAT(st, arglist, resbuf)
EliState_t     *st;
EliCons_t      *arglist;
EliSexp_t      *resbuf;
{
    EliSexp_t     **args, *err;
    eliDataTypes_t *typeV;
    int            *evalV, len = EliListLen(arglist), paramStat, i, resultLen = 1;
    EliStr_t       *strTmp;
    char           *thisStr, *buf, *bufPtr;
    static char     errStr[80];

    EliDebug(20, "Entering primitive STRCAT", st, FALSE);
    if (len < 1) {
        if (!(err = EliSexp_GetNew(st)))
            return;
        EliSexp_SetCons(st, err, arglist);
        EliError(st, ELI_ERR_ARGLISTSIZE, err, "ELI-PRIMITIVE [STRCAT (checking arglist size)]", 0);
        return;
    }
    if (!(args = (EliSexp_t **) malloc(len * sizeof(EliSexp_t *)))) {
        EliError(st, ELI_ERR_OUT_OF_MEM, NULL, "ELI-PRIMITIVE [STRCAT (allocating object array)]", 0);
        return;
    }
    if (!(typeV = (eliDataTypes_t *) malloc(len * sizeof(eliDataTypes_t *)))) {
        free(args);
        EliError(st, ELI_ERR_OUT_OF_MEM, NULL, "ELI-PRIMITIVE [STRCAT (allocating type array)]", 0);
        return;
    }
    if (!(evalV = (int *) malloc(len * sizeof(int)))) {
        free(args);
        free(typeV);
        EliError(st, ELI_ERR_OUT_OF_MEM, NULL, "ELI-PRIMITIVE [STRCAT (allocating eval array)]", 0);
        return;
    }
    for (i = 0; i < len; ++i) {
        typeV[i] = e_data_string;
        evalV[i] = TRUE;
    }
    paramStat = EliProcessList(st, arglist, 1, len, args, &err, typeV, evalV);
    free(typeV);
    free(evalV);
    if (paramStat <= -2000) {
        free(args);
        return;
    }
    if (paramStat <= -1000) {
        free(args);
        sprintf(errStr, "ELI-PRIMITIVE [STRCAT (arg %d is not a string)]", 1 - (paramStat + 1000));
        EliError(st, ELI_ERR_BAD_ARGS, err, errStr, 0);
        return;
    }
    for (i = 0; i < len; ++i)
        resultLen += strlen(EliStr_GetString(EliSexp_GetStr(args[i])));
    if (!(buf = bufPtr = EliStringOpBuf(resultLen))) {
        free(args);
        EliError(st, ELI_ERR_OUT_OF_MEM, NULL, "ELI-PRIMITIVE [STRCAT (allocating result string)]", 0);
        return;
    }
    for (i = 0; i < len; ++i) {
        strcpy(bufPtr, thisStr = EliStr_GetString(EliSexp_GetStr(args[i])));
        bufPtr += strlen(thisStr);
    }
    free(args);
    if (!(strTmp = eliStringTable_FindOrMake(st, EliStringTable(st), buf)))
        return;
    EliSexp_SetStr(st, resbuf, strTmp);
}

/* This one is called (index string char) [where the char is a one-character
 * string type].  Like index(3), it returns the first substring (left-to-right)
 * of string that begins with char.  If char is a more-than-one-character
 * string, only the first character is significant.  If there is no such substring,
 * NIL is returned.
 */
void            Prim_INDEX(st, arglist, resbuf)
EliState_t     *st;
EliCons_t      *arglist;
EliSexp_t      *resbuf;
{
    EliSexp_t      *args[2], *restmp, *err;
    EliStr_t       *str1, *str2, *yaStr;
    char           *cptr;

    EliDebug(20, "Entering primitive INDEX", st, FALSE);
    if (EliGetListCars(arglist, args, 2) != 2) {
        if (!(err = eliSexp_GetNew_trace(st, EliTraceStk(st))))
            return;
        EliSexp_SetCons(st, err, arglist);
        EliError(st, ELI_ERR_ARGLISTSIZE, err, "ELI-PRIMITIVE [INDEX (checking arglist size)]", 0);
        return;
    }
    if (!(restmp = eliSexp_GetNew_trace(st, EliTraceStk(st))))
        return;
    eliEval(st, args[0], restmp);
    if (EliErr_ErrP(st))
        return;
    if (EliSexp_GetType(restmp) != e_data_string) {
        EliError(st, ELI_ERR_BAD_ARGS, restmp, "ELI-PRIMITIVE [INDEX (1st arg not a string)]", 0);
        return;
    }
    str1 = EliSexp_GetStr(restmp);
    eliEval(st, args[1], restmp);
    if (EliErr_ErrP(st))
        return;
    if (EliSexp_GetType(restmp) != e_data_string) {
        EliError(st, ELI_ERR_BAD_ARGS, restmp, "ELI-PRIMITIVE [INDEX (2nd arg not a string)]", 0);
        return;
    }
    str2 = EliSexp_GetStr(restmp);
    cptr = index(EliStr_GetString(str1), *(EliStr_GetString(str2)));
    if (cptr) {
        if (!(yaStr = eliStringTable_FindOrMake(st, EliStringTable(st), cptr)))
            return;
        else
            EliSexp_SetStr(st, resbuf, yaStr);
    }
    else
        EliSexp_SetSym(st, resbuf, EliNilSym(st));
}

void            Prim_RINDEX(st, arglist, resbuf)
EliState_t     *st;
EliCons_t      *arglist;
EliSexp_t      *resbuf;
{
    EliSexp_t      *args[2], *restmp, *err;
    EliStr_t       *str1, *str2;
    char           *cptr;

    EliDebug(20, "Entering primitive RINDEX", st, FALSE);
    if (EliGetListCars(arglist, args, 2) != 2) {
        if (!(err = eliSexp_GetNew_trace(st, EliTraceStk(st))))
            return;
        EliSexp_SetCons(st, err, arglist);
        EliError(st, ELI_ERR_ARGLISTSIZE, err, "ELI-PRIMITIVE [RINDEX (checking arglist size)]", 0);
        return;
    }
    if (!(restmp = eliSexp_GetNew_trace(st, EliTraceStk(st))))
        return;
    eliEval(st, args[0], restmp);
    if (EliErr_ErrP(st))
        return;
    if (EliSexp_GetType(restmp) != e_data_string) {
        EliError(st, ELI_ERR_BAD_ARGS, restmp, "ELI-PRIMITIVE [RINDEX (1st arg not a string)]", 0);
        return;
    }
    str1 = EliSexp_GetStr(restmp);
    eliEval(st, args[1], restmp);
    if (EliErr_ErrP(st))
        return;
    if (EliSexp_GetType(restmp) != e_data_string) {
        EliError(st, ELI_ERR_BAD_ARGS, restmp, "ELI-PRIMITIVE [RINDEX (2nd arg not a string)]", 0);
        return;
    }
    str2 = EliSexp_GetStr(restmp);
    cptr = rindex(EliStr_GetString(str1), *(EliStr_GetString(str2)));
    if (cptr)
        EliSexp_SetStr(st, resbuf, eliStringTable_FindOrMake(st, EliStringTable(st), cptr));
    else
        EliSexp_SetSym(st, resbuf, EliNilSym(st));
}

void            Prim_STRDECOMPOSE(st, arglist, resbuf)
EliState_t     *st;
EliCons_t      *arglist;
EliSexp_t      *resbuf;
{
    EliSexp_t      *args[3], *err, *tmp;
    EliStr_t       *strNodes[3];
    int             ignoreCase, processResult, evalV[3];
    EliCons_t      *resultList = NULL;
    eliDataTypes_t  argV[3];
    char           *pat, *ref, c, *foundLoc, *hold;

    EliDebug(20, "Entering primitive STRDECOMPOSE", st, FALSE);
    argV[0] = argV[1] = e_data_string;
    argV[2] = e_data_none;
    evalV[0] = evalV[1] = evalV[2] = TRUE;
    processResult = EliProcessList(st, arglist, 2, 3, args, &err, argV, evalV);
    if ((processResult == -1) || (processResult == -2)) {
        EliError(st, ELI_ERR_ARGLISTSIZE, err, "ELI-PRIMITIVE [STRDECOMPOSE (checking arglist size)]", 0);
        return;
    }
    if (processResult == -1000) {
        EliError(st, ELI_ERR_BAD_ARGS, err, "ELI-PRIMITIVE [STRDECOMPOSE (1st arg is not a string)]", 0);
        return;
    }
    if (processResult == -1001) {
        EliError(st, ELI_ERR_BAD_ARGS, err, "ELI-PRIMITIVE [STRDECOMPOSE (2nd arg is not a string)]", 0);
        return;
    }
    if (processResult < 0)
        return;
    pat = EliStr_GetString(EliSexp_GetStr(args[0]));
    ref = EliStr_GetString(EliSexp_GetStr(args[1]));
    ignoreCase = ((processResult == 3) && !EliNilP(st, args[2]));

    if (foundLoc = eliFindPrefix(pat, ref, ignoreCase)) {

        c = *foundLoc;
        *foundLoc = '\0';
        if (!(hold = EliSaveString(ref))) {
            EliError(st, ELI_ERR_OUT_OF_MEM, NULL, "ELI-PRIMITIVE [STRDECOMPOSE (allocating space for 1st result string)]", 0);
            return;
        }
        *foundLoc = c;
        if (!(strNodes[0] = EliStringTable_FindOrMake(st, hold)))
            return;
        free(hold);

        c = *(foundLoc + strlen(pat));
        *(foundLoc + strlen(pat)) = '\0';
        if (!(hold = EliSaveString(foundLoc))) {
            EliError(st, ELI_ERR_OUT_OF_MEM, NULL, "ELI-PRIMITIVE [STRDECOMPOSE (allocating space for 2nd result string)]", 0);
            return;
        }
        *(foundLoc + strlen(pat)) = c;
        if (!(strNodes[1] = EliStringTable_FindOrMake(st, hold)))
            return;
        free(hold);

        if (!(strNodes[2] = EliStringTable_FindOrMake(st, (foundLoc + strlen(pat)))))
            return;

        if (!(tmp = EliSexp_GetNew(st)))
            return;
        EliSexp_SetStr(st, tmp, strNodes[0]);
        if (!(resultList = EliAddToList(st, resultList, tmp)))
            return;
        if (!(tmp = EliSexp_GetNew(st)))
            return;
        EliSexp_SetStr(st, tmp, strNodes[1]);
        if (!(resultList = EliAddToList(st, resultList, tmp)))
            return;
        if (!(tmp = EliSexp_GetNew(st)))
            return;
        EliSexp_SetStr(st, tmp, strNodes[2]);
        if (!(resultList = EliAddToList(st, resultList, tmp)))
            return;
        EliSexp_SetCons(st, resbuf, resultList);
    }
    else
        EliSexp_SetSym(st, resbuf, EliNilSym(st));
}

void            Prim_STRLEN(st, arglist, resbuf)
EliState_t     *st;
EliCons_t      *arglist;
EliSexp_t      *resbuf;
{
    EliSexp_t      *args[1], *err, *tmp;

    EliDebug(20, "Entering primitive STRLEN", st, FALSE);
    if (EliGetListCars(arglist, args, 1) != 1) {
        if (!(err = eliSexp_GetNew_trace(st, EliTraceStk(st))))
            return;
        EliSexp_SetCons(st, err, arglist);
        EliError(st, ELI_ERR_ARGLISTSIZE, err, "ELI-PRIMITIVE [STRLEN (checking arglist size)]", 0);
        return;
    }
    if (!(tmp = eliSexp_GetNew_trace(st, EliTraceStk(st))))
        return;
    eliEval(st, args[0], tmp);
    if (EliErr_ErrP(st))
        return;
    if (EliSexp_GetType(tmp) != e_data_string) {
        EliError(st, ELI_ERR_BAD_ARGS, tmp, "ELI-PRIMITIVE [STRLEN (arg is not a string)]", 0);
        return;
    }
    EliSexp_SetInt(st, resbuf, (long) strlen(EliStr_GetString(EliSexp_GetStr(tmp))));
}

void            Prim_LCSTRING(st, arglist, resbuf)
EliState_t     *st;
EliCons_t      *arglist;
EliSexp_t      *resbuf;
{
    EliSexp_t      *args[1], *err, *nodetmp;
    char           *oldstring, *oldptr, *newptr, c;
    int             len;
    char           *buf = NULL;
    EliStr_t       *newstr;

    EliDebug(20, "Entering primitive LCSTRING", st, FALSE);
    if (EliGetListCars(arglist, args, 1) != 1) {
        if (!(err = eliSexp_GetNew_trace(st, EliTraceStk(st))))
            return;
        EliSexp_SetCons(st, err, arglist);
        EliError(st, ELI_ERR_ARGLISTSIZE, err, "ELI-PRIMITIVE [LCSTRING (checking arglist size)]", 0);
        return;
    }
    if (!(nodetmp = eliSexp_GetNew_trace(st, EliTraceStk(st))))
        return;
    eliEval(st, args[0], nodetmp);
    if (EliErr_ErrP(st))
        return;
    if (EliSexp_GetType(nodetmp) != e_data_string) {
        EliError(st, ELI_ERR_BAD_ARGS, nodetmp, "ELI-PRIMITIVE [LCSTRING (arg is not a string)]", 0);
        return;
    }
    oldstring = EliStr_GetString(EliSexp_GetStr(nodetmp));
    len = strlen(oldstring) + 1;
    if (!(buf = EliStringOpBuf(len))) {
        EliError(st, ELI_ERR_OUT_OF_MEM, NULL, "ELI-PRIMITIVE [LCSTRING (allocating result string)]", 0);
        return;
    }
    for (oldptr = oldstring, newptr = buf; (*newptr) = (isupper((c = (*oldptr))) ? tolower(c) : c); ++oldptr, ++newptr);
    if (!(newstr = eliStringTable_FindOrMake(st, EliStringTable(st), buf)))
        return;
    EliSexp_SetStr(st, resbuf, newstr);
}

void            Prim_APPEND(st, arglist, resbuf)
EliState_t     *st;
EliCons_t      *arglist;
EliSexp_t      *resbuf;
{
    EliSexp_t      *thisnode, *evalnode, *aNode, *anotherNode, *yetAnotherNode;
    EliCons_t      *thisarg, *reslist = NULL, *lastcell = NULL, *newcell, *aCell;
    int             l = EliListLen(arglist), looping, i;

    EliDebug(20, "Entering primitive APPEND", st, FALSE);
    if (l) {
        thisarg = arglist;
        for (i = 0; i < l; ++i) {
            thisnode = EliCons_GetCar(thisarg);
            if (!(evalnode = eliSexp_GetNew_trace(st, EliTraceStk(st))))
                return;
            eliEval(st, thisnode, evalnode);
            if (EliErr_ErrP(st))
                return;
            if (!EliNilP(st, evalnode)) {
                if (EliSexp_GetType(evalnode) == e_data_list) {
                    aNode = evalnode;
                    looping = TRUE;
                    while (looping) {
                        aCell = EliSexp_GetCons(aNode);
                        anotherNode = EliCons_GetCar(aCell);
                        if (!(newcell = eliCons_GetNew_trace(st, EliTraceStk(st))))
                            return;
                        EliCons_BindCar(st, newcell, anotherNode);
                        if (lastcell) {
                            if (!(yetAnotherNode = eliSexp_GetNew_trace(st, EliTraceStk(st))))
                                return;
                            EliSexp_SetCons(st, yetAnotherNode, newcell);
                            EliCons_BindCdr(st, lastcell, yetAnotherNode);
                            lastcell = newcell;
                        }
                        else
                            reslist = lastcell = newcell;
                        aNode = EliCons_GetCdr(aCell);
                        looping = (EliSexp_GetType(aNode) == e_data_list);
                    }
                }
                else {
                    EliError(st, ELI_ERR_BAD_ARGS, evalnode, "ELI-PRIMITIVE [APPEND (an arg is not a list)]", 0);
                    return;
                }
            }
            thisarg = EliGetNextCell(thisarg);
        }
    }
    if (reslist)
        EliSexp_SetCons(st, resbuf, reslist);
    else
        EliSexp_SetSym(st, resbuf, EliNilSym(st));
}

void            Prim_RE_STRCONTAINS(st, arglist, resbuf)
EliState_t     *st;
EliCons_t      *arglist;
EliSexp_t      *resbuf;
{
    EliSexp_t      *args[3], *err, *patNode, *refNode;
    int             numargs, rxpResult;
    char           *pat, *ref;
    regexp         *rptr, *reg_comp();

    EliDebug(20, "Entering primitive RE-STRCONTAINS", st, FALSE);
    numargs = EliGetListCars(arglist, args, 2);
    if (numargs < 2) {
        if (!(err = eliSexp_GetNew_trace(st, EliTraceStk(st))))
            return;
        EliSexp_SetCons(st, err, arglist);
        EliError(st, ELI_ERR_ARGLISTSIZE, err, "ELI-PRIMITIVE [RE-STRCONTAINS (checking arglist size)]", 0);
        return;
    }
    if (!(patNode = eliSexp_GetNew_trace(st, EliTraceStk(st))))
        return;
    eliEval(st, args[0], patNode);
    if (EliErr_ErrP(st))
        return;
    if (EliSexp_GetType(patNode) != e_data_string) {
        EliError(st, ELI_ERR_BAD_ARGS, patNode, "ELI-PRIMITIVE [RE-STRCONTAINS (1st arg not a string)]", 0);
        return;
    }
    pat = EliStr_GetString(EliSexp_GetStr(patNode));
    if (!(refNode = eliSexp_GetNew_trace(st, EliTraceStk(st))))
        return;
    eliEval(st, args[1], refNode);
    if (EliErr_ErrP(st))
        return;
    if (EliSexp_GetType(refNode) != e_data_string) {
        EliError(st, ELI_ERR_BAD_ARGS, refNode, "ELI-PRIMITIVE [RE-STRCONTAINS (2nd arg not a string)]", 0);
        return;
    }
    ref = EliStr_GetString(EliSexp_GetStr(refNode));
    if (!(rptr = reg_comp(pat))) {
        EliError(st, ELI_ERR_BAD_ARGS, patNode, "ELI-PRIMITIVE [RE-STRCONTAINS (compiling regular expression)]", 0);
        return;
    }
    rxpResult = reg_exec(rptr, ref);
    free(rptr);
    if (rxpResult)
        EliSexp_SetSym(st, resbuf, EliTSym(st));
    else
        EliSexp_SetSym(st, resbuf, EliNilSym(st));
}

/* This one gets called (substring str start len) where the first character
 * is numbered 0
 *
 * By the way, I really overdid it with the error checking here.
 */
void            Prim_SUBSTRING(st, arglist, resbuf)
EliState_t     *st;
EliCons_t      *arglist;
EliSexp_t      *resbuf;
{
    EliSexp_t      *args[3], *err, *evalnode;
    EliStr_t       *theStringNode, *result;
    long            start, len;
    int             theLen;
    char           *buf, *theString;

    EliDebug(20, "Entering primitive SUBSTRING", st, FALSE);
    if (3 != EliGetListCars(arglist, args, 3)) {
        if (!(err = eliSexp_GetNew_trace(st, EliTraceStk(st))))
            return;
        EliSexp_SetCons(st, err, arglist);
        EliError(st, ELI_ERR_ARGLISTSIZE, err, "ELI-PRIMITIVE [SUBSTRING (checking arglist size)]", 0);
        return;
    }
    if (!(evalnode = eliSexp_GetNew_trace(st, EliTraceStk(st))))
        return;
    eliEval(st, args[0], evalnode);
    if (EliErr_ErrP(st))
        return;
    if (EliSexp_GetType(evalnode) != e_data_string) {
        EliError(st, ELI_ERR_BAD_ARGS, evalnode, "ELI-PRIMITIVE [SUBSTRING (1st arg not a string)]", 0);
        return;
    }
    theStringNode = EliSexp_GetStr(evalnode);
    eliEval(st, args[1], evalnode);
    if (EliErr_ErrP(st))
        return;
    if (EliSexp_GetType(evalnode) != e_data_integer) {
        EliError(st, ELI_ERR_BAD_ARGS, evalnode, "ELI-PRIMITIVE [SUBSTRING (2nd arg not an int)]", 0);
        return;
    }
    start = EliSexp_GetInt(evalnode);
    eliEval(st, args[2], evalnode);
    if (EliErr_ErrP(st))
        return;
    if (EliSexp_GetType(evalnode) != e_data_integer) {
        EliError(st, ELI_ERR_BAD_ARGS, evalnode, "ELI-PRIMITIVE [SUBSTRING (3rd arg not an int)]", 0);
        return;
    }
    len = EliSexp_GetInt(evalnode);

    if (start > ((long) (theLen = strlen(theString = EliStr_GetString(theStringNode))))) {
        if (!(err = eliSexp_GetNew_trace(st, EliTraceStk(st))))
            return;
        EliSexp_SetInt(st, err, start);
        EliError(st, ELI_ERR_BAD_ARGS, err, "ELI-PRIMITIVE [SUBSTRING (attempt to start past end of string)]", 0);
        return;
    }
    if ((start + len) > ((long) theLen)) {
        if (!(err = eliSexp_GetNew_trace(st, EliTraceStk(st))))
            return;
        EliSexp_SetInt(st, err, len);
        EliError(st, ELI_ERR_BAD_ARGS, err, "ELI-PRIMITIVE [SUBSTRING (attempt to extract beyond end of string)]", 0);
        return;
    }
    if (start < 0L) {
        if (!(err = eliSexp_GetNew_trace(st, EliTraceStk(st))))
            return;
        EliSexp_SetInt(st, err, start);
        EliError(st, ELI_ERR_BAD_ARGS, err, "ELI-PRIMITIVE [SUBSTRING (2nd arg less than zero)]", 0);
        return;
    }
    if (len < 0L) {
        if (!(err = eliSexp_GetNew_trace(st, EliTraceStk(st))))
            return;
        EliSexp_SetInt(st, err, len);
        EliError(st, ELI_ERR_BAD_ARGS, err, "ELI-PRIMITIVE [SUBSTRING (3rd arg less than zero)]", 0);
        return;
    }
    if (!(buf = EliStringOpBuf((int) (len + 1L)))) {
        EliError(st, ELI_ERR_OUT_OF_MEM, NULL, "ELI-PRIMITIVE [SUBSTRING (allocating result string)]", 0);
        return;
    }
    strncpy(buf, theString + start, (int) len);
    buf[len] = '\0';
    if (!(result = eliStringTable_FindOrMake(st, EliStringTable(st), buf)))
        return;
    EliSexp_SetStr(st, resbuf, result);
}

void            Prim_SYMBOLP(st, arglist, resbuf)
EliState_t     *st;
EliCons_t      *arglist;
EliSexp_t      *resbuf;
{
    EliSexp_t      *args[1], *tmp, *err;

    EliDebug(20, "Entering primitive SYMBOLP", st, FALSE);
    if (1 != EliGetListCars(arglist, args, 1)) {
        if (!(err = eliSexp_GetNew_trace(st, EliTraceStk(st))))
            return;
        EliSexp_SetCons(st, err, arglist);
        EliError(st, ELI_ERR_ARGLISTSIZE, err, "ELI-PRIMITIVE [SYMBOLP (checking arglist size)]", 0);
        return;
    }
    if (!(tmp = eliSexp_GetNew_trace(st, EliTraceStk(st))))
        return;
    eliEval(st, args[0], tmp);
    if (EliErr_ErrP(st))
        return;
    EliSexp_SetSym(st, resbuf, ((EliSexp_GetType(tmp) == e_data_symbol) || EliNilP(st, tmp)) ? EliTSym(st) : EliNilSym(st));
}

void            Prim_PLUMBER(st, arglist, resbuf)
EliState_t     *st;
EliCons_t      *arglist;
EliSexp_t      *resbuf;
{
    EliSexp_t      *args[1], *tmp, *err;
    FILE           *fp, *fopen();

    EliDebug(20, "Entering primitive PLUMBER", st, FALSE);
    if (1 != EliGetListCars(arglist, args, 1)) {
        if (!(err = eliSexp_GetNew_trace(st, EliTraceStk(st))))
            return;
        EliSexp_SetCons(st, err, arglist);
        EliError(st, ELI_ERR_ARGLISTSIZE, err, "ELI-PRIMITIVE [PLUMBER (checking arglist size)]", 0);
        return;
    }
    if (!(tmp = eliSexp_GetNew_trace(st, EliTraceStk(st))))
        return;
    eliEval(st, args[0], tmp);
    if (EliErr_ErrP(st))
        return;
    if (EliSexp_GetType(tmp) != e_data_string) {
        EliError(st, ELI_ERR_BAD_ARGS, tmp, "ELI-PRIMITIVE [PLUMBER (arg is not a string)]", 0);
        return;
    }
    if (!(fp = fopen(EliStr_GetString(EliSexp_GetStr(tmp)), "w"))) {
        EliError(st, ELI_ERR_BAD_ARGS, tmp, "ELI-PRIMITIVE [PLUMBER (error opening file)]", 0);
        return;
    }
#if defined(DEBUG_MALLOC_ENV) && defined(ELI_DEBUG_MALLOC_ENV)
    plumber(fp);
#else                                  /* #if defined(DEBUG_MALLOC_ENV) && defined(ELI_DEBUG_MALLOC_ENV) */
    fprintf(fp, "\nTHIS MODULE WAS COMPILED WITH DEBUG_MALLOC_ENV or ELI_DEBUG_MALLOC_ENV UNDEFINED.\n");
#endif                                 /* #if defined(DEBUG_MALLOC_ENV) && defined(ELI_DEBUG_MALLOC_ENV) */
    fclose(fp);
    EliSexp_SetSym(st, resbuf, EliTSym(st));    /* BOGUS -- change this to
                                                 * reflect something
                                                 * meaningful */
}

/* This primitive works something like strdecompose.
 * The first argument is a regular expression
 * The second is a reference string to match against.  The optional
 * third argument specifies whether to ignore case; i.e., if it's
 * present, and non-nil, then ignore case.
 */

void            Prim_RE_STRDECOMPOSE(st, arglist, resbuf)
EliState_t     *st;
EliCons_t      *arglist;
EliSexp_t      *resbuf;
{
    EliSexp_t      *args[3], *err, *patNode, *refNode, *strs[3], *cdrs[2];
    int             numargs, rxpResult;
    char           *pat, *ref, tempChar, *hold;
    EliStr_t       *strNodes[3];
    EliCons_t      *consCells[3];
    regexp         *rptr, *reg_comp();

    EliDebug(20, "Entering primitive RE-STRDECOMPOSE", st, FALSE);
    numargs = EliGetListCars(arglist, args, 2);
    if (numargs < 2) {
        if (!(err = eliSexp_GetNew_trace(st, EliTraceStk(st))))
            return;
        EliSexp_SetCons(st, err, arglist);
        EliError(st, ELI_ERR_ARGLISTSIZE, err, "ELI-PRIMITIVE [RE-STRDECOMPOSE (checking arglist size)]", 0);
        return;
    }
    if (!(patNode = eliSexp_GetNew_trace(st, EliTraceStk(st))))
        return;
    eliEval(st, args[0], patNode);
    if (EliErr_ErrP(st))
        return;
    if (EliSexp_GetType(patNode) != e_data_string) {
        EliError(st, ELI_ERR_BAD_ARGS, patNode, "ELI-PRIMITIVE [RE-STRDECOMPOSE (1st arg not a string)]", 0);
        return;
    }
    pat = EliStr_GetString(EliSexp_GetStr(patNode));
    if (!(refNode = eliSexp_GetNew_trace(st, EliTraceStk(st))))
        return;
    eliEval(st, args[1], refNode);
    if (EliErr_ErrP(st))
        return;
    if (EliSexp_GetType(refNode) != e_data_string) {
        EliError(st, ELI_ERR_BAD_ARGS, refNode, "ELI-PRIMITIVE [RE-STRDECOMPOSE (2nd arg not a string)]", 0);
        return;
    }
    ref = EliStr_GetString(EliSexp_GetStr(refNode));
    if (!(rptr = reg_comp(pat))) {
        EliError(st, ELI_ERR_BAD_ARGS, patNode, "ELI-PRIMITIVE [RE-STRDECOMPOSE (compiling regular expression)]", 0);
        return;
    }
    rxpResult = reg_exec(rptr, ref);
    if (rxpResult) {
        tempChar = *(rptr->startp[0]);
        *(rptr->startp[0]) = '\0';
        if (!(hold = EliSaveString(ref))) {
            EliError(st, ELI_ERR_OUT_OF_MEM, NULL, "ELI-PRIMITIVE [RE-STRDECOMPOSE (allocating space for 1st result string)]", 0);
            free(rptr);
            return;
        }
        *(rptr->startp[0]) = tempChar;
        strNodes[0] = eliStringTable_FindOrMake(st, EliStringTable(st), hold);
        free(hold);
        if (!(strNodes[0])) {
            free(rptr);
            return;
        }
        tempChar = *(rptr->endp[0]);
        *(rptr->endp[0]) = '\0';
        if (!(hold = EliSaveString(rptr->startp[0]))) {
            EliError(st, ELI_ERR_OUT_OF_MEM, NULL, "ELI-PRIMITIVE [RE-STRDECOMPOSE (allocating space for 2nd result string)]", 0);
            free(rptr);
            return;
        }
        *(rptr->endp[0]) = tempChar;
        strNodes[1] = eliStringTable_FindOrMake(st, EliStringTable(st), hold);
        free(hold);
        if (!(strNodes[1])) {
            free(rptr);
            return;
        }
        if (!(strNodes[2] = eliStringTable_FindOrMake(st, EliStringTable(st), rptr->endp[0]))) {
            free(rptr);
            return;
        }
        free(rptr);

        /*
         * Three string nodes are now allocated; place them in sexp nodes and
         * put them in a cons list
         */
        if ((!(strs[0] = eliSexp_GetNew_trace(st, EliTraceStk(st)))) || (!(strs[1] = eliSexp_GetNew_trace(st, EliTraceStk(st)))) || (!(strs[2] = eliSexp_GetNew_trace(st, EliTraceStk(st)))))
            return;
        EliSexp_SetStr(st, strs[0], strNodes[0]);
        EliSexp_SetStr(st, strs[1], strNodes[1]);
        EliSexp_SetStr(st, strs[2], strNodes[2]);
        if ((!(consCells[0] = eliCons_GetNew_trace(st, EliTraceStk(st)))) || (!(consCells[1] = eliCons_GetNew_trace(st, EliTraceStk(st)))) || (!(consCells[2] = eliCons_GetNew_trace(st, EliTraceStk(st)))))
            return;
        if ((!(cdrs[0] = eliSexp_GetNew_trace(st, EliTraceStk(st)))) || (!(cdrs[1] = eliSexp_GetNew_trace(st, EliTraceStk(st)))))
            return;
        EliSexp_SetCons(st, cdrs[0], consCells[1]);
        EliSexp_SetCons(st, cdrs[1], consCells[2]);
        EliCons_BindCar(st, consCells[0], strs[0]);
        EliCons_BindCdr(st, consCells[0], cdrs[0]);
        EliCons_BindCar(st, consCells[1], strs[1]);
        EliCons_BindCdr(st, consCells[1], cdrs[1]);
        EliCons_BindCar(st, consCells[2], strs[2]);
        EliSexp_SetCons(st, resbuf, consCells[0]);
    }
    else {
        free(rptr);
        EliSexp_SetSym(st, resbuf, EliNilSym(st));
    }
}

void            Prim_GENSYM(st, arglist, resbuf)
EliState_t     *st;
EliCons_t      *arglist;
EliSexp_t      *resbuf;
{
    static int      num = 1;
    char            name[GENSYM_NAMELEN];
    EliSexp_t      *err;
    EliSym_t       *symNode;

    EliDebug(20, "Entering primitive GENSYM", st, FALSE);
    if (EliListLen(arglist)) {
        if (!(err = eliSexp_GetNew_trace(st, st->g_errstk)))
            return;
        EliSexp_SetCons(st, err, arglist);
        EliError(st, ELI_ERR_ARGLISTSIZE, err, "ELI-PRIMITIVE [GENSYM (checking arglist size)]", 0);
        return;
    }
    do {
        sprintf(name, "G#%X", num++);
    } while (EliFindSym(st, name));
    if (!(symNode = eliSymTab_FindOrMake(st, EliTempSymTable(st), name)))
        return;
    eliSym_SetScope(symNode, e_sym_known);
    EliSexp_SetSym(st, resbuf, symNode);
}

void            Prim_FUNCTION(st, arglist, resbuf)
EliState_t     *st;
EliCons_t      *arglist;
EliSexp_t      *resbuf;
{
    EliSexp_t      *args[1], *err, *tmp;
    EliFn_t        *fnTmp;
    eliFnTypes_t    fnType;

    EliDebug(20, "Entering primitive FUNCTION", st, FALSE);
    if (1 != EliGetListCars(arglist, args, 1)) {
        if (!(err = eliSexp_GetNew_trace(st, EliTraceStk(st))))
            return;
        EliSexp_SetCons(st, err, arglist);
        EliError(st, ELI_ERR_ARGLISTSIZE, err, "ELI-PRIMITIVE [FUNCTION (checking arglist size)]", 0);
        return;
    }
    if (!(tmp = eliSexp_GetNew_trace(st, EliTraceStk(st))))
        return;
    eliEval(st, args[0], tmp);
    if (EliErr_ErrP(st))
        return;
    if (EliSexp_GetType(tmp) != e_data_symbol) {
        EliError(st, ELI_ERR_BAD_ARGS, tmp, "ELI-PRIMITIVE [FUNCTION (arg is not a symbol)]", 0);
        return;
    }
    if ((fnType = eliFn_GetType(fnTmp = EliSym_GetFn(EliSexp_GetSym(tmp)))) == e_fn_none) {
        EliError(st, ELI_ERR_BAD_ARGS, tmp, "ELI-PRIMITIVE [FUNCTION (symbol has no function bound to it)]", 0);
        return;
    }
    if (fnType == e_fn_compiled)
        EliSexp_SetFn(st, resbuf, fnTmp);
    else                               /* It's a list */
        EliSexp_SetCons(st, resbuf, eliFn_GetCons(fnTmp));
}

void            Prim_LOAD(st, arglist, resbuf)
EliState_t     *st;
EliCons_t      *arglist;
EliSexp_t      *resbuf;
{
    EliSexp_t      *argnodes[1], *err, *resbuf2;

    EliDebug(20, "Entering primitive LOAD", st, FALSE);
    if (EliGetListCars(arglist, argnodes, 1) != 1) {
        if (!(err = eliSexp_GetNew_trace(st, EliTraceStk(st)))) {
            return;
        }
        EliSexp_SetCons(st, err, arglist);
        EliError(st, ELI_ERR_ARGLISTSIZE, err, "ELI-PRIMITIVE [LOAD (checking arglist size)]", 0);
        return;
    }
    if (!(resbuf2 = eliSexp_GetNew_trace(st, EliTraceStk(st)))) {
        return;
    }
    eliEval(st, argnodes[0], resbuf2);
    if (EliErr_ErrP(st)) {
        return;
    }
    if (EliSexp_GetType(resbuf2) != e_data_string) {
        EliError(st, ELI_ERR_BAD_ARGS, resbuf2, "ELI-PRIMITIVE [LOAD (arg is not a string)]", 0);
        return;
    }
    eliLoadFromLibrary(st, resbuf, resbuf2);
}

static int      BrokenPipeHandler()
{
    longjmp(brokenPipeEnv, 1);
}

static int      AlarmHandler()
{
    longjmp(alarmEnv, 1);
}

/*
 * The following primitive is called as:
 *  (filter [timeout] input cmd [arg1 [arg2 ... [argn]]])
 * and returns a list of the form:
 *  (ec output diag)
 *
 * The string "input" is passed as the standard input to the command named
 * by "cmd" (whose arguments are given in arg1 ... argn, which are optional).
 * Ec is the exit code of the command, output is a string containing
 * its standard output, and diag is a string containing its diagnostic output
 * If input evaluates to NIL, no stdin is passed to the child.
 */

void            Prim_FILTER(st, arglist, resbuf)
EliState_t     *st;
EliCons_t      *arglist;
EliSexp_t      *resbuf;
{
    int             i, childpid, stdinpipe[2], stdoutpipe[2], numchildcmdargs;
    int             stderrpipe[2], listlen = EliListLen(arglist), gavetimeout;
    long            timeoutsecs = (long) FILTERTIMEOUT;
    char           *childcmd, **childcmdargs = (char **) 0, *stdinstring = NULL;
    EliSexp_t      *tmpsexp, *err;
    EliCons_t      *argptr;

    if (listlen < 2) {
        if (!(err = EliSexp_GetNew(st)))
            return;
        EliSexp_SetCons(st, err, arglist);
        EliError(st, ELI_ERR_ARGLISTSIZE, err,
                 "ELI_PRIMITIVE [FILTER (checking arglist size)]", 0);
        return;
    }

    /* Check first arg; is it an integer? */
    if (!(tmpsexp = EliEval(st, EliCons_GetCar(arglist))))
        return;
    if (EliSexp_GetType(tmpsexp) == e_data_integer) {
        gavetimeout = TRUE;
        numchildcmdargs = listlen - 1;
        timeoutsecs = EliSexp_GetInt(tmpsexp);
        if (timeoutsecs < ((long) 0)) {
            EliError(st, ELI_ERR_BAD_ARGS, tmpsexp,
                 "ELI-PRIMITIVE [FILTER (invalid timeout value specified)]", 0);
            return;
        }
        if (listlen < 3) {
            if (!(err = EliSexp_GetNew(st)))
                return;
            EliSexp_SetCons(st, err, arglist);
            EliError(st, ELI_ERR_ARGLISTSIZE, err,
                     "ELI_PRIMITIVE [FILTER (checking arglist size)]", 0);
            return;
        }
    }
    else {
        gavetimeout = FALSE;
        numchildcmdargs = listlen;
        if (!EliNilP(st, tmpsexp)) {
            if (EliSexp_GetType(tmpsexp) != e_data_string) {
                EliError(st, ELI_ERR_BAD_ARGS, tmpsexp,
                "ELI-PRIMITIVE [FILTER (`stdin' arg must be NIL or string)]", 0);
                return;
            }
            stdinstring = EliStr_GetString(EliSexp_GetStr(tmpsexp));
        }
    }
    argptr = EliGetNextCell(arglist);

    if (!(childcmdargs =
          (char **) malloc(numchildcmdargs *
                           (sizeof(char *))))) {
        EliError(st, ELI_ERR_OUT_OF_MEM, (EliSexp_t *) 0,
                 "ELI-PRIMITIVE [FILTER (allocating `argv' array)]", 0);
        return;
    }

    if (gavetimeout) {
        if (!(tmpsexp = EliEval(st, EliCons_GetCar(argptr)))) {
            free(childcmdargs);
            return;
        }
        if (!EliNilP(st, tmpsexp)) {
            if (EliSexp_GetType(tmpsexp) != e_data_string) {
                EliError(st, ELI_ERR_BAD_ARGS, tmpsexp,
                "ELI-PRIMITIVE [FILTER (`stdin' arg must be NIL or string)]", 0);
                free(childcmdargs);
                return;
            }
            stdinstring = EliStr_GetString(EliSexp_GetStr(tmpsexp));
        }
        argptr = EliGetNextCell(argptr);
    }

    if (!(tmpsexp = EliEval(st, EliCons_GetCar(argptr)))) {
        free(childcmdargs);
        return;
    }
    if (EliSexp_GetType(tmpsexp) != e_data_string) {
        EliError(st, ELI_ERR_BAD_ARGS, tmpsexp,
                 "ELI-PRIMITIVE [FILTER (command arg must be a string)]", 0);
        free(childcmdargs);
        return;
    }
    childcmd = EliStr_GetString(EliSexp_GetStr(tmpsexp));

    childcmdargs[0] = childcmd;
    for (i = 1; argptr = EliGetNextCell(argptr); ++i) {
        if (!(tmpsexp = EliEval(st, EliCons_GetCar(argptr)))) {
            free(childcmdargs);
            return;
        }
        if (EliSexp_GetType(tmpsexp) != e_data_string) {
            EliError(st, ELI_ERR_BAD_ARGS, tmpsexp,
                "ELI-PRIMITIVE [FILTER (a command option is not a string)]", 0);
            free(childcmdargs);
            return;
        }
        childcmdargs[i] = EliStr_GetString(EliSexp_GetStr(tmpsexp));
    }
    childcmdargs[numchildcmdargs - 1] = NULL;

    if (pipe(stdinpipe)) {
        EliError(st, ELI_ERR_SYSERROR, (EliSexp_t *) 0,
                 "ELI-PRIMITIVE [FILTER (opening stdin pipe)]", errno);
        free(childcmdargs);
    }
    if (pipe(stdoutpipe)) {
        EliError(st, ELI_ERR_SYSERROR, (EliSexp_t *) 0,
                 "ELI-PRIMITIVE [FILTER (opening stdout pipe)]", errno);
        close(stdinpipe[0]);
        close(stdinpipe[1]);
        free(childcmdargs);
        return;
    }
    if (pipe(stderrpipe)) {
        EliError(st, ELI_ERR_SYSERROR, (EliSexp_t *) 0,
                 "ELI-PRIMITIVE [FILTER (opening stderr pipe)]", errno);
        close(stdinpipe[0]);
        close(stdinpipe[1]);
        close(stdoutpipe[0]);
        close(stdoutpipe[1]);
        free(childcmdargs);
        return;
    }

    childpid = osi_vfork();

    if (childpid > 0) {                /* In parent */
        struct timeval  timeout;
#ifdef FD_SET
        fd_set          readfdset, writefdset;
#else /* FD_SET */
        int             readfdset, writefdset;
#endif /* FD_SET */
        int             cc, remaining, rwlen;
        int             selectval, writing = (stdinstring != NULL), readingstdout = TRUE;
        int             readingstderr = TRUE, stdoutstrsize = 0, waitval;
        int             numdescriptors, stdoutstrused = 0, stderrstrsize = 0;
        int             stderrstrused = 0, (*oldpipefunc) (), (*oldalarmfunc) ();
        char           *stdinstrptr = stdinstring, *stdoutstr;
        char           *stderrstr, buffer[1 + FILTERBUFSIZ];
#if POSIX_ENV
        int             waitstat;
#else
        union wait      waitstat;
#endif
        EliSexp_t      *resultnodes[3];
        EliCons_t      *reslist = (EliCons_t *) 0;
        EliStr_t       *resultstrs[2];
        struct itimerval itimer, olditimer;

	NEWPGRP();

        close(stdinpipe[0]);
        close(stdoutpipe[1]);
        close(stderrpipe[1]);
        if (!writing)
            close(stdinpipe[1]);

        if (!(stdoutstr = malloc(FILTERBUFSIZ))) {
            EliError(st, ELI_ERR_OUT_OF_MEM, (EliSexp_t *) 0, "ELI-PRIMITIVE [FILTER (creating stdout buffer)]", 0);
            if (writing)
                close(stdinpipe[1]);
            close(stdoutpipe[0]);
            close(stderrpipe[0]);
            free(childcmdargs);
            killpg(childpid, SIGKILL);
            return;
        }
        stdoutstrsize = FILTERBUFSIZ;
        *stdoutstr = '\0';

        if (!(stderrstr = malloc(FILTERBUFSIZ))) {
            EliError(st, ELI_ERR_OUT_OF_MEM, (EliSexp_t *) 0, "ELI-PRIMITIVE [FILTER (creating stderr buffer)]", 0);
            if (writing)
                close(stdinpipe[1]);
            close(stdoutpipe[0]);
            close(stderrpipe[0]);
            free(childcmdargs);
            free(stdoutstr);
            killpg(childpid, SIGKILL);
            return;
        }
        stderrstrsize = FILTERBUFSIZ;
        *stderrstr = '\0';

        if (timeoutsecs) {
            /* Set up a SIGALRM to go off after a certain timeout */
            itimer.it_interval.tv_sec = (long) 0;
            itimer.it_interval.tv_usec = (long) 0;
            itimer.it_value.tv_sec = (long) timeoutsecs;
            itimer.it_value.tv_usec = (long) 0;
            if (setjmp(alarmEnv)) {
                EliError(st, ELI_ERR_SYSERROR, (EliSexp_t *) 0, "ELI-PRIMITIVE [FILTER (subprocess timed out)]", 0);
                if (writing)
                    close(stdinpipe[1]);
                close(stdoutpipe[0]);
                close(stderrpipe[0]);
                free(childcmdargs);
                free(stdoutstr);
                free(stderrstr);
                killpg(childpid, SIGKILL);

                /* Restore old SIGALRM action */
                setitimer(ITIMER_REAL, &olditimer, (struct itimerval *) 0);
                signal(SIGALRM, oldalarmfunc);

                /* Restore old SIGPIPE action */
                signal(SIGPIPE, oldpipefunc);

                return;
            }
            /* Bogus: should this timeout be real time or virtual time? */
            setitimer(ITIMER_REAL, &itimer, &olditimer);
            oldalarmfunc = (int (*)()) signal(SIGALRM, AlarmHandler);
            if (((int) oldalarmfunc) == -1) {
                EliError(st, ELI_ERR_SYSERROR, (EliSexp_t *) 0,
                          "ELI-PRIMITIVE [FILTER (setting up timeout)]", errno);
                if (writing)
                    close(stdinpipe[1]);
                close(stdoutpipe[0]);
                close(stderrpipe[0]);
                free(childcmdargs);
                free(stdoutstr);
                free(stderrstr);
                killpg(childpid, SIGKILL);
                return;
            }
        }

        /* Set up SIGPIPE so we don't abort on broken pipe */
        if (setjmp(brokenPipeEnv)) {   /* Handle SIGPIPE here */
            EliError(st, ELI_ERR_SYSERROR, (EliSexp_t *) 0,
            "ELI-PRIMITIVE [FILTER (subprocess unexpectedly closed stdin)]", 0);
            if (writing)
                close(stdinpipe[1]);
            close(stdoutpipe[0]);
            close(stderrpipe[0]);
            free(childcmdargs);
            free(stdoutstr);
            free(stderrstr);
            killpg(childpid, SIGKILL);

            /* Restore old SIGALRM action */
            setitimer(ITIMER_REAL, &olditimer, (struct itimerval *) 0);
            signal(SIGALRM, oldalarmfunc);

            /* Restore old SIGPIPE action */
            signal(SIGPIPE, oldpipefunc);

            return;
        }
        oldpipefunc = (int (*)()) signal(SIGPIPE, BrokenPipeHandler);
        if (((int) oldpipefunc) == -1) {
            EliError(st, ELI_ERR_SYSERROR, (EliSexp_t *) 0,
                  "ELI-PRIMITIVE [FILTER (setting up broken-pipe catcher)]", errno);
            if (writing)
                close(stdinpipe[1]);
            close(stdoutpipe[0]);
            close(stderrpipe[0]);
            free(childcmdargs);
            free(stdoutstr);
            free(stderrstr);
            killpg(childpid, SIGKILL);

            /* Restore old SIGALRM action */
            setitimer(ITIMER_REAL, &olditimer, (struct itimerval *) 0);
            signal(SIGALRM, oldalarmfunc);

            return;
        }

        while (readingstdout || readingstderr) {
            if (writing) {
#ifdef FD_SET
                FD_ZERO(&writefdset);
                FD_SET(stdinpipe[1], &writefdset);
#else /* FD_SET */
                writefdset = 1 << stdinpipe[1];
#endif /* FD_SET */
                timeout.tv_sec = (long) 0;
                timeout.tv_usec = (long) 0;
                selectval = select(stdinpipe[1] + 1, 0,
                                   &writefdset, 0,
                                   &timeout);
                if (selectval > 0) {
                    remaining = strlen(stdinstrptr);
                    rwlen = (FILTERBUFSIZ < remaining) ? FILTERBUFSIZ : remaining;
                    if (rwlen) {
                        cc = write(stdinpipe[1], stdinstrptr, rwlen);
                        if (cc < 0) {
                            EliError(st, ELI_ERR_SYSERROR, (EliSexp_t *) 0,
                                     "ELI-PRIMITIVE [FILTER (write to subprocess failed!)]", errno);
                            if (writing)
                                close(stdinpipe[1]);
                            close(stdoutpipe[0]);
                            close(stderrpipe[0]);
                            free(childcmdargs);
                            free(stdoutstr);
                            free(stderrstr);
                            killpg(childpid, SIGKILL);

                            /* Restore old SIGALRM action */
                            setitimer(ITIMER_REAL, &olditimer, (struct itimerval *) 0);
                            signal(SIGALRM, oldalarmfunc);

                            /* Restore old SIGPIPE action */
                            signal(SIGPIPE, oldpipefunc);

                            return;
                        }
                        else {
                            stdinstrptr += cc;
                        }
                    }
                    else {             /* rwlen is zero */
                        close(stdinpipe[1]);
                        writing = FALSE;
                    }
                }
                else {
                    if (selectval < 0) {
                        EliError(st, ELI_ERR_SYSERROR, (EliSexp_t *) 0,
                         "ELI-PRIMITIVE [FILTER (select on write failed!)]", errno);
                        if (writing)
                            close(stdinpipe[1]);
                        if (readingstdout)
                            close(stdoutpipe[0]);
                        if (readingstderr)
                            close(stderrpipe[0]);
                        free(childcmdargs);
                        free(stdoutstr);
                        free(stderrstr);
                        killpg(childpid, SIGKILL);

                        /* Restore old SIGALRM action */
                        setitimer(ITIMER_REAL, &olditimer, (struct itimerval *) 0);
                        signal(SIGALRM, oldalarmfunc);

                        /* Restore old SIGPIPE action */
                        signal(SIGPIPE, oldpipefunc);

                        return;
                    }
                }
            }
            if (readingstdout || readingstderr) {
                numdescriptors = -1;
#ifdef FD_SET
                FD_ZERO(&readfdset);
#else /* FD_SET */
                readfdset = 0;
#endif /* FD_SET */
                if (readingstdout) {
#ifdef FD_SET
                    FD_SET(stdoutpipe[0], &readfdset);
#else /* FD_SET */
                    readfdset = 1 << stdoutpipe[0];
#endif /* FD_SET */
                    numdescriptors = stdoutpipe[0];
                }
                if (readingstderr) {
#ifdef FD_SET
                    FD_SET(stderrpipe[0], &readfdset);
#else /* FD_SET */
                    readfdset |= 1 << stderrpipe[0];
#endif /* FD_SET */
                    if (stderrpipe[0] > numdescriptors)
                        numdescriptors = stderrpipe[0];
                }
                timeout.tv_sec = (long) 0;
                timeout.tv_usec = (long) 0;
                selectval = select(1 + numdescriptors, &readfdset,
                                   0, 0, &timeout);
                if (selectval > 0) {
#ifdef FD_SET
                    if (FD_ISSET(stdoutpipe[0], &readfdset)) {
#else /* FD_SET */
                    if (readfdset & (1 << stdoutpipe[0])) {
#endif /* FD_SET */
                        cc = read(stdoutpipe[0], buffer, FILTERBUFSIZ);
                        if (cc > 0) {
                            if ((cc + stdoutstrused + 1) > stdoutstrsize) {
                                if (!(stdoutstr = realloc(stdoutstr, stdoutstrsize + FILTERBUFSIZ + 1))) {
                                    EliError(st, ELI_ERR_OUT_OF_MEM, (EliSexp_t *) 0,
                                             "ELI-PRIMITIVE [FILTER (growing stdout buffer)]", 0);
                                    if (writing)
                                        close(stdinpipe[1]);
                                    close(stdoutpipe[0]);
                                    if (readingstderr)
                                        close(stderrpipe[0]);
                                    free(childcmdargs);
                                    free(stderrstr);
                                    killpg(childpid, SIGKILL);

                                    /* Restore old SIGALRM action */
                                    setitimer(ITIMER_REAL, &olditimer, (struct itimerval *) 0);
                                    signal(SIGALRM, oldalarmfunc);

                                    /* Restore old SIGPIPE action */
                                    signal(SIGPIPE, oldpipefunc);

                                    return;
                                }
                                stdoutstrsize += (FILTERBUFSIZ + 1);
                            }
                            bcopy(buffer, stdoutstr + stdoutstrused, cc);
                            stdoutstrused += cc;
                        }
                        else {
                            if (cc < 0) {
                                EliError(st, ELI_ERR_SYSERROR, (EliSexp_t *) 0,
                                         "ELI-PRIMITIVE [FILTER (read from subprocess' stdout failed!)]", errno);
                                if (writing)
                                    close(stdinpipe[1]);
                                close(stdoutpipe[0]);
                                if (readingstderr)
                                    close(stderrpipe[0]);
                                free(childcmdargs);
                                free(stdoutstr);
                                free(stderrstr);
                                killpg(childpid, SIGKILL);

                                /* Restore old SIGALRM action */
                                setitimer(ITIMER_REAL, &olditimer, (struct itimerval *) 0);
                                signal(SIGALRM, oldalarmfunc);

                                /* Restore old SIGPIPE action */
                                signal(SIGPIPE, oldpipefunc);

                                return;
                            }
                            else {
                                readingstdout = FALSE;
                                close(stdoutpipe[0]);
                                stdoutstr[stdoutstrused] = '\0';
                            }
                        }
                    }
#ifdef FD_SET
                    if (FD_ISSET(stderrpipe[0], &readfdset)) {
#else /* FD_SET */
                    if (readfdset & (1 << stderrpipe[0])) {
#endif /* FD_SET */
                        cc = read(stderrpipe[0], buffer, FILTERBUFSIZ);
                        if (cc > 0) {
                            if ((cc + stderrstrused + 1) > stderrstrsize) {
                                if (!(stderrstr = realloc(stderrstr, stderrstrsize + FILTERBUFSIZ + 1))) {
                                    EliError(st, ELI_ERR_OUT_OF_MEM, (EliSexp_t *) 0,
                                             "ELI-PRIMITIVE [FILTER (growing stderr buffer)]", 0);
                                    if (writing)
                                        close(stdinpipe[1]);
                                    if (readingstdout)
                                        close(stdoutpipe[0]);
                                    close(stderrpipe[0]);
                                    free(childcmdargs);
                                    free(stderrstr);
                                    killpg(childpid, SIGKILL);

                                    /* Restore old SIGALRM action */
                                    setitimer(ITIMER_REAL, &olditimer, (struct itimerval *) 0);
                                    signal(SIGALRM, oldalarmfunc);

                                    /* Restore old SIGPIPE action */
                                    signal(SIGPIPE, oldpipefunc);

                                    return;
                                }
                                stderrstrsize += (FILTERBUFSIZ + 1);
                            }
                            bcopy(buffer, stderrstr + stderrstrused, cc);
                            stderrstrused += cc;
                        }
                        else {
                            if (cc < 0) {
                                EliError(st, ELI_ERR_SYSERROR, (EliSexp_t *) 0,
                                         "ELI-PRIMITIVE [FILTER (read from subprocess' stderr failed!)]", errno);
                                if (writing)
                                    close(stdinpipe[1]);
                                if (readingstdout)
                                    close(stdoutpipe[0]);
                                close(stderrpipe[0]);
                                free(childcmdargs);
                                free(stdoutstr);
                                free(stderrstr);
                                killpg(childpid, SIGKILL);

                                /* Restore old SIGALRM action */
                                setitimer(ITIMER_REAL, &olditimer, (struct itimerval *) 0);
                                signal(SIGALRM, oldalarmfunc);

                                /* Restore old SIGPIPE action */
                                signal(SIGPIPE, oldpipefunc);

                                return;
                            }
                            else {
                                readingstderr = FALSE;
                                close(stderrpipe[0]);
                                stderrstr[stderrstrused] = '\0';
                            }
                        }
                    }
                }
                else {
                    if (selectval < 0) {
                        EliError(st, ELI_ERR_SYSERROR, (EliSexp_t *) 0,
                          "ELI-PRIMITIVE [FILTER (select on read failed!)]", errno);
                        if (writing)
                            close(stdinpipe[1]);
                        if (readingstdout)
                            close(stdoutpipe[0]);
                        if (readingstderr)
                            close(stderrpipe[0]);
                        free(childcmdargs);
                        free(stdoutstr);
                        free(stderrstr);
                        killpg(childpid, SIGKILL);

                        /* Restore old SIGALRM action */
                        setitimer(ITIMER_REAL, &olditimer, (struct itimerval *) 0);
                        signal(SIGALRM, oldalarmfunc);

                        /* Restore old SIGPIPE action */
                        signal(SIGPIPE, oldpipefunc);

                        return;
                    }
                }
            }
        }

        /* In case things aren't closed.  I think they should be by now,
         * but I also believe that it doesn't matter if you close a closed
         * descriptor.
         */
        close(stdinpipe[1]);
        close(stdoutpipe[0]);
        close(stdoutpipe[0]);

        if (timeoutsecs) {
            /* Restore old SIGALRM action */
            setitimer(ITIMER_REAL, &olditimer, (struct itimerval *) 0);
            signal(SIGALRM, oldalarmfunc);
        }

        /* Restore old SIGPIPE action */
        signal(SIGPIPE, oldpipefunc);

        /* Wait for the child process to terminate */
        while ((waitval = wait(&waitstat)) != childpid) {
            if (waitval < 0) {
                EliError(st, ELI_ERR_SYSERROR, (EliSexp_t *) 0,
                         "ELI-PRIMITIVE [FILTER (wait failed!)]", errno);
                free(childcmdargs);
                free(stdoutstr);
                free(stderrstr);
                killpg(childpid, SIGKILL);
                return;
            }
        }

        free(childcmdargs);

        /* Prepare return value */
        if (!(resultnodes[0] = EliSexp_GetNew(st))) {
            free(stdoutstr);
            free(stderrstr);
            return;
        }
        if (!(resultnodes[1] = EliSexp_GetNew(st))) {
            free(stdoutstr);
            free(stderrstr);
            return;
        }
        if (!(resultnodes[2] = EliSexp_GetNew(st))) {
            free(stdoutstr);
            free(stderrstr);
            return;
        }
        if (!(resultstrs[0] = EliStringTable_FindOrMake(st, stdoutstr))) {
            free(stdoutstr);
            free(stderrstr);
            return;
        }
        free(stdoutstr);
        if (!(resultstrs[1] = EliStringTable_FindOrMake(st, stderrstr))) {
            free(stderrstr);
            return;
        }
        free(stderrstr);
#if POSIX_ENV
        /* This isn't quite right since the 2nd arg should have core dump status added. */
        EliSexp_SetInt(st, resultnodes[0], WEXITSTATUS(waitstat));
#else
        EliSexp_SetInt(st, resultnodes[0],
                        ((int) waitstat.w_T.w_Retcode)
                        + (((int) waitstat.w_T.w_Coredump) << 8));
#endif
        EliSexp_SetStr(st, resultnodes[1], resultstrs[0]);
        EliSexp_SetStr(st, resultnodes[2], resultstrs[1]);
        if (!(reslist = EliAddToList(st, reslist, resultnodes[0])))
            return;
        if (!(reslist = EliAddToList(st, reslist, resultnodes[1])))
            return;
        if (!(reslist = EliAddToList(st, reslist, resultnodes[2])))
            return;
        EliSexp_SetCons(st, resbuf, reslist);
        return;
    }
    else {
        if (!childpid) {               /* In child */

            close(stdinpipe[1]);
            close(stdoutpipe[0]);
            close(stderrpipe[0]);

            if (dup2(stdinpipe[0], 0) == -1) {
                close(stdinpipe[0]);
                close(stdoutpipe[1]);
                close(stderrpipe[1]);
                _exit(0373);
            }
            close(stdinpipe[0]);

            if (dup2(stdoutpipe[1], 1) == -1) {
                close(stdoutpipe[1]);
                close(stderrpipe[1]);
                _exit(0374);
            }
            close(stdoutpipe[1]);

            if (dup2(stderrpipe[1], 2) == -1) {
                close(stderrpipe[1]);
                _exit(0375);
            }
            close(stderrpipe[1]);

            execvp(childcmd, childcmdargs);


            /* Bogus: do the following? */
            close(0);
            close(1);
            close(2);

            _exit(0376);               /* This is what t2open does */
        }
        else {                         /* No fork happened */
            EliError(st, ELI_ERR_SYSERROR, (EliSexp_t *) 0, "ELI-PRIMITIVE [FILTER (couldn't fork)]", errno);
            close(stdinpipe[0]);
            close(stdinpipe[1]);
            close(stdoutpipe[0]);
            close(stdoutpipe[1]);
            free(childcmdargs);
            return;
        }
    }
}

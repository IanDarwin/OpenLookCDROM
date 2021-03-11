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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/eli/lib/RCS/ecommon.c,v 2.12 1994/04/01 21:17:53 rr2b Exp $";
#endif

#include <ecommon.h>

/*
 * calls the appropriate version of decr_refcount based upon the type of the
 * thing whose refcount is being decr'd. 
 */

/*
 * NOTE: stack nodes are not considered here.  They should never be referred
 * to by anything but the stack itself.  Same goes for bucketnodes 
 */
#include <eliy.h>

void            eliDecrRefcount_SexpRef(st, node)
EliState_t     *st;
EliSexp_t      *node;
{
    if (1 > eliDecrRefcount_SexpRef_aux(st, &(node->data.datum), node->data.type))
	eliSexp_SetType(node, e_data_none);
}

int             eliDecrRefcount_SexpRef_aux(st, u, type)
EliState_t     *st;
eliSexpUnion_t *u;
eliDataTypes_t  type;
{
    int             result;

    switch (type) {
	case e_data_symbol:
	    result = eliSym_DecrRefcount(st, u->symval);
	    break;
	case e_data_string:
	    result = eliStr_DecrRefcount(st, u->strval);
	    break;
	case e_data_list:
	    result = eliCons_DecrRefcount(st, u->consval);
	    break;
	case e_data_fn:
	    result = eliFn_DecrRefcount(st, u->fnval);
	    break;
	default:
	    result = 0;
	    break;
    }
    return (result);
}

/*
 * calls the appropriate version of incr_refcount based upon the type of the
 * thing whose refcount is being incr'd. 
 */

void            eliIncrRefcount_SexpRef(node)
EliSexp_t      *node;
{
    switch (EliSexp_GetType(node)) {
	case e_data_symbol:
	    eliSym_IncrRefcount(EliSexp_GetSym(node));
	    break;
	case e_data_string:
	    eliStr_IncrRefcount(EliSexp_GetStr(node));
	    break;
	case e_data_list:
	    eliCons_IncrRefcount(EliSexp_GetCons(node));
	    break;
	case e_data_fn:
            eliFn_IncrRefcount(EliSexp_GetFn(node));
            break;
    }
}

/*
 * Given an eli source string (null-terminated), like 
 *
 * "Bob \"The Dude\" Glickstein\n" 
 *
 * returns a decoded string, like 
 *
 * Bob "The Dude" Glickstein 
 *
 * which has a literal newline at the end. The (null-terminated) result is
 * stored in a static buffer. 
 *
 * WARNING: At present, doesn't check for buffer overflow. 
 */

char           *EliParseStr(s)
char           *s;
{
    char           *p1, *p2, *buf = EliStringOpBuf(1 + eliParseStrLen(s, PARSESTRING));
    eliStringStates_t state = e_ps_begin;

    if (buf) {
	p2 = buf;
	for (p1 = s; state != e_ps_end; ++p1) {
	    switch (state) {
		case e_ps_begin:
		    if (*p1 == DQUOTE)
			state = e_ps_normal;
		    break;
		case e_ps_normal:
		    switch (*p1) {
			case DQUOTE:
			    state = e_ps_end;
			    break;
			case BACKSLASH:
			    state = e_ps_escape;
			    break;
			default:
			    *(p2++) = *p1;
			    break;
		    }
		    break;
		case e_ps_escape:
		    switch (*p1) {
			case 'b':
			    *(p2++) = '\b';
			    break;
			case 'n':
			    *(p2++) = '\n';
			    break;
			case 'r':
			    *(p2++) = '\r';
			    break;
			case 't':
			    *(p2++) = '\t';
			    break;
			default:
			    *(p2++) = *p1;
			    break;
		    }
		    state = e_ps_normal;
		    break;
	    }
	}
	*p2 = '\0';
    }
    return (buf);
}

/*
 * Given a string like 
 *
 * Bob "The Dude" Glickstein 
 *
 * which contains, say, a literal newline at the end, returns the string 
 *
 * "Bob \"The Dude\" Glickstein\n" 
 *
 * That is, restores a string to its input appearance.  Result is stored in an
 * internal static buffer, overwritten with each call. 
 */

char           *EliUnParseStr(s)
char           *s;
{
    char           *p1, *p2, *buf = EliStringOpBuf(1 + eliParseStrLen(s, UNPARSESTRING));

    if (buf) {
	p2 = buf;
	*(p2++) = DQUOTE;
	for (p1 = s; *p1; ++p1) {
	    switch (*p1) {
		case DQUOTE:
		case BACKSLASH:
		    *(p2++) = BACKSLASH;
		    *(p2++) = *p1;
		    break;
		case '\b':
		    *(p2++) = BACKSLASH;
		    *(p2++) = 'b';
		    break;
		case '\n':
		    *(p2++) = BACKSLASH;
		    *(p2++) = 'n';
		    break;
		case '\r':
		    *(p2++) = BACKSLASH;
		    *(p2++) = 'r';
		    break;
		case '\t':
		    *(p2++) = BACKSLASH;
		    *(p2++) = 't';
		    break;
		default:
		    *(p2++) = *p1;
		    break;
	    }
	}
	*(p2++) = DQUOTE;
	*p2 = '\0';
    }
    return (buf);
}

/* DESTRUCTIVE conversion of str to upper-case */

void            EliUpCaseStr(string)
char           *string;
{
    char           *p;

    for (p = string; *p; ++p)
	if (islower(*p))
	    *p = toupper(*p);
}

/*
 * Check stack for symbol whose name is "name".  If found, return it.
 * Otherwise, check symtab for it.  If it's there, return it. Otherwise,
 * return NULL. This function uses the default (global) stack and symtab. 
 */

EliSym_t       *EliFindSym(st, name)
EliState_t     *st;
char           *name;
{
    EliSym_t       *result;

    if (!(result = EliEvalStk_FindSym(st, name)))
	result = EliSymTab_Find(st, name);
    return (result);
}


/* Return the length of a list [()'s length is zero] */

int             EliListLen(l)
EliCons_t      *l;
{
    int             result = 0;
    EliCons_t      *tmp;
    EliSexp_t      *nodetmp;

    if (EliLastCellP(l)) {
	nodetmp = EliCons_GetCar(l);
	if (EliSexp_GetType(nodetmp) != e_data_none)
	    result = 1;
    }
    else {
	tmp = l;
	while (!EliLastCellP(tmp)) {
	    ++result;
	    nodetmp = EliCons_GetCdr(tmp);
	    tmp = EliSexp_GetCons(nodetmp);
	}
	++result;
    }
    return (result);
}

/*
 * Returns TRUE if node is NIL or (), FALSE otherwise. 
 */
int             EliNilP(st, node)
EliState_t     *st;
EliSexp_t      *node;
{
    int             result;
    EliSexp_t      *a, *b;

    switch (EliSexp_GetType(node)) {
	case e_data_symbol:
	    result = (EliSexp_GetSym(node) == EliNilSym(st));
	    break;
	case e_data_list:
	    a = EliCons_GetCar(EliSexp_GetCons(node));
	    b = EliCons_GetCdr(EliSexp_GetCons(node));
	    result = ((EliSexp_GetType(a) == e_data_none) && (EliSexp_GetType(b) == e_data_none));
	    break;
	default:
	    result = FALSE;
	    break;
    }
    return (result);
}

/* Tells if a cons cell is the last in a list (on its level) */

int             EliLastCellP(l)
EliCons_t      *l;
{
    EliSexp_t      *tmp = EliCons_GetCdr(l);

    return (EliSexp_GetType(tmp) == e_data_none);
}

int             eliParseStrLen(s, action)
char           *s;
int             action;
{
    char           *p1;
    eliStringStates_t state = e_ps_begin;
    int             result = 0;

    switch (action) {
	case PARSESTRING:
	    for (p1 = s; state != e_ps_end; ++p1) {
		switch (state) {
		    case e_ps_begin:
			if (*p1 == DQUOTE)
			    state = e_ps_normal;
			break;
		    case e_ps_normal:
			switch (*p1) {
			    case DQUOTE:
				state = e_ps_end;
				break;
			    case BACKSLASH:
				state = e_ps_escape;
				break;
			    default:
				++result;
				break;
			}
			break;
		    case e_ps_escape:
			++result;
			state = e_ps_normal;
			break;
		}
	    }
	    break;
	case UNPARSESTRING:
	    ++result;
	    for (p1 = s; *p1; ++p1) {
		switch (*p1) {
		    case DQUOTE:
		    case BACKSLASH:
		    case '\b':
		    case '\n':
		    case '\r':
		    case '\t':
			result += 2;
			break;
		    default:
			++result;
			break;
		}
	    }
	    ++result;
	    break;
    }
    return (result);
}

char           *EliSaveString(s)
char           *s;
{
    char           *m = malloc(1 + strlen(s));

    if (m) {
	strcpy(m, s);
    }
    return (m);
}

eliSetClientLibrary(st, var, ext, defpath)
EliState_t     *st;
char           *var, *ext, *defpath;
{
    st->ClientLibraryPreference = EliSaveString(var);
    st->DefaultClientExtension = EliSaveString(ext);
    st->DefaultClientLibraryPath = EliSaveString(defpath);
}

/* set up global vars and so on */

void            EliInit(st, scheme)
EliState_t     *st;
eliMemSchemes_t scheme;
{
    EliSexp_t      *nodetmp;
    EliFn_t        *fntmp;
    EliStr_t       *strtmp;
    static int      stateNum = 1, everCalled = FALSE;           /* To distinguish separate state vars */

    st->initializedLibraries = FALSE;

    st->numNodes = st->numFnNodes = st->numBucketNodes = st->numConsCells = st->numErrStkNodes = st->numStkNodes = st->numStrNodes = st->numSymNodes = 0;
    st->numTotalNodes = st->numTotalFnNodes = st->numTotalBucketNodes = st->numTotalConsCells = st->numTotalErrStkNodes = st->numTotalStkNodes = st->numTotalStrNodes = st->numTotalSymNodes = 0;
    st->whichScheme = scheme;
    st->myNum = stateNum++;
    st->g_errflag = FALSE;
    st->g_err = &(st->g_errbuf);
    st->g_errcatchmask = 0;
    st->g_errcatchfn = NULL;
    eliErr_Init(st->g_err);
    st->g_stk = &(st->g_stkbuf);
    st->g_symtab = &(st->g_symtabbuf);
    st->g_strtab = &(st->g_strtabbuf);
    st->g_tmptab = &(st->g_tmptabbuf);
    st->g_errstk = &(st->g_errstkbuf);
    st->g_cons_freelist = NULL;
    st->g_sym_freelist = NULL;
    st->g_str_freelist = NULL;
    st->g_bucketnode_freelist = NULL;
    st->g_Node_freelist = NULL;
    st->g_FnNode_freelist = NULL;
    st->tracep = FALSE;
    st->indentTrace = 0;
    eliEvalStk_Init(st->g_stk);
    eliHT_Init(st->g_symtab);
    eliHT_Init(st->g_strtab);
    eliHT_Init(st->g_tmptab);
    eliTraceStk_Init(st->g_errstk);

    if (!everCalled) {      /* Initialize EliProcessInfo */
	if (!(EliProcessInfo.yparsebuf = (char *) malloc(EliProcessInfo.yparsebuflen = 1))) {
	    EliError(st, ELI_ERR_OUT_OF_MEM, NULL, "INTERNAL [EliInit (allocating parse buffer)]", 0);
	    return;
	}
        EliProcessInfo.debugStuff.curDebugLevel = 0;
        EliProcessInfo.debugStuff.numEntries = 0;
        EliProcessInfo.MajorVersion = MAJORVERSION;
        EliProcessInfo.MinorVersion = MINORVERSION;
	everCalled = TRUE;
    }

    if (!(nodetmp = eliSexp_GetNew_trace(st, EliTraceStk(st))))
	return;

    strtmp = eliStringTable_Make(st, EliStringTable(st), "NIL");
    if (EliErr_ErrP(st))
	return;
    if (!(st->g_nilptr = eliSym_GetNew_trace(st, EliTraceStk(st), strtmp)))
	return;
    EliSexp_SetSym(st, nodetmp, st->g_nilptr);
    EliSym_BindSexp(st, st->g_nilptr, nodetmp);
    eliHT_Insert(st, EliSymbolTable(st), nodetmp, "NIL");	/* Not sure this is
								 * kosher. There may
								 * have to be two
								 * DIFFERENT nodes for
								 * nil and its value
								 * (also nil) */
    if (EliErr_ErrP(st))
	return;

    if (!(nodetmp = eliSexp_GetNew_trace(st, EliTraceStk(st))))
	return;

    strtmp = eliStringTable_Make(st, EliStringTable(st), "T");
    if (EliErr_ErrP(st))
	return;
    if (!(st->g_tptr = eliSym_GetNew_trace(st, EliTraceStk(st), strtmp)))
	return;
    EliSexp_SetSym(st, nodetmp, st->g_tptr);
    EliSym_BindSexp(st, st->g_tptr, nodetmp);
    eliHT_Insert(st, EliSymbolTable(st), nodetmp, "T");	/* Same thing applies
							 * here */
    if (EliErr_ErrP(st))
	return;

    if (!(nodetmp = eliSexp_GetNew_trace(st, EliTraceStk(st))))
	return;

    strtmp = eliStringTable_Make(st, EliStringTable(st), "QUOTE");
    if (EliErr_ErrP(st))
	return;
    if (!(st->g_quoteptr = eliSym_GetNew_trace(st, EliTraceStk(st), strtmp)))
	return;
    EliSexp_SetSym(st, nodetmp, EliQuoteSym(st));
    eliHT_Insert(st, EliSymbolTable(st), nodetmp, "QUOTE");
    if (EliErr_ErrP(st))
	return;

    /*
     * Defer binding of quote's fnval to the end of the routine 
     */

    if (!(nodetmp = eliSexp_GetNew_trace(st, EliTraceStk(st))))
	return;

    strtmp = eliStringTable_Make(st, EliStringTable(st), "LAMBDA");
    if (EliErr_ErrP(st))
	return;
    if (!(st->g_lambdaptr = eliSym_GetNew_trace(st, EliTraceStk(st), strtmp)))
	return;
    EliSexp_SetSym(st, nodetmp, EliLambdaSym(st));
    eliHT_Insert(st, EliSymbolTable(st), nodetmp, "LAMBDA");
    if (EliErr_ErrP(st))
	return;

    if (!(nodetmp = eliSexp_GetNew_trace(st, EliTraceStk(st))))
	return;

    strtmp = eliStringTable_Make(st, EliStringTable(st), "LAMBDAQ");
    if (EliErr_ErrP(st))
	return;
    if (!(st->g_lambdaqptr = eliSym_GetNew_trace(st, EliTraceStk(st), strtmp)))
	return;
    EliSexp_SetSym(st, nodetmp, EliLambdaqSym(st));
    eliHT_Insert(st, EliSymbolTable(st), nodetmp, "LAMBDAQ");
    if (EliErr_ErrP(st))
	return;

    if (!(nodetmp = eliSexp_GetNew_trace(st, EliTraceStk(st))))
	return;

    strtmp = eliStringTable_Make(st, EliStringTable(st), "LAMBDAV");
    if (EliErr_ErrP(st))
	return;
    if (!(st->g_lambdavptr = eliSym_GetNew_trace(st, EliTraceStk(st), strtmp)))
	return;
    EliSexp_SetSym(st, nodetmp, EliLambdaSym(st));
    eliHT_Insert(st, EliSymbolTable(st), nodetmp, "LAMBDAV");
    if (EliErr_ErrP(st))
	return;

    if (!(nodetmp = eliSexp_GetNew_trace(st, EliTraceStk(st))))
	return;

    strtmp = eliStringTable_Make(st, EliStringTable(st), "LAMBDAVQ");
    if (EliErr_ErrP(st))
	return;
    if (!(st->g_lambdavqptr = eliSym_GetNew_trace(st, EliTraceStk(st), strtmp)))
	return;
    EliSexp_SetSym(st, nodetmp, EliLambdaSym(st));
    eliHT_Insert(st, EliSymbolTable(st), nodetmp, "LAMBDAVQ");
    if (EliErr_ErrP(st))
	return;

    /* NOW SET UP THE DEFINITION OF "QUOTE" */

    nodetmp = eliSGetSexp(st, "(lambdaq (x) x)");
    if (EliErr_ErrP(st))
	return;
    if (!(fntmp = eliFn_GetNew_trace(st, EliTraceStk(st))))
	return;
    eliFn_SetCons(st, fntmp, EliSexp_GetCons(nodetmp));
    EliSym_BindFn(st, EliQuoteSym(st), fntmp);

    st->DefaultClientLibraryPath = "";
    st->DefaultClientExtension = "";
    st->ClientLibraryPreference = NULL;
    st->LibElts = NULL;
    eliPrimInit(st);
    if (EliErr_ErrP(st))
	return;			/* Ha ha HA! */
}

void            EliSetCatchMask(st, errs)
EliState_t     *st;
int             errs;
{
    st->g_errcatchmask = errs;
}

void            EliSetCatchFn(st, fn)
EliState_t     *st;
void            (*fn) ();

{
    st->g_errcatchfn = fn;
}

void            EliClearErr(st)
EliState_t     *st;
{
    if (st->g_errflag) {
	st->g_errflag = FALSE;
	if (st->g_err->badnode) {
	    eliSexp_DecrRefcount(st, st->g_err->badnode);
	    st->g_err->badnode = NULL;
        }
        st->g_err->backtrace = NULL;
    }
}

static int locallexer(lexerrock, pyylval)
struct parser *lexerrock;
void *pyylval;
{
    int res=eliyylex();
    return parser_TranslateTokenNumber(lexerrock, res);
}

EliSexp_t      *eliGetSexp(st)
EliState_t     *st;
{
    struct parser *p=eliy_New();
    if(p==NULL) return NULL;
    EliProcessInfo.u_source = e_source_stdin;
    EliProcessInfo.curglobs = st;
    parser_Parse(p, locallexer, p);
    parser_Destroy(p);
    if (EliErr_ErrP(st))
	return (NULL);
    return (EliProcessInfo.u_parseval);
}

EliSexp_t      *eliFGetSexp(st, fp)
EliState_t     *st;
FILE           *fp;
{
    struct parser *p=eliy_New();
    if(p==NULL) return NULL;
    EliProcessInfo.u_source = e_source_file;
    EliProcessInfo.u_inputfp = fp;
    EliProcessInfo.curglobs = st;
    parser_Parse(p, locallexer, p);
    parser_Destroy(p);
    if (EliErr_ErrP(st))
	return (NULL);
    return (EliProcessInfo.u_parseval);
}

EliSexp_t      *eliSGetSexp(st, s)
EliState_t     *st;
char           *s;
{
    struct parser *p=eliy_New();
    if(p==NULL) return NULL;
    EliProcessInfo.u_source = e_source_string;
    EliProcessInfo.u_sourcestring = s;
    EliProcessInfo.curglobs = st;
    parser_Parse(p, locallexer, p);
    parser_Destroy(p);
    if (EliErr_ErrP(st))
	return (NULL);
    return (EliProcessInfo.u_parseval);
}

void            EliDisplaySexp(expr)
EliSexp_t      *expr;
{
    switch (EliSexp_GetType(expr)) {
	case e_data_integer:
	    printf("%ld", EliSexp_GetInt(expr));
	    break;
	case e_data_string:
	    printf("%s", EliUnParseStr(EliStr_GetString(EliSexp_GetStr(expr))));
	    break;
	case e_data_symbol:
	    printf("%s", EliStr_GetString(EliSym_GetName(EliSexp_GetSym(expr))));
	    break;
	case e_data_list:
	    putchar('(');
	    eliDisplayCons(EliSexp_GetCons(expr));
	    putchar(')');
	    break;
	case e_data_fn:
	    switch (eliFn_GetType(EliSexp_GetFn(expr))) {
		case e_fn_compiled:
		    printf(";<Subr:#%lx>", (long) eliFn_GetCompiled(EliSexp_GetFn(expr)));
		    break;
		case e_fn_list:
		    putchar('(');
		    eliDisplayCons(eliFn_GetCons(EliSexp_GetFn(expr)));
		    putchar(')');
		    break;
		case e_fn_none:
		    printf(";<Empty_Function_Value>");
		    break;
	    }
	    break;
	case e_data_none:
	    printf(";<Empty_Data_Node>");
	    break;
    }
}

void            eliDisplayCons(expr)
EliCons_t      *expr;
{
    if (EliSexp_GetType(EliCons_GetCar(expr)) != e_data_none)
	EliDisplaySexp(EliCons_GetCar(expr));
    if (EliSexp_GetType(EliCons_GetCdr(expr)) == e_data_list) {
	putchar(' ');
	eliDisplayCons(EliSexp_GetCons(EliCons_GetCdr(expr)));
    }
}

int EliSexpEq(node1, node2)
EliSexp_t *node1, *node2;
{
    int result = 0;

    if (EliSexp_GetType(node1) != EliSexp_GetType(node2))
        result = FALSE;
    else {
        switch (EliSexp_GetType(node1)) {
            case e_data_none:
                result = TRUE;
                break;
            case e_data_integer:
                result = (EliSexp_GetInt(node1) == EliSexp_GetInt(node2));
                break;
            case e_data_string:
                result = (EliSexp_GetStr(node1) == EliSexp_GetStr(node2));
                break;
            case e_data_symbol:
                result = (EliSexp_GetSym(node1) == EliSexp_GetSym(node2));
                break;
            case e_data_list:
                result = (EliSexp_GetCons(node1) == EliSexp_GetCons(node2));
                break;
            case e_data_fn:
                result = (EliSexp_GetFn(node1) == EliSexp_GetFn(node2));
                break;
        }
    }
    return (result);
}

int             EliSexpEqual(st, node1, node2)
EliState_t     *st;
EliSexp_t      *node1, *node2;
{
    int             result = 0;
    EliCons_t *c1, *c2;
    EliFn_t *f1, *f2;

    if (EliNilP(st, node1)) {
	if (EliNilP(st, node2))
	    result = TRUE;
	else
	    result = FALSE;
    }
    else {
	if (EliSexp_GetType(node1) != EliSexp_GetType(node2))
	    result = FALSE;
	else {
	    switch (EliSexp_GetType(node1)) {
		case e_data_none:
		    result = TRUE;
		    break;
		case e_data_integer:
		    result = (EliSexp_GetInt(node1) == EliSexp_GetInt(node2));
		    break;
		case e_data_string:
		    result = !strcmp(EliStr_GetString(EliSexp_GetStr(node1)), EliStr_GetString(EliSexp_GetStr(node2)));
		    break;
		case e_data_symbol:
		    result = !strcmp(EliStr_GetString(EliSym_GetName(EliSexp_GetSym(node1))), EliStr_GetString(EliSym_GetName(EliSexp_GetSym(node2))));
		    break;
                case e_data_list:
                    c1 = EliSexp_GetCons(node1);
                    c2 = EliSexp_GetCons(node2);
		    result = (EliSexpEqual(st, EliCons_GetCar(c1), EliCons_GetCar(c2)) && EliSexpEqual(st, EliCons_GetCdr(c1), EliCons_GetCdr(c2)));
		    break;
		case e_data_fn:
                    f1 = EliSexp_GetFn(node1);
                    f2 = EliSexp_GetFn(node2);
                    if (eliFn_GetType(f1) != eliFn_GetType(f2))
                        result = FALSE;
                    else {
                        switch (eliFn_GetType(f1)) {
                            case e_fn_none:
                                result = TRUE;
                                break;
                            case e_fn_list:
                                c1 = eliFn_GetCons(f1);
                                c2 = eliFn_GetCons(f2);
                                result = (EliSexpEqual(st, EliCons_GetCar(c1), EliCons_GetCar(c2)) && EliSexpEqual(st, EliCons_GetCdr(c1), EliCons_GetCdr(c2)));
                                break;
                            case e_fn_compiled:
                                result = (eliFn_GetCompiled(f1) == eliFn_GetCompiled(f2));
                                break;
                        }
                    }
		    break;
	    }
	}
    }
    return (result);
}

/* This routine takes a list, an array of pointers to
 * nodes, and a number, max.  If the length of the list is <= max,
 * then each node pointer in the array is assigned to the car of
 * each cons cell in sequence.  If the list is longer than max,
 * the function returns -1, otherwise it returns the length of the
 * list, which is also the number of nodes assigned
 */
int             EliGetListCars(list, nodes, max)
EliCons_t      *list;
EliSexp_t      *nodes[];
int             max;

{
    int             l = EliListLen(list), i;
    EliSexp_t      *nodetmp;
    EliCons_t      *constmp;

    if (l > max)
	return (-1);
    constmp = list;
    for (i = 0; i < l; ++i) {
	nodes[i] = EliCons_GetCar(constmp);
	if (i < (l - 1)) {
	    nodetmp = EliCons_GetCdr(constmp);
	    constmp = EliSexp_GetCons(nodetmp);
	}
    }
    return (l);
}

EliCons_t      *EliGetNextCell(cell)
EliCons_t      *cell;
{
    EliSexp_t      *tmpnode;

    tmpnode = EliCons_GetCdr(cell);
    if (EliSexp_GetType(tmpnode) != e_data_list)
	return (NULL);
    return (EliSexp_GetCons(tmpnode));
}

/* This function takes a list of sexps and returns a list of their evaluated
 * forms.
 * Returns NULL if an error occurred.  Returns a NIL list iff it is passed
 * an empty argument list.
 */

EliCons_t      *EliEvalListToList(st, list)
EliState_t     *st;
EliCons_t      *list;
{
    int             l = EliListLen(list), i;
    EliCons_t      *reslist = NULL, *thiscell, *newcell, *lastcell = NULL;
    EliSexp_t      *thisnode, *evalresult, *cellHolder;

    if (l) {
	thiscell = list;
	for (i = 0; i < l; ++i) {
	    thisnode = EliCons_GetCar(thiscell);
	    if (!(evalresult = eliSexp_GetNew_trace(st, EliTraceStk(st))))
		return (NULL);
	    eliEval(st, thisnode, evalresult);
	    if (EliErr_ErrP(st))
		return (NULL);
	    if (!(newcell = eliCons_GetNew_trace(st, EliTraceStk(st))))
		return (NULL);
	    EliCons_BindCar(st, newcell, evalresult);
	    if (lastcell) {
		if (!(cellHolder = eliSexp_GetNew_trace(st, EliTraceStk(st))))
		    return (NULL);
		EliSexp_SetCons(st, cellHolder, newcell);
		EliCons_BindCdr(st, lastcell, cellHolder);
		lastcell = newcell;
	    }
	    else
		reslist = lastcell = newcell;
	    thiscell = EliGetNextCell(thiscell);
	}
    }
    else if (!(reslist = eliCons_GetNew_trace(st, EliTraceStk(st))))
	return (NULL);
    return (reslist);
}

/* This functions maintains an internal static string buffer.
 * If you need such a buffer (dynamically sized), use the result of
 * calling this function with an argument telling it how many bytes you need.
 */
char           *EliStringOpBuf(size)
int             size;
{
    static char    *buf = NULL;
    static int      buflen = 0;

    if (buf) {
	if ((size <= (buflen << 1))	/* Shrink only if halving, or better */
	    ||(size > buflen))
	    buf = (char *) realloc(buf, buflen = size);
    }
    else
	buf = (char *) malloc(buflen = size);
    return (buf);
}

eliHashTable_t *EliSymbolTable(st)
EliState_t     *st;
{
    return (st->g_symtab);
}

eliHashTable_t *EliStringTable(st)
EliState_t     *st;
{
    return (st->g_strtab);
}

eliHashTable_t *EliTempSymTable(st)
EliState_t     *st;
{
    return (st->g_tmptab);
}

eliEvalStack_t *EliEvalStack(st)
EliState_t     *st;
{
    return (st->g_stk);
}

EliSym_t       *EliNilSym(st)
EliState_t     *st;
{
    return (st->g_nilptr);
}

EliSym_t       *EliTSym(st)
EliState_t     *st;
{
    return (st->g_tptr);
}

EliSym_t       *EliQuoteSym(st)
EliState_t     *st;
{
    return (st->g_quoteptr);
}

EliSym_t       *EliLambdaSym(st)
EliState_t     *st;
{
    return (st->g_lambdaptr);
}

EliSym_t       *EliLambdaqSym(st)
EliState_t     *st;
{
    return (st->g_lambdaqptr);
}

EliSym_t       *EliLambdavSym(st)
EliState_t     *st;
{
    return (st->g_lambdavptr);
}

EliSym_t       *EliLambdavqSym(st)
EliState_t     *st;
{
    return (st->g_lambdavqptr);
}

eliErrStuff_t  *EliErrNode(st)
EliState_t     *st;
{
    return (st->g_err);
}

int             EliCatchMask(st)
EliState_t     *st;
{
    return (st->g_errcatchmask);
}

void            (*EliCatchFn(st)) ()
EliState_t     *st;
{
    return (st->g_errcatchfn);
}

eliTraceStack_t *EliTraceStk(st)
EliState_t     *st;
{
    return (st->g_errstk);
}

int             eliSexpStringLen(expr)
EliSexp_t      *expr;
{
    int             result = 0, foo;

    switch (EliSexp_GetType(expr)) {
	case e_data_integer:
	    if ((foo = EliSexp_GetInt(expr)) < 0L) {
		foo = -foo;
		result = 1 + eliNumDigits(foo, 10);
	    }
	    else
		result = eliNumDigits(foo, 10);
	    break;
	case e_data_string:
	    result = eliParseStrLen(EliStr_GetString(EliSexp_GetStr(expr)), UNPARSESTRING);
	    break;
	case e_data_symbol:
	    result = strlen(EliStr_GetString(EliSym_GetName(EliSexp_GetSym(expr))));
	    break;
	case e_data_list:
	    result = 2 + eliConsStringLen(EliSexp_GetCons(expr));
	    break;
	case e_data_fn:
	    switch (eliFn_GetType(EliSexp_GetFn(expr))) {
		case e_fn_compiled:
		    result = 9 + eliNumDigits((long) eliFn_GetCompiled(EliSexp_GetFn(expr)), 16);
		    break;
		case e_fn_list:
		    result = 2 + eliConsStringLen(eliFn_GetCons(EliSexp_GetFn(expr)));
		    break;
		case e_fn_none:
		    result = 23;
		    break;
	    }
	    break;
	case e_data_none:
	    result = 18;
	    break;
    }
    return (result);
}

int             eliConsStringLen(cell)
EliCons_t      *cell;
{
    int             result = 0;

    if (EliSexp_GetType(EliCons_GetCar(cell)) != e_data_none)
	result += eliSexpStringLen(EliCons_GetCar(cell));
    if (EliSexp_GetType(EliCons_GetCdr(cell)) == e_data_list)
	result += 1 + eliConsStringLen(EliSexp_GetCons(EliCons_GetCdr(cell)));
    return (result);
}

/* Returns the number of digits in a standard ASCII representation
 * of the number num in base base.  Num must be non-negative.
 */
int             eliNumDigits(num, base)
long            num;
int             base;
{
    int             result = 1;
    long            compare = (long) base;

    while (compare <= num) {
	++result;
	compare *= base;
    }
    return (result);
}

/* Places expr in a static buffer in human-readable form.
 * Returns the buffer, or NULL if one couldn't be allocated.
 */
char           *EliSPutSexp(expr)
EliSexp_t      *expr;
{
    int             len = eliSexpStringLen(expr);
    char           *buf = malloc(1 + len), *retBuf = NULL;

    if (buf) {
        eliSPutSexp_buf(expr, buf, len);
        retBuf = EliStringOpBuf(1 + len);
        if (retBuf)
            strcpy(retBuf, buf);
        free(buf);
    }
    return (retBuf);
}

/* This one gets a buffer passed in.  DOES NOT CHECK FOR OVERFLOW! */
void            eliSPutSexp_buf(expr, buf, len)
EliSexp_t      *expr;
char           *buf;
int             len;
{
    switch (EliSexp_GetType(expr)) {
	case e_data_integer:
	    sprintf(buf, "%ld", EliSexp_GetInt(expr));
	    break;
	case e_data_string:
	    sprintf(buf, "%s", EliUnParseStr(EliStr_GetString(EliSexp_GetStr(expr))));
	    break;
	case e_data_symbol:
	    sprintf(buf, "%s", EliStr_GetString(EliSym_GetName(EliSexp_GetSym(expr))));
	    break;
	case e_data_list:
	    buf[0] = '(';
	    eliSPutCons_buf(EliSexp_GetCons(expr), buf + 1);
	    buf[len - 1] = ')';
	    buf[len] = '\0';
	    break;
	case e_data_fn:
	    switch (eliFn_GetType(EliSexp_GetFn(expr))) {
		case e_fn_compiled:
		    sprintf(buf, ";<Subr:#%lx>", (long) eliFn_GetCompiled(EliSexp_GetFn(expr)));
		    break;
		case e_fn_list:
		    buf[0] = '(';
		    eliSPutCons_buf(eliFn_GetCons(EliSexp_GetFn(expr)), buf + 1);
		    buf[len - 1] = ')';
		    buf[len] = '\0';
		    break;
		case e_fn_none:
		    sprintf(buf, ";<Empty_Function_Value>");
		    break;
	    }
	    break;
	case e_data_none:
	    sprintf(buf, ";<Empty_Data_Node>");
	    break;
    }
}

void            eliSPutCons_buf(cell, buf)
EliCons_t      *cell;
char           *buf;
{
    int             len = 0;

    if (EliSexp_GetType(EliCons_GetCar(cell)) != e_data_none) {
	len = eliSexpStringLen(EliCons_GetCar(cell));
	eliSPutSexp_buf(EliCons_GetCar(cell), buf, len);
    }
    if (EliSexp_GetType(EliCons_GetCdr(cell)) == e_data_list) {
	buf[len] = ' ';
	eliSPutCons_buf(EliSexp_GetCons(EliCons_GetCdr(cell)), buf + len + 1);
    }
}

/* Given a list of nodes, create a cons list with each
 * node as the car of each cell
 */
EliCons_t      *EliListFromCars(st, nodes, numNodes)
EliState_t     *st;
EliSexp_t      *nodes[];
int             numNodes;

{
    EliCons_t      *listHead = NULL, *listEnd = NULL, *ptr;
    int             i;
    EliSexp_t      *nodeTmp;

    for (i = 0; i < numNodes; ++i) {
	if (!listHead) {
	    if (!(listEnd = listHead = eliCons_GetNew_trace(st, EliTraceStk(st))))
		return (NULL);
	}
	else {
	    if (!(ptr = eliCons_GetNew_trace(st, EliTraceStk(st))))
		return (NULL);
	    if (!(nodeTmp = eliSexp_GetNew_trace(st, EliTraceStk(st))))
		return (NULL);
	    EliSexp_SetCons(st, nodeTmp, ptr);
	    EliCons_BindCdr(st, listEnd, nodeTmp);
	    listEnd = ptr;
	}
	EliCons_BindCar(st, listEnd, nodes[i]);
    }
    return (listHead);
}

/* Given a list, and a node, extends the list to contain
 * the new node at the end.  Returns NULL on error.
 *
 * Also: if list is NULL to start, will create a new cons cell and return that.
 */
EliCons_t      *EliAddToList(st, list, node)
EliState_t     *st;
EliCons_t      *list;
EliSexp_t      *node;
{
    EliCons_t      *oldEnd, *newCell, *result;
    EliSexp_t      *nodeTmp;

    if (list) {
	oldEnd = EliLastCell(result = list);
	if (!(newCell = eliCons_GetNew_trace(st, EliTraceStk(st))))
	    return (NULL);
	if (!(nodeTmp = eliSexp_GetNew_trace(st, EliTraceStk(st))))
	    return (NULL);
	EliCons_BindCar(st, newCell, node);
	EliSexp_SetCons(st, nodeTmp, newCell);
	EliCons_BindCdr(st, oldEnd, nodeTmp);
    }
    else {
	if (!(result = eliCons_GetNew_trace(st, EliTraceStk(st))))
	    return (NULL);
	EliCons_BindCar(st, result, node);
    }
    return (result);
}

/* Clean interface to eliEval */
EliSexp_t      *EliEval(st, expr)
EliState_t     *st;
EliSexp_t      *expr;
{
    EliSexp_t      *result = eliSexp_GetNew_trace(st, EliTraceStk(st));

    if (result) {
	eliEval(st, expr, result);
	if (EliErr_ErrP(st))
	    return (NULL);
    }
    return (result);
}

int EliGetStateNum(st)
EliState_t     *st;
{
    return (st->myNum);
}

/* If there is an nth debug message in the queue,
 * returns it; otherwise, returns NULL.
 * Zero refers to the latest debug message
 */
char *EliDebugMessage(n)
int n;
{
    if (EliProcessInfo.debugStuff.numEntries > n)
        return (EliProcessInfo.debugStuff.entries[n].message);
    else
        return (NULL);
}

/* If there is an nth debug message in the queue,
 * returns that message's state number,
 * otherwise returns 0.
 */

int EliDebugStateNum(n)
int n;
{
    if (EliProcessInfo.debugStuff.numEntries > n)
        return (EliProcessInfo.debugStuff.entries[n].whichState);
    else
        return (-1);
}

/* If there is an nth debug message in the queue,
 * returns that message's history number,
 * otherwise returns 0.
 */

long EliDebugHistNum(n)
int n;
{
    if (EliProcessInfo.debugStuff.numEntries > n)
        return (EliProcessInfo.debugStuff.entries[n].histNum);
    else
        return (0);
}

/* Prints the latest n debug messages on file fp,
 * where n is the minimum among num, numEntries and NUM_DEBUG_ENTRIES
 * Actually prints according to the format "%s (state id = %d)\n"
 * where the %s is the message and the %d is the state num.
 * Prints them in chronological order.
 * Returns the number printed.
 */
int EliDebugFPrint(fp, num)
FILE *fp;
int num;
{
    int i, numToPrint = num;

    if (EliProcessInfo.debugStuff.numEntries < num)
        numToPrint = EliProcessInfo.debugStuff.numEntries;
    for (i = numToPrint - 1; i >= 0; --i)
        fprintf(fp, "%s (state id = %d)\n", EliProcessInfo.debugStuff.entries[i].message, EliProcessInfo.debugStuff.entries[i].whichState);
    return (numToPrint);
}

/* Given a debug message history number, prints every
 * debug message still in the queue whose history
 * number is greater than since.  Returns the history
 * number of the last debug message printed, or zero
 * if none were.
 */
long EliDebugFPrintSince(fp, since)
FILE *fp;
long since;
{
    int i, latest = 0;

    for (i = EliProcessInfo.debugStuff.numEntries - 1; i >= 0; --i)
        if (EliProcessInfo.debugStuff.entries[i].histNum > since) {
            latest = EliProcessInfo.debugStuff.entries[i].histNum;
            fprintf(fp, "%s (state id = %d)\n", EliProcessInfo.debugStuff.entries[i].message, EliProcessInfo.debugStuff.entries[i].whichState);
        }
    return (latest);
}

void EliDebug(level, msg, st, freeP)
int             level;
char           *msg;
EliState_t     *st;
int freeP;
{
    int             i, end;
    static long     histNum = 1;

    if (level <= EliProcessInfo.debugStuff.curDebugLevel) {
        if (EliProcessInfo.debugStuff.numEntries == NUM_DEBUG_ENTRIES) {
            end = NUM_DEBUG_ENTRIES - 1;
            if (EliProcessInfo.debugStuff.entries[end].freeP)
                free(EliProcessInfo.debugStuff.entries[end].message);
        }
        else
            end = EliProcessInfo.debugStuff.numEntries;
        for (i = end; i > 0 ; --i) {
            EliProcessInfo.debugStuff.entries[i].whichState =   EliProcessInfo.debugStuff.entries[i - 1].whichState;
            EliProcessInfo.debugStuff.entries[i].level = EliProcessInfo.debugStuff.entries[i - 1].level;
            EliProcessInfo.debugStuff.entries[i].message = EliProcessInfo.debugStuff.entries[i - 1].message;
            EliProcessInfo.debugStuff.entries[i].freeP = EliProcessInfo.debugStuff.entries[i - 1].freeP;
            EliProcessInfo.debugStuff.entries[i].histNum = EliProcessInfo.debugStuff.entries[i - 1].histNum;
        }
        if (EliProcessInfo.debugStuff.numEntries < NUM_DEBUG_ENTRIES)
            ++(EliProcessInfo.debugStuff.numEntries);
        if (st)     /* It is legal to pass a NULL state */
            EliProcessInfo.debugStuff.entries[0].whichState = EliGetStateNum(st);
        else
            EliProcessInfo.debugStuff.entries[0].whichState = 0;
        EliProcessInfo.debugStuff.entries[0].level = level;
        EliProcessInfo.debugStuff.entries[0].freeP = freeP;
        EliProcessInfo.debugStuff.entries[0].message = msg;
        EliProcessInfo.debugStuff.entries[0].histNum = histNum++;
    }
}

void EliVersion(maj, min)
int *maj, *min;
{
    *maj = EliProcessInfo.MajorVersion;
    *min = EliProcessInfo.MinorVersion;
}

int EliFreeAllEvalStackNodes(st)
EliState_t *st;
{
    eliEvalStack_t *s = EliEvalStack(st);

    if (s->top || !(s->stack))
        return (FALSE);
    free(s->stack);
    s->size = 0;
    s->stack = NULL;
    st->numTotalStkNodes = st->numStkNodes = 0;
    return (TRUE);
}

int EliFreeAllTraceStackNodes(st)
EliState_t *st;
{
    eliTraceStack_t *s = EliTraceStk(st);

    if (s->top || !(s->stack))
        return (FALSE);
    free(s->stack);
    s->size = 0;
    s->stack = NULL;
    st->numTotalErrStkNodes = st->numErrStkNodes = 0;
    return (TRUE);
}

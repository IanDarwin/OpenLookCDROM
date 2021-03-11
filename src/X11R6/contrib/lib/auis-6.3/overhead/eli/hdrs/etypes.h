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



enum eliSymScopes_t {		/* Symbol status (status symbol?) */
    e_sym_unknown = 0, e_sym_known, e_sym_global
};

typedef enum eliSymScopes_t eliSymScopes_t;

enum eliStringStates_t {
    e_ps_begin = 0, e_ps_normal, e_ps_escape, e_ps_end
};

typedef enum eliStringStates_t eliStringStates_t;

enum eliDataTypes_t {		/* possible data value types */
    e_data_none = 0, e_data_integer, e_data_symbol, e_data_string, e_data_list, e_data_fn
};

typedef enum eliDataTypes_t eliDataTypes_t;

enum eliFnTypes_t {		/* possible fnval types for symnodes */
    e_fn_none = 0, e_fn_list, e_fn_compiled
};

typedef enum eliFnTypes_t eliFnTypes_t;

enum eliInputSources_t {
    e_source_stdin = 0, e_source_string, e_source_file
};

typedef enum eliInputSources_t eliInputSources_t;

enum eliMemSchemes_t {
    e_mem_malloc, e_mem_pool
};

typedef enum eliMemSchemes_t eliMemSchemes_t;

enum eliObjTypes_t {
    e_types_integer = 0, e_types_string, e_types_symbol, e_types_list, e_types_Node, e_types_FnNode, e_types_bucketnode
};

typedef enum eliObjTypes_t eliObjTypes_t;

union EliStr_t {
    union EliStr_t *freelink;
    struct {
	int             refcount;
	char           *string;
    }               data;
};

typedef union EliStr_t EliStr_t;

union eliBucketNode_t {
    union eliBucketNode_t *freelink;
    struct {
	int             refcount;	/* Don't think it will ever be more
					 * than 1 */
	union eliBucketNode_t *next, *prev;
	union EliSexp_t *datum;
	union EliStr_t *key;
    }               data;
};

typedef union eliBucketNode_t eliBucketNode_t;

struct eliBucket_t {
    union eliBucketNode_t *head, *tail;
};

typedef struct eliBucket_t eliBucket_t;

union EliCons_t {
    union EliCons_t *freelink;
    struct {
	int             refcount;
	union EliSexp_t *car, *cdr;
    }               data;
};

typedef union EliCons_t EliCons_t;

struct eliHashTable_t {
    int             (*hash) ();	/* Hashing function.  Takes a single
				 * argument, a string, and returns an integer
				 * in the interval [0,NUMBUCKETS-1].  A
				 * default is provided, but the user may
				 * supply his own */
    struct eliBucket_t buckets[NUMBUCKETS];
};

typedef struct eliHashTable_t eliHashTable_t;

struct eliEvalStack_t {
    int top, size;
    union EliSym_t **stack;
};

typedef struct eliEvalStack_t eliEvalStack_t;

union EliSym_t {
    union EliSym_t *freelink;
    struct {
	enum eliSymScopes_t type;
	int             refcount;
	union EliStr_t *name;
	union EliSexp_t *val;
	union EliFn_t  *fnval;
    }               data;
};

typedef union EliSym_t EliSym_t;

union eliFnUnion_t {
    union EliCons_t *consval;
    void            (*compiled) ();
};

typedef union eliFnUnion_t eliFnUnion_t;

union EliFn_t {
    union EliFn_t  *freelink;
    struct {
	int             refcount;
	enum eliFnTypes_t type;
	union eliFnUnion_t fn;
    }               data;
};

typedef union EliFn_t EliFn_t;

union eliSexpUnion_t {
    long            intval;
    union EliStr_t *strval;
    union EliSym_t *symval;
    union EliCons_t *consval;
    union EliFn_t  *fnval;
};

typedef union eliSexpUnion_t eliSexpUnion_t;

union EliSexp_t {
    union EliSexp_t *freelink;
    struct {
	int             refcount;
	enum eliDataTypes_t type;
	union eliSexpUnion_t datum;
    }               data;
};

typedef union EliSexp_t EliSexp_t;

struct eliErrStuff_t {
    int             errnum, unixerr;
    union EliSexp_t *badnode;
    union EliCons_t *backtrace;
    char           *errloc;
};

typedef struct eliErrStuff_t eliErrStuff_t;

struct eliTraceStackNode_t {
    enum eliObjTypes_t type;
    union {
        long            intval;
        union EliStr_t *strval;
        union EliSym_t *symval;
        union EliCons_t *consval;
        union EliSexp_t *Nodeval;
        union EliFn_t  *FnNodeval;
        union eliBucketNode_t *bucketnodeval;
    }               datum;
};

typedef struct eliTraceStackNode_t eliTraceStackNode_t;

struct eliTraceStack_t {
    int top, size;
    struct eliTraceStackNode_t *stack;
};

typedef struct eliTraceStack_t eliTraceStack_t;

struct eliLibElts_t {
    char           *dir;
    char           *ext;
};

typedef struct eliLibElts_t eliLibElts_t;

struct EliState_t {

    int             initializedLibraries;

    int             myNum;
    char           *g_tmpstr, *g_sourcestring;

    union EliSexp_t *g_Node_freelist;
    union EliFn_t  *g_FnNode_freelist;
    union eliBucketNode_t *g_bucketnode_freelist;
    union EliCons_t *g_cons_freelist, *g_cons1, *g_cons2, *g_cons;
    struct eliHashTable_t *g_symtab, *g_strtab, *g_tmptab;
    struct eliHashTable_t g_symtabbuf, g_strtabbuf, g_tmptabbuf;
    struct eliEvalStack_t *g_stk;	/* This will point to its associated
					 * buf */
    struct eliEvalStack_t g_stkbuf;
    union EliStr_t *g_strnode, *g_str_freelist;
    union EliSym_t *g_nilptr, *g_quoteptr, *g_sym_freelist, *g_lambdaptr, *g_lambdaqptr, *g_lambdavptr, *g_lambdavqptr, *g_tptr;
    struct eliErrStuff_t g_errbuf, *g_err;
    int             g_errflag, g_errcatchmask;
    void            (*g_errcatchfn) ();
    struct eliTraceStack_t g_errstkbuf, *g_errstk;
    char           *DefaultClientLibraryPath;
    char           *DefaultClientExtension;
    char           *ClientLibraryPreference;
    struct eliLibElts_t *LibElts;
    enum eliMemSchemes_t whichScheme;
    int             numTotalNodes, numNodes;
    int             numTotalFnNodes, numFnNodes;
    int             numTotalBucketNodes, numBucketNodes;
    int             numTotalConsCells, numConsCells;
    int             numTotalErrStkNodes, numErrStkNodes;
    int             numTotalStkNodes, numStkNodes;
    int             numTotalStrNodes, numStrNodes;
    int             numTotalSymNodes, numSymNodes;
    int             tracep, indentTrace;
};

typedef struct EliState_t EliState_t;

struct eliDebugEntry_t {
    int             whichState;	/* Describes the particular state var active
				 * when this entry was made; 0 means this was
				 * a stateless call */
    int             level;	/* What level message is this? */
    int             freeP;	/* When this message leaves the debug queue,
				 * should free() be called on the message? */
    long            histNum;	/* Unique number among all debug entries ever */
    char           *message;	/* The message itself */
};

struct eliDebug_t {
    int             curDebugLevel, numEntries;
    struct eliDebugEntry_t entries[NUM_DEBUG_ENTRIES];
};

typedef struct eliDebug_t eliDebug_t;

struct EliProcessInfo_t {	/* There must be exactly one of these,
				 * global, in the client */
    char           *u_sourcestring, *yparsebuf;
    FILE           *u_inputfp;
    union EliSexp_t *u_parseval;
    enum eliInputSources_t u_source;
    struct EliState_t *curglobs;
    int             yparsebuflen, u_wrap, MajorVersion, MinorVersion;
    struct eliDebug_t debugStuff;
};

typedef struct EliProcessInfo_t EliProcessInfo_t;

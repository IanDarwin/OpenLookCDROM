#ifndef _MapAg_h_
#define _MapAg_h_
#include <X11/Wc/COPY>

/*
* SCCS_data: @(#) MapAg.h 1.7 92/10/12 08:29:27
*
* Mapping Agent - MapAg.h
*
* This include file describes an agent for associating arbitrary data.  In all
* cases, the data consists of char* (pointers to anything).
*
* An adaptative hashing algorithm is used: when multiple collisions occur,
* an attempt is made to modify the hashing algorithm to reduce collisions.
*
* Functional Interface:
*
*	MapAg_New()	Create a new mapping agent
*	MapAg_Free()	Destroy an existing map database.
*
*	MapAg_Define()	Set mapping of (a,b,c) to data - replaces any 
*			existing mapping of (a,b,c) to anything.
*	MapAg_Find()	Find data associated with (a,b,c).
*	MapAg_FindMap()	Find Map which associates with (a,b,c).
*	MapAg_Forget()	Forget association from (a,b,c) to anything.
*
* All but the contructor (MapAg_New()) exist as macros and as actual functions.
* The functions perform sanity checks on the agent before invoking the methods.
* Otherwise, the functions do exactly the same thing as the macros.  One should
* use the macros unless core dumps occur very near to agent method invocations.
*
* A mapping agent can also be statically declared using the MapAg_STATIC macro:
*
*	static MapAgReg fooAgentRec = MapAg_STATIC;
*	static MapAg    fooAgent    = &fooAgentRec;
*
*******************************************************************************
*/

#include <stdio.h>

#ifndef NeedFunctionPrototypes
#if defined(FUNCPROTO) || defined(__STDC__) || defined(__cplusplus) || defined(c_plusplus)
#define NeedFunctionPrototypes 1
#else
#define NeedFunctionPrototypes 0
#endif /* __STDC__ */
#endif /* NeedFunctionPrototypes */

/* Macro for ANSI or K&R external declarations.  Declare them like this:
**	int foo _(( int bar, MapAg glorp ));
*/
#ifndef _
#if NeedFunctionPrototypes
#define _(a) a		/* ANSI results in: int foo ( int bar, MapAg glorp ); */
#else
#define _(a) ()		/* K&R  results in: int foo ();			      */
#endif
#endif

#ifdef __cplusplus	/* for C++ V2.0 */
extern "C" {
#endif

/*  -- MapAg Method Type Declarations
*******************************************************************************
    Integer return values are 1 if failure, 0 if OK.
*/
typedef struct _MapAgRec* MapAg;
typedef struct _MapRec*   Map;

typedef void  (*DefineMethod)	_(( MapAg, char*, char*, char*, char* ));
typedef char* (*FindMethod)	_(( MapAg, char*, char*, char* ));
typedef Map   (*FindMapMethod)	_(( MapAg, char*, char*, char* ));
typedef void  (*ForgetMethod)	_(( MapAg, char*, char*, char* ));
typedef void  (*FreeMethod)	_(( MapAg ));
typedef void  (*ResizeMethod)	_(( MapAg ));

/*  -- Map and MapAg Object Declarations
*******************************************************************************
*/
typedef struct _MapRec	{	/* Stores one entry. */
    char* 		a;
    char*		b;
    char*		c;
    char*		data;
    struct _MapRec*	next;
} MapRec;

typedef struct _MapAgRec {	/* Stores hash table for mapping.	*/
    Map*		table;	/* Pointer to hash table of Maps.	*/
    int			mask;	/* Current size of hash table minus 1.	*/
    int			numMaps;/* Maps currently in hash table.	*/
    int			shiftA;	/* used by Hash: num insig LSBs in `a'	*/
    int			shiftB;	/* used by Hash: num insig LSBs in `b'	*/
    int			shiftC;	/* used by Hash: num insig LSBs in `c'	*/
    DefineMethod	Define;	/* Add/change the data mapped to a,b,c	*/
    FindMethod		Find;	/* find data mapped to a,b,c		*/
    FindMapMethod	FindMap;/* find Map which maps a,b,c		*/
    ForgetMethod	Forget;	/* forget any mapping of a,b,c		*/
    FreeMethod		Free;	/* Free a MapAg and all its mappings	*/
    ResizeMethod	Resize;	/* PRIVATE resize table method 		*/
} MapAgRec;

/* -- MapAg Class Methods
*******************************************************************************
*/
extern MapAg	MapAg_New();	/* allocate and initialize a new agent */
extern void	MapAg_AssertLooksOk _(( MapAg, char*, int ));
/* usage:	MapAg_AssertLooksOk( agent, __FILE__, __LINE__ );	*/

/*  -- MapAg Instance Methods
*******************************************************************************
    The following macros should be used to invoke methods of Mapping Agents.
    The assertions are enabled by defining ASSERTIONS (see assert(3)).

    If they were actually implemented in C, the functions would be:

	void  MapAg_Define  _(( MapAg, char* a, char* b, char* c, char* data ));
	char* MapAg_Find    _(( MapAg, char* a, char* b, char* c ));
	Map   MapAg_FindMap _(( MapAg, char* a, char* b, char* c ));
	void  MapAg_Forget  _(( MapAg, char* a, char* b, char* c ));
	void  MapAg_Free    _(( MapAg ));

    Note that these macros cast all arguments to char* so the client does not
    need to.  Therefore, do NOT assume that the data is anything but a pointer!

    Note that the agent argument is used twice!  Don't use side effects!!
*/

#ifndef ASSERTIONS
#define MapAg_Define(ag,a,b,c,d) \
	((ag)->Define((ag),(char*)(a),(char*)(b),(char*)(c),(char*)(d)))

#define MapAg_Find(ag,a,b,c)	\
	((ag)->Find((ag),(char*)(a),(char*)(b),(char*)(c)))

#define MapAg_FindMap(ag,a,b,c) \
	((ag)->FindMap((ag),(char*)(a),(char*)(b),(char*)(c)))

#define MapAg_Forget(ag,a,b,c)	\
	((ag)->Forget((ag),(char*)(a),(char*)(b),(char*)(c)))

#define MapAg_Free(ag)	((ag)->Free(ag))

#else
#define MapAg_Define(ag,a,b,c,d)				\
	(MapAg_AssertLooksOk((ag),__FILE__,__LINE__),		\
	(ag)->Define((ag),(char*)(a),(char*)(b),(char*)(c),(char*)(d)))

#define MapAg_Find(ag,a,b,c)					\
	(MapAg_AssertLooksOk((ag),__FILE__,__LINE__),		\
	(ag)->Find((ag),(char*)(a),(char*)(b),(char*)(c)))

#define MapAg_FindMap(ag,a,b,c)					\
	(MapAg_AssertLooksOk((ag),__FILE__,__LINE__),		\
	(ag)->FindMap((ag),(char*)(a),(char*)(b),(char*)(c)))

#define MapAg_Forget(ag,a,b,c)					\
	(MapAg_AssertLooksOk((ag),__FILE__,__LINE__),		\
	(ag)->Forget((ag),(char*)(a),(char*)(b),(char*)(c)))

#define MapAg_Free(ag)						\
	(MapAg_AssertLooksOk((ag),__FILE__,__LINE__),		\
	(ag)->Free(ag))

#endif

/*  -- Variable Method Declarations
*******************************************************************************
	**** NEVER REFERENCE ANY OF THESE FUNCTIONS!! ****
    They are only declared here so the MapAg_STATIC macro can be defined.

    All integer return values reflect failure if non-zero.
*/

void  MapAg_Define_Initial	_(( MapAg, char*, char*, char*, char* ));
void  MapAg_Define_Normal 	_(( MapAg, char*, char*, char*, char* ));

char* MapAg_Find_Initial	_(( MapAg, char*, char*, char* ));
char* MapAg_Find_Normal		_(( MapAg, char*, char*, char* ));

Map   MapAg_FindMap_Initial	_(( MapAg, char*, char*, char* ));
Map   MapAg_FindMap_Normal	_(( MapAg, char*, char*, char* ));

void  MapAg_Forget_Initial	_(( MapAg, char*, char*, char* ));
void  MapAg_Forget_Normal	_(( MapAg, char*, char*, char* ));

void  MapAg_Free_Dynamic	_(( MapAg ));
void  MapAg_Free_Static		_(( MapAg ));

void  MapAg_Resize_Initial	_(( MapAg ));
void  MapAg_Resize_Normal	_(( MapAg ));

/*  -- Macro to allow mapping agents to be statically declared
*******************************************************************************
    This macro must be coordinated with MapAg_New() so they both do
    the same type of initialization.  Static agents have a different
    free method, of course.
*/

#define MapAg_STATIC { \
    /* newAgent->table	 */ (Map*)0,			\
    /* newAgent->mask	 */ (int)0,			\
    /* newAgent->numMaps */ (int)0,			\
    /* newAgent->shiftA  */ (int)0,			\
    /* newAgent->shiftB  */ (int)0,			\
    /* newAgent->shiftC  */ (int)0,			\
    /* newAgent->Define	 */ MapAg_Define_Initial,	\
    /* newAgent->Find	 */ MapAg_Find_Initial,		\
    /* newAgent->FindMap */ MapAg_FindMap_Initial,	\
    /* newAgent->Forget	 */ MapAg_Forget_Initial,	\
    /* newAgent->Free	 */ MapAg_Free_Static,		\
    /* newAgent->Resize	 */ MapAg_Resize_Initial	\
}

#ifdef __cplusplus	/* for C++ V2.0 */
}
#endif

#endif /* _MapAg_h_ */

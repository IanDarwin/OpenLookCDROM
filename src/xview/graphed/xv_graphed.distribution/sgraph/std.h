/* (C) Universitaet Passau 1986-1991 */
/* Sgraph Source, 1988-1990 by Michael Himsolt */

#ifndef STD
#define STD

/*
#define SGRAPH_STANDALONE
*/

#ifdef SGRAPH_STANDALONE
#undef GRAPHED
#endif


#include <assert.h>
#include <stdio.h>
#ifndef MALLOC_HEADER
#define MALLOC_HEADER
#include <malloc.h>
#endif


#define	nil	NULL

#ifndef TRUE
#define TRUE	(0==0)
#endif
#ifndef FALSE
#define	FALSE	(0==1)
#endif

#define	true	TRUE
#define	false	FALSE

#ifndef iif
#define	iif(b,e1,e2)	((b) ? (e1) : (e2))
#endif
#ifndef maximum
#define	maximum(x,y)	iif ((x) > (y), (x), (y))
#endif
#ifndef minimum
#define	minimum(x,y)	iif ((x) < (y), (x), (y))
#endif

#ifndef bool
#define	bool	int
#endif
#ifndef Local
#define	Local	static
#endif
#ifndef Global
#define	Global
#endif

typedef	union	attributes {
	int	flags;
	char	*data;
}
	Attributes;

#define	attr_flags(x)		((x)->attrs.flags)
#define	attr_data(x)		((x)->attrs.data)
#define	attr_data_of_type(x,t)	((t)attr_data(x))

typedef	enum {
	ATTR_FLAGS,
	ATTR_DATA
}
	Attributes_type;

extern	Attributes	make_attr ();


extern	char	*strsave();

#ifndef GRAPHED
#ifndef SGRAPH_STANDALONE
extern	char	*mymalloc ();
extern	char	*mycalloc ();
extern	char	*myfree   ();

#define malloc(s)   mymalloc(s)
#define calloc(n,s) mycalloc((n),(s))
#define free(s)     myfree(s)

#endif
#endif

#ifdef SGRAPH_STANDALONE
#define mymalloc(s)   malloc(s)
#define mycalloc(n,s) calloc((n),(s))
#define myfree(s)     free(s)
#endif

#endif

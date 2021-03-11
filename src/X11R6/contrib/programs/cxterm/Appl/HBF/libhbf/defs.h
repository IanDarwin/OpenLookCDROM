#ifndef	DEFS_H
#define	DEFS_H

#include <stdio.h>

/*
 *	ANSI C or like ANSI C?
 */
#ifdef	__STDC__
#	define	HAS_PROTOS
#	define	HAS_CONST
#else
#	ifdef	__TURBOC__
#		define	HAS_PROTOS
#		define	HAS_CONST
#	endif
#endif

#ifdef	HAS_PROTOS
#	define	ARGS(arglist)	arglist
#else
#	define	ARGS(arglist)	()
#endif

#ifndef HAS_CONST
#	define	const
#endif

/*
 *	Other general definitions.
 */
#ifdef	EBUG
#	define	reg
#	define	local
#else
#	define	reg	register
#	define	local	static
#endif
#define	global

typedef	unsigned int	natural;
typedef	int	bool;
typedef	char	sbool;
#define	TRUE	1
#define	FALSE	0

#define	when	break; case
#define	or	: case
#define	otherwise	break; default

#define	repeat	for (;;)
#define	until(c)	if (c) break

#define	SIZE(array)	(sizeof(array)/sizeof(array[0]))

#endif /* DEFS_H */

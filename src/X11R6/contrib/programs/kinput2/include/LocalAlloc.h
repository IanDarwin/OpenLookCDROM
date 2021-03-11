/* $Id: LocalAlloc.h,v 1.2 1991/01/22 11:53:28 ishisone Rel $ */

/*
 * (fast) local allocator macro
 *
 * if you use gcc, don't worry.
 * if you use cc and have reliable alloca(), define HAVE_ALLOCA.
 */

#ifdef __GNUC__
#define LOCAL_ALLOC(x)	__builtin_alloca((unsigned int)(x))
#define LOCAL_FREE(x)
#else
#ifdef HAVE_ALLOCA
#ifdef INCLUDE_ALLOCA_H
#include <alloca.h>
#endif
#define LOCAL_ALLOC(x)	alloca((unsigned int)(x))
#define LOCAL_FREE(x)
#else
#define LOCAL_ALLOC(x)	malloc((unsigned int)(x))
#define LOCAL_FREE(x)	free((char *)(x))
#endif /* HAVE_ALLOCA */
#endif /* __GNUC__ */

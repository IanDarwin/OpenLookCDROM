#ifndef UTILS_H
#define UTILS_H

/*
 * Copyright (c) 1988, 1989, 1990, Ellen M. Sentovich and Rick L. Spickelmier.
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation.
 */

#ifndef MAX
#define MAX(a,b) (((a)>(b))?(a):(b))
#endif

#ifndef _ARGUMENTS
#if defined(FUNCPROTO) || defined(__STD__) || defined(__cplusplus) || defined(c_plusplus)
#define _ARGUMENTS(arglist) arglist
#else
#define _ARGUMENTS(arglist) ()
#endif
#endif

#ifdef SYSV
#if defined(__STDC__)
typedef void    (*SIG_PF0) (int, ...);
#else
typedef void	(*SIG_PF0) ();
#endif
#else
#if defined(__STDC__)
typedef int    (*SIG_PF0) (int, ...);
#else
typedef int	(*SIG_PF0) ();
#endif
#endif

extern char *mktemp _ARGUMENTS((char *));
extern char *strtok _ARGUMENTS((char *, const char *));
extern char *index _ARGUMENTS((const char *, int));
extern char *rindex _ARGUMENTS((const char *, int));
extern char *getenv _ARGUMENTS((const char *));

#ifdef macII
extern int strcmp();
#endif

/* This is bad, we assume that Cardinal is unsigned int */
extern char *XtMalloc _ARGUMENTS((unsigned int));
extern char *XtNewString _ARGUMENTS((char *));
extern void XtFree _ARGUMENTS((char *));

/* allocation macros */
#define ALLOC(type)           (type *) XtMalloc((unsigned) sizeof(type))
#define ARRAYALLOC(type, sz)  (type *) XtMalloc((unsigned) (sizeof(type) * (sz)))
#define NIL(type)             (type *) 0
#define FREE(item)            if ((char *) item != NIL(char)) XtFree((char *) item)
#ifdef VMS
extern int utGroupToVmsFilename(char *filename, char *group);
#endif
#define STREQ(a,b)            (strcmp(a, b) == 0)
#define STREQN(a,b,n)         (strncmp(a, b, n) == 0)

extern char *utTrimSpaces _ARGUMENTS((char *));
extern char *utTildeExpand _ARGUMENTS((char *));
extern int utSubstring _ARGUMENTS((char *, char *));
extern void utDowncase _ARGUMENTS((char *));

#define utStrlen(s)	((s) ? strlen(s) : 0)

extern int utSubjectCompare _ARGUMENTS((char *, char *));

#ifdef NEED_TEMPNAM
extern char *tempnam _ARGUMENTS((char *, char *));
#endif

#endif


/* Sigh..., unfortunately if your cpp lies all you can do is add -DUnixCPP to the imake command, or edit this file... -rr2b */

/*
 * Concat - concatenates two strings.
 */
#ifndef Concat
#if __STDC__ && !defined(UnixCpp)
#define Concat(a,b)a##b
#else
#define Concat(a,b)a/**/b
#endif
#endif

/*
 * Concat3 - concatenates three strings.
 */
#ifndef Concat3
#if __STDC__ && !defined(UnixCpp)
#define Concat3(a,b,c)a##b##c
#else
#define Concat3(a,b,c)a/**/b/**/c
#endif
#endif

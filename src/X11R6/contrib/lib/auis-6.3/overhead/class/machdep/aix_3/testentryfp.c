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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/class/machdep/aix_3/RCS/testentryfp.c,v 1.1 1993/04/06 23:15:06 susan Exp $";
#endif


#ifdef __STDC__
#define ClassEntry(n) \
extern void ClassEntry ## n (); \
void RealFunc ## n (double arg1) \
{ \
    printf("RealFunc%d called with %lf.\n", n, arg1); \
}
#else
#define ClassEntry(n) \
extern void ClassEntry/**/n (); \
void RealFunc/**/n (arg1) \
    double arg1; \
{ \
   printf("RealFunc%d called with %lf.\n", n, arg1); \
}
#endif /* __STDC__ */

#include <../common/entrydefs.h>

#undef ClassEntry

#ifdef __STDC__
#define ClassEntry(n) \
    RealFunc ## n ,
#else
#define ClassEntry(n) \
    RealFunc/**/n ,
#endif /* __STDC__ */

typedef void (*fptr)();

fptr RealFuncs[] = {
#include <../common/entrydefs.h>
};

#undef ClassEntry

#ifdef __STDC__
#define ClassEntry(n) \
    ClassEntry ## n ,
#else
#define ClassEntry(n) \
    ClassEntry/**/n ,
#endif /* __STDC__ */

fptr Funcs[] = {
#include <../common/entrydefs.h>
};

Usage()
{
    puts("testentry <index>");
    exit(1);
}

double global;

main(argc, argv)
    int argc;
    char *argv[];
{

    int index;

    if (argc != 2 || (index = atoi(argv[1])) == 0)
        Usage();

#if 1
    global = 2.0 * index;
#endif

    Funcs[index](2.0);
    exit(0);
}

void blow(double arg1)
{
}

fptr *class_Lookup(index, crud)
    int index;
    void *crud;
{
    blow(3.0);
    return RealFuncs;
}

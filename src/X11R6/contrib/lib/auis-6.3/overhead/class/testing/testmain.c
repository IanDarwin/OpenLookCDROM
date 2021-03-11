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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/class/testing/RCS/testmain.c,v 2.8 1992/12/15 21:00:33 rr2b R6tape $";
#endif

/* test main program for dynamic loader */

#include <stdio.h>

#include "class.h"
#include "testobj.ih"
#include "testobj2.ih"


int debug = 1;

int caller(victim)
int (*victim)();
{
    int i, j;

    printf("In caller.\n");
    j = 1;
    i = victim(j);
    printf("Caller returning.\n");
    return i;
}

int callee(k)
int k;
{
    int n;

    printf("in callee, k = %d.\n", k);
    n = k + 1;
    return n;
}


main()
{
    struct testobject *X;
    struct testobject2 *Y;
    int m;
    char *bogus;

    class_Init(".");
    class_SetClassPath(".");
    printf("In main, beginning.\n");
    m = caller(callee);
    printf("first call done.\n");
    bogus = (char *)callee;
    m = (*(int (*)())bogus)(2);
    printf("second call done.\n");
    X = testobject_New();
    Y = testobject2_New();
    testobject_Diddle(X, 3);
    printf("In main, ending.\n");
    printf(" ...looping....\n");
    while (1);

}


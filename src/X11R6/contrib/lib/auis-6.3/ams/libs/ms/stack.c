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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/ams/libs/ms/RCS/stack.c,v 2.7 1992/12/15 21:21:37 rr2b R6tape $";
#endif

/*	stack.c -- push and pop on an otherwise private stack.
	You pass in pointers, and the stack routines do appropriate mallocs and frees
	to store *duplicates* of the strings you pass.   Pop returns a pointer to
	a static area overwritten with each call. */

#include <ms.h>
#include <ctype.h>

#define STACKSIZE 1000
#define MAXSTACKS 2
		/* MAXSTACKS is the number of stacks -- 2 stacks, stack 0 and 1 */
#ifndef _IBMR2
extern char *malloc(), *realloc();
#endif /* _IBMR2 */

static char *Stack[MAXSTACKS][STACKSIZE + 1];
static int StackTop[MAXSTACKS];
static int DoMapping = 1;
static int FavorHashes = 0;

Stack_MapHashes() {
    DoMapping = 1;
    FavorHashes = 0;
}
Stack_MapPluses() {
    DoMapping = 1;
    FavorHashes = 1;
}
Stack_MapNoChars() {
    DoMapping = 0;
}

StackSize(which)
int which;
{
    debug(1, ("StackSize %d\n", which));
    return(StackTop[which]);
}

clearstack(which) 
int which;
{
    int i;

    debug(1, ("clearstack %d\n", which));
    for (i=0; i<StackTop[which]; ++i) {
	free(Stack[which][i]);
    }
    StackTop[which] = 0;
}



push(string, maplc, which)
char *string;
Boolean maplc;
int which;
{
    char *s;

    debug(1, ("push %s %d %d\n", string, maplc, which));
    if (StackTop[which] >= STACKSIZE) {
	debug(16, ("Push failed\n"));
	return(-1);
    } 
    Stack[which][StackTop[which]] = malloc(strlen(string)+1);
    strcpy(Stack[which][StackTop[which]], string);
    if (maplc) {
	for(s=Stack[which][StackTop[which]]; *s; ++s) {
	    if (isupper(*s)) *s = tolower(*s);
	}
    }
    Stack_MapHashPlusAsAppropriate(Stack[which][StackTop[which]]);
    ++StackTop[which];
    debug(16, ("Pushed %s\n", string));
    return(0);
}

static char *LatestPop[MAXSTACKS];
static int NeedToInitStacks = 1;

char *
pop(which) 
int which;
{

    debug(1, ("pop %d\n", which));
    if (StackTop[which] <= 0) {
	debug(16, ("pop failed\n"));
	return((char *) -1);
    }
    --StackTop[which];
    if (NeedToInitStacks) {
	int i;

	for (i=0; i<MAXSTACKS; ++i) {
	    /* allocate them all, initial size as given by current pop */
	    LatestPop[i] = malloc(1+strlen(Stack[which][StackTop[which]]));
	    if (!LatestPop[which]) return((char *) -1);
	}
	NeedToInitStacks = 0;
    } else {
	LatestPop[which] = realloc(LatestPop[which], 1+strlen(Stack[which][StackTop[which]]));
    }
    strcpy(LatestPop[which], Stack[which][StackTop[which]]);
    free(Stack[which][StackTop[which]]);
    debug(16, ("Popped %s\n", LatestPop[which]));
    return (LatestPop[which]);
}

StackTopSize(which)
int which;
{
    if (StackTop[which] <= 0) {
	return(0);
    }
    return(1+strlen(Stack[which][StackTop[which]-1]));
}
    

PushNonEmptiness(which)
int which;
{
    int st;

    debug(1, ("pushnonemptiness %d\n", which));
    st = StackTop[which];
    if (st <= 0 || Stack[which][st-1][0] == '\0') {
	return(push("0", FALSE, which));
    } else {
	return(push("1", FALSE, which));
    }
}

Stack_MapHashPlusAsAppropriate(s)
char *s;
{
    if (s && DoMapping) {
	if (FavorHashes) {
	    for( ; *s; ++s) {
		if (*s == '+') *s = '#';
	    }
	} else {
	    for( ; *s; ++s) {
		if (*s == '#') *s = '+';
	    }
	}
    }
}

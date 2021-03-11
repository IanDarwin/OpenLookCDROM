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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/ams/libs/ms/RCS/fixsub.c,v 2.8 1993/09/21 21:54:48 gk5g Exp $";
#endif

#include <andrewos.h>
#include <ms.h>

struct {
    char *left, *right;
} SubjectProductions[] = {
    "re: re:", "Re:",
    "fwd: fwd:", "Fwd:",
    "re: fwd:", "Re:",
    "fwd: re:", "Fwd:",
    0, 0};

char *
RewriteSubject(oldsub)
char *oldsub;
{
    char *newsub, *tempsub;
    Boolean DidSomething = TRUE;
    int i;

    newsub = malloc(1+strlen(oldsub));
    if (!newsub) return(NULL);
    strcpy(newsub, StripWhiteEnds(oldsub));
    ReduceWhiteSpace(newsub);
    while (DidSomething) {
	DidSomething = FALSE;
	for (i=0; SubjectProductions[i].left; ++i) {
	    if (!lc2strncmp(SubjectProductions[i].left, newsub, strlen(SubjectProductions[i].left))) {
		DidSomething = TRUE;
		tempsub = malloc(1+strlen(newsub));
		if (!tempsub) {
		    free(newsub);
		    return(NULL);
		}
		sprintf(tempsub, "%s%s", SubjectProductions[i].right, newsub+strlen(SubjectProductions[i].left));
		free(newsub);
		newsub = tempsub;
		break;
	    }
	}
    }
    return(newsub);
}

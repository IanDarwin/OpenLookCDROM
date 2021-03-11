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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/ams/msclients/nns/RCS/mcb.c,v 2.6 1992/12/15 21:22:34 rr2b R6tape $";
#endif

/* Methods for Message Cache Buckets
*/

/* BOGUS: include relevant AMS header(s) here */

#include <big.h>

void            MCBInit(mcb)
MCacheBucket_t *mcb;
{
    *mcb = NULL;
}

struct MS_Message *MCBFind(mcb, string)
MCacheBucket_t *mcb;
char           *string;
{
    MCacheBucketEntry_t *mcbe = *mcb;
    struct MS_Message *result = NULL;

    while (mcbe && !result) {
	if (strcmp(MCBEGetFilename(mcbe), string))
	    mcbe = MCBEGetNext(mcbe);
	else
	    result = MCBEGetMsg(mcbe);
    }
    return (result);
}

int             MCBMake(mcb, string, Msg)
MCacheBucket_t *mcb;
char           *string;
struct MS_Message *Msg;
{
    MCacheBucketEntry_t *tmp = (MCacheBucketEntry_t *) malloc(sizeof(MCacheBucketEntry_t));

    if (tmp) {
	MCBESet(tmp, string, Msg, *mcb);
	*mcb = tmp;
	return (TRUE);
    }
    return (FALSE);
}

void            MCBDelete(mcb, string)
MCacheBucket_t *mcb;
char           *string;
{
    MCacheBucketEntry_t *mcbe = *mcb, *prevmcbe = NULL;

    while (mcbe) {
	if (strcmp(MCBEGetFilename(mcbe), string)) {
	    prevmcbe = mcbe;
	    mcbe = MCBEGetNext(mcbe);
	}
	else {
	    if (prevmcbe)
		MCBESetNext(prevmcbe, MCBEGetNext(mcbe));
	    else
		*mcb = MCBEGetNext(mcbe);
	    free(mcbe);
	    mcbe = NULL;
	}
    }
}

void            MCBPurge(mcb)
MCacheBucket_t *mcb;
{
    MCacheBucketEntry_t *mcbe = *mcb, *next;

    while (mcbe) {
	next = MCBEGetNext(mcbe);
	free(mcbe);
	mcbe = next;
    }
    *mcb = NULL;
}

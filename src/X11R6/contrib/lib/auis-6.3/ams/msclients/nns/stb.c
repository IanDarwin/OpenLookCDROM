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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/ams/msclients/nns/RCS/stb.c,v 2.7 1992/12/15 21:22:34 rr2b R6tape $";
#endif

/* Methods for string table buckets
*/

#include <big.h>

static int STBGrowIfNecessary();

void            STBInit(stb)
STableBucket_t *stb;
{
    stb->num = stb->size = 0;
    stb->entries = NULL;
}

char           *STBFind(stb, string)
STableBucket_t *stb;
char           *string;
{
    int             i;
    char           *result = NULL;

    for (i = 0; (i < stb->num) && (!result); ++i) {
	if (!strcmp(string, STBEGetString(&(stb->entries[i]))))
	    result = STBEGetString(&(stb->entries[i]));
    }
    return (result);
}

char           *STBMake(stb, string)
STableBucket_t *stb;
char           *string;
{
    char           *tmp = NULL;

    if (STBGrowIfNecessary(stb)) {
	if (tmp = (char *) malloc(strlen(string) + 1)) {
	    strcpy(tmp, string);
	    STBESetString(&(stb->entries[(stb->num)++]), tmp);
	}
    }
    return (tmp);
}

void            STBPurge(stb)
STableBucket_t *stb;
{
    int             i;

    for (i = 0; i < stb->num; ++i)
	free(STBEGetString(&(stb->entries[i])));
    free(stb->entries);
    stb->num = stb->size = 0;
    stb->entries = NULL;
}

static int      STBGrowIfNecessary(stb)
STableBucket_t *stb;
{
    STableBucketEntry_t *tmp;

    if (stb->num == stb->size) {
	if (stb->size) {
	    if (!(tmp = (STableBucketEntry_t *) realloc(stb->entries, (stb->size + STB_GROWSIZE) * sizeof(STableBucketEntry_t))))
		return (FALSE);
	}
	else {
	    if (!(tmp = (STableBucketEntry_t *) malloc(STB_GROWSIZE * sizeof(STableBucketEntry_t))))
		return (FALSE);
	}
	stb->entries = tmp;
	stb->size += STB_GROWSIZE;
    }
    return (TRUE);
}

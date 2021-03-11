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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/ams/msclients/nns/RCS/mcbe.c,v 2.6 1992/12/15 21:22:34 rr2b R6tape $";
#endif

/* Methods for Message Cache Bucket Entries
*/

#include <big.h>

char           *MCBEGetFilename(mcbe)
MCacheBucketEntry_t *mcbe;
{
    return (mcbe->filename);
}

MCacheBucketEntry_t *MCBEGetNext(mcbe)
MCacheBucketEntry_t *mcbe;
{
    return (mcbe->next);
}

struct MS_Message *MCBEGetMsg(mcbe)
MCacheBucketEntry_t *mcbe;
{
    return (mcbe->Msg);
}

void            MCBESet(mcbe, filename, Msg, next)
MCacheBucketEntry_t *mcbe;
char           *filename;
struct MS_Message *Msg;
MCacheBucketEntry_t *next;
{
    mcbe->filename = filename;
    mcbe->Msg = Msg;
    mcbe->next = next;
}

void            MCBESetNext(mcbe, next)
MCacheBucketEntry_t *mcbe, *next;
{
    mcbe->next = next;
}

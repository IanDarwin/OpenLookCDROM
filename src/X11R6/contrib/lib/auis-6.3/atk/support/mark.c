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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/support/RCS/mark.c,v 2.6 1992/12/15 21:41:42 rr2b R6tape $";
#endif


 

#include <class.h>
#include <mark.eh>

/* Crank out marks in 4k blocks. */
#define DESIREDBLOCKSIZE 4096
/* Number of marks per block */
#define NUMPERBLOCK DESIREDBLOCKSIZE / sizeof(struct mark)
#define BLOCKSIZE NUMPERBLOCK * sizeof(struct mark)

static struct mark *freeMarks = NULL;
static struct mark *lastBlock = NULL;

struct mark *mark__Allocate(classID)
struct classheader *classID;
{

    static int lastIndex = NUMPERBLOCK; /* Force a block malloc on first call. */

    if (freeMarks) {
        struct mark *tempMark = freeMarks;
        freeMarks = freeMarks->next;
        return tempMark;
    }
    if (lastIndex >= NUMPERBLOCK) {
        lastBlock = (struct mark *) malloc(BLOCKSIZE);
        lastIndex = 0;
    }
    return &lastBlock[lastIndex++];
}

void mark__Deallocate(classID, self)
struct classheader *classID;
    struct mark *self;
{

    self->next = freeMarks;
    freeMarks = self;
}

boolean mark__InitializeObject(classID, self)
struct classheader *classID;
struct mark *self;  {
    self->next = NULL;
    self->pos = 0;
    self->length = 0;
    self->modified = FALSE;
    self->objectFree = FALSE;
    self->includeBeginning = FALSE;
    self->includeEnding = TRUE;

    return TRUE;
}

void mark__SetStyle(self, beginning, ending)
struct mark *self;
boolean beginning;
boolean ending;  {
    self->includeBeginning = beginning;
    self->includeEnding = ending;
}

struct mark *mark__NewWithStyle(classID, beginning, ending)
struct classheader *classID;
boolean beginning;
boolean ending;  {
    struct mark *nmark;

    nmark = mark_New();
    nmark->includeBeginning = beginning;
    nmark->includeEnding = ending;
    return nmark;
}

void mark__UpdateMarks(self, pos, size)
    struct mark *self;
    long pos;
    long size;
{

    struct mark *mark;
    long tpos;
    long tsize;
    long endPos;

    if (size == 0) return;
    
    for (mark = self; mark != NULL; mark = mark_GetNext(mark))  {
	tpos = pos;
	tsize = size;
	if (tpos <= (endPos = mark_GetEndPos(mark)))  {
	    if (tsize > 0)  {
		if (tpos == endPos)  {
		    if (mark_IncludeEnding(mark))  {
			mark_SetModified(mark, TRUE);
			mark_SetLength(mark, mark_GetLength(mark) + tsize);
		    }
		}
		else if (tpos < mark_GetPos(mark) || tpos == mark_GetPos(mark) && ! mark_IncludeBeginning(mark))
		    mark_SetPos(mark, mark_GetPos(mark) + tsize);
		else {
		    mark_SetLength(mark, mark_GetLength(mark) + tsize);
		    mark_SetModified(mark, TRUE);
		}
	    }
	    else if (tpos < endPos) {
		if (tpos-tsize <= mark_GetPos(mark))  {
/* 		    Deleted region is before the mark
 */		    
		    mark_SetPos(mark, mark_GetPos(mark) + tsize);
		}
		else  {
		    if (tpos <= mark_GetPos(mark))  {
/* 			Delete portion before the mark
 */			
		    tsize += mark_GetPos(mark) - tpos;
			mark_SetPos(mark, tpos);
		    }
		    
/* 		    Reset the size of the deleted region to only include the mark
 */		    
		    if (tpos - tsize > endPos)
			tsize = tpos - endPos;
			
/* 		    Delete the characters from the mark
 */		    
		    mark_SetLength(mark, mark_GetLength(mark)+ tsize);
		    if (mark_GetLength(mark) < 0) mark_SetLength(mark, 0);
		    mark_SetModified(mark, TRUE);
		}
	    }
	}
    }
}

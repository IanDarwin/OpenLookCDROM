/* $XConsortium: CribBoardP.h,v 1.6 91/03/13 20:12:07 rws Exp $ */

/* Copyright	Massachusetts Institute of Technology	1987, 1988
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of M.I.T. not be used in advertising or
 * publicity pertaining to distribution of the software without specific,
 * written prior permission.  M.I.T. makes no representations about the
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * M.I.T. DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL M.I.T.
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION
 * OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 */

#ifndef _CribBoardP_h
#define _CribBoardP_h

#include "CribBoard.h"
/* include superclass private header file */
#include <X11/Xaw/SimpleP.h>

#define New(t) (t *) malloc(sizeof (t))
#define Dispose(p)  free((char *) p)
#define Some(t,n)   (t*) malloc(sizeof(t) * n)
#define More(p,t,n) ((p)? (t *) realloc((char *) p, sizeof(t)*n):Some(t,n)

typedef struct {
    int empty;
} CribBoardClassPart;

typedef struct _CribBoardClassRec {
    CoreClassPart	core_class;
    SimpleClassPart	simple_class;
    CribBoardClassPart	cribBoard_class;
} CribBoardClassRec;

extern CribBoardClassRec cribBoardClassRec;

typedef struct {
    /* resources */
    unsigned long   pegColor;
    unsigned long   holeColor;
    int		    pegSize;
    int		    holeSize;
    int		    numPegs;
    int		    groupSpace;
    int		    rowSpace;
    int		    numCols;
    int		    numRows;
    /* private state */
    int		    *pegs;
    GC		    pegGC;
    GC		    holeGC;
} CribBoardPart;

typedef struct _CribBoardRec {
    CorePart		core;
    SimplePart		simple;
    CribBoardPart	cribBoard;
} CribBoardRec;

#endif /* _CribBoardP_h */

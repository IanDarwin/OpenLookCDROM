#ifndef MXDIFF_H
#define MXDIFF_H

#ifndef lint
static char rcsid_mgdiff_h[] = "mgdiff.h,v 2.0 1994/05/19 02:01:15 dan Exp";
#endif

/*
 * Copyright (c) 1994    Daniel Williams
 * 
 * The X Consortium, and any party obtaining a copy of these files from
 * the X Consortium, directly or indirectly, is granted, free of charge,
 * a full and unrestricted irrevocable, world-wide, paid up,
 * royalty-free, nonexclusive right and license to deal in this software
 * and documentation files (the "Software"), including without limitation
 * the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons
 * who receive copies from any such party to do so.  This license
 * includes without limitation a license to do the foregoing actions
 * under any patents of the party supplying this software to the X
 * Consortium.  The following conditions apply:
 * 
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL DANIEL WILLIAMS OR SYSTEMS & SCIENTIFIC SOFTWARE BE
 * LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
 * OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
 * WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

/* 
 * these are the types of each chunk as identified by the difference 
 * algorithm and map to different colors for display purposes.
 */
typedef enum {DIFF = 0, SAME, INSERT, BLANK} ChunkType;

/* 
 * each file compared is broken up into a list of chunks, which are 
 * the smallest set of contiguous lines identified by the difference 
 * algorithm.
 */
typedef struct {
    ChunkType type;		/* what kind of chunk is this? */
    int fline;			/* file address (linenumber) */
    int fsize;			/* number of lines in file */
    char **text;		/* the text to display */
    char **wtext;		/* the text to write */
    short *tlen;		/* the lengths of each line */
} Chunk;

typedef enum {LEFT = 0, RIGHT, NEITHER} Side;

/* 
 * a block is an element of a doubly linked list containing a left chunk 
 * and a right chunk
 */
typedef struct block {
    struct block *prev, *next;
    Side selected;		/* which chunk of the Block is selected */
    int sline;			/* screen address (linenumber) */
    int ssize;			/* number of lines on screen */
    Chunk arr[2];		/* left and right Chunks */
} Block;

/* 
 * the results of a file comparison
 */
typedef struct {
    int status;			/* return value from diff */
    char **etext;		/* any error text from diff */
    int errors;			/* number of lines of error text */
    int lines;			/* total number of screen lines */
    int maxcols;		/* maximum length of any text line */
    char *longline;		/* longest string of characters to be rendered */
    int flines[2];		/* total number of file lines for each file */
    Block *first;		/* the first block in the block list */
    Block *last;		/* the last block in the block list */
} DiffInfo;

/* 
 * True if we are compiling under Release 5 or above of the Intrinsics
 */
#define X11R5 (defined(XtSpecificationRelease) && (XtSpecificationRelease >= 5))

#endif

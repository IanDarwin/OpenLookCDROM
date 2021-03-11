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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/ams/libs/ms/RCS/freedir.c,v 2.6 1992/12/15 21:19:11 rr2b R6tape $";
#endif

#include <ms.h>

/* This routine frees a directory and the things it points to.  If the
	directory is in the cache, it should be removed from the cache
	BEFORE this routine is called, or you'll regret it. */

/* This routine is not currently used, because nobody ever frees a directory
any more -- they are in a permanent cache, and freeing them will cause a
core dump.  However, this documents what they allocate, and might be useful
in future versions. */

FreeDirectory(Dir)
struct MS_Directory *Dir;
{
    debug(1, ("FreeDirectory\n"));
    if (Dir) {
	int i;

	if (Dir->UNIXDir) free (Dir->UNIXDir);
	if (Dir->AttrCount && Dir->AttrNames) {
	    for (i=0; i<Dir->AttrCount; ++i) {
		free(Dir->AttrNames[i]);
	    }
	    free(Dir->AttrNames);
	}
	free(Dir);
    }
}

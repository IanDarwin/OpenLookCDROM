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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/ams/libs/ms/RCS/getnmct.c,v 2.9 1992/12/15 21:19:11 rr2b R6tape $";
#endif

#include <ms.h>
#include <andrewos.h>
#include <sys/stat.h>

MS_GetNewMessageCount(FullDirName, numnew, numtotal, LastOldDate, InsistOnFetch)
char *FullDirName, *LastOldDate;
int *numnew, *numtotal, InsistOnFetch;
{
    struct MS_Directory *Dir = NULL;
    struct stat statbuf;
    char AssTime[AMS_DATESIZE+1];
    char SnapshotDum[AMS_SNAPSHOTSIZE];
    int comp, errsave, top, bottom, split;
    long lastdate;

    debug(1, ("In MS_GetNewMessageCount with dirname %s\n", FullDirName));
    *numtotal = *numnew = -1;
    *LastOldDate = '\0';
    if (MS_GetAssociatedTime(FullDirName, AssTime, AMS_DATESIZE)) {
	if (vdown(AMS_ERRNO)) return(mserrcode);
	/* If vice is down on my profile, forget short cuts */
    }
    if (MS_GetAssociatedFileTime(FullDirName, &lastdate) != 0) {
	return(mserrcode);
    }
    if (!InsistOnFetch) {
	if (stat(FullDirName, &statbuf) != 0) { 
	    AMS_RETURN_ERRCODE(errno, EIN_STAT, EVIA_HEADERSSINCE);
	}
	if (statbuf.st_mtime <= lastdate) {
	    debug(256, ("Short cut -- we don't have new notices %d <= %d\n", statbuf.st_mtime, lastdate));
	    *numnew = 0;
	    return(0);
	}
	debug(4, ("Do it the long way, mtime %d AssTime %d\n", statbuf.st_mtime, lastdate));
    }
    if (ReadOrFindMSDir(FullDirName, &Dir, MD_READ) != 0) {
	return(mserrcode);
    }
    *numtotal = Dir->MessageCount;
    if (*numtotal <= 0) {
	debug(256, ("Empty message directory, let's say we read it\n"));
	CloseMSDir(Dir, MD_READ); /* ignore errors -- read only */
	*numnew = 0;
	return(0);
    }
    if (AssTime[0] == '\0' || (conv64tolong(AssTime) == 0)) { /* First time ever */
	*numnew = (*numtotal > 10) ? 10 : *numtotal;
	if (GetSnapshotByNumber(Dir, *numtotal - *numnew, SnapshotDum)) {
	    errsave = mserrcode;
	    CloseMSDir(Dir, MD_READ);
	    return(errsave);
	}
	strcpy(LastOldDate, AMS_DATE(SnapshotDum));
	CacheDirectoryForClosing(Dir, MD_READ);
	return(0);
    }
    top = *numtotal;
    bottom = 0;
    split = *numtotal / 2;
    while (split < *numtotal) { 
	/* break when split is set right */
	if (GetSnapshotByNumber(Dir, split, SnapshotDum)) {
	    errsave = mserrcode;
	    CloseMSDir(Dir, MD_READ);
	    return(errsave);
	}
	comp = strcmp(AssTime, AMS_DATE(SnapshotDum));
	if (comp>0) {
	    if (top == split) {
		split++;
		break; 
	    }
	    if ((top - split) == 1) {
		bottom = split + 1;
		++split;
	    } else {
		bottom = split + 1;
		split += (top - split) / 2;
	    }
	} else if (comp < 0) {
	    if (split == bottom) break;
	    if ((split - bottom) == 1) {
		top = split - 1;
		--split;
	    } else {
		top = split - 1;
		split -= (split - bottom) / 2;
	    }
	} else {
	    ++split; /* We do not want the exact matching message */
	    break;
	}
    }
    if (split >= *numtotal) {
	*numnew = 0;
	split = *numtotal; /* Could be one greater, fencepost error for getsnapshotbynumber below */
    } else {
	*numnew = *numtotal - split;
    }
    if (split > 0) {
	if (GetSnapshotByNumber(Dir, split - 1, SnapshotDum)) {
	    errsave = mserrcode;
	    CloseMSDir(Dir, MD_READ);
	    return(errsave);
	}
	strcpy(LastOldDate, AMS_DATE(SnapshotDum));
    }
    CacheDirectoryForClosing(Dir, MD_READ);
    return(0);
}

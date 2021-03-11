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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/ams/libs/ms/RCS/headers.c,v 2.18 1993/08/25 20:35:56 susan Exp $";
#endif

#include <andrewos.h> /* sys/file.h */
#include <ms.h>
#include <sys/stat.h> 

extern char *fixDate();

/* The following implements the very simplest form of header retrieval.  It was written
	initially to postpone the writing of a full message-arithmetic parsing 
	language, but will be useful even when such a language is written, as it
	will provide a very quick way to do simple things with minimal parsing.
*/

MS_HeadersSince(FullDirName, datefield, ReturnBuf, MaxReturn, startbyte, numbytes, bytesleft)
char *FullDirName, *datefield, *ReturnBuf;
int MaxReturn, startbyte, *numbytes, *bytesleft;
{
    struct MS_Directory *Dir = NULL;
    struct stat statbuf;
    char AssTime[10];
    char SnapshotDum[AMS_SNAPSHOTSIZE];
    int comp, errsave;
    long lastdate;

    debug(1, ("In MS_HeadersSince with dirname %s\n", FullDirName));
    AssTime[0] = '\0';
    *numbytes = 0;
    *bytesleft = 0;
    if (datefield != NULL && *datefield != '\0') {
	if (!MS_GetAssociatedTime(FullDirName, AssTime, 10)) {
	    if (!strcmp(AssTime, datefield)) {
		if (MS_GetAssociatedFileTime(FullDirName, &lastdate) != 0) {
		    return(mserrcode);
		}
		if (stat(FullDirName, &statbuf) != 0) { 
		    AMS_RETURN_ERRCODE(errno, EIN_STAT, EVIA_HEADERSSINCE);
		}
		if (statbuf.st_mtime <= lastdate) {
		    debug(256, ("Short cut -- we don't have new notices %d <= %d\n", statbuf.st_mtime, lastdate));
		    return(0);
		}
	    }
	} else {
	    if (vdown(AMS_ERRNO)) return(mserrcode);
	    /* If vice is down on my profile, forget short cuts */
	}

    }
    debug(4, ("Do it the long way, mtime %d datefield %d\n", statbuf.st_mtime, lastdate));
    if (ReadOrFindMSDir(FullDirName, &Dir, MD_READ) != 0) {
	return(mserrcode);
    }
    if (Dir->MessageCount == 0) {
	debug(256, ("Empty message directory, let's say we read it\n"));
	CloseMSDir(Dir, MD_READ); /* ignore errors -- read only */
	if (AssTime[0] && strcmp(AssTime, "000000") && MS_SetAssociatedTime(FullDirName, "000000")) {
	    return(mserrcode);
	}
	return(0);
    }
    if (datefield != NULL && *datefield != '\0') {
	int top = Dir->MessageCount, 
	    bottom = 0,
	    split = Dir->MessageCount / 2;

	while (split < Dir->MessageCount) { 
	    /* break when split is set right */
	    if (GetSnapshotByNumber(Dir, split, SnapshotDum)) {
		errsave = mserrcode;
		CloseMSDir(Dir, MD_READ);
		return(errsave);
	    }
	    comp = strcmp(datefield, AMS_DATE(SnapshotDum));
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
	if (split >= Dir->MessageCount) {
	    /* None of them match */
	    split = Dir->MessageCount;
	}
	startbyte += split * AMS_SNAPSHOTSIZE;
    }
    *numbytes = (Dir->MessageCount * AMS_SNAPSHOTSIZE) - startbyte;
    *bytesleft = 0;
    if (*numbytes > MaxReturn) {
	*bytesleft = *numbytes - MaxReturn;
	*numbytes = MaxReturn;
    }
    if (*numbytes <= 0) {
	*numbytes = 0;
	ReturnBuf[0] = '\0';	/* Probably not needed, but cannot hurt */
	if (datefield != NULL && *datefield != '\0' && !strcmp(AssTime, datefield)) {
	    /* No new messages, but file mod time was not set right */
	    debug(256, ("Setting file time, hope to short cut next time\n"));
	    if (MS_SetAssociatedTime(FullDirName, AssTime)) {
		errsave = mserrcode;

		CloseMSDir(Dir, MD_READ);
		return(errsave);
	    }
	}
	CacheDirectoryForClosing(Dir, MD_READ);
	return(0);
    }
    startbyte += AMS_DIRHEADSIZE;
    if (startbyte != Dir->CurPos && lseek(Dir->fd, startbyte, L_SET) < 0) {
	errsave = errno;
	CloseMSDir(Dir, MD_READ);
	AMS_RETURN_ERRCODE(errsave, EIN_LSEEK, EVIA_HEADERSSINCE);
    }
    if (read(Dir->fd, ReturnBuf, *numbytes) != *numbytes) {
	errsave = errno;
	CloseMSDir(Dir, MD_READ);
	AMS_RETURN_ERRCODE(errsave, EIN_READ, EVIA_HEADERSSINCE);
    }
    Dir->CurPos = startbyte + *numbytes;
    if (Dir->Writable) {
	char *s;

	debug(4, ("Setting modifiable attribute bit\n"));
	for (s = ReturnBuf; s<(ReturnBuf+*numbytes); s+= AMS_SNAPSHOTSIZE) {
	    AMS_SET_ATTRIBUTE(s, AMS_ATT_MAYMODIFY);
	    fixDate(AMS_DATE(s));
	}
    } else {
	char *s;

	debug(4, ("Unsetting modifiable attribute bit\n"));
	for (s = ReturnBuf; s<(ReturnBuf+*numbytes); s+= AMS_SNAPSHOTSIZE) {
	    AMS_UNSET_ATTRIBUTE(s, AMS_ATT_MAYMODIFY);
	    fixDate(AMS_DATE(s));
	}
    }
    debug(4, ("Returning, %d bytes remain unsent\n", *bytesleft));
    CacheDirectoryForClosing(Dir, MD_READ); /* read only, ignore errors */
    return(0);
}



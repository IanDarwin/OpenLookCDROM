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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/ams/libs/ms/RCS/setasct.c,v 2.10 1994/03/01 23:08:05 rr2b Exp $";
#endif

#include <ms.h>
#include <andyenv.h>
#include <sys/stat.h>

MS_SetAssociatedTime(FullName, newvalue)
char *FullName, *newvalue;
{
    struct MS_Directory *Dir;
    struct stat statbuf;

    debug(257, ("MS_SetAssociatedTime %s %s\n", FullName, newvalue));
    if (*FullName != '/' || (newvalue && (strlen(newvalue) >= AMS_DATESIZE))) {
	AMS_RETURN_ERRCODE(EINVAL, EIN_PARAMCHECK, EVIA_SETASSOCIATEDTIME);
    }
    Dir = (struct MS_Directory *) FindInDirCache(FullName);
    if (newvalue == NULL || *newvalue == '\0') {
	newvalue = "0";
    }
#ifdef CMU_ENV
    if (strncmp(newvalue, "zzz", 3) != 0 && (*newvalue < '0' || *newvalue > '3')) {
	char ErrBuf[MAXPATHLEN+100];
	sprintf(ErrBuf, "BUG: setting time for folder %s to ``%s''", ap_Shorten(FullName), newvalue);
	NonfatalBizarreError(ErrBuf);
	if ((long) Dir == (long) -1 || (long) Dir == (long) NULL || strncmp(newvalue, Dir->LastMsgDate, AMS_DATESIZE) > 0) {	    
	    AMS_RETURN_ERRCODE(EFAULT, EIN_PARAMCHECK, EVIA_SETASSOCIATEDTIME);
	}
    }
#endif /* CMU_ENV */
    if ((long) Dir != (long) -1 && (long) Dir != (long) NULL && (Dir->MessageCount <= 0 || strncmp(newvalue, Dir->LastMsgDate, AMS_DATESIZE) >= 0)) {
	debug(257, ("Last message or later, writing time stamp\n"));
	if (stat(FullName, &statbuf)) {
	    AMS_RETURN_ERRCODE(errno, EIN_STAT, EVIA_SETASSOCIATEDTIME);
	}
	return(SetProfileEntry(FullName, newvalue, statbuf.st_mtime));
    } else {
	debug(257, ("Was not the last message (Dir %d count %d date %s), not writing time stamp\n", Dir, Dir ? Dir->MessageCount : -1, Dir ? Dir->LastMsgDate : 0));
	return(SetProfileEntry(FullName, newvalue, 0));
    }
}

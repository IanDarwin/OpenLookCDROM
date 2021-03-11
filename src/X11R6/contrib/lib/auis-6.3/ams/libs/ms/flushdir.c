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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/ams/libs/ms/RCS/flushdir.c,v 2.11 1993/05/04 00:58:50 susan Exp $";
#endif

#include <andrewos.h>
#include <ms.h>

#ifndef _IBMR2
extern char *malloc();
#endif /* _IBMR2 */

#define MAXNUMWAITING 5
struct MS_Directory *MyOpenDir = NULL;
static int NumberWaiting = 0;
struct {
    char *oldname;
    char *newname;
    Boolean DidRewrite;
    int dirlockfd;
    Boolean IsLast;
} ThingsToDo[MAXNUMWAITING];

PlanToCloseDir(Dir, lockfd, oldname, newname, DidRewrite, UnlinkFailures, IsLast)
struct MS_Directory *Dir;
int lockfd, DidRewrite, *UnlinkFailures;
char *oldname, *newname;
Boolean IsLast;
{
    debug(256, ("PlanToCloseDir %s (%d), myopen is %s (%d)\noldname is %s, newname is %s, IsLast is %d\n",
	Dir->UNIXDir, Dir, MyOpenDir ? MyOpenDir->UNIXDir : "none", MyOpenDir, oldname, newname, IsLast));
    if (Dir != MyOpenDir) {
	if (FlushClosableDir(UnlinkFailures)) {
	    return(mserrcode);
	}
    }
    MyOpenDir = Dir;
    ThingsToDo[NumberWaiting].oldname = malloc(1+strlen(oldname));
    if (ThingsToDo[NumberWaiting].oldname == NULL) {
	AMS_RETURN_ERRCODE(ENOMEM, EIN_MALLOC, EVIA_PLANTOCLOSEDIR);
    }
    strcpy(ThingsToDo[NumberWaiting].oldname, oldname);
    ThingsToDo[NumberWaiting].newname = malloc(1+strlen(newname));
    if (ThingsToDo[NumberWaiting].newname == NULL) {
	AMS_RETURN_ERRCODE(ENOMEM, EIN_MALLOC, EVIA_PLANTOCLOSEDIR);
    }
    strcpy(ThingsToDo[NumberWaiting].newname, newname);
    ThingsToDo[NumberWaiting].DidRewrite = DidRewrite;
    ThingsToDo[NumberWaiting].dirlockfd = lockfd;
    ThingsToDo[NumberWaiting].IsLast = IsLast;
    ++NumberWaiting;
    if (NumberWaiting >= MAXNUMWAITING) {
	if (FlushClosableDir(UnlinkFailures)) {
	    return(mserrcode);
	}
    }
    return(0);
}

FlushClosableDir(UnlinkFailures) 
int *UnlinkFailures;
{
    char ErrorText[100+MAXPATHLEN];
    int i;

    debug(256, ("Flush closables, MyOpenDir %d\n", MyOpenDir));
    if (!MyOpenDir) return(0);
    mserrcode = CloseMSDir(MyOpenDir, MD_APPEND);
    for (i = 0; i<NumberWaiting; ++i) {
	if (ThingsToDo[i].DidRewrite) {
	    if (mserrcode) {
		debug(4, ("Unlinking %s\n", ThingsToDo[i].newname));
		if (unlink(ThingsToDo[i].newname)) {
		    ++*UnlinkFailures;
		    debug(4, ("Unlink failed\n"));
		}
	    } else if (ThingsToDo[i].IsLast) {
		debug(4, ("Unlinking %s\n", ThingsToDo[i].oldname));
		if (unlink(ThingsToDo[i].oldname)) {
		    ++*UnlinkFailures;
		    debug(4, ("Unlink failed\n"));
		}
	    }
	} else {
	    if (mserrcode) {
		debug(4, ("Close failed -- renaming things back as they were\n"));
		if (RenameEvenInVice(ThingsToDo[i].newname, ThingsToDo[i].oldname)) {
		    sprintf(ErrorText, "rename failed -- invisible mail is left in file %s", ap_Shorten(ThingsToDo[i].newname));
		    NonfatalBizarreError(ErrorText);
		} else {
		    debug(4, ("Renamed %s to %s\n", ThingsToDo[i].newname, ThingsToDo[i].oldname));
		}
	    } else {
		debug(4, ("Everything worked out fine on the close\n"));
	    }
	}
	free(ThingsToDo[i].oldname);
	free(ThingsToDo[i].newname);
	if (ThingsToDo[i].IsLast) close(ThingsToDo[i].dirlockfd);
    }
    NumberWaiting = 0;
    MyOpenDir = NULL;
    return(mserrcode);
}

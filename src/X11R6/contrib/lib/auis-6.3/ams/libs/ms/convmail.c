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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/ams/libs/ms/RCS/convmail.c,v 2.10 1993/09/21 21:52:40 gk5g Exp $";
#endif

#include <andrewos.h>
#include <ms.h>

extern char home[];

MS_ConvertOldMail(good, bad)
int *good, *bad;
{
    char OldName[1+MAXPATHLEN], NewName[1+MAXPATHLEN], DirName[1+MAXPATHLEN], NewDirName[1+MAXPATHLEN], ErrorText[256];
    DIR *dirp;
    int mybad = 0;
    DIRENT_TYPE *dirent;

    debug(1, ("MS_ConvertOldMail"));
    if (EnsureMailDirExists()) {
	return(mserrcode);
    }
    *good = *bad = 0;
    sprintf(DirName, "%s/Maillib", home);
    sprintf(NewDirName, "%s/.MESSAGES/%s", home, AMS_DEFAULTMAILDIR);
    mserrcode = MS_SetSubscriptionEntry(NewDirName, AMS_DEFAULTMAILDIR, AMS_ALWAYSSUBSCRIBED);
    if (mserrcode) {
	return(mserrcode);
    }
    if ((dirp = opendir(DirName)) == NULL) {
	if (errno == ENOENT) {
	    return(0); /* Nothing to convert, that's fine */
	}
	AMS_RETURN_ERRCODE(errno, EIN_OPENDIR, EVIA_CONVERTOLD);
    }
    while ((dirent = readdir(dirp)) != NULL) {
	if (!strcmp(dirent->d_name, ".")) continue;
	if (!strcmp(dirent->d_name, "..")) continue;
	sprintf(OldName, "%s/%s", DirName, dirent->d_name);
	sprintf(NewName, "%s/.MESSAGES/%s/%s", home, AMS_DEFAULTMAILDIR, dirent->d_name);
	if (!strcmp(dirent->d_name, "view.grt") || !strcmp(dirent->d_name, "messages.grt")) {
	    if (unlink(OldName)) {
		sprintf(ErrorText, "Could not unlink file %s", ap_Shorten(dirent->d_name));
		NonfatalBizarreError(ErrorText);
	    }
	} else {
	    if (RenameEvenInVice(OldName, NewName)) {
		++mybad;
		sprintf(ErrorText, "Could not convert old mail file %s", ap_Shorten(dirent->d_name));
		NonfatalBizarreError(ErrorText);
	    }
	}
    }
    closedir(dirp);
    if (rmdir(DirName)) {
	sprintf(ErrorText, "Could not remove old Maillib directory (%d)", errno);
	NonfatalBizarreError(ErrorText);
    }
    mserrcode = MS_ReconstructDirectory(NewDirName, good, bad, 1);
    *bad += mybad;
    return(mserrcode);
}

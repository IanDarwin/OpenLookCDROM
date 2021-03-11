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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/ams/libs/ms/RCS/rmvdir.c,v 2.18 1993/09/21 21:59:37 gk5g Exp $";
#endif

#include <andrewos.h>
#include <sys/stat.h>
#include <ms.h>

extern char *getprofile(), *StripWhiteEnds();

long    MS_RemoveDirectory (DirName, MaxRemovals)
char   *DirName;
int MaxRemovals;
{
    struct MS_Directory *Dir;
    DIR *dirp;
    DIRENT_TYPE *dirent;
    struct stat stbuf;
    char FileName[1+MAXPATHLEN];

    debug(1, ("Entering MS_RemoveDirectory %s\n", DirName));
    if (ReadOrFindMSDir(DirName, &Dir, MD_OK)) {
	return(mserrcode);
    }
    if (Dir->MessageCount > MaxRemovals) {
	AMS_RETURN_ERRCODE(ENOTEMPTY, EIN_PARAMCHECK, EVIA_REMOVEDIR);
    }
    if ((dirp = opendir(DirName)) == NULL) {
	debug(4, ("Can't open source directory %s\n", DirName));
	AMS_RETURN_ERRCODE(errno, EIN_OPENDIR, EVIA_REMOVEDIR);
    }
    while ((dirent = readdir(dirp)) != NULL) {
	if (!strcmp(dirent->d_name, ".")) continue;
	if (!strcmp(dirent->d_name, "..")) continue;
	sprintf(FileName, "%s/%s", DirName, dirent->d_name);
	if (stat(FileName, &stbuf)) {
	    closedir(dirp);
	    AMS_RETURN_ERRCODE(errno, EIN_STAT, EVIA_REMOVEDIR);
	}
	if ((stbuf.st_mode & S_IFMT) == S_IFDIR) {
	    closedir(dirp);
	    AMS_RETURN_ERRCODE(EMSDIRHASKIDS, EIN_PARAMCHECK, EVIA_REMOVEDIR);
	}
    }
    rewinddir(dirp);
    while ((dirent = readdir(dirp)) != NULL) {
	if (!strcmp(dirent->d_name, ".")) continue;
	if (!strcmp(dirent->d_name, "..")) continue;
	sprintf(FileName, "%s/%s", DirName, dirent->d_name);
	if (unlink(FileName) && errno != ENOENT) { /* ENOENT is spurious */
	    closedir(dirp);
	    AMS_RETURN_ERRCODE(errno, EIN_UNLINK, EVIA_REMOVEDIR);
	}
    }
    closedir(dirp);
    if(rmdir(DirName) && errno != ENOENT) { /* BOGUS -- spurious vice ENOENT */
        if (errno == ENOTEMPTY) {
            /* STUPID NFS BUG! */
            char CmdBuf[1200];
            sprintf(CmdBuf, "rm %s/.nfs*", DirName);
            NonfatalBizarreError("Checking for stupid NFS tmp file bug...");
            system(CmdBuf);
            if (!rmdir(DirName) || errno == ENOENT) errno = 0;
        }
        if (errno) {
            AMS_RETURN_ERRCODE(errno, EIN_RMDIR, EVIA_REMOVEDIR);
        }
    }
    RemoveSubsEntry(DirName);
    SetSubsEntry(DirName, "", AMS_UNSUBSCRIBED);
    DropHint(DirName);
    RemoveFromCrucialClassesPreference(DirName, NULL);
    return(EnsureNotInSubscriptionMap(DirName));
}

RemoveFromCrucialClassesPreference(DirName, NewName)
char *DirName, *NewName;
{
    char *s, *t, *t2, NickName[1+MAXPATHLEN], NewPref[2500];
    Boolean FoundIt = FALSE, IsMagic;

    NewPref[0] = '\0';
    BuildNickName(DirName, NickName);
    s = getprofile("messages.CrucialClasses");
    if (!s) return(0);
    while (s && *s) {
	t = strchr(s, ',');
	if (t) *t++='\0';
	t2 = strchr(s, ':');
	if (t2) {
	    *t2++ = '\0';
	    t2 = StripWhiteEnds(t2);
	}
	s = StripWhiteEnds(s);
	if (*s == '*') {
	    ++s;
	    IsMagic = TRUE;
	} else {
	    IsMagic = FALSE;
	}
	if (!strcmp(s, NickName) || !strcmp(s, DirName) || (t2 && (!strcmp(t2, NickName) || !strcmp(t2, DirName)))) {
	    FoundIt = TRUE;
	    if (NewName && *NewName) {
		char NewNick[1+MAXPATHLEN];
		if (NewPref[0]) strcat(NewPref, ",");
		if (IsMagic) strcat(NewPref, "*");
		BuildNickName(NewName, NewNick);
		strcat(NewPref, NewNick);
	    }
	} else {
	    if (NewPref[0]) strcat(NewPref, ",");
	    if (IsMagic) strcat(NewPref, "*");
	    strcat(NewPref, s);
	    if (t2) {
		strcat(NewPref, ':');
		strcat(NewPref, t2);
	    }
	}
	s = t;
    }
    if (FoundIt) {
	if (setprofilestring("messages", "crucialclasses", NewPref)) {
	    NonfatalBizarreError("Could not rewrite your CrucialClasses preference; sorry!");
	}
    }
    return(0);
}

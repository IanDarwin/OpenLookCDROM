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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/ams/libs/ms/RCS/epoch.c,v 2.42 1993/09/21 21:54:19 gk5g Exp $";
#endif

#include <andrewos.h>
#include <ms.h>
#include <sys/stat.h>
#include <util.h>
#include <svcconf.h>
#ifdef AFS_ENV
#include <netinet/in.h>
#include <afs/param.h>
#include <afs/venus.h>
#endif /* AFS_ENV */

static int dirCmp(s1, s2)
char **s1, **s2;
{
	return strcmp(*s1, *s2);
}

static int tempErr(val)
int val;
{/* Return TRUE iff the AMS_ERRNO value ``val'' seems like a retry-able failure. */
    if (tfail(val)) return TRUE;
    /* add this case for persistent AFS errors */
    if (val == ENOENT) {
#ifdef AFS_ENV
	if (ViceIsRunning()) {
	    struct ViceIoctl blob;
	    blob.in = NULL;
	    blob.in_size = 0;
	    blob.out = NULL;
	    blob.out_size = 0;
	    (void) pioctl(ViceFile, VIOCCKBACK, &blob, 1);	/* fs checkbackups */
	    (void) pioctl(ViceFile, VIOCCKSERV, &blob, 1);	/* fs checkservers */
	}
#endif /* AFS_ENV */
	return TRUE;
    }
    switch (val) {
	case EWOULDBLOCK:
	case EMSVICE: case EMSBOGUS: case EMSRECONFAILED: case EMSYOUNGLOCK:
	case EMSSYNTAX: case EMSGUARDIANERR: case EMSNOMEM:
	case EMSSNAPAUTH: case EMSUNSUPPORTED: case EMSELIUNBOUND:
	case EMSELIUNDEF: case EMSSNAPINIT:
	    return TRUE;
	default:
	    return FALSE;
    }
}

struct DelayedDir {struct DelayedDir *Next; char *Name; int Recurse, Depth;};
static struct DelayedDir *Work, *MoreWork;

static int AddWork(Dir, Recurse, Depth)
char *Dir; int Recurse, Depth;
{/* Add an entry to the MoreWork list. */
    struct DelayedDir *This;
    char ErrTxt[100+MAXPATHLEN];

    This = (struct DelayedDir *) malloc(sizeof(struct DelayedDir));
    if (This == NULL) {
	sprintf(ErrTxt, "Can't remember %s to work on later: no memory", ap_Shorten(Dir));
	NonfatalBizarreError(ErrTxt);
	return -1;
    }
    This->Name = NewString(Dir);
    if (This->Name == NULL) {
	sprintf(ErrTxt, "Can't remember %s to work on later: no memory", ap_Shorten(Dir));
	NonfatalBizarreError(ErrTxt);
	free(This);
	return -1;
    }
    This->Recurse = Recurse;
    This->Depth = Depth;
    This->Next = MoreWork;
    MoreWork = This;
    return 0;
}

MS_Epoch(dirname, date64)
char *dirname;
char *date64;
{
    char ErrTxt[100+MAXPATHLEN];
    struct DelayedDir *DP;
    char DateSave[AMS_DATESIZE+1];
    int Ctr, ErrExit;
    int Persist;	/* Never changed in this implementation, but maybe someday. */

    Persist = 1;
    strncpy(DateSave, date64, AMS_DATESIZE);
    sprintf(ErrTxt, "Epoching messages dated before %s in %s and its descendants.", NiceTime(conv64tolong(DateSave)), ap_Shorten(dirname));
    NonfatalBizarreError(ErrTxt);
    Work = MoreWork = NULL;
    if (AddWork(dirname, 1, 0)) {
	NonfatalBizarreError("Could not begin to work on epoching: no memory");
	AMS_RETURN_ERRCODE(ENOMEM, EIN_MALLOC, EVIA_EPOCH);
    }
    ErrExit = 0;
    for (Ctr = 0; MoreWork != NULL && Ctr < 10; ++Ctr) {
	Work = MoreWork;
	MoreWork = NULL;
	while (Work != NULL) {
	    if (mserrcode = RealEpoch(Work->Name, DateSave, Work->Depth, Persist, Work->Recurse)) {
		if (!Persist) {
		    ErrExit = mserrcode;
		    break;
		}
	    }
	    DP = Work;
	    Work = Work->Next;
	    free(DP->Name);
	    free(DP);
	}
    }
    while (Work != NULL) {
	DP = Work;
	Work = Work->Next;
	free(DP->Name);
	free(DP);
    }
    while (MoreWork != NULL) {
	DP = MoreWork;
	MoreWork = MoreWork->Next;
	free(DP->Name);
	free(DP);
    }
    return(mserrcode = ErrExit);
}

static int RealEpoch(dirname, date64, depth, Persist, Recurse)
char *dirname;
char *date64;
int depth, Persist, Recurse;
{/* Epoch dirname's descendants, then dirname itself, back to date64.  If Persist is true, don't let errors in subfolders prevent us from continuing the tree walking. */
    DIR *dirp;
    DIRENT_TYPE *dirent;
    struct stat stbuf;
    char Prefix[MAXPATHLEN+1];
    char **Children, ErrorText[100+MAXPATHLEN];
    int PathOffset, ChildrenCt, KidIx, ChildrenAllocated, KidFailed, errsave;
    static char *epochfailed = "Epoch failed for %s and %s of its children";

    debug(262145, ("MS_Epoch %s %s\n", dirname, date64));
    if (depth > 0) {
	char Inhibitor[1+MAXPATHLEN];
	strcpy(Inhibitor, dirname);
	strcat(Inhibitor, "/.AMS_PurgingCutoff");
	if (stat(Inhibitor, &stbuf)) {
	    if (errno != ENOENT) {
		errsave = errno;
		sprintf(ErrorText, "Can't stat purging-cutoff file in %s: %s;", ap_Shorten(dirname), UnixError(errsave));
		NonfatalBizarreError(ErrorText);
		sprintf(ErrorText, epochfailed, ap_Shorten(dirname), "all");
		NonfatalBizarreError(ErrorText);
		AMS_RETURN_ERRCODE(errsave, EIN_STAT, EVIA_EPOCH);
	    }
	} else {
	    char ErrorText[100+MAXPATHLEN];
	    sprintf(ErrorText, "Not purging recursively in directory %s or its children", ap_Shorten(dirname));
	    NonfatalBizarreError(ErrorText);
	    return(0); /* shouldn't recurse into this directory */
	}
    }
    KidFailed = 0;
    ChildrenCt = -1;
    if (Recurse) {
	ChildrenCt = 0;
	ChildrenAllocated = 200;
	Children = (char **) malloc(ChildrenAllocated * sizeof(char *));
	if (!Children) {
	    sprintf(ErrorText, "No memory for dir %s;", ap_Shorten(dirname));
	    NonfatalBizarreError(ErrorText);
	    AddWork(dirname, 1, depth);
	    sprintf(ErrorText, epochfailed, ap_Shorten(dirname), "all");
	    NonfatalBizarreError(ErrorText);
	    AMS_RETURN_ERRCODE(ENOMEM, EIN_MALLOC, EVIA_EPOCH);
	}
	if ((dirp = opendir(dirname)) == NULL) {
	    errsave = errno;
	    sprintf(ErrorText, "Epoch: can't open %s: %s;", ap_Shorten(dirname), UnixError(errsave));
	    NonfatalBizarreError(ErrorText);
	    if (tempErr(errsave)) AddWork(dirname, 1, depth);
	    sprintf(ErrorText, epochfailed, ap_Shorten(dirname), "all");
	    NonfatalBizarreError(ErrorText);
	    free(Children);
	    AMS_RETURN_ERRCODE(errsave, EIN_OPENDIR, EVIA_EPOCH);
	}
	sprintf(Prefix, "%s/", dirname);
	PathOffset = strlen(Prefix);
	for (dirent = readdir(dirp); dirent != NULL; dirent = readdir(dirp)) {
	    if (*dirent->d_name == '+' || *dirent->d_name == '.') {
		continue;
	    }
	    Children[ChildrenCt] = malloc(PathOffset + strlen(dirent->d_name)+2);
	    if (Children[ChildrenCt] == NULL) {
		closedir(dirp);
		while (--ChildrenCt >= 0) free (Children[ChildrenCt]);
		free(Children);
		sprintf(ErrorText, "Out of memory for kid-name in %s;", ap_Shorten(dirname));
		NonfatalBizarreError(ErrorText);
		AddWork(dirname, 1, depth);
		sprintf(ErrorText, epochfailed, ap_Shorten(dirname), "all");
		NonfatalBizarreError(ErrorText);
		AMS_RETURN_ERRCODE(ENOMEM, EIN_MALLOC, EVIA_EPOCH);
	    }
	    sprintf(Children[ChildrenCt], "%s%s", Prefix, dirent->d_name);
	    stat(Children[ChildrenCt], &stbuf);
	    if ((stbuf.st_mode & S_IFMT) != S_IFDIR) {
		debug(4, ("Skipping non-directory file %s\n", Children[ChildrenCt]));
		free(Children[ChildrenCt]);
		continue;
	    }
	    if (++ChildrenCt >= ChildrenAllocated) {
		ChildrenAllocated = (2*ChildrenAllocated) + 30;
		Children = (char **) realloc(Children, ChildrenAllocated * sizeof(char *));
		if (!Children) {
		    sprintf(ErrorText, "Out of memory for kids of %s;", ap_Shorten(dirname));
		    NonfatalBizarreError(ErrorText);
		    AddWork(dirname, 1, depth);
		    closedir(dirp);
		    sprintf(ErrorText, epochfailed, ap_Shorten(dirname), "all");
		    NonfatalBizarreError(ErrorText);
		    AMS_RETURN_ERRCODE(ENOMEM, EIN_MALLOC, EVIA_EPOCH);
		}
	    }
	}
	closedir(dirp);

	if (ChildrenCt > 1) qsort(Children, ChildrenCt, sizeof(Children[0]), dirCmp);

	for (KidIx = 0; KidIx < ChildrenCt; ++KidIx) {
	    debug(4, ("full name is %s\n", Children[KidIx]));
	    if (mserrcode = RealEpoch(Children[KidIx], date64, depth + 1, Persist, Recurse)) {
		++KidFailed;
		if (!Persist) {
		    sprintf(ErrorText, epochfailed, ap_Shorten(dirname), "some or all");
		    NonfatalBizarreError(ErrorText);
		    for (; KidIx < ChildrenCt; ++KidIx) free(Children[KidIx]);
		    free(Children);
		    return(mserrcode);
		}
	    }
	    free(Children[KidIx]);
	}
	free(Children);
    }
    if (DeleteThrough(dirname, date64, ChildrenCt)
	|| MS_PurgeDeletedMessages(dirname))
    {
	if (depth > 0 || AMS_ERRNO != ENOENT) {
	    sprintf(ErrorText, "Epoch on %s: [%d/%d] ", ap_Shorten(dirname), AMS_ERRCAUSE, AMS_ERRVIA);
	    if (vdown(AMS_ERRNO)) strcat(ErrorText, "(AFS down) ");
	    strcat(ErrorText, UnixError(AMS_ERRNO));
	    strcat(ErrorText, ";");
	    NonfatalBizarreError(ErrorText);
	    sprintf(ErrorText, "Epoch failed for %s and %d of its %d children", ap_Shorten(dirname), KidFailed, ChildrenCt);
	    NonfatalBizarreError(ErrorText);
	    if (tempErr(AMS_ERRNO)) AddWork(dirname, 0, depth);
	    return(mserrcode);
	} else {	/* The root doesn't need to have a .MS_MsgDir */
	    sprintf(ErrorText, "Epoch vacuously succeeded for %s and failed for %d of its %d children", ap_Shorten(dirname), KidFailed, ChildrenCt);
	    NonfatalBizarreError(ErrorText);
	    return(0);
	}
    }
    if (KidFailed > 0) {
	sprintf(ErrorText, "Epoch worked for %s but failed for %d of its %d children", ap_Shorten(dirname), KidFailed, ChildrenCt);
	NonfatalBizarreError(ErrorText);
    }
    return(0);
}


static DeleteThrough(dirname, date64, anyKids)
char *dirname;
char *date64; int anyKids;
{
    char ErrorText[100+MAXPATHLEN], SnapshotDum[AMS_SNAPSHOTSIZE];
    struct MS_Directory *Dir;
    int     errsave,
            msgnum,
	    comp,
            top,
            bottom,
            retries = 2,
            split;

    debug(1,("DeleteThrough %s %s\n", dirname, date64));
    if (date64 == NULL || *date64 == '\0') {
	AMS_RETURN_ERRCODE(EINVAL, EIN_PARAMCHECK, EVIA_DELETETHROUGH);
    }
    CloseDirsThatNeedIt();
    mserrcode = ReadOrFindMSDir(dirname, &Dir, MD_WRITE);
    while ((--retries > 0) && (mserrcode != 0) && (AMS_ERRNO == EWOULDBLOCK)) {
	sprintf(ErrorText, "%s is locked; retrying in 5 seconds...", ap_Shorten(dirname));
	NonfatalBizarreError(ErrorText);
	sleep(5);
	mserrcode = ReadOrFindMSDir(dirname, &Dir, MD_WRITE);
    }
    if (mserrcode) return(mserrcode);
    if (Dir->MessageCount == 0) {
	debug(4,("Nothing to purge!\n"));
	split = 0;
    } else {
	top = Dir->MessageCount - 1;
	bottom = 0;

	for (;;) {
	    if (bottom > top) {split = top; break;}
	    split = (bottom + top) / 2;
	    if (GetSnapshotByNumber(Dir, split, SnapshotDum)) {
		errsave = mserrcode;
		CloseMSDir(Dir, MD_WRITE);
		return(mserrcode = errsave);
	    }
	    comp = strcmp(date64, AMS_DATE(SnapshotDum));
	    if (comp == 0) {
		++split;
		break;
	    } else if (comp < 0) {
		top = split - 1;
	    } else {
		bottom = split + 1;
	    }
	}
	if (split >= Dir->MessageCount) {
	    /* None of them matches */
	    split = Dir->MessageCount;
	}
    }
    if (Dir->MessageCount > 0) {
	if (GetSnapshotByNumber(Dir, Dir->MessageCount - 1, SnapshotDum)) {
	    sprintf(ErrorText, "Can't check for crazy dates in %s", ap_Shorten(dirname));
	    NonfatalBizarreError(ErrorText);
	} else if (conv64tolong(AMS_DATE(SnapshotDum)) >= time(0)) {
	    sprintf(ErrorText,
		    "Message ID %s in folder %s has a date in the future and should be investigated (caption is '%s').",
		    AMS_ID(SnapshotDum),
		    ap_Shorten(dirname), AMS_CAPTION(SnapshotDum));
	    NonfatalBizarreError(ErrorText);
	} else {
	    for (msgnum = 1; msgnum < split; ++msgnum) { /* start at 1 to preserve 1st msg */
		GetSnapshotByNumber(Dir, msgnum, SnapshotDum);
		if (strcmp(date64, AMS_DATE(SnapshotDum)) > 0) {
		    AMS_SET_ATTRIBUTE(SnapshotDum, AMS_ATT_DELETED);
		    debug(4,("Deleting message %d %s\n", msgnum, AMS_CAPTION(SnapshotDum)));
		    RewriteSnapshotInDirectory(Dir, msgnum, SnapshotDum);
		} else {
		    sprintf(ErrorText, "Message ID %s in folder %s (%d of %d of %d) was supposed to be purged, but its date is too recent: %s", AMS_ID(SnapshotDum), ap_Shorten(dirname), msgnum, split, Dir->MessageCount, NiceTime(conv64tolong(AMS_DATE(SnapshotDum))));
		    NonfatalBizarreError(ErrorText);
		    break;
		}
	    }
	}
    }
    if (Dir->MessageCount <= 1 && anyKids == 0) {
	long oldage = 0;
	if (Dir->MessageCount == 1) {
	    if (GetSnapshotByNumber(Dir, 0, SnapshotDum)) {
		oldage = -1;
	    } else {
		oldage = time(0) - conv64tolong(AMS_DATE(SnapshotDum));
	    }
	    sprintf(ErrorText, "The folder %s has only one message (%s old) and should itself be considered for deletion.", ap_Shorten(dirname), DescribeTimeInterval(oldage));
	} else {
	    sprintf(ErrorText, "The folder %s is empty and should itself be considered for deletion.", ap_Shorten(dirname));
	}
	NonfatalBizarreError(ErrorText);
    }
    errsave = CloseMSDir(Dir, MD_WRITE);
    if (errsave == 0 && split > 1) {
	sprintf(ErrorText, "Deleted %d messages from folder %s.", split-1, ap_Shorten(dirname));
	NonfatalBizarreError(ErrorText);
    }
    return(mserrcode = errsave);
}

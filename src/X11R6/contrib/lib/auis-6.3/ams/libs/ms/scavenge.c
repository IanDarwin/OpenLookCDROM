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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/ams/libs/ms/RCS/scavenge.c,v 1.10 1993/09/21 21:59:58 gk5g Exp $";
#endif

#include <andrewos.h>
#include <ms.h>
#include <sys/stat.h>
#include <ctype.h>

#ifndef EVIA_SCAVENGE
#define EVIA_SCAVENGE EVIA_UNKNOWN /* temp hack until first wash */
#endif

long MS_ScavengeDirectory(DirName, Recurse, numgood, numbad, quiet, Purge)
char *DirName;
int Recurse, *numgood, *numbad, quiet, Purge;
{
    *numgood = *numbad = 0;
    if (!Recurse) {
	ScavengeOneDirectory(DirName, numgood, numbad, quiet, Purge);
	return(0);
    } else {
	return(ScavengeDown(DirName, numgood, numbad, quiet, Purge));
    }
}

ScavengeDown(DirName, numgood, numbad, quiet, Purge)
char *DirName;
int *numgood, *numbad, quiet, Purge;
{
    DIR *dirp;
    DIRENT_TYPE *dirent;
    struct stat stbuf;
    char Prefix[MAXPATHLEN+1];
    char **Children;
    int PathOffset, ChildrenCt=0, ChildrenAllocated = 200, i;

    Children = (char **) malloc(ChildrenAllocated * sizeof(char *));
    if (!Children) {
	AMS_RETURN_ERRCODE(ENOMEM, EIN_MALLOC, EVIA_SCAVENGE);
    }
    if ((dirp = opendir(DirName)) == NULL) {
	free(Children);
	AMS_RETURN_ERRCODE(errno, EIN_OPENDIR, EVIA_SCAVENGE);
    }

    sprintf(Prefix, "%s/", DirName);
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
	    AMS_RETURN_ERRCODE(ENOMEM, EIN_MALLOC, EVIA_SCAVENGE);
	}
	sprintf(Children[ChildrenCt], "%s%s", Prefix, dirent->d_name);
	stat(Children[ChildrenCt], &stbuf);
	if ((stbuf.st_mode & S_IFMT) != S_IFDIR) {
	    debug(4, ("Skipping non-directory file %s\n", Children[ChildrenCt]));
	    continue;
	}
	if (++ChildrenCt >= ChildrenAllocated) {
	    ChildrenAllocated += 200;
	    Children = (char **) realloc(Children, ChildrenAllocated * sizeof(char *));
	    if (!Children) {
		closedir(dirp);
		AMS_RETURN_ERRCODE(ENOMEM, EIN_MALLOC, EVIA_SCAVENGE);
	    }
	}
    }
    closedir(dirp);
    for (i=0; i<ChildrenCt; ++i) {
	mserrcode = ScavengeDown(Children[i], numgood, numbad, quiet, Purge);
	if (mserrcode) {
	    while (ChildrenCt-- > 0) free (Children[ChildrenCt]);
	    free(Children);
	    return(mserrcode);
	}
    }
    while (ChildrenCt-- > 0) free (Children[ChildrenCt]);
    free(Children);
    ScavengeOneDirectory(DirName, numgood, numbad, quiet, Purge);
    return(0);
}

ScavengeOneDirectory(DirName, numgood, numbad, quiet, Purge)
char *DirName;
int *numgood, *numbad, quiet, Purge;
{
    struct MS_Directory *Dir;
    char ErrorText[100+MAXPATHLEN];

    if (ReadOrFindMSDir(DirName, &Dir, MD_APPEND)
	 || HandleMarksInProgress(Dir, quiet)
	 || CloseMSDir(Dir, MD_APPEND)
	 || DropHint(DirName)) {

	sprintf(ErrorText, "Scavenge failed for %s (%d, %d, %d)\n", DirName, AMS_ERRNO, AMS_ERRCAUSE, AMS_ERRVIA);
	NonfatalBizarreError(ErrorText);
	++*numbad;
    } else {
	++*numgood;
	if (Purge && MS_PurgeDeletedMessages(DirName)) {
	    sprintf(ErrorText, "Could not purge deletions in %s (%d, %d, %d)\n", DirName, AMS_ERRNO, AMS_ERRCAUSE, AMS_ERRVIA);
	    NonfatalBizarreError(ErrorText);
	}
    }
    return(0);
}

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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/ams/msclients/nns/RCS/gl.c,v 2.18 1993/09/29 20:09:00 gk5g Exp $";
#endif

/* Methods for the GroupList data type for the Netnews Snarfer
*/

#include <andrewos.h>
#include <big.h>

static int      GLAddEntries();
static int      GLAddEntry();
static int      GLGrowIfNecessary();
static int      FindNewsgroupsAndMaybeTime();
static void     GLDeleteRecentEntries();
static void     GLMoveFailed();
static void     GLSort();
static void     GLSortAndComputeRefs();

void	    GLInit(gl)
GList_t	*gl;
{
    gl->num = gl->size = 0;
    gl->entries = (GListEntry_t *) NULL;
}

/* This function returns the number of entries added to the list,
** or a negative error code
*/

int	     GLBuildList(gl, dir)
GList_t	*gl;
char	   *dir;
{
    DIR	    *dp, *opendir();
    DIRENT_TYPE  *dirent, *readdir();
    int	     startNum = gl->num;

    Verbiage(3, "In GLBuildList");
    if (!(dp = opendir(dir)))
	return (CANT_OPEN_DIR);
    while (dirent = readdir(dp)) {
	if (dirent->d_name[0] != '.' && !GLAddEntries(gl, dir, dirent)) {
	    GLMoveFailed(dir, dirent->d_name);
	}
    }
    closedir(dp);		/* BOGUS -- does this have a return value? */
    Verbiage(2, "Added all possible entries; now sorting...");
    GLSortAndComputeRefs(gl);
    return (gl->num - startNum);
}

static int      GLAddEntries(gl, dir, dirent)
GList_t	*gl;
char	   *dir;
DIRENT_TYPE  *dirent;
{
    int	     fd, dirNameLen = strlen(dir), added = 0;
    int      control, delayentry, delaygroup;
    char     groups[GROUPSLINESIZE], *groupStart, *groupEnd;
    char     fullName[MAXPATHLEN + 1], moveTo[MAXPATHLEN + 1], *folder;
    long     time;

    Verbiage(3, "In GLAddEntries");
    strcpy(fullName, dir);
    fullName[dirNameLen++] = '/';
    strcpy(fullName + dirNameLen, dirent->d_name);
    Verbiage(1, "Working on file:");
    Verbiage(1, fullName);
    if ((fd = open(fullName, O_RDONLY)) < 0)
	return (FALSE);	/* Nonfatal error */
    if (FindNewsgroupsAndMaybeTime(fd, groups, &time, &control)) {
	groupStart = groups;
	delayentry = 1;
	while (groupStart) {
	    if (groupEnd = strchr(groupStart, ','))
		*groupEnd = '\0';
	    if (folder = ConfDirForGroup(groupStart, &delaygroup)) {
		if (!delaygroup) delayentry = 0;
		if (GLAddEntry(gl, dirent->d_name, folder, time))
		  ++added;
		else {
		    Verbiage(1, "GLAddEntry failed; deleting recent entries");
		    GLDeleteRecentEntries(gl, added);
		    close(fd);	/* BOGUS -- is there a return value? */
		    return (FALSE);
		}
	    }
	    groupStart = groupEnd ? groupEnd + 1 : NULL;
	}
	if (!added) {
	    Verbiage(1, "All newsgroups are dropped, unlinking file");
	    (void) unlink(fullName);
	}
	else if (delayentry) {
	    GLDeleteRecentEntries(gl, added);
	    /*
	     * Don't bother checking if dir is DelayedDir.
	     * We won't be processing DelayedDir during peak hours.
	     */
	    strcpy(moveTo, DelayedDir);
	    strcat(moveTo, "/");
	    strcat(moveTo, dirent->d_name);
	    Verbiage(1, "All newsgroups are to be delayed, moving to:");
	    Verbiage(1, moveTo);
	    (void) mkdir(DelayedDir, 0777);
	    (void) rename(fullName, moveTo);
	}
    }
    else if (control) {
	strcpy(moveTo, ControlDir);
	strcat(moveTo, "/");
	strcat(moveTo, dirent->d_name);
	Verbiage(1, "Control header found, moving to:");
	Verbiage(1, moveTo);
	(void) mkdir(ControlDir, 0777);
	(void) rename(fullName, moveTo);
    }
    else {
	Verbiage(1, "No newsgroup found; sending to \"lost\"");
	folder = ConfDirForGroup(LOSTDIR, &delaygroup);
	if (!folder) {
	    Verbiage(1, "Strange...\"lost\" is dropped.  Unlinking file");
	    (void) unlink(fullName);
	}
	else if (!GLAddEntry(gl, dirent->d_name, folder, time)) {
	    Verbiage(1, "GLAddEntry failed");
	    close(fd);	/* BOGUS -- return value? */
	    return (FALSE);
	}
    }
    close(fd);		/* BOGUS -- return value? */
    return (TRUE);
}

static int      GLAddEntry(gl, filename, folder, time)
GList_t	*gl;
char	   *filename, *folder;
long	    time;
{
    char	   *filenameBuf, *folderBuf;

    Verbiage(3, "In GLAddEntry");
    if (!GLGrowIfNecessary(gl)) {
	Verbiage(1, "GLGrowIfNecessary failed");
	return (FALSE);
    }
    if (!(filenameBuf = STFindOrMake(&(Globals.STable), filename))) {
	Verbiage(1, "STFindOrMake of filenameBuf failed on string:");
	Verbiage(1, filenameBuf);
	return (FALSE);
    }
    if (!(folderBuf = STFindOrMake(&(Globals.STable), folder))) {
	Verbiage(1, "STFindOrMake of folderBuf failed on string:");
	Verbiage(1, folderBuf);
	return (FALSE);
    }
    GLESet(&(gl->entries[gl->num]), filenameBuf, folderBuf, 0, 0, 0, time);
    ++(gl->num);
    return (TRUE);
}

static int      GLGrowIfNecessary(gl)
GList_t	*gl;
{
    GListEntry_t   *tmp;

    Verbiage(3, "In GLGrowIfNecessary");
    if (gl->num == gl->size) {
	if (gl->size >= GL_TOOBIG)
	    return (FALSE);
	Verbiage(2, "Attempting to grow the Grouplist");
	if (gl->size) {
	    if (!(tmp = (GListEntry_t *) realloc(gl->entries, (gl->size + GL_GROWSIZE) * sizeof(GListEntry_t))))
		return (FALSE);
	    gl->entries = tmp;
	}
	else {
	    if (!(tmp = (GListEntry_t *) malloc(GL_GROWSIZE * sizeof(GListEntry_t))))
		return (FALSE);
	    gl->entries = tmp;
	}
	gl->size += GL_GROWSIZE;
    }
    return (TRUE);
}

static void     GLDeleteRecentEntries(gl, num)
GList_t	*gl;
int	     num;
{
    gl->num -= num;
}

static void     GLSortAndComputeRefs(gl)
GList_t	*gl;
{
    char	   *currentFile = NULL;
    int	     i, j, numRefs = 0;
    GListEntry_t   *base = 0;

    for (i = 0; i < gl->num; ++i) {
	if (currentFile) {
	    if (currentFile != GLEGetFilename(&(gl->entries[i]))) {
		/* We can do this since all strings are stored only once */
		GLSort(base, numRefs);
		for (j = 0; j < numRefs; ++j) {
		    GLESetAhead(&(base[j]), numRefs - j - 1);
		    GLESetBefore(&(base[j]), j);
		}
		currentFile = GLEGetFilename(base = &(gl->entries[i]));
		numRefs = 1;
	    }
	    else
		++numRefs;
	}
	else {
	    currentFile = GLEGetFilename(base = &(gl->entries[i]));
	    numRefs = 1;
	}
    }
    if (currentFile) {
	GLSort(base, numRefs);
	for (j = 0; j < numRefs; ++j) {
	    GLESetAhead(&(base[j]), numRefs - j - 1);
	    GLESetBefore(&(base[j]), j);
	}
    }
    GLSort(gl->entries, gl->num);
}

static void     GLSort(base, num)
GListEntry_t   *base;
int	     num;
{
    qsort(base, num, sizeof(GListEntry_t), GLECompare);
}

void	    GLPurge(gl)
GList_t	*gl;
{
    if (gl->size)
	free(gl->entries);
    gl->size = gl->num = 0;
    gl->entries = NULL;
}

static void    GLMoveFailed(dir, file)
char *dir, *file;
{
    char moveFrom[MAXPATHLEN + 1], moveTo[MAXPATHLEN + 1];

    if (!strcmp(dir, FailedDir)) return;

    (void) mkdir(FailedDir, 0777);

    strcpy(moveFrom, dir);
    strcat(moveFrom, "/");
    strcat(moveFrom, file);
    strcpy(moveTo, FailedDir);
    strcat(moveTo, "/");
    strcat(moveTo, file);

    Verbiage(1, "Moving to:");
    Verbiage(1, moveTo);
    (void) rename(moveFrom, moveTo);
}

/* This one looks in the file for the Newsgroups line and the Date line.
** If Newsgroups is not found, then FALSE is returned, otherwise TRUE.
** "groups" is filled in with the contents of the newsgroups header,
** and time is filled in with the Unix-style time which is parsed from the
** date header (if found), or zero (if not found).
**
** If a Control: header is found, returns FALSE, with "control" filled in
** with a non-zero value.
*/

static int      FindNewsgroupsAndMaybeTime(fd, groups, time, control)
int	     fd;
char	   *groups;
long	   *time;
int        *control;
{
    int	     foundNewsgroups = FALSE, foundTime = FALSE, looking = TRUE, numToRead = FILECHUNKSIZE, foo;
    char	    buffer[FILECHUNKSIZE + 1], *startPtr, *endPtr, *bufPtr = buffer;

    *time = 0L;			/* In case we never find it */
    *control = 0;
    while (looking) {
	if (read(fd, bufPtr, numToRead) < 1)
	    looking = FALSE;
	else {
	    startPtr = buffer;
	    while (startPtr) {
		if (*startPtr) {
		    if (endPtr = strchr(startPtr, '\n')) {
			if (startPtr == endPtr)	/* End of headers! */
			    looking = FALSE;
			else {
			    *endPtr = '\0';
			    if (!foundNewsgroups && !strncmp("Newsgroups: ", startPtr, 12)) {
				strcpy(groups, startPtr + 12);
				foundNewsgroups = TRUE;
			    }
			    if (!foundTime && !strncmp("Date: ", startPtr, 6)) {
				MS_ParseDate(startPtr + 6, &foo, &foo, &foo, &foo, &foo, &foo, &foo, time);

				/*
				 * We ignore the return value; errors don't
				 * matter 
				 */
				foundTime = TRUE;
			    }
			    if (!strncmp("Control: ", startPtr, 9)) {
				*control = 1;
				return FALSE;
			    }
			}
			startPtr = endPtr + 1;
		    }
		    else {	/* We have a partial line.  Move it to the
				 * beginning of the buffer, then read in more
				 * after it */
			strcpy(buffer, startPtr);
			bufPtr = buffer + strlen(startPtr);
			numToRead = FILECHUNKSIZE - strlen(startPtr);
			startPtr = NULL;
		    }
		}
		else {		/* The end of the line is also the end of the
				 * buffer */
		    bufPtr = buffer;
		    numToRead = FILECHUNKSIZE;
		    startPtr = NULL;
		}
	    }
	}
    }
    return (foundNewsgroups);
}

int	     GLGetNum(gl)
GList_t	*gl;
{
    return (gl->num);
}

GListEntry_t   *GLGetEntry(gl, i)
GList_t	*gl;
int	     i;
{
    return (&(gl->entries[i]));
}

/* This function sets the "ignore" flag for all entries in gl
** whose filenames are filename.  Will move the file from 'dir'
** to the "Failed" directory.
*/

void	    GLIgnore(gl, filename, dir)
GList_t	*gl;
char	   *filename, *dir;
{
    int	     i;

    for (i = 0; i < gl->num; ++i) {
	if (!strcmp(filename, GLEGetFilename(&(gl->entries[i]))))
	    GLESetIgnore(&(gl->entries[i]), TRUE);
    }
    GLMoveFailed(dir, filename);
}

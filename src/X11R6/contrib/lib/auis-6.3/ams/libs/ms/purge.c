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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/ams/libs/ms/RCS/purge.c,v 2.26 1993/08/25 20:36:06 susan Exp $";
#endif

#include <ms.h>
#include <andrewos.h> /* sys/file.h */

#ifndef _IBMR2
extern char *malloc();
#endif /* _IBMR2 */

MS_PurgeDeletedMessages(dirname)
char *dirname;
{
    char SnapshotDum[AMS_SNAPSHOTSIZE], FileNameBuf[1+MAXPATHLEN],
	TmpFileName[1+MAXPATHLEN], HeadDum[AMS_DIRHEADSIZE];
    struct MS_Directory *Dir;
    int msgnum, errsave, wfd, numdeleted = 0, savefd;

    debug(1, ("MS_PurgeDeletedMessages %s\n", dirname));
    CloseDirsThatNeedIt();

    /* Make a first pass unlinking the messages to be deleted
     * hopefully this will make enough space to rewrite the
     * message directory.
     * Directory is not locked or marked in progress since none
     * of that works if the volume or partiton is totaly full.
     */
    if (ReadOrFindMSDir(dirname, &Dir, MD_READ) != 0)
	return(mserrcode);
    for (msgnum = 0; msgnum < Dir->MessageCount; ++msgnum) {
	if (GetSnapshotByNumber(Dir, msgnum, SnapshotDum))
	  continue;
	if (!(AMS_GET_ATTRIBUTE(SnapshotDum, AMS_ATT_DELETED)))
	  continue;
	QuickGetBodyFileName(Dir->UNIXDir, AMS_ID(SnapshotDum), FileNameBuf);
	if (unlink(FileNameBuf) < 0 && RetryBodyFileName(FileNameBuf) == 0) {
	    (void) unlink(FileNameBuf);
	}
	debug(4, ("Unlinked: %s\n", FileNameBuf));
    }
    CloseMSDir(Dir, MD_READ); /* ignore errors -- read only */

    CloseDirsThatNeedIt();
    MarkQuietlyInProgress(dirname);
    if (ReadOrFindMSDir(dirname, &Dir, MD_APPEND) != 0) {
	return(mserrcode);
    }

    sprintf(TmpFileName, "%s/%s.p", dirname, MS_DIRNAME);
    wfd = open(TmpFileName, O_RDWR | O_CREAT | O_TRUNC, 0664);
    if (wfd < 0) {
	CloseMSDir(Dir, MD_APPEND);
	AMS_RETURN_ERRCODE(errno, EIN_OPEN, EVIA_PURGEDELETEDMESSAGES);
    }
    debug(4, ("Purging %s: reading FD is %d, writing FD is %d.\n", dirname, Dir->fd, wfd));
    if (osi_ExclusiveLockNoBlock(wfd)) { /* Hand-lock the thing */
	errsave = errno;
	CloseMSDir(Dir, MD_APPEND);
	unlink(TmpFileName);
	close(wfd);
	debug(128, ("Lock failed -- Closed dir %s\n", dirname));
	AMS_RETURN_ERRCODE(errsave, EIN_FLOCK, EVIA_PURGEDELETEDMESSAGES);
    }
    if (Dir->CurPos != 0 && lseek(Dir->fd, 0, L_SET) < 0) {
	errsave = errno;
	CloseMSDir(Dir, MD_APPEND);
	unlink(TmpFileName);
	close(wfd);
	AMS_RETURN_ERRCODE(errsave, EIN_LSEEK, EVIA_PURGEDELETEDMESSAGES);
    }
    if (read(Dir->fd, HeadDum, AMS_DIRHEADSIZE) != AMS_DIRHEADSIZE) {
	errsave = errno;
	CloseMSDir(Dir, MD_APPEND);
	unlink(TmpFileName);
	close(wfd);
	AMS_RETURN_ERRCODE(errsave, EIN_LSEEK, EVIA_PURGEDELETEDMESSAGES);
    }
    Dir->CurPos = AMS_DIRHEADSIZE;
    if (writeall(wfd, HeadDum, AMS_DIRHEADSIZE) != AMS_DIRHEADSIZE) {
	errsave = errno;
	CloseMSDir(Dir, MD_APPEND);
	unlink(TmpFileName);
	close(wfd);
	AMS_RETURN_ERRCODE(errsave, EIN_WRITE, EVIA_PURGEDELETEDMESSAGES);
    }
    for (msgnum = 0; msgnum < Dir->MessageCount; ++msgnum) {
	if (GetSnapshotByNumber(Dir, msgnum, SnapshotDum)) {
	    unlink(TmpFileName);
	    close(wfd);
	    CloseMSDir(Dir, MD_APPEND);
	    ++numdeleted;
	    return(mserrcode);
	}
	/* This one is still good, copy it over */
	if (!(AMS_GET_ATTRIBUTE(SnapshotDum, AMS_ATT_DELETED)))
	  if (writeall(wfd, SnapshotDum, AMS_SNAPSHOTSIZE) != AMS_SNAPSHOTSIZE) {
	    errsave = errno;
	    CloseMSDir(Dir, MD_APPEND);
	    unlink(TmpFileName);
	    close(wfd);
	    AMS_RETURN_ERRCODE(errsave, EIN_WRITE, EVIA_PURGEDELETEDMESSAGES);
	  }
    }

    Dir->MessageCount -= numdeleted;
    /* Start to make the new file be the one pointed to by the Dir structure. */
    savefd = Dir->fd;
    Dir->fd = wfd;
    Dir->CurPos = -1;
    if (DestructivelyWriteDirectoryHead(Dir)) {
	errsave = mserrcode;
	Dir->fd = savefd;
	CloseMSDir(Dir, MD_APPEND);
	unlink(TmpFileName);
	close(wfd);
	return(errsave);
    }
    if (errsave = CloseMSDir(Dir, MD_APPEND)) {
	unlink(TmpFileName);
	(void) close(savefd);
	return (errsave);
    }
    sprintf(FileNameBuf, "%s/%s", dirname, MS_DIRNAME);
    if (RenameEvenInVice(TmpFileName, FileNameBuf)) {
	errsave = errno;
	unlink(TmpFileName);
	(void) close(savefd);
	AMS_RETURN_ERRCODE(errsave, EIN_RENAME, EVIA_PURGEDELETEDMESSAGES);
    }
    (void) close(savefd);
    return (0);
}
